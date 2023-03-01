open Base
open Lang

(*  *)
open Either
open Env
open Term
open ProblemDefs
open Utils

let identify_rcalls (p : PsiDef.t) (lam : variable) (t : term) : VarSet.t =
  let join = Set.union in
  let case _ t =
    match t.tkind with
    (* lam x *)
    | TApp ({ tkind = TVar lam'; _ }, [ single_arg ]) when Variable.(lam' = lam) ->
      (match single_arg.tkind with
      | TVar x -> Some (VarSet.singleton x)
      | TApp ({ tkind = TVar repr; _ }, [ { tkind = TVar x; _ } ]) ->
        if Variable.equal repr p.PsiDef.repr.pmain_symb
        then Some (VarSet.singleton x)
        else None
      | _ -> None)
    | _ -> None
  in
  reduce ~init:VarSet.empty ~case ~join t
;;

let mk_recursion_elimination_term ~(ctx : env) (p : PsiDef.t) : (term * term) option =
  let _, g_out = RType.fun_typ_unpack (Variable.vtype_or_new ctx.ctx p.PsiDef.target.pvar)
  and f_out = get_alpha ctx in
  if Result.is_ok (RType.unify_one g_out f_out)
  then (
    (* No lifting present. *)
    let term = ctx >- mk_composite_base_type f_out in
    Some (term, term))
  else (
    (* Lifting present. *)
    match g_out, f_out with
    | TTup tl_lift, TTup tl' ->
      let args = List.map ~f:(ctx >- mk_composite_base_type ~prefix:"_elim_") tl_lift in
      let tuple_g = mk_tup ctx.ctx args in
      let tuple_f = mk_tup ctx.ctx (List.take args (List.length tl')) in
      Some (tuple_f, tuple_g)
    | TTup (_ :: _ as tl_lift), _ ->
      let args = List.map ~f:(ctx >- mk_composite_base_type ~prefix:"_elim_") tl_lift in
      let tuple_g = mk_tup ctx.ctx args in
      Some (List.hd_exn args, tuple_g)
    | _ -> None)
;;

let subst_recursive_calls ~(ctx : env) (p : PsiDef.t) (tl : term list)
    : (term * term) list * TermSet.t
  =
  let fsymb = p.PsiDef.reference.pmain_symb
  and gsymb = p.PsiDef.target.pmain_symb in
  let rcalls =
    let fold_f rcalled_vars t =
      let f_x = identify_rcalls p fsymb t in
      let g_x = identify_rcalls p gsymb t in
      VarSet.union_list [ rcalled_vars; f_x; g_x ]
    in
    List.fold ~init:VarSet.empty ~f:fold_f tl
  in
  let f (substs, invariants) var =
    let scalar_term_f, scalar_term_g =
      match mk_recursion_elimination_term ~ctx p with
      | Some (a, b) -> a, b
      | None -> failwith "Cannot make recursion elimination for this problem."
    in
    let invariant =
      Option.map
        (ctx >- Specifications.get_ensures p.PsiDef.reference.pvar)
        ~f:(fun inv ->
          first (infer_type ctx.ctx (ctx_reduce ctx (mk_app inv [ scalar_term_f ]))))
    in
    ( substs
      @ [ mk_app (mk_var ctx.ctx fsymb) [ mk_var ctx.ctx var ], scalar_term_f
        ; mk_app (mk_var ctx.ctx gsymb) [ mk_var ctx.ctx var ], scalar_term_g
        ; ( mk_app
              (mk_var ctx.ctx fsymb)
              [ mk_app (mk_var ctx.ctx p.PsiDef.repr.pmain_symb) [ mk_var ctx.ctx var ] ]
          , scalar_term_f )
        ]
    , match invariant with
      | Some inv -> Set.add invariants inv
      | None -> invariants )
  in
  List.fold ~f ~init:([], TermSet.empty) (Set.elements rcalls)
;;

let _subst_repr_calls ~(ctx : env) (p : PsiDef.t) (tl : term list) : (term * term) list =
  let fsymb = p.PsiDef.repr.pmain_symb in
  let rcalls =
    let fold_f rcalled_vars t = Set.union rcalled_vars (identify_rcalls p fsymb t) in
    List.fold ~init:VarSet.empty ~f:fold_f tl
  in
  let f var =
    let rtype_var =
      Variable.mk ctx.ctx ~t:(Some (get_tau ctx)) (Alpha.fresh ctx.ctx.names)
    in
    [ mk_app (mk_var ctx.ctx fsymb) [ mk_var ctx.ctx var ], mk_var ctx.ctx rtype_var ]
  in
  List.concat (List.map ~f (Set.elements rcalls))
;;

let maximally_reduced_app (p : PsiDef.t) (func : term) (args : term list) : bool =
  match func.tkind, args with
  | TVar f, [ { tkind = TApp ({ tkind = TVar r; _ }, [ x ]); _ } ] ->
    Variable.(f = p.PsiDef.reference.pmain_symb && r = p.PsiDef.repr.pmain_symb)
    &&
    (match x.tkind with
    | TVar _ -> true
    | _ -> false)
  | TVar x, args ->
    Variable.(
      x = p.PsiDef.reference.pmain_symb
      || x = p.PsiDef.target.pmain_symb
      || x = p.PsiDef.repr.pmain_symb)
    && List.for_all args ~f:(fun x ->
           match x.tkind with
           | TVar _ -> true
           | _ -> false)
  | _ -> false
;;

let nonreduced_terms (p : PsiDef.t) (non_terms : VarSet.t) (t : term)
    : (variable * term list) list
  =
  let join = ( @ ) in
  let case f t =
    match t.tkind with
    | TApp (func, args) ->
      (match func.tkind with
      | TVar func_var when Set.mem non_terms func_var ->
        (match List.concat (List.map ~f args) with
        | [] ->
          if maximally_reduced_app p func args then None else Some [ func_var, args ]
        | l -> Some l)
      | _ -> None)
    | _ -> None
  in
  let init = [] in
  reduce ~init ~join ~case t
;;

let nonreduced_terms_all (p : PsiDef.t) (t : term) =
  let all_nont =
    VarSet.union_list
      [ p.PsiDef.reference.pnon_terminals
      ; p.PsiDef.target.pnon_terminals
      ; p.PsiDef.reference.pnon_terminals
      ]
  in
  nonreduced_terms p all_nont t
;;

(* Replace subterms that correspond to the right-hand side of the main rule to avoid
   capturing f(v) when v is a variable. *)
let replace_rhs_of_main
    ?(verbose = false)
    ?(for_mr = false)
    ~(ctx : Context.t)
    (p : PsiDef.t)
    (f : PMRS.t)
    (t0 : term)
    : term
  =
  if verbose then Log.verbose_msg Fmt.(str "t0 = %a" (pp_term ctx) t0);
  let nr = nonreduced_terms p f.pnon_terminals t0 in
  let rule_set =
    Map.filter f.prules ~f:(fun (nt, _, _, _) -> Variable.(nt = f.pmain_symb))
  in
  let bound_params = Set.union f.psyntobjs (VarSet.of_list f.pargs) in
  let replacements =
    let f (nt, args) =
      match
        Map.max_elt
          (PMRS.inverted_rule_lookup
             ~ctx
             ~boundvars:bound_params
             rule_set
             (mk_var ctx nt)
             args)
      with
      | Some (_, lhs) ->
        if for_mr
        then (
          match lhs.tkind with
          | TApp (_, lhs_args) ->
            (match List.last lhs_args with
            | Some { tkind = TVar _; _ } ->
              if verbose
              then
                Log.verbose_msg
                  Fmt.(
                    str
                      "Replacement of %a by %a."
                      (pp_term ctx)
                      (mk_app (mk_var ctx nt) args)
                      (pp_term ctx)
                      lhs);
              Some (mk_app (mk_var ctx nt) args, lhs)
            | _ -> None)
          | _ -> None)
        else (
          if verbose
          then
            Log.verbose_msg
              Fmt.(
                str
                  "Replacement of %a by %a."
                  (pp_term ctx)
                  (mk_app (mk_var ctx nt) args)
                  (pp_term ctx)
                  lhs);
          Some (mk_app (mk_var ctx nt) args, lhs))
      | None -> None
    in
    List.filter_map ~f:(fun x -> x) (List.map ~f nr)
  in
  let t_out = substitution replacements t0 in
  t_out
;;

let replace_rhs_of_mains ~(ctx : Context.t) (p : PsiDef.t) (t0 : term) : term =
  let _t0 = replace_rhs_of_main ~ctx p p.PsiDef.repr t0 in
  let __t0 = replace_rhs_of_main ~ctx p p.PsiDef.reference _t0 in
  replace_rhs_of_main ~ctx p p.PsiDef.target __t0
;;

(* ============================================================================================= *)
(*                                   segis TERM EXPANSION                                        *)
(* ============================================================================================= *)

let simple
    ?(verbose = false)
    ?(max_height = !Config.Optims.expand_cut)
    ~(ctx : Context.t)
    (t0 : term)
  =
  if verbose then Log.verbose_msg Fmt.(str "@[Simple expansion of %a.@]" (pp_term ctx) t0);
  let rec aux d (t, u) =
    if d >= max_height
    then t, u
    else (
      match t with
      | _ :: _ -> t, u
      | [] ->
        (match List.sort ~compare:term_height_compare u with
        | uhd :: utl ->
          let t_exp = Analysis.expand_once ~ctx uhd in
          let t', u' = List.partition_tf ~f:(Analysis.is_novariant ~ctx) t_exp in
          aux (d + 1) (t', utl @ u')
        | [] -> t, u))
  in
  if Analysis.is_novariant ~ctx t0
  then TermSet.singleton t0, TermSet.empty
  else (
    let t, u = aux 0 ([], [ t0 ]) in
    if verbose
    then (
      Log.verbose
        Fmt.(fun f () -> pf f "t = @[<hov 2>%a@]" (list ~sep:comma (pp_term ctx)) t);
      Log.verbose
        Fmt.(fun f () -> pf f "u = @[<hov 2>%a@]" (list ~sep:comma (pp_term ctx)) u));
    TermSet.of_list t, TermSet.of_list u)
;;

let make_bounded ~(ctx : Context.t) (t0 : term) =
  let case _ t =
    if Analysis.is_bounded ~ctx t
    then Some t
    else (
      match t.tkind with
      | TVar _ ->
        (match
           List.filter
             ~f:(fun t -> not (Analysis.is_novariant ~ctx t))
             (Analysis.expand_once ~ctx t)
         with
        | hd :: _ ->
          let t_set, _ = simple ~ctx hd in
          Set.max_elt t_set
        | _ ->
          let t_set, _ = simple ~ctx t in
          Set.max_elt t_set)
      | _ -> None)
  in
  transform ~case t0
;;

(* ============================================================================================= *)
(*                               MAIN ENTRY POINTS: MR_TERMS                                     *)
(* ============================================================================================= *)
let expand_max
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (p : PsiDef.t)
    (f : PMRS.t)
    (t0 : term)
    : (term * term) list * term list
  =
  let nonterminals =
    VarSet.union_list
      [ f.pnon_terminals
      ; p.PsiDef.repr.pnon_terminals
      ; p.PsiDef.reference.pnon_terminals
      ]
  in
  let f_of_t0 = Reduce.reduce_pmrs ~ctx ~fctx f t0 in
  let simpl_f_of_t0 = replace_rhs_of_main ~ctx p f f_of_t0 in
  let nr = nonreduced_terms p nonterminals simpl_f_of_t0 in
  (* Collect all the variables that need to be expanded. *)
  let expand_reqs =
    let collect c (_, args) =
      match List.last args with
      | Some arg -> Set.union c (Analysis.free_variables ~ctx arg)
      | None -> c
    in
    List.fold ~f:collect ~init:VarSet.empty nr
  in
  let substs =
    let expansions =
      List.map
        ~f:(fun x ->
          List.cartesian_product
            [ mk_var ctx x ]
            (Analysis.expand_once ~ctx (mk_var ctx x)))
        (Set.to_list expand_reqs)
    in
    cartesian_nary_product (List.filter ~f:(not <| List.is_empty) expansions)
  in
  let all_ts = List.map substs ~f:(fun s -> substitution s t0) in
  let check_max_exp t =
    let tr = replace_rhs_of_main ~ctx p f (Reduce.reduce_pmrs ~ctx ~fctx f t) in
    match nonreduced_terms p f.pnon_terminals tr with
    | [] -> First (t, tr)
    | _ -> Second t
  in
  let mr_terms, rest = List.partition_map all_ts ~f:check_max_exp in
  let mr_terms =
    match check_max_exp t0 with
    | First x -> x :: mr_terms
    | _ -> mr_terms
  in
  (* Expand and replace in term *)
  mr_terms, rest
;;

let composed_reduction_sequence
    ~(ctx : Env.env)
    (p : PsiDef.t)
    (f : PMRS.t)
    (g : PMRS.t)
    (t0 : term)
  =
  let _t0 = ctx >>- Reduce.reduce_pmrs g t0 in
  let _t1 = ctx >>- Reduce.reduce_pmrs f _t0 in
  let _t2 = ctx >- replace_rhs_of_main ~for_mr:true p g _t1 in
  ctx >- replace_rhs_of_main p f _t2
;;

let check_max_exp ~(ctx : Env.env) p f g t =
  let t3 = composed_reduction_sequence ~ctx p f g t in
  match nonreduced_terms p (Set.union f.pnon_terminals g.pnon_terminals) t3 with
  | [] -> First (t, t3)
  | _ -> Second t
;;

let expand_max_main ~(ctx : Env.env) (p : PsiDef.t) (f : PMRS.t) (g : PMRS.t) (t0 : term)
    : (term * term) list * term list
  =
  let non_terminals = Set.union f.pnon_terminals g.pnon_terminals in
  let t3 = composed_reduction_sequence ~ctx p f g t0 in
  let nr = nonreduced_terms p non_terminals t3 in
  (* Collect all the variables that need to be expanded. *)
  let expand_reqs =
    let collect c (_, args) =
      match List.last args with
      | Some arg -> Set.union c (ctx >- Analysis.free_variables arg)
      | None -> c
    in
    List.fold ~f:collect ~init:VarSet.empty nr
  in
  let substs =
    let expansions =
      List.map
        ~f:(fun x ->
          List.cartesian_product
            [ mk_var ctx.ctx x ]
            (ctx >- Analysis.expand_once (mk_var ctx.ctx x)))
        (Set.to_list expand_reqs)
    in
    cartesian_nary_product (List.filter ~f:(not <| List.is_empty) expansions)
  in
  let all_ts = List.map substs ~f:(fun s -> substitution s t0) in
  let mr_terms, rest = List.partition_map all_ts ~f:(check_max_exp ~ctx p f g) in
  mr_terms, rest
;;

let expand_driver ~(ctx : Env.env) p f g t =
  match expand_max_main ~ctx p f g t with
  | [], rest ->
    (match rest with
    | hd :: tl ->
      let expanded_ts, rest' = expand_max_main ~ctx p f g hd in
      expanded_ts, tl @ rest'
    | [] -> [], [])
  | mr_terms, rest -> mr_terms, rest
;;

(**
  Expand so that terms are MR-terms for f @ g.
*)
let expand_max2
    ~(ctx : Env.env)
    (p : PsiDef.t)
    ~refr:(f : PMRS.t)
    ~target:(g : PMRS.t)
    (t0 : term)
    : (term * term) list * term list
  =
  match check_max_exp ~ctx p f g t0 with
  | First x -> [ x ], []
  | _ -> expand_driver ~ctx p f g t0
;;

let is_mr ~(ctx : Env.env) (p : PsiDef.t) (f : PMRS.t) (t0 : term) nt : bool =
  let f_t0 = ctx >>- Reduce.reduce_pmrs f t0 in
  let f_t0 = ctx >- replace_rhs_of_main p f f_t0 in
  let nr = nonreduced_terms p nt f_t0 in
  match nr with
  | [] -> true
  | _ -> false
;;

let is_mr_all ~(ctx : Env.env) (p : PsiDef.t) (t0 : term) =
  let nonterminals =
    VarSet.union_list
      [ p.PsiDef.target.pnon_terminals
      ; p.PsiDef.repr.pnon_terminals
      ; p.PsiDef.reference.pnon_terminals
      ]
  in
  is_mr ~ctx p p.PsiDef.target t0 nonterminals
  && Either.is_first (check_max_exp ~ctx p p.PsiDef.reference p.PsiDef.repr t0)
;;

(** `maximal p t0 ` expands the term `t0 ` into T, U such that all terms in T are MR-terms
  for (p.PsiDef.reference (p.PsiDef.repr)) and p.PsiDef.target and T,U is a boundary.
*)
let to_maximally_reducible ~(ctx : Env.env) (p : PsiDef.t) (t0 : term)
    : TermSet.t * TermSet.t
  =
  let nonterminals =
    VarSet.union_list
      [ p.PsiDef.target.pnon_terminals
      ; p.PsiDef.repr.pnon_terminals
      ; p.PsiDef.reference.pnon_terminals
      ]
  in
  let tset0, uset0 =
    let g = p.PsiDef.target in
    (* Expand only if there are non-reduced terms *)
    if is_mr ~ctx p g t0 nonterminals then [ t0, t0 ], [] else ctx >>- expand_max p g t0
  in
  (* Expand with orig (f) *)
  let f (tset, uset) (t_theta, _) =
    let new_ts, new_us =
      expand_max2 ~ctx p ~refr:p.PsiDef.reference ~target:p.PsiDef.repr t_theta
    in
    tset @ List.map ~f:first new_ts, uset @ new_us
  in
  let l1, l2 = List.fold tset0 ~f ~init:([], uset0) in
  TermSet.of_list l1, TermSet.of_list l2
;;

(**
  `expand_all ~fctx ~ctx p (t,u)` expands all terms in `u` to at least one MR-term according to
    `p`. If the pair (t,u) is a boundary then the pair of sets returned will also be a boundary.
*)
let expand_all
    ?(fuel = 0.0)
    ~(ctx : Env.env)
    (p : PsiDef.t)
    ((t, u) : TermSet.t * TermSet.t)
    : (TermSet.t * TermSet.t, unit) Result.t
  =
  let t0 = Unix.gettimeofday () in
  let fuel_left () = Float.((100.0 * (Unix.gettimeofday () - t0)) - fuel) in
  try
    Ok
      (List.fold (VarSet.elements u) ~init:(t, TermSet.empty) ~f:(fun (t', u') t0 ->
           if Float.(fuel_left () < 0.)
           then failwith "no fuel"
           else (
             let t'', u'' = to_maximally_reducible ~ctx p t0 in
             Set.union t' t'', Set.union u' u'')))
  with
  | _ -> (* No fuel left. *) Error ()
;;

let expand_fast ~(ctx : Context.t) (initial_term : term) =
  let aux () =
    let vars_to_expand =
      Set.to_list
        (Set.filter
           (Analysis.free_variables ~include_functions:false ~ctx initial_term)
           ~f:(fun v -> Option.is_some (Analysis.is_expandable_var ~ctx v)))
    in
    let n =
      let l = List.length vars_to_expand in
      if l > 1
      then
        Int.of_float
          Float.(of_int !Config.Optims.num_expansions_check ** (1.0 /. Float.of_int l))
        + 1
      else !Config.Optims.num_expansions_check
    in
    let subs =
      List.map vars_to_expand ~f:(fun v ->
          List.map
            ~f:(fun t -> mk_var ctx v, t)
            (Analysis.DType.gen_terms ~ctx (Variable.vtype_or_new ctx v) n))
    in
    List.map ~f:(fun subs -> substitution subs initial_term) (cartesian_nary_product subs)
  in
  if Analysis.is_bounded ~ctx initial_term then [ initial_term ] else aux ()
;;

(* ============================================================================================= *)
(*                                   EXPAND TERM UTILS                                           *)
(* ============================================================================================= *)
open Smtlib
open SmtInterface

let lwt_expand_loop
    (counter : int ref)
    (t_check : AsyncSmt.response -> term -> AsyncSmt.response)
    ?(r_stop =
      function
      | SmtLib.Sat -> true
      | _ -> false)
    ?(r_complete = SmtLib.Unsat)
    ~(ctx : Context.t)
    (u : term Lwt.t)
  =
  let open Lwt in
  let%lwt initial_term = u in
  let terms_to_check () = expand_fast ~ctx initial_term in
  let rec aux u =
    let%lwt u = u in
    match u, !counter < !Config.Optims.num_expansions_check with
    | t0 :: rest, true ->
      (match%lwt t_check (return SmtLib.Unknown) t0 with
      | Sat -> return SmtLib.Sat
      | _ as check_result ->
        Int.incr counter;
        if r_stop check_result then return check_result else aux (return rest))
    | [], true ->
      Log.verbose_msg "Bounded checking is complete.";
      (* All expansions have been checked. *)
      return r_complete
    | _, false ->
      (* Check reached limit. *)
      if !Config.no_bounded_sat_as_unsat
      then (* Return Unsat, as if all terms had been checked. *)
        return r_complete
      else (* Otherwise, it's unknown. *)
        return SmtLib.Unknown
  in
  aux (Lwt.return (terms_to_check ()))
;;
