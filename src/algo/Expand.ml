open Base
open Either
open Lang
open Lang.Term
open AState
open Utils

let identify_rcalls (p : psi_def) (lam : variable) (t : term) : VarSet.t =
  let join = Set.union in
  let case _ t =
    match t.tkind with
    (* lam x *)
    | TApp ({ tkind = TVar lam'; _ }, [ single_arg ]) when Variable.(lam' = lam) -> (
        match single_arg.tkind with
        | TVar x -> Some (VarSet.singleton x)
        | TApp ({ tkind = TVar repr; _ }, [ { tkind = TVar x; _ } ]) ->
            if Variable.equal repr p.psi_repr.pmain_symb then Some (VarSet.singleton x) else None
        | _ -> None)
    | _ -> None
  in
  reduce ~init:VarSet.empty ~case ~join t

let mk_recursion_elimination_term (p : psi_def) : (term * term) option =
  let _, g_out = RType.fun_typ_unpack (Variable.vtype_or_new p.psi_target.pvar)
  and f_out = fst !AState._alpha in
  if Result.is_ok (RType.unify_one g_out f_out) then
    let term = mk_composite_base_type f_out in
    Some (term, term)
  else
    match (g_out, f_out) with
    | TTup tl_lift, TTup tl' ->
        let args = List.map ~f:mk_composite_base_type tl_lift in
        let tuple_g = mk_tup args in
        let tuple_f = mk_tup (List.take args (List.length tl')) in
        Some (tuple_f, tuple_g)
    | TTup (_ :: _ as tl_lift), _ ->
        let args = List.map ~f:mk_composite_base_type tl_lift in
        let tuple_g = mk_tup args in
        Some (List.hd_exn args, tuple_g)
    | _ -> None

let subst_recursive_calls (p : psi_def) (tl : term list) : (term * term) list * TermSet.t =
  let fsymb = p.psi_reference.pmain_symb and gsymb = p.psi_target.pmain_symb in
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
      match mk_recursion_elimination_term p with
      | Some (a, b) -> (a, b)
      | None -> failwith "Cannot make recursion elimination for this problem."
    in
    let invariant =
      Option.map (second !AState._alpha) ~f:(fun inv ->
          first (infer_type (Reduce.reduce_term (mk_app inv [ scalar_term_f ]))))
    in
    ( substs
      @ [
          (mk_app (mk_var fsymb) [ mk_var var ], scalar_term_f);
          (mk_app (mk_var gsymb) [ mk_var var ], scalar_term_g);
          ( mk_app (mk_var fsymb) [ mk_app (mk_var p.psi_repr.pmain_symb) [ mk_var var ] ],
            scalar_term_f );
        ],
      match invariant with Some inv -> Set.add invariants inv | None -> invariants )
  in
  List.fold ~f ~init:([], TermSet.empty) (Set.elements rcalls)

let _subst_repr_calls (p : psi_def) (tl : term list) : (term * term) list =
  let fsymb = p.psi_repr.pmain_symb in
  let rcalls =
    let fold_f rcalled_vars t = Set.union rcalled_vars (identify_rcalls p fsymb t) in
    List.fold ~init:VarSet.empty ~f:fold_f tl
  in
  let f var =
    let rtype_var = Variable.mk ~t:(Some !AState._tau) (Alpha.fresh ()) in
    [ (mk_app (mk_var fsymb) [ mk_var var ], mk_var rtype_var) ]
  in
  List.concat (List.map ~f (Set.elements rcalls))

let maximally_reduced_app (p : psi_def) (func : term) (args : term list) : bool =
  match (func.tkind, args) with
  | TVar f, [ { tkind = TApp ({ tkind = TVar r; _ }, [ x ]); _ } ] -> (
      Variable.(f = p.psi_reference.pmain_symb && r = p.psi_repr.pmain_symb)
      && match x.tkind with TVar _ -> true | _ -> false)
  | TVar x, args ->
      Variable.(
        x = p.psi_reference.pmain_symb || x = p.psi_target.pmain_symb || x = p.psi_repr.pmain_symb)
      && List.for_all args ~f:(fun x -> match x.tkind with TVar _ -> true | _ -> false)
  | _ -> false

let nonreduced_terms (p : psi_def) (non_terms : VarSet.t) (t : term) : (variable * term list) list =
  let join = ( @ ) in
  let case f t =
    match t.tkind with
    | TApp (func, args) -> (
        match func.tkind with
        | TVar func_var when Set.mem non_terms func_var -> (
            match List.concat (List.map ~f args) with
            | [] -> if maximally_reduced_app p func args then None else Some [ (func_var, args) ]
            | l -> Some l)
        | _ -> None)
    | _ -> None
  in
  let init = [] in
  reduce ~init ~join ~case t

let nonreduced_terms_all (p : psi_def) (t : term) =
  let all_nont =
    VarSet.union_list
      [
        p.psi_reference.pnon_terminals; p.psi_target.pnon_terminals; p.psi_reference.pnon_terminals;
      ]
  in
  nonreduced_terms p all_nont t

(* Replace subterms that correspond to the right-hand side of the main rule to avoid
   capturing f(v) when v is a variable. *)
let replace_rhs_of_main ?(verbose = false) ?(for_mr = false) (p : psi_def) (f : PMRS.t) (t0 : term)
    : term =
  if verbose then Log.verbose_msg Fmt.(str "t0 = %a" pp_term t0);
  let nr = nonreduced_terms p f.pnon_terminals t0 in
  let rule_set = Map.filter f.prules ~f:(fun (nt, _, _, _) -> Variable.(nt = f.pmain_symb)) in
  let bound_params = Set.union f.psyntobjs (VarSet.of_list f.pargs) in
  let replacements =
    let f (nt, args) =
      match
        Map.max_elt (PMRS.inverted_rule_lookup ~boundvars:bound_params rule_set (mk_var nt) args)
      with
      | Some (_, lhs) ->
          if for_mr then
            match lhs.tkind with
            | TApp (_, lhs_args) -> (
                match List.last lhs_args with
                | Some { tkind = TVar _; _ } ->
                    if verbose then
                      Log.verbose_msg
                        Fmt.(
                          str "Replacement of %a by %a." pp_term
                            (mk_app (mk_var nt) args)
                            pp_term lhs);
                    Some (mk_app (mk_var nt) args, lhs)
                | _ -> None)
            | _ -> None
          else (
            if verbose then
              Log.verbose_msg
                Fmt.(str "Replacement of %a by %a." pp_term (mk_app (mk_var nt) args) pp_term lhs);
            Some (mk_app (mk_var nt) args, lhs))
      | None -> None
    in
    List.filter_map ~f:(fun x -> x) (List.map ~f nr)
  in
  let t_out = substitution replacements t0 in
  t_out

let replace_rhs_of_mains (p : psi_def) (t0 : term) : term =
  let _t0 = replace_rhs_of_main p p.psi_repr t0 in
  let __t0 = replace_rhs_of_main p p.psi_reference _t0 in
  replace_rhs_of_main p p.psi_target __t0

(* ============================================================================================= *)
(*                                   acegis TERM EXPANSION                                       *)
(* ============================================================================================= *)

let simple ?(verbose = false) ?(max_height = !Config.expand_cut) (t0 : term) =
  if verbose then Log.verbose_msg Fmt.(str "@[Simple expansion of %a.@]" pp_term t0);
  let rec aux d (t, u) =
    if d >= max_height then (t, u)
    else
      match t with
      | _ :: _ -> (t, u)
      | [] -> (
          match List.sort ~compare:term_height_compare u with
          | uhd :: utl ->
              let t_exp = Analysis.expand_once uhd in
              let t', u' = List.partition_tf ~f:Analysis.is_novariant t_exp in
              aux (d + 1) (t', utl @ u')
          | [] -> (t, u))
  in
  if Analysis.is_novariant t0 then (TermSet.singleton t0, TermSet.empty)
  else
    let t, u = aux 0 ([], [ t0 ]) in
    if verbose then (
      Log.verbose Fmt.(fun f () -> pf f "t = @[<hov 2>%a@]" (list ~sep:comma pp_term) t);
      Log.verbose Fmt.(fun f () -> pf f "u = @[<hov 2>%a@]" (list ~sep:comma pp_term) u));
    (TermSet.of_list t, TermSet.of_list u)

let make_bounded (t0 : term) =
  let case _ t =
    if Analysis.is_bounded t then Some t
    else
      match t.tkind with
      | TVar _ -> (
          match
            List.filter ~f:(fun t -> not (Analysis.is_novariant t)) (Analysis.expand_once t)
          with
          | hd :: _ ->
              let t_set, _ = simple hd in
              Set.max_elt t_set
          | _ ->
              let t_set, _ = simple t in
              Set.max_elt t_set)
      | _ -> None
  in
  transform ~case t0

(* ============================================================================================= *)
(*                               MAIN ENTRY POINTS: MR_TERMS                                     *)
(* ============================================================================================= *)
let expand_max (p : psi_def) (f : PMRS.t) (t0 : term) : (term * term) list * term list =
  let nonterminals =
    VarSet.union_list
      [ f.pnon_terminals; p.psi_repr.pnon_terminals; p.psi_reference.pnon_terminals ]
  in
  let f_of_t0 = Reduce.reduce_pmrs f t0 in
  let simpl_f_of_t0 = replace_rhs_of_main p f f_of_t0 in
  let nr = nonreduced_terms p nonterminals simpl_f_of_t0 in
  (* Collect all the variables that need to be expanded. *)
  let expand_reqs =
    let collect c (_, args) =
      match List.last args with Some arg -> Set.union c (Analysis.free_variables arg) | None -> c
    in
    List.fold ~f:collect ~init:VarSet.empty nr
  in
  let substs =
    let expansions =
      List.map
        ~f:(fun x -> List.cartesian_product [ mk_var x ] (Analysis.expand_once (mk_var x)))
        (Set.to_list expand_reqs)
    in
    cartesian_nary_product (List.filter ~f:(not <| List.is_empty) expansions)
  in
  let all_ts = List.map substs ~f:(fun s -> substitution s t0) in
  let check_max_exp t =
    let tr = replace_rhs_of_main p f (Reduce.reduce_pmrs f t) in
    match nonreduced_terms p f.pnon_terminals tr with [] -> First (t, tr) | _ -> Second t
  in
  let mr_terms, rest = List.partition_map all_ts ~f:check_max_exp in
  let mr_terms = match check_max_exp t0 with First x -> x :: mr_terms | _ -> mr_terms in
  Log.verbose (fun frmt () ->
      Fmt.(
        pf frmt "Expand > Result:@;@[%a ->@;@[%a@]@]" pp_term t0
          (list ~sep:comma (parens (pair ~sep:comma pp_term pp_term)))
          mr_terms));
  (* Expand and replace in term *)
  (mr_terms, rest)

let composed_reduction_sequence (p : psi_def) (f : PMRS.t) (g : PMRS.t) (t0 : term) =
  let _t0 = Reduce.reduce_pmrs g t0 in
  let _t1 = Reduce.reduce_pmrs f _t0 in
  let _t2 = replace_rhs_of_main ~for_mr:true p g _t1 in
  replace_rhs_of_main p f _t2

let check_max_exp p f g t =
  let _t3 = composed_reduction_sequence p f g t in
  match nonreduced_terms p (Set.union f.pnon_terminals g.pnon_terminals) _t3 with
  | [] -> First (t, _t3)
  | _ -> Second t

let expand_max_main (p : psi_def) (f : PMRS.t) (g : PMRS.t) (t0 : term) :
    (term * term) list * term list =
  let non_terminals = Set.union f.pnon_terminals g.pnon_terminals in
  let t3 = composed_reduction_sequence p f g t0 in
  let nr = nonreduced_terms p non_terminals t3 in
  (* Collect all the variables that need to be expanded. *)
  let expand_reqs =
    let collect c (_, args) =
      match List.last args with Some arg -> Set.union c (Analysis.free_variables arg) | None -> c
    in
    List.fold ~f:collect ~init:VarSet.empty nr
  in
  Log.verbose_msg Fmt.(str "@[Expand > Reqs: %a@]" VarSet.pp expand_reqs);
  let substs =
    let expansions =
      List.map
        ~f:(fun x -> List.cartesian_product [ mk_var x ] (Analysis.expand_once (mk_var x)))
        (Set.to_list expand_reqs)
    in
    cartesian_nary_product (List.filter ~f:(not <| List.is_empty) expansions)
  in
  List.iteri substs ~f:(fun i sub ->
      Log.verbose_msg Fmt.(str "@[Expand > Substs %i: %a@]" i Term.pp_subs sub));
  let all_ts = List.map substs ~f:(fun s -> substitution s t0) in
  Log.verbose (fun frmt () ->
      Fmt.(
        pf frmt "Expand > All ts:@;@[%a ->@;@[%a@]@]" pp_term t0 (list ~sep:comma pp_term) all_ts));
  let mr_terms, rest = List.partition_map all_ts ~f:(check_max_exp p f g) in
  Log.verbose (fun frmt () ->
      Fmt.(
        pf frmt "Expand > Result:@;@[%a ->@;@[%a@]@]" pp_term t0
          (list ~sep:comma (parens (pair ~sep:comma pp_term pp_term)))
          mr_terms));
  (mr_terms, rest)

let expand_driver p f g t =
  match expand_max_main p f g t with
  | [], rest -> (
      match rest with
      | hd :: tl ->
          let expanded_ts, rest' = expand_max_main p f g hd in
          (expanded_ts, tl @ rest')
      | [] -> ([], []))
  | mr_terms, rest -> (mr_terms, rest)

(**
  Expand so that terms are MR-terms for f @ g.
*)
let expand_max2 (p : psi_def) (f : PMRS.t) (g : PMRS.t) (t0 : term) : (term * term) list * term list
    =
  match check_max_exp p f g t0 with First x -> ([ x ], []) | _ -> expand_driver p f g t0

let is_mr (p : psi_def) (f : PMRS.t) (t0 : term) nt : bool =
  Log.verbose_msg Fmt.(str "@[Is %a maximally reducible by %s?@]" pp_term t0 f.PMRS.pvar.vname);
  let f_t0 = Reduce.reduce_pmrs f t0 in
  let f_t0 = replace_rhs_of_main p f f_t0 in
  let nr = nonreduced_terms p nt f_t0 in
  match nr with
  | [] ->
      Log.verbose_msg "Yes.";
      true
  | _ ->
      Log.verbose_msg "No.";
      false

let is_mr_all (p : psi_def) (t0 : term) =
  let nonterminals =
    VarSet.union_list
      [ p.psi_target.pnon_terminals; p.psi_repr.pnon_terminals; p.psi_reference.pnon_terminals ]
  in
  is_mr p p.psi_target t0 nonterminals
  && Either.is_first (check_max_exp p p.psi_reference p.psi_repr t0)

(** `maximal p t0 ` expands the term `t0 ` into T, U such that all terms in T are MR-terms
  for (p.psi_reference (p.psi_repr)) and p.psi_target and T,U is a boundary.
*)
let to_maximally_reducible (p : psi_def) (t0 : term) : TermSet.t * TermSet.t =
  let nonterminals =
    VarSet.union_list
      [ p.psi_target.pnon_terminals; p.psi_repr.pnon_terminals; p.psi_reference.pnon_terminals ]
  in
  let tset0, uset0 =
    let g = p.psi_target in
    (* Expand only if there are non-reduced terms *)
    if is_mr p g t0 nonterminals then ([ (t0, t0) ], []) else expand_max p g t0
  in
  (* Expand with orig (f) *)
  let f (tset, uset) (t_theta, _) =
    let new_ts, new_us = expand_max2 p p.psi_reference p.psi_repr t_theta in
    (tset @ List.map ~f:first new_ts, uset @ new_us)
  in
  let l1, l2 = List.fold tset0 ~f ~init:([], uset0) in
  (TermSet.of_list l1, TermSet.of_list l2)
