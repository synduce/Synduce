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
            if Variable.equal repr p.repr.pmain_symb then Some (VarSet.singleton x) else None
        | _ -> None)
    | _ -> None
  in
  reduce ~init:VarSet.empty ~case ~join t

let subst_recursive_calls (p : psi_def) (tl : term list) : (term * term) list * TermSet.t =
  let fsymb = p.orig.pmain_symb and gsymb = p.target.pmain_symb in
  let rcalls =
    let fold_f rcalled_vars t =
      let f_x = identify_rcalls p fsymb t in
      let g_x = identify_rcalls p gsymb t in
      VarSet.union_list [ rcalled_vars; f_x; g_x ]
    in
    List.fold ~init:VarSet.empty ~f:fold_f tl
  in
  let f (substs, invariants) var =
    let scalar_term = mk_composite_base_type (first !AState._alpha) in
    let invariant =
      Option.map (second !AState._alpha) ~f:(fun inv ->
          first (infer_type (Reduce.reduce_term (mk_app inv [ scalar_term ]))))
    in
    ( substs
      @ [
          (mk_app (mk_var fsymb) [ mk_var var ], scalar_term);
          (mk_app (mk_var gsymb) [ mk_var var ], scalar_term);
          (mk_app (mk_var fsymb) [ mk_app (mk_var p.repr.pmain_symb) [ mk_var var ] ], scalar_term);
        ],
      match invariant with Some inv -> Set.add invariants inv | None -> invariants )
  in
  List.fold ~f ~init:([], TermSet.empty) (Set.elements rcalls)

let subst_repr_calls (p : psi_def) (tl : term list) : (term * term) list =
  let fsymb = p.repr.pmain_symb in
  let rcalls =
    let fold_f rcalled_vars t = Set.union rcalled_vars (identify_rcalls p fsymb t) in
    List.fold ~init:VarSet.empty ~f:fold_f tl
  in
  let f var =
    let rtype_var = Variable.mk ~t:(Some !AState._tau) (Alpha.fresh "x") in
    [ (mk_app (mk_var fsymb) [ mk_var var ], mk_var rtype_var) ]
  in
  List.concat (List.map ~f (Set.elements rcalls))

let maximally_reduced_app (p : psi_def) (func : term) (args : term list) : bool =
  match (func.tkind, args) with
  | TVar f, [ { tkind = TApp ({ tkind = TVar r; _ }, [ x ]); _ } ] -> (
      Variable.(f = p.orig.pmain_symb && r = p.repr.pmain_symb)
      && match x.tkind with TVar _ -> true | _ -> false)
  | TVar x, args ->
      Variable.(x = p.orig.pmain_symb || x = p.target.pmain_symb || x = p.repr.pmain_symb)
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
    VarSet.union_list [ p.orig.pnon_terminals; p.target.pnon_terminals; p.orig.pnon_terminals ]
  in
  nonreduced_terms p all_nont t

let replace_rhs_of_main (p : psi_def) (f : PMRS.t) (t0 : term) : term =
  let nr = nonreduced_terms p f.pnon_terminals t0 in
  let rule_set = Map.filter f.prules ~f:(fun (nt, _, _, _) -> Variable.(nt = f.pmain_symb)) in
  let bound_params = Set.union f.psyntobjs (VarSet.of_list f.pargs) in
  let replacements =
    let f (nt, args) =
      match
        Map.max_elt (PMRS.inverted_rule_lookup ~boundvars:bound_params rule_set (mk_var nt) args)
      with
      | Some (_, lhs) -> Some (mk_app (mk_var nt) args, lhs)
      | None -> None
    in
    List.filter_map ~f:(fun x -> x) (List.map ~f nr)
  in
  let t_out = substitution replacements t0 in
  t_out

let replace_rhs_of_mains (p : psi_def) (t0 : term) : term =
  let _t0 = replace_rhs_of_main p p.repr t0 in
  let __t0 = replace_rhs_of_main p p.orig _t0 in
  replace_rhs_of_main p p.target __t0

(* ============================================================================================= *)
(*                                   acegis TERM EXPANSION                                        *)
(* ============================================================================================= *)

let is_max_expanded (t : term) = Term.is_novariant t

let simple ?(max_height = !Config.expand_cut) (t0 : term) =
  Log.verbose_msg Fmt.(str "Simple expansion of %a." pp_term t0);
  let rec aux d (t, u) =
    if d >= max_height then (t, u)
    else
      match t with
      | _ :: _ -> (t, u)
      | [] -> (
          match List.sort ~compare:term_height_compare u with
          | uhd :: utl ->
              let t_exp = Analysis.expand_once uhd in
              let t', u' = List.partition_tf ~f:is_max_expanded t_exp in
              aux (d + 1) (t', utl @ u')
          | [] -> (t, u))
  in
  if is_novariant t0 then (TermSet.singleton t0, TermSet.empty)
  else
    let t, u = aux 0 ([], [ t0 ]) in
    Log.verbose Fmt.(fun f () -> pf f "t = @[<hov 2>%a@]" (list ~sep:comma pp_term) t);
    Log.verbose Fmt.(fun f () -> pf f "u = @[<hov 2>%a@]" (list ~sep:comma pp_term) u);
    (TermSet.of_list t, TermSet.of_list u)

(* ============================================================================================= *)
(*                               MAIN ENTRY POINTS: MR_TERMS                                     *)
(* ============================================================================================= *)
let expand_max (p : psi_def) (f : PMRS.t) (t0 : term) : (term * term) list * term list =
  let nonterminals =
    VarSet.union_list [ f.pnon_terminals; p.repr.pnon_terminals; p.orig.pnon_terminals ]
  in
  let f_of_t0 = Reduce.reduce_pmrs f t0 in
  let simpl_f_of_t0 = replace_rhs_of_main p f f_of_t0 in
  Log.verbose_msg Fmt.(str "@[<hov 2>Expand > f(t0) = %a@]" pp_term simpl_f_of_t0);
  let nr = nonreduced_terms p nonterminals simpl_f_of_t0 in
  Log.verbose_msg
    Fmt.(
      str "@[<hov 2>Expand > Non reduced terms = %a@]" (list pp_term)
        (List.map ~f:(fun (a, b) -> mk_app (mk_var a) b) nr));
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
  let _t2 = replace_rhs_of_main p g _t1 in
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
  Log.verbose_msg Fmt.(str "@[<hov 2>Expand > f(%a) = %a@]" pp_term t0 pp_term t3);
  let nr = nonreduced_terms p non_terminals t3 in
  Log.verbose_msg
    Fmt.(
      str "@[<hov 2>Expand > Non reduced terms = %a@]" (list pp_term)
        (List.map ~f:(fun (a, b) -> mk_app (mk_var a) b) nr));
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

let expand_max2 (p : psi_def) (f : PMRS.t) (g : PMRS.t) (t0 : term) : (term * term) list * term list
    =
  match check_max_exp p f g t0 with First x -> ([ x ], []) | _ -> expand_driver p f g t0

let is_mr (p : psi_def) (f : PMRS.t) (t0 : term) nt : bool =
  let f_t0 = Reduce.reduce_pmrs f t0 in
  let f_t0 = replace_rhs_of_main p f f_t0 in
  let nr = nonreduced_terms p nt f_t0 in
  match nr with [] -> true | _ -> false

let is_mr_all (p : psi_def) (t0 : term) =
  let nonterminals =
    VarSet.union_list [ p.target.pnon_terminals; p.repr.pnon_terminals; p.orig.pnon_terminals ]
  in
  is_mr p p.target t0 nonterminals && Either.is_first (check_max_exp p p.orig p.repr t0)

(** `maximal p t0 ` expands the term `t0 ` such that `p.orig (p.repr t0)` and 'p.target t0`
    are maximally reduced terms.
*)
let to_maximally_reducible (p : psi_def) (t0 : term) : TermSet.t * TermSet.t =
  Log.verbose_msg Fmt.(str "@[Expand > t0 = %a@]" pp_term t0);
  let nonterminals =
    VarSet.union_list [ p.target.pnon_terminals; p.repr.pnon_terminals; p.orig.pnon_terminals ]
  in
  let tset0, uset0 =
    let g = p.target in
    (* Expand only if there are non-reduced terms *)
    if is_mr p g t0 nonterminals then ([ (t0, t0) ], []) else expand_max p g t0
  in
  Log.verbose_msg
    Fmt.(str "@[Expand > tset_target = %a@]" (list ~sep:comma pp_term) (List.map ~f:first tset0));
  (* Expand with orig (f) *)
  let f (tset, uset) (t_theta, _) =
    Log.verbose_msg Fmt.(str "===== Expand step: orig =====");
    let new_ts, new_us = expand_max2 p p.orig p.repr t_theta in
    (tset @ List.map ~f:first new_ts, uset @ new_us)
  in
  let l1, l2 = List.fold tset0 ~f ~init:([], uset0) in
  (TermSet.of_list l1, TermSet.of_list l2)
