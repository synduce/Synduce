open Base
open Lang
open Lang.Term
open AState
open Utils


let maximally_reduced_app (p : psi_def) (func : term) (args : term list) : bool =
  (match func.tkind with
   | TVar x ->
     Variable.(x = p.orig.pmain_symb || x = p.target.pmain_symb || x = p.repr.pmain_symb)
   | _ -> false)
  &&
  (List.for_all args ~f:(fun x -> match x.tkind with TVar _ -> true | _ -> false))


let nonreduced_terms (p : psi_def) (non_terms : VarSet.t) (t : term)
  : (variable * term list) list =
  let join = (@) in
  let case _ t =
    match t.tkind with
    | TApp(func, args) ->
      (match func.tkind with
       | TVar func_var when Set.mem non_terms func_var ->
         if maximally_reduced_app p func args then None
         else Some [func_var, args]
       | _ -> None)
    | _ -> None
  in
  let init = [] in
  reduce ~init ~join ~case t


let nonreduced_terms_all (p : psi_def) (t : term) =
  let all_nont =
    VarSet.union_list [p.orig.pnon_terminals; p.target.pnon_terminals; p.orig.pnon_terminals]
  in
  nonreduced_terms p all_nont t


let replace_nonreduced_by (p : psi_def) (f : PMRS.t) (t0 : term) : term =
  let nr = nonreduced_terms p f.pnon_terminals t0 in
  let rule_set =
    Map.filter  f.prules ~f:(fun (nt, _, _, _) -> Variable.(nt = f.pmain_symb))
  in
  let replacements =
    let f (nt, args) =
      match Map.max_elt (PMRS.inverted_rule_lookup rule_set (mk_var nt) args) with
      | Some (_, lhs) -> Some (mk_app (mk_var nt) args, lhs)
      | None -> None
    in
    List.filter_map ~f:(fun x -> x) (List.map ~f nr)
  in
  substitution replacements t0


let expand_max (p : psi_def) (f : PMRS.t) (t0 : term) : TermSet.t * TermSet.t =
  Fmt.(pf stdout "@[<hov 2>t0 = %a@]@." pp_term t0);
  let t0' = PMRS.reduce f t0 in
  Fmt.(pf stdout "@[<hov 2>t0' = %a@]@." pp_term t0');
  let t' = replace_nonreduced_by p f t0' in
  Fmt.(pf stdout "@[<hov 2>t' = %a@]@." pp_term t');
  let nr = nonreduced_terms p f.pnon_terminals t' in
  (* Collect all the variables that need to be expanded. *)
  let expand_reqs =
    let collect c (_, args) =
      match List.last args with
      | Some arg -> Set.union c (Analysis.free_variables arg)
      | None -> c
    in List.fold ~f:collect ~init:VarSet.empty nr
  in
  let substs =
    let expansions =
      List.map ~f:(fun x -> List.cartesian_product [mk_var x] (Analysis.expand_once (mk_var x)))
        (Set.to_list expand_reqs)
    in cartesian_nary_product expansions
  in
  let all_ts = List.map substs ~f:(fun s -> substitution s t0) in
  let check_max_exp t =
    let tr = replace_nonreduced_by p f (PMRS.reduce f t) in
    Fmt.(pf stdout "@[<hov 2>Check max -exp = %a@]@." pp_term tr);
    match nonreduced_terms p f.pnon_terminals tr with
      [] -> Fmt.(pf stdout "@[<hov 2>ok@]@."); true
    | _ ->
      false
  in
  let mr_terms, rest =
    List.partition_tf all_ts ~f:check_max_exp
  in
  let mr_terms = if check_max_exp t0 then t0::mr_terms else mr_terms in
  Log.debug
    (fun frmt () -> Fmt.(pf frmt "@[<hov 2>%a -> %a@]" pp_term t0 (list ~sep:comma pp_term) mr_terms));
  (* Expand and replace in term *)
  TermSet.of_list mr_terms, TermSet.of_list rest

(** `maximal p t0 ` expands the term `t0 ` such that `p.orig (p.repr t0)` is a maximally
    reduced term.
*)
let maximal (p : psi_def) (t0 : term) : TermSet.t * TermSet .t =
  let tset0, uset0 = expand_max p p.repr t0 in
  Fmt.(pf stdout "==> Expansion with repr@.%a@." (list ~sep:comma pp_term) (Set.elements tset0));
  let f (tset, uset) t =
    let t', u' = expand_max p p.orig t in
    Set.union tset t', Set.union uset u'
  in
  List.fold (Set.elements tset0) ~f ~init:(TermSet.empty, uset0)

