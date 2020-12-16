open Base
open Lang
open Lang.Term
open AState
open Utils


let maximally_reduced_app (p : psi_def) (func : term) (args : term list) : bool =
  (match func.tkind with
   | TVar x -> Variable.(x = p.orig.pmain_symb || x = p.target.pmain_symb)
   | _ -> false)
  &&
  (List.for_all args ~f:(fun x -> match x.tkind with TVar _ -> true | _ -> false))


let nonreduced_terms (p : psi_def) (t : term) : (variable * term list) list =
  let all_nont =
    VarSet.union_list [p.target.pnon_terminals; p.orig.pnon_terminals; p.repr.pnon_terminals]
  in
  let join = (@) in
  let case _ t =
    match t.tkind with
    | TApp(func, args) ->
      (match func.tkind with
       | TVar func_var when Set.mem all_nont func_var ->
         if maximally_reduced_app p func args then None
         else Some [func_var, args]
       | _ -> None)
    | _ -> None
  in
  let init = [] in
  reduce ~init ~join ~case t

let replace_nonreduced_by_main (p : psi_def) (t : term) : term =
  let nr = nonreduced_terms p t in
  let f_init_rules =
    Map.filter  p.orig.prules ~f:(fun (nt, _, _, _) -> Variable.(nt = p.orig.pmain_symb))
  and g_init_rules =
    Map.filter p.target.prules ~f:(fun (nt, _, _, _) -> Variable.(nt = p.target.pmain_symb))
  in
  let replacements rule_set =
    let f (nt, args) =
      match Map.max_elt (PMRS.inverted_rule_lookup rule_set (mk_var nt) args) with
      | Some (_, lhs) -> Some (mk_app (mk_var nt) args, lhs)
      | None -> None
    in
    List.filter_map ~f:(fun x -> x) (List.map ~f nr)
  in
  let subst = (replacements g_init_rules) @ (replacements f_init_rules) in
  substitution subst t

let maximal (p : psi_def) (t : term) : TermSet.t * TermSet .t =
  let t' = replace_nonreduced_by_main p (PMRS.reduce p.orig t) in
  let nr = nonreduced_terms p t' in
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
    in
    cartesian_nary_product expansions
  in
  let all_ts = List.map substs ~f:(fun s -> substitution s t) in
  (* Log.debug_msg "Terms after expansions:";
     List.iter all_ts ~f:(fun t -> Log.debug_msg Fmt.(str "@[<hov 2>%a@]" pp_term t)); *)
  let mr_terms, rest =
    List.partition_tf (t :: all_ts)
      ~f:(fun t ->
          let tr = replace_nonreduced_by_main p (PMRS.reduce p.orig t) in
          match nonreduced_terms p tr with
            [] -> true | _ -> false)
  in
  (* Expand and replace in term *)
  TermSet.of_list mr_terms, TermSet.of_list rest