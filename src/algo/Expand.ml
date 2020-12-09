open Base
open Lang
open Lang.Term
open AState



let maximal (p : psi_def) (t : term) : TermSet.t * TermSet .t =
  let p_orig = p.orig in
  let f_init_rules =
    Map.filter ~f:(fun (nt, _, _, _) -> Variable.(nt = p_orig.pmain_symb)) p.orig.prules
  in
  let nonreduced_terms =
    let join = (@) in
    let case _ t =
      match t.tkind with
      | TApp(func, args) ->
        (match func.tkind with
         | TVar func_var ->
           (if Set.mem p.orig.PMRS.pnon_terminals func_var then
              Some [func_var, args] else None)
         | _ -> None)
      | _ -> None
    in
    let init = [] in
    reduce ~init ~join ~case t
  in
  let _ =
    let f (nt, args) =
      match t.tkind with
      |
    in
  in
  Fmt.(pf stdout "%a@." (list ~sep:comma pp_term) (Set.to_list nonreduced_terms));
  TermSet.singleton t, TermSet.empty