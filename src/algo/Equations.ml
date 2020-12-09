open Base
open Lang
open Lang.Term


let identify_rcalls (lam : variable) (t : term) =
  let join = Set.union in
  let case _ t =
    match t.tkind with
    (* lam x *)
    | TApp({tkind = TVar lam';_}, [{tkind = TVar x; _}]) when Variable.(lam' = lam) ->
      Some (VarSet.singleton x)
    | _ -> None
  in
  reduce ~init:VarSet.empty ~case ~join t



let make_equations ~(f : PMRS.t) ~(g : PMRS.t) ~(r : PMRS.t) (t : Terms.t) =
  (* Replace recursive calls to r(x) *)
  let r_x = identify_rcalls r.pmain_symb t in
  let r_x_subs =
    List.map ~f:(fun x -> x, Variable.mk ~t:(Some !AState.tau) (Alpha.fresh x.vname))
      (Set.elements r_x)
  in
  let t' =
    substitution
      (List.map ~f:(fun (x, y) -> mk_app (mk_var r.pmain_symb) [mk_var x], mk_var y) r_x_subs)
      t
  in
  let _ = identify_rcalls f.pmain_symb t' in
  let _ = identify_rcalls g.pmain_symb t' in
  ()


