open AState
open Base
open Lang
open Lang.Term

(* ============================================================================================= *)
(*                        PROJECTION : OPTIMIZATION FOR TUPLES                                   *)
(* ============================================================================================= *)

let mk_projs (targs : RType.t list) (tl : RType.t list) (xi : Variable.t) =
  let f i t =
    match targs with
    | [] -> Variable.mk ~t:(Some t) (xi.vname ^ Int.to_string i)
    | _ -> Variable.mk ~t:(Some (RType.fun_typ_pack targs t)) (xi.vname ^ Int.to_string i)
  in
  List.mapi ~f tl

let projection_eqns (lhs : term) (rhs : term) =
  match (lhs.tkind, rhs.tkind) with
  | TTup lhs_tl, TTup rhs_tl -> List.map ~f:(fun (r, l) -> (r, l)) (List.zip_exn lhs_tl rhs_tl)
  | _ -> [ (lhs, rhs) ]

(** [invar invariants lhs_e rhs_e] filters the terms in `invariants` that have no free variable
  in common with  [lhs_e] or [rhs_e] and return the conjuction of all these invariants.
  *)
let invar invariants lhs_e rhs_e =
  let f inv_expr =
    not
      (Set.is_empty
         (Set.inter
            (Set.union (Analysis.free_variables lhs_e) (Analysis.free_variables rhs_e))
            (Analysis.free_variables inv_expr)))
  in
  let conjs = List.filter ~f (Set.elements invariants) in
  mk_assoc Binop.And conjs

let proj_and_detuple_eqns (projections : (int, variable list, Int.comparator_witness) Map.t)
    (eqns : equation list) =
  let apply_p = Analysis.apply_projections projections in
  let f eqn =
    let lhs' = apply_p eqn.elhs and rhs' = apply_p eqn.erhs in
    let eqs = projection_eqns lhs' rhs' in
    List.map ~f:(fun (_l, _r) -> { eqn with elhs = _l; erhs = _r }) eqs
  in
  List.concat (List.map ~f eqns)

let proj_unknowns (unknowns : VarSet.t) =
  let unknowns_projs, new_unknowns =
    let f (l, vs) xi =
      match Variable.vtype_or_new xi with
      | RType.TFun _ -> (
          let targs, tout = RType.fun_typ_unpack (Variable.vtype_or_new xi) in
          match tout with
          | TTup tl ->
              let new_vs = mk_projs targs tl xi in
              (l @ [ (xi, Some new_vs) ], Set.union vs (VarSet.of_list new_vs))
          | _ -> (l @ [ (xi, None) ], Set.add vs xi))
      | RType.TTup tl ->
          let new_vs = mk_projs [] tl xi in
          (l @ [ (xi, Some new_vs) ], Set.union vs (VarSet.of_list new_vs))
      | _ -> (l @ [ (xi, None) ], Set.add vs xi)
    in
    List.fold ~f ~init:([], VarSet.empty) (Set.elements unknowns)
  in
  let proj_map =
    let mmap =
      Map.of_alist
        (module Int)
        (List.map
           ~f:(fun (x, p) -> (x.vid, p))
           (* Only select relevant xi for projection *)
           (List.filter_map
              ~f:(fun (_x, _o) -> match _o with Some o -> Some (_x, o) | None -> None)
              unknowns_projs))
    in
    match mmap with `Ok x -> x | `Duplicate_key _ -> failwith "Unexpected error while projecting."
  in
  (new_unknowns, proj_map)
