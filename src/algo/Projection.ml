open AState
open Base
open Lang
open Lang.Term

(* ============================================================================================= *)
(*                        PROJECTION : OPTIMIZATION FOR TUPLES                                   *)
(* ============================================================================================= *)

(** Construct the projection functions for the given variable.
*)
let mk_projs (targs : RType.t list) (tl : RType.t list) (xi : Variable.t) =
  let f i t =
    match targs with
    | [] -> Variable.mk ~t:(Some t) (xi.vname ^ Int.to_string i)
    | _ -> Variable.mk ~t:(Some (RType.fun_typ_pack targs t)) (xi.vname ^ Int.to_string i)
  in
  List.mapi ~f tl

(**
  [apply_projections projection_map t] replaces in [t] all the variables whose id appears
  in the projection map by the associated tuple of variables.
  Does not reduce the term after applying the projections.
*)
let apply_projections (projs : (int, variable list, Int.comparator_witness) Map.t) (t : term) : term
    =
  let pack xlist args =
    List.map xlist ~f:(fun newx -> fst (infer_type (mk_app (mk_var newx) args)))
  in
  let case f t0 =
    match t0.tkind with
    | TApp ({ tkind = TVar x; _ }, args) -> (
        match Map.find projs x.vid with
        | Some xlist ->
            let args' = List.map ~f args in
            let projected = pack xlist args' in
            Some (mk_tup projected)
        | None -> None)
    | TVar x -> (
        match Map.find projs x.vid with
        | Some components -> Some (mk_tup (List.map ~f:mk_var components))
        | None -> None)
    | _ -> None
  in
  transform ~case t

(** [projection_eqns lhs rhs] returns a list of equations that corresponds to equating
    every components of [lhs] and [rhs]. If [rhs] and [lhs] are tuples of the same length n,
    then a list of equations of length n is returned. Otherwise [[(lhs,rhs)]] is returned.
  *)
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
      Analysis.(
        Set.is_empty
          (Set.inter
             (Set.union (free_variables lhs_e) (free_variables rhs_e))
             (free_variables inv_expr)))
  in
  let conjs = List.filter ~f (Set.elements invariants) in
  mk_assoc Binop.And conjs

let proj_and_detuple_eqns (projections : (int, variable list, Int.comparator_witness) Map.t)
    (eqns : equation list) =
  let apply_p = apply_projections projections in
  let f eqn =
    let lhs' = Reduce.reduce_term (apply_p eqn.elhs)
    and rhs' = Reduce.reduce_term (apply_p eqn.erhs) in
    let eqs = projection_eqns lhs' rhs' in
    List.map ~f:(fun (_l, _r) -> { eqn with elhs = _l; erhs = _r }) eqs
  in
  List.concat (List.map ~f eqns)

(** [proj_functions functions] creates a map from variables in [functions]
  to tuple of new functions that represent each component of the original
  function, when the return type of that function is a tuple.
  If the return type of the function is not a tuple, then it is left unchanged.
  Use [apply_projections] to apply the projection map to a term.
  @returns a tuple containing the set of new functions created and a map from
  non-projected functions to the list of functions corrresponding to the projections.
 *)
let proj_functions (functions : VarSet.t) :
    VarSet.t * (int, variable list, Int.comparator_witness) Map.t =
  let function_projections, new_functions =
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
    List.fold ~f ~init:([], VarSet.empty) (Set.elements functions)
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
              function_projections))
    in
    match mmap with `Ok x -> x | `Duplicate_key _ -> failwith "Unexpected error while projecting."
  in
  (new_functions, proj_map)
