open Common.ProblemDefs
open Base
open Lang
open Lang.Term

(** Flatten a list of terms that may be tuples into a flat list with no tuples.  *)
let rec simple_flattening (tl : term list) =
  List.concat_map
    ~f:(fun t ->
      match t.tkind with
      | TTup tl -> simple_flattening tl
      | _ -> [ t ])
    tl
;;

(* ============================================================================================= *)
(*                        PROJECTION : OPTIMIZATION FOR TUPLES                                   *)
(* ============================================================================================= *)

(** Construct the projection functions for the given variable.
  The character '$' is used to build projections and should not be used anywhere else.
  (It is compatible with usage in smtlib but not in the ocaml program.)
*)
let mk_projs
    ~(ctx : Context.t)
    (targs : RType.t list)
    (tl : RType.t list)
    (xi : Variable.t)
  =
  let f i t =
    match targs with
    | [] -> Variable.mk ctx ~t:(Some t) (xi.vname ^ "$" ^ Int.to_string i)
    | _ ->
      Variable.mk
        ctx
        ~t:(Some (RType.fun_typ_pack targs t))
        (xi.vname ^ "$" ^ Int.to_string i)
  in
  List.mapi ~f tl
;;

(**
  [apply_projections projection_map t] replaces in [t] all the variables whose id appears
  in the projection map by the associated tuple of variables.
  Does not reduce the term after applying the projections.
*)
let apply_projections
    ~(ctx : Context.t)
    (projs : (int, variable list, Int.comparator_witness) Map.t)
    (t : term)
    : term
  =
  let pack xlist args =
    List.map xlist ~f:(fun newx -> fst (infer_type ctx (mk_app (mk_var ctx newx) args)))
  in
  let case f t0 =
    match t0.tkind with
    | TApp ({ tkind = TVar x; _ }, args) ->
      (match Map.find projs x.vid with
      | Some xlist ->
        let args' = List.map ~f args in
        let projected = pack xlist args' in
        Some (mk_tup ctx projected)
      | None -> None)
    | TVar x ->
      (match Map.find projs x.vid with
      | Some components -> Some (mk_tup ctx (List.map ~f:(mk_var ctx) components))
      | None -> None)
    | _ -> None
  in
  transform ~case t
;;

(** [projection_eqns lhs rhs] returns a list of equations that corresponds to equating
    every components of [lhs] and [rhs]. If [rhs] and [lhs] are tuples of the same length n,
    then a list of equations of length n is returned. Otherwise [[(lhs,rhs)]] is returned.
  *)
let projection_eqns (lhs : term) (rhs : term) =
  match lhs.tkind, rhs.tkind with
  | TTup lhs_tl, TTup rhs_tl ->
    List.map ~f:(fun (r, l) -> r, l) (List.zip_exn lhs_tl rhs_tl)
  | _ -> [ lhs, rhs ]
;;

(** [invar invariants lhs_e rhs_e] filters the terms in `invariants` that have no free variable
  in common with  [lhs_e] or [rhs_e] and return the conjunction of all these invariants.
  *)
let invar ~(ctx : Context.t) invariants lhs_e rhs_e =
  let f inv_expr =
    not
      Analysis.(
        Set.is_empty
          (Set.inter
             (Set.union (free_variables ~ctx lhs_e) (free_variables ~ctx rhs_e))
             (free_variables ~ctx inv_expr)))
  in
  let conjs = List.filter ~f (Set.elements invariants) in
  mk_assoc Binop.And conjs
;;

let proj_and_detuple_eqns
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (projections : (int, variable list, Int.comparator_witness) Map.t)
    (eqns : equation list)
  =
  let apply_p = apply_projections projections in
  let f eqn =
    let lhs' = Reduce.reduce_term ~ctx ~fctx (apply_p ~ctx eqn.elhs)
    and rhs' = Reduce.reduce_term ~ctx ~fctx (apply_p ~ctx eqn.erhs) in
    let eqs = projection_eqns lhs' rhs' in
    List.map ~f:(fun (_l, _r) -> { eqn with elhs = _l; erhs = _r }) eqs
  in
  List.concat (List.map ~f eqns)
;;

(** [proj_functions functions] creates a map from variables in [functions]
  to tuple of new functions that represent each component of the original
  function, when the return type of that function is a tuple.
  If the return type of the function is not a tuple, then it is left unchanged.
  Use [apply_projections] to apply the projection map to a term.
  @returns a tuple containing the set of new functions created and a map from
  non-projected functions to the list of functions corrresponding to the projections.
 *)
let proj_functions ~(ctx : Context.t) (functions : VarSet.t)
    : VarSet.t * (int, variable list, Int.comparator_witness) Map.t
  =
  let function_projections, new_functions =
    let f (l, vs) xi =
      match Variable.vtype_or_new ctx xi with
      | RType.TFun _ ->
        let targs, tout = RType.fun_typ_unpack (Variable.vtype_or_new ctx xi) in
        (match tout with
        | TTup tl ->
          let new_vs = mk_projs ~ctx targs tl xi in
          l @ [ xi, Some new_vs ], Set.union vs (VarSet.of_list new_vs)
        | _ -> l @ [ xi, None ], Set.add vs xi)
      | RType.TTup tl ->
        let new_vs = mk_projs ~ctx [] tl xi in
        l @ [ xi, Some new_vs ], Set.union vs (VarSet.of_list new_vs)
      | _ -> l @ [ xi, None ], Set.add vs xi
    in
    List.fold ~f ~init:([], VarSet.empty) (Set.elements functions)
  in
  let proj_map =
    let mmap =
      Map.of_alist
        (module Int)
        (List.map
           ~f:(fun (x, p) -> x.vid, p)
           (* Only select relevant xi for projection *)
           (List.filter_map
              ~f:(fun (_x, _o) ->
                match _o with
                | Some o -> Some (_x, o)
                | None -> None)
              function_projections))
    in
    match mmap with
    | `Ok x -> x
    | `Duplicate_key _ -> failwith "Unexpected error while projecting."
  in
  new_functions, proj_map
;;
