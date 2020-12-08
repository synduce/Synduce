open Base
open Term

let free_variables (t : term) : VarSet.t =
  let rec f t =
    match t.tkind with
    | TBin (_, t1, t2) -> Set.union (f t1) (f t2)
    | TUn (_, t1) -> f t1
    | TConst _ -> VarSet.empty
    | TVar v -> VarSet.singleton v
    | TIte (c, t1, t2) -> VarSet.union_list [f c; f t1; f t2]
    | TTup tl -> VarSet.union_list (List.map ~f tl)
    | TFun (vl, t1) -> let v1 = f t1 in Set.diff v1 (VarSet.of_list vl)
    | TApp (ft, targs) -> Set.union (f ft) (VarSet.union_list (List.map ~f targs))
    | TData (_, mems) -> VarSet.union_list (List.map ~f mems)
  in
  f t



(* ============================================================================================= *)
(*                                  TERM MATCHING                                                *)
(* ============================================================================================= *)

let unify (terms : term list) =
  match terms with
  | [] -> None
  | hd :: tl ->
    if List.for_all ~f:(fun x -> Terms.equal x hd) tl then Some hd else None

let matches ?(boundvars = VarSet.empty) (t : term) ~(pattern : term) =
  let rec aux pat t = match pat.tkind, t.tkind with
    | TVar vpat, _ ->
      if Set.mem boundvars vpat then
        if Terms.equal pat t then Ok [] else Error [pat,t]
      else
        let pat_ty = Variable.vtype_or_new vpat in
        if Poly.equal pat_ty t.ttyp then Ok [vpat, t] else Error[pat,t]

    | TData (cstr, mems), TData(cstr', mems') ->
      if String.equal cstr cstr' && List.length mems = List.length mems' then
        (match List.map2 ~f:aux mems mems' with
         | List.Or_unequal_lengths.Unequal_lengths -> Error [(pat,t)]
         | List.Or_unequal_lengths.Ok ls ->
           (match Result.combine_errors ls with
            | Ok subs -> Ok (List.concat subs)
            | Error errs -> Error (List.concat errs) ))
      else Error [pat,t]
    | TApp(pat_f, pat_args), TApp(t_f, t_args) ->
      (match List.map2 ~f:aux pat_args t_args with
       | Unequal_lengths -> Error [pat,t]
       | Ok arg_match ->
         let f_match = aux pat_f t_f in
         (match Result.combine_errors (f_match :: arg_match) with
          | Ok subs -> Ok (List.concat subs)
          | Error errs -> Error (List.concat errs)))
    | _ -> Error []
  in
  match aux pattern t with
  | Error _ -> None (* TODO: print error message *)
  | Ok substs ->
    let im = Map.empty (module Variable) in
    let f accum (var, t) = Map.add_multi ~key:var ~data:t accum in
    try
      Some
        (Map.map
           ~f:(fun bindings -> match unify bindings with Some e -> e | _ -> failwith "X")
           (List.fold ~init:im ~f substs))
    with _ -> None

let matches_subpattern ?(boundvars = VarSet.empty) (t : term) ~(pattern : term) =
  let match_locations = ref [] in
  let rrule (pat : term) =
    match matches ~boundvars t ~pattern:pat with
    | Some substmap ->
      let match_loc_place = Variable.mk ~t:(Some pat.ttyp) (Alpha.fresh "*") in
      match_locations := (match_loc_place, substmap) :: !match_locations;
      Some {pat with tkind = TVar match_loc_place; }
    | None -> None
  in
  let pat_skel = rewrite_top_down rrule pattern in
  let unified_map, loc_set =
    let merger ~key:_ v =
      match v with
      | `Left binding -> Some binding
      | `Right binding -> Some binding
      | `Both (b1, b2) -> unify [b1;b2]
    in
    List.fold ~init:(Map.empty (module Variable), VarSet.empty)
      ~f:(fun (subst_map, loc_set) (loc, loc_substs) ->
          Map.merge ~f:merger subst_map loc_substs, Set.add loc_set loc)
      !match_locations
  in
  let substs = List.map ~f:(fun (a,b) -> (mk_var a, b)) (Map.to_alist unified_map) in
  if Set.is_empty loc_set then
    None
  else
    Some (pat_skel, substs, loc_set)



(* ============================================================================================= *)
(*                                  TERM EXPANSION                                               *)
(* ============================================================================================= *)

let expand_once (t : term) : term list =
  let fvt = free_variables t in
  (* Pick a variable to expand on *)
  let expansions =
    let filter v =
      match RType.get_variants (Variable.vtype_or_new v) with
      | [] -> None
      | l -> Some (v, l)
    in
    List.filter_map (Set.elements fvt)  ~f:filter
  in
  let aux (v, v_expan) =
    let f (cstr_name, cstr_arg_types) =
      let cstr_args =
        List.map ~f:(fun ty -> mk_var (Variable.mk ~t:(Some ty) (Alpha.fresh v.vname))) cstr_arg_types
      in
      let t, _ =
        infer_type
          (substitution [mk_var v, mk_data cstr_name cstr_args] t)
      in t
    in
    List.map ~f v_expan
  in
  List.concat (List.map ~f:aux expansions)