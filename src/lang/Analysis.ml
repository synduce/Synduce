open Base
open Term
open Utils

let free_variables (t : term) : VarSet.t =
  let rec f t =
    match t.tkind with
    | TBin (_, t1, t2) -> Set.union (f t1) (f t2)
    | TUn (_, t1) -> f t1
    | TConst _ -> VarSet.empty
    | TVar v -> VarSet.singleton v
    | TIte (c, t1, t2) -> VarSet.union_list [f c; f t1; f t2]
    | TTup tl -> VarSet.union_list (List.map ~f tl)
    | TSel (t, _) -> f t
    | TFun (vl, t1) -> let v1 = f t1 in Set.diff v1 (fpat_vars (PatTup vl))
    | TApp (ft, targs) -> Set.union (f ft) (VarSet.union_list (List.map ~f targs))
    | TData (_, mems) -> VarSet.union_list (List.map ~f mems)
  in
  f t

let has_ite (t : term) : bool =
  reduce t
    ~init:false ~join:(||)
    ~case:(fun _ t -> match t.tkind with TIte _ -> Some true | _ -> None)


let subst_args fpatterns args =
  let rec f (fpat, t) =
    match fpat, t.tkind with
    | PatVar x, _ -> [mk_var x, t]
    | PatTup pl, TTup tl ->
      (match List.zip pl tl with
       | Ok ptl -> List.concat (List.map ~f ptl)
       | _ -> failwith "no sub")
    | _ -> failwith "no sub"

  in
  match List.zip fpatterns args with
  | Ok l -> (try Some (List.concat (List.map ~f l)) with _ -> None)
  | _ -> None


let replace_calls_to (funcs : VarSet.t) =
  let case _ t =
    match t.tkind with
    | TApp({tkind = TVar x; _}, _) when Set.mem funcs x ->
      Some (mk_var (Variable.mk ~t:(Some t.ttyp) (Alpha.fresh x.vname)))
    | _ -> None
  in
  transform ~case


let apply_projections (projs : (int, variable list, Int.comparator_witness) Map.t) (t : term) =
  let pack xlist args =
    List.map xlist
      ~f:(fun newx -> first (infer_type (mk_app (mk_var newx) args)))
  in
  let case f t0 =
    match t0.tkind with
    | TApp({tkind=TVar(x); _}, args) ->
      (match Map.find projs x.vid with
       | Some xlist ->
         let args' = List.map ~f args in
         let projected = pack xlist args' in
         Some (mk_tup projected)
       | None -> None)

    | _ -> None
  in transform ~case t


(* ============================================================================================= *)
(*                                  TERM MATCHING                                                *)
(* ============================================================================================= *)

let unify (terms : term list) =
  match terms with
  | [] -> None
  | hd :: tl ->
    if List.for_all ~f:(fun x -> Terms.equal x hd) tl then Some hd else None

(**cat /tmt
   matches ~boundvars ~pattern t returns a map from variables in pattern to subterms of t
   if the term t matches the pattern.
   During the matching process the variables in boundvars cannot be substituted in the
   pattern.
*)
let matches ?(boundvars = VarSet.empty) (t : term) ~(pattern : term) =
  let rec aux pat t = match pat.tkind, t.tkind with
    | TVar vpat, _ ->
      if Set.mem boundvars vpat then
        if Terms.equal pat t then Ok [] else Error [pat,t]
      else
        let pat_ty = Variable.vtype_or_new vpat in
        (match RType.unify_one pat_ty t.ttyp with
         | Some _ -> Ok [vpat, t]
         | _ -> Error[pat,t])

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
    | _ -> if Terms.equal pat t then Ok [] else Error []
  in
  match aux pattern t with
  | Error _ -> None
  | Ok substs ->
    let im = Map.empty (module Variable) in
    let f accum (var, t) = Map.add_multi ~key:var ~data:t accum in
    try
      let substs =
        Map.filter_map
          ~f:(fun bindings ->
              match bindings with
              | [] -> None
              | _ -> match unify bindings with Some e -> Some e | _ -> failwith "X")
          (List.fold ~init:im ~f substs)
      in Some substs
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
  (* Pick variables to expand on *)
  let expandable =
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
        List.map  cstr_arg_types
          ~f:(fun ty -> mk_var (Variable.mk ~t:(Some ty) (Alpha.fresh "new")))
      in
      let t, _ =
        infer_type
          (substitution [mk_var v, mk_data cstr_name cstr_args] t)
      in t
    in
    List.map ~f v_expan
  in
  List.rev (List.concat (List.map ~f:aux expandable))


(**
  Given a list of variants, returns a list of terms that corresponds to the
  non-recursive constructors, with their arguments instantiated as variables.
*)
let variant_no_recurse (variants : (string * RType.t list) list) : term list =
  let inst_or_none t =
    match RType.get_variants t with
    | [] -> Some (mk_var (Variable.mk ~t:(Some t) (Alpha.fresh "l")))
    | _ -> None
  in
  let f (variant, targs) =
    match targs with
    | [] -> Some (mk_data variant [])
    | targs ->
      let l = List.map ~f:inst_or_none targs in
      if List.for_all ~f:Option.is_some l then
        Some (mk_data variant (List.filter_opt l))
      else None
  in
  List.filter_opt (List.map ~f variants)



let terms_of_max_depth (depth : int) (typ : RType.t) : term list =
  let rec constr_t d _typ : term list =
    match RType.get_variants _typ with
    | [] -> [mk_var (Variable.mk ~t:(Some _typ) (Alpha.fresh "a"))]
    | l ->
      if d >= depth then variant_no_recurse l
      else List.concat (List.map ~f:(constr_variant d) l)
  and constr_variant d (cname, cargs) : term list =
    match cargs with
    | [] -> [mk_data cname []]
    | _ ->
      let subterms =
        List.map ~f:(constr_t (d + 1)) cargs
      in
      let choices =
        List.map (Utils.cartesian_nary_product subterms)
          ~f:(fun cargs -> mk_data cname (List.rev cargs))
      in
      choices
  in
  constr_t 0 typ


(* ============================================================================================= *)
(*                                  TERM CONCRETIZATION                                          *)
(* ============================================================================================= *)

(** Arbitrary min and max values. Actual useful values will be generated by solvers anyway. *)
let _INT_MAX = 124
let _INT_MIN = -124

let rec rand_const_of_type (typ : RType.t) : term =
  RType.(match typ with
  | TInt -> mk_const (Constant.CInt ((Random.int (_INT_MAX - _INT_MIN)) + _INT_MIN))
  | TBool -> mk_const (if Random.bool () then Constant.CTrue else Constant.CFalse)
  | TChar -> failwith "Char unsupported."
  | TString -> failwith "String unsupported."
  | _ ->
    match get_variants typ with
    | [] -> failwith Fmt.(str "Unsupported value type %a" pp typ)
    | l ->
      match variant_no_recurse l with
      | [] -> failwith Fmt.(str "Unsupported value type %a (no non-recursive constructor)" pp typ)
      | hd :: _ -> concretize hd)

and concretize ?(model = Map.empty (module String) ) (t : term) : term =
    let case _ tin =
      let tk = tin.tkind in
      match tk with
      | TVar x ->
        (match Map.find model x.vname with
        | Some t -> Some t
        | None -> Some (rand_const_of_type (Variable.vtype_or_new x)))
      | _ -> None
    in
    transform ~case t