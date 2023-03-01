open Analysis
open Base
open Term
open Utils

(* ============================================================================================= *)
(*                                  TERM MATCHING                                                *)
(* ============================================================================================= *)

let unify (terms : term list) : term option =
  match terms with
  | [] -> None
  | hd :: tl -> if List.for_all ~f:(fun x -> Terms.equal x hd) tl then Some hd else None
;;

let is_simple_subterm ~(sub : term) (t : term) =
  let case _ t' = if Terms.equal sub t' then Some true else None in
  reduce ~init:false ~join:( || ) ~case t
;;

(**
   [matches ~boundvars ~pattern t] returns a map from variables in [pattern] to subterms of [t]
   if the term [t] matches the [pattern].
   During the matching process the variables in [boundvars] cannot be substituted in the
   pattern.
*)
let matches ?(boundvars = VarSet.empty) ~(ctx : Context.t) (t : term) ~(pattern : term)
    : term VarMap.t option
  =
  let rec aux pat t =
    match pat.tkind, t.tkind with
    | TVar vpat, _ ->
      if Set.mem boundvars vpat
      then if Terms.equal pat t then Ok [] else Error [ pat, t ]
      else (
        let pat_ty = Variable.vtype_or_new ctx vpat in
        match RType.unify_one pat_ty t.ttyp with
        | Ok _ -> Ok [ vpat, t ]
        | Error _ -> Error [ pat, t ])
    | TData (cstr, mems), TData (cstr', mems') ->
      if String.equal cstr cstr' && List.length mems = List.length mems'
      then (
        match List.map2 ~f:aux mems mems' with
        | List.Or_unequal_lengths.Unequal_lengths -> Error [ pat, t ]
        | List.Or_unequal_lengths.Ok ls ->
          (match Result.combine_errors ls with
          | Ok subs -> Ok (List.concat subs)
          | Error errs -> Error (List.concat errs)))
      else Error [ pat, t ]
    | TTup mems, TTup mems' ->
      if List.length mems = List.length mems'
      then (
        match List.map2 ~f:aux mems mems' with
        | List.Or_unequal_lengths.Unequal_lengths -> Error [ pat, t ]
        | List.Or_unequal_lengths.Ok ls ->
          (match Result.combine_errors ls with
          | Ok subs -> Ok (List.concat subs)
          | Error errs -> Error (List.concat errs)))
      else Error [ pat, t ]
    | TApp (pat_f, pat_args), TApp (t_f, t_args) ->
      (match List.map2 ~f:aux pat_args t_args with
      | Unequal_lengths -> Error [ pat, t ]
      | Ok arg_match ->
        let f_match = aux pat_f t_f in
        (match Result.combine_errors (f_match :: arg_match) with
        | Ok subs -> Ok (List.concat subs)
        | Error errs -> Error (List.concat errs)))
    | TBin (pat_op, pat1, pat2), TBin (e_op, e1, e2) ->
      if Binop.equal pat_op e_op
      then (
        match aux pat1 e1, aux pat2 e2 with
        | Ok subs, Ok subs' -> Ok (subs @ subs')
        | Error e, _ -> Error e
        | _, Error e -> Error e)
      else Error []
    | TUn (pat_op, pat1), TUn (e_op, e1) ->
      if Unop.equal pat_op e_op then aux pat1 e1 else Error []
    | _ -> if Terms.equal pat t then Ok [] else Error []
  in
  match aux pattern t with
  | Error _ -> None
  | Ok substs ->
    let im = VarMap.empty in
    let f accum (var, t) = Map.add_multi ~key:var ~data:t accum in
    (try
       let substs =
         Map.filter_map
           ~f:(fun bindings ->
             match bindings with
             | [] -> None
             | _ ->
               (match unify bindings with
               | Some e -> Some e
               | _ -> failwith "X"))
           (List.fold ~init:im ~f substs)
       in
       Some substs
     with
    | _ -> None)
;;

let alpha_equal
    ?(boundvars = VarSet.empty)
    ~(ctx : Context.t)
    (t : term)
    ~(pattern : term)
    : variable VarMap.t option
  =
  match matches ~boundvars ~ctx t ~pattern with
  | Some term_map ->
    let var_map =
      Map.filter_map term_map ~f:(fun t ->
          match t.tkind with
          | TVar v -> Some v
          | _ -> None)
    in
    if Map.length var_map = Map.length term_map then Some var_map else None
  | None -> None
;;

(** [matches_subpattern ~boundvars t ~pattern] returns some triple [t', subs, vs]
    when some sub-pattern of [pattern] matches [t].
*)
let matches_subpattern
    ?(boundvars = VarSet.empty)
    ~(ctx : Context.t)
    (t : term)
    ~(pattern : term)
    : (term * (term * term) list * VarSet.t) option
  =
  let match_locations = ref [] in
  let rrule (pat : term) =
    match matches ~ctx ~boundvars t ~pattern:pat with
    | Some substmap ->
      let match_loc_place =
        Variable.mk ctx ~t:(Some pat.ttyp) (Alpha.fresh ctx.names ~s:"*_")
      in
      match_locations := (match_loc_place, substmap) :: !match_locations;
      Some { pat with tkind = TVar match_loc_place }
    | None -> None
  in
  let pat_skel = rewrite_top_down rrule pattern in
  let unified_map, loc_set =
    let merger ~key:_ v =
      match v with
      | `Left binding -> Some binding
      | `Right binding -> Some binding
      | `Both (b1, b2) -> unify [ b1; b2 ]
    in
    List.fold
      ~init:(Map.empty (module Variable), VarSet.empty)
      ~f:(fun (subst_map, loc_set) (loc, loc_substs) ->
        Map.merge ~f:merger subst_map loc_substs, Set.add loc_set loc)
      !match_locations
  in
  let substs = List.map ~f:(fun (a, b) -> mk_var ctx a, b) (Map.to_alist unified_map) in
  if Set.is_empty loc_set then None else Some (pat_skel, substs, loc_set)
;;

(** [matches_pattern t p] returns Some map if [t] matches [p], where the map binds the variables of
  the pattern to the relevant subterms of [t].
  If [t] does not match the pattern [p] then None is returned.
*)
let matches_pattern (t : term) (p : pattern) : term VarMap.t option =
  let combine ~key:_ = function
    | `Both (v1, v2) -> if Terms.equal v1 v2 then Some v1 else None
    | `Right v1 | `Left v1 -> Some v1
  in
  let rec aux (p, t) =
    match p with
    | PatAny ->
      (* Any matches anything, and no binding is created. *)
      Some VarMap.empty
    | PatVar x ->
      (* A variable matches anything, but we add a binding.  *)
      Some (VarMap.singleton x t)
    | PatConstant c ->
      (* A constant must be matched by the same constant. *)
      (match t.tkind with
      | TConst c' -> if Constant.equal c c' then Some VarMap.empty else None
      | _ -> None)
    | PatConstr (c, args) ->
      (* A constructor is matched by a constructor, and all argument must match.
           A variable can be bound several times, but in that cases it must bind
           to the same term.
        *)
      (match t.tkind with
      | TData (c', args') when String.equal c c' ->
        (match List.zip args args' with
        | Unequal_lengths -> None
        | Ok pairs ->
          Option.map
            (all_or_none (List.map ~f:aux pairs))
            ~f:(fun maps ->
              match maps with
              | [] -> VarMap.empty
              | init :: tl -> List.fold ~f:(Map.merge ~f:combine) ~init tl))
      | _ -> None)
    | PatTuple args ->
      (* Matching a tuple is almost the same as matching a constructor.*)
      (match t.tkind with
      | TTup args' ->
        (match List.zip args args' with
        | Unequal_lengths -> None
        | Ok pairs ->
          Option.map
            (all_or_none (List.map ~f:aux pairs))
            ~f:(fun maps ->
              match maps with
              | [] -> VarMap.empty
              | init :: tl -> List.fold ~f:(Map.merge ~f:combine) ~init tl))
      | _ -> None)
  in
  aux (p, t)
;;

(**
  [skeletize ~functions t] returns the term t where each subterm that does not
  contain a call to the functions in [functions] has been reblaced by a box containing
  a fresh variable.
  Returns a pair of a boxed var-substitution list and the term with the boxes replacing the
  subexpressions.
*)
let skeletize ~(functions : VarSet.t) ~(ctx : Context.t) (t : term) : term VarMap.t * term
  =
  let skel_id = ref 0 in
  let boxes = ref (Map.empty (module Terms)) in
  let case _ t0 =
    if has_calls ~ctx ~_to:functions t0
    then None
    else (
      let boxv =
        match Map.find !boxes t0 with
        | Some boxv -> boxv
        | None ->
          let boxv = Variable.mk ctx ~t:(Some t0.ttyp) (Fmt.str "?%i" !skel_id) in
          Int.incr skel_id;
          boxes := Map.set !boxes ~key:t0 ~data:boxv;
          boxv
      in
      Some (mk_var ctx boxv))
  in
  let boxed_ts = transform ~case t in
  let varmap =
    Map.fold !boxes ~init:VarMap.empty ~f:(fun ~key ~data m ->
        Map.set m ~key:data ~data:key)
  in
  varmap, boxed_ts
;;
