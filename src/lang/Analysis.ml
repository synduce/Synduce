open Base
open Term
open Utils

(* ============================================================================================= *)
(*                                  UTILITIES                                                    *)
(* ============================================================================================= *)
let rec vars_of_pattern (p : pattern) : VarSet.t =
  match p with
  | PatAny | PatConstant _ -> VarSet.empty
  | PatVar v -> VarSet.singleton v
  | PatConstr (_, tl) | PatTuple tl -> VarSet.union_list (List.map ~f:vars_of_pattern tl)

let free_variables ?(include_functions = true) (t : term) : VarSet.t =
  let rec f t =
    match t.tkind with
    | TBox t -> f t
    | TBin (_, t1, t2) -> Set.union (f t1) (f t2)
    | TUn (_, t1) -> f t1
    | TConst _ -> VarSet.empty
    | TVar v -> (
        if include_functions then VarSet.singleton v
        else
          match Variable.vtype_or_new v with
          | RType.TFun _ -> VarSet.empty
          | _ -> VarSet.singleton v)
    | TIte (c, t1, t2) -> VarSet.union_list [ f c; f t1; f t2 ]
    | TTup tl -> VarSet.union_list (List.map ~f tl)
    | TSel (t, _) -> f t
    | TFun (vl, t1) ->
        let v1 = f t1 in
        Set.diff v1 (fpat_vars (FPatTup vl))
    | TApp (ft, targs) -> Set.union (f ft) (VarSet.union_list (List.map ~f targs))
    | TData (_, mems) -> VarSet.union_list (List.map ~f mems)
    | TMatch (tm, cases) ->
        Set.union (f tm)
          (VarSet.union_list (List.map ~f:(fun (p, t) -> Set.diff (f t) (vars_of_pattern p)) cases))
  in
  f t

let has_ite (t : term) : bool =
  reduce t ~init:false ~join:( || ) ~case:(fun _ t ->
      match t.tkind with TIte _ -> Some true | _ -> None)

let has_calls ~(_to : VarSet.t) (t : term) : bool =
  not (Set.is_empty (Set.inter _to (free_variables t)))

let operators_of (t : term) : OpSet.t =
  let case f t =
    match t.tkind with
    | TBin (binop, t1, t2) -> Some (Set.add (Set.union (f t1) (f t2)) (Operator.Binary binop))
    | TUn (unop, t1) -> Some (Set.add (f t1) (Operator.Unary unop))
    | _ -> None
  in
  reduce ~init:OpSet.empty ~join:Set.union ~case t

let is_constant =
  let case _ t = match t.tkind with TConst _ -> Some true | _ -> None in
  reduce ~init:false ~join:( && ) ~case

let is_norec =
  let case _ t =
    match t.tkind with
    | TVar x -> Some (not (RType.is_recursive (Variable.vtype_or_new x)))
    | _ -> None
  in
  reduce ~init:true ~join:( && ) ~case

(** A term has no variant if it is a variable and the type of that
  variable has no variants. This means that term cannot be expanded.
*)
let is_novariant =
  let case _ t =
    match t.tkind with
    | TVar x -> Some (List.is_empty (RType.get_variants (Variable.vtype_or_new x)))
    | _ -> None
  in
  reduce ~init:true ~join:( && ) ~case

let is_bounded (t : term) =
  Term.reduce ~init:true ~join:( && )
    ~case:(fun _ t -> match t.tkind with TVar _ -> Some (is_novariant t) | _ -> None)
    t

let subst_args (fpatterns : fpattern list) (args : term list) : (term * term) list option =
  let rec f (fpat, t) =
    match (fpat, t.tkind) with
    | FPatVar x, _ -> [ (mk_var x, t) ]
    | FPatTup pl, TTup tl -> (
        match List.zip pl tl with Ok ptl -> List.concat (List.map ~f ptl) | _ -> failwith "no sub")
    | _ -> failwith "no sub"
  in

  match List.zip fpatterns args with
  | Ok l -> ( try Some (List.concat (List.map ~f l)) with _ -> None)
  | _ -> None

let replace_calls_to (funcs : VarSet.t) : term -> term =
  let case _ t =
    match t.tkind with
    | TApp ({ tkind = TVar x; _ }, _) when Set.mem funcs x ->
        Some (mk_var (Variable.mk ~t:(Some t.ttyp) (Alpha.fresh ~s:x.vname ())))
    | _ -> None
  in
  transform ~case

let apply_projections (projs : (int, variable list, Int.comparator_witness) Map.t) (t : term) : term
    =
  let pack xlist args =
    List.map xlist ~f:(fun newx -> first (infer_type (mk_app (mk_var newx) args)))
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

let replace_id_calls ~(func : variable) (t : term) : term =
  (* Collect the arguments of the calls (func x) *)
  let args_of_calls_to_func =
    let case _ t =
      match t.tkind with
      | TApp ({ tkind = TVar func'; _ }, [ x ]) when Variable.(func = func') ->
          Some (TermSet.singleton x)
      | _ -> None
    in
    reduce ~init:TermSet.empty ~join:Set.union ~case t
  in
  let subs =
    (* Replace all func x by just x *)
    List.map ~f:(fun x -> (mk_app (mk_var func) [ x ], x)) (Set.elements args_of_calls_to_func)
  in
  substitution subs t

(* ============================================================================================= *)
(*                                  TERM MATCHING                                                *)
(* ============================================================================================= *)

let unify (terms : term list) : term option =
  match terms with
  | [] -> None
  | hd :: tl -> if List.for_all ~f:(fun x -> Terms.equal x hd) tl then Some hd else None

(**
   [matches ~boundvars ~pattern t] returns a map from variables in [pattern] to subterms of [t]
   if the term [t] matches the [pattern].
   During the matching process the variables in [boundvars] cannot be substituted in the
   pattern.
*)
let matches ?(boundvars = VarSet.empty) (t : term) ~(pattern : term) : term VarMap.t option =
  let rec aux pat t =
    match (pat.tkind, t.tkind) with
    | TVar vpat, _ -> (
        if Set.mem boundvars vpat then if Terms.equal pat t then Ok [] else Error [ (pat, t) ]
        else
          let pat_ty = Variable.vtype_or_new vpat in
          match RType.unify_one pat_ty t.ttyp with
          | Ok _ -> Ok [ (vpat, t) ]
          | Error _ -> Error [ (pat, t) ])
    | TData (cstr, mems), TData (cstr', mems') ->
        if String.equal cstr cstr' && List.length mems = List.length mems' then
          match List.map2 ~f:aux mems mems' with
          | List.Or_unequal_lengths.Unequal_lengths -> Error [ (pat, t) ]
          | List.Or_unequal_lengths.Ok ls -> (
              match Result.combine_errors ls with
              | Ok subs -> Ok (List.concat subs)
              | Error errs -> Error (List.concat errs))
        else Error [ (pat, t) ]
    | TTup mems, TTup mems' ->
        if List.length mems = List.length mems' then
          match List.map2 ~f:aux mems mems' with
          | List.Or_unequal_lengths.Unequal_lengths -> Error [ (pat, t) ]
          | List.Or_unequal_lengths.Ok ls -> (
              match Result.combine_errors ls with
              | Ok subs -> Ok (List.concat subs)
              | Error errs -> Error (List.concat errs))
        else Error [ (pat, t) ]
    | TApp (pat_f, pat_args), TApp (t_f, t_args) -> (
        match List.map2 ~f:aux pat_args t_args with
        | Unequal_lengths -> Error [ (pat, t) ]
        | Ok arg_match -> (
            let f_match = aux pat_f t_f in
            match Result.combine_errors (f_match :: arg_match) with
            | Ok subs -> Ok (List.concat subs)
            | Error errs -> Error (List.concat errs)))
    | TBin (pat_op, pat1, pat2), TBin (e_op, e1, e2) ->
        if Binop.equal pat_op e_op then
          match (aux pat1 e1, aux pat2 e2) with
          | Ok subs, Ok subs' -> Ok (subs @ subs')
          | Error e, _ -> Error e
          | _, Error e -> Error e
        else Error []
    | TUn (pat_op, pat1), TUn (e_op, e1) -> if Unop.equal pat_op e_op then aux pat1 e1 else Error []
    | _ -> if Terms.equal pat t then Ok [] else Error []
  in
  match aux pattern t with
  | Error _ -> None
  | Ok substs -> (
      let im = VarMap.empty in
      let f accum (var, t) = Map.add_multi ~key:var ~data:t accum in
      try
        let substs =
          Map.filter_map
            ~f:(fun bindings ->
              match bindings with
              | [] -> None
              | _ -> ( match unify bindings with Some e -> Some e | _ -> failwith "X"))
            (List.fold ~init:im ~f substs)
        in
        Some substs
      with _ -> None)

(** [matches_subpattern ~boundvars t ~pattern] returns some triple [t', subs, vs]
    when some sub-pattern of [pattern] matches [t].
*)
let matches_subpattern ?(boundvars = VarSet.empty) (t : term) ~(pattern : term) :
    (term * (term * term) list * VarSet.t) option =
  let match_locations = ref [] in
  let rrule (pat : term) =
    match matches ~boundvars t ~pattern:pat with
    | Some substmap ->
        let match_loc_place = Variable.mk ~t:(Some pat.ttyp) (Alpha.fresh ~s:"*_" ()) in
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
        (Map.merge ~f:merger subst_map loc_substs, Set.add loc_set loc))
      !match_locations
  in
  let substs = List.map ~f:(fun (a, b) -> (mk_var a, b)) (Map.to_alist unified_map) in
  if Set.is_empty loc_set then None else Some (pat_skel, substs, loc_set)

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
    | PatConstant c -> (
        (* A constant must be matched by the same constant. *)
        match t.tkind with
        | TConst c' -> if Constant.equal c c' then Some VarMap.empty else None
        | _ -> None)
    | PatConstr (c, args) -> (
        (* A constructor is matched by a constructor, and all argument must match.
           A variable can be bound several times, but in that cases it must bind
           to the same term.
        *)
        match t.tkind with
        | TData (c', args') when String.equal c c' -> (
            match List.zip args args' with
            | Unequal_lengths -> None
            | Ok pairs ->
                Option.map
                  (all_or_none (List.map ~f:aux pairs))
                  ~f:(fun maps ->
                    match maps with
                    | [] -> VarMap.empty
                    | init :: tl -> List.fold ~f:(Map.merge ~f:combine) ~init tl))
        | _ -> None)
    | PatTuple args -> (
        (* Matching a tuple is almost the same as matching a constructor.*)
        match t.tkind with
        | TTup args' -> (
            match List.zip args args' with
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

(**
  [skeletize ~functions t] returns the term t where each subterm that does not
  contain a call to the functions in [functions] has been reblaced by a box containing
  a fresh variable.
  Returns a pair of a boxed var-substitution list and the term with the boxes replacing the
  subexpressions.
*)
let skeletize ~(functions : VarSet.t) (t : term) : term VarMap.t * term =
  let skel_id = ref 0 in
  let boxes = ref (Map.empty (module Terms)) in
  let case _ t0 =
    if has_calls ~_to:functions t0 then None
    else
      let boxv =
        match Map.find !boxes t0 with
        | Some boxv -> boxv
        | None ->
            let boxv = Variable.mk ~t:(Some t0.ttyp) (Fmt.str "?%i" !skel_id) in
            Int.incr skel_id;
            boxes := Map.set !boxes ~key:t0 ~data:boxv;
            boxv
      in
      Some (mk_var boxv)
  in
  let boxed_ts = transform ~case t in
  let varmap =
    Map.fold !boxes ~init:VarMap.empty ~f:(fun ~key ~data m -> Map.set m ~key:data ~data:key)
  in
  (varmap, boxed_ts)

(* ============================================================================================= *)
(*                                  NAIVE TERM EXPANSION                                         *)
(* ============================================================================================= *)

let expand_once (t : term) : term list =
  let fvt = free_variables t in
  (* Pick variables to expand on *)
  let expandable =
    let filter v =
      match RType.get_variants (Variable.vtype_or_new v) with [] -> None | l -> Some (v, l)
    in
    List.filter_map (Set.elements fvt) ~f:filter
  in
  let aux (v, v_expan) =
    let f (cstr_name, cstr_arg_types) =
      let cstr_args =
        List.map cstr_arg_types ~f:(fun ty ->
            mk_composite_base_type ty
            (* mk_var
               (Variable.mk ~t:(Some ty)
                  (if RType.is_recursive ty then Alpha.fresh ~s:"l" () else Alpha.fresh ~s:"n" ())
                  ) *))
      in
      let t, _ = infer_type (substitution [ (mk_var v, mk_data cstr_name cstr_args) ] t) in
      t
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
    | [] -> Some (mk_var (Variable.mk ~t:(Some t) (Alpha.fresh ~s:"l" ())))
    | _ -> None
  in
  let f (variant, targs) =
    match targs with
    | [] -> Some (mk_data variant [])
    | targs ->
        let l = List.map ~f:inst_or_none targs in
        if List.for_all ~f:Option.is_some l then Some (mk_data variant (List.filter_opt l))
        else None
  in
  List.filter_opt (List.map ~f variants)

let terms_of_max_depth (depth : int) (typ : RType.t) : term list =
  let rec constr_t d _typ : term list =
    match RType.get_variants _typ with
    | [] -> [ mk_var (Variable.mk ~t:(Some _typ) (Alpha.fresh ~s:"a" ())) ]
    | l ->
        if d >= depth then variant_no_recurse l else List.concat (List.map ~f:(constr_variant d) l)
  and constr_variant d (cname, cargs) : term list =
    match cargs with
    | [] -> [ mk_data cname [] ]
    | _ ->
        let subterms = List.map ~f:(constr_t (d + 1)) cargs in
        let choices =
          List.map (Utils.cartesian_nary_product subterms) ~f:(fun cargs ->
              mk_data cname (List.rev cargs))
        in
        choices
  in
  constr_t 1 typ

type virtual_term =
  | VConstr of string * virtual_term list
  | VChoice of virtual_term list
  | VVarLeaf of Variable.t

let rec pp_virtual_term (f : Formatter.t) (v : virtual_term) : unit =
  match v with
  | VConstr (cname, args) -> Fmt.(pf f "%s(%a)" cname (list ~sep:comma pp_virtual_term) args)
  | VChoice args -> Fmt.(pf f "(choose %a)" (list ~sep:sp (parens pp_virtual_term)) args)
  | VVarLeaf v -> Variable.pp f v

let virtual_variant_no_recurse (variants : (string * RType.t list) list) : virtual_term =
  let inst_or_none t =
    match RType.get_variants t with
    | [] -> Some (VVarLeaf (Variable.mk ~t:(Some t) (Alpha.fresh ~s:"l" ())))
    | _ -> None
  in
  let f (variant, targs) =
    match targs with
    | [] -> Some (VConstr (variant, []))
    | targs ->
        let l = List.map ~f:inst_or_none targs in
        if List.for_all ~f:Option.is_some l then Some (VConstr (variant, List.filter_opt l))
        else None
  in
  VChoice (List.filter_opt (List.map ~f variants))

let virtual_term_of_max_depth (depth : int) (typ : RType.t) : virtual_term =
  let rec for_each_variant d (constrname, tl) = VConstr (constrname, List.map ~f:(constr (d + 1)) tl)
  and constr d t =
    match RType.get_variants t with
    | [] -> VVarLeaf (Variable.mk ~t:(Some t) (Alpha.fresh ()))
    | variants ->
        if d >= depth then virtual_variant_no_recurse variants
        else VChoice (List.map ~f:(for_each_variant d) variants)
  in
  constr 0 typ

(** Pick some term in a vitual term, removing the choices made from the virtual term. *)
let pick_some (virt : virtual_term) : term option * virtual_term option =
  let rec aux vt =
    match vt with
    | VVarLeaf x -> (Some (mk_var x), None)
    | VChoice choices -> (
        let n = List.length choices in
        if n = 0 then (None, None)
        else
          let i = Random.int n in
          match List.split_n choices i with
          | l, hd :: tl -> (
              let t, new_hd = aux hd in
              match new_hd with
              | Some hd' -> (t, Some (VChoice (l @ (hd' :: tl))))
              | None -> (t, Some (VChoice (l @ tl))))
          | _, _ -> failwith "Unexpected failure in pick_some.")
    | VConstr (cname, args) -> (
        match args with
        | [] -> (Some (mk_data cname []), None)
        | _ ->
            let tl, vl = List.unzip (List.map ~f:aux args) in
            let t = Option.map (all_or_none tl) ~f:(fun args' -> mk_data cname args') in
            let v = Option.map (all_or_none vl) ~f:(fun args' -> VConstr (cname, args')) in
            (t, v))
  in
  aux virt

(* ============================================================================================= *)
(*                                  TERM CONCRETIZATION                                          *)
(* ============================================================================================= *)

(** Arbitrary min and max values. Actual useful values will be generated by solvers anyway. *)
let _INT_MAX = 124

let _INT_MIN = -124

let rec rand_const_of_type (typ : RType.t) : term =
  RType.(
    match typ with
    | TInt -> mk_const (Constant.CInt (Random.int (_INT_MAX - _INT_MIN) + _INT_MIN))
    | TBool -> mk_const (if Random.bool () then Constant.CTrue else Constant.CFalse)
    | TChar -> failwith "Char unsupported."
    | TString -> failwith "String unsupported."
    | TVar _ -> mk_const (Constant.CInt (Random.int (_INT_MAX - _INT_MIN) + _INT_MIN))
    | _ -> (
        match get_variants typ with
        | [] -> failwith Fmt.(str "Unsupported value type %a" pp typ)
        | l -> (
            match variant_no_recurse l with
            | [] ->
                failwith Fmt.(str "Unsupported value type %a (no non-recursive constructor)" pp typ)
            | hd :: _ -> concretize hd)))

and concretize ?(model = Map.empty (module String)) (t : term) : term =
  let case _ tin =
    let tk = tin.tkind in
    match tk with
    | TVar x -> (
        match Map.find model x.vname with
        | Some t -> Some t
        | None -> Some (rand_const_of_type (Variable.vtype_or_new x)))
    | _ -> None
  in
  transform ~case t
