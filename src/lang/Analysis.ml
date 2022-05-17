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
;;

let free_variables ?(include_functions = true) ~(ctx : Context.t) (t : term) : VarSet.t =
  let rec f t =
    match t.tkind with
    | TBox t -> f t
    | TBin (_, t1, t2) -> Set.union (f t1) (f t2)
    | TUn (_, t1) -> f t1
    | TConst _ -> VarSet.empty
    | TVar v ->
      if include_functions
      then VarSet.singleton v
      else (
        match Variable.vtype_or_new ctx v with
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
      Set.union
        (f tm)
        (VarSet.union_list
           (List.map ~f:(fun (p, t) -> Set.diff (f t) (vars_of_pattern p)) cases))
  in
  f t
;;

(**
  Given a hashtable where keys are variables ids, count the occurrences of those
  variables in a term, and update the hashtable to contain those counts.
  *)
let count_occurrences (m : (int, int) Hashtbl.t) (t : term) =
  let case _ t =
    match t.tkind with
    | TVar v ->
      Hashtbl.change m v.vid ~f:(function
          | Some x -> Some (x + 1)
          | None -> None);
      Some ()
    | _ -> None
  in
  reduce ~init:() ~case ~join:(fun _ _ -> ()) t
;;

let has_ite (t : term) : bool =
  reduce t ~init:false ~join:( || ) ~case:(fun _ t ->
      match t.tkind with
      | TIte _ -> Some true
      | _ -> None)
;;

let has_calls ~(ctx : Context.t) ~(_to : VarSet.t) (t : term) : bool =
  not (Set.is_empty (Set.inter _to (free_variables ~ctx t)))
;;

let operators_of (t : term) : OpSet.t =
  let case f t =
    match t.tkind with
    | TBin (binop, t1, t2) ->
      Some (Set.add (Set.union (f t1) (f t2)) (Operator.Binary binop))
    | TUn (unop, t1) -> Some (Set.add (f t1) (Operator.Unary unop))
    | _ -> None
  in
  reduce ~init:OpSet.empty ~join:Set.union ~case t
;;

let is_constant =
  let case _ t =
    match t.tkind with
    | TConst _ -> Some true
    | _ -> None
  in
  reduce ~init:false ~join:( && ) ~case
;;

let is_norec ~(ctx : Context.t) =
  let case _ t =
    match t.tkind with
    | TVar x -> Some (not (RType.is_recursive ctx.types (Variable.vtype_or_new ctx x)))
    | _ -> None
  in
  reduce ~init:true ~join:( && ) ~case
;;

(** A term has no variant if it is a variable and the type of that
  variable has no variants. This means that term cannot be expanded.
*)
let is_novariant ~(ctx : Context.t) =
  let case _ t =
    match t.tkind with
    | TVar x ->
      Some (List.is_empty (RType.get_variants ctx.types (Variable.vtype_or_new ctx x)))
    | _ -> None
  in
  reduce ~init:true ~join:( && ) ~case
;;

let is_bounded ~(ctx : Context.t) (t : term) =
  Term.reduce
    ~init:true
    ~join:( && )
    ~case:(fun _ t ->
      match t.tkind with
      | TVar _ -> Some (is_novariant ~ctx t)
      | _ -> None)
    t
;;

let subst_args ~(ctx : Context.t) (fpatterns : fpattern list) (args : term list)
    : (fpattern list * (term * term) list, string) Result.t
  =
  let rec match_pattern_to_arg (fpat, t) =
    match fpat, t.tkind with
    | FPatVar x, _ -> Some [ mk_var ctx x, t ]
    | FPatAny, _ -> Some []
    | FPatTup pl, TTup tl ->
      (match List.zip pl tl with
      | Ok ptl ->
        Option.map ~f:List.concat (all_or_none (List.map ~f:match_pattern_to_arg ptl))
      | _ -> None)
    | _ -> None
  in
  List.fold_until
    args
    ~init:(fpatterns, [])
    ~finish:Result.return
    ~f:(fun (fpatterns_left, substs) arg ->
      match fpatterns_left with
      | hd :: tl ->
        (match match_pattern_to_arg (hd, arg) with
        | Some substs' -> Continue (tl, substs @ substs')
        | None ->
          Stop
            (Error
               (Fmt.str
                  "Could not match %a with %a."
                  (pp_fpattern ctx)
                  hd
                  (pp_term ctx)
                  arg)))
      | _ -> Stop (Error "Too many arguments in function application."))
;;

let argset_of ~(ctx : Context.t) (f : variable) =
  let case _ t =
    match t.tkind with
    | TApp ({ tkind = TVar f'; _ }, args) when Variable.equal f f' ->
      Some (VarSet.union_list (List.map ~f:(free_variables ~ctx) args))
    | _ -> None
  in
  reduce ~init:VarSet.empty ~case ~join:Set.union
;;

(** [replace_calls_to functions t] replaces all calls to functions in [functions]
  by a fresh variable.
 *)
let replace_calls_to ~(ctx : Context.t) (funcs : VarSet.t) : term -> term =
  let case _ t =
    match t.tkind with
    | TApp ({ tkind = TVar x; _ }, _) when Set.mem funcs x ->
      Some
        (mk_var ctx (Variable.mk ctx ~t:(Some t.ttyp) (Alpha.fresh ctx.names ~s:x.vname)))
    | _ -> None
  in
  transform ~case
;;

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
    List.map
      ~f:(fun x -> mk_app (mk_var_no_ctx func) [ x ], x)
      (Set.elements args_of_calls_to_func)
  in
  substitution subs t
;;

let rename_nicely ~(ctx : Context.t) (soln : (string * variable list * term) list)
    : (string * variable list * term) list
  =
  let f (sname, args, body) =
    let newargs =
      List.map args ~f:(fun v ->
          v, Variable.mk ctx ~t:(Some (Variable.vtype_or_new ctx v)) (Alpha.Nice.next ()))
    in
    ( sname
    , snd (List.unzip newargs)
    , Rewriter.simplify_term
        ~ctx
        (substitution
           (List.map newargs ~f:(fun (v1, v2) -> mk_var ctx v1, mk_var ctx v2))
           body) )
  in
  List.map ~f soln
;;

(* ============================================================================================= *)
(*                                  NAIVE TERM EXPANSION                                         *)
(* ============================================================================================= *)

let is_expandable_var ~(ctx : Context.t) (v : variable) =
  match RType.get_variants ctx.types (Variable.vtype_or_new ctx v) with
  | [] -> None
  | l -> Some (v, l)
;;

let expand_once ~(ctx : Context.t) (t : term) : term list =
  let fvt = free_variables ~ctx t in
  (* Pick variables to expand on *)
  let expandables = List.filter_map (Set.elements fvt) ~f:(is_expandable_var ~ctx) in
  let aux (v, v_constrs) =
    let f (cstr_name, cstr_arg_types) =
      let cstr_args =
        List.map cstr_arg_types ~f:(fun ty -> mk_composite_base_type ~ctx ty)
      in
      let t, _ =
        infer_type ctx (substitution [ mk_var ctx v, mk_data ctx cstr_name cstr_args ] t)
      in
      t
    in
    List.map ~f v_constrs
  in
  List.rev (List.concat (List.map ~f:aux expandables))
;;

(**
  Given a list of variants, returns a list of terms that corresponds to the
  non-recursive constructors, with their arguments instantiated as variables.
*)
let variant_no_recurse ~(ctx : Context.t) (variants : (string * RType.t list) list)
    : term list
  =
  let inst_or_none t =
    match RType.get_variants ctx.types t with
    | [] -> Some (mk_var ctx (Variable.mk ctx ~t:(Some t) (Alpha.fresh ctx.names ~s:"l")))
    | _ -> None
  in
  let f (variant, targs) =
    match targs with
    | [] -> Some (mk_data ctx variant [])
    | targs ->
      let l = List.map ~f:inst_or_none targs in
      if List.for_all ~f:Option.is_some l
      then Some (mk_data ctx variant (List.filter_opt l))
      else None
  in
  List.filter_opt (List.map ~f variants)
;;

let terms_of_max_depth ~(ctx : Context.t) (depth : int) (typ : RType.t) : term list =
  let rec constr_t d _typ : term list =
    match RType.get_variants ctx.types _typ with
    | [] -> [ mk_var ctx (Variable.mk ctx ~t:(Some _typ) (Alpha.fresh ctx.names ~s:"a")) ]
    | l ->
      if d >= depth
      then variant_no_recurse ~ctx l
      else List.concat (List.map ~f:(constr_variant d) l)
  and constr_variant d (cname, cargs) : term list =
    match cargs with
    | [] -> [ mk_data ctx cname [] ]
    | _ ->
      let subterms = List.map ~f:(constr_t (d + 1)) cargs in
      let choices =
        List.map (Utils.cartesian_nary_product subterms) ~f:(fun cargs ->
            mk_data ctx cname cargs)
      in
      choices
  in
  constr_t 1 typ
;;

type virtual_term =
  | VConstr of string * virtual_term list
  | VChoice of virtual_term list
  | VVarLeaf of Variable.t

let rec pp_virtual_term ~(ctx : Context.t) (f : Formatter.t) (v : virtual_term) : unit =
  match v with
  | VConstr (cname, args) ->
    Fmt.(pf f "%s(%a)" cname (list ~sep:comma (pp_virtual_term ~ctx)) args)
  | VChoice args ->
    Fmt.(pf f "(choose %a)" (list ~sep:sp (parens (pp_virtual_term ~ctx))) args)
  | VVarLeaf v -> Variable.pp ctx f v
;;

let virtual_variant_no_recurse
    ~(ctx : Context.t)
    (variants : (string * RType.t list) list)
    : virtual_term
  =
  let inst_or_none t =
    match RType.get_variants ctx.types t with
    | [] -> Some (VVarLeaf (Variable.mk ctx ~t:(Some t) (Alpha.fresh ctx.names ~s:"l")))
    | _ -> None
  in
  let f (variant, targs) =
    match targs with
    | [] -> Some (VConstr (variant, []))
    | targs ->
      let l = List.map ~f:inst_or_none targs in
      if List.for_all ~f:Option.is_some l
      then Some (VConstr (variant, List.filter_opt l))
      else None
  in
  VChoice (List.filter_opt (List.map ~f variants))
;;

let virtual_term_of_max_depth ~(ctx : Context.t) (depth : int) (typ : RType.t)
    : virtual_term
  =
  let rec for_each_variant d (constrname, tl) =
    VConstr (constrname, List.map ~f:(constr (d + 1)) tl)
  and constr d t =
    match RType.get_variants ctx.types t with
    | [] -> VVarLeaf (Variable.mk ctx ~t:(Some t) (Alpha.fresh ctx.names))
    | variants ->
      if d >= depth
      then virtual_variant_no_recurse ~ctx variants
      else VChoice (List.map ~f:(for_each_variant d) variants)
  in
  constr 0 typ
;;

(** Pick some term in a vitual term, removing the choices made from the virtual term. *)
let pick_some ~(ctx : Context.t) (virt : virtual_term) : term option * virtual_term option
  =
  let rec aux vt =
    match vt with
    | VVarLeaf x -> Some (mk_var ctx x), None
    | VChoice choices ->
      let n = List.length choices in
      if n = 0
      then None, None
      else (
        let i = Random.int n in
        match List.split_n choices i with
        | l, hd :: tl ->
          let t, new_hd = aux hd in
          (match new_hd with
          | Some hd' -> t, Some (VChoice (l @ (hd' :: tl)))
          | None -> t, Some (VChoice (l @ tl)))
        | _, _ -> failwith "Unexpected failure in pick_some.")
    | VConstr (cname, args) ->
      (match args with
      | [] -> Some (mk_data ctx cname []), None
      | _ ->
        let tl, vl = List.unzip (List.map ~f:aux args) in
        let t = Option.map (all_or_none tl) ~f:(fun args' -> mk_data ctx cname args') in
        let v = Option.map (all_or_none vl) ~f:(fun args' -> VConstr (cname, args')) in
        t, v)
  in
  aux virt
;;

(* ============================================================================================= *)
(*                                  TERM CONCRETIZATION                                          *)
(* ============================================================================================= *)

(** Arbitrary min and max values. Actual useful values will be generated by solvers anyway. *)
let _INT_MAX = 124

let _INT_MIN = -124

let rec rand_const_of_type ~(ctx : Context.t) (typ : RType.t) : term =
  RType.(
    match typ with
    | TInt -> mk_const (Constant.CInt (Random.int (_INT_MAX - _INT_MIN) + _INT_MIN))
    | TBool -> mk_const (if Random.bool () then Constant.CTrue else Constant.CFalse)
    | TChar -> mk_const (Constant.CChar (Random.char ()))
    | TString -> failwith "String unsupported."
    | TVar _ -> mk_const (Constant.CInt (Random.int (_INT_MAX - _INT_MIN) + _INT_MIN))
    | TTup tl -> mk_tup ctx (List.map ~f:(rand_const_of_type ~ctx) tl)
    | _ ->
      (match get_variants ctx.types typ with
      | [] -> failwith Fmt.(str "Unsupported value type %a" pp typ)
      | l ->
        (match variant_no_recurse ~ctx l with
        | [] ->
          failwith
            Fmt.(str "Unsupported value type %a (no non-recursive constructor)" pp typ)
        | hd :: _ -> concretize ~ctx hd)))

and concretize ?(model = Map.empty (module String)) ~(ctx : Context.t) (t : term) : term =
  let case _ tin =
    let tk = tin.tkind in
    match tk with
    | TVar x ->
      (match Map.find model x.vname with
      | Some t -> Some t
      | None -> Some (rand_const_of_type ~ctx (Variable.vtype_or_new ctx x)))
    | _ -> None
  in
  transform ~case t
;;
