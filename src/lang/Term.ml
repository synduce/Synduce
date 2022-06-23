open Base
open Utils
include TermTypes
include Variables

(* F-Patterns helpers *)

let rec fpat_ty (ctx : Context.t) (fp : fpattern) : RType.t =
  match fp with
  | FPatVar v -> Variable.vtype_or_new ctx v
  | FPatTup tl -> RType.(TTup (List.map ~f:(fpat_ty ctx) tl))
  | FPatAny -> RType.get_fresh_tvar ctx.types
;;

let rec fpat_vars (fp : fpattern) : VarSet.t =
  match fp with
  | FPatVar v -> VarSet.singleton v
  | FPatTup tl -> VarSet.union_list (List.map ~f:fpat_vars tl)
  | FPatAny -> VarSet.empty
;;

let pattern_of_term (t : term) =
  let rec aux t =
    match t.tkind with
    | TVar x -> PatVar x
    | TTup tl -> PatTuple (List.map ~f:aux tl)
    | TData (c, args) -> PatConstr (c, List.map ~f:aux args)
    | TConst c -> PatConstant c
    | _ -> PatAny
  in
  aux t
;;

(* ============================================================================================= *)
(*                        CONSTRUCTION FUNCTIONS                                                 *)
(* ============================================================================================= *)

let mk_pat_any = PatAny
let mk_pat_var v = PatVar v
let mk_pat_const c = PatConstant c
let mk_pat_tuple l = PatTuple l
let mk_pat_constr c l = PatConstr (c, l)

let mk_var (ctx : Context.t) ?(pos = dummy_loc) (v : variable) : term =
  { tpos = pos; tkind = TVar v; ttyp = Variable.vtype_or_new ctx v }
;;

let mk_var_no_ctx ?(pos = dummy_loc) (v : variable) : term =
  { tpos = pos; tkind = TVar v; ttyp = RType.TVar 0 }
;;

let var_or_none (t : term) : variable option =
  match t.tkind with
  | TVar x -> Some x
  | _ -> None
;;

let ext_var_or_none (t : term) : variable list option =
  match t.tkind with
  | TVar x -> Some [ x ]
  | TTup tl ->
    let tl' = List.map ~f:var_or_none tl in
    if List.for_all ~f:Option.is_some tl' then Some (List.filter_opt tl') else None
  | _ -> None
;;

let mk_const ?(pos = dummy_loc) (c : Constant.t) =
  let ctyp =
    Constant.(
      match c with
      | CInt _ -> RType.TInt
      | CChar _ -> RType.TChar
      | CTrue | CFalse -> RType.TBool
      | CEmptySet t -> RType.TSet t)
  in
  { tpos = pos; tkind = TConst c; ttyp = ctyp }
;;

let mk_box ?(pos = dummy_loc) (t : term) : term =
  { tpos = pos; tkind = TBox t; ttyp = t.ttyp }
;;

let mk_app ?(pos = dummy_loc) ?(typ = None) (f : term) (x : term list) =
  let typ =
    match typ with
    | Some t -> t
    | None ->
      let args_t, ret_t = RType.fun_typ_unpack f.ttyp in
      (match List.drop args_t (List.length x) with
      | [] -> ret_t
      | _ as remaining_args -> RType.fun_typ_pack remaining_args ret_t)
  in
  { tpos = pos; tkind = TApp (f, x); ttyp = typ }
;;

let mk_app_v
    (ctx : Context.t)
    ?(pos = dummy_loc)
    ?(typ = None)
    (f : variable)
    (x : term list)
  =
  let typ =
    match typ with
    | Some t -> t
    | None ->
      let args_t, ret_t = RType.fun_typ_unpack (Variable.vtype_or_new ctx f) in
      (match List.drop args_t (List.length x) with
      | [] -> ret_t
      | _ as remaining_args -> RType.fun_typ_pack remaining_args ret_t)
  in
  { tpos = pos; tkind = TApp (mk_var ctx f, x); ttyp = typ }
;;

let mk_bin ?(pos = dummy_loc) ?(typ = None) (op : Binop.t) (t1 : term) (t2 : term) =
  let typ =
    match typ with
    | Some t -> t
    | None -> Binop.result_type op
  in
  { tpos = pos; tkind = TBin (op, t1, t2); ttyp = typ }
;;

(** Applies an associative operator to a list of terms recursively.
  Returns None if the list of arguments is empty.
*)
let mk_assoc (op : Binop.t) (tl : term list) : term option =
  let rec aux t rest =
    match rest with
    | hd :: tl -> aux (mk_bin ~pos:hd.tpos ~typ:(Some hd.ttyp) op t hd) tl
    | [] -> t
  in
  match tl with
  | [] -> None
  | [ x ] -> Some x
  | hd :: tl -> Some (aux hd tl)
;;

let mk_data (ctx : Context.t) ?(pos = dummy_loc) (c : string) (xs : term list) =
  let typ =
    match RType.type_of_variant ctx.types c with
    | Some (t, _) -> t
    | _ -> failwith (Fmt.str "Trying to construct term with unknown constructor %s" c)
  in
  { tpos = pos; tkind = TData (c, xs); ttyp = typ }
;;

let mk_fun (ctx : Context.t) ?(pos = dummy_loc) (args : fpattern list) (body : term) =
  let targs = List.map ~f:(fun t -> fpat_ty ctx t) args in
  { tpos = pos; tkind = TFun (args, body); ttyp = RType.fun_typ_pack targs body.ttyp }
;;

let mk_ite ?(pos = dummy_loc) ?(typ = None) (c : term) (th : term) (el : term) =
  let typ =
    match typ with
    | Some t -> t
    | None -> th.ttyp
  in
  { tpos = pos; tkind = TIte (c, th, el); ttyp = typ }
;;

let mk_tup (_ : Context.t) ?(pos = dummy_loc) (l : term list) =
  { tpos = pos; tkind = TTup l; ttyp = RType.TTup (List.map ~f:(fun t -> t.ttyp) l) }
;;

let mk_tup_no_ctx ?(pos = dummy_loc) (l : term list) =
  { tpos = pos; tkind = TTup l; ttyp = RType.TTup (List.map ~f:(fun t -> t.ttyp) l) }
;;

let mk_sel ?(pos = dummy_loc) ?(typ = None) (ctx : Context.t) (t : term) (i : int) =
  let typ =
    match typ with
    | Some t -> t
    | None ->
      (match t.ttyp with
      | RType.TTup tl ->
        (match List.nth tl i with
        | Some x -> x
        | None -> RType.get_fresh_tvar ctx.types)
      | _ -> RType.get_fresh_tvar ctx.types)
  in
  { tpos = pos; tkind = TSel (t, i); ttyp = typ }
;;

let mk_un ?(pos = dummy_loc) ?(typ = None) (op : Unop.t) (t : term) =
  let typ =
    match typ with
    | Some t -> t
    | None -> Unop.result_type op
  in
  { tpos = pos; tkind = TUn (op, t); ttyp = typ }
;;

let mk_match ?(pos = dummy_loc) (ctx : Context.t) (x : term) (cases : match_case list) =
  let typ =
    match cases with
    | (_, t) :: _ -> t.ttyp
    | _ -> RType.get_fresh_tvar ctx.types
  in
  { tpos = pos; tkind = TMatch (x, cases); ttyp = typ }
;;

let mk_let
    ?(pos = dummy_loc)
    ?(typ = None)
    (ctx : Context.t)
    (bindings : (variable * term) list)
    (body : term)
    : term
  =
  let var_args, term_args = List.unzip bindings in
  let t =
    mk_app
      ~pos
      (mk_fun ctx ~pos (List.map ~f:(fun x -> FPatVar x) var_args) body)
      term_args
  in
  match typ with
  | Some typ -> { t with ttyp = typ }
  | None -> { t with ttyp = body.ttyp }
;;

let term_of_pattern (ctx : Context.t) (p : pattern) : term =
  let rec aux p =
    match p with
    | PatVar v -> mk_var ctx v
    | PatTuple tl -> mk_tup ctx (List.map ~f:aux tl)
    | PatConstant c -> mk_const c
    | PatConstr (c, args) -> mk_data ctx c (List.map ~f:aux args)
    | PatAny -> mk_var ctx (Variable.mk ctx "_")
  in
  aux p
;;

let rec fpat_to_term fp =
  match fp with
  | FPatVar v -> mk_var_no_ctx v
  | FPatTup tl -> mk_tup_no_ctx (List.map ~f:fpat_to_term tl)
  | _ -> mk_tup_no_ctx []
;;

let fpat_sub (fp1 : fpattern) (fp2 : fpattern) =
  let rec aux (fp1, fp2) =
    match fp1, fp2 with
    | FPatVar v1, FPatVar v2 -> [ mk_var_no_ctx v1, mk_var_no_ctx v2 ]
    | FPatTup tl1, FPatTup tl2 ->
      (match List.zip tl1 tl2 with
      | Ok l -> List.concat (List.map ~f:aux l)
      | _ -> failwith "no sub")
    | FPatVar v1, _ -> [ mk_var_no_ctx v1, fpat_to_term fp2 ]
    | _, FPatVar v2 -> [ fpat_to_term fp1, mk_var_no_ctx v2 ]
    | _, _ -> []
  in
  try Some (aux (fp1, fp2)) with
  | _ -> None
;;

let fpat_sub_all fp1s fp2s =
  match List.zip fp1s fp2s with
  | Ok z ->
    (try
       Some
         (List.fold
            ~init:[]
            ~f:(fun l (a, b) ->
              match fpat_sub a b with
              | Some subs -> l @ subs
              | None -> failwith "done")
            z)
     with
    | _ -> None)
  | _ -> None
;;

let sexp_of_term (_ : term) = Sexp.Atom "TODO"

let rec mk_composite_base_type ?(prefix = "") ~(ctx : Context.t) (t : RType.t) : term =
  match t with
  | RType.TInt ->
    mk_var ctx (Variable.mk ctx ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "i") ctx.names))
  | RType.TBool ->
    mk_var ctx (Variable.mk ctx ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "b") ctx.names))
  | RType.TString ->
    mk_var ctx (Variable.mk ctx ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "s") ctx.names))
  | RType.TChar ->
    mk_var ctx (Variable.mk ctx ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "c") ctx.names))
  | RType.TTup tl -> mk_tup ctx (List.map ~f:(mk_composite_base_type ~ctx) tl)
  | RType.TNamed _ ->
    mk_var ctx (Variable.mk ctx ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "l") ctx.names))
  | _ ->
    mk_var ctx (Variable.mk ctx ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "p") ctx.names))
;;

(* | RType.TFun (_, _) | RType.TParam (_, _) | RType.TVar _ ->
    failwith Fmt.(str "mk_composite_base_type: %a is not a base type." RType.pp t) *)

(* ============================================================================================= *)
(*                             EQUALITY                                                          *)
(* ============================================================================================= *)

let rec term_compare (t1 : term) (t2 : term) : int =
  match t1.tkind, t2.tkind with
  | TConst c1, TConst c2 -> Constant.compare c1 c2
  | TVar v1, TVar v2 -> Variable.compare v1 v2
  | TBox t1', TBox t2' -> term_compare t1' t2'
  | TBox t1', _ -> term_compare t1' t2
  | _, TBox t2' -> term_compare t1 t2'
  | TData (c1, args1), TData (c2, args2) ->
    let c = String.compare c1 c2 in
    if c = 0 then List.compare term_compare args1 args2 else c
  | TApp (f1, args1), TApp (f2, args2) ->
    let c = term_compare f1 f2 in
    if c = 0 then List.compare term_compare args1 args2 else c
  | TBin (b1, t11, t12), TBin (b2, t21, t22) ->
    let c = Binop.compare b1 b2 in
    if c = 0
    then (
      let c' = term_compare t11 t21 in
      if c' = 0 then term_compare t12 t22 else c')
    else c
  | TUn (u1, t11), TUn (u2, t21) ->
    let c = Unop.compare u1 u2 in
    if c = 0 then term_compare t11 t21 else c
  | TFun (fargs1, body1), TFun (fargs2, body2) ->
    let c = compare (List.length fargs1) (List.length fargs2) in
    if c = 0
    then (
      match fpat_sub_all fargs1 fargs2 with
      | Some subs -> term_compare body1 (substitution subs body2)
      | None -> -1)
    else c
  | TTup tl1, TTup tl2 -> List.compare term_compare tl1 tl2
  | TSel (t1', i1), TSel (t2', i2) ->
    let c = compare i1 i2 in
    if c = 0 then term_compare t1' t2' else c
  | TMatch (t1', cases1), TMatch (t2', cases2) ->
    let c = term_compare t1' t2' in
    if c = 0
    then List.compare term_compare (snd (List.unzip cases1)) (snd (List.unzip cases2))
    else c
  | _, _ -> Poly.compare t1 t2

and substitution (substs : (term * term) list) (term : term) : term =
  let rec aux (_t : term) =
    match List.Assoc.find substs ~equal:term_equal _t with
    | Some t' -> t'
    | None ->
      let new_kind =
        match _t.tkind with
        | TBin (b1, t1, t2) -> TBin (b1, aux t1, aux t2)
        | TBox t -> TBox (aux t)
        | TUn (u, t1) -> TUn (u, aux t1)
        | TIte (c, tt, tf) -> TIte (aux c, aux tt, aux tf)
        | TTup tl -> TTup (List.map ~f:aux tl)
        | TSel (t, i) -> TSel (aux t, i)
        | TFun (args, body) -> TFun (args, aux body)
        | TApp (f, args) -> TApp (aux f, List.map ~f:aux args)
        | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
        | TMatch (tm, cases) -> TMatch (aux tm, List.map ~f:(fun (c, t) -> c, aux t) cases)
        | TVar _ | TConst _ -> _t.tkind
      in
      { _t with tkind = new_kind }
  in
  aux term

and term_equal t1 t2 = term_compare t1 t2 = 0

let mk_with_fresh_vars (ctx : Context.t) (vs : VarSet.t) (t : term) : VarSet.t * term =
  let substs =
    let f var =
      let fresh =
        let t = Some (Variable.vtype_or_new ctx var) in
        Variable.mk ctx ~t (Alpha.fresh ~s:var.vname ctx.names)
      in
      fresh, (mk_var ctx var, mk_var ctx fresh)
    in
    List.map ~f (Set.elements vs)
  in
  VarSet.of_list (List.map ~f:first substs), substitution (List.map ~f:second substs) t
;;

module VarMap = struct
  module M = Map.M (Variable)
  include M

  type 'value t = 'value M.t

  let empty = Map.empty (module Variable)
  let keyset (m : 'a t) : VarSet.t = VarSet.of_list (Map.keys m)

  let assigns_varname (m : 'a t) (s : string) =
    Map.existsi ~f:(fun ~key ~data:_ -> String.equal key.vname s) m
  ;;

  let singleton (v : variable) (elt : 'a) = Map.singleton (module Variable) v elt
  let of_alist (al : (variable * 'a) list) = Map.of_alist (module Variable) al
  let of_alist_exn (al : (variable * 'a) list) = Map.of_alist_exn (module Variable) al

  let init_from_varset ~(init : variable -> 'a) (vs : VarSet.t) : 'a t =
    Set.fold ~init:empty ~f:(fun m v -> Map.set m ~key:v ~data:(init v)) vs
  ;;

  let to_subst (ctx : Context.t) (map : term t) =
    List.map ~f:(fun (v, t) -> mk_var ctx v, t) (Map.to_alist map)
  ;;

  let to_subst2 (ctx : Context.t) (map : variable t) =
    List.map ~f:(fun (v, t) -> mk_var ctx v, mk_var ctx t) (Map.to_alist map)
  ;;

  let ( $@ ) (map : term t) (v : variable) = Map.find map v
end

(* ============================================================================================= *)
(*                              TRANFORMATION / REDUCTION  UTILS                                 *)
(* ============================================================================================= *)

(**
   `rewrite_with f t` rewrites the term t by applying the rule f bottom-up.
*)
let rewrite_with (f : term -> term) (t : term) =
  let rec aux t0 =
    let tk = t0.tkind in
    let tk' =
      match tk with
      | TApp (func, args) -> TApp (aux func, List.map ~f:aux args)
      | TBin (op, t1, t2) -> TBin (op, aux t1, aux t2)
      | TBox _ -> tk (* Do not rewrite Tbox *)
      | TConst _ -> (f t0).tkind
      | TUn (op, t1) -> TUn (op, aux t1)
      | TVar _ -> tk
      | TIte (c, t1, t2) -> TIte (aux c, aux t1, aux t2)
      | TTup tl -> TTup (List.map ~f:aux tl)
      | TSel (t, i) -> TSel (aux t, i)
      | TFun (fargs, body) -> TFun (fargs, aux body)
      | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
      | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases)
    in
    f { t0 with tkind = tk' }
  in
  aux t
;;

(**
   `rewrite_top_down f t` rewrites the term t by applying the rule f top-down.
*)
let rewrite_top_down (f : term -> term option) (t : term) =
  let rec aux t0 =
    match f t0 with
    | Some t0' -> t0'
    | None ->
      let tk' =
        match t0.tkind with
        | TBin (op, t1, t2) -> TBin (op, aux t1, aux t2)
        | TUn (op, t1) -> TUn (op, aux t1)
        | TConst _ -> t0.tkind
        | TVar _ -> t0.tkind
        | TBox _ -> t0.tkind
        | TIte (c, t1, t2) -> TIte (aux c, aux t1, aux t2)
        | TTup tl -> TTup (List.map ~f:aux tl)
        | TSel (t, i) -> TSel (aux t, i)
        | TFun (fargs, body) -> TFun (fargs, aux body)
        | TApp (func, args) -> TApp (aux func, List.map ~f:aux args)
        | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
        | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases)
      in
      { t0 with tkind = tk' }
  in
  aux t
;;

(**
   `rewrite_accum ~init ~f t` rewrites the term t by applying the rule f in a top-down manner,
   but as opposed to `rewrite_top_down` the function `f` can use an accumulator that accumulates
   information during the traversal.
*)
let rewrite_accum ~(init : 'a) ~(f : 'a -> term -> (term, 'a) Either.t) (t : term) =
  let rec aux a t0 =
    match f a t0 with
    | Either.First t0' -> t0'
    | Either.Second a' ->
      let tk' =
        match t0.tkind with
        | TBin (op, t1, t2) -> TBin (op, aux a' t1, aux a' t2)
        | TUn (op, t1) -> TUn (op, aux a' t1)
        | TConst _ -> t0.tkind
        | TVar _ -> t0.tkind
        | TBox _ -> t0.tkind
        | TIte (c, t1, t2) -> TIte (aux a' c, aux a' t1, aux a' t2)
        | TTup tl -> TTup (List.map ~f:(aux a') tl)
        | TSel (t, i) -> TSel (aux a' t, i)
        | TFun (fargs, body) -> TFun (fargs, aux a' body)
        | TApp (func, args) -> TApp (aux a' func, List.map ~f:(aux a') args)
        | TData (cstr, args) -> TData (cstr, List.map ~f:(aux a') args)
        | TMatch (tm, cases) -> TMatch (aux a' tm, list_map_snd ~f:(aux a') cases)
      in
      { t0 with tkind = tk' }
  in
  aux init t
;;

let rewrite_types ctx t_subs =
  Variable.update_var_types ctx t_subs;
  rewrite_with (fun _t -> { _t with ttyp = RType.sub_all t_subs _t.ttyp })
;;

(**
   `reduce ~init ~case ~join t` reduces the term by reducing each leaf to `init`, and at each node
    of the syntax tree, using `join` to merge the values. In a top-down traversal, if `case` returns
    `Some a` then the subterm is not recrusively reduced, but the value `a` is used instead.
*)
let reduce
    ~(init : 'a)
    ~(case : (term -> 'a) -> term -> 'a option)
    ~(join : 'a -> 'a -> 'a)
    (t : term)
    : 'a
  =
  let rec aux (t : term) : 'a =
    match case aux t with
    | Some x -> x
    | None ->
      (match t.tkind with
      | TBin (_, t1, t2) -> join (aux t1) (aux t2)
      | TUn (_, t1) -> aux t1
      | TConst _ -> init
      | TVar _ -> init
      | TBox t -> aux t
      | TIte (c, a, b) -> join (aux c) (join (aux a) (aux b))
      | TTup tl -> aux_l tl
      | TSel (t, _) -> aux t
      | TFun (_, body) -> aux body
      | TApp (func, args) -> join (aux func) (aux_l args)
      | TData (_, args) -> aux_l args
      | TMatch (tm, cases) -> join (aux tm) (aux_l (snd (List.unzip cases))))
  and aux_l l = List.fold ~init ~f:join (List.map ~f:aux l) in
  aux t
;;

let transform ~(case : (term -> term) -> term -> term option) (t : term) : term =
  let rec aux (t : term) : 'a =
    match case aux t with
    | Some x -> x
    | None ->
      { t with
        tkind =
          (match t.tkind with
          | TBin (bo, t1, t2) -> TBin (bo, aux t1, aux t2)
          | TUn (uo, t1) -> TUn (uo, aux t1)
          | TConst _ -> t.tkind
          | TVar _ -> t.tkind
          | TBox _ -> t.tkind
          | TIte (c, a, b) -> TIte (aux c, aux a, aux b)
          | TTup tl -> TTup (aux_l tl)
          | TSel (t, i) -> TSel (aux t, i)
          | TFun (args, body) -> TFun (args, aux body)
          | TApp (func, args) -> TApp (aux func, aux_l args)
          | TData (cstr, args) -> TData (cstr, aux_l args)
          | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases))
      }
  and aux_l l = List.map ~f:aux l in
  aux t
;;

let transform_at_depth
    (min_depth : int)
    ~(case : (term -> term) -> term -> term option)
    (t : term)
    : term
  =
  let rec aux (d : int) (t : term) : term =
    if d >= min_depth
    then (
      match case (aux d) t with
      | Some x -> x
      | None -> drec d t)
    else drec d t
  and drec d t =
    let aux = aux (d + 1) in
    let aux_l l = List.map ~f:aux l in
    { t with
      tkind =
        (match t.tkind with
        | TBin (bo, t1, t2) -> TBin (bo, aux t1, aux t2)
        | TUn (uo, t1) -> TUn (uo, aux t1)
        | TConst _ -> t.tkind
        | TVar _ -> t.tkind
        | TBox _ -> t.tkind
        | TIte (c, a, b) -> TIte (aux c, aux a, aux b)
        | TTup tl -> TTup (aux_l tl)
        | TSel (t, i) -> TSel (aux t, i)
        | TFun (args, body) -> TFun (args, aux body)
        | TApp (func, args) -> TApp (aux func, aux_l args)
        | TData (cstr, args) -> TData (cstr, aux_l args)
        | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases))
    }
  in
  aux 0 t
;;

let transform_info ~(f : term -> term) (t : term) : term =
  let rec aux (t : term) : 'a =
    { (f t) with
      tkind =
        (match t.tkind with
        | TBin (bo, t1, t2) -> TBin (bo, aux t1, aux t2)
        | TUn (uo, t1) -> TUn (uo, aux t1)
        | TConst _ -> t.tkind
        | TVar _ -> t.tkind
        | TBox _ -> t.tkind
        | TIte (c, a, b) -> TIte (aux c, aux a, aux b)
        | TTup tl -> TTup (aux_l tl)
        | TSel (t, i) -> TSel (aux t, i)
        | TFun (args, body) -> TFun (args, aux body)
        | TApp (func, args) -> TApp (aux func, aux_l args)
        | TData (cstr, args) -> TData (cstr, aux_l args)
        | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases))
    }
  and aux_l l = List.map ~f:aux l in
  aux t
;;

let remove_boxes (t : term) =
  transform
    ~case:(fun f t ->
      match t.tkind with
      | TBox t -> Some (f t)
      | _ -> None)
    t
;;

let var_count (ctx : Context.t) (typ : RType.t) (t : term) =
  let case _ t =
    match t.tkind with
    | TVar v -> Some (if Poly.equal (Variable.vtype_or_new ctx v) typ then 1 else 0)
    | _ -> None
  in
  reduce ~init:0 ~case ~join:(fun a b -> a + b) t
;;

let var_count_compare (ctx : Context.t) typ (t1 : term) (t2 : term) =
  compare (var_count ctx typ t1) (var_count ctx typ t2)
;;

let term_size (t : term) =
  let case _ t =
    match t.tkind with
    | TConst _ | TVar _ -> Some 1
    | _ -> None
  in
  reduce ~init:0 ~case ~join:(fun a b -> a + b + 1) t
;;

let term_size_compare (t1 : term) (t2 : term) = compare (term_size t1) (term_size t2)

let term_height (t : term) : int =
  let rec aux (t : term) : 'a =
    match t.tkind with
    | TBin (_, t1, t2) -> 1 + max (aux t1) (aux t2)
    | TUn (_, t1) -> 1 + aux t1
    | TConst _ -> 1
    | TVar _ -> 1
    | TBox t -> aux t
    | TIte (c, a, b) -> 1 + max (aux c) (max (aux a) (aux b))
    | TTup tl -> aux_l tl + 1
    | TSel (t, _) -> aux t
    | TFun (_, body) -> aux body
    | TApp (func, args) -> 1 + max (aux func) (aux_l args)
    | TData (_, args) -> 1 + aux_l args
    | TMatch (tm, cases) -> 1 + max (aux tm) (aux_l (snd (List.unzip cases)))
  and aux_l l = List.fold ~init:0 ~f:(fun a t -> max a (aux t)) l in
  aux t
;;

let term_height_compare (t1 : term) (t2 : term) =
  compare (term_height t1) (term_height t2)
;;

(** [proj_var v] returns the tuple term (v.0, v.1, ..., v.n) if [v] is a variable of
    tuple type with n compoenents, otherwise returns v as a term.
  *)
let proj_var (ctx : Context.t) (v : variable) : term =
  let rec p t =
    match t with
    | RType.TTup tl -> mk_tup ctx (List.mapi ~f:(fun i ti -> mk_sel ctx (p ti) i) tl)
    | _ -> mk_var ctx v
  in
  p (Variable.vtype_or_new ctx v)
;;

(** [tuplify t] transforms every subterm of [t] that has a tuple type into a tuple of
  projections. For example if t is a term of tuple type with n components, it returns
  (t.1, t.2, .., t.n).
*)
let tuplify (ctx : Context.t) (t : term) =
  let case _ t =
    match t.tkind with
    | TVar v ->
      (match Variable.vtype_or_new ctx v with
      | TTup _ -> Some (proj_var ctx v)
      | _ -> None)
    | _ ->
      (match t.ttyp with
      | RType.TTup tl -> Some (mk_tup ctx (List.mapi ~f:(fun i _ -> mk_sel ctx t i) tl))
      | _ -> None)
  in
  transform ~case t
;;

(* ============================================================================================= *)
(*                                    PRETTY PRINTERS                                            *)
(* ============================================================================================= *)
open Fmt

let rec pp_fpattern (ctx : Context.t) (frmt : Formatter.t) (fp : fpattern) =
  match fp with
  | FPatVar x -> (Variable.pp ctx) frmt x
  | FPatTup tl -> pf frmt "%a" (box (parens (list ~sep:comma (pp_fpattern ctx)))) tl
  | FPatAny -> pf frmt "_"
;;

let rec pp_pattern (ctx : Context.t) (frmt : Formatter.t) (p : pattern) =
  match p with
  | PatAny -> pf frmt "_"
  | PatVar v -> (Variable.pp ctx) frmt v
  | PatConstant c -> Constant.pp frmt c
  | PatConstr (c, args) ->
    (match args with
    | [] -> string frmt c
    | _ ->
      pf frmt "%a(%a)" (styled `Italic string) c (list ~sep:comma (pp_pattern ctx)) args)
  | PatTuple tl -> (parens (list ~sep:comma (pp_pattern ctx))) frmt tl
;;

let pp_term (ctx : Context.t) (frmt : Formatter.t) (x : term) =
  let rec aux (paren : bool) (frmt : Formatter.t) (t : term) =
    match t.tkind with
    | TConst c -> pf frmt "%a" (styled (`Fg `Cyan) Constant.pp) c
    | TVar v -> pf frmt "%a" (styled (`Fg `Blue) (Variable.pp ctx)) v
    | TBox t -> (aux paren) frmt t
    | TBin (op, t1, t2) ->
      (match op with
      | Binop.Max | Binop.Min ->
        if paren
        then pf frmt "@[<hov 2>(%a@;%a@;%a)@]" Binop.pp op (aux true) t1 (aux true) t2
        else pf frmt "@[<hov 2>%a@;%a@;%a@]" Binop.pp op (aux true) t1 (aux true) t2
      | Binop.Or when !pp_nice ->
        (match t1.tkind with
        | TUn (Unop.Not, ante) ->
          if paren
          then pf frmt "@[<hov 2>(%a@;=>@;%a)@]" (aux false) ante (aux false) t2
          else pf frmt "@[<hov 2>%a@;=>@;%a@]" (aux false) ante (aux false) t2
        | _ ->
          if paren
          then pf frmt "@[<hov 2>(%a@;%a@;%a)@]" (aux true) t1 Binop.pp op (aux true) t2
          else pf frmt "@[<hov 2>%a@;%a@;%a@]" (aux true) t1 Binop.pp op (aux true) t2)
      | _ ->
        if paren
        then pf frmt "@[<hov 2>(%a@;%a@;%a)@]" (aux true) t1 Binop.pp op (aux true) t2
        else pf frmt "@[<hov 2>%a@;%a@;%a@]" (aux true) t1 Binop.pp op (aux true) t2)
    | TUn (op, t1) ->
      if paren
      then pf frmt "@[<hov 2>(%a@;%a)@]" Unop.pp op (aux true) t1
      else pf frmt "@[<hov 2>%a@;%a@]" Unop.pp op (aux true) t1
    | TIte (c, t1, t2) ->
      if paren
      then
        pf
          frmt
          "@[<hov 2>(%a@;?@;%a@;:@;%a)@]"
          (aux false)
          c
          (aux false)
          t1
          (aux false)
          t2
      else
        pf frmt "@[<hov 2>%a@;?@;%a@;:@;%a@]" (aux false) c (aux false) t1 (aux false) t2
    | TTup tl -> pf frmt "@[<hov 2>(%a)@]" (list ~sep:comma (box (aux false))) tl
    | TSel (t, i) -> pf frmt "@[<hov 2>%a.%i@]" (aux true) t i
    (* Some application terms can be printed like let .. = .. in .. for readability. *)
    | TApp ({ tkind = TFun ([ arg_fpat ], body); _ }, [ app_arg ]) ->
      pf
        frmt
        "@[<v>@[let %a@[<hov 2> =@;%a@]@;in@]@;@[<hov 2>%a@]@]"
        (pp_fpattern ctx)
        arg_fpat
        (aux false)
        app_arg
        (aux false)
        body
    | TFun (args, body) ->
      if paren
      then
        pf
          frmt
          "@[<hov 2>(fun %a -> @;%a)@]"
          (list ~sep:sp (pp_fpattern ctx))
          args
          (aux false)
          body
      else
        pf
          frmt
          "@[<hov 2>fun %a -> %a@]"
          (list ~sep:sp (pp_fpattern ctx))
          args
          (aux false)
          body
    | TApp (func, args) ->
      if paren
      then pf frmt "@[<hov 2>(%a@ %a)@]" (aux true) func (list ~sep:sp (aux true)) args
      else pf frmt "@[<hov 2>%a@ %a@]" (aux true) func (list ~sep:sp (aux true)) args
    | TData (cstr, args) ->
      if List.length args = 0
      then pf frmt "%a" (styled (`Fg `Green) string) cstr
      else
        pf
          frmt
          "%a(%a)"
          (styled (`Fg `Green) string)
          cstr
          (list ~sep:comma (aux false))
          args
    | TMatch (tm, cases) ->
      pf
        frmt
        "@[<hov 2>@[match %a with@]@;@[<v>%a@]@]"
        (aux false)
        tm
        (list ~sep:sp (fun fmt (l, r) ->
             pf fmt "@[<hov 2>| %a ->@;%a@]" (box (pp_pattern ctx)) l (box (aux false)) r))
        cases
  in
  aux false frmt x
;;

let pp_subs (ctx : Context.t) (f : Formatter.t) (subs : (term * term) list) : unit =
  Fmt.(
    pf
      f
      "@[<hov 2>%a@]"
      (fun f l ->
        List.iter
          ~f:(fun (t1, t2) -> pf f "@[[%a -> %a]@]" (pp_term ctx) t1 (pp_term ctx) t2)
          l)
      subs)
;;

let pp_function_descr (ctx : Context.t) (fmt : Formatter.t) (fd : function_descr) : unit =
  let _, t_out = RType.fun_typ_unpack (Variable.vtype_or_new ctx fd.f_var) in
  pf
    fmt
    "@[<hov 2>@[let rec %s %a : %a@] =@;@[%a@]@]"
    fd.f_var.vname
    (list ~sep:sp (pp_pattern ctx))
    fd.f_args
    RType.pp
    t_out
    (pp_term ctx)
    fd.f_body
;;

(* ============================================================================================= *)
(*                                  TYPE INFERENCE                                               *)
(* ============================================================================================= *)

let infer_type (ctx : Context.t) (t : term) : term * RType.substitution =
  let rec aux t0 =
    let eloc = t0.tpos in
    let merge_subs = RType.merge_subs eloc in
    match t0.tkind with
    | TBox t -> aux t
    | TBin (op, t1, t2) ->
      let t_t1, c_t1 = aux t1
      and t_t2, c_t2 = aux t2 in
      (* Collect possible operand types. *)
      let possible_opty = Binop.operand_types op in
      (* Find a pair of operaand types that work. *)
      let maybe_t =
        match op with
        | Binop.Eq ->
          (match RType.unify [ t_t1.ttyp, t_t2.ttyp ] with
          | Ok subs ->
            Some
              ( mk_bin ~pos:t0.tpos ~typ:(Some (Binop.result_type op)) op t_t1 t_t2
              , merge_subs subs (merge_subs c_t1 c_t2) )
          | Error _ -> None)
        | _ ->
          List.find_map possible_opty ~f:(fun (ta, tb) ->
              match RType.unify [ t_t1.ttyp, ta; t_t2.ttyp, tb ] with
              | Ok subs ->
                Some
                  ( mk_bin ~pos:t0.tpos ~typ:(Some (Binop.result_type op)) op t_t1 t_t2
                  , merge_subs subs (merge_subs c_t1 c_t2) )
              | Error _ -> None)
      in
      (match maybe_t with
      | Some x -> x
      | None ->
        Log.error_msg
          Fmt.(str "Cannot infer type of binary expression %a." (pp_term ctx) t0);
        Log.error_msg
          Fmt.(
            str
              "%a has type %a, and %a has type %a, expected pair to be one  of %a."
              (pp_term ctx)
              t1
              RType.pp
              t_t1.ttyp
              (pp_term ctx)
              t2
              RType.pp
              t_t2.ttyp
              (list ~sep:sp (parens (pair ~sep:comma RType.pp RType.pp)))
              possible_opty);
        failwith "Type inference failure.")
    | TUn (op, t1) ->
      let t_t1, c_t1 = aux t1 in
      (match RType.unify [ t_t1.ttyp, Unop.operand_type op ] with
      | Ok subs ->
        mk_un ~pos:t0.tpos ~typ:(Some (Unop.result_type op)) op t_t1, merge_subs subs c_t1
      | Error e ->
        Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
        Log.error_msg
          Fmt.(str "Cannot infer type of unary expression %a." (pp_term ctx) t0);
        Log.error_msg
          Fmt.(
            str
              "%a has type %a, expected type %a."
              (pp_term ctx)
              t1
              RType.pp
              t_t1.ttyp
              RType.pp
              (Unop.operand_type op));
        Log.loc_fatal_errmsg eloc "Type inference failure.")
    | TConst c -> { t0 with ttyp = Constant.type_of c }, []
    | TVar v ->
      let tv = Variable.vtype_or_new ctx v in
      (match RType.unify [ tv, t0.ttyp ] with
      | Ok res -> { t0 with ttyp = tv }, res
      | Error e ->
        Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
        failwith "Type inference failure")
    | TIte (c, t1, t2) ->
      let t_c, c_c = aux c
      and t_t1, c_t1 = aux t1
      and t_t2, c_t2 = aux t2 in
      (match RType.unify [ t_c.ttyp, RType.TBool; t_t1.ttyp, t_t2.ttyp ] with
      | Ok subs ->
        ( mk_ite ~pos:t0.tpos ~typ:(Some t_t1.ttyp) t_c t_t1 t_t2
        , merge_subs subs (merge_subs c_c (merge_subs c_t1 c_t2)) )
      | Error e ->
        Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
        Log.error_msg
          Fmt.(str "ite(%a, %a, %a)." RType.pp c.ttyp RType.pp t1.ttyp RType.pp t2.ttyp);
        failwith "Type inference failure.")
    | TTup tl ->
      let term_l, c_l = List.unzip (List.map ~f:aux tl) in
      mk_tup ctx ~pos:t0.tpos term_l, merge_subs (List.concat c_l) []
    | TSel (t, i) ->
      let t_c, c_c = aux t in
      (match t_c.ttyp with
      | RType.TTup tl ->
        (match List.nth tl i with
        | Some tout -> mk_sel ctx ~pos:t0.tpos ~typ:(Some tout) t_c i, c_c
        | None ->
          Log.error_msg
            Fmt.(str "In tuple acessor %a, index out of bounds." (pp_term ctx) t0);
          failwith "Type inference: tuple acessor, accesed tuple of wrong type.")
      | _ ->
        Log.error_msg
          Fmt.(
            str
              "Tuple accessor argument %a of type %a."
              (pp_term ctx)
              t_c
              RType.pp
              t_c.ttyp);
        failwith "Type inference: tuple accessor on type acessor.")
    | TFun (args, body) ->
      let t_body, c_body = aux body in
      mk_fun ctx ~pos:t0.tpos args t_body, c_body
    | TApp (func, fargs) ->
      let t_func, c_func = aux func
      and t_args, c_args = List.unzip (List.map ~f:aux fargs) in
      let argst = List.map ~f:(fun t -> t.ttyp) t_args in
      let csub = RType.(mkv c_func @ mkv (List.concat c_args)) in
      (match t_func.ttyp with
      | RType.TFun (_, _) ->
        let func_targs, func_tout = RType.fun_typ_unpack t_func.ttyp in
        (match List.zip func_targs argst with
        | Ok typ_pairs ->
          (match RType.(unify (csub @ typ_pairs)) with
          | Ok subs -> mk_app ~pos:t0.tpos ~typ:(Some func_tout) t_func t_args, subs
          | Error e ->
            Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
            Log.loc_fatal_errmsg
              eloc
              (Fmt.str
                 "Type inference failure: could not unify types in application %a(%a)"
                 (pp_term ctx)
                 func
                 (box (list ~sep:sp (pp_term ctx)))
                 fargs))
        | Unequal_lengths ->
          Log.loc_fatal_errmsg
            eloc
            (Fmt.str
               "Type inference failure: %a expects %i argument, given %i: %a."
               (pp_term ctx)
               func
               (List.length func_targs)
               (List.length fargs)
               (box (list ~sep:sp (pp_term ctx)))
               fargs))
      | RType.TVar f_tvar ->
        (* |- f_tvar : (_ -> _ -> _ .. -> _) -> 'b *)
        let t_out = RType.get_fresh_tvar ctx.types in
        let tf = RType.fun_typ_pack argst t_out in
        (match RType.(unify (csub @ [ tf, RType.TVar f_tvar ])) with
        | Ok subs -> mk_app ~pos:t0.tpos ~typ:(Some t_out) t_func t_args, subs
        | Error e ->
          Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
          failwith "Type inference failure.")
      | _ as tf ->
        if List.length fargs > 0
        then
          Log.loc_fatal_errmsg
            eloc
            (Fmt.str
               "Type inference failure: in %a, could not type %a as function."
               (pp_term ctx)
               t
               RType.pp
               tf)
        else t_func, c_func)
    | TData (cstr, args) ->
      (match RType.type_of_variant ctx.types cstr with
      | Some (tout, targs) ->
        let t_args, c_args = List.unzip (List.map ~f:aux args) in
        (match List.zip targs (List.map ~f:(fun term -> term.ttyp) t_args) with
        | Ok pairs ->
          (match RType.unify (pairs @ RType.mkv (List.concat c_args)) with
          | Ok subs -> { t0 with ttyp = tout; tkind = TData (cstr, t_args) }, subs
          | Error e ->
            Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
            Log.loc_fatal_errmsg
              eloc
              (Fmt.str
                 "Type inference failure: could not unify %s arguments %a."
                 cstr
                 (list ~sep:comma (pp_term ctx))
                 t_args))
        | Unequal_lengths ->
          Log.loc_fatal_errmsg
            eloc
            (Fmt.str
               "Type inference failure: could not match %s arguments: %a and %a."
               cstr
               (list ~sep:comma (pp_term ctx))
               t_args
               (list ~sep:comma RType.pp)
               targs))
      | None ->
        Log.loc_fatal_errmsg
          eloc
          Fmt.(str "Type inference failure: could not find type of %s." cstr))
    | TMatch (t, cases) ->
      let t_t, c_t = aux t in
      let per_case (pat, rhs) =
        let pat_term, pat_c = aux (term_of_pattern ctx pat) in
        match RType.(unify [ pat_term.ttyp, t_t.ttyp ]) with
        | Ok subs ->
          let t_branch, c_branch = aux rhs in
          (pat, t_branch), merge_subs (merge_subs c_t pat_c) (merge_subs c_branch subs)
        | Error _ ->
          Log.loc_fatal_errmsg
            eloc
            Fmt.(str "Type inference failure: could unify match case")
      in
      let f (cases, subs, t) ((pat, rhs), sub) =
        match RType.unify ([ t, rhs.ttyp ] @ RType.mkv subs) with
        | Ok s -> cases @ [ pat, rhs ], merge_subs s sub, rhs.ttyp
        | Error _ ->
          Log.loc_fatal_errmsg
            eloc
            Fmt.(str "Type inference failure: could unify match case")
      in
      (match List.map ~f:per_case cases with
      | [] -> failwith "Empty match case?"
      | ((hd_pat, hd_rhs), sub1) :: tl ->
        let cases, subs, typ =
          List.fold ~f ~init:([ hd_pat, hd_rhs ], sub1, hd_rhs.ttyp) tl
        in
        { t0 with ttyp = typ; tkind = TMatch (t_t, cases) }, subs)
  in
  let t', subs = aux t in
  match RType.unify (RType.mkv subs) with
  | Ok merge_subs ->
    let tsubs = RType.mkv merge_subs in
    rewrite_types ctx tsubs t', merge_subs
  | Error e ->
    Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
    Log.loc_fatal_errmsg t'.tpos "Could not infer type."
;;

(** Erase the types stored in the term AST (replaced them by a fresh
    type variable in the environment).*)
let erase_term_type (ctx : Context.t) (t : term) =
  let f t = { t with ttyp = RType.get_fresh_tvar ctx.types } in
  transform_info ~f t
;;

let type_of (t : term) = t.ttyp

(* ============================================================================================= *)
(*                                  SETS OF TERMS                                                *)
(* ============================================================================================= *)

(** This module aggregates operations on terms useful to build data structures and small terms.
*)
module Terms = struct
  module E = struct
    type t = term [@@deriving hash]

    let compare t1 t2 =
      let c = compare (term_size t1) (term_size t2) in
      if c = 0 then term_compare t1 t2 else c
    ;;

    let equal = term_equal
    let sexp_of_t = sexp_of_term
  end

  include E
  module C = Comparator.Make (E)
  include C

  let substs_of_alist (alist : (variable * term) list) : (term * term) list =
    List.map ~f:(fun (a, b) -> mk_var_no_ctx a, b) alist
  ;;

  (* Term building shortcuts. *)

  (** Create a term equal to the addition two terms. *)
  let ( + ) : t -> t -> t = mk_bin Binop.Plus

  (** Create a term equal to the substraction of two terms. *)
  let ( - ) : t -> t -> t = mk_bin Binop.Minus

  (** Create a term equal to the multiplication two terms. *)
  let ( * ) : t -> t -> t = mk_bin Binop.Times

  (** Create a term equal to the division two terms. *)
  let ( / ) : t -> t -> t = mk_bin Binop.Div

  (** Create a term equal to the disjunction of two terms. *)
  let ( || ) : t -> t -> t = mk_bin Binop.Or

  (** Create a term equal to the conjuction of two terms. *)
  let ( && ) : t -> t -> t = mk_bin Binop.And

  (** Create a term equal to the > comparison of two terms. *)
  let ( > ) : t -> t -> t = mk_bin Binop.Gt

  (** Create a term equal to the >= comparison of two terms. *)
  let ( >= ) : t -> t -> t = mk_bin Binop.Ge

  (** Create a term equal to the <= comparison of two terms. *)
  let ( <= ) : t -> t -> t = mk_bin Binop.Le

  (** Create a term equal to the < comparison of two terms. *)
  let ( < ) : t -> t -> t = mk_bin Binop.Lt

  (** Create a term equal to the equality of two terms. *)
  let ( == ) : t -> t -> t = mk_bin Binop.Eq

  (** Create a term equivalent to the implication of two terms  *)
  let ( => ) t1 t2 : t = mk_bin Binop.Or (mk_un Unop.Not t1) t2

  (** Create a term equal to the max of two terms. *)
  let max : t -> t -> t = mk_bin Binop.Max

  (** Create a term equal to the min of two terms. *)
  let min : t -> t -> t = mk_bin Binop.Min

  (** Create an integer constant term. *)
  let int (i : int) : t = mk_const (Constant.of_int i)

  (** Create a char constant term.*)
  let char (c : char) : t = mk_const (Constant.of_char c)

  (** Create a boolean constant term. *)
  let bool (b : bool) : t = mk_const (Constant.of_bool b)

  (** Create a negation of a term. *)
  let not (t : term) : t = mk_un Unop.Not t

  (** Create an if-then-else term. *)
  let ite : t -> t -> t -> t = mk_ite

  (** Create a term from a variable.  *)
  let ( ~^ ) : variable -> t = mk_var_no_ctx

  (** Create a constant: an emptyset of elements of a given type. *)
  let emptyset typ = mk_const (Constant.CEmptySet typ)

  (**
    Infers the type of the term, and returns the term with the correct types
    assigned.
  *)
  let typed (ctx : Context.t) (te : t) : t = fst (infer_type ctx te)
end

module KeyedTerms = struct
  module E = struct
    type t = term * term option

    let compare (t1, o1) (t2, o2) =
      let c = Terms.compare t1 t2 in
      if c = 0 then Option.compare Terms.compare o1 o2 else c
    ;;

    let equal (t1, o1) (t2, o2) = Terms.equal t1 t2 && (Option.equal Terms.equal) o1 o2
    let sexp_of_t (t1, o1) = Sexp.List [ sexp_of_term t1; sexp_of_option sexp_of_term o1 ]
  end

  include E
  module C = Comparator.Make (E)
  include C
end
