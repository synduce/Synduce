open Base
open Lexing
open Utils

let _tid = ref 0
let _tvar_idx = ref 0

type ident = string

(** Used for type declarations*)
type typekind =
  | TyInt (** The int type.*)
  | TyBool (** The bool type. *)
  | TyString (** The string type. *)
  | TyChar (** The char type. *)
  | TyFun of type_term * type_term (** The function type. *)
  (* Non-standard *)
  | TySet of type_term
  (* User defined*)
  | TyTyp of ident
  | TyParam of ident
  | TyConstr of type_term list * type_term
  | TySum of type_term list
  | TyVariant of ident * type_term list

and type_term =
  { pos : position * position
  ; tkind : typekind
  }

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos
let mk_t_int pos = { pos; tkind = TyInt }
let mk_t_bool pos = { pos; tkind = TyBool }
let mk_t_string pos = { pos; tkind = TyString }
let mk_t_char pos = { pos; tkind = TyChar }
let mk_t_set pos t = { pos; tkind = TySet t }
let mk_t_typ pos t = { pos; tkind = TyTyp t }
let mk_t_param pos t = { pos; tkind = TyParam t }
let mk_t_constr pos tl t = { pos; tkind = TyConstr (tl, t) }
let mk_t_sum pos t = { pos; tkind = TySum t }
let mk_t_variant pos c t = { pos; tkind = TyVariant (c, t) }
let mk_t_fun pos t1 t2 = { pos; tkind = TyFun (t1, t2) }

type t =
  | TInt
  | TBool
  | TString
  | TChar
  | TNamed of string
  | TTup of t list
  | TSet of t
  | TFun of t * t
  | TParam of t list * t
  | TVar of int
[@@deriving sexp, hash, ord, eq, show]

let rec pp (frmt : Formatter.t) (typ : t) =
  match typ with
  | TInt -> if !Config.math_display then Fmt.(pf frmt "ℤ") else Fmt.(pf frmt "int")
  | TBool -> if !Config.math_display then Fmt.(pf frmt "𝐁") else Fmt.(pf frmt "bool")
  | TString ->
    if !Config.math_display then Fmt.(pf frmt "String") else Fmt.(pf frmt "string")
  | TChar -> if !Config.math_display then Fmt.(pf frmt "𝐂") else Fmt.(pf frmt "char")
  | TNamed s -> Fmt.(pf frmt "%s" s)
  | TTup tl -> Fmt.(pf frmt "%a" (parens (list ~sep:Utils.ast pp)) tl)
  | TFun (tin, tout) -> Fmt.(pf frmt "(%a ⟶  %a)" pp tin pp tout)
  | TSet t' ->
    if !Config.math_display
    then Fmt.(pf frmt "{%a}" pp t')
    else Fmt.(pf frmt "@[%a@;set@]" pp t')
  | TParam (alpha, t') ->
    (match alpha with
    | [] ->
      if !Config.math_display
      then Fmt.(pf frmt "ɑ? %a" pp t')
      else Fmt.(pf frmt "param? %a" pp t')
    | [ a ] -> Fmt.(pf frmt "%a %a" pp a pp t')
    | _ -> Fmt.(pf frmt "(%a) %a" (list ~sep:comma pp) alpha pp t'))
  | TVar i ->
    if !Config.math_display
    then Fmt.(pf frmt "α%s" (to_subscript_unicode i))
    else Fmt.(pf frmt "param%i" i)
;;

(**
  [fun_type_unpack t] uncurries the type [t] as a function. Returns a pair of
  a list (the type of the arguments of the function) and a type (the output
  type of the function). If t is not a function type, then ([],t) is returned.
*)
let fun_typ_unpack (t : t) : t list * t =
  let rec aux pre = function
    | TFun (a, b) -> aux (pre @ [ a ]) b
    | _ as _t -> pre, _t
  in
  let targs, tout = aux [] t in
  targs, tout
;;

(** [fun_typ_pack args t] construct the type of a function args -> t:
    for example [fun_typ_pack [t1;t2] t_out] is [TFun(t1, TFun(t2, tout))].
*)
let rec fun_typ_pack (targs : t list) (tout : t) =
  match List.last targs, List.drop_last targs with
  | Some elt, Some pre -> fun_typ_pack pre (TFun (elt, tout))
  | _ -> tout
;;

(* ============================================================================================= *)
(* Type names and storage *)

let rec base_name (typ : t) : string option =
  match typ with
  | TParam (_, t) -> base_name t
  | TNamed s -> Some s
  | TInt -> Some "Int"
  | TBool -> Some "Bool"
  | TChar -> Some "Char"
  | TString -> Some "String"
  | _ -> None
;;

let rec smt_name (typ : t) : string option =
  match typ with
  | TParam (_, t) -> smt_name t
  | TNamed s -> Some s
  | TInt -> Some "Int"
  | TBool -> Some "Bool"
  | TChar -> Some "Char"
  | TString -> Some "String"
  | TVar _ -> Some "Int"
  | _ -> None
;;

let _unsafe_pair_t_ t = Option.value_exn (base_name t), t

(**
   Type environment module
*)

type env =
  { variant_typenames : (string, string) Hashtbl.t
  ; typenames_variants : (string, string list) Hashtbl.t
  ; variant_types : (string, t * t list) Hashtbl.t
  ; typenames : (string, t) Hashtbl.t
  ; idx : int ref
  }

let create () =
  { variant_typenames = Hashtbl.create (module String)
  ; typenames_variants = Hashtbl.create (module String)
  ; variant_types = Hashtbl.create (module String)
  ; typenames =
      Hashtbl.of_alist_exn
        (module String)
        (List.map ~f:_unsafe_pair_t_ [ TInt; TBool; TChar; TString ])
  ; idx = ref 0
  }
;;

let copy (ctx : env) =
  { variant_typenames = Hashtbl.copy ctx.variant_typenames
  ; typenames_variants = Hashtbl.copy ctx.typenames_variants
  ; variant_types = Hashtbl.copy ctx.variant_types
  ; typenames = Hashtbl.copy ctx.typenames
  ; idx = ref !(ctx.idx)
  }
;;

let get_fresh_tvar (env : env) =
  Int.incr env.idx;
  TVar !(env.idx)
;;

let get_type (env : env) (typename : string) = Hashtbl.find env.typenames typename

let dump_types (frmt : Formatter.t) (env : env) =
  let f ~key ~data = Fmt.(pf frmt "@[? %s : %a@]@." key pp data) in
  Hashtbl.iteri env.typenames ~f;
  let f_v ~key ~data =
    let t, tl = data in
    Fmt.(pf frmt "@[ %s : %a -> %a @]@." key pp t (list ~sep:comma pp) tl)
  in
  Fmt.(pf frmt "@[Variants:@]@.");
  Hashtbl.iteri env.variant_types ~f:f_v
;;

let reinit (env : env) =
  Hashtbl.clear env.variant_types;
  Hashtbl.clear env.variant_typenames;
  Hashtbl.clear env.typenames_variants;
  _tid := 0;
  _tvar_idx := 0
;;

(* ============================================================================================= *)

let substitute ~(old : t) ~(by : t) ~(in_ : t) =
  let rec s ty =
    if Poly.(ty = old)
    then by
    else (
      match ty with
      | TInt | TBool | TChar | TString | TNamed _ | TVar _ -> ty
      | TTup tl -> TTup (List.map ~f:s tl)
      | TSet a -> TSet (s a)
      | TFun (a, b) -> TFun (s a, s b)
      | TParam (params, t) -> TParam (List.map ~f:s params, s t))
  in
  s in_
;;

let sub_all (subs : (t * t) list) (ty : t) =
  List.fold_right ~f:(fun (old, by) acc -> substitute ~old ~by ~in_:acc) ~init:ty subs
;;

let rec subtype_of (t1 : t) (t2 : t) =
  if Poly.(t1 = t2)
  then true
  else (
    match t1, t2 with
    | TFun (a1, b1), TFun (a2, b2) -> subtype_of a2 a1 && subtype_of b1 b2
    | TTup tl1, TTup tl2 ->
      let f a b = subtype_of a b in
      (match List.for_all2 ~f tl1 tl2 with
      | Ok b -> b
      | Unequal_lengths -> false)
    | TParam (p1, t1'), TParam (p2, t2') ->
      (match List.zip p1 p2 with
      | Ok subs -> subtype_of (sub_all subs t1') t2'
      | Unequal_lengths -> false)
    | _ -> false)
;;

let rec occurs (x : int) (typ : t) : bool =
  match typ with
  | TInt | TBool | TString | TChar | TNamed _ -> false
  | TTup tl -> List.exists ~f:(occurs x) tl
  | TSet a -> occurs x a
  | TFun (tin, tout) -> occurs x tin || occurs x tout
  | TParam (param, te) -> List.exists ~f:(occurs x) param || occurs x te
  | TVar y -> x = y
;;

(** A substitution is a list pairs of type variable id, type.
  Applied to a (parametric) type to subsitute tyep parameters with types.
 *)
type substitution = (int * t) list

(* unify one pair *)
let rec unify_one (s : t) (t : t) : (substitution, Sexp.t) Result.t =
  match s, t with
  | TVar x, TVar y -> if x = y then Ok [] else Ok [ x, t ]
  | TFun (f, sc), TFun (g, tc) ->
    Result.(
      unify_one f g
      >>= fun u1 ->
      (match unify_one sc tc with
      | Ok u2 -> unify (mkv u1 @ mkv u2)
      | Error e ->
        Error
          (Sexp.List
             [ e; Atom Fmt.(str "Type unification: cannot unify %a and %a." pp s pp t) ])))
  | TParam (params1, t1), TParam (params2, t2) ->
    (match List.zip (params1 @ [ t1 ]) (params2 @ [ t2 ]) with
    | Ok pairs -> unify pairs
    | Unequal_lengths ->
      Error (Sexp.Atom (Fmt.str "Type unification: cannot unify %a and %a." pp s pp t)))
  | TTup tl1, TTup tl2 ->
    (match List.zip tl1 tl2 with
    | Ok tls -> unify tls
    | Unequal_lengths ->
      let emsg =
        Fmt.(str "Type unification: Tuples %a and %a have different sizes" pp s pp t)
      in
      Error (Sexp.Atom emsg))
  | TSet t1, TSet t2 -> unify_one t1 t2
  | TVar x, t' | t', TVar x ->
    if occurs x t'
    then (
      let emsg = Fmt.(str "Type unification: circularity %a - %a" pp s pp t) in
      Error (Sexp.Atom emsg))
    else Ok [ x, t' ]
  | TParam ([], t1), _ -> unify_one t1 t
  | _, TParam ([], t2) -> unify_one s t2
  | _ ->
    if Poly.equal s t
    then Ok []
    else (
      let emsg = Fmt.(str "Type unification: cannot unify %a and %a" pp s pp t) in
      Error (Sexp.Atom emsg))

and mkv = List.map ~f:(fun (a, b) -> TVar a, b)

(* unify a list of pairs *)
and unify (s : (t * t) list) : (substitution, Sexp.t) Result.t =
  match s with
  | [] -> Ok []
  | (x, y) :: t ->
    Result.(
      unify t
      >>= fun t2 ->
      unify_one (sub_all (mkv t2) x) (sub_all (mkv t2) y) >>= fun t1 -> Ok (t1 @ t2))
;;

let merge_subs loc (s : substitution) (t : substitution) : substitution =
  match unify (List.map ~f:(fun (a, b) -> TVar a, b) (s @ t)) with
  | Ok subs -> subs
  | Error _ -> Log.loc_fatal_errmsg loc "Error merging constraints."
;;

let t_equals a b =
  match unify_one a b with
  | Ok _ -> true
  | _ -> false
;;

(* ============================================================================================= *)
(*                      VARIANT TYPES                                                            *)
(* ============================================================================================= *)

let type_of_variant (env : env) (variant : string) : (t * t list) option =
  Hashtbl.find env.variant_types variant
;;

let instantiate_variant (vargs : type_term list) (instantiator : (ident * int) list) =
  let rec variant_arg tt =
    match tt.tkind with
    | TyInt | TyTyp "int" -> TInt
    | TyBool | TyTyp "bool" -> TBool
    | TyChar | TyTyp "char" -> TChar
    | TyString | TyTyp "string" -> TString
    | TyFun (tin, tout) -> TFun (variant_arg tin, variant_arg tout)
    | TySet telt -> TSet (variant_arg telt)
    | TyTyp e -> TNamed e
    | TyParam x ->
      (match List.Assoc.find instantiator ~equal:String.equal x with
      | Some i -> TVar i
      | None -> Log.loc_fatal_errmsg tt.pos "Unknown type parameter.")
    | TySum _ -> Log.loc_fatal_errmsg tt.pos "Variant is a sum type."
    | TyVariant (_, tl) -> TTup (List.map ~f:variant_arg tl)
    | TyConstr (params, te) -> TParam (List.map ~f:variant_arg params, variant_arg te)
  in
  List.map vargs ~f:variant_arg
;;

(* Add the builtin types *)
let add_variant (env : env) ~(variant : string) ~(typename : string) (vdec : t * t list) =
  Hashtbl.add_exn env.variant_types ~key:variant ~data:vdec;
  Hashtbl.add_multi env.typenames_variants ~key:typename ~data:variant;
  Hashtbl.add_exn env.variant_typenames ~key:variant ~data:typename
;;

let add_all_variants
    (env : env)
    ~(params : (ident * int) list)
    ~(main_type : t)
    ~(typename : ident)
    (tl : type_term list)
    : unit
  =
  let add_one_variant m_variant =
    match m_variant.tkind with
    | TyVariant (vname, vdef) ->
      let vargs = instantiate_variant vdef params in
      add_variant env ~variant:vname ~typename (main_type, vargs)
    | _ -> failwith "Unexpected variant form."
  in
  List.iter ~f:add_one_variant tl
;;

(**
   get_variants t returns a list of variants of a given type.
   If the type is a sum type with constructors, it returns a list of pairs
   of constructors, list of the types of the arguments of the constructor.
   If the type has no variant, the list is empty.
*)
let get_variants (env : env) (typ : t) : (string * t list) list =
  let variantnames in_params tname =
    let f variant_name =
      let var_args =
        match type_of_variant env variant_name with
        | Some (typ', tl') ->
          (match typ' with
          | TParam (params', _) when List.length params' > 0 ->
            (match List.zip in_params params' with
            | Unequal_lengths ->
              Log.error_msg
                Fmt.(
                  str
                    "Internal type error: %a and %s should have the same numbers of \
                     parameters. "
                    pp
                    typ
                    tname);
              failwith "Type error : same variant with different number of args."
            | Ok l ->
              (match unify l with
              | Ok subs -> List.map ~f:(sub_all (mkv subs)) tl'
              | Error _ ->
                Log.error_msg
                  Fmt.(str "Internal type error: %a and %s should unify." pp typ tname);
                failwith "Type error."))
          | TNamed _ -> tl'
          | _ -> failwith "Unexpexcted")
        | None -> []
      in
      variant_name, var_args
    in
    match Hashtbl.find env.typenames_variants tname with
    | Some variants -> List.map ~f variants
    | None -> []
  in
  (* A type can be parametric, in which case we need to replace the type parameters
     in the definition by the type parameters in the input type.
  *)
  match typ with
  | TParam (params, TNamed tname) -> variantnames params tname
  | TNamed tname -> variantnames [] tname
  | _ -> []
;;

let is_recursive_variant (env : env) ((_, types) : string * t list) =
  let f t =
    match get_variants env t with
    | [] -> false
    | _ -> true
  in
  List.exists types ~f
;;

(**
   `add_type ?params ~typename tterm` adds a type with name typename and parameters
   params (default empty list) and type term tterm to the global store.
*)
let add_type
    (env : env)
    ?(params : ident list = [])
    ~(typename : string)
    (tterm : type_term)
  =
  let ty_params_inst =
    List.map
      ~f:(fun s ->
        match get_fresh_tvar env with
        | TVar i -> s, i
        | _ -> failwith "unexpected")
      params
  in
  let main_type =
    match params with
    | [] -> TNamed typename
    | _ -> TParam (List.map ~f:(fun (_, b) -> TVar b) ty_params_inst, TNamed typename)
  in
  let add_only () =
    match Hashtbl.add env.typenames ~key:typename ~data:main_type with
    | `Ok -> Ok ()
    | `Duplicate ->
      Error Log.(satom (Fmt.str "Type %s already declared" typename) @! tterm.pos)
  in
  match tterm.tkind with
  | TySum variants ->
    add_all_variants env ~params:ty_params_inst ~typename ~main_type variants;
    add_only ()
  | _ -> add_only ()
;;

(* ============================================================================================= *)
(*                                  REDUCTION / TRANSFORMATION                                   *)
(* ============================================================================================= *)

let reduce ~(case : (t -> 'a) -> t -> 'a option) ~(init : 'a) ~(join : 'a -> 'a -> 'a) t
    : 'a
  =
  let rec aux t : 'a =
    match case aux t with
    | Some value -> value
    | None ->
      (match t with
      | TInt | TBool | TString | TChar -> init
      | TNamed _ -> init
      | TVar _ -> init
      | TSet t -> aux t
      | TTup tl -> List.fold ~init ~f:(fun acc t' -> join acc (aux t')) tl
      | TFun (tin, tout) -> join (aux tin) (aux tout)
      | TParam (_, t) -> aux t)
  in
  aux t
;;

let rec is_base t =
  match t with
  | TInt | TBool | TChar | TString -> true
  | TSet t -> is_base t
  | _ -> false
;;

let is_function t =
  match t with
  | TFun _ -> true
  | _ -> false
;;

let is_recursive (env : env) =
  let case _ t =
    match get_variants env t with
    | [] -> None
    | l -> if List.exists l ~f:(is_recursive_variant env) then Some true else None
  in
  reduce ~case ~init:false ~join:( || )
;;

let is_datatype (env : env) t =
  match t with
  | TNamed tname | TParam (_, TNamed tname) ->
    (match Hashtbl.find env.typenames_variants tname with
    | Some (_ :: _) -> true
    | _ -> false)
  | _ -> false
;;

let rec is_user_defined t =
  match t with
  | TBool | TInt | TString | TChar | TVar _ -> false
  | TNamed _ | TTup _ | TParam _ -> true
  | TSet a -> is_user_defined a
  | TFun (a, b) -> is_user_defined a || is_user_defined b
;;

let get_datatype_depends (env : env) (t : t) =
  List.filter
    ~f:(fun t' -> is_user_defined t' && not (subtype_of t' t))
    (List.concat_map ~f:snd (get_variants env t))
;;
