open Base
open Lexing
open Utils


let _tid = ref 0


type ident = string


type typekind =
  | TyInt
  | TyBool
  | TyString
  | TyChar
  | TyFun of type_term * type_term
  | TyTyp of ident
  | TyParam of ident
  | TyConstr of (type_term list) * type_term
  | TySum of type_term list
  | TyVariant of ident * (type_term list)

and type_term = { pos : position * position; tkind : typekind }

let dummy_loc = Lexing.dummy_pos , Lexing.dummy_pos
let mk_t_int pos = {pos; tkind = TyInt}
let mk_t_bool pos = {pos; tkind = TyBool}
let mk_t_string pos = {pos; tkind = TyString}
let mk_t_char pos = {pos; tkind = TyChar }
let mk_t_typ pos t = {pos; tkind = TyTyp t }
let mk_t_param pos t = {pos; tkind = TyParam t }
let mk_t_constr pos tl t = {pos; tkind = TyConstr(tl, t)}
let mk_t_sum pos t = {pos; tkind = TySum t }
let mk_t_variant pos c t = {pos; tkind = TyVariant (c, t) }
let mk_t_fun pos t1 t2 = {pos; tkind = TyFun(t1, t2) }


type t =
  | TInt
  | TBool
  | TString
  | TChar
  | TAnon
  | TNamed of ident
  | TTup of t list
  | TFun of t * t
  | TPoly of ident list * t


(**
   This hashtable maps type names to the type term of their declaration.
   It is initialized with the builtin types int, bool, char and string.
*)
let _types : (ident, ident list * type_term) Hashtbl.t =
  Hashtbl.of_alist_exn (module String)
    [ "int", ([], mk_t_int dummy_loc);
      "bool", ([], mk_t_bool dummy_loc);
      "char", ([], mk_t_char dummy_loc);
      "string", ([], mk_t_string dummy_loc)]

(**
   This hashtable maps variant names to the type name.
   Variant names must be unique!
*)
let _variants : (string, string) Hashtbl.t = Hashtbl.create (module String)

(* Add the builtin types *)

let add_variant ~(variant : string) ~(typename: string) =
  Hashtbl.add _variants ~key:variant ~data:typename

let add_type ?(params: ident list = [])  ~(typename: string) (tterm : type_term) =
  let add_only () =
    match Hashtbl.add _types ~key:typename ~data:(params, tterm) with
    | `Ok -> Ok ()
    | `Duplicate ->
      Error Log.(satom (Fmt.str "Type %s already declared" typename) @! tterm.pos)
  in
  let add_with_variants variants =
    Result.(
      List.fold_result ~init:[]
        ~f:(fun l variant ->
            match variant.tkind with
            | TyVariant (n, _) -> Ok ((n, variant.pos)::l)
            | _ ->
              Error Log.((satom "Sum types should only have constructor variants.") @! variant.pos))
        variants
      >>=
      List.fold_result ~init:()
        ~f:(fun _ (vname, pos) ->
            match add_variant ~variant:vname ~typename with
            | `Ok -> Ok ()
            | `Duplicate ->
              Error Log.((satom Fmt.(str "Variant %s already declared" vname)) @! pos))
      >>=
      (fun _ -> add_only ()))
  in
  match tterm.tkind with
  | TySum variants ->
    add_with_variants variants
  | _ -> add_only ()

let type_of_variant (variant : string) : t option =
  match Hashtbl.find _variants variant with
  | Some tname ->
    (match Hashtbl.find _types tname with
     | Some _ -> Some (TNamed tname)
     | _ -> None)
  | None -> None


(* ============================================================================================= *)
let substitute ~(old:t) ~(by:t) ~(in_: t) =
  let rec s ty =
    if Poly.(ty = old) then by else
      match ty with
      | TInt | TBool | TChar | TAnon | TString | TNamed _ -> ty
      | TTup tl -> TTup (List.map ~f:s tl)
      | TFun (a,b) -> TFun(s a, s b)
      | TPoly(params, t) -> TPoly(params, s t)
  in s in_

let sub_all (subs : (t * t) list) (ty : t) =
  List.fold ~f:(fun acc (old, by) -> substitute ~old ~by ~in_:acc) ~init:ty subs

let rec subtype_of (t1 : t) (t2 : t) =
  if Poly.(t1 = t2) then true else
    match t1, t2 with
    | TFun(a1, b1), TFun(a2, b2) -> subtype_of a2 a1 && subtype_of b1 b2
    | TTup tl1, TTup tl2 ->
      let f a b = subtype_of a b in
      (match List.for_all2 ~f tl1 tl2 with
       | Ok b -> b
       | Unequal_lengths -> false)
    | TPoly (p1, t1'), TPoly (p2, t2') ->
      (match List.map2 p1 p2 ~f:(fun a b -> (TNamed a, TNamed b)) with
       | Ok subs -> subtype_of (sub_all subs t1') t2'
       | Unequal_lengths -> false)
    | _ -> false