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
  | TNamed of ident
  | TTup of t list
  | TFun of t * t
  | TParam of t list * t
  | TVar of int

let _tvar_idx = ref 0
let get_fresh_tvar () = Int.incr _tvar_idx; TVar (!_tvar_idx)

let rec pp (frmt : Formatter.t) (typ : t) =
  match typ with
  | TInt -> Fmt.(pf frmt "int")
  | TBool -> Fmt.(pf frmt "bool")
  | TString -> Fmt.(pf frmt "string")
  | TChar -> Fmt.(pf frmt "char")
  | TNamed s -> Fmt.(pf frmt "%s" s)
  | TTup tl -> Fmt.(pf frmt "%a" (parens (list ~sep:Utils.ast pp)) tl)
  | TFun (tin, tout) -> Fmt.(pf frmt "%a -> %a" pp tin pp tout)
  | TParam (alpha, t') ->
    Fmt.(pf frmt "%a[%a]" pp t' (list ~sep:comma pp) alpha)
  | TVar i -> Fmt.(pf frmt "α%i" i)




let instantiate_variant (vargs : type_term list) (instantiator : (ident * int) list) =
  let rec variant_arg tt =
    match tt.tkind with
    | TyInt -> TInt | TyBool -> TBool | TyChar -> TChar | TyString -> TString
    | TyFun (tin, tout) -> TFun(variant_arg tin, variant_arg tout)
    | TyTyp e -> TNamed e
    | TyParam x ->
      (match List.Assoc.find instantiator ~equal:String.equal x with
       | Some i -> TVar i
       | None -> Log.loc_fatal_errmsg tt.pos "Unknown type parameter.")
    | TySum _ -> Log.loc_fatal_errmsg tt.pos "Variant is a sum type."
    | TyVariant (_, tl) -> TTup(List.map ~f:variant_arg tl)
    | TyConstr (params, te) -> TParam(List.map ~f:variant_arg params, variant_arg te)
  in
  List.map vargs ~f:variant_arg



(**
   This hashtable maps type names to the type term of their declaration.
   It is initialized with the builtin types int, bool, char and string.
*)
let _types : (ident, t) Hashtbl.t =
  Hashtbl.of_alist_exn (module String)
    [ "Int", TInt; "Bool", TBool; "Char", TChar; "String", TString ]

(**
   This hashtable maps variant names to the type name.
   Variant names must be unique!
*)
let _variant_to_tname : (string, string) Hashtbl.t = Hashtbl.create (module String)

let _tname_to_variants : (string, string list) Hashtbl.t = Hashtbl.create (module String)

let _variants : (string, t * t list) Hashtbl.t = Hashtbl.create (module String)

let type_of_variant (variant : string) : (t * t list) option = Hashtbl.find _variants variant


(* Add the builtin types *)
let add_variant ~(variant : string) ~(typename: string) (vdec : t * t list) =
  Hashtbl.add_exn _variants ~key:variant ~data:vdec;
  Hashtbl.add_multi _tname_to_variants ~key:typename ~data:variant;
  Hashtbl.add_exn _variant_to_tname ~key:variant ~data:typename


let add_all_variants
    ~(params : (ident * int) list)
    ~(main_type : t)
    ~(typename : ident)
    (tl : type_term list) : unit =
  let add_one_variant m_variant =
    match m_variant.tkind with
    | TyVariant (vname, vdef) ->
      let vargs = instantiate_variant vdef params in
      add_variant ~variant:vname ~typename (main_type, vargs)

    | _ -> failwith "Unexpected variant form."
  in
  List.iter ~f:add_one_variant tl

let get_type (typename : string) = Hashtbl.find _types typename

(**
   `add_type ?params ~typename tterm` adds a type with name typename and parameters
   params (default empty list) and type term tterm to the global store.
*)
let add_type ?(params: ident list = [])  ~(typename: string) (tterm : type_term) =
  let ty_params_inst =
    List.map
      ~f:(fun s ->
          match get_fresh_tvar () with
          |TVar i -> s, i
          | _ -> failwith "unexpected")
      params
  in
  let main_type =
    match params with
    | [] -> TNamed typename
    | _ -> TParam(List.map ~f:(fun (_,b) -> TVar b) ty_params_inst, TNamed typename)
  in
  let add_only () =
    match Hashtbl.add _types ~key:typename ~data:main_type with
    | `Ok -> Ok ()
    | `Duplicate ->
      Error Log.(satom (Fmt.str "Type %s already declared" typename) @! tterm.pos)
  in
  match tterm.tkind with
  | TySum variants ->
    add_all_variants ~params:ty_params_inst
      ~typename ~main_type variants;
    add_only ()
  | _ -> add_only ()



(* ============================================================================================= *)


let substitute ~(old:t) ~(by:t) ~(in_: t) =
  let rec s ty =
    if Poly.(ty = old) then by else
      match ty with
      | TInt | TBool | TChar | TString | TNamed _ | TVar _ -> ty
      | TTup tl -> TTup (List.map ~f:s tl)
      | TFun (a,b) -> TFun(s a, s b)
      | TParam(params, t) -> TParam(List.map ~f:s params, s t)
  in s in_

let sub_all (subs : (t * t) list) (ty : t) =
  List.fold_right ~f:(fun (old, by) acc -> substitute ~old ~by ~in_:acc) ~init:ty subs

let rec subtype_of (t1 : t) (t2 : t) =
  if Poly.(t1 = t2) then true else
    match t1, t2 with
    | TFun(a1, b1), TFun(a2, b2) -> subtype_of a2 a1 && subtype_of b1 b2
    | TTup tl1, TTup tl2 ->
      let f a b = subtype_of a b in
      (match List.for_all2 ~f tl1 tl2 with
       | Ok b -> b
       | Unequal_lengths -> false)
    | TParam (p1, t1'), TParam (p2, t2') ->
      (match List.zip p1 p2 with
       | Ok subs -> subtype_of (sub_all subs t1') t2'
       | Unequal_lengths -> false)
    | _ -> false


let rec occurs (x : int) (typ : t) : bool =
  match typ with
  | TInt | TBool | TString | TChar | TNamed _ -> false
  | TTup tl -> List.exists ~f:(occurs x) tl
  | TFun (tin, tout) -> occurs x tin || occurs x tout
  | TParam (param, te) -> List.exists ~f:(occurs x) param || occurs x te
  | TVar y -> x = y

type substitution = (int * t) list

(* unify one pair *)
let rec unify_one (s : t) (t : t) : substitution option =
  match (s, t) with
  | TVar x, TVar y -> if x = y then Some [] else Some [(x, t)]
  | TFun (f, sc), TFun (g, tc) ->
    Option.(unify_one f g >>=
            (fun u1 -> match unify_one sc tc with
               | Some u2 -> unify ((mkv u1) @ (mkv u2))
               | None ->
                 (Log.verbose
                    (fun frmt () ->
                       Fmt.(pf frmt "Type unification: cannot unify %a and %a.") pp s pp t);
                  None)))
  | TParam(params1, t1), TParam(params2, t2) ->
    (match List.zip (params1 @ [t1]) (params2 @ [t2]) with
     | Ok pairs -> unify pairs
     | Unequal_lengths -> (Log.verbose
                             (fun frmt () ->
                                Fmt.(pf frmt "Type unification: cannot unify %a and %a.") pp s pp t);
                           None))

  | TTup tl1, TTup tl2 ->
    (match List.zip tl1 tl2 with
     | Ok tls -> unify tls
     | Unequal_lengths ->
       Log.error
         (fun frmt () ->
            Fmt.(pf frmt "Type unification: Tuples %a and %a have different sizes") pp s pp t);
       None)
  | (TVar x, t' | t', TVar x) ->
    if occurs x t'
    then
      (Log.verbose
         (fun frmt () ->
            Fmt.(pf frmt "Type unification: circularity %a - %a") pp s pp t);
       None)
    else Some [x, t']
  | _ -> if Poly.equal s t then Some [] else
      (Log.verbose
         (fun frmt () ->
            Fmt.(pf frmt "Type unification: cannot unify %a and %a") pp s pp t);
       None)

and mkv = List.map ~f:(fun (a, b) -> (TVar a, b))
(* unify a list of pairs *)
and unify (s : (t * t) list) : substitution option =
  match s with
  | [] -> Some []
  | (x, y) :: t ->
    Option.(unify t
            >>=(fun t2 -> unify_one (sub_all (mkv t2) x) (sub_all (mkv t2) y)
                 >>= (fun t1 -> Some (t1 @ t2))))

let merge_subs loc (s : substitution) (t : substitution) : substitution =
  match unify (List.map ~f:(fun (a,b) -> TVar a, b) (s @ t)) with
  | Some subs -> subs
  | None -> Log.loc_fatal_errmsg loc "Error merging constraints."


(* ============================================================================================= *)
(**
   get_variants t returns a list of variants of a given types.
   If the type is a sum type with constructore, it returns a list of paris
   of constructor, list of the types of the arguments of the constructor.
   If the type has no variant, the list is empty.
*)
let get_variants (typ : t) : (string * t list) list =
  let variantnames params tname =
    let f variant_name =
      let var_args =
        match type_of_variant variant_name with
        | Some (typ', tl') ->
          (match typ' with
           | TParam(params', _) ->
             (match unify (List.zip_exn params params') with
              | Some subs -> List.map ~f:(sub_all (mkv subs)) tl'
              | None -> failwith "TODO")
           | TNamed _ -> tl'
           | _ -> failwith "Unexpexcted"
          )
        | None -> []
      in
      variant_name, var_args
    in
    match Hashtbl.find _tname_to_variants tname with
    | Some variants -> List.map ~f variants
    | None -> []
  in
  match typ with
  | TParam (params, TNamed tname) -> variantnames params tname
  | TNamed tname -> variantnames [] tname
  | _ -> []


(* ============================================================================================= *)
(*                                  REDUCTION / TRANSFORMATION                                   *)
(* ============================================================================================= *)

let reduce ~(case : (t -> t) -> t -> 'a option) ~(init :'a) ~(join: 'a -> 'a -> 'a) t :'a =
  let rec aux t =
    match case aux t with
    | Some value -> value
    | None ->
      (match t with
       | TInt | TBool | TString | TChar -> init
       | TNamed _ -> init
       | TVar _ -> init
       | TTup tl -> List.fold ~init ~f:(fun acc t' -> join acc (aux t')) tl
       | TFun (tin, tout) -> join (aux tin) (aux tout)
       | TParam (_, t) -> aux t)
  in
  aux t

let is_recursive =
  let case f t =
    match get_variants t with 
    | [] -> None
    | l -> 
      (if List.exists l ~f:(is_recursive_variant t)
  in
  reduce ~case ~init:false ~join:(||)