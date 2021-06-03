open Base
open Fmt
open Utils

(* Misc. attributes, qualified names. *)

type d_attribute = string option

type d_qualified_name = string list

type d_decl_modifier = DDeclMAbstract | DDeclMGhost | DDeclMStatic

type d_generic_param = string option

(* Types *)
type d_typename_segment = string * d_generic_param

type d_domain_type =
  | DTyBool
  | DTyChar
  | DTyInt
  | DTyReal
  | DTyOrdinal
  | DTyBitVector
  | DTyObject
  | DTyFiniteSet of d_domain_type
  | DTyInfiniteSet of d_domain_type
  | DTyMultiset of d_domain_type
  | DTyFiniteMap of d_domain_type * d_domain_type
  | DTyInfiniteMap of d_domain_type * d_domain_type
  | DTySequence of d_domain_type
  | DTyNat
  | DTyString
  | DTyArray
  | DTyTuple of d_domain_type list
  | DTyNamed of d_typename_segment list

(* Class member declarations. *)
type d_class_member_decl = DCConstantField | DCFunction | DCMethod

(* Toplevel objects. *)

let d_decl_modifier_to_string d =
  match d with DDeclMAbstract -> "abstract" | DDeclMGhost -> "ghost" | DDeclMStatic -> "static"

(* Top lelevel declarations.  *)
type d_toplevel = { dt_modifiers : d_decl_modifier list; dt_kind : d_decl_kind }

(** The kinds of top level declarations in a Dafny program. *)
and d_decl_kind =
  | DSubModuleDefinition of
      d_attribute list * d_qualified_name * d_qualified_name option * d_toplevel list
      (** DSubModuleDefinition(attributes, name, refname, declarations)  is the submodule definition
      "module" attributes name (refines refname) { declarations }
      *)
  | DSubModuleImport of bool * d_qualified_name
      (** DSubModuleImport(false, qname) is "import qname".
          DSubModuleImport(true, qname) is "import opened qname".
          TODO: consider the different kind of imports in the reference.
     *)
  | DSubModuleExport  (** TODO: implement module exports. Probably not needed for this project.  *)
  | DClassDecl of d_attribute list * string * d_generic_param * d_toplevel list
      (** A class declaration. Todo: add the "extends".  *)
  | DDatatypeDecl
  | DNewtypeDecl
  | DSynonymTypeDecl
  | DIteratorDecl
  | DTraitDecl
  | DClassMemberDecl of d_class_member_decl

type d_program = { dp_includes : string list; dp_topdecls : d_toplevel list }
(**  A Dafny program is a list of includes followed by a list of toplevel declarations. *)

(* ============================================================================================= *)
(*                                         Pretty Printing                                       *)
(* ============================================================================================= *)
let pp_d_typename_segment (fmt : Formatter.t) ((t, p) : d_typename_segment) : unit =
  match p with Some param -> pf fmt "%s<%s>" t param | None -> string fmt t

let rec pp_d_domain_type (fmt : Formatter.t) = function
  | DTyBool -> pf fmt "bool"
  | DTyChar -> pf fmt "char"
  | DTyInt -> pf fmt "int"
  | DTyReal -> pf fmt "real"
  | DTyOrdinal -> failwith "Dafny ORDINAL type not supported"
  | DTyBitVector -> pf fmt "bitvector"
  | DTyObject -> failwith "Dafny Object type not supported"
  | DTyFiniteSet t -> pf fmt "set<%a>" pp_d_domain_type t
  | DTyInfiniteSet t -> pf fmt "iset<%a>" pp_d_domain_type t
  | DTyMultiset t -> pf fmt "multiset<%a>" pp_d_domain_type t
  | DTyFiniteMap (t, u) -> pf fmt "map<%a,%a>" pp_d_domain_type t pp_d_domain_type u
  | DTyInfiniteMap (t, u) -> pf fmt "imap<%a,%a>" pp_d_domain_type t pp_d_domain_type u
  | DTySequence t -> pf fmt "seq<%a>" pp_d_domain_type t
  | DTyNat -> pf fmt "nat"
  | DTyString -> pf fmt "string"
  | DTyArray -> pf fmt "array"
  | DTyTuple tl -> (list ~sep:comma pp_d_domain_type) fmt tl
  | DTyNamed tsl -> (list ~sep:comma pp_d_typename_segment) fmt tsl

let pp_attribute = option string

let pp_d_qualified_name (fmt : Formatter.t) (qn : d_qualified_name) : unit =
  pf fmt "%a" (list ~sep:(fun fmt () -> pf fmt ".") string) qn

let pp_d_decl_modifier (fmt : Formatter.t) (dm : d_decl_modifier) : unit =
  pf fmt "%s" (d_decl_modifier_to_string dm)

let pp_d_includes (fmt : Formatter.t) (includes : string list) : unit =
  List.iter includes ~f:(fun iname -> pf fmt "include %s" iname)

let rec pp_d_decl_kind (fmt : Formatter.t) (dk : d_decl_kind) : unit =
  match dk with
  | DSubModuleDefinition (attributes, qname, maybe_refines, decls) ->
      pf fmt "@[<hov 2>@[module %a %a%a@;@]{@;@[%a@]@;}@]" (list ~sep:sp pp_attribute) attributes
        pp_d_qualified_name qname
        (option (fun fmt qn -> pf fmt " refines %a" pp_d_qualified_name qn))
        maybe_refines (list ~sep:sp pp_d_toplevel) decls
  | DSubModuleImport (opened, qname) ->
      if opened then pf fmt "@[import %a@]" pp_d_qualified_name qname
      else pf fmt "@[import opened %a@]" pp_d_qualified_name qname
  | DClassDecl (attributes, name, generics, decls) ->
      pf fmt "@[<hov 2>@[class%a%s%a@]@;{@;@[%a@]}"
        (list_or_space ~sep:sp ~f:pp_attribute)
        attributes name (option_or_space ~f:string) generics (list ~sep:sp pp_d_toplevel) decls
  | _ -> ()

and pp_d_toplevel (fmt : Formatter.t) (topl : d_toplevel) : unit =
  match topl.dt_modifiers with
  | [] -> pp_d_decl_kind fmt topl.dt_kind
  | _ as l ->
      pf fmt "@[<hov 2>%a %a@]" (list ~sep:sp pp_d_decl_modifier) l pp_d_decl_kind topl.dt_kind
