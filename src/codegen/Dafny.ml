open Base
open Fmt
open Utils
open Lang

(*
  This module defines the Dafny input language.
  The reference can be found here: https://dafny-lang.github.io/dafny/DafnyRef/DafnyRef.html
  Types and constructor are prefixed with d_ and D respectively.
*)

(* Misc. attributes, qualified names. *)

type d_attribute =
  | DAttrAssumption
  | DAttrAutoReq
  | DAttrAutocontract
  | DAttrAxiom
  | DAttrCompile
  | DAttrDecl
  | DAttrFuel of string * int * int option
  | DAttrHeapQuantifier
  | DAttrImported
  | DAttrInduction of bool * string list
      (** DAttrInduction(b,l) is {:induction b} if l is empty, {:induction l} otherwise.
        l is a list consisting of bound variables.
        *)
  | DAttrLayerQuantifier
  | DAttrNaiveType of bool option * string option
  | DAttrOpaque of bool  (** DAttrOpaque b is opaque_full is b is true. *)
  | DAttrTailRecursion of bool
  | DAttrTimeLimitMultiplier of int
  | DAttrTypeQuantifier

type d_qualified_name = string list

type d_decl_modifier = DDeclMAbstract | DDeclMGhost | DDeclMStatic

let d_decl_modifier_to_string d =
  match d with DDeclMAbstract -> "abstract" | DDeclMGhost -> "ghost" | DDeclMStatic -> "static"

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

type d_ident_type = string * d_domain_type

(* Function and method bodies. *)
type d_body = string

(* Specs *)
type d_clause = Term.term

type d_spec_kind = DSpecMethod | DSpecFunction | DSpecLambda | DSpecIterator | DSpecLoop

type d_spec = {
  dpsec_kind : d_spec_kind;
  dspec_requires : d_clause list;
  dspec_ensures : d_clause list;
  dspec_decreases : d_clause list;
  (* For non-functional specifications. *)
  dspec_reads : d_clause list;
  dspec_modifies : d_clause list;
  (* For loops. *)
  dspec_invariant : d_clause;
}
(**
This type is used for all kinds of specifications. Some fields should not appear in some
specifications, for example if the spec if for a function, the modifies clauses will not be used.
*)

(* Class member declarations. *)
type d_method_signature = {
  dsig_params : string option;
  dsig_ktype : d_domain_type option;
  dsig_formals : d_ident_type list;
  dsig_returns : d_ident_type list;
}

type d_method_kind =
  | DMkMethod
  | DMkConstructor
  | DMkLemma of bool  (** DMkLemma(b) is "twostate lemma" if b is true, otherwise just "lemma" *)
  | DMkLemmaExtreme of bool
      (** DMkLemmaExtreme(true) is "greatest lemma", if false "least lemma" *)

type d_class_member_decl =
  | DClassConstantField of d_ident_type list
  | DClassFunction
  | DClassMethod of string * d_method_kind * d_attribute list * d_method_signature * d_spec * d_body

(* Toplevel objects. *)

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
  | DClassMemberDecl of d_decl_modifier * d_class_member_decl

type d_program = { dp_includes : string list; dp_topdecls : d_toplevel list }
(**  A Dafny program is a list of includes followed by a list of toplevel declarations. *)

(* ============================================================================================= *)
(*                                         Pretty Printing                                       *)
(* ============================================================================================= *)
let pp_d_typename_segment (fmt : Formatter.t) ((t, p) : d_typename_segment) : unit =
  match p with Some param -> pf fmt "%s<%s>" t param | None -> string fmt t

let pp_d_qualified_name (fmt : Formatter.t) (qn : d_qualified_name) : unit =
  pf fmt "%a" (list ~sep:(fun fmt () -> pf fmt ".") string) qn

let pp_d_decl_modifier (fmt : Formatter.t) (dm : d_decl_modifier) : unit =
  pf fmt "%s" (d_decl_modifier_to_string dm)

let pp_d_includes (fmt : Formatter.t) (includes : string list) : unit =
  List.iter includes ~f:(fun iname -> pf fmt "include %s" iname)

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

let pp_d_ident_type (fmt : Formatter.t) ((id, ty) : d_ident_type) : unit =
  pf fmt "%s : %a" id pp_d_domain_type ty

let pp_attribute (fmt : Formatter.t) (attr : d_attribute) : unit =
  match attr with
  | DAttrAssumption -> pf fmt "{:assumption}"
  | DAttrAutoReq -> pf fmt "{:autoReq}"
  | DAttrAutocontract -> pf fmt "{:autocontracts}"
  | DAttrAxiom -> pf fmt "{:axiom}"
  | DAttrCompile -> pf fmt "{:compile}"
  | DAttrDecl -> pf fmt "{:decl}"
  | DAttrFuel (fname, low_fuel, maybe_high_fuel) -> (
      match maybe_high_fuel with
      | Some hf -> pf fmt "{:fuel %s,%i,%i}" fname low_fuel hf
      | None -> pf fmt "{:fuel %s,%i}" fname low_fuel)
  | DAttrHeapQuantifier -> pf fmt "{:heapQuantifier}"
  | DAttrImported -> pf fmt "{:imported}"
  | DAttrInduction (b, l) -> (
      match l with
      | [] -> pf fmt "{:induction %b}" b
      | _ -> pf fmt "{:induction %a}" (list ~sep:sp string) l)
  | DAttrLayerQuantifier -> pf fmt "{:layerQuantifier}"
  | DAttrNaiveType (maybe_bool, maybe_typename) -> (
      match (maybe_bool, maybe_typename) with
      | Some b, None -> pf fmt "{:nativeType %b}" b
      | _, Some tname -> pf fmt "{:nativeType %s}" tname
      | _ -> pf fmt "{:nativeType}")
  | DAttrOpaque is_full -> if is_full then pf fmt "{:opaque_full}" else pf fmt "{:opaque}"
  | DAttrTailRecursion b -> pf fmt "{:tailRecursion %b}" b
  | DAttrTimeLimitMultiplier mult -> pf fmt "{:timeLimitMultiplier %i}" mult
  | DAttrTypeQuantifier -> pf fmt "{:typeQuantifier}"

let pp_clause (clause_name : string) (fmt : Formatter.t) (c : d_clause) : unit =
  pf fmt "@[%s %a@]" clause_name Term.pp_term c

let pp_d_spec (fmt : Formatter.t) (spec : d_spec) : unit =
  match spec.dpsec_kind with
  | DSpecMethod ->
      list ~sep:sp (pp_clause "modifies") fmt spec.dspec_modifies;
      list ~sep:sp (pp_clause "requires") fmt spec.dspec_requires;
      list ~sep:sp (pp_clause "ensures") fmt spec.dspec_ensures;
      list ~sep:sp (pp_clause "decreases") fmt spec.dspec_decreases
  | DSpecFunction ->
      list ~sep:sp (pp_clause "reads") fmt spec.dspec_reads;
      list ~sep:sp (pp_clause "requires") fmt spec.dspec_requires;
      list ~sep:sp (pp_clause "ensures") fmt spec.dspec_ensures;
      list ~sep:sp (pp_clause "decreases") fmt spec.dspec_decreases
  | DSpecLambda ->
      list ~sep:sp (pp_clause "reads") fmt spec.dspec_reads;
      list ~sep:sp (pp_clause "requires") fmt spec.dspec_requires
  | DSpecIterator ->
      list ~sep:sp (pp_clause "reads") fmt spec.dspec_reads;
      list ~sep:sp (pp_clause "yield requires") fmt spec.dspec_requires;
      list ~sep:sp (pp_clause "yield ensures") fmt spec.dspec_ensures;
      list ~sep:sp (pp_clause "decreases") fmt spec.dspec_decreases
  | DSpecLoop ->
      pf fmt "%a@;" (pp_clause "invariant") spec.dspec_invariant;
      list ~sep:sp (pp_clause "decreases") fmt spec.dspec_decreases;
      list ~sep:sp (pp_clause "modifies") fmt spec.dspec_modifies

let pp_d_body (fmt : Formatter.t) (body : d_body) : unit = pf fmt "%a" string body

let pp_d_method_signature (fmt : Formatter.t) (dsig : d_method_signature) : unit =
  (match dsig.dsig_params with Some p -> pf fmt "<%a>" string p | None -> ());
  pf fmt "(%a)@;returns (%a)" (list ~sep:comma pp_d_ident_type) dsig.dsig_formals
    (list ~sep:comma pp_d_ident_type) dsig.dsig_formals

let pp_d_method_kind (fmt : Formatter.t) (dmk : d_method_kind) : unit =
  match dmk with
  | DMkMethod -> pf fmt "method"
  | DMkConstructor -> pf fmt "constructor"
  | DMkLemma is_twostate -> if is_twostate then pf fmt "twostate lemma" else pf fmt "lemma"
  | DMkLemmaExtreme is_greatest ->
      if is_greatest then pf fmt "greatest lemma" else pf fmt "least lemma"

let pp_d_class_member_decl (fmt : Formatter.t) (decl : d_class_member_decl) : unit =
  match decl with
  | DClassConstantField fields -> pf fmt "var %a;" (list ~sep:comma pp_d_ident_type) fields
  | DClassFunction -> pf fmt "TODO"
  | DClassMethod (fname, method_kind, attributes, signature, spec, body) ->
      pf fmt "%a%a%s%a%a%a" pp_d_method_kind method_kind
        (list_or_space ~sep:sp ~f:pp_attribute)
        attributes fname pp_d_method_signature signature pp_d_spec spec pp_d_body body

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
