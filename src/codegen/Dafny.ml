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
  | DAttrOpaque of bool (** DAttrOpaque b is opaque_full is b is true. *)
  | DAttrTailRecursion of bool
  | DAttrTimeLimitMultiplier of int
  | DAttrTypeQuantifier

type d_qualified_name = string list

type d_decl_modifier =
  | DDmNone
  | DDmAbstract
  | DDmGhost
  | DDmStatic

let d_decl_modifier_to_string d =
  match d with
  | DDmNone -> ""
  | DDmAbstract -> "abstract"
  | DDmGhost -> "ghost"
  | DDmStatic -> "static"
;;

type d_variance = int * string

(* Types *)

(** A generic parameter has an optional variance ( *, +, ! or - ) and a type name.
*)
type d_generic_param = d_variance option * string

and d_domain_type =
  | DTyBool (** The bool type. *)
  | DTyChar (** The char type. *)
  | DTyInt (** The int type. *)
  | DTyReal (** The real type. *)
  | DTyOrdinal (** The ordinal type. *)
  | DTyBitVector (** The bitvector type (incomplete implementation). *)
  | DTyObject (** The Object type. *)
  | DTyFiniteSet of d_domain_type (** The set<T> type where T is a d_domain_type. *)
  | DTyInfiniteSet of d_domain_type (** The iset<T> type where T is a d_domain_type. *)
  | DTyMultiset of d_domain_type (** The multiset<T> type where T is a d_domain_type. *)
  | DTyFiniteMap of d_domain_type * d_domain_type
      (** The map<K,T> type where K,T are d_domain_type. *)
  | DTyInfiniteMap of d_domain_type * d_domain_type
      (** The imap<K,T> type where K,T are d_domain_type. *)
  | DTySequence of d_domain_type (** The seq<T> type where T is a d_domain_type. *)
  | DTyNat (** The nat type. *)
  | DTyString (** The string type. *)
  | DTyArray of d_domain_type (** The array<T> type. *)
  | DTyTuple of d_domain_type list (** The tuple<T1,T2, .., Tn> type.*)
  | DTyNamed of string * d_domain_type list
      (**
      A named type is a type typename<typeparams>.
      This is a simplification of the NamedType of the Dafny reference.
      For example, [DTyNamed(["list", [DTyInt]])] is the type list<int>. *)
  | DTyComposite of (string * d_domain_type list) list

type d_ident_type = string * d_domain_type

(** A datatype constructor declaration, e.g. Cons(head: int, tail: list).
  A datatype constructor is also called a DatatypeMember in the Dafny reference.
 *)
type d_datatype_constr_decl =
  { dconstr_attributes : d_attribute option (** Optional attribute of the constructor *)
  ; dconstr_name : string (** The constructor name.  *)
  ; dconstr_args : (string option * d_domain_type) list
        (** The constructor arguments are a list of types, optionally with a name.
      For example, a list datatype declaration can be:
      datatype List<T> = Nil | Cons(head: T, tail: List<T>)
      where each consntructor argument is named, or
      datatype List<T> = Nil | Cons(T, List<T>)
      which is simpler, but then no destructor is created in Dafny.
      *)
  }

(* Function and method bodies. *)
type d_body =
  | Body of string (** A string body for hardcoded parts. *)
  | DMatch of d_body * (Term.pattern * d_body) list (** A pattern matching construct. *)
  | DTerm of Term.term (** A wrapper for a term. *)
  | DBlock of d_body list (** A block of d_body in braces, each separeted by semicolon. *)
  | DAssert of Term.term (** Assert(term) *)
  | DCalc of d_body list (** Calc statement. *)
  | DAssign of Term.term * d_body (** Assign statement.  *)
  | DStmt of d_body (** A body statement with a semicolon after it*)

(* Specs *)
type d_clause = Term.term

type d_spec_kind =
  | DSpecMethod (** A method specification kind.*)
  | DSpecFunction (** A function specification kind.*)
  | DSpecLambda (** A lambda specification kind.*)
  | DSpecIterator (** An iterator specification kind.*)
  | DSpecLoop (** A loop specification kind.*)

(**
This type is used for all kinds of specifications. Some fields should not appear in some
specifications, for example if the spec if for a function, the modifies clauses will not be used.
*)
type d_spec =
  { dspec_kind : d_spec_kind
        (** The kind of specification (method, function, ...)
          This changes how the specification is printed by filtering only the clauses
          allowed in the given specification.
  *)
  ; dspec_decreases : d_clause list (** The list of decreases clauses. *)
  ; dspec_ensures : d_clause list (** The list of ensures clauses. *)
  ; dspec_modifies : d_clause list (** The list of modifies clauses. *)
  ; dspec_requires : d_clause list (** The list of requires clauses. *)
  ; (* For non-functional specifications. *)
    dspec_reads : d_clause list (** The list of reads clauses *)
  ; (* For loops. *)
    dspec_invariant : d_clause option (** The loop invariant clause. *)
  }

let dspec_is_empty (x : d_spec) : bool =
  List.is_empty x.dspec_decreases
  && List.is_empty x.dspec_ensures
  && List.is_empty x.dspec_requires
  && List.is_empty x.dspec_reads
  && List.is_empty x.dspec_modifies
  && Option.is_none x.dspec_invariant
;;

(* Class member declarations. *)

(** The method kind: a simple method, a constructor, a lemma or an extreme lemma.
  Most used methods are lemmas and constructors.
*)
type d_method_kind =
  | DMkMethod (** A simple method. *)
  | DMkConstructor (** A constructor method. *)
  | DMkLemma of bool
      (** DMkLemma(b) is "twostate lemma" if b is true, otherwise just "lemma" *)
  | DMkLemmaExtreme of bool
      (** DMkLemmaExtreme(true) is "greatest lemma", if false "least lemma" *)

type d_method_signature =
  { dmsig_params : d_generic_param list (** The optional signature parameters. *)
  ; dmsig_ktype : d_domain_type option (** Optional, for "least" and "greatest" lemmas. *)
  ; dmsig_formals : d_ident_type list (** The formal arguments of the method. *)
  ; dmsig_returns : d_ident_type list (** The return of the method. *)
  }

(** The function kind: a simple method, a constructor, a lemma or an extreme lemma.
  Most used methods are lemmas and constructors.
*)
type d_function_kind =
  | DFkFunction of bool (** A simple function, with also "method" is bool is true. *)
  | DFkPredicate of bool
      (** DFkPredicate(b) is "predicate method" if b is true, otherwise just "predicate" *)
  | DFkPredicateExtreme of bool
      (** DMkPredicateExtreme(true) is "greatest predicate", if false "least predicate" *)

type d_function_signature =
  { dfsig_params : d_generic_param list (** The optional signature parameters. *)
  ; dfsig_ktype : d_domain_type option (** Optional, for "least" and "greatest" lemmas. *)
  ; dfsig_formals : d_ident_type list (** The formal arguments of the method. *)
  ; dfsig_return : string option * d_domain_type list
        (** The return type of the function. *)
  }

(** Class members are functions, constant fields and methods. *)
type d_class_member_decl =
  | DClassConstantField of d_ident_type list
  | DClassFunction of
      string * d_function_kind * d_attribute list * d_function_signature * d_spec * d_body
  | DClassMethod of
      string * d_method_kind * d_attribute list * d_method_signature * d_spec * d_body

(* Toplevel objects. *)

(* Top lelevel declarations.  *)
type d_toplevel =
  { dt_modifiers : d_decl_modifier list
  ; dt_kind : d_decl_kind
  }

(** The kinds of top level declarations in a Dafny program. *)
and d_decl_kind =
  | DSubModuleDefinition of
      d_attribute list * d_qualified_name * d_qualified_name option * d_toplevel list
      (** DSubModuleDefinition(attributes, name, refname, declarations)  is the submodule definition
      "module" attributes name (refines refname) \{ declarations \}
      *)
  | DSubModuleImport of bool * d_qualified_name
      (** DSubModuleImport(false, qname) is "import qname".
          DSubModuleImport(true, qname) is "import opened qname".
          TODO: consider the different kind of imports in the reference.
     *)
  | DSubModuleExport
      (** TODO: implement module exports. Probably not needed for this project.  *)
  | DClassDecl of d_attribute list * string * d_generic_param list * d_toplevel list
      (** A class declaration. Todo: add the "extends".  *)
  | DDatatypeDecl of
      d_attribute list * string * d_generic_param list * d_datatype_constr_decl list
      (** A Datatype declaration, e.g. datatype type<params> = c1 | c2(param) | .. *)
  | DCodatatypeDecl of
      d_attribute list * string * d_generic_param list * d_datatype_constr_decl list
      (** A Coinductive datatype declaration, e.g. codatatype type<params> = c1 | c2(param) | .. *)
  | DNewtypeDecl
  | DSynonymTypeDecl
  | DIteratorDecl
  | DTraitDecl
  | DClassMemberDecl of d_decl_modifier * d_class_member_decl

(**  A Dafny program is a list of includes followed by a list of toplevel declarations. *)
type d_program =
  { dp_includes : string list
  ; dp_topdecls : d_toplevel list
  }

(* ============================================================================================= *)
(*                                         Pretty Printing                                       *)
(* ============================================================================================= *)
let pp_attribute (fmt : Formatter.t) (attr : d_attribute) : unit =
  match attr with
  | DAttrAssumption -> pf fmt "{:assumption}"
  | DAttrAutoReq -> pf fmt "{:autoReq}"
  | DAttrAutocontract -> pf fmt "{:autocontracts}"
  | DAttrAxiom -> pf fmt "{:axiom}"
  | DAttrCompile -> pf fmt "{:compile}"
  | DAttrDecl -> pf fmt "{:decl}"
  | DAttrFuel (fname, low_fuel, maybe_high_fuel) ->
    (match maybe_high_fuel with
    | Some hf -> pf fmt "{:fuel %s,%i,%i}" fname low_fuel hf
    | None -> pf fmt "{:fuel %s,%i}" fname low_fuel)
  | DAttrHeapQuantifier -> pf fmt "{:heapQuantifier}"
  | DAttrImported -> pf fmt "{:imported}"
  | DAttrInduction (b, l) ->
    (match l with
    | [] -> pf fmt "{:induction %b}" b
    | _ -> pf fmt "{:induction %a}" (list ~sep:sp string) l)
  | DAttrLayerQuantifier -> pf fmt "{:layerQuantifier}"
  | DAttrNaiveType (maybe_bool, maybe_typename) ->
    (match maybe_bool, maybe_typename with
    | Some b, None -> pf fmt "{:nativeType %b}" b
    | _, Some tname -> pf fmt "{:nativeType %s}" tname
    | _ -> pf fmt "{:nativeType}")
  | DAttrOpaque is_full -> if is_full then pf fmt "{:opaque_full}" else pf fmt "{:opaque}"
  | DAttrTailRecursion b -> pf fmt "{:tailRecursion %b}" b
  | DAttrTimeLimitMultiplier mult -> pf fmt "{:timeLimitMultiplier %i}" mult
  | DAttrTypeQuantifier -> pf fmt "{:typeQuantifier}"
;;

let pp_d_qualified_name (fmt : Formatter.t) (qn : d_qualified_name) : unit =
  pf fmt "%a" (list ~sep:(fun fmt () -> pf fmt ".") string) qn
;;

let pp_d_decl_modifier (fmt : Formatter.t) (dm : d_decl_modifier) : unit =
  pf fmt "%s" (d_decl_modifier_to_string dm)
;;

let pp_d_includes (fmt : Formatter.t) (includes : string list) : unit =
  List.iter includes ~f:(fun iname -> pf fmt "include %s" iname)
;;

let rec pp_d_typename_segment (fmt : Formatter.t) ((t, p) : string * d_domain_type list)
    : unit
  =
  match p with
  | [] -> string fmt t
  | _ -> pf fmt "%s<%a>" t (list ~sep:comma pp_d_domain_type) p

and pp_d_domain_type (fmt : Formatter.t) = function
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
  | DTyArray t -> pf fmt "array<%a>" pp_d_domain_type t
  | DTyTuple tl -> pf fmt "(%a)" (list ~sep:comma pp_d_domain_type) tl
  | DTyNamed (tname, tparams) -> pp_d_typename_segment fmt (tname, tparams)
  | DTyComposite tsl -> (list ~sep:dot pp_d_typename_segment) fmt tsl
;;

let pp_d_datatype_constr_decl (frmt : Formatter.t) (dc : d_datatype_constr_decl) : unit =
  let pp_constr_arg fmt (maybe_arg_name, arg_type) =
    match maybe_arg_name with
    | Some name -> pf fmt "%s : %a" name pp_d_domain_type arg_type
    | None -> pp_d_domain_type fmt arg_type
  in
  let pp_args fmt args =
    match args with
    | [] -> ()
    | _ -> (box (parens (list ~sep:comma pp_constr_arg))) fmt args
  in
  pf
    frmt
    "%a%s%a"
    (option_or_space ~f:pp_attribute)
    dc.dconstr_attributes
    dc.dconstr_name
    pp_args
    dc.dconstr_args
;;

let pp_d_ident_type (fmt : Formatter.t) ((id, ty) : d_ident_type) : unit =
  pf fmt "%s : %a" id pp_d_domain_type ty
;;

(** Force a newline by asking to print 100 spaces or a new line. *)
let newl frmt () = pf frmt "@;<100 0>"

let rec pp_d_term ~ctx (frmt : Formatter.t) (x : Term.term) =
  let pp_d_term = pp_d_term ~ctx in
  match x.tkind with
  | TConst c -> pf frmt "%a" Term.Constant.pp c
  | TVar v -> pf frmt "%a" (Term.Variable.pp ctx) v
  | TBin (op, t1, t2) ->
    (match op with
    | Term.Binop.Max | Term.Binop.Min ->
      pf frmt "@[<hov 2>%a@;(%a,@;%a)@]" Term.Binop.pp op pp_d_term t1 pp_d_term t2
    | Term.Binop.Eq ->
      pf
        frmt
        "@[<hov 2>(%a@;%a@;%a)@]"
        pp_d_term
        t1
        (fun f _ -> Fmt.string f "==")
        op
        pp_d_term
        t2
    | Term.Binop.Le ->
      pf
        frmt
        "@[<hov 2>(%a@;%a@;%a)@]"
        pp_d_term
        t1
        (fun f _ -> Fmt.string f "<=")
        op
        pp_d_term
        t2
    | Term.Binop.Ge ->
      pf
        frmt
        "@[<hov 2>(%a@;%a@;%a)@]"
        pp_d_term
        t1
        (fun f _ -> Fmt.string f ">=")
        op
        pp_d_term
        t2
    | _ -> pf frmt "@[<hov 2>%a@;%a@;%a@]" pp_d_term t1 Term.Binop.pp op pp_d_term t2)
  | TUn (op, t1) -> pf frmt "@[<hov 2>%a@;%a@]" Term.Unop.pp op pp_d_term t1
  | TIte (c, t1, t2) ->
    pf frmt "@[<hov 2>%a@;?@;%a@;:@;%a@]" pp_d_term c pp_d_term t1 pp_d_term t2
  | TTup tl -> pf frmt "@[<hov 2>(%a)@]" (list ~sep:comma pp_d_term) tl
  | TSel (t, i) -> pf frmt "@[<hov 2>%a.%i@]" pp_d_term t i
  | TFun (args, body) ->
    pf
      frmt
      "@[<hov 2>((%a) => %a)@]"
      (list ~sep:comma (Term.pp_fpattern ctx))
      args
      pp_d_term
      body
  | TApp (func, args) ->
    pf frmt "@[<hov 2>%a(%a)@]" pp_d_term func (list ~sep:comma pp_d_term) args
  | TData (cstr, args) ->
    if List.length args = 0
    then pf frmt "%a" (styled (`Fg `Green) string) cstr
    else
      pf frmt "%a(%a)" (styled (`Fg `Green) string) cstr (list ~sep:comma pp_d_term) args
  | TMatch (tm, cases) ->
    let pp_d_match_case (frmt : Formatter.t) ((pat, term) : Term.match_case) =
      pf frmt "@[<hov 2> case %a =>@;%a@]" (Term.pp_pattern ctx) pat pp_d_term term
    in
    pf
      frmt
      "@[<v>match @[%a@]@;<1 1>@[<v>%a@]@]"
      pp_d_term
      tm
      (* 30 spaces should be enough to force a new line. *)
      (list ~sep:(fun fmt () -> pf fmt "@;<30 0>") pp_d_match_case)
      cases
  | TBox t -> pp_d_term frmt t
;;

let pp_clause ~ctx (clause_name : string) (fmt : Formatter.t) (c : d_clause) : unit =
  pf fmt "@[%s %a@]" clause_name (pp_d_term ~ctx) c
;;

(* Todo: Clean this up*)
let pp_d_spec ~ctx (fmt : Formatter.t) (spec : d_spec) : unit =
  match spec.dspec_kind with
  | DSpecMethod ->
    list ~sep:newl (pp_clause ~ctx "modifies") fmt spec.dspec_modifies;
    (match spec.dspec_requires @ spec.dspec_ensures @ spec.dspec_decreases with
    | [] -> ()
    | _ ->
      (match spec.dspec_modifies with
      | [] -> ()
      | _ -> newl fmt ()));
    list ~sep:newl (pp_clause ~ctx "requires") fmt spec.dspec_requires;
    (match spec.dspec_ensures @ spec.dspec_decreases with
    | [] -> ()
    | _ ->
      (match spec.dspec_modifies @ spec.dspec_requires with
      | [] -> ()
      | _ -> newl fmt ()));
    list ~sep:newl (pp_clause ~ctx "ensures") fmt spec.dspec_ensures;
    (match spec.dspec_decreases with
    | [] -> ()
    | _ ->
      (match spec.dspec_modifies @ spec.dspec_requires @ spec.dspec_ensures with
      | [] -> ()
      | _ -> newl fmt ()));
    list ~sep:newl (pp_clause ~ctx "decreases") fmt spec.dspec_decreases
  | DSpecFunction ->
    list ~sep:newl (pp_clause ~ctx "reads") fmt spec.dspec_reads;
    (match spec.dspec_requires @ spec.dspec_ensures @ spec.dspec_decreases with
    | [] -> ()
    | _ ->
      (match spec.dspec_reads with
      | [] -> ()
      | _ -> newl fmt ()));
    list ~sep:newl (pp_clause ~ctx "requires") fmt spec.dspec_requires;
    (match spec.dspec_ensures @ spec.dspec_decreases with
    | [] -> ()
    | _ ->
      (match spec.dspec_reads @ spec.dspec_requires with
      | [] -> ()
      | _ -> newl fmt ()));
    list ~sep:newl (pp_clause ~ctx "ensures") fmt spec.dspec_ensures;
    (match spec.dspec_decreases with
    | [] -> ()
    | _ ->
      (match spec.dspec_reads @ spec.dspec_requires @ spec.dspec_ensures with
      | [] -> ()
      | _ -> newl fmt ()));
    list ~sep:newl (pp_clause ~ctx "decreases") fmt spec.dspec_decreases
  | DSpecLambda ->
    list ~sep:sp (pp_clause ~ctx "reads") fmt spec.dspec_reads;
    list ~sep:sp (pp_clause ~ctx "requires") fmt spec.dspec_requires
  | DSpecIterator ->
    list ~sep:sp (pp_clause ~ctx "reads") fmt spec.dspec_reads;
    list ~sep:sp (pp_clause ~ctx "yield requires") fmt spec.dspec_requires;
    list ~sep:sp (pp_clause ~ctx "yield ensures") fmt spec.dspec_ensures;
    list ~sep:sp (pp_clause ~ctx "decreases") fmt spec.dspec_decreases
  | DSpecLoop ->
    pf fmt "%a@;" (option (pp_clause ~ctx "invariant")) spec.dspec_invariant;
    list ~sep:sp (pp_clause ~ctx "decreases") fmt spec.dspec_decreases;
    list ~sep:sp (pp_clause ~ctx "modifies") fmt spec.dspec_modifies
;;

let rec pp_d_body ~ctx (fmt : Formatter.t) (body : d_body) : unit =
  let pp_d_body = pp_d_body ~ctx in
  let pp_d_term = pp_d_term ~ctx in
  match body with
  | Body content -> pf fmt "@[<v>{@;<1 2>@[<hov 2>%a@]@;}@]" (box string) content
  | DMatch (t, cases) ->
    let pp_d_match_case (frmt : Formatter.t) ((pat, body) : Term.pattern * d_body) =
      pf frmt "@[<hov 2> case %a =>@;%a@]" (Term.pp_pattern ctx) pat pp_d_body body
    in
    pf
      fmt
      "@[<v>match @[%a@]@;<1 1>@[<v>%a@]@]"
      pp_d_body
      t
      (* 30 spaces should be enough to force a new line. *)
      (list ~sep:(fun fmt () -> pf fmt "@;<30 0>") pp_d_match_case)
      cases
  | DTerm t -> pp_d_term fmt t
  | DBlock stmts ->
    pf
      fmt
      "@[<v>{@;<1 2>@[<hov 2>%a@;}@]"
      (list ~sep:(fun fmt () -> pf fmt "@;<100 0>") pp_d_body)
      stmts
  | DAssert asserted -> pf fmt "@[assert(%a)@]" pp_d_term asserted
  | DCalc stmts ->
    pf
      fmt
      "@[<v 2>calc == {@;@[<v>%a@]@;}@]"
      (list ~sep:(fun fmt () -> pf fmt "@;<100 0>") pp_d_body)
      stmts
  | DAssign (x, e) -> pf fmt "@[<hov 2>var %a:=%a@]" pp_d_term x pp_d_body e
  | DStmt st -> pf fmt "@[<hov 2>%a;@]" pp_d_body st
;;

let pp_d_generic_param (fmt : Formatter.t) ((_vo, t) : d_generic_param) =
  (* Just print the type for now, we don't need the variance. *)
  string fmt t
;;

let pp_params (fmt : Formatter.t) (params : d_generic_param list) : unit =
  match params with
  | [] -> ()
  | _ -> pf fmt "<%a>" (list ~sep:comma pp_d_generic_param) params
;;

let pp_d_method_signature (fmt : Formatter.t) (dsig : d_method_signature) : unit =
  (match dsig.dmsig_params with
  | [] -> ()
  | _ -> pf fmt "<%a>" (list ~sep:comma pp_d_generic_param) dsig.dmsig_params);
  match dsig.dmsig_returns with
  | [] -> pf fmt "(%a)@;" (list ~sep:comma pp_d_ident_type) dsig.dmsig_formals
  | _ ->
    pf
      fmt
      "(%a)@;returns (%a)@;"
      (list ~sep:comma pp_d_ident_type)
      dsig.dmsig_formals
      (list ~sep:comma pp_d_ident_type)
      dsig.dmsig_returns
;;

let pp_d_function_signature (fmt : Formatter.t) (dsig : d_function_signature) : unit =
  (match dsig.dfsig_params with
  | [] -> ()
  | _ -> pf fmt "<%a>" (list ~sep:comma pp_d_generic_param) dsig.dfsig_params);
  match dsig.dfsig_return with
  | Some name, returns ->
    pf
      fmt
      "(%a)@;: (%s: (%a))"
      (list ~sep:comma pp_d_ident_type)
      dsig.dfsig_formals
      name
      (list ~sep:comma pp_d_domain_type)
      returns
  | None, returns ->
    pf
      fmt
      "(%a)@;: (%a)"
      (list ~sep:comma pp_d_ident_type)
      dsig.dfsig_formals
      (list ~sep:comma pp_d_domain_type)
      returns
;;

let pp_d_method_kind (fmt : Formatter.t) (dmk : d_method_kind) : unit =
  match dmk with
  | DMkMethod -> pf fmt "method"
  | DMkConstructor -> pf fmt "constructor"
  | DMkLemma is_twostate ->
    if is_twostate then pf fmt "twostate lemma" else pf fmt "lemma"
  | DMkLemmaExtreme is_greatest ->
    if is_greatest then pf fmt "greatest lemma" else pf fmt "least lemma"
;;

let pp_d_function_kind (fmt : Formatter.t) (dmk : d_function_kind) : unit =
  match dmk with
  | DFkFunction b -> if b then pf fmt "function method" else pf fmt "function"
  | DFkPredicate is_m -> if is_m then pf fmt "predicate method" else pf fmt "method"
  | DFkPredicateExtreme is_greatest ->
    if is_greatest then pf fmt "greatest predicate" else pf fmt "least predicate"
;;

let pp_d_class_member_decl ~ctx (fmt : Formatter.t) (decl : d_class_member_decl) : unit =
  match decl with
  | DClassConstantField fields ->
    pf fmt "var %a;" (list ~sep:comma pp_d_ident_type) fields
  | DClassFunction (fname, function_kind, attributes, signature, spec, body) ->
    pf
      fmt
      "@[<v>@[%a%a%s%a@]@;<4 0>%a@;<4 0>%a@]"
      pp_d_function_kind
      function_kind
      (list_or_space ~sep:sp ~f:pp_attribute)
      attributes
      fname
      (box pp_d_function_signature)
      signature
      (box (pp_d_spec ~ctx))
      spec
      (box (pp_d_body ~ctx))
      body
  | DClassMethod (fname, method_kind, attributes, signature, spec, body) ->
    pf
      fmt
      "@[<v>@[%a%a%s%a@]%a@;<4 0>%a@]"
      pp_d_method_kind
      method_kind
      (list_or_space ~sep:sp ~f:pp_attribute)
      attributes
      fname
      (box pp_d_method_signature)
      signature
      (* Print the spec, or do nothing if spec is empty. *)
        (fun fmt () ->
        if dspec_is_empty spec then () else pf fmt "@;<4 0>%a" (box (pp_d_spec ~ctx)) spec)
      ()
      (* Print the body *)
      (box (pp_d_body ~ctx))
      body
;;

let rec pp_d_decl_kind ~ctx (fmt : Formatter.t) (dk : d_decl_kind) : unit =
  match dk with
  | DSubModuleDefinition (attributes, qname, maybe_refines, decls) ->
    pf
      fmt
      "@[<hov 2>@[module %a %a%a@;@]{@;@[%a@]@;}@]"
      (list ~sep:sp pp_attribute)
      attributes
      pp_d_qualified_name
      qname
      (option (fun fmt qn -> pf fmt " refines %a" pp_d_qualified_name qn))
      maybe_refines
      (list ~sep:sp (pp_d_toplevel ~ctx))
      decls
  | DSubModuleImport (opened, qname) ->
    if opened
    then pf fmt "@[import %a@]" pp_d_qualified_name qname
    else pf fmt "@[import opened %a@]" pp_d_qualified_name qname
  | DClassDecl (attributes, name, generics, decls) ->
    pf
      fmt
      "@[<hov 2>@[class%a%s%a@]@;{@;@[%a@]}"
      (list_or_space ~sep:sp ~f:pp_attribute)
      attributes
      name
      (list ~sep:comma pp_d_generic_param)
      generics
      (list ~sep:sp (pp_d_toplevel ~ctx))
      decls
  | DDatatypeDecl (attributes, name, params, constructors) ->
    pf
      fmt
      "@[datatype%a%s%a@;=@;%a@]"
      (list_or_space ~sep:sp ~f:pp_attribute)
      attributes
      name
      pp_params
      params
      (list ~sep:vbar pp_d_datatype_constr_decl)
      constructors
  | DCodatatypeDecl (attributes, name, params, constructors) ->
    pf
      fmt
      "@[codatatype%a%s%a@;=@;%a@]"
      (list_or_space ~sep:sp ~f:pp_attribute)
      attributes
      name
      pp_params
      params
      (list ~sep:vbar pp_d_datatype_constr_decl)
      constructors
  | DClassMemberDecl (modifier, mem_decl) ->
    pf fmt "%a%a" pp_d_decl_modifier modifier (pp_d_class_member_decl ~ctx) mem_decl
  | DNewtypeDecl -> Log.error_msg "Dafny: New type declaration has not been implemented"
  | DSynonymTypeDecl ->
    Log.error_msg "Dafny: Synonym type declaration has not been implemented"
  | DTraitDecl -> Log.error_msg "Dafny: Trait declaration has not been implemented"
  | DSubModuleExport -> Log.error_msg "Dafny: Submodule export has not been implemented"
  | DIteratorDecl -> Log.error_msg "Dafny: Iterator declaration has not been implemented"

and pp_d_toplevel ~ctx (fmt : Formatter.t) (topl : d_toplevel) : unit =
  match topl.dt_modifiers with
  | [] -> pp_d_decl_kind ~ctx fmt topl.dt_kind
  | _ as l ->
    pf
      fmt
      "@[<hov 2>%a %a@]"
      (list ~sep:sp pp_d_decl_modifier)
      l
      (pp_d_decl_kind ~ctx)
      topl.dt_kind
;;

let pp_d_program ~ctx (frmt : Formatter.t) (p : d_program) : unit =
  List.iter p.dp_includes ~f:(fun incl -> pf frmt "include %s@." incl);
  (* Skip two lines after includes, for readability. *)
  if List.is_empty p.dp_includes then () else pf frmt "@.@.";
  pf
    frmt
    "%a"
    (list
       ~sep:(fun fmt () -> pf fmt "@.@.")
       (fun fmt topd -> pf fmt "@[<hov 2>%a@]" (pp_d_toplevel ~ctx) topd))
    p.dp_topdecls;
  pf frmt "@."
;;

(* ============================================================================================= *)
(*                                        BUilding     helpers                                   *)
(* ============================================================================================= *)

let mk_int_type = DTyInt
let mk_bool_type = DTyBool
let mk_char_type = DTyChar
let mk_string_type = DTyString
let mk_set_type t = DTyFiniteSet t
let mk_tuple_type (el : d_domain_type list) = DTyTuple el
let mk_named_type ?(params = []) name : d_domain_type = DTyNamed (name, params)

let mk_datatype_constr
    ?(attr = None)
    (name : string)
    (args : (string option * d_domain_type) list)
    : d_datatype_constr_decl
  =
  { dconstr_attributes = attr; dconstr_name = name; dconstr_args = args }
;;

let mk_datatype_decl
    ?(attrs = [])
    ?(params = [])
    (name : string)
    (constructors : d_datatype_constr_decl list)
    : d_decl_kind
  =
  DDatatypeDecl (attrs, name, params, constructors)
;;

(*  Build functions, lemmas, etc. ..*)

let mk_simple_spec
    ?(decreases = [])
    ~(requires : d_clause list)
    ~(ensures : d_clause list)
    kind
    : d_spec
  =
  { dspec_kind = kind
  ; dspec_ensures = ensures
  ; dspec_requires = requires
  ; dspec_decreases = decreases
  ; dspec_reads = []
  ; dspec_modifies = []
  ; dspec_invariant = None
  }
;;

(** Create a method signature. By default, there are not parameters, no ktype
    and no returns.
*)
let mk_method_sig
    ?(params = [])
    ?(ktype = None)
    ?(returns = [])
    (formals : d_ident_type list)
    : d_method_signature
  =
  { dmsig_params = params
  ; dmsig_ktype = ktype
  ; dmsig_formals = formals
  ; dmsig_returns = returns
  }
;;

(** Create a lemma. By default, there are no attributes. *)
let mk_lemma
    ?(attrs = [])
    (method_name : string)
    (signature : d_method_signature)
    (spec : d_spec)
    (body : d_body)
    : d_decl_kind
  =
  let lemma_decl =
    DClassMethod (method_name, DMkLemma false, attrs, signature, spec, body)
  in
  DClassMemberDecl (DDmNone, lemma_decl)
;;

let mk_toplevel ?(modifiers = []) (dt_kind : d_decl_kind) : d_toplevel =
  { dt_modifiers = modifiers; dt_kind }
;;

let mk_func
    ?(attrs = [])
    (func_name : string)
    (signature : d_function_signature)
    (spec : d_spec)
    (body : d_body)
    : d_decl_kind
  =
  let func_decl =
    DClassFunction (func_name, DFkFunction false, attrs, signature, spec, body)
  in
  DClassMemberDecl (DDmNone, func_decl)
;;

let mk_func_sig
    ?(params = [])
    ?(ktype = None)
    ?(returns = None, [])
    (formals : d_ident_type list)
    : d_function_signature
  =
  { dfsig_params = params
  ; dfsig_ktype = ktype
  ; dfsig_formals = formals
  ; dfsig_return = returns
  }
;;
