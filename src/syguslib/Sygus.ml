open Base
open Sexplib

type symbol = string

type literal = 
  | LitNum of int
  | LitDec of float
  | LitBool of bool
  | LitHex of string
  | LitBin of bool list
  | LitString of string

type index =
  | INum of int
  | ISym of symbol

type identifier =
  | IdSimple of symbol
  | IdIndexed of symbol * index list

type sort =
  | SId of identifier
  | SApp of identifier * sort list

type term =
  | TId of identifier
  | TLit of literal
  | TApp of identifier * term list
  | TExists of sorted_var list * term
  | TForall of sorted_var list * term
  | TLet of binding list * term

and sorted_var = symbol * sort

and binding = symbol * term


type feature = FGrammar | FFwdDecls | FRecursion

type command =
  | CCheckSynth
  | CConstraint of term
  | CDeclareVar of symbol * sort
  | CInvConstraint of symbol * symbol * symbol * symbol
  | CSetFeature of feature * bool
  | CSynthFun of symbol * sorted_var list * sort * grammar_def
  | CSynthInv of symbol * sorted_var list * grammar_def
  | CDeclareDataType of symbol * dt_cons_dec list
  | CDeclareDataTypes of sort_decl list * dt_cons_dec list
  | CDeclareSort of symbol * int
  | CDefineFun of symbol * sorted_var list * sort * term
  | CDefineSort of symbol * sort
  | CSetInfo of symbol * literal
  | CSetLogic of symbol
  | CSetOption of symbol * literal

and sort_decl = symbol * int

and dt_cons_dec = symbol * sorted_var list

and grammar_def = (sorted_var * grouped_rule_list) list

and grouped_rule_list = symbol * sort * gterm list

and gterm =
  | GConstant of sort
  | GTerm of term
  | GVar of sort


type program = command list


let special_chars : char list =
  ['_';'+';'-';'*';'&';'|';'!';'~';'<';'>';'=';'/';'%';'?';'.';'$';'^']


let reserved_words : string list =
  ["check-synth"; "Constant"; "constraint"; "declare-datatype"; "declare-datatypes";
   "declare-sort"; "declare-var"; "define-fun"; "define-sort"; "exists"; "forall";
   "inv-constraint"; "let"; "set-feature"; "set-info"; "set-logic"; "set-option";
   "synth-fun"; "synth-inv"; "Variable"]


(**
   `valid_ident name` is `true` whenever `name` is not a reserved word, and does not contain
   any special character.
*)
let valid_ident (name : string) =
  (List.mem ~equal:String.equal reserved_words name) &&
  (List.for_all special_chars ~f:(fun x -> not (String.mem name x)))


let symbol_of_sexp (s : Sexp.t) : symbol =
  match s with
  | Atom symb -> symb
  | _ -> failwith "Not a symbol"


let index_of_sexp (s : Sexp.t) : index =
  match s with
  | Atom s ->
    (match Caml.int_of_string_opt s with
     | Some i -> INum i
     | None -> ISym s)
  | _ -> failwith "Not an index"


let identifier_of_sexp (s : Sexp.t) : identifier =
  match s with
  | Atom name ->
    if valid_ident name then  IdSimple name else failwith "Not an identifier."
  | List ((Atom "_") :: (main_s :: (i0 :: indexes))) ->
    IdIndexed (symbol_of_sexp main_s, List.map ~f:index_of_sexp (i0 :: indexes))
  | _ -> failwith "Not an identifier."


let rec sort_of_sexp (s : Sexp.t) : sort =
  try SId (identifier_of_sexp s) with
  | _ ->
    match s with
    | List (id :: (s1 :: sorts)) ->
      SApp (identifier_of_sexp id, List.map ~f:sort_of_sexp (s1 :: sorts))
    | _ -> failwith "Not a sort"


let char_to_bool (c : char) =
  if Char.(c = '0') then false else true


let literal_of_string (s : string) : literal =
  if String.is_prefix ~prefix:"\"" s then
    LitString s
  else
  if String.is_prefix ~prefix:"#x" s then
    LitHex (String.chop_prefix_exn ~prefix:"#x" s)
  else if String.is_prefix ~prefix:"#b" s then
    let b = String.chop_prefix_exn ~prefix:"#b" s in
    LitBin (List.map ~f:char_to_bool (String.to_list b))
  else
    match Caml.int_of_string_opt s with
    | Some i -> LitNum i
    | None ->
      match Caml.float_of_string_opt s with
      | Some f -> LitDec f
      | None -> failwith "Not a literal."


let literal_of_sexp (s : Sexp.t) : literal =
  match s with
  | Atom atom -> literal_of_string atom
  | _ -> failwith "not a literal"


let sorted_var_of_sexp (s : Sexp.t) : sorted_var =
  match s with
  | Sexp.(List [symb; sort]) ->
    symbol_of_sexp symb, sort_of_sexp sort
  | _ -> failwith "Not a sorted var"


let rec term_of_sexp (s : Sexp.t) : term =
  match s with
  | List [Atom "exists"; List _vars; term] ->
    TExists (List.map ~f:sorted_var_of_sexp _vars, term_of_sexp term)

  | List [Atom "forall"; List _vars; term] ->
    TForall (List.map ~f:sorted_var_of_sexp _vars, term_of_sexp term)

  | List [Atom "let"; List bindings; term] ->
    TLet (List.map ~f:binding_of_sexp bindings, term_of_sexp term)

  | List (hd :: tl) ->
    TApp(identifier_of_sexp hd, List.map ~f:term_of_sexp tl)

  | _ ->
    try TLit (literal_of_sexp s)
    with _ -> TId (identifier_of_sexp s)


and binding_of_sexp (s: Sexp.t) : binding =
  match s with
  | List [symb; term] -> symbol_of_sexp symb, term_of_sexp term
  | _ -> failwith "not a binding"


let feature_of_sexp (s : Sexp.t) : feature =
  match s with
  | Atom "grammars" -> FGrammar
  | Atom "fwd-decls" -> FFwdDecls
  | Atom "recursion" -> FRecursion
  | _ -> failwith "Not a feature."


let sort_decl_of_sexp (s : Sexp.t) : sort_decl =
  match s with
  | List [symb; Atom num] -> symbol_of_sexp symb, Int.of_string num
  | _ -> failwith "Not a sort declaration."


let sort_decl_list_of_sexp (s : Sexp.t) : sort_decl list =
  match s with
  | List (sd1::sdrest) ->
    List.map ~f:sort_decl_of_sexp (sd1::sdrest)
  | _ -> failwith "Not a list+ of sort declarations."


let dt_cons_dec_of_sexp (s : Sexp.t) : dt_cons_dec =
  match s with
  | List (symb :: args) -> symbol_of_sexp symb, List.map ~f:sorted_var_of_sexp args
  | _ -> failwith "Not a data constructor declaration."


let dt_cons_dec_list_of_sexp (s : Sexp.t) : dt_cons_dec list =
  match s with
  | List (d1 :: drest) -> List.map ~f:dt_cons_dec_of_sexp (d1 :: drest)
  | _ -> failwith "Not a list+ of data constructor declarations."


let gterm_of_sexp (s : Sexp.t) : gterm =
  match s with
  | List [Atom "Constant"; sort] -> GConstant (sort_of_sexp sort)
  | List [Atom "Variable"; sort] -> GVar (sort_of_sexp sort)
  | t ->
    try GTerm (term_of_sexp t) with _ -> failwith "Not a grammar term."


let grouped_rule_of_sexp (s : Sexp.t) : grouped_rule_list =
  match s with
  | List [name; sort; List gramterms] ->
    symbol_of_sexp name, sort_of_sexp sort, List.map ~f:gterm_of_sexp gramterms
  | _ -> failwith "Not a grouped rule."


let grammar_def_of_sexps (sv : Sexp.t) (gr : Sexp.t) : grammar_def =
  match sv, gr with
  | List sorts, List grouped_rules ->
    (match List.zip
             (List.map ~f:sorted_var_of_sexp sorts)
             (List.map ~f:grouped_rule_of_sexp grouped_rules)
     with
     | Ok l -> l
     | _ -> failwith "Not a grammar definition.")
  | _ -> failwith "Not a grammar definition."


let command_of_sexp (s : Sexp.t) : command =
  let command_of_elts (sl : Sexp.t list) : command =
    match sl with
    | [Atom single] ->
      (match single with
       | "check-synth" -> CCheckSynth
       | _ -> failwith "Not a command")

    | [Atom com_name; arg] ->
      (match com_name with
       | "constraint" -> CConstraint (term_of_sexp arg)
       | "set-logic" -> CSetLogic (symbol_of_sexp arg)
       | _ -> failwith "not a command")

    | [Atom com_name; arg1; arg2] ->
      (match com_name with
       | "declare-var" -> CDeclareVar (symbol_of_sexp arg1, sort_of_sexp arg2)
       | "declare-datatype" ->
         CDeclareDataType (symbol_of_sexp arg1, dt_cons_dec_list_of_sexp arg2)
       | "declare-datatypes" ->
         CDeclareDataTypes (sort_decl_list_of_sexp arg1, dt_cons_dec_list_of_sexp arg2)
       | "declare-sort" -> CDeclareSort (symbol_of_sexp arg1, Int.t_of_sexp arg2)
       | "define-sort" -> CDefineSort (symbol_of_sexp arg1, sort_of_sexp arg2)
       | _ -> failwith "Not a command")

    | [Atom com_name; Atom ":"; arg1; arg2] ->
      (match com_name with
       | "set-info" -> CSetInfo (symbol_of_sexp arg1, literal_of_sexp arg2)
       | "set-feature" -> CSetFeature (feature_of_sexp arg1, bool_of_sexp arg2)
       | _ -> failwith "not a command")

    | [Atom "synth-inv"; name; List args; gd1; gd2] ->
      CSynthInv(symbol_of_sexp name, List.map ~f:sorted_var_of_sexp args, grammar_def_of_sexps gd1 gd2)

    | [Atom "synth-fun"; name; List args; res; gd1; gd2] ->
      CSynthFun(symbol_of_sexp name, List.map ~f:sorted_var_of_sexp args, sort_of_sexp res,
                grammar_def_of_sexps gd1 gd2)

    | [Atom "inv-constraint"; a; b; c; d] ->
      CInvConstraint (symbol_of_sexp a, symbol_of_sexp b, symbol_of_sexp c, symbol_of_sexp d)

    | _ -> failwith "Not a command"

  in
  match s with
  | List elts -> command_of_elts elts
  | _ -> failwith "Not a command."

let program_of_sexp_list (sexps : Sexp.t list) : program =
  List.map ~f:command_of_sexp sexps