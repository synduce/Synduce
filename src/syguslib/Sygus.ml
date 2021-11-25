open Base
open Sexplib

(**
   This implementation is based on the SyGuS Language Standard Version 2.0.
   Documentation can be found at https://sygus.org/language/
*)

let use_v1 = ref false

(* ============================================================================================= *)
(*                               TYPES FOR SYGUS SYNTAX                                          *)
(* ============================================================================================= *)

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
  | IdQual of symbol * sygus_sort

and sygus_sort =
  | SId of identifier
  | SApp of identifier * sygus_sort list

type sygus_term =
  | SyId of identifier
  | SyLit of literal
  | SyApp of identifier * sygus_term list
  | SyExists of sorted_var list * sygus_term
  | SyForall of sorted_var list * sygus_term
  | SyLet of binding list * sygus_term

and sorted_var = symbol * sygus_sort

and binding = symbol * sygus_term

type feature =
  | FGrammar
  | FFwdDecls
  | FRecursion

type command =
  | CCheckSynth
  | CConstraint of sygus_term
  | CDeclareVar of symbol * sygus_sort
  | CInvConstraint of symbol * symbol * symbol * symbol
  | CSetFeature of feature * bool
  | CSynthFun of symbol * sorted_var list * sygus_sort * grammar_def option
  | CSynthInv of symbol * sorted_var list * grammar_def option
  | CDeclareDataType of symbol * dt_cons_dec list
  | CDeclareDataTypes of sygus_sort_decl list * dt_cons_dec list list
  | CDeclareSort of symbol * int
  | CDefineFun of symbol * sorted_var list * sygus_sort * sygus_term
  | CDefineSort of symbol * sygus_sort
  | CSetInfo of symbol * literal
  | CSetLogic of symbol
  | CSetOption of symbol * literal

and sygus_sort_decl = symbol * int

and dt_cons_dec = symbol * sorted_var list

and grammar_def = (sorted_var * grouped_rule_list) list

and grouped_rule_list = sygus_gsterm list

and sygus_gsterm =
  | GConstant of sygus_sort
  | GTerm of sygus_term
  | GVar of sygus_sort

type program = command list

let special_chars : char list =
  [ '_'; '+'; '-'; '*'; '&'; '|'; '!'; '~'; '<'; '>'; '='; '/'; '%'; '?'; '.'; '$'; '^' ]
;;

let reserved_words : string list =
  [ "check-synth"
  ; "Constant"
  ; "constraint"
  ; "declare-datatype"
  ; "declare-datatypes"
  ; "declare-sort"
  ; "declare-var"
  ; "define-fun"
  ; "define-sort"
  ; "exists"
  ; "forall"
  ; "inv-constraint"
  ; "let"
  ; "set-feature"
  ; "set-info"
  ; "set-logic"
  ; "set-option"
  ; "synth-fun"
  ; "synth-inv"
  ; "Variable"
  ]
;;

let digits = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

(**
   `valid_ident name` is `true` whenever `name` is not a reserved word, and does not contain
   any special character.
*)
let valid_ident (name : string) =
  (not (List.mem ~equal:String.equal reserved_words name))
  &&
  match String.to_list name with
  | hd :: _ -> not (List.mem ~equal:Char.equal digits hd)
  | [] -> false
;;

(* ============================================================================================= *)
(*               CONVERSION BETWEEN SYGUS TYPES AND S-EXPRESSIONS                                *)
(* ============================================================================================= *)
let symbol_of_sexp (s : Sexp.t) : symbol =
  match s with
  | Atom symb -> symb
  | _ -> failwith (Fmt.str "%a is not a symbol" Sexp.pp_hum s)
;;

let sexp_of_symbol (s : symbol) : Sexp.t = Atom s

let index_of_sexp (s : Sexp.t) : index =
  match s with
  | Atom s ->
    (match Caml.int_of_string_opt s with
    | Some i -> INum i
    | None -> ISym s)
  | _ -> failwith "Not an index"
;;

let sexp_of_index (i : index) : Sexp.t =
  match i with
  | INum i -> Atom (Int.to_string i)
  | ISym s -> Atom s
;;

let identifier_of_sexp (s : Sexp.t) : identifier =
  match s with
  | Atom name -> if valid_ident name then IdSimple name else failwith "Not an identifier."
  | List (Atom "_" :: main_s :: i0 :: indexes) ->
    IdIndexed (symbol_of_sexp main_s, List.map ~f:index_of_sexp (i0 :: indexes))
  | _ -> failwith "Not an identifier."
;;

let rec sexp_of_identifier (id : identifier) : Sexp.t =
  match id with
  | IdSimple name -> Atom name
  | IdIndexed (name, indices) ->
    List (Atom "_" :: sexp_of_symbol name :: List.map ~f:sexp_of_index indices)
  | IdQual (name, sort) ->
    List [ Atom "as"; sexp_of_symbol name; sexp_of_sygus_sort sort ]

and sygus_sort_of_sexp (s : Sexp.t) : sygus_sort =
  try SId (identifier_of_sexp s) with
  | _ ->
    (match s with
    | List (id :: s1 :: sygus_sorts) ->
      SApp (identifier_of_sexp id, List.map ~f:sygus_sort_of_sexp (s1 :: sygus_sorts))
    | _ -> failwith "Not a sygus_sort")

and sexp_of_sygus_sort (s : sygus_sort) : Sexp.t =
  match s with
  | SId s -> sexp_of_identifier s
  | SApp (sname, sygus_sort_params) ->
    List (sexp_of_identifier sname :: List.map ~f:sexp_of_sygus_sort sygus_sort_params)
;;

let char_to_bool (c : char) = if Char.(c = '0') then false else true

let literal_of_string (s : string) : literal =
  if String.is_prefix ~prefix:"\"" s
  then LitString s
  else if String.is_prefix ~prefix:"#x" s
  then LitHex (String.chop_prefix_exn ~prefix:"#x" s)
  else if String.is_prefix ~prefix:"#b" s
  then (
    let b = String.chop_prefix_exn ~prefix:"#b" s in
    LitBin (List.map ~f:char_to_bool (String.to_list b)))
  else (
    match s with
    | "true" -> LitBool true
    | "false" -> LitBool false
    | _ ->
      (match Caml.int_of_string_opt s with
      | Some i -> LitNum i
      | None ->
        (match Caml.float_of_string_opt s with
        | Some f -> LitDec f
        | None -> failwith "Not a literal.")))
;;

let literal_of_sexp (s : Sexp.t) : literal =
  match s with
  | Atom atom -> literal_of_string atom
  | _ -> failwith "not a literal"
;;

let bool_list_to_bin_string (l : bool list) =
  String.concat (List.map ~f:(fun b -> if b then "1" else "0") l)
;;

let sexp_of_literal (l : literal) : Sexp.t =
  match l with
  | LitNum i -> Atom (Int.to_string i)
  | LitDec f -> Atom (Float.to_string f)
  | LitBool b -> Atom (Bool.to_string b)
  | LitHex s -> Atom ("#x" ^ s)
  | LitBin b -> Atom ("#b" ^ bool_list_to_bin_string b)
  | LitString s -> Atom ("\"" ^ s ^ "\"")
;;

let sorted_var_of_sexp (s : Sexp.t) : sorted_var =
  match s with
  | List [ symb; sygus_sort ] -> symbol_of_sexp symb, sygus_sort_of_sexp sygus_sort
  | _ -> failwith (Fmt.str "Not a sygus_sorted var: %a" Sexp.pp_hum s)
;;

let sexp_of_sorted_var ((symb, sygus_sort) : sorted_var) : Sexp.t =
  List [ sexp_of_symbol symb; sexp_of_sygus_sort sygus_sort ]
;;

let rec sygus_term_of_sexp (s : Sexp.t) : sygus_term =
  match s with
  | List [ Atom "exists"; List _vars; sygus_term ] ->
    SyExists (List.map ~f:sorted_var_of_sexp _vars, sygus_term_of_sexp sygus_term)
  | List [ Atom "forall"; List _vars; sygus_term ] ->
    SyForall (List.map ~f:sorted_var_of_sexp _vars, sygus_term_of_sexp sygus_term)
  | List [ Atom "let"; List bindings; sygus_term ] ->
    SyLet (List.map ~f:binding_of_sexp bindings, sygus_term_of_sexp sygus_term)
  | List (hd :: tl) -> SyApp (identifier_of_sexp hd, List.map ~f:sygus_term_of_sexp tl)
  | _ ->
    (try SyLit (literal_of_sexp s) with
    | _ -> SyId (identifier_of_sexp s))

and binding_of_sexp (s : Sexp.t) : binding =
  match s with
  | List [ symb; sygus_term ] -> symbol_of_sexp symb, sygus_term_of_sexp sygus_term
  | _ -> failwith "not a binding"
;;

let rec sexp_of_sygus_term (t : sygus_term) : Sexp.t =
  match t with
  | SyId s -> sexp_of_identifier s
  | SyLit l -> sexp_of_literal l
  | SyApp (f, args) -> List (sexp_of_identifier f :: List.map ~f:sexp_of_sygus_term args)
  | SyExists (vars, body) ->
    List
      [ Atom "exists"
      ; List (List.map ~f:sexp_of_sorted_var vars)
      ; sexp_of_sygus_term body
      ]
  | SyForall (vars, body) ->
    List
      [ Atom "forall"
      ; List (List.map ~f:sexp_of_sorted_var vars)
      ; sexp_of_sygus_term body
      ]
  | SyLet (bindings, body) ->
    List
      [ Atom "let"
      ; List
          (List.map bindings ~f:(fun (s, t) ->
               Sexp.List [ sexp_of_symbol s; sexp_of_sygus_term t ]))
      ; sexp_of_sygus_term body
      ]
;;

let feature_of_sexp (s : Sexp.t) : feature =
  match s with
  | Atom "grammars" -> FGrammar
  | Atom "fwd-decls" -> FFwdDecls
  | Atom "recursion" -> FRecursion
  | _ -> failwith "Not a feature."
;;

let sexp_of_feature (f : feature) : Sexp.t =
  match f with
  | FGrammar -> Atom "grammars"
  | FFwdDecls -> Atom "fwd-decls"
  | FRecursion -> Atom "recursion"
;;

let sygus_sort_decl_of_sexp (s : Sexp.t) : sygus_sort_decl =
  match s with
  | List [ symb; Atom num ] -> symbol_of_sexp symb, Int.of_string num
  | _ -> failwith "Not a sygus_sort declaration."
;;

let sexp_of_sort_decl ((name, id) : sygus_sort_decl) : Sexp.t =
  List [ Atom name; Atom (Int.to_string id) ]
;;

let sygus_sort_decl_list_of_sexp (s : Sexp.t) : sygus_sort_decl list =
  match s with
  | List (sd1 :: sdrest) -> List.map ~f:sygus_sort_decl_of_sexp (sd1 :: sdrest)
  | _ -> failwith "Not a list+ of sygus_sort declarations."
;;

let dt_cons_dec_of_sexp (s : Sexp.t) : dt_cons_dec =
  match s with
  | List (symb :: args) -> symbol_of_sexp symb, List.map ~f:sorted_var_of_sexp args
  | _ -> failwith "Not a data constructor declaration."
;;

let sexp_of_dt_cons_dec ((s, args_sorts) : dt_cons_dec) : Sexp.t =
  List (Atom s :: List.map ~f:sexp_of_sorted_var args_sorts)
;;

let dt_cons_dec_list_of_sexp (s : Sexp.t) : dt_cons_dec list =
  match s with
  | List (d1 :: drest) -> List.map ~f:dt_cons_dec_of_sexp (d1 :: drest)
  | _ -> failwith "Not a list+ of data constructor declarations."
;;

let sygus_gsterm_of_sexp (s : Sexp.t) : sygus_gsterm =
  match s with
  | List [ Atom "Constant"; sygus_sort ] -> GConstant (sygus_sort_of_sexp sygus_sort)
  | List [ Atom "Variable"; sygus_sort ] -> GVar (sygus_sort_of_sexp sygus_sort)
  | t ->
    (try GTerm (sygus_term_of_sexp t) with
    | _ -> failwith (Fmt.str "Not a grammar sygus_term (%a)" Sexp.pp s))
;;

let sexp_of_sygus_gsterm (t : sygus_gsterm) : Sexp.t =
  match t with
  | GConstant c -> List [ Atom "Constant"; sexp_of_sygus_sort c ]
  | GTerm t -> sexp_of_sygus_term t
  | GVar v -> List [ Atom "Variable"; sexp_of_sygus_sort v ]
;;

let pre_grouped_rule_of_sexp (s : Sexp.t) =
  match s with
  | List [ name; sygus_sort; List gramsygus_terms ] ->
    ( symbol_of_sexp name
    , sygus_sort_of_sexp sygus_sort
    , List.map ~f:sygus_gsterm_of_sexp gramsygus_terms )
  | _ -> failwith "Not a grouped rule."
;;

let grammar_def_of_sexps (sv : Sexp.t) (gr : Sexp.t) : grammar_def =
  match sv, gr with
  | List sygus_sorts, List grouped_rules ->
    (match
       List.zip
         (List.map ~f:sorted_var_of_sexp sygus_sorts)
         (List.map ~f:pre_grouped_rule_of_sexp grouped_rules)
     with
    | Ok l -> List.map ~f:(fun (s, (_, _, g)) -> s, g) l
    | _ -> failwith "Not a grammar definition.")
  | _ -> failwith "Not a grammar definition."
;;

let sexp_of_grammar_def (gr : grammar_def) : Sexp.t * Sexp.t =
  let headers = List.map ~f:(fun (s, _) -> sexp_of_sorted_var s) gr in
  let grammar_decls =
    List.map gr ~f:(fun ((x, y), z) ->
        Sexp.List
          [ sexp_of_symbol x
          ; sexp_of_sygus_sort y
          ; Sexp.List (List.map ~f:sexp_of_sygus_gsterm z)
          ])
  in
  List headers, List grammar_decls
;;

let command_of_sexp (s : Sexp.t) : command =
  let command_of_elts (sl : Sexp.t list) : command =
    match sl with
    | [ Atom single ] ->
      (match single with
      | "check-synth" -> CCheckSynth
      | _ -> failwith (Fmt.str "Not a command: %a." Sexp.pp_hum s))
    | [ Atom com_name; arg ] ->
      (match com_name with
      | "constraint" -> CConstraint (sygus_term_of_sexp arg)
      | "set-logic" -> CSetLogic (symbol_of_sexp arg)
      | _ -> failwith (Fmt.str "Not a command: %a." Sexp.pp_hum s))
    | [ Atom "synth-inv"; name; List args ] ->
      CSynthInv (symbol_of_sexp name, List.map ~f:sorted_var_of_sexp args, None)
    | [ Atom "synth-fun"; name; List args; res ] ->
      CSynthFun
        ( symbol_of_sexp name
        , List.map ~f:sorted_var_of_sexp args
        , sygus_sort_of_sexp res
        , None )
    | [ Atom com_name; arg1; arg2 ] ->
      (match com_name with
      | "declare-var" -> CDeclareVar (symbol_of_sexp arg1, sygus_sort_of_sexp arg2)
      | "declare-datatype" ->
        CDeclareDataType (symbol_of_sexp arg1, dt_cons_dec_list_of_sexp arg2)
      | "declare-datatypes" ->
        (match arg2 with
        | List args2 ->
          let decls_l = sygus_sort_decl_list_of_sexp arg1 in
          let dt_const = List.map ~f:dt_cons_dec_list_of_sexp args2 in
          CDeclareDataTypes (decls_l, dt_const)
        | _ -> failwith "Not a proper datatypes-declaration.")
      | "declare-sort" -> CDeclareSort (symbol_of_sexp arg1, Int.t_of_sexp arg2)
      | "define-sort" -> CDefineSort (symbol_of_sexp arg1, sygus_sort_of_sexp arg2)
      | _ -> failwith (Fmt.str "Not a command: %a." Sexp.pp_hum s))
    | [ Atom com_name; Atom ":"; arg1; arg2 ] ->
      (match com_name with
      | "set-info" -> CSetInfo (symbol_of_sexp arg1, literal_of_sexp arg2)
      | "set-option" -> CSetOption (symbol_of_sexp arg1, literal_of_sexp arg2)
      | "set-feature" -> CSetFeature (feature_of_sexp arg1, bool_of_sexp arg2)
      | _ -> failwith (Fmt.str "Not a command: %a." Sexp.pp_hum s))
    | [ Atom "define-fun"; name; List args; res; body ] ->
      CDefineFun
        ( symbol_of_sexp name
        , List.map ~f:sorted_var_of_sexp args
        , sygus_sort_of_sexp res
        , sygus_term_of_sexp body )
    | [ Atom "synth-inv"; name; List args; gd1; gd2 ] ->
      CSynthInv
        ( symbol_of_sexp name
        , List.map ~f:sorted_var_of_sexp args
        , Some (grammar_def_of_sexps gd1 gd2) )
    | [ Atom "synth-fun"; name; List args; res; gd1; gd2 ] ->
      CSynthFun
        ( symbol_of_sexp name
        , List.map ~f:sorted_var_of_sexp args
        , sygus_sort_of_sexp res
        , Some (grammar_def_of_sexps gd1 gd2) )
    | [ Atom "inv-constraint"; a; b; c; d ] ->
      CInvConstraint
        (symbol_of_sexp a, symbol_of_sexp b, symbol_of_sexp c, symbol_of_sexp d)
    | _ -> failwith (Fmt.str "Not a command: %a." Sexp.pp_hum s)
  in
  match s with
  | List elts -> command_of_elts elts
  | _ -> failwith (Fmt.str "Not a command: %a." Sexp.pp_hum s)
;;

let sexp_of_command (c : command) : Sexp.t =
  match c with
  | CCheckSynth -> List [ Atom "check-synth" ]
  | CConstraint t -> List [ Atom "constraint"; sexp_of_sygus_term t ]
  | CDeclareVar (name, sygus_sort) ->
    List [ Atom "declare-var"; sexp_of_symbol name; sexp_of_sygus_sort sygus_sort ]
  | CInvConstraint (a, b, c, d) ->
    List [ Atom "inv-constraint"; Atom a; Atom b; Atom c; Atom d ]
  | CSetFeature (feat, boolc) ->
    List
      [ Atom "set-feature"; Atom ":"; sexp_of_feature feat; Atom (Bool.to_string boolc) ]
  | CSynthFun (name, args, res, body) ->
    (match body with
    | Some body ->
      let decls, gramm = sexp_of_grammar_def body in
      if !use_v1
      then
        (* No predeclaration in v1 *)
        List
          [ Atom "synth-fun"
          ; Atom name
          ; List (List.map ~f:sexp_of_sorted_var args)
          ; sexp_of_sygus_sort res
          ; gramm
          ]
      else
        List
          [ Atom "synth-fun"
          ; Atom name
          ; List (List.map ~f:sexp_of_sorted_var args)
          ; sexp_of_sygus_sort res
          ; decls
          ; gramm
          ]
    | None ->
      List
        [ Atom "synth-fun"
        ; Atom name
        ; List (List.map ~f:sexp_of_sorted_var args)
        ; sexp_of_sygus_sort res
        ])
  | CSynthInv (name, args, body) ->
    (match body with
    | Some body ->
      let decls, gramm = sexp_of_grammar_def body in
      List
        [ Atom "synth-fun"
        ; Atom name
        ; List (List.map ~f:sexp_of_sorted_var args)
        ; sexp_of_sygus_sort (SId (IdSimple "Bool"))
        ; decls
        ; gramm
        ]
    | None ->
      if !use_v1
      then
        List [ Atom "synth-inv"; Atom name; List (List.map ~f:sexp_of_sorted_var args) ]
      else
        List
          [ Atom "synth-fun"
          ; Atom name
          ; List (List.map ~f:sexp_of_sorted_var args)
          ; sexp_of_sygus_sort (SId (IdSimple "Bool"))
          ])
  | CDeclareDataType (name, dtdecls) ->
    List
      [ Atom "declare-datatype"
      ; Atom name
      ; List (List.map ~f:sexp_of_dt_cons_dec dtdecls)
      ]
  | CDeclareDataTypes (sortdec, dtdecls) ->
    List
      [ Atom "declare-datatypes"
      ; List (List.map ~f:sexp_of_sort_decl sortdec)
      ; List
          (List.map ~f:(fun x -> Sexp.List (List.map ~f:sexp_of_dt_cons_dec x)) dtdecls)
      ]
  | CDeclareSort (name, id) ->
    List [ Atom "declare-sort"; Atom name; Atom (Int.to_string id) ]
  | CDefineFun (name, args, res, body) ->
    List
      [ Atom "define-fun"
      ; Atom name
      ; List (List.map ~f:sexp_of_sorted_var args)
      ; sexp_of_sygus_sort res
      ; sexp_of_sygus_term body
      ]
  | CDefineSort (name, sygus_sort) ->
    List [ Atom "define-sort"; Atom name; sexp_of_sygus_sort sygus_sort ]
  | CSetInfo (sym, lit) ->
    List [ Atom "set-info"; Atom ":"; Atom sym; sexp_of_literal lit ]
  | CSetLogic sym -> List [ Atom "set-logic"; Atom sym ]
  | CSetOption (sym, lit) ->
    List [ Atom "set-option"; Atom ":"; Atom sym; sexp_of_literal lit ]
;;

let program_of_sexp_list (sexps : Sexp.t list) : program =
  List.map ~f:command_of_sexp sexps
;;

let sexp_list_of_program (p : program) : Sexp.t list = List.map ~f:sexp_of_command p

(* ============================================================================================= *)
(*                                      SOLVER RESPONSES                                         *)
(* ============================================================================================= *)

type solver_response =
  | RSuccess of (symbol * sorted_var list * sygus_sort * sygus_term) list
  | RInfeasible
  | RFail
  | RUnknown

let is_sat = function
  | RSuccess _ -> true
  | _ -> false
;;

let is_failed = function
  | RFail -> true
  | _ -> false
;;

let is_infeasible = function
  | RInfeasible -> true
  | _ -> false
;;

let reponse_of_sexps (s : Sexp.t list) : solver_response =
  let atomic_response (s : Sexp.t list) =
    match s with
    | [ Atom "fail" ] -> Some RFail
    | [ Atom "infeasible" ] -> Some RInfeasible
    | [ Atom "unknown" ] -> Some RUnknown
    | [ Atom "sat" ] -> Some RInfeasible
    | _ -> None
  in
  let one_command cmd =
    try
      match command_of_sexp cmd with
      | CDefineFun (f, args, res, body) -> Some [ f, args, res, body ]
      | _ -> None
    with
    | Failure _ -> None
  in
  let success_response s =
    RSuccess
      (List.concat
         (List.filter_map s ~f:(function
             | Sexp.Atom "unsat" -> None (* Ignore 'unsat' printed by CVC4. *)
             | Sexp.List l as cmd ->
               (match one_command cmd with
               (* A response composed of a single command. *)
               | Some s -> Some s
               | None ->
                 (match Option.all (List.map ~f:one_command l) with
                 | Some defs -> Some (List.concat defs)
                 | None -> None))
             | Sexp.Atom _ -> None)))
  in
  match atomic_response s with
  | Some r -> r
  | None -> success_response s
;;

(* ============================================================================================= *)
(*                                      WRAPPER MODULES                                          *)
(* ============================================================================================= *)

module SyCommand = struct
  type t = command

  let of_sexp = command_of_sexp
  let sexp_of = sexp_of_command
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

module SyTerm = struct
  type t = sygus_term

  let of_sexp = sygus_term_of_sexp
  let sexp_of = sexp_of_sygus_term
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

module SyIdentifier = struct
  type t = identifier

  let of_sexp = identifier_of_sexp
  let sexp_of = sexp_of_identifier
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

module SyLiteral = struct
  type t = literal

  let of_sexp = literal_of_sexp
  let sexp_of = sexp_of_literal
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

module SySort = struct
  type t = sygus_sort

  let of_sexp = sygus_sort_of_sexp
  let sexp_of = sexp_of_sygus_sort
  let pp fmt c = Sexp.pp fmt (sexp_of c)
  let pp_hum fmt c = Sexp.pp_hum fmt (sexp_of c)
end

let write_command (out : Stdio.Out_channel.t) (c : command) : unit =
  let comm_s = Fmt.(to_to_string SyCommand.pp c) in
  Stdio.Out_channel.(
    output_lines out [ comm_s ];
    flush out)
;;

(* ============================================================================================= *)
(*                       SEMANTIC PROPERTIES                                                     *)
(* ============================================================================================= *)

let is_setter_command (c : SyCommand.t) =
  match c with
  | CSetFeature _ | CSetInfo _ | CSetLogic _ | CSetOption _ -> true
  | _ -> false
;;

let is_well_formed (p : program) : bool =
  let setter_then_other l =
    let _, b =
      List.fold l ~init:(false, true) ~f:(fun (b0, b1) c ->
          let b = is_setter_command c in
          b0 && b, b1 && ((not b) || (b && b0)))
    in
    b
  in
  match p with
  | [] -> true
  | hd :: tl ->
    (match hd with
    | CSetLogic _ -> setter_then_other tl
    | _ -> setter_then_other p)
;;

let declares (c : command) : symbol list =
  match c with
  | CCheckSynth
  | CInvConstraint _
  | CSetFeature _
  | CSetInfo _
  | CSetOption _
  | CSetLogic _
  | CConstraint _ -> []
  | CDeclareVar (s, _)
  | CSynthFun (s, _, _, _)
  | CSynthInv (s, _, _)
  | CDeclareSort (s, _)
  | CDefineFun (s, _, _, _)
  | CDefineSort (s, _) -> [ s ]
  | CDeclareDataType (s, constrs) -> s :: List.map ~f:fst constrs
  | CDeclareDataTypes (sl, cd) ->
    List.map ~f:fst sl @ List.concat_map ~f:(List.map ~f:fst) cd
;;

let compare_declares (c1 : command) (c2 : command) =
  List.compare String.compare (declares c1) (declares c2)
;;

(* ============================================================================================= *)
(*                       STATIC DEFINITIONS                                                      *)
(* ============================================================================================= *)

let max_definition =
  SyCommand.of_sexp
    (Sexp.of_string "(define-fun max ((x Int) (y Int)) Int (ite (>= x y) x y))")
;;

let min_definition =
  SyCommand.of_sexp
    (Sexp.of_string "(define-fun min ((x Int) (y Int)) Int (ite (<= x y) x y))")
;;

(* ============================================================================================= *)
(*                       TRANSFORMERS                                                            *)
(* ============================================================================================= *)
let rec rename (subs : (symbol * symbol) list) (t : sygus_term) : sygus_term =
  match t with
  | SyId (IdSimple s) ->
    (match List.Assoc.find ~equal:String.equal subs s with
    | Some s' -> SyId (IdSimple s')
    | None -> t)
  | SyApp (IdSimple f, args) ->
    let args' = List.map ~f:(rename subs) args in
    (match List.Assoc.find ~equal:String.equal subs f with
    | Some f' -> SyApp (IdSimple f', args')
    | None -> SyApp (IdSimple f, args'))
  | SyApp (f, args) -> SyApp (f, List.map ~f:(rename subs) args)
  | SyExists (vars, body) ->
    let subs' =
      List.filter ~f:(fun (l, _) -> List.Assoc.mem ~equal:String.equal vars l) subs
    in
    SyExists (vars, rename subs' body)
  | SyForall (vars, body) ->
    let subs' =
      List.filter ~f:(fun (l, _) -> List.Assoc.mem ~equal:String.equal vars l) subs
    in
    SyForall (vars, rename subs' body)
  | SyLit _ | SyId _ -> t
  | SyLet (bindings, body) ->
    let bindings' =
      List.map ~f:(fun (varname, body) -> varname, rename subs body) bindings
    in
    let subs' =
      List.filter ~f:(fun (l, _) -> List.Assoc.mem ~equal:String.equal bindings l) subs
    in
    let body' = rename subs' body in
    SyLet (bindings', body')
;;
