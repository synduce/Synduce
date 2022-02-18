(** This module implements an interface to the SMT-LIB standard. Some parts of documentation
  will refer to a specific section of the SMT-LIB standard available online
   ({i http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2021-05-12.pdf}).
*)

open Base
open Sexp
open Option.Let_syntax
module OC = Stdio.Out_channel
module IC = Stdio.In_channel

(** The type of numbers. *)
type numeral = int

(** A SMT symbol is either a string identifier (e.g. [x]) or a quoted symbol (e.g. [| this|]).
  See Section 3.1 of the SMT-LIB language standard.
 *)
type smtSymbol =
  | SSimple of string
  | SQuoted of string

let symb_equal s1 s2 =
  match s1, s2 with
  | SSimple name1, SSimple name2 -> String.equal name1 name2
  | SQuoted name1, SQuoted name2 -> String.equal name1 name2
  | _, _ -> false
;;

let str_of_symb s1 =
  match s1 with
  | SSimple s -> s
  | SQuoted s -> "`" ^ s
;;

(** An SMT index is either a number or a symbol (Section 3.3). *)
type smtIndex =
  | INum of numeral
  | ISym of smtSymbol

(** A SMT identifier is either a symbol or an indexed symbol (Section 3.3). *)
type smtIdentifier =
  | Id of smtSymbol
  | IdC of smtSymbol * smtIndex list
      (** For example, [(_ vector-add 4 5)] is an indexed identifier. *)

(** A SMT Sort (a type) is an identifier or a composite type. *)
type smtSort =
  | SmtSort of smtIdentifier
      (** For example [SmtSort(IdC(SSimple("BitVec"), INum(3)))] is [(_ BitVec 3)]*)
  | Comp of smtIdentifier * smtSort list
      (** For example [Comp(Id(SSimple("Set")),SmtSort(Id(SSimple("Int"))))] is [(Set Int)]*)

(** A SMT sorted var is a symbol associated with a sort. *)
type smtSortedVar = smtSymbol * smtSort

(** A qualified identifier is either a simple identifier [QI ...] or an identifier
  qualified with a sort [QIas(x,y)] is [(as x y)].
*)
type smtQualIdentifier =
  | QI of smtIdentifier
  | QIas of smtIdentifier * smtSort

(** An SMT attribute that is either a string symbol or a symbol with a string value. *)
type smtAttribute =
  | ASymb of string
  | ASymbVal of string * string

(** A SMT pattern is either a variable (a SMT symbol) or a constructor (a pair
 of a constructor name with a lsit of arguments).  *)
type smtPattern =
  | Pat of smtSymbol
  | PatComp of smtSymbol * smtSymbol list

(** The type of SMT special constants (Section 3.1). *)
type smtSpecConstant =
  | SCNumeral of numeral
  | SCDecimal of numeral * int * numeral
  | SCHexaDecimal of string
  | SCBinary of bool list
  | SCString of string

type smtTerm =
  | SmtTSpecConst of smtSpecConstant
  | SmtTQualdId of smtQualIdentifier
  | SmtTApp of smtQualIdentifier * smtTerm list
  | SmtTLet of var_binding list * smtTerm
  | SmtTForall of smtSortedVar list * smtTerm
  | SmtTExists of smtSortedVar list * smtTerm
  | SmtTMatch of smtTerm * match_case list
  | SmtTAnnot of smtTerm * smtAttribute list

and match_case = smtPattern * smtTerm
and var_binding = smtSymbol * smtTerm

type smtSortDec = smtSymbol * numeral
type smtSelectorDec = smtSymbol * smtSort
type smtConstructorDec = smtSymbol * smtSelectorDec list

type datatype_dec =
  | DDConstr of smtConstructorDec list
  | DDParametric of smtSymbol list * smtConstructorDec list

type func_dec = smtSymbol * smtSortedVar list * smtSort
type func_def = smtSymbol * smtSortedVar list * smtSort * smtTerm

type prop_literal =
  | PL of smtSymbol
  | PLNot of smtSymbol

type info_flag = int
type solver_option = string * string

type command =
  | Assert of smtTerm
  | CheckSat
  | CheckSatAssuming of prop_literal list
  | DeclareConst of smtSymbol * smtSort
  | DeclareDatatype of smtSymbol * datatype_dec
  | DeclareDatatypes of smtSortDec list * datatype_dec list
  | DeclareFun of smtSymbol * smtSort list * smtSort
  | DeclareSmtSort of smtSymbol * numeral
  | DefineFun of func_def
  | DefineFunRec of func_def
  | DefineFunsRec of func_dec list * smtTerm list
  | DefineSmtSort of smtSymbol * smtSymbol list * smtSort
  | Echo of string
  | Exit
  | GetAssertions
  | GetAssignment
  | GetInfo of info_flag
  | GetModel
  | GetOption of string
  | GetProof
  | GetUnsatAssumptions
  | GetUnsatCore
  | GetValue of smtTerm list
  | Pop of numeral
  | Push of numeral
  | Reset
  | ResetAssertions
  | SetInfo of string
  | SetLogic of smtSymbol
  | SetOption of solver_option
  (* Z3 specific *)
  | Simplify of smtTerm * solver_option list

type script = command list

let sexps_of_option ((oname, ovalue) : solver_option) : t list =
  [ Atom (":" ^ oname); Atom ovalue ]
;;

let option_of_sexp (sexp : t) : solver_option option =
  match sexp with
  | List [ Atom s; Atom v ] ->
    if String.is_prefix s ~prefix:":" then Some (String.drop_prefix s 1, v) else None
  | _ -> None
;;

let sexp_of_info_flag (i : info_flag) : t =
  match i with
  | 0 -> Atom ":all-statistics"
  | 1 -> Atom ":assertion-stack-levels"
  | 2 -> Atom ":authors"
  | 3 -> Atom ":error-behavior"
  | 4 -> Atom ":name"
  | 5 -> Atom ":reason-unknown"
  | 6 -> Atom ":version"
  | _ -> Atom Fmt.(str "UNKNOWN_INFO_FLAG(%i)" i)
;;

let info_flag_of_sexp (sexp : t) : info_flag =
  match sexp with
  | Atom ":all-statistics" -> 0
  | Atom ":assertion-stack-levels" -> 1
  | Atom ":authors" -> 2
  | Atom ":error-behavior" -> 3
  | Atom ":name" -> 4
  | Atom ":reason-unknown" -> 5
  | Atom ":version" -> 6
  | _ -> 7
;;

let sexp_of_numeral (n : numeral) : Sexp.t =
  if Int.(n >= 0)
  then Sexp.Atom Fmt.(str "%i" n)
  else Sexp.(List [ Atom "-"; Atom Fmt.(str "%i" (abs n)) ])
;;

let numeral_of_sexp (sexp : Sexp.t) : numeral option =
  match sexp with
  | Atom s ->
    (try Some (Int.of_string s) with
    | _ -> None)
  | _ -> None
;;

let sexp_of_smtSpecConstant (sc : smtSpecConstant) : t =
  match sc with
  | SCNumeral num -> sexp_of_numeral num
  | SCDecimal (n1, d, n2) ->
    Atom
      Fmt.(
        str
          "%s.%a%s"
          (Int.to_string n1)
          (list ~sep:(fun _ _ -> ()) (fun f' _ -> Fmt.(pf f' "0")))
          (List.init d ~f:(fun _ -> 0))
          (Int.to_string n2))
  | SCHexaDecimal s -> Atom Fmt.(str "#x%s" s)
  | SCBinary bl ->
    Atom
      Fmt.(
        str
          "#b%a"
          (list ~sep:(fun _ _ -> ()) (fun f b -> if b then pf f "1" else pf f "0"))
          bl)
  | SCString s -> Atom Fmt.(str "\"%s\"" s)
;;

let smtSpecConstant_of_sexp (sexp : t) : smtSpecConstant option =
  match sexp with
  | Atom s ->
    if String.is_prefix ~prefix:"#b" s
    then failwith "Binary unsupported for now!"
    else if String.is_prefix ~prefix:"#b" s
    then Some (SCHexaDecimal (String.drop_prefix s 2))
    else if String.is_prefix ~prefix:"\"" s
    then Some (SCString (String.drop_prefix (String.drop_prefix s 1) 1))
    else (
      try Some (SCNumeral (Int.of_string s)) with
      | _ -> None)
  | _ -> None
;;

let sexp_of_smtSymbol (s : smtSymbol) : t =
  match s with
  | SSimple x -> Atom x
  | SQuoted x -> Atom ("|" ^ x ^ "|")
;;

let smtSymbol_of_sexp (s : t) : smtSymbol option =
  match s with
  | Atom x ->
    if String.is_prefix ~prefix:"|" x && String.is_suffix ~suffix:"|" x
    then Some (SQuoted (String.drop_prefix (String.drop_suffix x 1) 1))
    else Some (SSimple x)
  | _ -> None
;;

let sexp_of_smtIndex (idx : smtIndex) : t =
  match idx with
  | INum n -> Atom (Int.to_string n)
  | ISym s -> sexp_of_smtSymbol s
;;

let smtIndex_of_sexp (s : t) : smtIndex option =
  let num =
    let%map n = numeral_of_sexp s in
    INum n
  in
  let sym =
    let%map symb = smtSymbol_of_sexp s in
    ISym symb
  in
  Option.first_some num sym
;;

let sexp_of_smtIdentifier (id : smtIdentifier) : t =
  match id with
  | Id s -> sexp_of_smtSymbol s
  | IdC (s, ixl) ->
    let l = List (List.map ~f:sexp_of_smtIndex ixl) in
    List [ Atom "_"; sexp_of_smtSymbol s; l ]
;;

let smtIdentifier_of_sexp (sexp : t) : smtIdentifier option =
  match sexp with
  | Atom _ ->
    let%bind s = smtSymbol_of_sexp sexp in
    Some (Id s)
  | List (Atom "_" :: symb :: indexes) ->
    let%bind s = smtSymbol_of_sexp symb in
    let%bind l = Option.all (List.map ~f:smtIndex_of_sexp indexes) in
    Some (IdC (s, l))
  | _ -> None
;;

let rec sexp_of_smtSort (s : smtSort) : t =
  match s with
  | SmtSort x -> sexp_of_smtIdentifier x
  | Comp (x, l) -> List (sexp_of_smtIdentifier x :: List.map ~f:sexp_of_smtSort l)
;;

let rec smtSort_of_sexp (s : t) : smtSort option =
  match smtIdentifier_of_sexp s with
  | Some x -> Some (SmtSort x)
  | None ->
    (match s with
    | List (hd :: tl) ->
      let%bind id = smtIdentifier_of_sexp hd in
      let%bind args = Option.all (List.map ~f:smtSort_of_sexp tl) in
      Some (Comp (id, args))
    | _ -> None)
;;

let sexp_of_smtAttribute (attr : smtAttribute) : t =
  match attr with
  | ASymb anam -> Atom anam
  | ASymbVal (aname, aval) -> List [ Atom aname; Atom aval ]
;;

let smtAttribute_of_sexp (sexp : t) : smtAttribute option =
  match sexp with
  | Atom s -> Some (ASymb s)
  | List [ Atom name; Atom value ] -> Some (ASymbVal (name, value))
  | _ -> None
;;

let sexp_of_sortedVar ((s, srt) : smtSortedVar) : t =
  List [ sexp_of_smtSymbol s; sexp_of_smtSort srt ]
;;

let sortedVar_of_sexp (s : t) : smtSortedVar option =
  match s with
  | List [ symb; sort ] ->
    let%bind s1 = smtSymbol_of_sexp symb in
    let%bind s2 = smtSort_of_sexp sort in
    Some (s1, s2)
  | _ -> None
;;

let sexp_of_smtQualIdentifier (qi : smtQualIdentifier) =
  match qi with
  | QI s -> sexp_of_smtIdentifier s
  | QIas (s, smtSort) ->
    List [ Atom "as"; sexp_of_smtIdentifier s; sexp_of_smtSort smtSort ]
;;

let smtQualIdentifier_of_sexp (s : t) : smtQualIdentifier option =
  match s with
  | List [ Atom "as"; sid; ssort ] ->
    let%bind id = smtIdentifier_of_sexp sid in
    let%map sort = smtSort_of_sexp ssort in
    QIas (id, sort)
  | _ ->
    let%map id = smtIdentifier_of_sexp s in
    QI id
;;

let sexp_of_smtPattern (p : smtPattern) : t =
  match p with
  | Pat s -> sexp_of_smtSymbol s
  | PatComp (s, sl) -> List (sexp_of_smtSymbol s :: List.map ~f:sexp_of_smtSymbol sl)
;;

let smtPattern_of_sexp (s : t) : smtPattern option =
  match s with
  | Atom _ ->
    let%map symb = smtSymbol_of_sexp s in
    Pat symb
  | List (hd :: tl) ->
    let%bind s = smtSymbol_of_sexp hd in
    let%map l = Option.all (List.map ~f:smtSymbol_of_sexp tl) in
    PatComp (s, l)
  | _ -> None
;;

let rec sexp_of_smtTerm (t : smtTerm) : t =
  match t with
  | SmtTSpecConst sc -> sexp_of_smtSpecConstant sc
  | SmtTQualdId qi -> sexp_of_smtQualIdentifier qi
  | SmtTApp (func, args) ->
    List (sexp_of_smtQualIdentifier func :: List.map ~f:sexp_of_smtTerm args)
  | SmtTLet (bindings, t') ->
    List [ Atom "let"; List (List.map ~f:sexp_of_binding bindings); sexp_of_smtTerm t' ]
  | SmtTForall (quants, t') ->
    List
      [ Atom "forall"; List (List.map ~f:sexp_of_sortedVar quants); sexp_of_smtTerm t' ]
  | SmtTExists (quants, t') ->
    List
      [ Atom "exists"; List (List.map ~f:sexp_of_sortedVar quants); sexp_of_smtTerm t' ]
  | SmtTMatch (t', cases) ->
    List [ Atom "match"; sexp_of_smtTerm t'; List (List.map ~f:sexp_of_match_case cases) ]
  | SmtTAnnot (t', attrs) ->
    List (Atom "!" :: sexp_of_smtTerm t' :: List.map ~f:sexp_of_smtAttribute attrs)

and sexp_of_binding ((v, e) : var_binding) : t =
  List [ sexp_of_smtSymbol v; sexp_of_smtTerm e ]

and sexp_of_match_case ((p, t) : match_case) =
  List [ sexp_of_smtPattern p; sexp_of_smtTerm t ]
;;

let rec smtTerm_of_sexp (s : t) : smtTerm option =
  match s with
  | List [ Atom "let"; List bindings; body ] ->
    let%bind bindings = Option.all (List.map ~f:binding_of_sexp bindings) in
    let%map body = smtTerm_of_sexp body in
    SmtTLet (bindings, body)
  | List [ Atom "forall"; List quants; t ] ->
    let%bind qs = Option.all (List.map ~f:sortedVar_of_sexp quants) in
    let%map body = smtTerm_of_sexp t in
    SmtTForall (qs, body)
  | List [ Atom "exists"; List quants; t ] ->
    let%bind qs = Option.all (List.map ~f:sortedVar_of_sexp quants) in
    let%map body = smtTerm_of_sexp t in
    SmtTExists (qs, body)
  | List [ Atom "match"; trm; List cases ] ->
    let%bind t = smtTerm_of_sexp trm in
    let%map cases' = Option.all (List.map ~f:match_case_of_sexp cases) in
    SmtTMatch (t, cases')
  | List (Atom "!" :: t :: attrs) ->
    let%bind t' = smtTerm_of_sexp t in
    let%map attrs' = Option.all (List.map ~f:smtAttribute_of_sexp attrs) in
    SmtTAnnot (t', attrs')
  | _ ->
    (match smtSpecConstant_of_sexp s with
    | Some cs -> Some (SmtTSpecConst cs)
    | None ->
      (match smtQualIdentifier_of_sexp s with
      | Some qi -> Some (SmtTQualdId qi)
      | None ->
        (match s with
        | List (func :: args) ->
          let%bind func' = smtQualIdentifier_of_sexp func in
          let%map args' = Option.all (List.map ~f:smtTerm_of_sexp args) in
          SmtTApp (func', args')
        | _ -> None)))

and match_case_of_sexp (s : t) : match_case option =
  match s with
  | List [ pat; rhs ] ->
    let%bind pat' = smtPattern_of_sexp pat in
    let%map t = smtTerm_of_sexp rhs in
    pat', t
  | _ -> None

and binding_of_sexp (s : t) : var_binding option =
  match s with
  | List [ x; v ] ->
    let%bind x' = smtSymbol_of_sexp x in
    let%map v' = smtTerm_of_sexp v in
    x', v'
  | _ -> None
;;

let sexp_of_smtSelectorDec ((s, srt) : smtSelectorDec) : t =
  List [ sexp_of_smtSymbol s; sexp_of_smtSort srt ]
;;

let smtSelectorDec_of_sexp (s : Sexp.t) : smtSelectorDec option =
  match s with
  | List [ s; srt ] ->
    let%bind s = smtSymbol_of_sexp s in
    let%map srt = smtSort_of_sexp srt in
    s, srt
  | _ -> None
;;

let sexp_of_smtConstructorDec ((s, sdecs) : smtConstructorDec) : t =
  List (sexp_of_smtSymbol s :: List.map ~f:sexp_of_smtSelectorDec sdecs)
;;

let smtConstructorDec_of_sexp (s : Sexp.t) : smtConstructorDec option =
  match s with
  | List (s :: sdecs) ->
    let%bind s = smtSymbol_of_sexp s in
    let%map sdecs = Option.all (List.map ~f:smtSelectorDec_of_sexp sdecs) in
    s, sdecs
  | _ -> None
;;

let sexp_of_datatype_dec (dtdec : datatype_dec) : t =
  match dtdec with
  | DDConstr cd_list -> List (List.map ~f:sexp_of_smtConstructorDec cd_list)
  | DDParametric (symbs, cd_list) ->
    List
      [ Atom "par"
      ; List (List.map ~f:sexp_of_smtSymbol symbs)
      ; List (List.map ~f:sexp_of_smtConstructorDec cd_list)
      ]
;;

let datatype_dec_of_sexp (s : Sexp.t) : datatype_dec option =
  match s with
  | List [ Atom "par"; List symbs; List cdecs ] ->
    let%bind symbs = Option.all (List.map ~f:smtSymbol_of_sexp symbs) in
    let%map constrs = Option.all (List.map ~f:smtConstructorDec_of_sexp cdecs) in
    DDParametric (symbs, constrs)
  | List cd_list ->
    let%map x = Option.all (List.map ~f:smtConstructorDec_of_sexp cd_list) in
    DDConstr x
  | _ -> None
;;

let sexp_of_func_dec ((s, svs, srt) : func_dec) : t =
  List
    [ sexp_of_smtSymbol s; List (List.map ~f:sexp_of_sortedVar svs); sexp_of_smtSort srt ]
;;

let func_dec_of_sexpl (s : Sexp.t list) : func_dec option =
  match s with
  | [ name; List args; res_sort ] ->
    let%bind name = smtSymbol_of_sexp name in
    let%bind args = Option.all (List.map ~f:sortedVar_of_sexp args) in
    let%map res = smtSort_of_sexp res_sort in
    name, args, res
  | _ -> None
;;

let func_dec_of_sexp (s : Sexp.t) : func_dec option =
  match s with
  | List [ name; List args; res_sort ] ->
    let%bind name = smtSymbol_of_sexp name in
    let%bind args = Option.all (List.map ~f:sortedVar_of_sexp args) in
    let%map res = smtSort_of_sexp res_sort in
    name, args, res
  | _ -> None
;;

let sexp_of_func_def ((s, svs, srt, t) : func_def) : t list =
  [ sexp_of_smtSymbol s
  ; List (List.map ~f:sexp_of_sortedVar svs)
  ; sexp_of_smtSort srt
  ; sexp_of_smtTerm t
  ]
;;

let func_def_of_sexp (s : Sexp.t list) : func_def option =
  match s with
  | [ name; List args; res_sort; body ] ->
    let%bind name = smtSymbol_of_sexp name in
    let%bind args = Option.all (List.map ~f:sortedVar_of_sexp args) in
    let%bind res = smtSort_of_sexp res_sort in
    let%map body = smtTerm_of_sexp body in
    name, args, res, body
  | _ -> None
;;

let sexp_of_prop_literal (pl : prop_literal) : t =
  match pl with
  | PL x -> sexp_of_smtSymbol x
  | PLNot x -> List [ Atom "not"; sexp_of_smtSymbol x ]
;;

let prop_literal_of_sexp (s : Sexp.t) : prop_literal option =
  match s with
  | List [ Atom "not"; x ] ->
    let%map x = smtSymbol_of_sexp x in
    PLNot x
  | x ->
    let%map x = smtSymbol_of_sexp x in
    PL x
;;

let sexp_of_smtSortDec ((s, n) : smtSortDec) : t =
  List [ sexp_of_smtSymbol s; sexp_of_numeral n ]
;;

let smtSortDec_of_sexp (s : Sexp.t) : smtSortDec option =
  match s with
  | List [ s; n ] ->
    let%bind s = smtSymbol_of_sexp s in
    let%map n = numeral_of_sexp n in
    s, n
  | _ -> None
;;

let sexp_of_command (c : command) =
  match c with
  | Assert t -> List [ Atom "assert"; sexp_of_smtTerm t ]
  | CheckSat -> List [ Atom "check-sat" ]
  | CheckSatAssuming pl_list ->
    List [ Atom "check-sat-assuming"; List (List.map ~f:sexp_of_prop_literal pl_list) ]
  | DeclareConst (sym, smtSort) ->
    List [ Atom "declare-const"; sexp_of_smtSymbol sym; sexp_of_smtSort smtSort ]
  | DeclareDatatype (sym, ddec) ->
    List [ Atom "declare-datatype"; sexp_of_smtSymbol sym; sexp_of_datatype_dec ddec ]
  | DeclareDatatypes (sdec_l, ddec_l) ->
    List
      [ Atom "declare-datatypes"
      ; List (List.map ~f:sexp_of_smtSortDec sdec_l)
      ; List (List.map ~f:sexp_of_datatype_dec ddec_l)
      ]
  | DeclareFun (name, args, res) ->
    List
      [ Atom "declare-fun"
      ; sexp_of_smtSymbol name
      ; List (List.map ~f:sexp_of_smtSort args)
      ; sexp_of_smtSort res
      ]
  | DeclareSmtSort (s, n) ->
    List [ Atom "declare-smtSort"; sexp_of_smtSymbol s; sexp_of_numeral n ]
  | DefineFun fd -> List (Atom "define-fun" :: sexp_of_func_def fd)
  | DefineFunRec fd -> List (Atom "define-fun-rec" :: sexp_of_func_def fd)
  | DefineFunsRec (fdl, tl) ->
    List
      [ Atom "define-funs-rec"
      ; List (List.map ~f:sexp_of_func_dec fdl)
      ; List (List.map ~f:sexp_of_smtTerm tl)
      ]
  | DefineSmtSort (s, sl, smtSort) ->
    List
      [ Atom "define-sort"
      ; sexp_of_smtSymbol s
      ; List (List.map ~f:sexp_of_smtSymbol sl)
      ; sexp_of_smtSort smtSort
      ]
  | Echo s -> List [ Atom "echo"; Atom s ]
  | Exit -> List [ Atom "exit" ]
  | GetAssertions -> List [ Atom "get-assertions" ]
  | GetAssignment -> List [ Atom "get-assignment" ]
  | GetInfo iflag -> List [ Atom "get-info"; sexp_of_info_flag iflag ]
  | GetModel -> List [ Atom "get-model" ]
  | GetOption kw -> List [ Atom "get-option"; Atom kw ]
  | GetProof -> List [ Atom "get-proof" ]
  | GetUnsatAssumptions -> List [ Atom "get-unsat-assumptions" ]
  | GetUnsatCore -> List [ Atom "get-unsat-core" ]
  | GetValue tl -> List (Atom "get-value" :: (List.map ~f:sexp_of_smtTerm) tl)
  | Pop n -> List [ Atom "pop"; sexp_of_numeral n ]
  | Push n -> List [ Atom "push"; sexp_of_numeral n ]
  | Reset -> List [ Atom "reset" ]
  | ResetAssertions -> List [ Atom "reset-assertions" ]
  | SetInfo attr -> List [ Atom "set-info"; Atom attr ]
  | SetLogic s -> List [ Atom "set-logic"; sexp_of_smtSymbol s ]
  | SetOption o -> List (Atom "set-option" :: sexps_of_option o)
  (* Solver-specific commands *)
  | Simplify (t, ol) ->
    if List.is_empty ol
    then List [ Atom "simplify"; sexp_of_smtTerm t ]
    else
      List
        (Atom "simplify"
        :: sexp_of_smtTerm t
        :: List.concat (List.map ~f:sexps_of_option ol))
;;

let command_of_sexp (s : Sexp.t) : command option =
  match s with
  | List [ Atom "assert"; t ] -> Option.map ~f:(fun x -> Assert x) (smtTerm_of_sexp t)
  | List [ Atom "check-sat" ] -> Some CheckSat
  | List [ Atom "check-sat-assuming"; List pl_list ] ->
    let%map pls = Option.all (List.map ~f:prop_literal_of_sexp pl_list) in
    CheckSatAssuming pls
  | List [ Atom "declare-const"; sym; smts ] ->
    let%bind sym = smtSymbol_of_sexp sym in
    let%map sort = smtSort_of_sexp smts in
    DeclareConst (sym, sort)
  | List [ Atom "declare-datatype"; sym; ddec ] ->
    let%bind name = smtSymbol_of_sexp sym in
    let%map ddec = datatype_dec_of_sexp ddec in
    DeclareDatatype (name, ddec)
  | List [ Atom "declare-datatypes"; List sdec_l; List ddec_l ] ->
    let%bind decls = Option.all (List.map ~f:smtSortDec_of_sexp sdec_l) in
    let%map ddecls = Option.all (List.map ~f:datatype_dec_of_sexp ddec_l) in
    DeclareDatatypes (decls, ddecls)
  | List [ Atom "declare-fun"; name; List args; res ] ->
    let%bind name = smtSymbol_of_sexp name in
    let%bind args = Option.all (List.map ~f:smtSort_of_sexp args) in
    let%map res = smtSort_of_sexp res in
    DeclareFun (name, args, res)
  | List [ Atom "declare-smtSort"; s; n ] ->
    let%bind s = smtSymbol_of_sexp s in
    let%map n = numeral_of_sexp n in
    DeclareSmtSort (s, n)
  | List (Atom "define-fun" :: fd) ->
    let%map x = func_def_of_sexp fd in
    DefineFun x
  | List (Atom "define-fun-rec" :: fd) ->
    let%map x = func_def_of_sexp fd in
    DefineFunRec x
  | List [ Atom "define-funs-rec"; List fdl; List tl ] ->
    let%bind decs = Option.all (List.map ~f:func_dec_of_sexp fdl) in
    let%map bodies = Option.all (List.map ~f:smtTerm_of_sexp tl) in
    DefineFunsRec (decs, bodies)
  | List [ Atom "define-sort"; s; List sl; smts ] ->
    let%bind s = smtSymbol_of_sexp s in
    let%bind sl = Option.all (List.map ~f:smtSymbol_of_sexp sl) in
    let%map smts = smtSort_of_sexp smts in
    DefineSmtSort (s, sl, smts)
  | List [ Atom "echo"; Atom s ] -> Some (Echo s)
  | List [ Atom "exit" ] -> Some Exit
  | List [ Atom "get-assertions" ] -> Some GetAssertions
  | List [ Atom "get-assignment" ] -> Some GetAssignment
  | List [ Atom "get-info"; iflag ] -> Some (GetInfo (info_flag_of_sexp iflag))
  | List [ Atom "get-model" ] -> Some GetModel
  | List [ Atom "get-option"; Atom kw ] -> Some (GetOption kw)
  | List [ Atom "get-proof" ] -> Some GetProof
  | List [ Atom "get-unsat-assumptions" ] -> Some GetUnsatAssumptions
  | List [ Atom "get-unsat-core" ] -> Some GetUnsatCore
  | List (Atom "get-value" :: tl) ->
    let%map x = Option.all (List.map ~f:smtTerm_of_sexp tl) in
    GetValue x
  | List [ Atom "pop"; n ] ->
    let%map x = numeral_of_sexp n in
    Pop x
  | List [ Atom "push"; n ] ->
    let%map x = numeral_of_sexp n in
    Push x
  | List [ Atom "reset" ] -> Some Reset
  | List [ Atom "reset-assertions" ] -> Some ResetAssertions
  | List [ Atom "set-info"; Atom attr ] -> Some (SetInfo attr)
  | List [ Atom "set-logic"; s ] ->
    Option.map ~f:(fun x -> SetLogic x) (smtSymbol_of_sexp s)
  | List (Atom "set-option" :: o) ->
    Option.map ~f:(fun x -> SetOption x) (option_of_sexp (List o))
  | _ -> None
;;

let write_command (out : OC.t) (c : command) : unit =
  let comm_s = Sexp.to_string_hum (sexp_of_command c) in
  OC.output_lines out [ comm_s ];
  OC.flush out
;;

let pp_script (f : Formatter.t) (s : script) =
  List.iter ~f:(fun cm -> Fmt.pf f "%a@." Sexp.pp (sexp_of_command cm)) s
;;

(* Term construction *)
(* SmtSymbol *)
let mk_symb s = SSimple s

(* Sorts *)
let mk_int_sort = SmtSort (Id (SSimple "Int"))
let mk_bool_sort = SmtSort (Id (SSimple "Bool"))
let mk_seq_sort t = Comp (Id (SSimple "Seq"), [ t ])
let mk_tuple_sort lt = Comp (Id (SSimple "Tuple"), lt)
let mk_simple_sort name = SmtSort (Id (SSimple name))

(* Terms *)
let mk_qi s = QI (Id (SSimple s))
let mk_int (i : int) = SmtTSpecConst (SCNumeral i)
let mk_false = SmtTQualdId (QI (Id (SSimple "false")))
let mk_true = SmtTQualdId (QI (Id (SSimple "true")))
let mk_nil = SmtTQualdId (QI (Id (SSimple "nil")))
let mk_simple_app (s : string) (args : smtTerm list) = SmtTApp (QI (Id (SSimple s)), args)
let mk_var (s : string) = SmtTQualdId (QI (Id (SSimple s)))
let mk_ite c e1 e2 = mk_simple_app "ite" [ c; e1; e2 ]
let mk_add t1 t2 = mk_simple_app "+" [ t1; t2 ]
let mk_sub t1 t2 = mk_simple_app "-" [ t1; t2 ]
let mk_mul t1 t2 = mk_simple_app "*" [ t1; t2 ]
let mk_div t1 t2 = mk_simple_app "/" [ t1; t2 ]
let mk_and t1 t2 = mk_simple_app "and" [ t1; t2 ]

let mk_assoc_and tl =
  match tl with
  | [] -> mk_true
  | _ -> mk_simple_app "and" tl
;;

let mk_or t1 t2 = mk_simple_app "or" [ t1; t2 ]
let mk_eq t1 t2 = mk_simple_app "=" [ t1; t2 ]
let mk_lt t1 t2 = mk_simple_app "<" [ t1; t2 ]
let mk_gt t1 t2 = mk_simple_app ">" [ t1; t2 ]
let mk_le t1 t2 = mk_simple_app "<=" [ t1; t2 ]
let mk_ge t1 t2 = mk_simple_app ">=" [ t1; t2 ]
let mk_not t1 = mk_simple_app "not" [ t1 ]
let mk_let (bindings : var_binding list) (t : smtTerm) = SmtTLet (bindings, t)

let mk_exists (sorted_vars : smtSortedVar list) (term : smtTerm) =
  SmtTExists (sorted_vars, term)
;;

let mk_forall (sorted_vars : smtSortedVar list) (term : smtTerm) =
  SmtTForall (sorted_vars, term)
;;

(* Commands *)
let mk_assert (t : smtTerm) = Assert t
let mk_const_decl (s : string) (t : smtSort) = DeclareConst (SSimple s, t)
let mk_check_sat = CheckSat

let mk_fun_decl (s : string) (args : smtSort list) (res : smtSort) =
  DeclareFun (SSimple s, args, res)
;;

let mk_fun_def
    (s : string)
    (args : (string * smtSort) list)
    (res_sort : smtSort)
    (body : smtTerm)
  =
  DefineFun (mk_symb s, List.map ~f:(fun (s, t) -> mk_symb s, t) args, res_sort, body)
;;

let mk_named_assert (name : string) (t : smtTerm) =
  Assert (SmtTAnnot (t, [ ASymbVal (":named", name) ]))
;;

let mk_set_option (oname : string) (oval : string) = SetOption (oname, oval)
let mk_set_logic (l : Logics.logic) = SetLogic (SSimple (Logics.to_string l))
let mk_simplify ?(options : solver_option list = []) (t : smtTerm) = Simplify (t, options)
let mk_print_success = SetOption ("print-success", "true")
let mk_exit = Exit
let mk_pop i = Pop i
let mk_push i = Push i

(* Specific function declarations *)

(** [mk_min_def] is the following SMT-LIB command:
  {v
  (define-function min ((x Int) (y Int)) Int (ite (<= x y) x y))
  v}
*)
let mk_min_def =
  mk_fun_def
    "min"
    [ "x", mk_int_sort; "y", mk_int_sort ]
    mk_int_sort
    (mk_ite (mk_le (mk_var "x") (mk_var "y")) (mk_var "x") (mk_var "y"))
;;

(** [mk_max_def] is the following SMT-LIB command:
  {v
  (define-function max ((x Int) (y Int)) Int (ite (>= x y) x y))
  v}
*)
let mk_max_def =
  mk_fun_def
    "max"
    [ "x", mk_int_sort; "y", mk_int_sort ]
    mk_int_sort
    (mk_ite (mk_ge (mk_var "x") (mk_var "y")) (mk_var "x") (mk_var "y"))
;;

(** [mk_inv_def] is the following SMT-LIB command:
  {v
  (define-function inv ((x Int)) Int (div 1 x))
  v}
*)
let mk_inv_def =
  mk_fun_def "inv" [ "x", mk_int_sort ] mk_int_sort (mk_div (mk_int 1) (mk_var "x"))
;;

(* SmtTerm / command helpers *)

let smtSymbol_of_decl (decl : command) =
  match decl with
  | DeclareConst (s, _) -> Some s
  | DeclareFun (s, _, _) -> Some s
  | DeclareDatatype (s, _) -> Some s
  | DeclareSmtSort (s, _) -> Some s
  | DefineFun (s, _, _, _) -> Some s
  | DefineFunRec (s, _, _, _) -> Some s
  | _ -> None
;;

(* Given a list of commands, filter out the commands that are not declarations.
   Then, in the declaration, remove duplicates.
*)
let remove_duplicate_decls (s : String.t Hash_set.t) (decls : command list) =
  let decls, nondecls =
    List.partition_tf
      ~f:(fun (s, _) -> Option.is_some s)
      (List.map ~f:(fun comm -> smtSymbol_of_decl comm, comm) decls)
  in
  let uniq_decls =
    List.fold_left
      ~f:(fun uniq_decls (opt_name, decl) ->
        match opt_name with
        | Some name ->
          if List.Assoc.mem uniq_decls ~equal:symb_equal name
             || Hash_set.mem s (str_of_symb name)
          then uniq_decls
          else (name, decl) :: uniq_decls
        | None -> uniq_decls)
      ~init:[]
      decls
  in
  List.map ~f:snd uniq_decls @ List.map ~f:snd nondecls
;;

(* Reading solver responses *)
type solver_response =
  | Error of string
  | Sat (** The solver responded [sat] *)
  | Unsat (** The solver responded [unsat] *)
  | Unknown (** The solver responded  [unknown] *)
  | Success (** The solver responded [success] (the [:print-success] option is on) *)
  | Unsupported (** The solver reponded that the input is not supported. *)
  | SExps of Sexp.t list [@sexp.list]
      (** Any other type of response is a list of S-Expressions, which can be interpreted later. *)
[@@deriving_sexp]

(* Parse solver reponses *)
let parse_response (r : Sexp.t list) : solver_response =
  match r with
  | [ Sexp.Atom "sat" ] -> Sat
  | [ Sexp.Atom "unsat" ] -> Unsat
  | [ Sexp.Atom "unknown" ] -> Unknown
  | [ Sexp.Atom "success" ] -> Success
  | [ Sexp.List [ Sexp.Atom "error"; msg ] ] -> Error (Sexp.to_string msg)
  | _ -> SExps r
;;

let pp_solver_response f r =
  match r with
  | Sat -> Fmt.pf f "sat"
  | Unsat -> Fmt.pf f "unsat"
  | Unknown -> Fmt.pf f "unknown"
  | Success -> Fmt.pf f "success"
  | Unsupported -> Fmt.pf f "unsupported"
  | SExps sl -> Fmt.(pf f "%a" (list ~sep:sp Sexp.pp) sl)
  | Error s -> Fmt.(pf f "(error %s)" s)
;;
