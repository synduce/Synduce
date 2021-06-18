open Base
open Sexp
open Option.Let_syntax
module OC = Stdio.Out_channel
module IC = Stdio.In_channel

type numeral = int

type smtSymbol = SSimple of string | SQuoted of string

let symb_equal s1 s2 =
  match (s1, s2) with
  | SSimple name1, SSimple name2 -> String.equal name1 name2
  | SQuoted name1, SQuoted name2 -> String.equal name1 name2
  | _, _ -> false

let str_of_symb s1 = match s1 with SSimple s -> s | SQuoted s -> "`" ^ s

type smtIndex = INum of numeral | ISym of smtSymbol

type smtIdentifier = Id of smtSymbol | IdC of smtSymbol * smtIndex list

type smtSort = SmtSort of smtIdentifier | Comp of smtIdentifier * smtSort list

type smtSortedVar = smtSymbol * smtSort

type smtQualIdentifier = QI of smtIdentifier | QIas of smtIdentifier * smtSort

type smtAttribute = ASymb of string | ASymbVal of string * string

type smtPattern = Pat of smtSymbol | PatComp of smtSymbol * smtSymbol list

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

type prop_literal = PL of smtSymbol | PLNot of smtSymbol

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

let sexps_of_option ((oname, ovalue) : solver_option) : t list = [ Atom (":" ^ oname); Atom ovalue ]

let option_of_sexp (sexp : t) : solver_option option =
  match sexp with
  | List [ Atom s; Atom v ] ->
      if String.is_prefix s ~prefix:":" then Some (String.drop_prefix s 1, v) else None
  | _ -> None

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

let sexp_of_numeral (n : numeral) : Sexp.t = Sexp.Atom Fmt.(str "%i" n)

let numeral_of_sexp (sexp : Sexp.t) : numeral option =
  match sexp with Atom s -> ( try Some (Int.of_string s) with _ -> None) | _ -> None

let sexp_of_smtSpecConstant (sc : smtSpecConstant) : t =
  match sc with
  | SCNumeral num -> sexp_of_numeral num
  | SCDecimal (n1, d, n2) ->
      Atom
        Fmt.(
          str "%s.%a%s" (Int.to_string n1)
            (list ~sep:(fun _ _ -> ()) (fun f' _ -> Fmt.(pf f' "0")))
            (List.init d ~f:(fun _ -> 0))
            (Int.to_string n2))
  | SCHexaDecimal s -> Atom Fmt.(str "#x%s" s)
  | SCBinary bl ->
      Atom
        Fmt.(
          str "#b%a" (list ~sep:(fun _ _ -> ()) (fun f b -> if b then pf f "1" else pf f "0")) bl)
  | SCString s -> Atom Fmt.(str "\"%s\"" s)

let smtSpecConstant_of_sexp (sexp : t) : smtSpecConstant option =
  match sexp with
  | Atom s -> (
      if String.is_prefix ~prefix:"#b" s then failwith "Binary unsupported for now!"
      else if String.is_prefix ~prefix:"#b" s then Some (SCHexaDecimal (String.drop_prefix s 2))
      else if String.is_prefix ~prefix:"\"" s then
        Some (SCString (String.drop_prefix (String.drop_prefix s 1) 1))
      else try Some (SCNumeral (Int.of_string s)) with _ -> None)
  | _ -> None

let sexp_of_smtSymbol (s : smtSymbol) : t =
  match s with SSimple x -> Atom x | SQuoted x -> Atom ("|" ^ x ^ "|")

let smtSymbol_of_sexp (s : t) : smtSymbol option =
  match s with
  | Atom x ->
      if String.is_prefix ~prefix:"|" x && String.is_suffix ~suffix:"|" x then
        Some (SQuoted (String.drop_prefix (String.drop_suffix x 1) 1))
      else Some (SSimple x)
  | _ -> None

let sexp_of_smtIndex (idx : smtIndex) : t =
  match idx with INum n -> Atom (Int.to_string n) | ISym s -> sexp_of_smtSymbol s

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

let sexp_of_smtIdentifier (id : smtIdentifier) : t =
  match id with
  | Id s -> sexp_of_smtSymbol s
  | IdC (s, ixl) ->
      let l = List (List.map ~f:sexp_of_smtIndex ixl) in
      List [ Atom "_"; sexp_of_smtSymbol s; l ]

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

let rec sexp_of_smtSort (s : smtSort) : t =
  match s with
  | SmtSort x -> sexp_of_smtIdentifier x
  | Comp (x, l) -> List (sexp_of_smtIdentifier x :: List.map ~f:sexp_of_smtSort l)

let rec smtSort_of_sexp (s : t) : smtSort option =
  match smtIdentifier_of_sexp s with
  | Some x -> Some (SmtSort x)
  | None -> (
      match s with
      | List (hd :: tl) ->
          let%bind id = smtIdentifier_of_sexp hd in
          let%bind args = Option.all (List.map ~f:smtSort_of_sexp tl) in
          Some (Comp (id, args))
      | _ -> None)

let sexp_of_smtAttribute (attr : smtAttribute) : t =
  match attr with
  | ASymb anam -> Atom anam
  | ASymbVal (aname, aval) -> List [ Atom aname; Atom aval ]

let smtAttribute_of_sexp (sexp : t) : smtAttribute option =
  match sexp with
  | Atom s -> Some (ASymb s)
  | List [ Atom name; Atom value ] -> Some (ASymbVal (name, value))
  | _ -> None

let sexp_of_sortedVar ((s, srt) : smtSortedVar) : t =
  List [ sexp_of_smtSymbol s; sexp_of_smtSort srt ]

let sortedVar_of_sexp (s : t) : smtSortedVar option =
  match s with
  | List [ symb; sort ] ->
      let%bind s1 = smtSymbol_of_sexp symb in
      let%bind s2 = smtSort_of_sexp sort in
      Some (s1, s2)
  | _ -> None

let sexp_of_smtQualIdentifier (qi : smtQualIdentifier) =
  match qi with
  | QI s -> sexp_of_smtIdentifier s
  | QIas (s, smtSort) -> List [ Atom "as"; sexp_of_smtIdentifier s; sexp_of_smtSort smtSort ]

let smtQualIdentifier_of_sexp (s : t) : smtQualIdentifier option =
  match s with
  | List [ Atom "as"; sid; ssort ] ->
      let%bind id = smtIdentifier_of_sexp sid in
      let%map sort = smtSort_of_sexp ssort in
      QIas (id, sort)
  | _ ->
      let%map id = smtIdentifier_of_sexp s in
      QI id

let sexp_of_smtPattern (p : smtPattern) : t =
  match p with
  | Pat s -> sexp_of_smtSymbol s
  | PatComp (s, sl) -> List (sexp_of_smtSymbol s :: List.map ~f:sexp_of_smtSymbol sl)

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

let rec sexp_of_smtTerm (t : smtTerm) : t =
  match t with
  | SmtTSpecConst sc -> sexp_of_smtSpecConstant sc
  | SmtTQualdId qi -> sexp_of_smtQualIdentifier qi
  | SmtTApp (func, args) ->
      List (sexp_of_smtQualIdentifier func :: List.map ~f:sexp_of_smtTerm args)
  | SmtTLet (bindings, t') ->
      List [ Atom "let"; List (List.map ~f:sexp_of_binding bindings); sexp_of_smtTerm t' ]
  | SmtTForall (quants, t') ->
      List [ Atom "forall"; List (List.map ~f:sexp_of_sortedVar quants); sexp_of_smtTerm t' ]
  | SmtTExists (quants, t') ->
      List [ Atom "exists"; List (List.map ~f:sexp_of_sortedVar quants); sexp_of_smtTerm t' ]
  | SmtTMatch (t', cases) ->
      List [ Atom "match"; sexp_of_smtTerm t'; List (List.map ~f:sexp_of_match_case cases) ]
  | SmtTAnnot (t', attrs) ->
      List (Atom "!" :: sexp_of_smtTerm t' :: List.map ~f:sexp_of_smtAttribute attrs)

and sexp_of_binding ((v, e) : var_binding) : t = List [ sexp_of_smtSymbol v; sexp_of_smtTerm e ]

and sexp_of_match_case ((p, t) : match_case) = List [ sexp_of_smtPattern p; sexp_of_smtTerm t ]

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
  | _ -> (
      match smtSpecConstant_of_sexp s with
      | Some cs -> Some (SmtTSpecConst cs)
      | None -> (
          match smtQualIdentifier_of_sexp s with
          | Some qi -> Some (SmtTQualdId qi)
          | None -> (
              match s with
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
      (pat', t)
  | _ -> None

and binding_of_sexp (s : t) : var_binding option =
  match s with
  | List [ x; v ] ->
      let%bind x' = smtSymbol_of_sexp x in
      let%map v' = smtTerm_of_sexp v in
      (x', v')
  | _ -> None

let sexp_of_smtSelectorDec ((s, srt) : smtSelectorDec) : t =
  List [ sexp_of_smtSymbol s; sexp_of_smtSort srt ]

let sexp_of_smtConstructorDec ((s, sdecs) : smtConstructorDec) : t =
  List (sexp_of_smtSymbol s :: List.map ~f:sexp_of_smtSelectorDec sdecs)

let sexp_of_datatype_dec (dtdec : datatype_dec) : t =
  match dtdec with
  | DDConstr cd_list -> List (List.map ~f:sexp_of_smtConstructorDec cd_list)
  | DDParametric (symbs, cd_list) ->
      List
        [
          Atom "par";
          List (List.map ~f:sexp_of_smtSymbol symbs);
          List (List.map ~f:sexp_of_smtConstructorDec cd_list);
        ]

let sexp_of_func_dec ((s, svs, srt) : func_dec) : t =
  List [ sexp_of_smtSymbol s; List (List.map ~f:sexp_of_sortedVar svs); sexp_of_smtSort srt ]

let sexp_of_func_def ((s, svs, srt, t) : func_def) : t list =
  [
    sexp_of_smtSymbol s;
    List (List.map ~f:sexp_of_sortedVar svs);
    sexp_of_smtSort srt;
    sexp_of_smtTerm t;
  ]

let sexp_of_prop_literal (pl : prop_literal) : t =
  match pl with PL x -> sexp_of_smtSymbol x | PLNot x -> List [ Atom "not"; sexp_of_smtSymbol x ]

let sexp_of_smtSortDec ((s, n) : smtSortDec) : t = List [ sexp_of_smtSymbol s; sexp_of_numeral n ]

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
        [
          Atom "declare-datatypes";
          List (List.map ~f:sexp_of_smtSortDec sdec_l);
          List (List.map ~f:sexp_of_datatype_dec ddec_l);
        ]
  | DeclareFun (name, args, res) ->
      List
        [
          Atom "declare-fun";
          sexp_of_smtSymbol name;
          List (List.map ~f:sexp_of_smtSort args);
          sexp_of_smtSort res;
        ]
  | DeclareSmtSort (s, n) -> List [ Atom "declare-smtSort"; sexp_of_smtSymbol s; sexp_of_numeral n ]
  | DefineFun fd -> List (Atom "define-fun" :: sexp_of_func_def fd)
  | DefineFunRec fd -> List (Atom "define-fun-rec" :: sexp_of_func_def fd)
  | DefineFunsRec (fdl, tl) ->
      List
        [
          Atom "define-funs-rec";
          List (List.map ~f:sexp_of_func_dec fdl);
          List (List.map ~f:sexp_of_smtTerm tl);
        ]
  | DefineSmtSort (s, sl, smtSort) ->
      List
        [
          Atom "define-sort";
          sexp_of_smtSymbol s;
          List (List.map ~f:sexp_of_smtSymbol sl);
          sexp_of_smtSort smtSort;
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
      if List.is_empty ol then List [ Atom "simplify"; sexp_of_smtTerm t ]
      else
        List (Atom "simplify" :: sexp_of_smtTerm t :: List.concat (List.map ~f:sexps_of_option ol))

let write_command (out : OC.t) (c : command) : unit =
  let comm_s = Sexp.to_string_hum (sexp_of_command c) in
  OC.output_lines out [ comm_s ];
  OC.flush out

let pp_script (f : Formatter.t) (s : script) =
  List.iter ~f:(fun cm -> Fmt.pf f "%a@." Sexp.pp (sexp_of_command cm)) s

(* Term construction *)
(* SmtSymbol *)
let mk_symb s = SSimple s

(* Sorts *)
let mk_int_sort = SmtSort (Id (SSimple "Int"))

let mk_bool_sort = SmtSort (Id (SSimple "Bool"))

let mk_seq_sort t = Comp (Id (SSimple "Seq"), [ t ])

let mk_tuple_sort lt = Comp (Id (SSimple "Tuple"), lt)

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

let mk_assoc_and tl = mk_simple_app "and" tl

let mk_or t1 t2 = mk_simple_app "or" [ t1; t2 ]

let mk_eq t1 t2 = mk_simple_app "=" [ t1; t2 ]

let mk_lt t1 t2 = mk_simple_app "<" [ t1; t2 ]

let mk_gt t1 t2 = mk_simple_app ">" [ t1; t2 ]

let mk_le t1 t2 = mk_simple_app "<=" [ t1; t2 ]

let mk_ge t1 t2 = mk_simple_app ">=" [ t1; t2 ]

let mk_not t1 = mk_simple_app "not" [ t1 ]

let mk_exists (sorted_vars : smtSortedVar list) (term : smtTerm) = SmtTExists (sorted_vars, term)

let mk_forall (sorted_vars : smtSortedVar list) (term : smtTerm) = SmtTForall (sorted_vars, term)

(* Commands *)
let mk_assert (t : smtTerm) = Assert t

let mk_const_decl (s : string) (t : smtSort) = DeclareConst (SSimple s, t)

let mk_check_sat = CheckSat

let mk_fun_decl (s : string) (args : smtSort list) (res : smtSort) =
  DeclareFun (SSimple s, args, res)

let mk_fun_def (s : string) (args : (string * smtSort) list) (res_sort : smtSort) (body : smtTerm) =
  DefineFun (mk_symb s, List.map ~f:(fun (s, t) -> (mk_symb s, t)) args, res_sort, body)

let mk_named_assert (name : string) (t : smtTerm) =
  Assert (SmtTAnnot (t, [ ASymbVal (":named", name) ]))

let mk_set_option (oname : string) (oval : string) = SetOption (oname, oval)

let mk_simplify ?(options : solver_option list = []) (t : smtTerm) = Simplify (t, options)

let mk_print_success = SetOption ("print-success", "true")

let mk_exit = Exit

let mk_pop i = Pop i

let mk_push i = Push i

(* Specific function declarations *)
let mk_min_def =
  mk_fun_def "min"
    [ ("x", mk_int_sort); ("y", mk_int_sort) ]
    mk_int_sort
    (mk_ite (mk_le (mk_var "x") (mk_var "y")) (mk_var "x") (mk_var "y"))

let mk_max_def =
  mk_fun_def "max"
    [ ("x", mk_int_sort); ("y", mk_int_sort) ]
    mk_int_sort
    (mk_ite (mk_ge (mk_var "x") (mk_var "y")) (mk_var "x") (mk_var "y"))

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

(* Given a list of commands, filter out the commands that are not declarations.
   Then, in the declaration, remove duplicates.
*)
let remove_duplicate_decls (s : String.t Hash_set.t) (decls : command list) =
  let decls, nondecls =
    List.partition_tf
      ~f:(fun (s, _) -> Option.is_some s)
      (List.map ~f:(fun comm -> (smtSymbol_of_decl comm, comm)) decls)
  in
  let uniq_decls =
    List.fold_left
      ~f:(fun uniq_decls (opt_name, decl) ->
        match opt_name with
        | Some name ->
            if List.Assoc.mem uniq_decls ~equal:symb_equal name || Hash_set.mem s (str_of_symb name)
            then uniq_decls
            else (name, decl) :: uniq_decls
        | None -> uniq_decls)
      ~init:[] decls
  in
  List.map ~f:snd uniq_decls @ List.map ~f:snd nondecls

(* Reading solver responses *)
type solver_response =
  | Error of string
  | Sat
  | Unsat
  | Unknown
  | Success
  | SExps of Sexp.t list [@sexp.list]
[@@deriving_sexp]

(* Parse solver reponses *)
let parse_response (r : Sexp.t list) : solver_response =
  match r with
  | [ Sexp.Atom "sat" ] -> Sat
  | [ Sexp.Atom "unsat" ] -> Unsat
  | [ Sexp.Atom "unknown" ] -> Unknown
  | [ Sexp.Atom "success" ] -> Success
  | _ -> SExps r

let pp_solver_response f r =
  match r with
  | Sat -> Fmt.pf f "sat"
  | Unsat -> Fmt.pf f "unsat"
  | Unknown -> Fmt.pf f "unknown"
  | Success -> Fmt.pf f "success"
  | SExps sl -> Fmt.(pf f "%a" (list ~sep:sp Sexp.pp) sl)
  | Error s -> Fmt.(pf f "(error %s)" s)
