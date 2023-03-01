open Base
open Sygus

module Annot = struct
  open Parsexp

  type kind =
    | AAtom of string
    | AList of t list

  and t =
    { loc : Positions.range
    ; sexp : kind
    }

  let mk_atom loc s = { loc; sexp = AAtom s }
  let mk_list loc sexps = { loc; sexp = AList sexps }

  let of_sexp_and_positions : Sexp.t -> position list -> t * position list =
    let rec loop (sexp : Sexp.t) (positions : Positions.pos list) =
      match sexp, positions with
      | Atom s, start_pos :: last_pos :: rest ->
        mk_atom (Positions.make_range_incl ~start_pos ~last_pos) s, rest
      | List l, start_pos :: rest ->
        let annots_rev, rest =
          List.fold_left l ~init:([], rest) ~f:(fun (acc, positions) sexp ->
              let annot, rest = loop sexp positions in
              annot :: acc, rest)
        in
        (match rest with
        | [] -> assert false
        | last_pos :: rest ->
          ( mk_list (Positions.make_range_incl ~start_pos ~last_pos) (List.rev annots_rev)
          , rest ))
      | _ -> assert false
    in
    fun sexp positions ->
      let t, rest = loop sexp positions in
      t, rest
  ;;

  let of_sexp (s : Sexp.t) : t =
    let rec f s =
      match s with
      | Sexp.Atom s -> mk_atom dummy_loc s
      | Sexp.List l -> mk_list dummy_loc (List.map ~f l)
    in
    f s
  ;;

  let to_sexp (annotated : t) : Sexp.t =
    let rec loop (annot : t) =
      match annot.sexp with
      | AAtom s -> Sexp.Atom s
      | AList l -> Sexp.List (List.map ~f:loop l)
    in
    loop annotated
  ;;

  let rec iter t ~f =
    match t.sexp with
    | AAtom s -> f t.loc Sexp.(Atom s)
    | AList subs ->
      f t.loc (to_sexp t);
      List.iter subs ~f:(iter ~f)
  ;;
end

exception ParseError of location * Sexp.t * string
exception NonConforming of command * string

let raise_parse_error (s : Annot.t) (msg : string) =
  raise (ParseError (s.loc, Annot.to_sexp s, msg))
;;

let raise_nonconforming (s : command) (msg : string) = raise (NonConforming (s, msg))

let symbol_of_asexp (s : Annot.t) : symbol =
  match s.sexp with
  | AAtom symb -> symb
  | _ -> raise_parse_error s "Not a symbol"
;;

let keyword_of_asexp (s : Annot.t) : string option =
  match s.sexp with
  | AAtom kw -> String.chop_prefix ~prefix:":" kw
  | _ -> None
;;

let feature_of_asexp (s : Annot.t) : (feature, location * string) Result.t =
  match keyword_of_asexp s with
  | Some "grammar" -> Ok FGrammar
  | Some "fwd-decls" -> Ok FFwdDecls
  | Some "recursion" -> Ok FRecursion
  | Some "oracles" -> Ok FOracles
  | Some "weights" -> Ok FWeights
  | _ -> raise_parse_error s "Not a valid feature"
;;

let attributes_of_sexps (sl : Annot.t list) : (attribute list, location * string) Result.t
  =
  let f (l, p) atom =
    let loc = atom.Annot.loc in
    match atom.Annot.sexp with
    | AAtom x when String.length x > 0 ->
      (match keyword_of_asexp atom with
      | Some attr_name ->
        (match p with
        | Some (_, key) ->
          Continue_or_stop.Continue (mk_attr ~loc key :: l, Some (atom.loc, attr_name))
        | None -> Continue (l, Some (atom.loc, attr_name)))
      | None ->
        (match p with
        | Some (_, key) -> Continue (mk_attr_val ~loc key x :: l, None)
        | None ->
          Stop
            (Error
               ( loc
               , Fmt.(str "(%a) A lone value in a list of attributes." pp_loc atom.loc) ))))
    | _ ->
      Stop (Error (loc, Fmt.(str "(%a) Attributes can only use atoms." pp_loc atom.loc)))
  in
  List.fold_until ~init:([], None) ~f sl ~finish:(function
      | l, Some (loc, v) -> Ok (List.rev (mk_attr ~loc v :: l))
      | l, None -> Ok (List.rev l))
;;

let index_of_asexp (s : Annot.t) : index =
  match s.sexp with
  | AAtom a ->
    (match Caml.int_of_string_opt a with
    | Some i -> mk_index_num ~loc:s.loc i
    | None -> mk_index_sym ~loc:s.loc a)
  | _ -> raise_parse_error s "Not an index"
;;

let rec sygus_sort_of_asexp (s : Annot.t) : sygus_sort =
  try mk_sort ~loc:s.loc (identifier_of_asexp s) with
  | _ ->
    (match s.sexp with
    | AList (id :: s1 :: sygus_sorts) ->
      mk_sort_app
        ~loc:s.loc
        (identifier_of_asexp id)
        (List.map ~f:sygus_sort_of_asexp (s1 :: sygus_sorts))
    | _ -> raise_parse_error s "Not a sygus_sort")

and identifier_of_asexp (s : Annot.t) : identifier =
  match s.sexp with
  | AAtom name ->
    if valid_ident name
    then mk_id_simple ~loc:s.loc name
    else raise_parse_error s (Fmt.str "%s is not an identifier." name)
  | AList ({ sexp = AAtom "_"; _ } :: main_s :: i0 :: indexes) ->
    mk_id_indexed
      ~loc:s.loc
      (symbol_of_asexp main_s)
      (List.map ~f:index_of_asexp (i0 :: indexes))
  | AList [ { sexp = AAtom "as"; _ }; main_s; sort_s ] ->
    mk_id_qual ~loc:s.loc (symbol_of_asexp main_s) (sygus_sort_of_asexp sort_s)
  | _ -> raise_parse_error s "Not an identifier."
;;

let sorted_var_of_asexp (s : Annot.t) : sorted_var =
  match s.sexp with
  | AList [ symb; sygus_sort ] ->
    s.loc, symbol_of_asexp symb, sygus_sort_of_asexp sygus_sort
  | _ -> raise_parse_error s "Not a sygus_sorted var."
;;

let literal_of_string ?(loc = dummy_loc) (s : string) : literal =
  if String.is_empty s
  then mk_lit_string ~loc s
  else if String.is_prefix ~prefix:"\"" s
  then mk_lit_string ~loc s
  else if String.is_prefix ~prefix:"#x" s
  then mk_lit_hex ~loc (String.chop_prefix_exn ~prefix:"#x" s)
  else if String.is_prefix ~prefix:"#b" s
  then (
    let b = String.chop_prefix_exn ~prefix:"#b" s in
    mk_lit_bin ~loc (List.map ~f:char_to_bool (String.to_list b)))
  else (
    match s with
    | "true" -> mk_lit_bool ~loc true
    | "false" -> mk_lit_bool ~loc false
    | _ ->
      (match Caml.int_of_string_opt s with
      | Some i -> mk_lit_num ~loc i
      | None ->
        (match Caml.float_of_string_opt s with
        | Some f -> mk_lit_dec ~loc f
        | None -> raise_parse_error (Annot.mk_atom loc s) "Not a literal.")))
;;

let literal_of_asexp (s : Annot.t) : literal =
  match s.sexp with
  | AAtom atom -> literal_of_string ~loc:s.loc atom
  | _ -> raise_parse_error s "Not a literal."
;;

let rec sygus_term_of_asexp (s : Annot.t) : sygus_term =
  match s.sexp with
  | AList [ { sexp = AAtom "exists"; _ }; { sexp = AList _vars; _ }; sygus_term ] ->
    mk_t_exists
      ~loc:s.loc
      (List.map ~f:sorted_var_of_asexp _vars)
      (sygus_term_of_asexp sygus_term)
  | AList [ { sexp = AAtom "forall"; _ }; { sexp = AList _vars; _ }; sygus_term ] ->
    mk_t_forall
      ~loc:s.loc
      (List.map ~f:sorted_var_of_asexp _vars)
      (sygus_term_of_asexp sygus_term)
  | AList [ { sexp = AAtom "let"; _ }; { sexp = AList bindings; _ }; sygus_term ] ->
    mk_t_let
      ~loc:s.loc
      (List.map ~f:binding_of_asexp bindings)
      (sygus_term_of_asexp sygus_term)
  | AList ({ sexp = AAtom "as"; _ } :: _) -> mk_t_id ~loc:s.loc (identifier_of_asexp s)
  | AList (hd :: tl) ->
    mk_t_app ~loc:s.loc (identifier_of_asexp hd) (List.map ~f:sygus_term_of_asexp tl)
  | _ ->
    (try mk_t_lit ~loc:s.loc (literal_of_asexp s) with
    | _ ->
      (try mk_t_id ~loc:s.loc (identifier_of_asexp s) with
      | ParseError (_, _, msg) -> raise_parse_error s (msg ^ "(while parsing a term)")))

and binding_of_asexp (s : Annot.t) : binding =
  match s.sexp with
  | AList [ symb; sygus_term ] ->
    s.loc, symbol_of_asexp symb, sygus_term_of_asexp sygus_term
  | _ -> raise_parse_error s "not a binding"
;;

let sygus_sort_decl_of_asexp (s : Annot.t) : sygus_sort_decl =
  match s.sexp with
  | AList [ symb; { sexp = AAtom num; _ } ] -> symbol_of_asexp symb, Int.of_string num
  | _ -> raise_parse_error s "Not a sygus_sort declaration."
;;

let dt_cons_dec_of_asexp (s : Annot.t) : dt_cons_dec =
  match s.sexp with
  | AList (symb :: args) -> symbol_of_asexp symb, List.map ~f:sorted_var_of_asexp args
  | _ -> raise_parse_error s "Not a data constructor declaration."
;;

let dt_cons_dec_list_of_asexp (s : Annot.t) : dt_cons_dec list =
  match s.sexp with
  | AList (d1 :: drest) -> List.map ~f:dt_cons_dec_of_asexp (d1 :: drest)
  | _ -> raise_parse_error s "Not a list+ of data constructor declarations."
;;

let sygus_sort_decl_list_of_asexp (s : Annot.t) : sygus_sort_decl list =
  match s.sexp with
  | AList (sd1 :: sdrest) -> List.map ~f:sygus_sort_decl_of_asexp (sd1 :: sdrest)
  | _ -> raise_parse_error s "Not a list+ of sygus_sort declarations."
;;

let sygus_gsterm_of_asexp (s : Annot.t) : sygus_gsterm =
  match s.sexp with
  | AList [ { sexp = AAtom "Constant"; _ }; sygus_sort ] ->
    mk_g_constant ~loc:s.loc (sygus_sort_of_asexp sygus_sort)
  | AList [ { sexp = AAtom "Variable"; _ }; sygus_sort ] ->
    mk_g_var ~loc:s.loc (sygus_sort_of_asexp sygus_sort)
  | _ ->
    (try mk_g_term ~loc:s.loc (sygus_term_of_asexp s) with
    | ParseError (loc, _, msg) ->
      let new_msg = Fmt.str "at %a, %s" pp_loc loc msg in
      raise_parse_error s (new_msg ^ "(while parsing a sygus grammar term)"))
;;

let pre_grouped_rule_of_asexp (s : Annot.t) =
  match s.sexp with
  | AList [ name; sygus_sort; { sexp = AList gramsygus_terms; _ } ] ->
    ( s.loc
    , symbol_of_asexp name
    , sygus_sort_of_asexp sygus_sort
    , List.map ~f:sygus_gsterm_of_asexp gramsygus_terms )
  | _ -> raise_parse_error s "Not a grouped rule."
;;

let grammar_def_of_asexps (sv : Annot.t option) (gr : Annot.t) : grammar_def =
  match sv, gr.sexp with
  | Some { sexp = AList sygus_sorts; loc = subloc }, AList grouped_rules ->
    (match
       List.zip
         (List.map ~f:sorted_var_of_asexp sygus_sorts)
         (List.map ~f:pre_grouped_rule_of_asexp grouped_rules)
     with
    | Ok l -> List.map ~f:(fun (s, (_, _, _, g)) -> s, g) l
    | _ ->
      raise_parse_error
        (Annot.mk_list gr.loc [ Annot.mk_list subloc sygus_sorts; gr ])
        "Number of non-terminal symbols and grammar rules do not match.")
  | None, AList grouped_rules ->
    let l = List.map ~f:pre_grouped_rule_of_asexp grouped_rules in
    List.map ~f:(fun (loc, s, t, g) -> (loc, s, t), g) l
  | _ -> raise_parse_error gr "Not a grammar definition."
;;

let command_of_asexp (s : Annot.t) : command =
  let command_of_elts (loc : location) (cn : string) (sl : Annot.t list)
      : (command, location * string) Result.t
    =
    match cn with
    | "assume" ->
      (match sl with
      | [ a ] -> Ok (mk_c_assume ~loc (sygus_term_of_asexp a))
      | _ -> Error (loc, "assume accepts exactly one argument."))
    | "check-synth" ->
      (match sl with
      | [] -> Ok (mk_c_check_synth ~loc ())
      | _ -> Error (loc, "check-synth should not have any arguments."))
    | "chc-constraint" ->
      (match sl with
      | [ { sexp = AList svars; _ }; t1; t2 ] ->
        Ok
          (mk_c_chc_constraint
             ~loc
             (List.map ~f:sorted_var_of_asexp svars)
             (sygus_term_of_asexp t1)
             (sygus_term_of_asexp t2))
      | _ -> Error (loc, "wrong number of arguments for chc-constraint."))
    | "constraint" ->
      (match sl with
      | [ a ] -> Ok (mk_c_constraint ~loc (sygus_term_of_asexp a))
      | _ -> Error (loc, "constraint should have only one term as input."))
    | "declare-var" ->
      (match sl with
      | [ vname; sort ] ->
        Ok (mk_c_declare_var ~loc (symbol_of_asexp vname) (sygus_sort_of_asexp sort))
      | _ ->
        Error (loc, "declare-var should have only one symbol and one sort as arguments."))
    | "declare-weight" ->
      (match sl with
      | symb :: attributes ->
        Result.(
          attributes_of_sexps attributes
          >>| fun attrs -> mk_c_declare_weight ~loc (symbol_of_asexp symb) attrs)
      | _ -> Error (loc, "declare-weight should have at least one argument."))
    | "inv-constraint" ->
      (match sl with
      | [ { sexp = AAtom s; _ }
        ; { sexp = AAtom spre; _ }
        ; { sexp = AAtom strans; _ }
        ; { sexp = AAtom spost; _ }
        ] -> Ok (mk_c_inv_constraint ~loc s spre strans spost)
      | _ -> Error (loc, "inv-constraint takes exactly 4 symbols as arguments."))
    | "optimize-synth" ->
      (match sl with
      | { sexp = AList terms; _ } :: attributes ->
        Result.(
          attributes_of_sexps attributes
          >>| fun attrs ->
          mk_c_optimize_synth ~loc (List.map ~f:sygus_term_of_asexp terms) attrs)
      | _ ->
        Error
          ( loc
          , "optimize-synth takes a list of terms and a list of attributes as arguments."
          ))
    | "set-feature" ->
      (match sl with
      | [ ({ sexp = AAtom _; _ } as feature); { sexp = AAtom setting; _ } ] ->
        Result.(
          feature_of_asexp feature
          >>| fun fture -> mk_c_set_feature ~loc fture (bool_of_sexp (Atom setting)))
      | _ -> Error (loc, "set-feature takes a feature and a boolean as arguments."))
    | "synth-fun" ->
      (match sl with
      | [ name; { sexp = AList args; _ }; ret_sort; gd1; gd2 ] ->
        Ok
          (mk_c_synth_fun
             ~loc
             ~g:(Some (grammar_def_of_asexps (Some gd1) gd2))
             (symbol_of_asexp name)
             (List.map ~f:sorted_var_of_asexp args)
             (sygus_sort_of_asexp ret_sort))
      | [ name; { sexp = AList args; _ }; ret_sort ] ->
        Ok
          (mk_c_synth_fun
             ~loc
             (symbol_of_asexp name)
             (List.map ~f:sorted_var_of_asexp args)
             (sygus_sort_of_asexp ret_sort))
      | [ name; { sexp = AList args; _ }; ret_sort; gd2 ] ->
        let v1 =
          mk_c_synth_fun
            ~loc
            ~g:(Some (grammar_def_of_asexps None gd2))
            (symbol_of_asexp name)
            (List.map ~f:sorted_var_of_asexp args)
            (sygus_sort_of_asexp ret_sort)
        in
        raise_nonconforming
          v1
          "This synth-fun should have non-terminal declarations before the grammar."
      | _ ->
        Error
          ( loc
          , "synth-fun takes as arguments a function name, variables with sorts a return \
             sort and an optional grammar" ))
    | "synth-inv" ->
      (match sl with
      | [ name; { sexp = AList args; _ }; gd1; gd2 ] ->
        Ok
          (mk_c_synth_inv
             ~loc
             ~g:(Some (grammar_def_of_asexps (Some gd1) gd2))
             (symbol_of_asexp name)
             (List.map ~f:sorted_var_of_asexp args))
      | [ name; { sexp = AList args; _ } ] ->
        Ok
          (mk_c_synth_inv
             ~loc
             (symbol_of_asexp name)
             (List.map ~f:sorted_var_of_asexp args))
      | [ name; { sexp = AList args; _ }; gd2 ] ->
        let v1 =
          mk_c_synth_inv
            ~loc
            ~g:(Some (grammar_def_of_asexps None gd2))
            (symbol_of_asexp name)
            (List.map ~f:sorted_var_of_asexp args)
        in
        raise_nonconforming
          v1
          "This synth-fun should have non-terminal declarations before the grammar."
      | _ ->
        Error
          ( loc
          , "synth-fun takes as arguments a function name, variables with sorts a return \
             sort and an optional grammar" ))
    | "declare-datatype" ->
      (match sl with
      | [ a; b ] ->
        Ok (mk_c_declare_datatype ~loc (symbol_of_asexp a) (dt_cons_dec_list_of_asexp b))
      | _ ->
        Error
          (loc, "declare-datatypes takes a symbol and a list of constructors as arguments"))
    | "declare-datatypes" ->
      (match sl with
      | [ a; { sexp = AList b; _ } ] ->
        Ok
          (mk_c_declare_datatypes
             ~loc
             (sygus_sort_decl_list_of_asexp a)
             (List.map ~f:dt_cons_dec_list_of_asexp b))
      | _ ->
        Error
          (loc, "declare-datatypes takes a symbol and a list of constructors as arguments"))
    | "declare-sort" ->
      (match sl with
      | [ s; n ] ->
        Ok (mk_c_declare_sort ~loc (symbol_of_asexp s) (Int.t_of_sexp (Annot.to_sexp n)))
      | _ -> Error (loc, "A sort declaration with declare-sort takes a name and an index."))
    | "define-fun" ->
      (match sl with
      | [ name; { sexp = AList args; _ }; ret_sort; body ] ->
        Ok
          (mk_c_define_fun
             ~loc
             (symbol_of_asexp name)
             (List.map ~f:sorted_var_of_asexp args)
             (sygus_sort_of_asexp ret_sort)
             (sygus_term_of_asexp body))
      | _ -> Error (loc, ""))
    | "define-sort" ->
      (match sl with
      | [ name; sort ] ->
        Ok (mk_c_define_sort ~loc (symbol_of_asexp name) (sygus_sort_of_asexp sort))
      | _ -> Error (loc, "define-sort takes a name and a sort as arguments."))
    | "set-info" ->
      (match sl with
      | [ kw; lit ] ->
        Result.of_option
          ~error:(loc, "Bad keyword")
          Option.(
            keyword_of_asexp kw
            >>| fun keyw -> mk_c_set_info ~loc keyw (literal_of_asexp lit))
      | _ -> Error (loc, "set-info takes a keyword and a literal as arguments."))
    | "set-logic" ->
      (match sl with
      | [ lname ] -> Ok (mk_c_set_logic ~loc (symbol_of_asexp lname))
      | _ -> Error (loc, "set-logic only takes a logic name as argument."))
    | "set-option" ->
      (match sl with
      | [ kw; lit ] ->
        Result.of_option
          ~error:(loc, "Bad keyword")
          Option.(
            keyword_of_asexp kw
            >>| fun keyw -> mk_c_set_option keyw (literal_of_asexp lit))
      | _ -> Error (loc, "set-option takes a keyword and a literal as arguments."))
    | "oracle-assume" ->
      (match sl with
      | [ { sexp = AList q1; _ }; { sexp = AList q2; _ }; body; oname ] ->
        Ok
          (mk_c_oracle
             (OAssume
                ( List.map ~f:sorted_var_of_asexp q1
                , List.map ~f:sorted_var_of_asexp q2
                , sygus_term_of_asexp body
                , symbol_of_asexp oname )))
      | _ ->
        Error
          (loc, "Oracle command oracle-assume doesn't have the right number of arguments."))
    | "oracle-constraint" ->
      (match sl with
      | [ { sexp = AList q1; _ }; { sexp = AList q2; _ }; body; oname ] ->
        Ok
          (mk_c_oracle
             ~loc
             (OConstraint
                ( List.map ~f:sorted_var_of_asexp q1
                , List.map ~f:sorted_var_of_asexp q2
                , sygus_term_of_asexp body
                , symbol_of_asexp oname )))
      | _ ->
        Error
          ( loc
          , "Oracle command oracle-constraint doesn't have the right number of arguments."
          ))
    | "declare-oracle-fun" ->
      (match sl with
      | [ name; { sexp = AList args; _ }; ret_sort; oname ] ->
        Ok
          (mk_c_oracle
             ~loc
             (ODeclareFun
                ( symbol_of_asexp name
                , List.map ~f:sygus_sort_of_asexp args
                , sygus_sort_of_asexp ret_sort
                , symbol_of_asexp oname )))
      | _ ->
        Error
          ( loc
          , "Oracle command declare-oracle-fun doesn't have the right number of \
             arguments." ))
    | "oracle-constraint-io" ->
      (match sl with
      | [ s1; s2 ] ->
        Ok (mk_c_oracle ~loc (OConstraintIO (symbol_of_asexp s1, symbol_of_asexp s2)))
      | _ ->
        Error
          ( loc
          , "Oracle command oracle-constraint-io accepts exactly two symbol arguments." ))
    | "oracle-constraint-cex" ->
      (match sl with
      | [ s1; s2 ] ->
        Ok (mk_c_oracle ~loc (OConstraintCex (symbol_of_asexp s1, symbol_of_asexp s2)))
      | _ ->
        Error
          ( loc
          , "Oracle command oracle-constraint-cex accepts exactly two symbol arguments."
          ))
    | "oracle-constraint-membership" ->
      (match sl with
      | [ s1; s2 ] ->
        Ok (mk_c_oracle ~loc (OConstraintMem (symbol_of_asexp s1, symbol_of_asexp s2)))
      | _ ->
        Error
          ( loc
          , "Oracle command oracle-constraint-membership accepts exactly two symbol \
             arguments." ))
    | "oracle-constraint-poswitness" ->
      (match sl with
      | [ s1; s2 ] ->
        Ok (mk_c_oracle ~loc (OConstraintPosw (symbol_of_asexp s1, symbol_of_asexp s2)))
      | _ ->
        Error
          ( loc
          , "Oracle command oracle-constraint-poswitness accepts exactly two symbol \
             arguments." ))
    | "oracle-constraint-negwitness" ->
      (match sl with
      | [ s1; s2 ] ->
        Ok (mk_c_oracle ~loc (OConstraintNegw (symbol_of_asexp s1, symbol_of_asexp s2)))
      | _ ->
        Error
          ( loc
          , "Oracle command oracle-constraint-negwitness accepts exactly two symbol \
             arguments." ))
    | "declare-correctness-oracle" ->
      (match sl with
      | [ s1; s2 ] ->
        Ok (mk_c_oracle ~loc (OCorrectness (symbol_of_asexp s1, symbol_of_asexp s2)))
      | _ ->
        Error
          ( loc
          , "Oracle command oracle-correctness-oracle accepts exactly two symbol \
             arguments." ))
    | "declare-correctness-cex-oracle" ->
      (match sl with
      | [ s1; s2 ] ->
        Ok (mk_c_oracle ~loc (OCorrectnessCex (symbol_of_asexp s1, symbol_of_asexp s2)))
      | _ ->
        Error
          ( loc
          , "Oracle command oracle-correctness-cex-oracle accepts exactly two symbol \
             arguments." ))
    | _ ->
      Error (loc, Fmt.(str "I do not know this command: %a" Sexp.pp_hum (Annot.to_sexp s)))
  in
  match s.sexp with
  | AList ({ sexp = AAtom command_name; _ } :: elts) ->
    (match command_of_elts s.loc command_name elts with
    | Ok c -> c
    | Error (loc, msg) ->
      raise_parse_error
        s
        Fmt.(str "%s@;@[(at location %a while parsing a command)@}" msg pp_loc loc))
  | _ -> raise_parse_error s "A command should start with a name."
;;

let program_of_asexp_list (sexps : Annot.t list) : program =
  List.map ~f:command_of_asexp sexps
;;

let response_of_asexps (s : Annot.t list) : solver_response =
  let atomic_response (s : Annot.t list) =
    match s with
    | [ { sexp = AAtom "fail"; _ } ] -> Some RFail
    | [ { sexp = AAtom "infeasible"; _ } ] -> Some RInfeasible
    | [ { sexp = AAtom "unknown"; _ } ] -> Some RUnknown
    | [ { sexp = AAtom "sat"; _ } ] -> Some RInfeasible
    | _ -> None
  in
  let one_command cmd =
    try
      match command_of_asexp cmd with
      | CDefineFun (_, f, args, res, body) -> Some [ f, args, res, body ]
      | _ -> None
    with
    | Failure _ -> None
  in
  let success_response s =
    RSuccess
      (List.concat
         (List.filter_map s ~f:(function
             | Annot.{ sexp = AAtom "unsat"; _ } ->
               None (* Ignore 'unsat' printed by CVC4. *)
             | { sexp = AList l; _ } as cmd ->
               (match
                  try one_command cmd with
                  | _ -> None
                with
               (* A response composed of a single command. *)
               | Some s -> Some s
               | None ->
                 (match Option.all (List.map ~f:one_command l) with
                 | Some defs -> Some (List.concat defs)
                 | None -> None))
             | { sexp = AAtom _; _ } -> None)))
  in
  match atomic_response s with
  | Some r -> r
  | None -> success_response s
;;

let sexp_parse (filename : string) : program =
  match Parsexp_io.load (module Parsexp.Many_and_positions) ~filename with
  | Ok (sexp_list, positions) ->
    let positions = Parsexp.Positions.to_list positions in
    let annotated_sexps, _ =
      List.fold
        ~init:([], positions)
        ~f:(fun (t, pos) sexp ->
          let t', pos' = Annot.of_sexp_and_positions sexp pos in
          t' :: t, pos')
        sexp_list
    in
    program_of_asexp_list (List.rev annotated_sexps)
  | Error parse_error ->
    Parsexp.Parse_error.report Fmt.stderr ~filename parse_error;
    failwith "Failed to parse file."
;;
