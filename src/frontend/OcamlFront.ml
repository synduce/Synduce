open Base
open OcamlAstHelpers
open Parsetree
open Utils
open Front
open Lang.RType

let psi_comps : (string * string * string) option ref = ref None

(* ============================================================================================= *)
(*                 IN-FILE OPTION PARSING                                                        *)
(* ============================================================================================= *)

let option_parse (vals : string list) =
  let placeholder = ref false in
  Log.debug
    Fmt.(
      fun fmt () -> pf fmt "@[Option given in file:@;%a@]" (list ~sep:comma string) vals);
  Getopt.parse
    (Config.options (fun _ -> ()) placeholder)
    (fun _ -> ())
    (Array.of_list vals)
    0
    (List.length vals - 1)
;;

(** [parse_option option] parses a string that defines a setting in Synduce. *)
let parse_option (setting : attribute) =
  let analyze_parts value =
    let value = Str.(global_replace (regexp "\"") "" value) in
    let parts = Str.(split (regexp "[ ]+")) value in
    match parts with
    | option_keyword :: option_args ->
      if String.equal option_keyword "@synduce" then option_parse option_args else ()
    | _ -> ()
  in
  match setting.attr_payload with
  | PStr [ str_attr ] ->
    (match str_attr.pstr_desc with
    | Pstr_eval (e, _) ->
      let value = Fmt.str "%a" Pprintast.expression e in
      analyze_parts value
    | _ -> ())
  | _ -> ()
;;

(* ============================================================================================= *)
(*                                                                                               *)
(* ============================================================================================= *)
[%%if ocaml_version < (4, 12, 0)]

let extract_params decl =
  let f (ct, variance) =
    match variance with
    | Asttypes.Invariant ->
      (match ct.ptyp_desc with
      | Ptyp_var x -> x
      | _ -> failwith "Only type variable as parameters supported.")
    | _ -> failwith "Covariant and contravariant types unsupported."
  in
  List.map ~f decl.ptype_params
;;

[%%else]

let extract_params decl =
  let f (ct, variance) =
    match variance with
    | Asttypes.NoVariance, _ ->
      (match ct.ptyp_desc with
      | Ptyp_var x -> x
      | _ -> failwith "Only type variable as parameters supported.")
    | _ -> failwith "Covariant and contravariant types unsupported."
  in
  List.map ~f decl.ptype_params
;;

[%%endif]
[%%if ocaml_version < (4, 12, 0)]

(** Parse set defintions.  *)
let define_module (binding : module_binding) : unit =
  match binding.pmb_expr.pmod_desc with
  | Pmod_apply (f, arg) ->
    (match binding.pmb_name.txt, f.pmod_desc, arg.pmod_desc with
    | module_name, Pmod_ident fid, Pmod_ident b ->
      (match Longident.flatten fid.txt with
      | [ "Set"; "Make" ] ->
        (* TODO: extend supported types in sets. *)
        (match Longident.flatten b.txt with
        | [ "Int" ] -> add_module module_name (TSet TInt)
        | [ "Bool" ] -> add_module module_name (TSet TBool)
        | _ -> ())
      | _ -> ())
    | _ -> ())
  | _ -> ()
;;

[%%else]

(** Parse set defintions.  *)
let define_module (binding : module_binding) : unit =
  match binding.pmb_expr.pmod_desc with
  | Pmod_apply (f, arg) ->
    (match binding.pmb_name.txt, f.pmod_desc, arg.pmod_desc with
    | Some module_name, Pmod_ident fid, Pmod_ident b ->
      (match Longident.flatten fid.txt with
      | [ "Set"; "Make" ] ->
        (* TODO: extend supported types in sets. *)
        (match Longident.flatten b.txt with
        | [ "Int" ] -> add_module module_name (TSet TInt)
        | [ "Bool" ] -> add_module module_name (TSet TBool)
        | _ -> ())
      | _ -> ())
    | _ -> ())
  | _ -> ()
;;

[%%endif]
[%%if ocaml_version < (4, 13, 0)]

let constructor_arguments args =
  match args with
  | Some pat ->
    (match pat.ppat_desc with
    | Ppat_var ident -> [ mk_var (wloc pat.ppat_loc) ident.txt ]
    | Ppat_tuple pats -> List.map ~f:fterm_of_pattern pats
    | Ppat_any -> [ mk_any (wloc pat.ppat_loc) ]
    | _ ->
      failwith
        (Fmt.str
           "Pattern not supported in constructor arguments: %a."
           Pprintast.pattern
           pat))
  | None -> []
;;

[%%else]

let constructor_arguments args =
  match args with
  | Some (_, pat) ->
    (match pat.ppat_desc with
    | Ppat_var ident -> [ mk_var (wloc pat.ppat_loc) ident.txt ]
    | Ppat_tuple pats -> List.map ~f:fterm_of_pattern pats
    | Ppat_any -> [ mk_any (wloc pat.ppat_loc) ]
    | _ ->
      failwith
        (Fmt.str
           "Pattern not supported in constructor arguments: %a."
           Pprintast.pattern
           pat))
  | None -> []
;;

[%%endif]

let read_sig filename =
  Location.input_name := filename;
  let handle =
    try Stdio.In_channel.create filename with
    | Sys_error msg ->
      Stdio.prerr_endline msg;
      Caml.exit 1
  in
  let buf = Lexing.from_channel handle in
  Location.init buf filename;
  let ast = Parse.implementation buf in
  Stdio.In_channel.close handle;
  Log.debug (fun f () -> Fmt.(pf f "Input program:@;%a" Pprintast.structure ast));
  ast
;;

let rec type_term_of_core_type (t : core_type) : type_term =
  let of_params =
    let f (s : string Location.loc) = mk_t_param (sloc s) s.txt in
    List.map ~f
  in
  match t.ptyp_desc with
  | Ptyp_var s -> mk_t_param (wloc t.ptyp_loc) s
  | Ptyp_poly (params, t) ->
    (match params with
    | [] -> type_term_of_core_type t
    | _ -> mk_t_constr (wloc t.ptyp_loc) (of_params params) (type_term_of_core_type t))
  | Ptyp_tuple _ -> failwith "tuple not supported"
  | Ptyp_constr (c, targs) ->
    (match c.txt with
    | Lident cid ->
      let t1 = mk_t_typ (wloc c.loc) cid in
      (match targs with
      | [] -> t1
      | _ -> mk_t_constr (wloc t.ptyp_loc) (List.map ~f:type_term_of_core_type targs) t1)
    | _ -> failwith Fmt.(str "constr %s unsupported" "unk"))
  | _ ->
    Log.error_msg Fmt.(str "%a" Pprintast.core_type t);
    failwith "type unsupported"
;;

let type_term_of_type_decl (decl : type_declaration) =
  let variants_of_cstr clist =
    let f (cstr : constructor_declaration) =
      let variant = function
        | Pcstr_tuple types -> List.map ~f:type_term_of_core_type types
        | Pcstr_record _ -> failwith "Constructor with record not supported."
      in
      mk_t_variant (wloc cstr.pcd_loc) cstr.pcd_name.txt (variant cstr.pcd_args)
    in
    List.map ~f clist
  in
  let tterm =
    match decl.ptype_kind with
    | Ptype_variant cstrs -> mk_t_sum (wloc decl.ptype_loc) (variants_of_cstr cstrs)
    | _ -> mk_t_int (wloc decl.ptype_loc)
  in
  let tname = decl.ptype_name in
  tname, extract_params decl, tterm
;;

let _is_recursive_flag (is_rec : Asttypes.rec_flag) =
  match is_rec with
  | Asttypes.Recursive -> Log.info (fun f () -> Fmt.(pf f "Recursive"))
  | _ -> ()
;;

(**
  `fun_args expr` unwraps the function arguments of expr, if expr is an expression
    deifning a function (Pexp_fun).
*)
let unwrap_args (expr : expression) =
  let rec extract (e : expression) =
    match e.pexp_desc with
    | Pexp_fun (label, default, pat, e) ->
      let args_rest, body = extract e in
      (label, default, Some pat) :: args_rest, body
    | Pexp_function _ -> [ Asttypes.Nolabel, None, None ], e
    | _ -> [], e
  in
  extract expr
;;

let _tuple_of_idents_attribute (name : string) (attr : attribute) : string list option =
  let all_items_tuples items =
    all_or_none
      (List.map
         ~f:(fun sitem ->
           match sitem.pstr_desc with
           | Pstr_eval (expr, _) -> tuple_of_idents_expr expr
           | _ -> None)
         items)
  in
  if String.equal attr.attr_name.txt name
  then (
    match attr.attr_payload with
    | PStr items ->
      (match all_items_tuples items with
      | Some [ l ] -> Some l
      | _ -> None)
    | _ -> None)
  else None
;;

(* ============================================================================================= *)
(*                               ATTRIBUTES : ENSURES & REQUIRES                                 *)
(* ============================================================================================= *)

let parse_term_attribute ~(name : string) (attr : attribute) : Front.term option =
  if String.equal attr.attr_name.txt name
  then (
    match attr.attr_payload with
    | PStr [ s ] ->
      (match s.pstr_desc with
      | Pstr_eval (ensures_expr, _) -> Some (fterm_of_expr ensures_expr)
      | _ ->
        Log.error_msg Fmt.(str "Ignore %s %a." name (Printast.structure 0) [ s ]);
        None)
    | _ as pa ->
      Log.error (fun f () ->
          Fmt.(pf f "Ensures: wrong payload %a" (Printast.payload 0) pa));
      None)
  else None
;;

(** Returns requires, ensures *)
let get_predicate (b : value_binding) =
  (* TODO: parse mutlitple predicates? Currently, only the first "ensures" and the first "requires"
     is kept.
  *)
  let ensures_pred =
    match List.filter_map ~f:(parse_term_attribute ~name:"ensures") b.pvb_attributes with
    | [] -> None
    | hd :: _ -> Some hd
  in
  let requires_pred =
    match List.filter_map ~f:(parse_term_attribute ~name:"requires") b.pvb_attributes with
    | [] -> None
    | hd :: _ -> Some hd
  in
  requires_pred, ensures_pred
;;

(* ============================================================================================= *)

let type_definitions (_ : Asttypes.rec_flag) (decls : type_declaration list) =
  let declare (decl : type_declaration) =
    let tname, params, tterm = type_term_of_type_decl decl in
    Front.TypeDef
      ( wloc decl.ptype_loc
      , match params with
        | [] -> Front.TDSimple (tname.txt, tterm)
        | _ -> Front.TDParametric (params, tname.txt, tterm) )
  in
  List.map ~f:declare decls
;;

let rules_of_case_list
    loc
    (nont : ident)
    (preargs : ident option list)
    (cases : case list)
  =
  let fsymb = mk_var loc nont in
  let preargs =
    List.map
      ~f:(fun x -> Option.(value ~default:(mk_any loc) (map ~f:(mk_var loc) x)))
      preargs
  in
  let f (c : case) =
    if Option.is_some c.pc_guard
    then failwith "Case with guard not supported."
    else (
      let pat = c.pc_lhs in
      let loc = wloc pat.ppat_loc in
      match c.pc_lhs.ppat_desc with
      | Ppat_construct (constr_name, args) ->
        (match simple_ident_of_longident constr_name.txt with
        | Some cname ->
          let lhs = mk_data loc cname (constructor_arguments args) in
          let rhs = fterm_of_expr c.pc_rhs in
          loc, mk_app loc fsymb (preargs @ [ lhs ]), rhs
        | None -> failwith "Bad constructor name.")
      | Ppat_or _ -> failwith "Or pattern."
      | _ -> failwith "All match cases should be constructors.")
  in
  match cases with
  | [ c ] when Option.is_none c.pc_guard ->
    (match c.pc_lhs.ppat_desc with
    | Ppat_construct _ -> [ f c ]
    | Ppat_var x ->
      let rhs = fterm_of_expr c.pc_rhs in
      let loc = wloc x.loc in
      [ loc, mk_app loc fsymb (preargs @ [ mk_var loc x.txt ]), rhs ]
    | _ -> failwith "Did not recognize case.")
  | _ -> List.map ~f cases
;;

(** `as_pmrs pat expr _` interprets the binding let pat = expr as a definition of
  a set of rules that are part of a PMRS.
  `pat` should be a single identifier.
  `expr` should be either a `function ...`, in which case we have one rule per match
  case, or a `fun` expression, in which case we have only one rule without pattern
  matching.
 *)
let as_pmrs (vb : value_binding) : (ident * (loc * term * term) list * term option) option
  =
  let pat, expr = vb.pvb_pat, vb.pvb_expr in
  (* Prepend `s preargs ..` as the head of each rule in expr. *)
  let rec as_pmrs_named s preargs expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, arg_pat, body) ->
      (match arg_pat.ppat_desc with
      | Ppat_var id -> as_pmrs_named s (preargs @ [ Some id.txt ]) body
      | Ppat_any -> as_pmrs_named s (preargs @ [ None ]) body
      | _ ->
        failwith
          (Fmt.str
             "Pattern not supported in function arguments: %a."
             Pprintast.pattern
             pat))
    | Pexp_function cl -> rules_of_case_list (wloc pat.ppat_loc) s preargs cl
    | _ ->
      (try
         let t = fterm_of_expr expr in
         let loc = wloc expr.pexp_loc in
         [ ( loc
           , mk_app
               loc
               (mk_var loc s)
               (List.map
                  ~f:(fun v ->
                    Option.value (Option.map ~f:(mk_var loc) v) ~default:(mk_any loc))
                  preargs)
           , t )
         ]
       with
      | _ -> [])
  in
  match pat.ppat_desc with
  | Ppat_var iloc ->
    Some (iloc.txt, as_pmrs_named iloc.txt [] expr, snd (get_predicate vb))
  | _ -> None
;;

let params_of (expr : expression) =
  let pats_to_name l =
    let f (_, _, pat) =
      match pat with
      | Some pattern ->
        (match pattern.ppat_desc with
        | Ppat_var ident -> Some ident.txt
        | _ -> None)
      | None -> None
    in
    all_or_none (List.map ~f l)
  in
  Option.bind ~f:pats_to_name (List.drop_last (first (unwrap_args expr)))
;;

(** Interpret a list of value bindings as a set of mutually recursive functions
    defining a PMRS. Each binding should be a function. Each binding will be interpreted as a
    set of rules.
*)
let to_rules (vb : value_binding list) : ident list * pmrs_body * (ident * term) list =
  let f vb =
    match as_pmrs vb with
    | Some (fname, l, e) ->
      (match e with
      | Some ensures -> Some fname, l, [ fname, ensures ]
      | None -> Some fname, l, [])
    | None -> None, [], []
  in
  (* Each binding is interpreted as a list of rules.
      Each binding should be a function.
  *)
  let fnames, rule_sets, ensure_preds = List.unzip3 (List.map ~f vb) in
  List.filter_opt fnames, List.concat rule_sets, List.concat ensure_preds
;;

let pmrs_head_of_rec_def loc (b : value_binding) (rest : value_binding list) =
  let fname =
    match b.pvb_pat.ppat_desc with
    | Ppat_var id -> Some id.txt
    | _ -> None
  in
  let ppargs = params_of b.pvb_expr in
  let requires, ensures = get_predicate b in
  let _, prules, pensures = to_rules (b :: rest) in
  let pparams = List.map ~f:(fun (x, _) -> x) (get_objects ()) in
  reset_synt_objects ();
  match fname, ppargs with
  | Some fname, Some ppargs ->
    [ Front.PMRSDef (loc, pparams, fname, ppargs, requires, ensures, prules) ]
    @ List.map ~f:(fun (i, e) -> Front.EnsuresDef (loc, i, e)) pensures
  | _ -> []
;;

let pmrs_def_of_nonrec_def loc (b : value_binding) : definition list =
  let fname =
    match b.pvb_pat.ppat_desc with
    | Ppat_var id -> Some id.txt
    | _ -> None
  in
  let ppargs = params_of b.pvb_expr in
  let requires, ensures = get_predicate b in
  let _, core = unwrap_args b.pvb_expr in
  match core.pexp_desc with
  | Pexp_let (Asttypes.Recursive, bindings, expr) ->
    let fnames, rules, part_ensures = to_rules bindings in
    let pparams = List.map ~f:(fun (x, _) -> x) (get_objects ()) in
    reset_synt_objects ();
    let is_apply_first_rule =
      match expr.pexp_desc with
      | Pexp_apply (f, [ (_, recursive_arg) ]) ->
        (* The body after let is of the form f t *)
        (match fnames, f.pexp_desc, recursive_arg.pexp_desc with
        | hd_f :: _, Pexp_ident f, Pexp_ident _ ->
          (* f should be hd_f *)
          Option.value
            ~default:false
            (Option.map (simple_ident_of_longident f.txt) ~f:(fun x -> String.(hd_f = x)))
        | _ -> false)
      | _ -> false
    in
    if is_apply_first_rule
    then
      Option.to_list
        (Option.map2 fname ppargs ~f:(fun x y ->
             Front.PMRSDef (loc, pparams, x, y, requires, ensures, rules)))
      @ List.map ~f:(fun (i, e) -> Front.EnsuresDef (loc, i, e)) part_ensures
    else []
  | _ -> []
;;

(* `define_value loc is_rec binding` attempts to extract a PMRS definition of a function
   definition out of a Caml value definition.
*)
let define_value loc (is_rec : Asttypes.rec_flag) (bindings : value_binding list) =
  match bindings, is_rec with
  | hd :: tl, Asttypes.Recursive ->
    (* A recursive function may be a PMRS. *)
    pmrs_head_of_rec_def loc hd tl
  | [ vb ], Asttypes.Nonrecursive ->
    (* A non-recursive function may be a parametric PMRS.
         It should have the form:
         let f x1 .. xn t =
           let rec g = function ...
           and h _ = function ..
         in
         g t
      *)
    pmrs_def_of_nonrec_def loc vb
  | _ -> []
;;

let declare_synt_obj (assert_expr : expression) =
  let t = fterm_of_expr assert_expr in
  match t.kind with
  (* assert (target = repr @ reference) *)
  | FTBin (T.Binop.Eq, target, { kind = FTApp (_, [ repr; reference ]); _ }) ->
    (match target.kind, repr.kind, reference.kind with
    | FTVar target, FTVar reprname, FTVar refname ->
      psi_comps := Some (target, refname, reprname)
    | _ -> ())
  (* assert (target = reference)  (repr is identity) *)
  | FTBin (T.Binop.Eq, target, reference) ->
    (match target.kind, reference.kind with
    | FTVar target, FTVar refname -> psi_comps := Some (target, refname, "repr")
    | _ -> ())
  | _ ->
    Log.debug_msg Fmt.(str "Ignore (assert %a)" pp_fterm t);
    ()
;;

let parse_ocaml (input_file_name : string)
    : definition list * (string * string * string) option
  =
  let input_folder_name = FilePath.dirname input_file_name in
  let definitions = read_sig input_file_name in
  let seek_options def =
    match def.pstr_desc with
    (* Toplevel attributes can be used to set some options *)
    | Pstr_attribute attribute -> parse_option attribute
    | _ -> ()
  in
  List.iter definitions ~f:seek_options;
  let pre_definitions =
    List.concat_map
      ~f:(fun pre_f ->
        let include_file_name = FilePath.make_filename [ input_folder_name; pre_f ] in
        Log.debug_msg (Fmt.str "Parsing %s" include_file_name);
        read_sig include_file_name)
      !Config.included_files
  in
  let seek_defs def =
    match def.pstr_desc with
    | Pstr_type (rec_flag, ps_type_decls) ->
      (* match type definitions. *)
      type_definitions rec_flag ps_type_decls
    | Pstr_value (rec_flag, binding_list) ->
      (* match let .. definitions. *)
      define_value (wloc def.pstr_loc) rec_flag binding_list
    | Pstr_eval ({ pexp_desc = Pexp_assert maybe_synt_obj; _ }, _) ->
      (* match assert ... for declaration of synthesis objectives. *)
      declare_synt_obj maybe_synt_obj;
      []
    | Pstr_module mod_binding ->
      define_module mod_binding;
      []
    | Pstr_include _include_declaration -> []
    | Pstr_open _include_declaration -> []
    | _ -> []
  in
  let defs = List.concat (List.map ~f:seek_defs (pre_definitions @ definitions)) in
  defs, !psi_comps
;;
