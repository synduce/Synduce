open Base
open OcamlAstHelpers
open Parsetree
open Utils
open Front
open Lang.RType

let psi_comps : (string * string * string) option ref = ref None

let read_sig filename =
  Location.input_name := filename;
  let handle =
    try Stdio.In_channel.create filename
    with Sys_error msg ->
      Stdio.prerr_endline msg;
      Caml.exit 1
  in
  let buf = Lexing.from_channel handle in
  Location.init buf filename;
  let ast = Parse.implementation buf in
  Stdio.In_channel.close handle;
  Fmt.(pf stdout "%a@." Pprintast.structure ast);
  ast

let rec type_term_of_core_type (t : core_type) : type_term =
  let of_params =
    let f (s : string Location.loc) = mk_t_param (sloc s) s.txt in
    List.map ~f
  in
  match t.ptyp_desc with
  | Ptyp_var s -> mk_t_param (wloc t.ptyp_loc) s
  | Ptyp_poly (params, t) ->
      mk_t_constr (wloc t.ptyp_loc) (of_params params) (type_term_of_core_type t)
  | Ptyp_tuple _ -> failwith "tuple unsupported"
  | Ptyp_constr (c, targs) -> (
      match c.txt with
      | Lident cid ->
          mk_t_constr (wloc t.ptyp_loc)
            (List.map ~f:type_term_of_core_type targs)
            (mk_t_typ (wloc c.loc) cid)
      | _ -> failwith Fmt.(str "constr %s unsupported" "unk"))
  | _ ->
      Log.error_msg Fmt.(str "%a" Pprintast.core_type t);
      failwith "type unsupported"

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
  let params =
    let f (ct, variance) =
      match variance with
      | Asttypes.Invariant -> (
          match ct.ptyp_desc with
          | Ptyp_var x -> x
          | _ -> failwith "Only type variable as parameters supported.")
      | _ -> failwith "Covariant and contravariant types unsupported."
    in
    List.map ~f decl.ptype_params
  in
  (tname, params, tterm)

let is_recursive_flag (is_rec : Asttypes.rec_flag) =
  match is_rec with Asttypes.Recursive -> Log.info (fun f () -> Fmt.(pf f "Recursive")) | _ -> ()

let fun_args (expr : expression) =
  let rec extract (e : expression) =
    match e.pexp_desc with
    | Pexp_fun (label, default, pat, e) -> (label, default, Some pat) :: extract e
    | Pexp_function _ -> [ (Asttypes.Nolabel, None, None) ]
    | _ -> []
  in
  extract expr

let tuple_of_idents_attribute (name : string) (attr : attribute) : string list option =
  let all_items_tuples items =
    all_or_none
      (List.map
         ~f:(fun sitem ->
           match sitem.pstr_desc with Pstr_eval (expr, _) -> tuple_of_idents_expr expr | _ -> None)
         items)
  in
  if String.equal attr.attr_name.txt name then
    match attr.attr_payload with
    | PStr items -> ( match all_items_tuples items with Some [ l ] -> Some l | _ -> None)
    | _ -> None
  else None

let ensures_attribute (attr : attribute) : Front.term option =
  if String.equal attr.attr_name.txt "ensures" then (
    match attr.attr_payload with
    | PStr [ s ] -> (
        match s.pstr_desc with
        | Pstr_eval (ensures_expr, _) -> Some (fterm_of_expr ensures_expr)
        | _ ->
            Log.error_msg Fmt.(str "Ignore ensures %a." (Printast.structure 0) [ s ]);
            None)
    | _ as pa ->
        Log.error (fun f () -> Fmt.(pf f "Ensures: wrong paylod %a" (Printast.payload 0) pa));
        None)
  else None

(* ============================================================================================= *)

let type_declarations (_ : Asttypes.rec_flag) (decls : type_declaration list) =
  let declare (decl : type_declaration) =
    let tname, params, tterm = type_term_of_type_decl decl in
    Front.TypeDecl
      ( wloc decl.ptype_loc,
        match params with
        | [] -> Front.TDSimple (tname.txt, tterm)
        | _ -> Front.TDParametric (params, tname.txt, tterm) )
  in
  List.map ~f:declare decls

let rules_of_case_list loc (nont : ident) (preargs : ident list) (cases : case list) =
  let fsymb = mk_var loc nont in
  let preargs = List.map ~f:(fun x -> mk_var loc x) preargs in
  let constructor_arguments args =
    match args with
    | Some pat -> (
        match pat.ppat_desc with
        | Ppat_var ident -> [ mk_var (wloc pat.ppat_loc) ident.txt ]
        | Ppat_tuple pats -> List.map ~f:fterm_of_pattern pats
        | _ -> failwith "Pattern not supported.")
    | None -> []
  in
  let f (c : case) =
    if Option.is_some c.pc_guard then failwith "Case with guard not supported."
    else
      let pat = c.pc_lhs in
      let loc = wloc pat.ppat_loc in
      match c.pc_lhs.ppat_desc with
      | Ppat_construct (constr_name, args) -> (
          match simple_ident_of_longident constr_name.txt with
          | Some cname ->
              let lhs = mk_data loc cname (constructor_arguments args) in
              let rhs = fterm_of_expr c.pc_rhs in
              (loc, mk_app loc fsymb (preargs @ [ lhs ]), rhs)
          | None -> failwith "Bad constructor name.")
      | Ppat_or _ -> failwith "Or pattern."
      | _ -> failwith "All match cases should be constructors."
  in
  List.map ~f cases

let as_pmrs (pat : pattern) (expr : expression) (_ : attribute list) =
  let rec as_pmrs_named s preargs expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, arg_pat, body) -> (
        match arg_pat.ppat_desc with
        | Ppat_var id -> as_pmrs_named s (preargs @ [ id.txt ]) body
        | _ -> failwith "pattern not supported")
    | Pexp_function cl -> rules_of_case_list (wloc pat.ppat_loc) s preargs cl
    | _ -> (
        try
          let t = fterm_of_expr expr in
          let loc = wloc expr.pexp_loc in
          [ (loc, mk_app loc (mk_var loc s) (List.map ~f:(mk_var loc) preargs), t) ]
        with _ -> [])
  in
  match pat.ppat_desc with Ppat_var iloc -> Some (as_pmrs_named iloc.txt [] expr) | _ -> None

let pmrs_head (b : value_binding) =
  let fname = match b.pvb_pat.ppat_desc with Ppat_var id -> Some id.txt | _ -> None in
  let ppargs =
    let pats_to_name l =
      let f (_, _, pat) =
        match pat with
        | Some pattern -> (
            match pattern.ppat_desc with Ppat_var ident -> Some ident.txt | _ -> None)
        | None -> None
      in
      all_or_none (List.map ~f l)
    in
    Option.bind ~f:pats_to_name (List.drop_last (fun_args b.pvb_expr))
  in
  let ppred =
    match List.filter_map ~f:ensures_attribute b.pvb_attributes with
    | [] -> None
    | hd :: _ -> Some hd
  in
  match (fname, ppargs) with Some fname, Some ppargs -> Some (fname, ppargs, ppred) | _ -> None

let declare_value loc (is_rec : Asttypes.rec_flag) (bindings : value_binding list) =
  let to_rules (vb : value_binding list) =
    let f vb =
      match as_pmrs vb.pvb_pat vb.pvb_expr vb.pvb_attributes with Some l -> l | None -> []
    in
    List.concat (List.map ~f vb)
  in
  match (bindings, is_rec) with
  | hd :: _, Asttypes.Recursive -> (
      match pmrs_head hd with
      | Some (fname, pargs, pred) ->
          let prules = to_rules bindings in
          let pparams = List.map ~f:(fun (x, _) -> x) (get_objects ()) in
          reset_synt_objects ();
          [ Front.PMRSDecl (loc, pparams, fname, pargs, pred, prules) ]
      | _ -> [])
  | _ -> []

let declare_synt_obj (assert_expr : expression) =
  let t = fterm_of_expr assert_expr in
  match t.kind with
  | FTBin (T.Binop.Eq, target, { kind = FTApp (_, [ repr; reference ]); _ }) -> (
      match (target.kind, repr.kind, reference.kind) with
      | FTVar target, FTVar reprname, FTVar refname -> psi_comps := Some (target, refname, reprname)
      | _ -> ())
  | _ -> ()

let parse_ocaml (filename : string) =
  let definitions = read_sig filename in
  let per_def def =
    match def.pstr_desc with
    | Pstr_type (rec_flag, ps_type_decls) -> type_declarations rec_flag ps_type_decls
    | Pstr_value (rec_flag, binding_list) -> declare_value (wloc def.pstr_loc) rec_flag binding_list
    | Pstr_eval ({ pexp_desc = Pexp_assert maybe_synt_obj; _ }, _) ->
        declare_synt_obj maybe_synt_obj;
        []
    | _ -> []
  in
  let defs = List.concat (List.map ~f:per_def definitions) in
  (defs, !psi_comps)
