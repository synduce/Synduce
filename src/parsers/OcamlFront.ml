open Base
open Parsetree
open Utils
open Lang.RType



let read_sig filename =
  Location.input_name := filename ;
  let handle =
    try Stdio.In_channel.create filename
    with Sys_error msg -> Stdio.prerr_endline msg; Caml.exit 1
  in
  let buf = Lexing.from_channel handle in
  Location.init buf filename ;
  let ast = Parse.implementation buf in
  Stdio.In_channel.close  handle ;
  Pprintast.structure Fmt.stdout ast;
  ast


let rec type_term_of_core_type (t : core_type) : type_term =
  let of_params =
    let f (s: string Location.loc) = mk_t_param dummy_loc s.txt in
    List.map ~f
  in
  match t.ptyp_desc with
  | Ptyp_var s -> mk_t_param dummy_loc s
  | Ptyp_poly (params, t) -> mk_t_constr dummy_loc (of_params params) (type_term_of_core_type t)
  | Ptyp_tuple _ -> failwith "tuple unsupported"
  | Ptyp_constr _ -> failwith "constr unsupported"
  | _ ->
    Log.error_msg Fmt.(str "%a" Pprintast.core_type t);
    failwith "type unsupported"



let type_term_of_type_decl (decl : type_declaration) =
  let variants_of_cstr clist =
    let f (cstr : constructor_declaration) =
      let variant =
        function
        | Pcstr_tuple types ->
            List.map ~f:type_term_of_core_type types
        | Pcstr_record _ -> failwith "Constructor with record not supported."
      in
      mk_t_variant dummy_loc (cstr.pcd_name.txt) (variant cstr.pcd_args)
    in
    List.map ~f clist
  in
  let tterm =
    match decl.ptype_kind with
    | Ptype_variant cstrs ->
          mk_t_sum dummy_loc (variants_of_cstr cstrs)
    | _ -> mk_t_int dummy_loc
  in
  let tname = decl.ptype_name in
  let params =
      let f (ct, variance) =
        match variance with
        | Asttypes.Invariant ->
          (match ct.ptyp_desc with
          | Ptyp_var x -> x
          | _ -> failwith "Only type variable as parameters supported.")
        | _ -> failwith "Covariant and contravariant types unsupported."
      in
      List.map ~f decl.ptype_params
  in
  tname, params, tterm


let declare_types (is_rec : Asttypes.rec_flag) (decls : type_declaration list) =
  let declare (decl : type_declaration) =
    let tname, params, _ = type_term_of_type_decl decl in
    Log.info (fun f () -> Fmt.(pf f "type %a %s = ..." (list string) params tname.txt))
  in
  (match is_rec with
  | Asttypes.Recursive ->  Log.info (fun f () -> Fmt.(pf f "Recursive" ))
  | _ -> ());
  List.iter ~f:declare decls


let declare_value (_ : Asttypes.rec_flag) (bindings : value_binding list) =
  let declare (vb : value_binding) =
    let pat = vb.pvb_pat in
    let expr = vb.pvb_expr in
    let attrs = vb.pvb_attributes in
    Log.info (fun f () -> Fmt.(pf f  "Pattern:%a" Pprintast.pattern pat));
    Log.info (fun f () -> Fmt.(pf f  "Expression:%a" Pprintast.expression expr));
    Log.info (fun f () -> Fmt.(pf f "%i attributes." (List.length attrs)))
  in
  List.iter ~f:declare bindings

let parse_ocaml (filename : string) =
  let definitions = read_sig filename in
  let per_def def =
    match def.pstr_desc with
    | Pstr_type (rec_flag, type_declarations) -> declare_types rec_flag type_declarations
    | Pstr_value (rec_flag, binding_list) -> declare_value rec_flag binding_list
    | _ -> ();
  in
  List.iter ~f:per_def definitions;
  Caml.exit 1