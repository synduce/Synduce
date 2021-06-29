open Codegen.Dafny
open Lang.Term
open Lang.RType

let rec convert_tkind (t : type_term) : d_domain_type =
  match t.tkind with
  | TyInt -> mk_int_type
  | TyBool -> mk_bool_type
  | TyString -> mk_string_type
  | TyChar -> mk_char_type
  | TyTyp name -> mk_named_type name
  | TyParam name -> mk_named_type name
  | TyConstr (parameters, typ) -> (
      match typ.tkind with
      | TyTyp name -> DTyNamed (name, List.map convert_tkind parameters)
      | _ -> DTyNamed ("", List.map convert_tkind parameters))
  | _ -> failwith "Unsupported type declaration"

let gen_variant (variant : type_term) =
  match variant.tkind with
  | TyVariant (name, terms) ->
      mk_datatype_constr name (List.map (fun term -> (None, convert_tkind term)) terms)
  | _ -> failwith "Unexpected variant form."

let gen_type_decl (typename : string) (tterm : type_term) =
  match tterm.tkind with
  | TySum variants ->
      let constructors : d_datatype_constr_decl list = List.map gen_variant variants in
      mk_toplevel (mk_datatype_decl typename constructors)
  | _ -> failwith "Unsupported type declaration."

let rec convert_otyp_to_dfy (typ : t) : d_domain_type =
  match typ with
  | TInt -> mk_int_type
  | TBool -> mk_bool_type
  | TString -> mk_string_type
  | TChar -> mk_char_type
  | TNamed name -> mk_named_type name
  | TTup el -> mk_tuple_type (List.map convert_otyp_to_dfy el)
  | TParam (params, typ) -> (
      match typ with
      | TNamed name -> mk_named_type ~params:(List.map convert_otyp_to_dfy params) name
      | _ -> mk_named_type ~params:(List.map convert_otyp_to_dfy params) "")
  | _ -> failwith "Unsupported type."

let rec gen_func_tup_arg (arg : fpattern) : d_domain_type =
  match arg with
  | FPatVar _ -> failwith "Unexpected argument pattern"
  | FPatTup vars ->
      mk_tuple_type
        (List.map
           (fun fpats ->
             match fpats with
             | FPatVar var -> (
                 match Variable.vtype var with
                 | Some t -> convert_otyp_to_dfy t
                 | None -> mk_named_type "FILL IN TYPE")
             | FPatTup _ -> gen_func_tup_arg fpats)
           vars)

let gen_func_arg (arg : fpattern) : string * d_domain_type =
  match arg with
  | FPatVar var -> (
      let name = var.vname in
      let typ = Variable.vtype var in
      match typ with
      | Some t -> (name, convert_otyp_to_dfy t)
      | None -> (name, mk_named_type "FILL IN TYPE"))
  | FPatTup _ -> ("tup", gen_func_tup_arg arg)

let gen_func_decl (func : term) =
  match func.tkind with
  | TFun (args, body) ->
      let signature = mk_func_sig ~returns:[] (List.map gen_func_arg args) in
      let spec = mk_simple_spec ~ensures:[] ~requires:[] DSpecFunction in
      let body = Body (Fmt.str "%a" pp_term body) in
      mk_toplevel (mk_func "target" signature spec body)
  | _ -> failwith "Unsupported function declaration."

let ex_tree_decl =
  mk_t_sum dummy_loc
    [
      mk_t_variant dummy_loc "Nil" [];
      mk_t_variant dummy_loc "Node"
        [
          mk_t_param dummy_loc "a";
          mk_t_constr dummy_loc [ mk_t_param dummy_loc "a" ] (mk_t_typ dummy_loc "Tree");
          mk_t_constr dummy_loc [ mk_t_param dummy_loc "a" ] (mk_t_typ dummy_loc "Tree");
        ];
    ]

let ex_repr_decl =
  let x = ... in
  mk_fun
    [ FPatVar (Variable.mk ~t:(Some (TParam ([ TNamed "a" ], TNamed "Tree"))) "x") ]
    (mk_var (Variable.mk "x"))
  
let f = infer_type (mk_var (Variable.mk "x"))

let tree_datatype_decl = gen_type_decl "Tree" ex_tree_decl

let repr_decl = gen_func_decl ex_repr_decl
let example_program : d_program = { dp_includes = []; dp_topdecls = [ tree_datatype_decl; repr_decl] }

;;
Fmt.(pf stdout "%a" pp_d_program example_program)
