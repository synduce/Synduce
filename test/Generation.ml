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

module SS = Set.Make (String)

let rec get_params_from_type_term (tterm : type_term) =
  match tterm.tkind with
  | TyFun (t1, t2) -> SS.union (get_params_from_type_term t1) (get_params_from_type_term t2)
  | TyParam name -> SS.singleton name
  | TyConstr (params, _) ->
      List.fold_left SS.union SS.empty (List.map get_params_from_type_term params)
  | TySum variants -> List.fold_left SS.union SS.empty (List.map get_params_from_type_term variants)
  | TyVariant (_, terms) ->
      List.fold_left SS.union SS.empty (List.map get_params_from_type_term terms)
  | _ -> SS.empty

let gen_type_decl (typename : string) (tterm : type_term) =
  match tterm.tkind with
  | TySum variants ->
      let constructors : d_datatype_constr_decl list = List.map gen_variant variants in
      let params = SS.elements (get_params_from_type_term tterm) in
      mk_toplevel
        (mk_datatype_decl
           ~params:(List.map (fun name -> (None, name)) params)
           typename constructors)
  | _ -> failwith "Unsupported type declaration."

let rec get_params_from_type = function
  | TTup el -> List.fold_left SS.union SS.empty (List.map get_params_from_type el)
  | TFun (t1, t2) -> SS.union (get_params_from_type t1) (get_params_from_type t2)
  | TParam (params, _) ->
      List.fold_left SS.union SS.empty
        (List.map
           (fun param -> match param with TNamed name -> SS.singleton name | _ -> SS.empty)
           params)
  | _ -> SS.empty

let rec get_params_from_arg = function
  | FPatVar var -> (
      let typ = Variable.vtype var in
      match typ with Some t -> get_params_from_type t | None -> SS.empty)
  | FPatTup vars -> List.fold_left SS.union SS.empty (List.map get_params_from_arg vars)

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

let gen_func_decl (name : ident) (func : term) =
  match func.tkind with
  | TFun (args, body) ->
      let return_type = match infer_type body with t, _ -> convert_otyp_to_dfy t.ttyp in
      let arguments = List.map gen_func_arg args in
      let params : string list =
        SS.elements (List.fold_left SS.union SS.empty (List.map get_params_from_arg args))
      in
      let signature =
        mk_func_sig
          ~params:(List.map (fun name -> (None, name)) params)
          ~returns:[ return_type ] arguments
      in
      let spec = mk_simple_spec ~ensures:[] ~requires:[] DSpecFunction in
      let body = Body (Fmt.str "%a" pp_d_term body) in
      mk_toplevel (mk_func name signature spec body)
  | _ -> failwith "Unsupported function declaration."

let rec convert_t_tkind (typ : t) : type_term =
  match typ with
  | TInt -> mk_t_int dummy_loc
  | TBool -> mk_t_bool dummy_loc
  | TString -> mk_t_string dummy_loc
  | TChar -> mk_t_char dummy_loc
  | TNamed name -> mk_t_typ dummy_loc name
  | TTup _ -> failwith "Unsupported Type"
  | TFun (t1, t2) -> mk_t_fun dummy_loc (convert_t_tkind t1) (convert_t_tkind t2)
  | TParam (params, term) ->
      let convert_param (param : t) =
        match param with
        | TInt -> convert_t_tkind param
        | TBool -> convert_t_tkind param
        | TString -> convert_t_tkind param
        | TChar -> convert_t_tkind param
        | TVar i -> mk_t_param dummy_loc (Fmt.str "a%i" i)
        | TNamed name -> mk_t_param dummy_loc name
        | _ -> failwith "Unsupported param type"
      in
      mk_t_constr dummy_loc (List.map convert_param params) (convert_t_tkind term)
  | TVar i -> mk_t_typ dummy_loc (Fmt.str "a%i" i)

let get_t_name = function
  | TParam (_, TNamed name) -> name
  | TNamed tname -> tname
  | _ -> "Can't get name of type"

let get_typ_decl (typ : t) =
  let type_vars =
    List.map
      (fun (name, variant) -> mk_t_variant dummy_loc name (List.map convert_t_tkind variant))
      (get_variants typ)
  in
  gen_type_decl (get_t_name typ) (mk_t_sum dummy_loc type_vars)

let gen_func_descr (desc : function_descr) =
  let name = desc.f_var.vname in
  let rec f arg =
    match arg with
    | PatVar variable -> FPatVar variable
    | PatTuple variables -> FPatTup (List.map f variables)
    | _ -> failwith (Fmt.str "Unsupported argument type. %a" pp_pattern arg)
  in
  let args = List.map f desc.f_args in
  let func = mk_fun args desc.f_body in
  gen_func_decl name func

let gen_target (s : Algo.AState.soln option) =
  match s with
  | Some sol ->
      let aux ((name, args, body) : string * variable list * term) =
        gen_func_decl name (mk_fun (List.map (fun x -> FPatVar x) args) body)
      in
      List.map aux sol.soln_implems
  | None -> []

let sum, soln = Lib.solve_file "benchmarks/tree/sum.ml"

let tau_decl = get_typ_decl !Algo.AState._tau

let theta_decl = get_typ_decl !Algo.AState._theta

let toplevel = List.map gen_func_descr (sum.pd_repr @ sum.pd_reference @ sum.pd_target)

let correctness_lemma : d_toplevel =
  let tree_type = mk_named_type (get_t_name !Algo.AState._theta) in
  let signature = mk_method_sig [ ("x", tree_type) ] in
  let spec =
    mk_simple_spec
      ~ensures:
        [
          mk_bin Binop.Eq
            (mk_app (mk_var (Variable.mk "target")) [ mk_var (Variable.mk "x") ])
            (mk_app
               (mk_var (Variable.mk "spec"))
               [ mk_app (mk_var (Variable.mk "repr")) [ mk_var (Variable.mk "x") ] ]);
        ]
        (* target(t)  == spec(repr(t))*)
      ~requires:[] DSpecMethod
  in
  let body = Body "" in
  mk_toplevel (mk_lemma "correctness_lemma" signature spec body)

let get_num_params = function
  | DDatatypeDecl (_, _, params, _) -> List.length params
  | _ -> failwith "Can't get params from a non-datatype declaration."

let incl_decl =
  (*If tau and theta are the same type, prioritize one based on the number of parameters*)
  if get_t_name !Algo.AState._tau = get_t_name !Algo.AState._theta then
    if get_num_params tau_decl.dt_kind >= get_num_params theta_decl.dt_kind then [ tau_decl ]
    else [ theta_decl ]
  else [ tau_decl; theta_decl ]

let proof_skeleton = incl_decl @ toplevel @ gen_target soln @ [ correctness_lemma ]

let ref_program : d_program = { dp_includes = []; dp_topdecls = proof_skeleton }

;;
Utils.Log.to_file "test/generated.dfy" (fun fmt () -> Fmt.pf fmt "%a" pp_d_program ref_program)
