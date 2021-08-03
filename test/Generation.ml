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
      let last_arg =
        match List.nth_opt (List.rev arguments) 0 with
        | Some (var, _) -> [ mk_var (Variable.mk var) ]
        | None -> []
      in
      let spec = mk_simple_spec ~decreases:last_arg ~ensures:[] ~requires:[] DSpecFunction in
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

let sum, soln = Lib.solve_file "benchmarks/list/sum.ml"

(* Assumption is that the main function is always the first one *)
let repr_name = (List.hd sum.pd_repr).f_var.vname

let spec_name = (List.hd sum.pd_reference).f_var.vname

let target_name = (List.hd sum.pd_target).f_var.vname

let tau_decl = get_typ_decl !Algo.AState._tau

let theta_decl = get_typ_decl !Algo.AState._theta

let is_identity (args : variable list) (fbody : term) =
  if List.length args != 1 then false else term_equal (mk_var (List.nth args 0)) fbody

let gen_match (v : term) (expansions : term list) (cases : term list) =
  if List.length expansions != List.length cases then
    failwith "Number of type cases don't match the number of given cases"
  else mk_match v (List.map2 (fun t case -> (pattern_of_term t, case)) expansions cases)

let toplevel =
  let new_target =
    let aux (t : function_descr) =
      let fv = Lang.Analysis.free_variables t.f_body in
      let const_subs : (term * term) list =
        match soln with
        | Some soln ->
            List.concat
              (List.map
                 (fun (name, _, body) ->
                   match VarSet.find_by_name fv name with
                   | Some var -> [ (mk_var var, body) ]
                   | None -> [])
                 (List.filter (fun (_, args, _) -> List.length args = 0) soln.soln_implems))
        | None -> []
      in
      let id_subs : term =
        match soln with
        | Some soln ->
            List.fold_left
              (fun term (name, _, _) ->
                match VarSet.find_by_name fv name with
                | Some var -> Lang.Analysis.replace_id_calls ~func:var term
                | None -> term)
              t.f_body
              (List.filter (fun (_, args, body) -> is_identity args body) soln.soln_implems)
        | None -> t.f_body
      in
      { t with f_body = substitution const_subs id_subs }
    in
    List.map aux sum.pd_target
  in
  List.map gen_func_descr (sum.pd_repr @ sum.pd_reference @ new_target)

let _MAX_LEMMA_ID = ref 0

let new_lemma_id () =
  let i = !_MAX_LEMMA_ID in
  _MAX_LEMMA_ID := !_MAX_LEMMA_ID + 1;
  i

let is_recur_case (case : term) =
  let free_v = VarSet.elements (Lang.Analysis.free_variables case) in
  List.fold_left ( || ) false (List.map (fun fv -> not (Lang.Analysis.is_norec (mk_var fv))) free_v)

let get_var_sig (var : variable) =
  let vtype = mk_named_type (get_t_name (Variable.vtype_or_new var)) in
  (var.vname, vtype)

let empty_assert = mk_var (Variable.mk "assert(true);")

let mk_assert (s : term) = mk_var (Variable.mk (Fmt.str "assert(%a);" pp_d_term s))

let rec gen_nested_match (args : term list) ?(cur_case : term list = [])
    (base_f : term list -> term) =
  match args with
  | [] -> base_f cur_case
  | hd :: tl ->
      let expansions = Lang.Analysis.expand_once hd in
      let cases =
        List.map (fun case -> gen_nested_match tl ~cur_case:(cur_case @ [ case ]) base_f) expansions
      in
      gen_match hd expansions cases

let add_case_lemma (case : term) =
  let args = VarSet.elements (Lang.Analysis.free_variables case) in
  let signature = mk_method_sig (List.map get_var_sig args) in
  let spec =
    mk_simple_spec
      ~ensures:
        [
          mk_bin Binop.Eq
            (mk_app
               (mk_var (Variable.mk spec_name))
               [ mk_app (mk_var (Variable.mk repr_name)) [ case ] ])
            (mk_app (mk_var (Variable.mk target_name)) [ case ]);
        ]
      ~requires:[] DSpecMethod
  in
  let body =
    match case.tkind with
    | TData (_, params) ->
        gen_nested_match params (fun cur_cases ->
            mk_assert
              (mk_bin Binop.Eq
                 (mk_app
                    (mk_var (Variable.mk spec_name))
                    [ mk_app (mk_var (Variable.mk repr_name)) [ case ] ])
                 (mk_app
                    (mk_var (Variable.mk spec_name))
                    [
                      mk_app
                        (mk_var (Variable.mk repr_name))
                        [
                          mk_app
                            (mk_var
                               (Variable.mk
                                  (match case.tkind with
                                  | TData (name, _) -> name
                                  | _ -> failwith "unexpected")))
                            cur_cases;
                        ];
                    ])))
    | _ -> failwith "Unexpected match case type"
  in
  let name = Fmt.str "lemma%d" (new_lemma_id ()) in
  (mk_toplevel (mk_lemma name signature spec (Body (Fmt.str "%a" pp_d_term body))), name)

let skeleton : d_toplevel list ref = ref []

let add_toplevel (n : d_toplevel) = skeleton := !skeleton @ [ n ]

let correctness_lemma : d_toplevel =
  let theta_type = mk_named_type (get_t_name !Algo.AState._theta) in
  let signature = mk_method_sig [ ("x", theta_type) ] in
  let spec =
    mk_simple_spec
      ~ensures:
        [
          mk_bin Binop.Eq
            (mk_app (mk_var (Variable.mk target_name)) [ mk_var (Variable.mk "x") ])
            (mk_app
               (mk_var (Variable.mk spec_name))
               [ mk_app (mk_var (Variable.mk repr_name)) [ mk_var (Variable.mk "x") ] ]);
        ]
        (* target(t)  == spec(repr(t))*)
      ~requires:[] DSpecMethod
  in
  let v = mk_var (Variable.mk ~t:(Some !Algo.AState._theta) "x") in
  let expansions = Lang.Analysis.expand_once v in
  let match_theta =
    gen_match v expansions
      (List.map
         (fun t ->
           if is_recur_case t then (
             let new_lemma, lemma_name = add_case_lemma t in
             add_toplevel new_lemma;
             match t.tkind with
             | TData (_, params) -> mk_app (mk_var (Variable.mk lemma_name)) params
             | _ -> failwith "Unexpected match case type")
           else empty_assert)
         expansions)
  in
  let body = Body (Fmt.str "%a" pp_d_term match_theta) in
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

let proof_skeleton = incl_decl @ toplevel @ gen_target soln @ !skeleton @ [ correctness_lemma ]

let ref_program : d_program = { dp_includes = []; dp_topdecls = proof_skeleton }

;;
Utils.Log.to_file "test/generated.dfy" (fun fmt () -> Fmt.pf fmt "%a" pp_d_program ref_program)
