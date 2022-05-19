open Dafny
open Lang.Term
open Lang.RType
open Common.ProblemDefs
open Common.Env
open Commons

let rec convert_tkind (t : type_term) : d_domain_type =
  match t.tkind with
  | TyInt -> mk_int_type
  | TyBool -> mk_bool_type
  | TyString -> mk_string_type
  | TyChar -> mk_char_type
  | TyTyp name -> mk_named_type name
  | TyParam name -> mk_named_type name
  | TyConstr (parameters, typ) ->
    (match typ.tkind with
    | TyTyp name -> DTyNamed (name, List.map convert_tkind parameters)
    | _ -> DTyNamed ("", List.map convert_tkind parameters))
  | _ -> failwith "Unsupported type declaration"
;;

let gen_variant (variant : type_term) =
  match variant.tkind with
  | TyVariant (name, terms) ->
    mk_datatype_constr name (List.map (fun term -> None, convert_tkind term) terms)
  | _ -> failwith "Unexpected variant form."
;;

module SS = Set.Make (String)

let rec get_params_from_type = function
  | TTup el -> List.fold_left SS.union SS.empty (List.map get_params_from_type el)
  | TFun (t1, t2) -> SS.union (get_params_from_type t1) (get_params_from_type t2)
  | TParam (params, _) ->
    List.fold_left
      SS.union
      SS.empty
      (List.map
         (fun param ->
           match param with
           | TNamed name -> SS.singleton name
           | _ -> SS.empty)
         params)
  | _ -> SS.empty
;;

let rec get_params_from_type_term (tterm : type_term) =
  match tterm.tkind with
  | TyFun (t1, t2) ->
    SS.union (get_params_from_type_term t1) (get_params_from_type_term t2)
  | TyParam name -> SS.singleton name
  | TyConstr (params, _) ->
    List.fold_left SS.union SS.empty (List.map get_params_from_type_term params)
  | TySum variants ->
    List.fold_left SS.union SS.empty (List.map get_params_from_type_term variants)
  | TyVariant (_, terms) ->
    List.fold_left SS.union SS.empty (List.map get_params_from_type_term terms)
  | _ -> SS.empty
;;

let gen_type_decl (typename : string) (tterm : type_term) =
  match tterm.tkind with
  | TySum variants ->
    let constructors : d_datatype_constr_decl list = List.map gen_variant variants in
    let params = SS.elements (get_params_from_type_term tterm) in
    mk_toplevel
      (mk_datatype_decl
         ~params:(List.map (fun name -> None, name) params)
         typename
         constructors)
  | _ -> failwith "Unsupported type declaration."
;;

let rec get_params_from_arg ~ctx = function
  | FPatVar var ->
    let typ = Variable.vtype ctx var in
    (match typ with
    | Some t -> get_params_from_type t
    | None -> SS.empty)
  | FPatTup vars ->
    List.fold_left SS.union SS.empty (List.map (get_params_from_arg ~ctx) vars)
  | _ -> failwith "FIXME: _ - Pattern not supported in proof generation."
;;

let rec convert_otyp_to_dfy (typ : t) : d_domain_type =
  match typ with
  | TInt -> mk_int_type
  | TBool -> mk_bool_type
  | TString -> mk_string_type
  | TChar -> mk_char_type
  | TSet t -> mk_set_type (convert_otyp_to_dfy t)
  | TNamed name -> mk_named_type name
  | TTup el -> mk_tuple_type (List.map convert_otyp_to_dfy el)
  | TParam (params, typ) ->
    (match typ with
    | TNamed name -> mk_named_type ~params:(List.map convert_otyp_to_dfy params) name
    | _ -> mk_named_type ~params:(List.map convert_otyp_to_dfy params) "")
  | TFun (_, _) -> failwith "Unsupported TFun type"
  | TVar x -> mk_named_type (Fmt.str "t%i" x)
;;

(* failwith (Fmt.str "Unsupported TVar %d type." x) *)

let rec gen_func_tup_arg ~ctx (arg : fpattern) : d_domain_type =
  match arg with
  | FPatAny | FPatVar _ -> failwith "Unexpected argument pattern"
  | FPatTup vars ->
    mk_tuple_type
      (List.map
         (fun fpats ->
           match fpats with
           | FPatAny -> failwith "Unexpected _ pattern."
           | FPatVar var ->
             (match Variable.vtype ctx var with
             | Some t -> convert_otyp_to_dfy t
             | None -> mk_named_type "FILL IN TYPE")
           | FPatTup _ -> gen_func_tup_arg ~ctx fpats)
         vars)
;;

let gen_func_arg ~ctx (arg : fpattern) : string * d_domain_type =
  match arg with
  | FPatVar var ->
    let name = var.vname in
    let typ = Variable.vtype ctx var in
    (match typ with
    | Some t -> name, convert_otyp_to_dfy t
    | None -> name, mk_named_type "FILL IN TYPE")
  | FPatTup _ -> "tup", gen_func_tup_arg ~ctx arg
  | FPatAny -> "_", mk_named_type "FILL IN TYPE"
;;

let gen_func_decl ~ctx (name : ident) (func : term) =
  match func.tkind with
  | TFun (args, body) ->
    let return_type =
      match infer_type ctx body with
      | t, _ -> convert_otyp_to_dfy t.ttyp
    in
    let arguments = List.map (gen_func_arg ~ctx) args in
    let params : string list =
      SS.elements
        (List.fold_left SS.union SS.empty (List.map (get_params_from_arg ~ctx) args))
    in
    let signature =
      mk_func_sig
        ~params:(List.map (fun name -> None, name) params)
        ~returns:(None, [ return_type ])
        arguments
    in
    let last_arg =
      match List.nth_opt (List.rev arguments) 0 with
      | Some (var, _) -> [ mk_var ctx (Variable.mk ctx var) ]
      | None -> []
    in
    let spec =
      mk_simple_spec ~decreases:last_arg ~ensures:[] ~requires:[] DSpecFunction
    in
    let body = Body (Fmt.str "%a" (pp_d_term ~ctx) body) in
    mk_toplevel (mk_func name signature spec body)
  | _ -> failwith "Unsupported function declaration."
;;

let _MAX_PARAM_ID = ref 0

let new_param_id () =
  let i = !_MAX_PARAM_ID in
  _MAX_PARAM_ID := !_MAX_PARAM_ID + 1;
  i
;;

let rec convert_t_tkind (typ : t) : type_term =
  match typ with
  | TInt -> mk_t_int dummy_loc
  | TBool -> mk_t_bool dummy_loc
  | TString -> mk_t_string dummy_loc
  | TChar -> mk_t_char dummy_loc
  | TNamed name -> mk_t_typ dummy_loc name
  | TSet t -> mk_t_set dummy_loc (convert_t_tkind t)
  | TTup _ -> failwith "Unsupported Type"
  | TFun (t1, t2) -> mk_t_fun dummy_loc (convert_t_tkind t1) (convert_t_tkind t2)
  | TParam (params, term) ->
    let convert_param (param : t) =
      match param with
      | TInt -> mk_t_param dummy_loc (Fmt.str "b%i" (new_param_id ()))
      | TBool -> mk_t_param dummy_loc (Fmt.str "b%i" (new_param_id ()))
      | TString -> mk_t_param dummy_loc (Fmt.str "b%i" (new_param_id ()))
      | TChar -> mk_t_param dummy_loc (Fmt.str "b%i" (new_param_id ()))
      | TVar i -> mk_t_param dummy_loc (Fmt.str "a%i" i)
      | TNamed name -> mk_t_param dummy_loc name
      | _ -> failwith "Unsupported param type"
    in
    mk_t_constr dummy_loc (List.map convert_param params) (convert_t_tkind term)
  | TVar i -> mk_t_typ dummy_loc (Fmt.str "a%i" i)
;;

let get_t_name = function
  | TParam (_, TNamed name) -> name
  | TNamed tname -> tname
  | _ -> "Can't get name of type"
;;

let get_typ_decl ~(ctx : Context.t) (typ : t) =
  let type_vars =
    List.map
      (fun (name, variant) ->
        mk_t_variant dummy_loc name (List.map convert_t_tkind variant))
      (get_variants ctx.types typ)
  in
  gen_type_decl (get_t_name typ) (mk_t_sum dummy_loc type_vars)
;;

let gen_func_descr ~ctx (desc : function_descr) =
  let name = desc.f_var.vname in
  let rec f arg =
    match arg with
    | PatVar variable -> FPatVar variable
    | PatTuple variables -> FPatTup (List.map f variables)
    | _ -> failwith (Fmt.str "Unsupported argument type. %a" (pp_pattern ctx) arg)
  in
  let args = List.map f desc.f_args in
  let func = mk_fun ctx args desc.f_body in
  gen_func_decl ~ctx name func
;;

let gen_target ~(ctx : Context.t) (s : Common.ProblemDefs.soln option) =
  match s with
  | Some sol ->
    let aux ((name, args, body) : string * variable list * term) =
      gen_func_decl ~ctx name (mk_fun ctx (List.map (fun x -> FPatVar x) args) body)
    in
    List.map aux sol.soln_implems
  | None -> []
;;

let is_identity (args : variable list) (fbody : term) =
  if List.length args != 1
  then false
  else term_equal (mk_var_no_ctx (List.nth args 0)) fbody
;;

let gen_match (v : term) (expansions : term list) (cases : d_body list) =
  if List.length expansions != List.length cases
  then failwith "Number of type cases don't match the number of given cases"
  else (
    let match_term = DTerm v in
    DBlock
      [ DMatch
          (match_term, List.map2 (fun t case -> pattern_of_term t, case) expansions cases)
      ])
;;

let _MAX_LEMMA_ID = ref 0

let new_lemma_id () =
  let i = !_MAX_LEMMA_ID in
  _MAX_LEMMA_ID := !_MAX_LEMMA_ID + 1;
  i
;;

let is_recur_case ~ctx (case : term) =
  let free_v = VarSet.elements (Lang.Analysis.free_variables ~ctx case) in
  List.fold_left
    ( || )
    false
    (List.map (fun fv -> not (Lang.Analysis.is_norec ~ctx (mk_var ctx fv))) free_v)
;;

let get_var_sig ~ctx (var : variable) =
  let vtype = convert_otyp_to_dfy (Variable.vtype_or_new ctx var) in
  var.vname, vtype
;;

let rec gen_nested_match
    ?(cur_case : (term * term) list = [])
    (args : term list)
    ~(ctx : Context.t)
    (base_f : (term * term) list -> d_body list)
  =
  match args with
  | [] -> DBlock (base_f cur_case)
  | hd :: tl ->
    let expansions = Lang.Analysis.expand_once ~ctx hd in
    let cases =
      List.map
        (fun case -> gen_nested_match ~ctx tl ~cur_case:(cur_case @ [ hd, case ]) base_f)
        expansions
    in
    gen_match hd expansions cases
;;

let rec get_all_terms_of_type ~ctx (te : term) (typ : t) (acc : term list) =
  let cur_type, _ = infer_type ctx te in
  let cur_term = if cur_type.ttyp = typ then [ te ] else [] in
  let new_terms =
    let get_all_terms_of_type = get_all_terms_of_type ~ctx in
    match te.tkind with
    | TBin (_, t1, t2) ->
      get_all_terms_of_type t1 typ acc @ get_all_terms_of_type t2 typ acc
    | TUn (_, t1) -> get_all_terms_of_type t1 typ acc
    | TConst _ -> []
    | TVar _ -> []
    | TBox t1 -> get_all_terms_of_type t1 typ acc
    | TIte (_, a, b) -> get_all_terms_of_type a typ acc @ get_all_terms_of_type b typ acc
    | TTup tl ->
      List.fold_left ( @ ) [] (List.map (fun x -> get_all_terms_of_type x typ acc) tl)
    | TSel (t, i) ->
      (match t.tkind with
      | TTup tl -> List.nth (List.map (fun x -> get_all_terms_of_type x typ acc) tl) i
      | _ -> failwith "Unexpected tuple projection")
    | TFun (_, body) -> get_all_terms_of_type body typ acc
    | TApp (_, args) ->
      List.fold_left ( @ ) [] (List.map (fun x -> get_all_terms_of_type x typ acc) args)
    | TData (_, args) ->
      List.fold_left ( @ ) [] (List.map (fun x -> get_all_terms_of_type x typ acc) args)
    | TMatch (_, _) -> []
  in
  acc @ new_terms @ cur_term
;;

let rec cartesian_prod terms =
  match terms with
  | [] -> [ [] ]
  | hd :: tl ->
    let rest = cartesian_prod tl in
    List.concat (List.map (fun x -> List.map (fun rs -> x :: rs) rest) hd)
;;

let rec has_dup = function
  | [] -> false
  | hd :: tl -> List.mem hd tl || has_dup tl
;;

let skeleton : d_toplevel list ref = ref []
let add_toplevel (n : d_toplevel) = skeleton := !skeleton @ [ n ]

let add_min_max ~ctx =
  let loc_mkv s = mk_var ctx (Variable.mk ctx s) in
  let min_func : d_toplevel =
    let signature =
      mk_func_sig
        ~returns:(Some "x", [ mk_int_type ])
        [ "a", mk_int_type; "b", mk_int_type ]
    in
    let spec =
      mk_simple_spec
        ~ensures:
          [ mk_bin
              Binop.Or
              (mk_bin Binop.Eq (loc_mkv "a") (loc_mkv "x"))
              (mk_bin Binop.Eq (loc_mkv "b") (loc_mkv "x"))
          ; mk_bin
              Binop.And
              (mk_bin Binop.Le (loc_mkv "x") (loc_mkv "a"))
              (mk_bin Binop.Le (loc_mkv "x") (loc_mkv "b"))
          ]
        ~requires:[]
        DSpecFunction
    in
    let body = Body "if a >= b then b else a" in
    mk_toplevel (mk_func "min" signature spec body)
  in
  let max_func : d_toplevel =
    let signature =
      mk_func_sig
        ~returns:(Some "x", [ mk_int_type ])
        [ "a", mk_int_type; "b", mk_int_type ]
    in
    let spec =
      mk_simple_spec
        ~ensures:
          [ mk_bin
              Binop.Or
              (mk_bin Binop.Eq (loc_mkv "a") (loc_mkv "x"))
              (mk_bin Binop.Eq (loc_mkv "b") (loc_mkv "x"))
          ; mk_bin
              Binop.And
              (mk_bin Binop.Ge (loc_mkv "x") (loc_mkv "a"))
              (mk_bin Binop.Ge (loc_mkv "x") (loc_mkv "a"))
          ]
        ~requires:[]
        DSpecFunction
    in
    let body = Body "if a >= b then a else b" in
    mk_toplevel (mk_func "max" signature spec body)
  in
  add_toplevel max_func;
  add_toplevel min_func
;;

let add_case_lemma ~fctx ~ctx ~(pd : problem_descr) ~(target_name : string) (case : term) =
  let args = VarSet.elements (Lang.Analysis.free_variables ~ctx case) in
  let signature = mk_method_sig (List.map (get_var_sig ~ctx) args) in
  (* Context methods *)
  let mk_app_v = mk_app_v ctx in
  let mk_var = mk_var ctx in
  let lhs =
    mk_app_v
      (List.hd pd.pd_reference).f_var
      [ mk_app_v (List.hd pd.pd_repr).f_var [ case ] ]
  in
  let rhs = mk_app (mk_var (Variable.mk ctx target_name)) [ case ] in
  let spec =
    mk_simple_spec ~ensures:[ mk_bin Binop.Eq lhs rhs ] ~requires:[] DSpecMethod
  in
  let name = Fmt.str "lemma%d" (new_lemma_id ()) in
  let body =
    match case.tkind with
    | TData (_, params) ->
      gen_nested_match
        ~ctx
        (List.filter (fun param -> not (Lang.Analysis.is_norec ~ctx param)) params)
        (fun cases ->
          let lhs_sub = substitution cases lhs in
          let rhs_sub = substitution cases rhs in
          let lhs_calc =
            List.map (fun x -> DStmt (DTerm x)) (Lang.Reduce.calc_term ~ctx ~fctx lhs_sub)
          in
          let last_term = List.hd (List.rev (Lang.Reduce.calc_term ~fctx ~ctx lhs_sub)) in
          let lemma_opts =
            List.map
              (fun arg ->
                let arg_type = Variable.vtype_or_new ctx arg in
                get_all_terms_of_type ~ctx last_term arg_type [])
              args
          in
          let prod = cartesian_prod lemma_opts in
          let filtered_prod =
            List.filter
              (fun (p : term list) ->
                not
                  (has_dup p
                  || List.fold_left
                       (fun acc (_, case) -> acc || List.mem case p)
                       false
                       cases))
              prod
          in
          let new_lemmas =
            List.map
              (fun args -> DStmt (DTerm (mk_app_v (Variable.mk ctx name) args)))
              filtered_prod
          in
          new_lemmas @ [ DCalc (lhs_calc @ [ DStmt (DTerm rhs_sub) ]) ])
    | _ -> failwith "Unexpected match case type"
  in
  mk_toplevel (mk_lemma name signature spec body), name
;;

let get_num_params = function
  | DDatatypeDecl (_, _, params, _) -> List.length params
  | _ -> failwith "Can't get params from a non-datatype declaration."
;;

(* Uncomment for termination template *)

let add_depth_f ~ctx () =
  let mkv ?(t = None) s = mk_var ctx.ctx (Variable.mk ctx.ctx ~t s) in
  let v = mkv ~t:(Some (get_theta ctx)) "x" in
  let expansions = ctx >- Lang.Analysis.expand_once v in
  let case_bodies =
    List.map
      (fun cur ->
        if ctx >- is_recur_case cur
        then (
          let get_recur c =
            let free_v =
              List.map
                (mk_var ctx.ctx)
                (VarSet.elements (ctx >- Lang.Analysis.free_variables c))
            in
            List.filter (fun var -> not (ctx >- Lang.Analysis.is_norec var)) free_v
          in
          mk_app
            (mkv (Lang.Alpha.fresh ~s:"hdepth" ctx.ctx.names))
            (List.map (fun var -> mk_app (mkv "depth") [ var ]) (get_recur cur)))
        else mkv (Lang.Alpha.fresh ctx.ctx.names))
      expansions
  in
  let depth_f =
    let signature =
      mk_func_sig
        ~returns:(None, [ mk_int_type ])
        [ "x", mk_named_type (get_t_name (get_theta ctx)) ]
    in
    let spec =
      mk_simple_spec
        ~ensures:
          [ mk_bin
              Binop.Ge
              (mk_app (mkv "depth") [ mkv "x" ])
              (mk_const (Constant.CInt 0))
          ]
        ~requires:[]
        DSpecFunction
    in
    let match_theta =
      let cases =
        List.map2 (fun body case -> pattern_of_term case, body) case_bodies expansions
      in
      mk_match ctx.ctx v cases
    in
    let variable_decls : d_body list =
      List.map
        (fun (body, _) -> DStmt (DAssign (body, DTerm (mkv "Fill me in"))))
        (List.filter
           (fun (_, case) -> not (ctx >- is_recur_case case))
           (List.combine case_bodies expansions))
    in
    let body = DBlock (variable_decls @ [ DTerm match_theta ]) in
    mk_toplevel (mk_func "depth" signature spec body)
  in
  let helper_functions =
    List.map
      (fun ((body, _) : term * term) ->
        match body.tkind with
        | TApp (f, args) ->
          let name =
            match f.tkind with
            | TVar v -> v.vname
            | _ -> failwith "Unsupported function type"
          in
          let help_sig =
            mk_func_sig
              ~returns:(None, [ mk_int_type ])
              (List.map
                 (fun _ -> Lang.Alpha.fresh ~s:"i" ctx.ctx.names, mk_int_type)
                 args)
          in
          let help_spec = mk_simple_spec ~ensures:[] ~requires:[] DSpecFunction in
          let help_body = Body "Fill me in" in
          mk_toplevel (mk_func name help_sig help_spec help_body)
        | _ -> failwith "Unexpected non-function app")
      (List.filter
         (fun (_, case) -> ctx >- is_recur_case case)
         (List.combine case_bodies expansions))
  in
  List.iter add_toplevel helper_functions;
  add_toplevel depth_f
;;

let correctness_lemma
    ~fctx
    ~ctx
    ~(pd : problem_descr)
    ~(target_name : string)
    ~(spec_name : string)
    ~(repr_name : string)
    : d_toplevel
  =
  let mkv v = mk_var ctx (Variable.mk ctx v) in
  let theta_type =
    (* We grab the type from the target function to ensure that parametrization matches *)
    let target_descr = List.hd pd.pd_target in
    let rec f arg =
      match arg with
      | PatVar variable -> FPatVar variable
      | PatTuple variables -> FPatTup (List.map f variables)
      | _ -> failwith (Fmt.str "Unsupported argument type. %a" (pp_pattern ctx) arg)
    in
    match f (List.hd target_descr.f_args) with
    | FPatVar var ->
      let typ = Variable.vtype ctx var in
      (match typ with
      | Some t -> t
      | None -> TNamed "FILL IN TYPE")
    | _ -> failwith "Unsupported argument type"
  in
  let signature = mk_method_sig [ "x", convert_otyp_to_dfy theta_type ] in
  let spec =
    mk_simple_spec
      ~ensures:
        [ mk_bin
            Binop.Eq
            (mk_app (mkv target_name) [ mkv "x" ])
            (mk_app (mkv spec_name) [ mk_app (mkv repr_name) [ mkv "x" ] ])
        ]
        (* target(t) == spec(repr(t))*)
      ~requires:[]
      DSpecMethod
  in
  let v = mk_var ctx (Variable.mk ctx ~t:(Some theta_type) "x") in
  let expansions = Lang.Analysis.expand_once ~ctx v in
  let match_theta =
    gen_match
      v
      expansions
      (List.map
         (fun t ->
           if is_recur_case ~ctx t
           then (
             let new_lemma, lemma_name = add_case_lemma ~fctx ~ctx ~pd ~target_name t in
             add_toplevel new_lemma;
             match t.tkind with
             | TData (_, params) ->
               DStmt (DTerm (mk_app (mk_var ctx (Variable.mk ctx lemma_name)) params))
             | _ -> failwith "Unexpected match case type")
           else DStmt (DAssert (mk_const Constant.CTrue)))
         expansions)
  in
  let body = match_theta in
  mk_toplevel (mk_lemma "correctness_lemma" signature spec body)
;;

let incl_decl ~ctx ~(theta_decl : d_toplevel) ~(tau_decl : d_toplevel) : d_toplevel list =
  (* If tau and theta are the same type, prioritize one based on the number of parameters *)
  if get_t_name (get_tau ctx) = get_t_name (get_theta ctx)
  then
    if get_num_params tau_decl.dt_kind >= get_num_params theta_decl.dt_kind
    then [ tau_decl ]
    else [ theta_decl ]
  else [ tau_decl; theta_decl ]
;;

(* ============================================================================================= *)
(*                                    MAIN ENTRY POINT : GENERATING THE DAFNY PROOF SKETCH       *)
(* ============================================================================================= *)

(**   *)
let gen_proof ~ctx (pd, soln) (out_file : string) =
  (* Assumption is that the main function is always the first one *)
  let repr_name = (List.hd pd.pd_repr).f_var.vname in
  let spec_name = (List.hd pd.pd_reference).f_var.vname in
  let target_name = (List.hd pd.pd_target).f_var.vname in
  let tau_decl = ctx >- get_typ_decl (get_tau ctx) in
  let theta_decl = ctx >- get_typ_decl (get_theta ctx) in
  let toplevel =
    let new_target =
      let aux (t : function_descr) =
        let fv = ctx >- Lang.Analysis.free_variables t.f_body in
        let const_subs : (term * term) list =
          match soln with
          | Some soln ->
            List.concat
              (List.map
                 (fun (name, _, body) ->
                   match VarSet.find_by_name fv name with
                   | Some var -> [ mk_var ctx.ctx var, body ]
                   | None -> [])
                 (List.filter
                    (fun (_, args, _) -> List.length args = 0)
                    soln.soln_implems))
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
              (List.filter
                 (fun (_, args, body) -> is_identity args body)
                 soln.soln_implems)
          | None -> t.f_body
        in
        { t with f_body = substitution const_subs id_subs }
      in
      List.map aux pd.pd_target
    in
    List.map (ctx >- gen_func_descr) (pd.pd_repr @ pd.pd_reference @ new_target)
  in
  let proof_skeleton =
    incl_decl ~ctx ~tau_decl ~theta_decl
    @ toplevel
    @ (ctx >- gen_target soln)
    @ !skeleton
    @ [ ctx >>- correctness_lemma ~pd ~target_name ~repr_name ~spec_name ]
  in
  let ref_program : d_program = { dp_includes = []; dp_topdecls = proof_skeleton } in
  Utils.Log.to_file out_file (fun fmt () ->
      Fmt.pf fmt "%a" (ctx >- pp_d_program) ref_program)
;;
