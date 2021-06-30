open Base
open Term
open Smtlib.SmtLib
open Utils
open Option.Let_syntax

let string_of_smtSymbol (s : smtSymbol) : string =
  match s with SSimple s -> s | SQuoted s -> "'" ^ s

let rec rtype_of_smtSort (s : smtSort) : RType.t option =
  match s with
  | SmtSort (Id sname) -> (
      let%bind x = RType.get_type (string_of_smtSymbol sname) in
      match x with RType.TParam (_, maint) -> Some maint | _ -> Some x)
  | Comp (Id sname, sort_params) -> (
      let%bind x = RType.get_type (string_of_smtSymbol sname) in
      let%bind y = all_or_none (List.map ~f:rtype_of_smtSort sort_params) in
      match x with
      | RType.TParam (params, maint) -> (
          match List.zip params y with
          | Ok l -> Some (RType.TParam (y, RType.sub_all l maint))
          | _ -> None)
      | _ -> None)
  | SmtSort (IdC (_, _)) ->
      Log.error_msg "Indexed sorts not implemented.";
      None
  | Comp (IdC (_, _), _) ->
      Log.error_msg "Indexed sorts not implemented.";
      None

let rec sort_of_rtype (t : RType.t) : smtSort =
  match t with
  | RType.TInt -> SmtSort (Id (SSimple "Int"))
  | RType.TBool -> SmtSort (Id (SSimple "Bool"))
  | RType.TString -> SmtSort (Id (SSimple "String"))
  | RType.TChar -> SmtSort (Id (SSimple "Char"))
  | RType.TNamed s -> SmtSort (Id (SSimple s))
  | RType.TTup tl -> Comp (Id (SSimple "Tuple"), List.map ~f:sort_of_rtype tl)
  | RType.TFun (tin, tout) -> Comp (Id (SSimple "->"), [ sort_of_rtype tin; sort_of_rtype tout ])
  | RType.TParam (args, t) -> dec_parametric t args
  | RType.TVar _ -> SmtSort (Id (SSimple "Int"))
  (* Assuming polymorphism means int ok *)

and dec_parametric t args =
  match t with
  | RType.TParam _ -> failwith "only one level of parameters supported in types."
  | RType.TNamed s -> Comp (Id (SSimple s), List.map ~f:sort_of_rtype args)
  | t -> sort_of_rtype t
(* Not really parametric? *)

let declare_datatype_of_rtype (t0 : RType.t) : (smtSymbol list * command) list =
  let declare_orig tname t =
    let params =
      match t with
      | RType.TParam (params, _t) ->
          List.concat_map
            ~f:
              RType.(
                function
                | TVar i ->
                    let param_name = "T" ^ Int.to_string i in
                    [ (mk_symb param_name, (TVar i, TNamed param_name)) ]
                | _ -> [])
            params
      | _ -> []
    in
    let smt_sort : smtSortDec = (mk_symb tname, List.length params) in
    let constructors =
      List.map
        ~f:(fun (cstr_name, cstr_args) ->
          ( mk_symb cstr_name,
            List.mapi cstr_args ~f:(fun i t ->
                ( mk_symb Fmt.(str "%s_%i" cstr_name i),
                  sort_of_rtype (RType.sub_all (snd (List.unzip params)) t) )) ))
        (RType.get_variants t)
    in
    let smt_constrs : datatype_dec =
      match params with
      | [] -> DDConstr constructors
      | _ -> DDParametric (fst (List.unzip params), constructors)
    in
    [ ([ fst smt_sort ], DeclareDatatypes ([ smt_sort ], [ smt_constrs ])) ]
  in
  match RType.base_name t0 with
  | Some tname -> (
      match RType.get_type tname with Some orig_t -> declare_orig tname orig_t | None -> [])
  | None -> []

let smtPattern_of_pattern (p : pattern) =
  match p with
  | PatAny -> Pat (mk_symb "_")
  | PatConstant c -> Pat (mk_symb (Fmt.str "%a" Constant.pp c))
  | PatVar v -> Pat (mk_symb v.vname)
  | PatConstr (c, pats) ->
      PatComp (mk_symb c, List.map ~f:(fun p -> mk_symb Fmt.(str "%a" pp_pattern p)) pats)
  | PatTuple _ ->
      failwith "Tuple pattern in smt not supported. Need to implement tuple-type declarations."

let term_of_const (c : Constant.t) : smtTerm =
  match c with
  | Constant.CInt i -> SmtTSpecConst (SCNumeral i)
  | Constant.CTrue -> mk_true
  | Constant.CFalse -> mk_false

let rec smt_of_term (t : term) : smtTerm =
  let tk = t.tkind in
  match tk with
  | TBox t -> smt_of_term t
  | TBin (op, t1, t2) -> mk_simple_app (Binop.to_string op) (List.map ~f:smt_of_term [ t1; t2 ])
  | TUn (op, t1) -> mk_simple_app (Unop.to_string op) [ smt_of_term t1 ]
  | TConst c -> term_of_const c
  | TVar x -> mk_var x.vname
  | TIte (c, a, b) -> mk_ite (smt_of_term c) (smt_of_term a) (smt_of_term b)
  | TTup tl ->
      Log.error_msg Fmt.(str "SMT: creating tuple %a might cause errors in Z3." pp_term t);
      mk_simple_app "mkTuple" (List.map ~f:smt_of_term tl)
  | TSel (t, i) -> SmtTApp (QI (IdC (SSimple "tupleSel", [ INum i ])), [ smt_of_term t ])
  | TApp ({ tkind = TVar v; _ }, args) -> mk_simple_app v.vname (List.map ~f:smt_of_term args)
  | TData (cstr, args) -> mk_simple_app cstr (List.map ~f:smt_of_term args)
  | TMatch (tm, cases) -> SmtTMatch (smt_of_term tm, List.map ~f:smt_of_case cases)
  | TApp (_, _) ->
      Log.error_msg Fmt.(str "Smt of term %a impossible." pp_term t);
      failwith "Smt: application function can only be variable."
  | TFun (_, _) -> failwith "Smt: functions in terms not supported."

and smt_of_case ((p, t) : Term.match_case) : match_case = (smtPattern_of_pattern p, smt_of_term t)

let constant_of_smtConst (l : smtSpecConstant) : Constant.t =
  match l with
  | SCNumeral i -> Constant.CInt i
  | SCDecimal _ -> failwith "No reals in base language."
  | SCString _ | SCBinary _ | SCHexaDecimal _ ->
      failwith "No hex, bin or string constants in language."

type id_kind =
  | ICstr of string
  | IVar of variable
  | IBinop of Binop.t
  | IUnop of Unop.t
  | IBool of bool
  | INotDef

let id_kind_of_s env s =
  match Map.find env s with
  | Some v -> IVar v
  | None -> (
      match Binop.of_string s with
      | Some bop -> IBinop bop
      | None -> (
          match Unop.of_string s with
          | Some unop -> IUnop unop
          | None -> (
              match RType.type_of_variant s with
              | Some _ -> ICstr s
              | None -> (
                  match s with
                  | "true" -> IBool true
                  | "false" -> IBool false
                  | _ -> (
                      (* Last possibility: a global function or a pmrs. *)
                      match Option.first_some (PMRS.find_by_name s) (Term.find_global s) with
                      | Some v -> IVar v
                      | _ -> INotDef)))))

let rec term_of_smt (env : (string, variable, String.comparator_witness) Map.t) (st : smtTerm) :
    term =
  match st with
  | SmtTQualdId (QI (Id (SSimple s))) -> (
      match id_kind_of_s env s with
      | IVar v -> Term.mk_var v
      | IBool true -> mk_const Constant.CTrue
      | IBool false -> mk_const Constant.CFalse
      | _ -> failwith Fmt.(str "Smt: undefined variable %s" s))
  | SmtTSpecConst l -> mk_const (constant_of_smtConst l)
  | SmtTApp (QI (Id (SSimple s)), args) -> (
      let args' = List.map ~f:(term_of_smt env) args in
      match id_kind_of_s env s with
      | ICstr c -> mk_data c args'
      | IVar v -> mk_app (Term.mk_var v) args'
      | IBinop op -> (
          match args' with
          | [ t1; t2 ] -> mk_bin op t1 t2
          | [ t1 ] when Operator.(equal (Binary op) (Binary Minus)) -> mk_un Unop.Neg t1
          | _ -> failwith Fmt.(str "Smt: %a operator with more than two arguments." Binop.pp op))
      | IUnop op -> (
          match args' with
          | [ t1 ] -> mk_un op t1
          | _ -> failwith "Smt: a unary operator with more than one argument.")
      | IBool true -> mk_const Constant.CTrue
      | IBool false -> mk_const Constant.CFalse
      | INotDef -> failwith Fmt.(str "Smt: Undefined variable: %s" s))
  | SmtTExists (_, _) -> failwith "Smt: exists-terms not supported."
  | SmtTForall (_, _) -> failwith "Smt: forall-terms not supported."
  | SmtTLet (_, _) -> failwith "Smt: let-terms not supported."
  | _ -> failwith "Composite identifier not supported."

let sorted_vars_of_vars (vars : VarSet.t) : smtSortedVar list =
  List.map
    ~f:(fun v -> (mk_symb v.vname, sort_of_rtype (Variable.vtype_or_new v)))
    (Set.elements vars)

(* ============================================================================================= *)
(*                           MODELS                                                              *)
(* ============================================================================================= *)
type term_model = (string, term, Base.String.comparator_witness) Base.Map.t

let constmap_of_s_exprs (starting_map : (string, term, String.comparator_witness) Map.t)
    (s_exprs : Sexp.t list) =
  let add_sexp map sexp =
    Sexp.(
      match sexp with
      | List [ Atom "define-fun"; Atom s; args; _; value ] -> (
          match args with
          | List [] -> (
              let t_val_o =
                let%map smt_value = smtTerm_of_sexp value in
                term_of_smt (Map.empty (module String)) smt_value
              in
              match t_val_o with Some t_val -> Map.set map ~key:s ~data:t_val | None -> map)
          | _ -> map)
      | _ -> map)
  in
  match s_exprs with
  | [ Sexp.List l ] -> List.fold ~f:add_sexp ~init:starting_map l
  | _ -> starting_map

let model_to_constmap (s : solver_response) =
  let empty_map = Map.empty (module String) in
  match s with
  | Unknown | Unsat | Sat | Success -> empty_map
  | SExps s_exprs -> constmap_of_s_exprs empty_map s_exprs
  | Error _ -> failwith "Smt solver error"

let model_to_subst (ctx : VarSet.t) (s : solver_response) =
  let map = Map.to_alist (model_to_constmap s) in
  let f (vname, t) =
    match VarSet.find_by_name ctx vname with Some v -> [ (Term.mk_var v, t) ] | None -> []
  in
  List.concat_map ~f map

(* ============================================================================================= *)
(*                           COMMANDS                                                            *)
(* ============================================================================================= *)

let decls_of_vars (vars : VarSet.t) =
  let f v =
    let sort = sort_of_rtype (Variable.vtype_or_new v) in
    DeclareConst (SSimple v.vname, sort)
  in
  List.map ~f (Set.elements vars)

(* ============================================================================================= *)
(*                             TRANSLATION FROM PMRS TO SMT define-funs-rec                      *)
(* ============================================================================================= *)

let smtPattern_of_term (t : term) : smtPattern option =
  match t.tkind with
  | TVar x -> Some (Pat (mk_symb x.vname))
  | TData (constr, args) ->
      let maybe_pat_args =
        let f t = match t.tkind with TVar x -> Some (mk_symb x.vname) | _ -> None in
        all_or_none (List.map ~f args)
      in
      Option.map maybe_pat_args ~f:(fun pat_args -> PatComp (mk_symb constr, pat_args))
  | _ -> None

(* Work in progress *)
let build_match_cases pmrs _nont vars (relevant_rules : PMRS.rewrite_rule list) :
    (smtTerm * match_case list) option =
  let build_with matched_var rest_args =
    let rule_to_match_case (_, var_args, pat, body) =
      let pattern =
        match pat with Some (d, l) -> smtPattern_of_term (mk_data d l) | None -> None
      in
      let non_pattern_matched_args = pmrs.PMRS.pargs @ var_args in
      let case_body =
        let extra_param_args = List.map ~f:Term.mk_var pmrs.pargs in
        let body' =
          let case f t =
            match t.tkind with
            | TApp ({ tkind = TVar fv; _ }, args) ->
                if Set.mem pmrs.pnon_terminals fv then
                  let args' = List.map ~f args in
                  Some (mk_app (Term.mk_var fv) (extra_param_args @ args'))
                else None
            | _ -> None
          in
          Term.transform ~case body
        in
        let sub =
          match
            List.map2
              ~f:(fun v x -> (Term.mk_var v, Term.mk_var x))
              non_pattern_matched_args rest_args
          with
          | Ok zipped -> zipped
          | Unequal_lengths -> failwith "Unexpected."
        in
        smt_of_term (substitution sub body')
      in
      Option.map ~f:(fun x -> (x, case_body)) pattern
    in
    Option.map
      ~f:(fun l -> (mk_var matched_var.vname, l))
      (all_or_none (List.map ~f:rule_to_match_case relevant_rules))
  in
  match (List.last vars, List.drop_last vars) with
  | Some x, Some rest -> build_with x rest
  | _ -> None

let single_rule_case _pmrs _nont vars (args, body) : smtTerm =
  let sub =
    match List.map2 ~f:(fun v x -> (Term.mk_var v, Term.mk_var x)) args vars with
    | Ok zipped -> zipped
    | Unequal_lengths -> failwith "Unexpected."
  in
  smt_of_term (substitution sub body)

let vars_and_formals (pmrs : PMRS.t) (fvar : variable) =
  let args_t, out_t = RType.fun_typ_unpack (Variable.vtype_or_new fvar) in
  let vars, formals =
    List.unzip
      (List.map
         ~f:(fun rt ->
           let v = Variable.mk ~t:(Some rt) (Alpha.fresh ~s:("x" ^ fvar.vname) ()) in
           (v, (mk_symb v.vname, sort_of_rtype rt)))
         (List.map ~f:(fun v -> Variable.vtype_or_new v) pmrs.pargs @ args_t))
  in
  (out_t, vars, formals)

let _smt_of_pmrs (pmrs : PMRS.t) : (smtSymbol list * command) list * command list =
  (* Sort declarations. *)
  let datatype_decls = List.concat_map ~f:declare_datatype_of_rtype pmrs.PMRS.pinput_typ in
  (* Define recursive functions. *)
  let fun_of_nont (nont : variable) : (func_dec * smtTerm) option =
    let out_t, vars, formals = vars_and_formals pmrs nont in
    let maybe_body =
      let relevant_rules =
        Map.data (Map.filter ~f:(fun (k, _, _, _) -> k.vid = nont.vid) pmrs.prules)
      in
      let all_pattern_matching =
        List.for_all relevant_rules ~f:(fun (_, _, pat, _) -> Option.is_some pat)
      in
      if all_pattern_matching then
        match build_match_cases pmrs nont vars relevant_rules with
        | Some (x, match_cases) -> Some (SmtTMatch (x, match_cases))
        | None -> (
            match relevant_rules with
            | [ (_, args, _, body) ] -> Some (single_rule_case pmrs nont vars (args, body))
            | _ -> None)
      else
        match relevant_rules with
        | [ (_, args, _, body) ] -> Some (single_rule_case pmrs nont vars (args, body))
        | _ -> None
    in
    Option.map maybe_body ~f:(fun body ->
        ((SSimple nont.vname, formals, sort_of_rtype out_t), body))
  in
  let decls, bodies =
    List.unzip (List.filter_map ~f:fun_of_nont (Set.elements pmrs.pnon_terminals))
  in
  let definition_commands =
    let main_f =
      (* When PMRS is parametric, main symbol might be different from function symbol.  *)
      if not Variable.(pmrs.pvar = pmrs.pmain_symb) then
        let out_t, vars, formals = vars_and_formals pmrs pmrs.pvar in
        let body = Term.mk_app_v pmrs.pmain_symb (List.map ~f:Term.mk_var vars) in
        [ DefineFun (mk_symb pmrs.pvar.vname, formals, sort_of_rtype out_t, smt_of_term body) ]
      else []
    in
    DefineFunsRec (decls, bodies) :: main_f
  in
  (datatype_decls, definition_commands)

let smt_of_pmrs (pmrs : PMRS.t) : command list =
  (* TODO : order of declarations matters. *)
  let deps = PMRS.depends pmrs in
  let sort_decls_of_deps, decls_of_deps = List.unzip (List.map ~f:_smt_of_pmrs deps) in
  let sort_decls, main_decl = _smt_of_pmrs pmrs in
  let datatype_decls =
    List.map ~f:snd
      (List.dedup_and_sort
         ~compare:(fun (sorts, _) (sorts', _) ->
           List.compare
             (fun a b -> String.compare (string_of_smtSymbol a) (string_of_smtSymbol b))
             sorts sorts')
         (List.concat sort_decls_of_deps @ sort_decls))
  in
  datatype_decls @ List.concat decls_of_deps @ main_decl
