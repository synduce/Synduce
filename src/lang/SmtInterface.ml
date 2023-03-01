open Base
open Term
open Smtlib
open SmtLib
open Utils
open Option.Let_syntax
open Lwt.Syntax
module Stats : Solvers.Statistics = Utils.Stats

let is_unsat (resp : solver_response) =
  match resp with
  | Unsat -> true
  | _ -> false
;;

let is_sat (resp : solver_response) =
  match resp with
  | Sat -> true
  | _ -> false
;;

module SmtLog : Solvers.Logger = struct
  let verb = Log.verbose
  let debug = Log.debug
  let error = Log.error
  let log_queries = !Config.smt_log_queries
  let log_file = !Config.smt_solver_log_file
  let verbose = !Config.smt_solve_verbose
end

let cvc_opts = [ "--incremental"; "--lang=smt2.6" ]
let yices_opts = [ "--incremental"; "--smt2-model-format" ]
let z3_opts = [ "-in"; "-smt2" ]

module AsyncSmt = struct
  module S = Solvers.Asyncs (SmtLog) (Stats)
  include S

  (** Create a process with a Z3 solver. *)
  let make_z3_solver ?(hint = "") () =
    make_solver ~hint ~name:"Z3-SMT" Utils.Config.z3_binary_path ("z3" :: z3_opts)
  ;;

  let make_yices_solver ?(hint = "") () =
    match Utils.Config.yices_binary_path with
    | Some yices -> make_solver ~hint ~name:"Yices-SMT2" yices ("yices-smt2" :: yices_opts)
    | None ->
      Log.error_msg "Yices not found. Using z3 instead.";
      make_z3_solver ~hint ()
  ;;

  (** Create a process with a CVC4 solver. *)
  let make_cvc_solver ?(hint = "") () =
    let cvc_path = Config.cvc_binary_path () in
    let using_cvc5 = Config.using_cvc5 () in
    let name = if using_cvc5 then "CVC5-SMT" else "CVC4-SMT" in
    let executable_name = if using_cvc5 then "cvc5" else "cvc4" in
    make_solver ~hint ~name cvc_path (executable_name :: cvc_opts)
  ;;

  let make_solver ?(hint = "") (name : string) =
    match name with
    | "cvc" | "cvc4" | "cvc5" -> make_cvc_solver ~hint ()
    | "yices" -> make_yices_solver ~hint ()
    | _ -> make_z3_solver ~hint ()
  ;;
end

let wait_on_failure (counter : int ref) (t : (solver_response * 'a) Lwt.t)
    : (solver_response * 'a) Lwt.t
  =
  Lwt.bind t (fun t ->
      match t with
      (* Wait on failure. *)
      | (Unknown | Error _), _ ->
        let rec wait_and_check t =
          Lwt.bind (Lwt_unix.sleep 1.0) (fun _ ->
              if !counter > 1 || Float.(t < 0.)
              then wait_and_check (t -. 1.0)
              else Lwt.return ())
        in
        Lwt.map
          (fun _ -> t)
          (if !counter > 1
          then (
            Int.decr counter;
            wait_and_check !Config.Optims.wait_parallel_tlimit)
          else Lwt.return ())
      (* Continue on success. *)
      | _ ->
        Int.decr counter;
        Lwt.return t)
;;

(* ============================================================================================= *)
(*                                           CONVERSION FUNCTIONS                                *)
(* ============================================================================================= *)

let string_of_smtSymbol (s : smtSymbol) : string =
  match s with
  | SSimple s -> s
  | SQuoted s -> "'" ^ s
;;

let rec rtype_of_smtSort ~(ctx : Context.t) (s : smtSort) : RType.t option =
  match s with
  | SmtSort (Id sname) ->
    let%bind x = RType.get_type ctx.types (string_of_smtSymbol sname) in
    (match x with
    | RType.TParam (_, maint) -> Some maint
    | _ -> Some x)
  | Comp (Id (SSimple "Set"), [ sort ]) ->
    let%map telt = rtype_of_smtSort ~ctx sort in
    RType.TSet telt
  | Comp (Id sname, sort_params) ->
    let%bind x = RType.get_type ctx.types (string_of_smtSymbol sname) in
    let%bind y = all_or_none (List.map ~f:(rtype_of_smtSort ~ctx) sort_params) in
    (match x with
    | RType.TParam (params, maint) ->
      (match List.zip params y with
      | Ok l -> Some (RType.TParam (y, RType.sub_all l maint))
      | _ -> None)
    | _ -> None)
  | SmtSort (IdC (_, _)) ->
    Log.error_msg "Indexed sorts not implemented.";
    None
  | Comp (IdC (_, _), _) ->
    Log.error_msg "Indexed sorts not implemented.";
    None
;;

let rec sort_of_rtype (t : RType.t) : smtSort =
  match t with
  | RType.TInt -> SmtSort (Id (SSimple "Int"))
  | RType.TBool -> SmtSort (Id (SSimple "Bool"))
  | RType.TString -> SmtSort (Id (SSimple "String"))
  | RType.TChar -> SmtSort (Id (SSimple "Char"))
  | RType.TSet s -> Comp (Id (SSimple "Set"), [ sort_of_rtype s ])
  | RType.TNamed s -> SmtSort (Id (SSimple s))
  | RType.TTup tl -> SmtSort (Id (SSimple (Tuples.type_name_of_types tl)))
  | RType.TFun (tin, tout) ->
    Comp (Id (SSimple "->"), [ sort_of_rtype tin; sort_of_rtype tout ])
  | RType.TParam (args, t) -> dec_parametric t args
  | RType.TVar _ -> SmtSort (Id (SSimple "Int"))
(* Assuming polymorphism means int ok *)

and dec_parametric t args =
  match t with
  | RType.TParam _ -> failwith "only one level of parameters supported in types."
  | RType.TNamed s -> Comp (Id (SSimple s), List.map ~f:sort_of_rtype args)
  | t -> sort_of_rtype t
;;

(* Not really parametric? *)

let decl_of_tup_type (tl : RType.t list) : smtSymbol * command =
  let name = mk_symb (Tuples.type_name_of_types tl) in
  ( name
  , DeclareDatatype
      ( name
      , (* type name *)
        DDConstr
          (* one constructor *)
          [ ( mk_symb (Tuples.constr_name_of_types tl)
            , (* one selector per tuple component *)
              List.mapi tl ~f:(fun i t ->
                  let prof = Tuples.proj_name_of_types tl i in
                  mk_symb prof, sort_of_rtype t) )
          ] ) )
;;

let declare_datatype_of_rtype ~(ctx : Context.t) (t0 : RType.t)
    : (smtSymbol list * command) list
  =
  let rec aux declared_types t0 =
    let is_decl tname =
      List.exists declared_types ~f:(fun (decl_names, _) ->
          List.mem (List.map ~f:string_of_smtSymbol decl_names) tname ~equal:String.equal)
    in
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
                  [ mk_symb param_name, (TVar i, TNamed param_name) ]
                | _ -> [])
            params
        | _ -> []
      in
      let smt_sort : smtSortDec = mk_symb tname, List.length params in
      let constructors =
        List.map
          ~f:(fun (cstr_name, cstr_args) ->
            ( mk_symb cstr_name
            , List.mapi cstr_args ~f:(fun i t ->
                  ( mk_symb Fmt.(str "%s_%i" cstr_name i)
                  , sort_of_rtype (RType.sub_all (snd (List.unzip params)) t) )) ))
          (RType.get_variants ctx.types t)
      in
      let smt_constrs : datatype_dec =
        match params with
        | [] -> DDConstr constructors
        | _ -> DDParametric (fst (List.unzip params), constructors)
      in
      [ [ fst smt_sort ], DeclareDatatypes ([ smt_sort ], [ smt_constrs ]) ]
    in
    match t0 with
    (* No declaration needed for base types. *)
    | _ when RType.is_base t0 -> []
    | RType.TTup tl ->
      let s, decl = decl_of_tup_type tl in
      let deps = RType.get_datatype_depends ctx.types t0 in
      let decls =
        List.fold
          ~init:(([ s ], decl) :: declared_types)
          ~f:(fun d newt -> aux d newt)
          deps
      in
      decls
    | _ ->
      (match RType.base_name t0 with
      | Some tname ->
        if is_decl tname
        then declared_types
        else (
          let deps = RType.get_datatype_depends ctx.types t0 in
          let decl_one =
            match RType.get_type ctx.types tname with
            | Some orig_t -> declare_orig tname orig_t
            | None -> []
          in
          List.fold ~init:(decl_one @ declared_types) ~f:(fun d newt -> aux d newt) deps)
      | None -> declared_types)
  in
  aux [] t0
;;

let smtPattern_of_pattern ~(ctx : Context.t) (p : pattern) =
  match p with
  | PatAny -> Pat (mk_symb "_")
  | PatConstant c -> Pat (mk_symb (Fmt.str "%a" Constant.pp c))
  | PatVar v -> Pat (mk_symb v.vname)
  | PatConstr (c, pats) ->
    (match pats with
    | [] -> Pat (mk_symb c)
    | _ ->
      PatComp
        (mk_symb c, List.map ~f:(fun p -> mk_symb Fmt.(str "%a" (pp_pattern ctx) p)) pats))
  | PatTuple pats ->
    let smt_pats =
      List.map ~f:(fun p -> mk_symb Fmt.(str "%a" (pp_pattern ctx) p)) pats
    in
    let tl =
      List.map
        ~f:(fun pat ->
          let t, _ = infer_type ctx (term_of_pattern ctx pat) in
          t.ttyp)
        pats
    in
    let cname = Tuples.constr_name_of_types tl in
    PatComp (mk_symb cname, smt_pats)
;;

let term_of_const (c : Constant.t) : smtTerm =
  match c with
  | Constant.CInt i -> SmtTSpecConst (SCNumeral i)
  | Constant.CChar c -> SmtTSpecConst (SCString (String.of_char c))
  | Constant.CTrue -> mk_true
  | Constant.CFalse -> mk_false
  | Constant.CEmptySet t ->
    SmtTQualdId (QIas (Id (SSimple "emptyset"), sort_of_rtype (TSet t)))
;;

let rec smt_of_term ~(ctx : Context.t) (t : term) : smtTerm =
  let open Result.Let_syntax in
  let rec aux t : (smtTerm, string) Result.t =
    match t.tkind with
    | TBox t -> aux t
    | TBin (op, t1, t2) ->
      let%map args = Result.all (List.map ~f:aux [ t1; t2 ]) in
      mk_simple_app (Binop.to_string op) args
    | TUn (op, t1) ->
      let%map t1' = aux t1 in
      mk_simple_app (Unop.to_string op) [ t1' ]
    | TConst c -> return (term_of_const c)
    | TVar x -> return (mk_var x.vname)
    | TIte (c, a, b) ->
      let%bind c' = aux c in
      let%bind a' = aux a in
      let%map b' = aux b in
      mk_ite c' a' b'
    | TTup tl ->
      let tuple_constructor =
        Tuples.constr_name_of_types (List.map ~f:(fun t -> t.ttyp) tl)
      in
      let%map args = Result.all (List.map ~f:aux tl) in
      mk_simple_app tuple_constructor args
    | TSel (tup, i) ->
      (match (fst (infer_type ctx tup)).ttyp with
      | TTup tl ->
        let proj_fun = Tuples.proj_name_of_types tl i in
        let%map tup' = aux tup in
        mk_simple_app proj_fun [ tup' ]
      | _ as typ ->
        Log.error_msg
          Fmt.(str "SMT: tuple projection %a is not correctly typed." (pp_term ctx) t);
        Log.error_msg Fmt.(str "It has type %a." RType.pp typ);
        failwith "Wrong tuple type.")
    | TData (cstr, args) ->
      (match args with
      | [] ->
        let t = t.ttyp in
        let sort = sort_of_rtype t in
        return (SmtTQualdId (QIas (Id (SSimple cstr), sort)))
      | _ ->
        let%map args' = Result.all (List.map ~f:aux args) in
        mk_simple_app cstr args')
    | TMatch (tm, cases) ->
      let%map tm' = aux tm in
      SmtTMatch (tm', List.map ~f:(smt_of_case ~ctx) cases)
    | TApp (func, args) ->
      (match func.tkind with
      | TVar v ->
        let%map args' = Result.all (List.map ~f:aux args) in
        mk_simple_app v.vname args'
      | TFun (f_args, fbody) ->
        (match List.zip f_args args with
        | Ok pre_bindings ->
          let%bind bindings, subs = make_bindings pre_bindings in
          let%map fbody' = aux (substitution subs fbody) in
          mk_let bindings fbody'
        | Unequal_lengths -> Error "Smt: unexpected malformed term.")
      | TConst _ when List.length args = 0 -> aux func
      | _ ->
        Log.error_msg Fmt.(str "Smt of term %a impossible." (pp_term ctx) t);
        Error "Smt: application function can only be variable.")
    | TFun (_, _) -> failwith "Smt: functions in terms not supported."
  and make_bindings pre_bindings =
    let bindings_of_varmap varmap =
      Result.all
        (List.map
           ~f:(fun (v, t) ->
             let%map t' = aux t in
             mk_symb v.vname, t')
           (Map.to_alist varmap))
    in
    let make_one_binding bto bdg =
      let tto = fpat_to_term bto in
      (* Try to transform the function into a let-binding, by first creating a tuple. *)
      match Matching.matches ~ctx ~pattern:tto (tuplify ctx bdg) with
      | Some varmap -> bindings_of_varmap varmap, []
      | None ->
        (match Matching.matches ~ctx ~pattern:tto bdg with
        | Some varmap -> bindings_of_varmap varmap, []
        | None ->
          (match tto.tkind with
          | TTup tl ->
            (* Replace tuple parts that are bound by a single variable. *)
            let tl_typ =
              RType.TTup (List.map ~f:(fun x -> (fst (infer_type ctx x)).ttyp) tl)
            in
            let tup_var =
              Variable.mk ctx ~t:(Some tl_typ) (Alpha.fresh ~s:"_mt_" ctx.names)
            in
            ( (let%map x = aux bdg in
               [ mk_symb tup_var.vname, x ])
            , List.mapi ~f:(fun i t -> t, mk_sel ctx (Term.mk_var ctx tup_var) i) tl )
          | _ ->
            ( Error
                (Fmt.str
                   "%a cannot match %a or %a in smt term conversion."
                   (pp_term ctx)
                   (fpat_to_term bto)
                   (pp_term ctx)
                   (tuplify ctx bdg)
                   (pp_term ctx)
                   bdg)
            , [] )))
    in
    let bindings, subs =
      List.fold ~init:(Ok [], []) pre_bindings ~f:(fun (binds, subs) (bto, bdg) ->
          let new_binds, new_subs = make_one_binding bto bdg in
          Result.combine binds new_binds ~ok:( @ ) ~err:( ^ ), subs @ new_subs)
    in
    Result.map bindings ~f:(fun b -> b, subs)
  in
  try
    match aux t with
    | Ok smt_t -> smt_t
    | Error s ->
      Log.error_msg Fmt.(str "Error when trying to convert %a." (pp_term ctx) t);
      Log.error_msg s;
      failwith "Smt term conversion failure."
  with
  | e ->
    Log.error_msg "Failure when converting to smt";
    raise e

and smt_of_case ~(ctx : Context.t) ((p, t) : Term.match_case) : match_case =
  smtPattern_of_pattern ~ctx p, smt_of_term ~ctx t
;;

let constant_of_smtConst (l : smtSpecConstant) : Constant.t =
  match l with
  | SCNumeral i -> Constant.CInt i
  | SCDecimal _ -> failwith "No reals in base language."
  | SCString _ | SCBinary _ | SCHexaDecimal _ ->
    failwith "No hex, bin or string constants in language."
;;

type id_kind =
  | ICstr of string
  | IVar of variable
  | IBinop of Binop.t
  | IUnop of Unop.t
  | IBool of bool
  | ITupCstr
  | INotDef

let id_kind_of_s
    ?(typ_param = None)
    ~(ctx : Context.t)
    ~(functions : PMRS.Functions.ctx)
    (env : (string, Variable.t, String.comparator_witness) Map.t)
    (s : string)
  =
  match Map.find env s with
  | Some v -> IVar v
  | None ->
    (match Binop.of_string ~typ_param s with
    | Some bop -> IBinop bop
    | None ->
      (match Unop.of_string ~typ_param s with
      | Some unop -> IUnop unop
      | None ->
        (match RType.type_of_variant ctx.types s with
        | Some _ -> ICstr s
        | None ->
          (match s with
          | "true" -> IBool true
          | "false" -> IBool false
          | _ ->
            (* Last possibility: a global function or a pmrs. *)
            (match
               Option.first_some
                 (PMRS.Functions.find_by_name functions s)
                 (Context.find_global_var ctx s)
             with
            | Some v -> IVar v
            | _ -> if Tuples.is_constr_name s then ITupCstr else INotDef)))))
;;

let rec term_of_smt
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (env : (string, variable, String.comparator_witness) Map.t)
    (st : smtTerm)
    : term
  =
  match st with
  | SmtTQualdId (QIas (Id (SSimple "emptyset"), sort)) ->
    (match rtype_of_smtSort ~ctx sort with
    | Some (RType.TSet t) -> mk_const (Constant.CEmptySet t)
    | _ -> failwith "Unrecognized emptyset term")
  | SmtTQualdId (QIas (Id (SSimple s), _)) | SmtTQualdId (QI (Id (SSimple s))) ->
    (match id_kind_of_s ~ctx ~functions:fctx env s with
    | IVar v -> Term.mk_var ctx v
    | IBool true -> mk_const Constant.CTrue
    | IBool false -> mk_const Constant.CFalse
    | ICstr c -> mk_data ctx c []
    | _ -> failwith Fmt.(str "Smt: undefined variable %s" s))
  | SmtTSpecConst l -> mk_const (constant_of_smtConst l)
  | SmtTApp (QIas (Id (SSimple s), sort), args) ->
    let args' = List.map ~f:(term_of_smt ~ctx ~fctx env) args in
    let typ_param =
      match rtype_of_smtSort ~ctx sort with
      | Some (RType.TSet t) -> Some t
      | _ -> None
    in
    (* The line below is a hack! Make a real fix *)
    if String.(equal s "mkTuple_int_int")
    then mk_tup ctx args'
    else (
      match id_kind_of_s ~typ_param ~ctx ~functions:fctx env s with
      | ICstr c -> mk_data ctx c args'
      | ITupCstr -> mk_tup ctx args'
      | IVar v -> mk_app (Term.mk_var ctx v) args'
      | IBinop op ->
        (match args' with
        | [ t1; t2 ] -> mk_bin op t1 t2
        | [ t1 ] when Operator.(equal (Binary op) (Binary Minus)) -> mk_un Unop.Neg t1
        | _ ->
          failwith Fmt.(str "Smt: %a operator with more than two arguments." Binop.pp op))
      | IUnop op ->
        (match args' with
        | [ t1 ] -> mk_un op t1
        | _ -> failwith "Smt: a unary operator with more than one argument.")
      | IBool true -> mk_const Constant.CTrue
      | IBool false -> mk_const Constant.CFalse
      | INotDef -> failwith Fmt.(str "Smt: Undefined variable: %s" s))
  | SmtTApp (QI (Id (SSimple s)), args) ->
    let args' = List.map ~f:(term_of_smt ~ctx ~fctx env) args in
    let typ_param =
      match args' with
      | hd :: _ ->
        (match type_of (fst (infer_type ctx hd)) with
        | RType.TSet t -> Some t
        | t' -> Some t')
      | _ -> None
    in
    (* The line below is a hack! Make a real fix *)
    if String.(equal s "mkTuple_int_int")
    then mk_tup ctx args'
    else (
      match id_kind_of_s ~typ_param ~ctx ~functions:fctx env s with
      | ICstr c -> mk_data ctx c args'
      | ITupCstr -> mk_tup ctx args'
      | IVar v -> mk_app (Term.mk_var ctx v) args'
      | IBinop op ->
        (match args' with
        | [ t1; t2 ] -> mk_bin op t1 t2
        | [ t1 ] when Operator.(equal (Binary op) (Binary Minus)) -> mk_un Unop.Neg t1
        | _ ->
          failwith Fmt.(str "Smt: %a operator with more than two arguments." Binop.pp op))
      | IUnop op ->
        (match args' with
        | [ t1 ] -> mk_un op t1
        | _ -> failwith "Smt: a unary operator with more than one argument.")
      | IBool true -> mk_const Constant.CTrue
      | IBool false -> mk_const Constant.CFalse
      | INotDef -> failwith Fmt.(str "Smt: Undefined variable: %s" s))
  | SmtTExists (_, _) -> failwith "Smt: exists-terms not supported."
  | SmtTForall (_, _) -> failwith "Smt: forall-terms not supported."
  | SmtTLet (_, _) -> failwith "Smt: let-terms not supported."
  | _ ->
    failwith
      (Fmt.str "Composite identifier %a not supported." Sexp.pp_hum (sexp_of_smtTerm st))
;;

let sorted_vars_of_vars ~(ctx : Context.t) (vars : VarSet.t) : smtSortedVar list =
  List.map
    ~f:(fun v -> mk_symb v.vname, sort_of_rtype (Variable.vtype_or_new ctx v))
    (Set.elements vars)
;;

(* ============================================================================================= *)
(*                           MODELS                                                              *)
(* ============================================================================================= *)
type term_model = (string, term, Base.String.comparator_witness) Base.Map.t

let constmap_of_s_exprs
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (starting_map : (string, term, String.comparator_witness) Map.t)
    (s_exprs : Sexp.t list)
  =
  let add_sexp map sexp =
    Sexp.(
      try
        match sexp with
        | List [ Atom "define-fun"; Atom s; args; _; value ] ->
          (match args with
          | List [] ->
            let t_val_o =
              let%map smt_value = smtTerm_of_sexp value in
              term_of_smt ~ctx ~fctx (Map.empty (module String)) smt_value
            in
            (match t_val_o with
            | Some t_val -> Map.set map ~key:s ~data:t_val
            | None -> map)
          | _ -> map)
        | _ -> map
      with
      | Failure s ->
        Log.debug_msg s;
        Log.debug_msg
          Fmt.(
            str
              "Failed at converting Sexpr \"%a\" to smtTerm. Skipping."
              Sexplib.Sexp.pp
              sexp);
        map)
  in
  match s_exprs with
  | [ Sexp.List l ] -> List.fold ~f:add_sexp ~init:starting_map l
  | _ -> starting_map
;;

let model_to_constmap
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (s : solver_response)
  =
  let empty_map = Map.empty (module String) in
  match s with
  | Unsupported | Unknown | Unsat | Sat | Success -> empty_map
  | SExps s_exprs -> constmap_of_s_exprs ~ctx ~fctx empty_map s_exprs
  | Error _ -> failwith "Smt solver error"
;;

let model_to_varmap
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (hctx : VarSet.t)
    (s : solver_response)
    : term VarMap.t
  =
  let map = Map.to_alist (model_to_constmap ~ctx ~fctx s) in
  let f imap (vname, t) =
    match VarSet.find_by_name hctx vname with
    | Some v -> Map.set imap ~key:v ~data:t
    | None -> imap
  in
  List.fold_left ~init:VarMap.empty ~f map
;;

(** Given a term model, request additional models from the solver from the point where
    the current model was obtained. Simply calls (get-model) multiple times, asserting
    that the values must not be equal to the value in the previous model at each new
    call.
*)
let request_different_models
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (model : term_model)
    (num_models : int)
    (solver : AsyncSmt.solver)
  =
  let open AsyncSmt in
  let rec req_loop model i models =
    (* Assert all variables different. *)
    let%lwt _ =
      Lwt_list.iter_p
        (fun (varname, value) ->
          let smt_val = smt_of_term ~ctx value in
          smt_assert solver (mk_not (mk_eq (mk_var varname) smt_val)))
        (Map.to_alist model)
    in
    match%lwt check_sat solver with
    | Sat ->
      (match%lwt get_model solver with
      | SExps s ->
        (* New model has been found, recursively find new ones. *)
        let new_model = model_to_constmap ~ctx ~fctx (SExps s) in
        if i > 0
        then req_loop new_model (i - 1) (new_model :: models)
        else Lwt.return (new_model :: models)
      | _ -> Lwt.return models)
    | _ -> Lwt.return models
  in
  req_loop model num_models []
;;

let request_different_models_async
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (model : term_model Lwt.t)
    (num_models : int)
    (solver : AsyncSmt.solver)
  =
  let rec req_loop model i models =
    let* models = models in
    (* Assert all variables different. *)
    let* _ =
      Lwt_list.iter_s
        (fun (varname, value) ->
          let smt_val = smt_of_term ~ctx value in
          AsyncSmt.smt_assert solver (mk_not (mk_eq (mk_var varname) smt_val)))
        (Map.to_alist model)
    in
    let* resp = AsyncSmt.check_sat solver in
    match resp with
    | Sat ->
      let* model = AsyncSmt.get_model solver in
      (match model with
      | SExps s ->
        (* New model has been found, recursively find new ones. *)
        let new_model = model_to_constmap ~ctx ~fctx (SExps s) in
        if i > 0
        then req_loop new_model (i - 1) (Lwt.return (new_model :: models))
        else Lwt.return (new_model :: models)
      | _ -> Lwt.return models)
    | _ -> Lwt.return models
  in
  Lwt.bind model (fun m -> req_loop m num_models (Lwt.return []))
;;

(* ============================================================================================= *)
(*                           COMMANDS                                                            *)
(* ============================================================================================= *)
module Commands = struct
  let decl_of_tuple_type (t : RType.t) : command list =
    match t with
    | TTup tl -> [ snd (decl_of_tup_type tl) ]
    | _ -> []
  ;;

  let decls_of_vars ~(ctx : Context.t) (vars : VarSet.t) : command list =
    let f v =
      let t = Variable.vtype_or_new ctx v in
      match t with
      | RType.TFun _ ->
        let intypes, outtypes = RType.fun_typ_unpack t in
        let in_sorts = List.map ~f:sort_of_rtype intypes in
        let out_sort = sort_of_rtype outtypes in
        List.concat_map ~f:decl_of_tuple_type (outtypes :: intypes)
        @ [ DeclareFun (mk_symb v.vname, in_sorts, out_sort) ]
      | _ ->
        let sort = sort_of_rtype t in
        decl_of_tuple_type t @ [ DeclareConst (mk_symb v.vname, sort) ]
    in
    List.concat_map ~f (Set.elements vars)
  ;;

  let mk_def_fun
      ~(ctx : Context.t)
      (name : string)
      (args : (string * RType.t) list)
      (rtype : RType.t)
      (body : term)
      : command
    =
    let smt_args =
      List.map ~f:(fun (name, rtype) -> mk_symb name, sort_of_rtype rtype) args
    in
    DefineFun (mk_symb name, smt_args, sort_of_rtype rtype, smt_of_term ~ctx body)
  ;;

  let mk_preamble
      ~(logic : Logics.logic)
      ?(induction = false)
      ?(incremental = false)
      ?(load_defs = true)
      ?(models = true)
      ?(proofs = false)
      ()
      : command list
    =
    let logic_command = mk_set_logic logic
    and models_commands =
      mk_set_option "produce-models" (if models then "true" else "false")
    and tlimit_opt =
      if induction && !Config.Optims.induction_proof_tlimit >= 0
      then
        [ mk_set_option "tlimit" (Int.to_string !Config.Optims.induction_proof_tlimit) ]
      else []
    and induction_on = if induction then [ mk_set_option "quant-ind" "true" ] else []
    and proofs_on = if proofs then [ mk_set_option "produce-proofs" "true" ] else []
    and incremental_on =
      if incremental then [ mk_set_option "incremental" "true" ] else []
    and pre_defs = if load_defs then [ mk_max_def; mk_min_def ] else [] in
    logic_command
    :: models_commands
    :: (induction_on @ proofs_on @ incremental_on @ tlimit_opt @ pre_defs)
  ;;
end

(* ============================================================================================= *)
(*                             TRANSLATION FROM PMRS TO SMT define-funs-rec                      *)
(* ============================================================================================= *)

let smtPattern_of_term (t : term) : smtPattern option =
  match t.tkind with
  | TVar x -> Some (Pat (mk_symb x.vname))
  | TData (constr, args) ->
    let maybe_pat_args =
      let f t =
        match t.tkind with
        | TVar x -> Some (mk_symb x.vname)
        | _ -> None
      in
      all_or_none (List.map ~f args)
    in
    Option.map maybe_pat_args ~f:(function
        | [] -> Pat (mk_symb constr)
        | _ as pat_args -> PatComp (mk_symb constr, pat_args))
  | _ -> None
;;

(* Work in progress *)
let build_match_cases
    ~(ctx : Context.t)
    (pmrs : PMRS.t)
    vars
    (relevant_rules : PMRS.rewrite_rule list)
    : (smtTerm * match_case list) option
  =
  let build_with matched_var rest_args =
    let rule_to_match_case (_, var_args, pat, body) =
      let pattern =
        match pat with
        | Some p -> Some (smtPattern_of_pattern ~ctx p)
        | None -> None
      in
      let non_pattern_matched_args = var_args in
      let case_body =
        let body' =
          (* Tranform the function applications that do not have the extra parameters. *)
          let case f t =
            match t.tkind with
            | TApp ({ tkind = TVar fv; _ }, args) ->
              if Set.mem pmrs.pnon_terminals fv
              then (
                let all_args = List.map ~f args in
                Some
                  Term.(
                    mk_app
                      (* Only use the variable name of the nonterminal *)
                      (mk_var ctx (Variable.mk ctx fv.vname))
                      all_args))
              else None
            | _ -> None
          in
          Term.transform ~case body
        in
        let sub =
          match
            List.map2
              ~f:(fun v x -> Term.mk_var ctx v, Term.mk_var ctx x)
              non_pattern_matched_args
              rest_args
          with
          | Ok zipped -> zipped
          | Unequal_lengths -> failwith "Unexpected."
        in
        smt_of_term ~ctx (substitution sub body')
      in
      Option.map ~f:(fun x -> x, case_body) pattern
    in
    Option.map
      ~f:(fun l -> mk_var matched_var.vname, l)
      (all_or_none (List.map ~f:rule_to_match_case relevant_rules))
  in
  match List.last vars, List.drop_last vars with
  | Some x, Some rest -> build_with x rest
  | _ -> None
;;

let single_rule_case ~(ctx : Context.t) (pmrs : PMRS.t) nont vars (args, body) : smtTerm =
  let _, tout = RType.fun_typ_unpack (Variable.vtype_or_new ctx nont) in
  let body' =
    let case f t =
      match t.tkind with
      | TApp ({ tkind = TVar fv; _ }, args) ->
        if Set.mem pmrs.pnon_terminals fv
        then (
          let all_args = List.map ~f args in
          Some
            (mk_app
               ~typ:(Some tout)
               (Term.mk_var ctx (Variable.mk ctx fv.vname))
               all_args))
        else None
      | _ -> None
    in
    Term.transform ~case body
  in
  let sub =
    match List.map2 ~f:(fun v x -> Term.mk_var ctx v, Term.mk_var ctx x) args vars with
    | Ok zipped -> zipped
    | Unequal_lengths ->
      Log.error_msg "single_rule_case";
      failwith "Unexpected."
  in
  smt_of_term ~ctx (substitution sub body')
;;

let vars_and_formals ~(ctx : Context.t) (fvar : variable) =
  let args_t, out_t = RType.fun_typ_unpack (Variable.vtype_or_new ctx fvar) in
  let all_args =
    List.map
      ~f:(fun rt ->
        let v =
          Variable.mk
            ctx
            ~t:(Some rt)
            (Alpha.fresh ~s:("x" ^ String.prefix fvar.vname 2) ctx.names)
        in
        v, (mk_symb v.vname, sort_of_rtype rt))
      args_t
  in
  let vars, formals = List.unzip all_args in
  out_t, vars, formals
;;

let smt_recdefs_of_pmrs ~(ctx : Context.t) (pmrs : PMRS.t)
    : (smtSymbol list * command) list * command list
  =
  (* Sort declarations. *)
  let datatype_decls =
    List.concat_map
      ~f:(declare_datatype_of_rtype ~ctx)
      (* All the types that can contain dataypes, including tuples. *)
      PMRS.(
        (pmrs.poutput_typ :: pmrs.pinput_typ)
        @ List.concat_map (Set.elements pmrs.pnon_terminals) ~f:(fun v ->
              let int, out = RType.fun_typ_unpack (Variable.vtype_or_new ctx v) in
              out :: int))
  in
  (* Define recursive functions. *)
  let fun_of_nont (nont : variable) : (func_dec * smtTerm) option =
    let out_t, vars, formals = vars_and_formals ~ctx nont in
    let maybe_body =
      let relevant_rules =
        Map.data (Map.filter ~f:(fun (k, _, _, _) -> k.vid = nont.vid) pmrs.prules)
      in
      let all_pattern_matching =
        List.for_all relevant_rules ~f:(fun (_, _, pat, _) -> Option.is_some pat)
      in
      if all_pattern_matching
      then (
        match build_match_cases ~ctx pmrs vars relevant_rules with
        | Some (x, match_cases) -> Some (SmtTMatch (x, match_cases))
        | None ->
          (match relevant_rules with
          | [ (_, args, _, body) ] ->
            Some (single_rule_case ~ctx pmrs nont vars (args, body))
          | _ -> None))
      else (
        match relevant_rules with
        | [ (_, args, _, body) ] ->
          Some (single_rule_case ~ctx pmrs nont vars (args, body))
        | _ -> None)
    in
    Option.map maybe_body ~f:(fun body ->
        (SSimple nont.vname, formals, sort_of_rtype out_t), body)
  in
  let decls, bodies =
    List.unzip (List.filter_map ~f:fun_of_nont (Set.elements pmrs.pnon_terminals))
  in
  let definition_commands =
    let main_f =
      (* When PMRS is parametric, main symbol might be different from function symbol.  *)
      if not Variable.(pmrs.pvar = pmrs.pmain_symb)
      then (
        let out_t, vars, formals = vars_and_formals ~ctx pmrs.pvar in
        let body =
          Term.mk_app_v ctx pmrs.pmain_symb (List.map ~f:(Term.mk_var ctx) vars)
        in
        [ DefineFun
            (mk_symb pmrs.pvar.vname, formals, sort_of_rtype out_t, smt_of_term ~ctx body)
        ])
      else []
    in
    DefineFunsRec (decls, bodies) :: main_f
  in
  datatype_decls, definition_commands
;;

let mk_assert = mk_assert

let smt_of_pmrs ~(fctx : PMRS.Functions.ctx) ~(ctx : Context.t) (pmrs : PMRS.t)
    : command list
  =
  let deps = PMRS.depends ~ctx ~glob:fctx pmrs in
  let sort_decls_of_deps, decls_of_deps =
    List.unzip (List.map ~f:(smt_recdefs_of_pmrs ~ctx) deps)
  in
  let sort_decls, main_decl =
    try smt_recdefs_of_pmrs ~ctx pmrs with
    | Failure s ->
      Log.error_msg "Failed to translate PMRS to SMT definitions.";
      failwith s
  in
  let parameter_decls =
    List.map
      ~f:(fun v -> mk_fun_decl v.vname [] (sort_of_rtype (Variable.vtype_or_new ctx v)))
      pmrs.pargs
  in
  let datatype_decls = List.map ~f:snd (List.concat sort_decls_of_deps @ sort_decls) in
  datatype_decls @ parameter_decls @ List.concat decls_of_deps @ main_decl
;;
