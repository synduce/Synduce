open Base
open Utils
open Front
open Lang
module P = Pmrs_parser
module L = Pmrs_lexer

exception SyntaxError of string

let verbose = ref true
let text = ref ""

let loc_fatal_errmsg loc msg =
  Log.(
    error (fun f () -> log_with_excerpt f !text loc Fmt.string msg);
    fatal ())
;;

(* Some fatal error that print information. *)
let variable_not_found loc k =
  Log.(
    error (fun f () ->
        log_with_excerpt f !text loc (fun ff s -> Fmt.(pf ff "%s undefined." s)) k));
  Log.fatal ()
;;

let parsefile filename =
  (* Save the text for nicer error reporting. *)
  text := Stdio.In_channel.read_all filename;
  Log.reference_text := !text;
  let lexbuf =
    Lexing.from_channel ~with_positions:true (Stdio.In_channel.create filename)
  in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let module I = P.MenhirInterpreter in
  let checkpoint = P.Incremental.main lexbuf.lex_curr_p
  and supplier = I.lexer_lexbuf_to_supplier L.token lexbuf
  and succeed cp = cp
  and fail checkpoint =
    Log.(error (wrap1 "%a" Fmt.string (ErrorReports.report !text checkpoint)));
    []
  in
  (* Parse the program incrementally *)
  let prog = I.loop_handle succeed fail supplier checkpoint in
  (* Preprocessing Caml definitions into PMRS *)
  preprocess prog
;;

(**
   Iterates through the toplevel declarations of the program and adds the type declaration
   to the global type environment in Lang.RType.
*)
let seek_types ~(ctx : Term.Context.t) (prog : program) =
  List.iter
    ~f:(fun decl ->
      match decl with
      | TypeDef (_, TDParametric (p, typename, term)) ->
        (match Lang.RType.add_type ctx.types ~params:p ~typename term with
        | Ok _ -> ()
        | Error es ->
          Log.(error (fun f () -> log_with_excerpt f !text term.pos Sexp.pp_hum es));
          Log.fatal ())
      | TypeDef (_, TDSimple (typename, term)) ->
        (match Lang.RType.add_type ctx.types ~typename term with
        | Ok _ -> ()
        | Error es ->
          Log.(error (fun f () -> log_with_excerpt f !text term.pos Sexp.pp_hum es));
          Log.fatal ())
      | _ -> ())
    prog
;;

(**
   [fterm_to_term loc ~ctx  g l t] transforms fterm t at location loc into a term,
    using the variables in ctx, the global variables in g and the local variables in l.
*)
let fterm_to_term
    ~(ctx : Term.Context.t)
    (allv : Term.VarSet.t)
    globs
    (locals : Term.VarSet.t)
    rterm
  =
  let fterm_function_args args =
    let rec f x =
      match x.kind with
      | FTTup tl -> Term.(FPatTup (List.map ~f tl))
      | FTVar id -> Term.(FPatVar (Variable.mk ctx id))
      | FTAny -> Term.(FPatAny)
      | _ -> loc_fatal_errmsg x.pos "Function arguments can only be variables or tuples."
    in
    let t_args = List.map ~f args in
    Term.(fpat_vars (FPatTup t_args)), t_args
  in
  let findv loc env k =
    match Map.find env k with
    | None ->
      (match Term.VarSet.find_by_name allv k with
      | None ->
        (match Hashtbl.find globs k with
        | None -> variable_not_found loc k
        | Some x -> x)
      | Some x -> x)
    | Some x -> x
  in
  let _env = Term.VarSet.to_env locals in
  let rec f env t =
    match t.kind with
    | FTConst c -> Term.mk_const ~pos:t.pos c
    | FTApp (func, args) ->
      Term.(mk_app ~pos:t.pos (f env func) (List.map ~f:(f env) args))
    | FTData (constr, args) ->
      Term.(mk_data ctx ~pos:t.pos constr (List.map ~f:(f env) args))
    | FTVar v -> Term.mk_var ctx (findv t.pos env v)
    | FTTup l -> Term.(mk_tup ctx ~pos:t.pos (List.map ~f:(f env) l))
    | FTFun (args, body) ->
      let new_vs, fargs = fterm_function_args args in
      let new_env =
        Set.fold new_vs ~init:env ~f:(fun a v -> Map.set a ~key:v.Term.vname ~data:v)
      in
      Term.(mk_fun ctx ~pos:t.pos fargs (f new_env body))
    | FTLet (args, e, body) ->
      let _fun = { pos = t.pos; kind = FTFun ([ args ], body) } in
      f env { pos = t.pos; kind = FTApp (_fun, [ e ]) }
    | FTBin (op, t1, t2) -> Term.(mk_bin ~pos:t.pos op (f env t1) (f env t2))
    | FTUn (op, t1) -> Term.(mk_un ~pos:t.pos op (f env t1))
    | FTIte (c, a, b) -> Term.(mk_ite ~pos:t.pos (f env c) (f env a) (f env b))
    | FTHOBin _ -> failwith "Higher order binary operator not supported."
    | FTAny -> failwith "Pattern _ not supported."
  in
  f _env rterm
;;

let pmrs_of_rules
    ~(ctx : Term.Context.t)
    loc
    (globs : (string, Term.variable) Hashtbl.t)
    (synt_objs : Term.variable list)
    (args : Term.variable list)
    (pvar : Term.variable)
    ((requires, ensures) : term option * term option)
    (body : pmrs_body)
    : PMRS.t
  =
  (* Check that params and args do not have variables with the same name.
     Params and args can shadow globals though.
  *)
  List.iter
    ~f:(fun vp ->
      if List.mem args vp ~equal:Term.Variable.same_name
      then loc_fatal_errmsg loc "Duplicate parameter and argument name:")
    synt_objs;
  let pset = Term.VarSet.of_list synt_objs
  and aset = Term.VarSet.of_list args in
  (* First pass to collect the non-terminal variables. *)
  let nont =
    let f accum (rloc, rhead, _) =
      match rhead.kind with
      | FTApp (func, _) ->
        (match func.kind with
        | FTVar nt ->
          (match Term.VarSet.find_by_name accum nt with
          | Some _ -> accum
          | None ->
            Set.add
              accum
              (Term.Variable.mk ctx ~attrs:Term.Attributes.(singleton (NonTerminal 0)) nt))
        | _ ->
          loc_fatal_errmsg
            rloc
            (Fmt.str "Rule head first term should be a variable, not %a." pp_fterm func))
      | _ ->
        loc_fatal_errmsg
          rloc
          (Fmt.str "Rule head should be an applicative term, not %a." pp_fterm rhead)
    in
    List.fold ~f ~init:Term.VarSet.empty body
  in
  (* Find a variable in environment. If it's not in the local enviroment, it should be in the globals.*)
  let allv = Term.VarSet.union_list [ nont; pset; aset ] in
  let translate_pattern constr args : Term.pattern =
    let f arg =
      let rec aux t =
        Term.(
          match t.kind with
          | FTVar x -> PatVar (Variable.mk ctx x)
          | FTData (cstr, xl) -> PatConstr (cstr, List.map ~f:aux xl)
          | FTAny -> PatAny
          | _ ->
            loc_fatal_errmsg
              arg.pos
              (Fmt.str "expected a variable in pattern, got %a" pp_fterm arg))
      in
      aux arg
    in
    PatConstr (constr, List.map ~f args)
  in
  let rules : PMRS.rewrite_rule list =
    let transf_rule ((rloc, rhead, rterm) : pmrs_rule) =
      let nt, (xs, pat) =
        let rec f r_args =
          match r_args with
          | [ { kind = FTVar xt; _ } ] -> [ Term.Variable.mk ctx xt ], None
          | [ { kind = FTData (constr, args); _ } ] ->
            [], Some (translate_pattern constr args)
          | { kind = FTVar xt; _ } :: rest ->
            let s, p = f rest in
            Term.Variable.mk ctx xt :: s, p
          | [] -> [], None
          | hd :: _ -> loc_fatal_errmsg hd.pos "Unexpected term in rule head."
        in
        match rhead.kind with
        | FTApp ({ kind = FTVar nt; _ }, rule_args) ->
          ( (match Term.VarSet.find_by_name nont nt with
            | Some x -> x
            | None -> failwith "impossible")
          , f rule_args )
        | _ -> loc_fatal_errmsg rloc "Rule head is empty."
      in
      let local_vars : Term.VarSet.t =
        let pat_vars =
          match pat with
          | Some pattern -> Analysis.vars_of_pattern pattern
          | None -> Term.VarSet.empty
        in
        Set.union (Term.VarSet.of_list xs) pat_vars
      in
      let rule_rhs = fterm_to_term ~ctx allv globs local_vars rterm in
      nt, xs, pat, rule_rhs
    in
    List.map ~f:transf_rule body
  in
  let rules =
    match Map.of_alist (module Int) (List.mapi ~f:(fun i a -> i, a) rules) with
    | `Duplicate_key _ -> failwith "impossible"
    | `Ok m -> m
  in
  let main_symb =
    match Map.find rules 0 with
    | Some (x, _, _, _) -> x
    | None -> loc_fatal_errmsg loc "No main rule."
  in
  let requires_func, ensures_func =
    let f x = fterm_to_term ~ctx allv globs Term.VarSet.empty x in
    Option.map requires ~f, Option.map ~f ensures
  in
  Specifications.set_spec ~ctx pvar { ensures = ensures_func; requires = requires_func };
  let pmrs0 =
    PMRS.
      { pvar
      ; pargs = args
      ; pinput_typ = [ RType.TNamed "_?" ]
      ; poutput_typ = RType.TNamed "_?"
      ; psyntobjs = Term.VarSet.of_list synt_objs
      ; pnon_terminals = nont
      ; prules = rules
      ; porder = -1
      ; pmain_symb = main_symb
      ; plogic = SmtLogic.base_logic_info
      }
  in
  PMRS.infer_pmrs_types ~ctx pmrs0
;;

let translate_function
    ~(ctx : Term.Context.t)
    _loc
    (globs : (id, Term.Variable.t) Hashtbl.t)
    (f : Term.Variable.t)
    (args : Term.Variable.t list)
    (invariant : term option)
    (body : term)
  =
  let arg_set = Term.VarSet.of_list (f :: args) in
  let body = fterm_to_term ~ctx Term.VarSet.empty globs arg_set body in
  Log.debug_msg Fmt.(str "Term before typing:@;%a" (Term.pp_term ctx) body);
  let typed_body, _ = Term.infer_type ctx body in
  let f_type =
    match args with
    | [] -> typed_body.ttyp
    | [ a ] -> RType.TFun (Term.Variable.vtype_or_new ctx a, typed_body.ttyp)
    | _ ->
      RType.(
        TFun
          ( TTup (List.map ~f:(fun a -> Term.Variable.vtype_or_new ctx a) args)
          , typed_body.ttyp ))
  in
  let t_invar =
    Option.map invariant ~f:(fun x ->
        let x = fterm_to_term ~ctx Term.VarSet.empty globs arg_set x in
        first (Term.infer_type ctx x))
  in
  Term.Variable.update_var_types ctx [ Term.Variable.vtype_or_new ctx f, f_type ];
  f, List.map ~f:(fun v -> Term.FPatVar v) args, t_invar, typed_body
;;

let translate ~(fctx : PMRS.Functions.ctx) ~(ctx : Term.Context.t) (prog : program) =
  let globals : (string, Term.variable) Hashtbl.t = Hashtbl.create (module String) in
  (* First pass to create the global variables *)
  List.iter prog ~f:(fun decl ->
      match decl with
      | FunDef (loc, fname, _, _, _) | PMRSDef (loc, _, fname, _, _, _, _) ->
        (match Hashtbl.add globals ~key:fname ~data:(Term.Variable.mk ctx fname) with
        | `Ok -> ()
        | `Duplicate -> loc_fatal_errmsg loc (Fmt.str "%s already declared." fname))
      | _ -> ());
  (* Second pass  *)
  let pmrses =
    List.fold
      ~init:(Map.empty (module String))
      prog
      ~f:(fun pmrses decl ->
        match decl with
        | PMRSDef (loc, params, pname, args, requires, ensures, body) ->
          let vparams = List.map ~f:(Term.Variable.mk ctx) params in
          let pvar = Hashtbl.find_exn globals pname in
          let vargs = List.map ~f:(Term.Variable.mk ctx) args in
          let pmrs =
            pmrs_of_rules ~ctx loc globals vparams vargs pvar (requires, ensures) body
          in
          PMRS.Functions.register_global fctx pmrs;
          Set.iter pmrs.pnon_terminals ~f:(fun x ->
              PMRS.Functions.register_nonterminal fctx x.vid pmrs);
          Map.set pmrses ~key:pname ~data:pmrs
        | FunDef (loc, fname, args, invariant, body) ->
          let vargs = List.map ~f:(Term.Variable.mk ctx) args in
          let fvar = Hashtbl.find_exn globals fname in
          let func_info = translate_function ~ctx loc globals fvar vargs invariant body in
          (match Term.Context.add_global ctx ~key:fvar.vname ~data:func_info with
          | `Ok -> Log.verbose_msg ("Parsed " ^ fvar.vname)
          | `Duplicate -> Log.error_msg (fvar.vname ^ " already declared."));
          pmrses
        | _ -> pmrses)
  in
  (* Third pass : extra ensures. *)
  List.iter prog ~f:(function
      | EnsuresDef (_, ident, ensures) ->
        let t = fterm_to_term ~ctx Term.VarSet.empty globals Term.VarSet.empty ensures in
        (match Term.Context.find_global ctx ident with
        | Some (func, _, _, _) -> Specifications.set_ensures ~ctx func t
        | None ->
          (match PMRS.Functions.find_nonterminal_by_name fctx ident with
          | Some x -> Specifications.set_ensures ~ctx x t
          | None -> ()))
      | _ -> ());
  pmrses
;;
