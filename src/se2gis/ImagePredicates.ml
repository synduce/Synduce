open Base
open Common
open Env
open ProblemDefs
open Lwt
open Lang
open Term
open Utils
open Smtlib
open SmtInterface
open Syguslib
open SygusInterface
module S = Smtlib.SmtLib

let _NUM_POSIIVE_EXAMPLES_ = 30
let _POSITIVE_EXAMPLES_ : (int, term list) Hashtbl.t = Hashtbl.create (module Int) ~size:5

let add_positive_example (f : Variable.t) (ex : term) : unit =
  Hashtbl.add_multi ~key:f.vid ~data:ex _POSITIVE_EXAMPLES_
;;

let get_positive_examples (f : Variable.t) : term list =
  Hashtbl.find_multi _POSITIVE_EXAMPLES_ f.vid
;;

(** Generate positive examples for the input PMRS, using a SMT solver to find
  different possible outputs.
  *)
let gen_pmrs_positive_examples ~(ctx : env) (p : PMRS.t) =
  let open AsyncSmt in
  let ref_typ_out = List.last_exn p.pinput_typ in
  let reference t = ctx_reduce ctx (ctx >>- Reduce.reduce_pmrs p t) in
  let out_term = ctx >- mk_composite_base_type (get_alpha ctx) in
  let atoms = ctx >- Analysis.free_variables out_term in
  let iterations = ref 0 in
  let task (z3, _b) =
    let%lwt _ = _b in
    let%lwt () = load_min_max_defs z3 in
    let%lwt () =
      exec_all
        z3
        (List.map ~f:snd (ctx >- SmtInterface.declare_datatype_of_rtype (get_alpha ctx)))
    in
    let%lwt () = exec_all z3 (ctx >- Commands.decls_of_vars atoms) in
    let mk_ex _ t =
      let t' = reference t in
      let fv = ctx >- Analysis.free_variables t' in
      let%lwt () = spush z3 in
      let%lwt () = exec_all z3 (ctx >- Commands.decls_of_vars fv) in
      let%lwt () = smt_assert z3 (ctx >- smt_of_term (mk_bin Binop.Eq t' out_term)) in
      let resp =
        match%lwt check_sat z3 with
        | SmtLib.Sat ->
          (* SAT: get the model. *)
          let%lwt model = get_model z3 in
          let mmap = ctx >>- model_to_varmap atoms model in
          let value = ctx >- Eval.in_model mmap out_term in
          (* Add the positive example for the current function. *)
          add_positive_example p.pvar value;
          (* Pop the current stack. *)
          let%lwt () = spop z3 in
          (* Make the values forbidden in subsequent solver calls. *)
          let%lwt () =
            smt_assert
              z3
              SmtLib.(
                mk_not (mk_eq (ctx >- smt_of_term out_term) (ctx >- smt_of_term value)))
          in
          Lwt.return SmtLib.Sat
        | _ as t ->
          (* UNSAT: the loop will stop, but pop the current stack before.  *)
          let%lwt () = spop z3 in
          Lwt.return t
      in
      resp
    in
    let _ =
      ctx
      >- Expand.lwt_expand_loop
           iterations (* Stop at _NUM_POSITIVES_EXAMEPLES_ examples. *)
           ~r_stop:(fun _ -> !iterations > _NUM_POSIIVE_EXAMPLES_)
           mk_ex
           (Lwt.return
              (mk_var
                 ctx.ctx
                 (Variable.mk ctx.ctx ~t:(Some ref_typ_out) (Alpha.fresh ctx.ctx.names))))
    in
    close_solver z3
  in
  let task, r = cancellable_task (make_solver "z3") task in
  Lwt.wakeup r 1;
  let%lwt () = task in
  Lwt.return (get_positive_examples p.pvar)
;;

let set_up_bounded_solver
    ~(ctx : Context.t)
    (logic : Logics.logic)
    (vars : VarSet.t)
    (solver : AsyncSmt.solver)
    : unit Lwt.t
  =
  let preamble =
    Commands.mk_preamble
      ~incremental:(String.is_prefix ~prefix:"CVC" solver.s_name)
      ~induction:false
      ~logic
      ()
  in
  let additional_decls = Commands.decls_of_vars ~ctx vars in
  let%lwt () = AsyncSmt.exec_all solver (preamble @ additional_decls) in
  return ()
;;

let smt_of_aux_ensures ~(fctx : PMRS.Functions.ctx) ~(ctx : Context.t) ~(p : PsiDef.t)
    : S.smtTerm list
  =
  let mk_sort maybe_rtype =
    match maybe_rtype with
    | None -> S.mk_int_sort
    | Some rtype -> SmtInterface.sort_of_rtype rtype
  in
  let pmrss : PMRS.t list =
    [ p.PsiDef.reference; p.PsiDef.target; p.PsiDef.reference ]
    @
    match p.tinv with
    | None -> []
    | Some tinv -> [ tinv ]
  in
  let vars : variable list =
    List.concat
      (List.map
         ~f:(fun (pmrs : PMRS.t) ->
           List.filter
             ~f:(fun v ->
               (not Variable.(v = pmrs.pmain_symb)) && not Variable.(v = pmrs.pvar))
             (Set.elements pmrs.pnon_terminals))
         pmrss)
  in
  List.fold
    ~init:[]
    ~f:(fun acc v ->
      let maybe_ens = Specifications.get_ensures ~ctx v in
      match maybe_ens with
      | None -> acc
      | Some t ->
        let arg_types = fst (RType.fun_typ_unpack (Variable.vtype_or_new ctx v)) in
        let arg_vs =
          List.map
            ~f:(fun t -> Variable.mk ctx ~t:(Some t) (Alpha.fresh ctx.names))
            arg_types
        in
        let args = List.map ~f:(mk_var ctx) arg_vs in
        let quants =
          List.map
            ~f:(fun var -> S.SSimple var.vname, mk_sort (Variable.vtype ctx var))
            arg_vs
        in
        let ens = Reduce.reduce_term ~fctx ~ctx (mk_app t [ mk_app_v ctx v args ]) in
        let smt = S.mk_forall quants (SmtInterface.smt_of_term ~ctx ens) in
        smt :: acc)
    vars
;;

let smt_of_ensures_validity
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (ensures : term)
  =
  let mk_sort maybe_rtype =
    match maybe_rtype with
    | None -> S.mk_int_sort
    | Some rtype -> SmtInterface.sort_of_rtype rtype
  in
  let f_compose_r t =
    let repr_of_v =
      if p.PsiDef.repr_is_identity
      then t
      else Reduce.reduce_pmrs ~fctx ~ctx p.PsiDef.repr t
    in
    Reduce.reduce_term
      ~fctx
      ~ctx
      (Reduce.reduce_pmrs ~fctx ~ctx p.PsiDef.reference repr_of_v)
  in
  let t = List.last_exn p.PsiDef.repr.pinput_typ in
  let quants = [ S.SSimple "t", mk_sort (Some t) ] in
  let ensures_app =
    SmtInterface.smt_of_term
      ~ctx
      (mk_app ensures [ f_compose_r (mk_var ctx (Variable.mk ctx "t" ~t:(Some t))) ])
  in
  [ S.mk_assert (S.mk_not (S.mk_forall quants ensures_app)) ]
;;

let set_up_to_get_ensures_model
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    solver
    ~(p : PsiDef.t)
    (ensures : term)
  =
  let t = List.last_exn p.PsiDef.repr.pinput_typ in
  let var = Variable.mk ctx "t" ~t:(Some t) in
  let f_compose_r t =
    let repr_of_v =
      if p.PsiDef.repr_is_identity
      then t
      else Reduce.reduce_pmrs ~ctx ~fctx p.PsiDef.repr t
    in
    Reduce.reduce_term
      ~ctx
      ~fctx
      (Reduce.reduce_pmrs ~fctx ~ctx p.PsiDef.reference repr_of_v)
  in
  let%lwt () =
    SmtInterface.(
      AsyncSmt.exec_all solver (Commands.decls_of_vars ~ctx (VarSet.singleton var)))
  in
  let ensures_app =
    SmtInterface.smt_of_term
      ~ctx
      (mk_app ensures [ f_compose_r (mk_var ctx (Variable.mk ctx "t" ~t:(Some t))) ])
  in
  SmtInterface.AsyncSmt.exec_command solver (S.mk_assert (S.mk_not ensures_app))
;;

let handle_ensures_synth_response
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    ((task, resolver) : Sygus.solver_response option Lwt.t * int Lwt.u)
    (var : variable)
    : [ `Solution of (string * string list * term) list | `Infeasible | `Failure ] Lwt.t
  =
  let parse_synth_fun (fname, _fargs, _, fbody) =
    let body, _ =
      infer_type
        ctx
        (term_of_sygus ~fctx ~ctx (VarSet.to_env (VarSet.of_list [ var ])) fbody)
    in
    fname, [], body
  in
  match%lwt
    Lwt.wakeup resolver 0;
    task
  with
  | Some (RSuccess resps) ->
    let soln = List.map ~f:parse_synth_fun resps in
    Lwt.return (`Solution soln)
  | Some RInfeasible -> Lwt.return `Infeasible
  | Some RFail | Some RUnknown | None -> Lwt.return `Failure
;;

let make_ensures_name (id : int) = "ensures_" ^ Int.to_string id

let synthfun_ensures ~(ctx : Context.t) ~(p : PsiDef.t) (id : int)
    : Sygus.command * variable * string
  =
  let var =
    Variable.mk ctx ~t:(Some p.PsiDef.reference.poutput_typ) (Alpha.fresh ctx.names)
  in
  let opset =
    List.fold
      ~init:OpSet.empty
      ~f:(fun acc func -> Set.union acc (Analysis.operators_of func.f_body))
      (PMRS.func_of_pmrs ~ctx p.PsiDef.reference
      @ PMRS.func_of_pmrs ~ctx p.PsiDef.repr
      @
      match p.tinv with
      | None -> []
      | Some pmrs -> PMRS.func_of_pmrs ~ctx pmrs)
  in
  (* OpSet.of_list [ Binary Binop.Mod ] in *)
  let grammar =
    Grammars.generate_grammar ~ctx ~guess:None ~bools:true opset [ var ] RType.TBool
  in
  let logic = dt_extend_base_logic (logic_of_operators opset) in
  mk_synthinv ~ctx (make_ensures_name id) [ var ] grammar, var, logic
;;

let set_up_ensures_solver
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    solver
    ~(p : PsiDef.t)
    (ensures : term)
    : unit Lwt.t
  =
  let open SmtInterface.AsyncSmt in
  ignore ensures;
  let loc_smt = smt_of_pmrs ~ctx ~fctx in
  let preamble = Commands.mk_preamble ~logic:Logics.ALL ~induction:true ~models:true () in
  let%lwt () = exec_all solver preamble in
  let%lwt () =
    Lwt_list.iter_p
      (fun x ->
        let%lwt _ = exec_command solver x in
        return ())
      ((match p.tinv with
       | None -> []
       | Some tinv -> loc_smt tinv)
      @ (if p.PsiDef.repr_is_identity
        then loc_smt p.PsiDef.reference
        else loc_smt p.PsiDef.reference @ loc_smt p.PsiDef.repr)
      (* Assert invariants on functions *)
      @ List.map ~f:S.mk_assert (smt_of_aux_ensures ~fctx ~ctx ~p))
  in
  return ()
;;

let verify_ensures_inductive
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (ensures : term)
    : SmtInterface.AsyncSmt.response * int Lwt.u
  =
  let build_task (cvc4_instance, task_start) =
    let%lwt _ = task_start in
    let%lwt () = set_up_ensures_solver ~fctx ~ctx cvc4_instance ~p ensures in
    let%lwt () =
      (Lwt_list.iter_p (fun x ->
           let%lwt _ = SmtInterface.AsyncSmt.exec_command cvc4_instance x in
           return ()))
        (smt_of_ensures_validity ~fctx ~ctx ~p ensures)
    in
    let%lwt resp = SmtInterface.AsyncSmt.check_sat cvc4_instance in
    let%lwt final_response =
      match resp with
      | Sat | Unknown ->
        let%lwt _ = set_up_to_get_ensures_model ~fctx ~ctx cvc4_instance ~p ensures in
        let%lwt resp' = SmtInterface.AsyncSmt.check_sat cvc4_instance in
        (match resp' with
        | Sat | Unknown -> SmtInterface.AsyncSmt.get_model cvc4_instance
        | _ -> return resp')
      | _ -> return resp
    in
    let%lwt () = SmtInterface.AsyncSmt.close_solver cvc4_instance in
    Log.debug_msg "Unbounded ensures verification is complete.";
    return final_response
  in
  SmtInterface.AsyncSmt.(
    cancellable_task (SmtInterface.AsyncSmt.make_solver "cvc") build_task)
;;

let verify_ensures_bounded
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (ensures : term)
    (var : variable)
    : SmtInterface.AsyncSmt.response * int Lwt.u
  =
  let open SmtInterface.AsyncSmt in
  let loc_red = Reduce.reduce_term ~fctx ~ctx in
  let loc_pmrs_red = Reduce.reduce_pmrs ~fctx ~ctx in
  let base_term =
    mk_var ctx (Variable.mk ctx "t" ~t:(Some (List.last_exn p.PsiDef.repr.pinput_typ)))
  in
  let task (solver, starter) =
    let%lwt _ = starter in
    let%lwt _ = set_up_bounded_solver ~ctx Logics.ALL VarSet.empty solver in
    let steps = ref 0 in
    let rec check_bounded_sol accum terms =
      let f accum t =
        let%lwt _ = accum in
        let rec_instantation =
          Option.value ~default:VarMap.empty (Matching.matches ~ctx t ~pattern:base_term)
        in
        let f_compose_r t =
          let repr_of_v =
            if p.PsiDef.repr_is_identity then t else loc_pmrs_red p.PsiDef.repr t
          in
          loc_red (loc_pmrs_red p.PsiDef.reference repr_of_v)
        in
        let%lwt _ =
          exec_all solver (Commands.decls_of_vars ~ctx (VarSet.singleton var))
        in
        let%lwt () = spush solver in
        let%lwt _ =
          exec_all
            solver
            (Commands.decls_of_vars
               ~ctx
               (VarSet.of_list
                  (List.concat_map
                     ~f:(fun t -> Set.to_list (Analysis.free_variables ~ctx t))
                     (Map.data rec_instantation))))
        in
        let instance_equals =
          let t' = loc_red (f_compose_r t) in
          smt_of_term ~ctx (loc_red Terms.(mk_var ctx var == t'))
        in
        let%lwt _ = SmtInterface.AsyncSmt.smt_assert solver instance_equals in
        (* Assert that TInv is true for this concrete term t *)
        let%lwt _ =
          match p.tinv with
          | None -> return ()
          | Some tinv ->
            let tinv_t = loc_pmrs_red tinv t in
            let%lwt _ =
              exec_all
                solver
                (Commands.decls_of_vars ~ctx (Analysis.free_variables ~ctx tinv_t))
            in
            let%lwt _ = smt_assert solver (smt_of_term ~ctx tinv_t) in
            return ()
        in
        (* Assert that ensures is false for this concrete term t  *)
        let ensures_reduc = loc_red (mk_app ensures [ loc_red (f_compose_r t) ]) in
        (* let%lwt _ =
             SmtInterface.AsyncSmt.exec_all solver
               (SmtInterface.decls_of_vars (Analysis.free_variables ensures_reduc))
           in *)
        let%lwt _ =
          exec_command solver (S.mk_assert (S.mk_not (smt_of_term ~ctx ensures_reduc)))
        in
        let%lwt resp = check_sat solver in
        (* Note that I am getting a model after check-sat unknown response. This may not halt.  *)
        let%lwt result =
          match resp with
          | SmtLib.Sat | SmtLib.Unknown ->
            let%lwt model = get_model solver in
            return (Some model)
          | _ -> return None
        in
        let%lwt () = spop solver in
        return (resp, result)
      in
      match terms with
      | [] -> accum
      | t0 :: tl ->
        let%lwt accum' = f accum t0 in
        (match accum' with
        | status, Some model -> return (status, Some model)
        | _ -> check_bounded_sol (return accum') tl)
    in
    let rec expand_loop u =
      match Set.min_elt u, !steps < !Config.Optims.num_expansions_check with
      | Some t0, true ->
        let tset, u' = Expand.simple ~ctx t0 in
        let%lwt check_result =
          check_bounded_sol (return (SmtLib.Unknown, None)) (Set.elements tset)
        in
        steps := !steps + Set.length tset;
        (match check_result with
        | _, Some model ->
          Log.debug_msg
            "Bounded ensures verification has found a counterexample to the ensures \
             candidate.";
          return model
        | _ -> expand_loop (Set.union (Set.remove u t0) u'))
      | None, true ->
        (* All expansions have been checked. *)
        return SmtLib.Unsat
      | _, false ->
        (* Check reached limit. *)
        Log.debug_msg "Bounded ensures verification has reached limit.";
        if !Config.bounded_lemma_check then return SmtLib.Unsat else return SmtLib.Unknown
    in
    let%lwt res = expand_loop (TermSet.singleton base_term) in
    let%lwt () = close_solver solver in
    return res
  in
  cancellable_task (make_solver "cvc") task
;;

let verify_ensures_candidate
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    ~(p : PsiDef.t)
    (maybe_ensures : term option)
    (var : variable)
    : S.solver_response Lwt.t
  =
  match maybe_ensures with
  | None -> failwith "Cannot verify ensures candidate; there is none."
  | Some ensures ->
    Log.verbose (fun f () -> Fmt.(pf f "Checking ensures candidate..."));
    let resp =
      try
        let pr1, resolver1 = verify_ensures_bounded ~fctx ~ctx ~p ensures var in
        let pr2, resolver2 = verify_ensures_inductive ~fctx ~ctx ~p ensures in
        Lwt.wakeup resolver2 1;
        Lwt.wakeup resolver1 1;
        (* The first call to return is kept, the other one is ignored. *)
        Lwt.pick [ pr1; pr2 ]
      with
      | End_of_file ->
        Log.error_msg "Solvers terminated unexpectedly  ⚠️ .";
        Log.error_msg "Please inspect logs.";
        Lwt.return SmtLib.Unknown
    in
    resp
;;

let handle_ensures_verif_response
    ~(ctx : Context.t)
    (response : S.solver_response Lwt.t)
    (ensures : term)
    : (bool * Sexp.t list option) Lwt.t
  =
  match%lwt response with
  | Unsat ->
    Log.verbose (fun f () -> Fmt.(pf f "This ensures has been proven correct."));
    Log.verbose (fun frmt () -> Fmt.pf frmt "Ensures is %a" (pp_term ctx) ensures);
    Lwt.return (true, None)
  | SmtLib.SExps x ->
    Log.verbose (fun f () ->
        Fmt.(pf f "This ensures has not been proven correct. Refining ensures..."));
    Lwt.return (false, Some x)
  | Sat ->
    Log.error_msg "Ensures verification returned Sat, which was not expected.";
    Lwt.return (false, None)
  | Unknown ->
    Log.error_msg "Ensures verification returned Unknown.";
    Lwt.return (false, None)
  | _ ->
    Log.error_msg "Ensures verification is indeterminate.";
    Lwt.return (false, None)
;;

let constraint_of_neg ~(ctx : Context.t) (id : int) ~(p : PsiDef.t) (witness : witness)
    : term
  =
  ignore p;
  let conjs =
    List.map
      ~f:(fun (_, elimv) ->
        mk_un
          Unop.Not
          (mk_app
             (mk_var ctx (Variable.mk ctx (make_ensures_name id)))
             [ Eval.in_model ~ctx witness.witness_model elimv ]))
      witness.witness_eqn.eelim
  in
  match mk_assoc Binop.And conjs with
  | Some _constraint -> _constraint
  | None -> Terms.bool true
;;

let constraint_of_pos ~(ctx : Context.t) (id : int) (term : term) : term =
  mk_app (mk_var ctx (Variable.mk ctx (make_ensures_name id))) [ term ]
;;

let rec synthesize
    ~(ctx : env)
    ~(p : PsiDef.t)
    (positives : witness list)
    (negatives : witness list)
    (prev_positives : term list)
    : term option Lwt.t
  =
  Log.(info (wrap "Synthesize predicates.."));
  ctx >- AlgoLog.violates_ensures p negatives;
  let%lwt new_positives =
    match prev_positives with
    | [] -> gen_pmrs_positive_examples ~ctx p.PsiDef.reference
    | _ -> Lwt.return prev_positives
  in
  ctx >- AlgoLog.positives_ensures p new_positives;
  let id = 0 in
  let synth_objs, var, _logic = ctx >- synthfun_ensures ~p id in
  let extra_defs = Semantic.[ max_definition; min_definition ] in
  let solver =
    HLSolver.(
      make ~extra_defs ()
      |> set_logic _logic
      |> synthesize [ synth_objs ]
      |> (ctx
         >- constrain
              (List.map ~f:(ctx >- constraint_of_neg id ~p) negatives
              @ List.map ~f:(ctx >- constraint_of_pos id) new_positives)))
  in
  match%lwt
    ctx
    >>- handle_ensures_synth_response
          (ctx
          >- HLSolver.solve ~timeout:(Some !Config.Optims.wait_parallel_tlimit) solver)
          var
  with
  | `Infeasible ->
    (* TODO check logic *)
    Lwt.return (Some (Terms.bool true))
  | `Failure -> Lwt.return None
  | `Solution solns ->
    let _, _, body = List.nth_exn solns 0 in
    let ensures = mk_fun ctx.ctx [ FPatVar var ] (Eval.simplify body) in
    Log.debug_msg Fmt.(str "Ensures candidate is %a." (pp_term ctx.ctx) ensures);
    let var =
      Variable.mk
        ctx.ctx
        ~t:(Some p.PsiDef.reference.poutput_typ)
        (Alpha.fresh ctx.ctx.names)
    in
    (match%lwt
       ctx
       >- handle_ensures_verif_response
            (ctx >>- verify_ensures_candidate ~p (Some ensures) var)
            ensures
     with
    | true, _ -> Lwt.return (Some ensures)
    | false, Some sexprs ->
      let result =
        Map.find (ctx >>- SmtInterface.model_to_constmap (SmtLib.SExps sexprs)) var.vname
      in
      (match result with
      | None ->
        Log.debug_msg "No model found; cannot refine ensures.";
        Lwt.return None
      | Some r ->
        Log.debug_msg
          Fmt.(
            str "The counterexample to the ensures candidate is %a" (pp_term ctx.ctx) r);
        synthesize ~ctx ~p positives negatives (r :: new_positives))
    | false, _ -> Lwt.return None)
;;
