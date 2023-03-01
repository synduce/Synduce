open Common
open ProblemDefs
open Base
open Env
open Lang
open Lang.Term
open Projection
open SmtInterface
open Smtlib
open Utils
open Lwt.Syntax

(* ============================================================================================= *)
(*                               CHECKING UNREALIZABILITY                                        *)
(* ============================================================================================= *)

let smt_unsatisfiability_check
    ~(ctx : Context.t)
    (unknowns : VarSet.t)
    (eqns : equation list)
    : unit Lwt.t
  =
  let free_vars =
    let f fvs eqn =
      let precond, lhs, rhs = eqn.eprecond, eqn.elhs, eqn.erhs in
      let set' =
        VarSet.union_list
          [ Analysis.free_variables ~ctx lhs
          ; Analysis.free_variables ~ctx rhs
          ; Option.value_map
              precond
              ~f:(Analysis.free_variables ~ctx)
              ~default:VarSet.empty
          ]
      in
      Set.union fvs set'
    in
    let fvs = List.fold eqns ~f ~init:VarSet.empty in
    Set.diff fvs unknowns
  in
  let constraint_of_eqns, terms_for_logic_deduction =
    let eqns_constraints =
      let f eqn =
        let lhs = smt_of_term ~ctx eqn.elhs in
        let rhs = smt_of_term ~ctx eqn.erhs in
        let pre = Option.map ~f:(smt_of_term ~ctx) eqn.eprecond in
        match pre with
        | Some precondition -> SmtLib.(mk_or (mk_not precondition) (mk_eq lhs rhs))
        | None -> SmtLib.mk_eq lhs rhs
      in
      List.map ~f eqns
    in
    ( SmtLib.(
        mk_forall (sorted_vars_of_vars ~ctx free_vars) (mk_assoc_and eqns_constraints))
    , List.map ~f:(fun eqn -> Terms.(eqn.elhs == eqn.erhs)) eqns )
  in
  let task (solver, binder) =
    let open AsyncSmt in
    let* _ = binder in
    let preamble =
      Commands.mk_preamble
        ~logic:
          (SmtLogic.infer_logic
             ~ctx
             ~with_uninterpreted_functions:true
             ~for_induction:false
             ~quantifier_free:false
             terms_for_logic_deduction)
        ()
    in
    let* () = exec_all solver (preamble @ Commands.decls_of_vars ~ctx unknowns) in
    let* () = smt_assert solver constraint_of_eqns in
    let* resp = check_sat solver in
    (match resp with
    | Unsat ->
      Log.debug
        Fmt.(
          fun fmt () ->
            pf
              fmt
              "Z3 query answer for unsatisfiability of synthesis problem: %a"
              SmtLib.pp_solver_response
              Unsat)
    | x ->
      Log.error
        Fmt.(
          fun fmt () ->
            pf fmt "Z3 unsat check failed with %a@." SmtLib.pp_solver_response x));
    close_solver solver
  in
  let p, r =
    AsyncSmt.(
      cancellable_task
        (make_solver ~hint:"smt-unsat-check " !Config.verification_solver)
        task)
  in
  Lwt.wakeup r 1;
  p
;;

let merge_all (cl : unrealizability_witness list) : unrealizability_witness list =
  let same_origin orig other =
    Terms.equal other.ci.witness_eqn.eterm orig.ci.witness_eqn.eterm
    && Terms.equal other.cj.witness_eqn.eterm orig.cj.witness_eqn.eterm
  in
  let complete_model (cm : witness) (other_m : witness) =
    Map.fold other_m.witness_model ~init:cm ~f:(fun ~key:v_of_other ~data cm ->
        if Set.mem cm.witness_vars v_of_other && Map.mem cm.witness_model v_of_other
        then cm
        else
          { cm with
            witness_vars = Set.add cm.witness_vars v_of_other
          ; witness_model = Map.set cm.witness_model ~key:v_of_other ~data
          })
  in
  let merge_in hd sames =
    let f cex other =
      { cex with
        ci = complete_model cex.ci other.ci
      ; cj = complete_model cex.cj other.cj
      }
    in
    List.fold ~f ~init:hd sames
  in
  List.map cl ~f:(fun cex -> merge_in cex (List.filter ~f:(same_origin cex) cl))
;;

let reinterpret_model ~ctx vseti (m0i, m0j') var_subst =
  let mi, mj =
    List.fold var_subst ~init:(VarMap.empty, VarMap.empty) ~f:(fun (mi, mj) (v, v') ->
        let primed_name = v'.vname in
        Variable.free ctx v';
        match Map.find m0i v.vname with
        | Some data ->
          let new_m = Map.set mi ~key:v ~data in
          (match Map.find m0j' primed_name with
          | Some data -> new_m, Map.set mj ~key:v ~data
          | None -> new_m, mj)
        | None ->
          (match Map.find m0j' primed_name with
          | Some data -> mi, Map.set mj ~key:v ~data
          | None -> mi, mj))
  in
  let mi =
    Map.fold m0i ~init:mi ~f:(fun ~key:vname ~data:v_val m ->
        if VarMap.assigns_varname m vname
        then m
        else (
          match VarSet.find_by_name vseti vname with
          | Some vi -> Map.set m ~key:vi ~data:v_val
          | None -> m))
  in
  mi, mj
;;

let unrealizability_witness_of_constmap
    ~ctx
    (i, j)
    (eqn_i, eqn_j)
    (vseti, vsetj)
    var_subst
    model
  =
  let m0i, m0j =
    Map.partitioni_tf
      ~f:(fun ~key ~data:_ -> Option.is_some (VarSet.find_by_name vseti key))
      model
  in
  (* Remap the names to ids of the original variables in m' *)
  let m_i, m_j = reinterpret_model ~ctx vseti (m0i, m0j) var_subst in
  let vset = Set.union vseti vsetj in
  let witness_i : witness =
    { witness_eqn = eqn_i
    ; witness_model = m_i
    ; witness_vars = vset
    ; witness_stat = Unknown
    }
  in
  let witness_j : witness =
    { witness_eqn = eqn_j
    ; witness_model = m_j
    ; witness_vars = vset
    ; witness_stat = Unknown
    }
  in
  { i; j; ci = witness_i; cj = witness_j }
;;

let skeleton_match ~ctx ~unknowns (e1 : term) (e2 : term) : (term * term) list option =
  let args1, e1' = Matching.skeletize ~functions:unknowns ~ctx e1
  and args2, e2' = Matching.skeletize ~functions:unknowns ~ctx e2 in
  match Matching.matches ~ctx ~boundvars:unknowns ~pattern:e1' e2' with
  | Some subs ->
    let f (v1, packedv) =
      match packedv.tkind with
      | TVar v2 -> Option.both (Map.find args1 v1) (Map.find args2 v2)
      | _ -> None
    in
    all_or_none (List.map ~f (Map.to_alist subs))
  | None -> None
;;

let components_of_unrealizability ~ctx ~unknowns (eqn1 : equation) (eqn2 : equation)
    : ((term * term) list * (term * term)) option
  =
  let validate terms =
    List.for_all terms ~f:(fun (t, t') ->
        Set.is_empty (Set.inter (Analysis.free_variables ~ctx t) unknowns)
        && Set.is_empty (Set.inter (Analysis.free_variables ~ctx t') unknowns))
  in
  match skeleton_match ~ctx ~unknowns eqn1.erhs eqn2.erhs with
  | Some args_1_2 ->
    if validate ((eqn1.elhs, eqn2.elhs) :: args_1_2)
    then Some (args_1_2, (eqn1.elhs, eqn2.elhs))
    else None
  | None ->
    if Terms.equal eqn1.erhs eqn2.erhs
       && validate [ eqn1.erhs, eqn2.erhs; eqn1.elhs, eqn2.elhs ]
    then Some ([], (eqn1.elhs, eqn2.erhs))
    else None
;;

let gen_info ~ctx (eqn_i, eqn_j) unknowns =
  let fv e =
    VarSet.union_list
      (List.map e.eelim ~f:(fun (_, elim) -> Analysis.free_variables ~ctx elim)
      @ [ Set.filter
            ~f:(fun v -> RType.is_base (Variable.vtype_or_new ctx v))
            (Analysis.free_variables ~ctx e.eterm)
        ]
      @ [ Analysis.free_variables ~ctx e.elhs
        ; Analysis.free_variables ~ctx e.erhs
        ; Option.value_map
            ~default:VarSet.empty
            ~f:(Analysis.free_variables ~ctx)
            e.eprecond
        ])
  in
  let vseti = Set.diff (fv eqn_i) unknowns
  and vsetj = Set.diff (fv eqn_j) unknowns in
  let var_subst = VarSet.prime ctx vsetj in
  let vsetj' = VarSet.of_list (List.map ~f:snd var_subst) in
  let sub = List.map ~f:(fun (v, v') -> mk_var ctx v, mk_var ctx v') var_subst in
  vseti, vsetj, vsetj', sub, var_subst
;;

(** Check if system of equations defines a functionally realizable synthesis problem.
  If any equation defines an unsolvable problem, an unrealizability_witness is added to the
  list of counterexamples to be returned.
  If the returned list is empty, the problem may be solvable/realizable.
  If the returned list is not empty, the problem is not solvable / unrealizable.
*)
let check_unrealizable
    ~(fctx : PMRS.Functions.ctx)
    ~(ctx : Context.t)
    (unknowns : VarSet.t)
    (eqns : equation_system)
    : unrealizability_witness list Lwt.t * int Lwt.u
  =
  Log.debug (fun f () -> Fmt.(pf f "Checking unrealizability..."));
  let start_time = Unix.gettimeofday () in
  let task (solver, binder) =
    let open AsyncSmt in
    let* _ = binder in
    let* () = set_logic solver Logics.ALL in
    let* () = set_option solver "produce-models" "true" in
    let* () = load_min_max_defs solver in
    (* Main part of the check, applied to each equation in eqns. *)
    let check_eqn_accum (witnesses : unrealizability_witness list) ((i, eqn_i), (j, eqn_j))
      =
      let vseti, vsetj, vsetj', sub, var_subst = gen_info ~ctx (eqn_i, eqn_j) unknowns in
      (* Extract the arguments of the rhs, if it is a proper skeleton. *)
      match components_of_unrealizability ~ctx ~unknowns eqn_i eqn_j with
      | None ->
        Lwt.return witnesses (* If we cannot match the expected structure, skip it. *)
      | Some (rhs_args_ij, (lhs_i, lhs_j)) ->
        let lhs_diff =
          let projs = projection_eqns lhs_i (substitution sub lhs_j) in
          List.map ~f:(fun (ei, ej) -> Terms.(not (ei == ej))) projs
        in
        (* (push). *)
        let* () = spush solver in
        (* Declare the variables. *)
        let* () =
          exec_all solver (Commands.decls_of_vars ~ctx (Set.union vseti vsetj'))
        in
        (* Assert preconditions, if they exist. *)
        let* () =
          match eqn_i.eprecond with
          | Some pre_i ->
            let* () =
              exec_all
                solver
                (Commands.decls_of_vars ~ctx (Analysis.free_variables ~ctx pre_i))
            in
            AsyncSmt.smt_assert solver (smt_of_term ~ctx pre_i)
          | None -> Lwt.return ()
        in
        (* Assert the precondition for j *)
        let* () =
          match eqn_j.eprecond with
          | Some pre_j ->
            let* () =
              exec_all
                solver
                (Commands.decls_of_vars ~ctx (Analysis.free_variables ~ctx pre_j))
            in
            smt_assert solver (smt_of_term ~ctx (substitution sub pre_j))
          | None -> Lwt.return ()
        in
        (* Assert that the lhs of i and j must be different. **)
        let* () =
          Lwt_list.iter_s
            (fun eqn -> AsyncSmt.smt_assert solver (smt_of_term ~ctx eqn))
            lhs_diff
        in
        (* Assert that the rhs must be equal. *)
        let* () =
          Lwt_list.iter_s
            (fun (rhs_i_arg_term, rhs_j_arg_term) ->
              let rhs_eqs =
                projection_eqns rhs_i_arg_term (substitution sub rhs_j_arg_term)
              in
              Lwt_list.iter_s
                (fun (lhs, rhs) ->
                  AsyncSmt.smt_assert solver (smt_of_term ~ctx Terms.(lhs == rhs)))
                rhs_eqs)
            rhs_args_ij
        in
        (* Check sat and get model. *)
        let* new_witnesses =
          let* resp = check_sat solver in
          match resp with
          | Sat ->
            let* model_sexps = get_model solver in
            (match model_sexps with
            | SExps s ->
              let model = model_to_constmap ~ctx ~fctx (SExps s) in
              (* Search for a few additional models. *)
              let* other_models =
                if !Config.Optims.fuzzing_count > 0
                then
                  request_different_models_async
                    ~ctx
                    ~fctx
                    (Lwt.return model)
                    !Config.Optims.fuzzing_count
                    solver
                else Lwt.return []
              in
              let new_witnesss =
                List.map
                  ~f:
                    (unrealizability_witness_of_constmap
                       ~ctx
                       (i, j)
                       (eqn_i, eqn_j)
                       (vseti, vsetj)
                       var_subst)
                  (model :: other_models)
              in
              Lwt.return (new_witnesss @ witnesses)
            | _ -> Lwt.return witnesses)
          | _ -> Lwt.return witnesses
        in
        let+ () = spop solver in
        new_witnesses
    in
    let* new_witnesses =
      Lwt.map
        merge_all
        (Lwt_list.fold_left_s
           check_eqn_accum
           []
           (List.mapi ~f:(fun i eqn -> i, eqn) eqns |> combinations))
    in
    let+ () = close_solver solver in
    let elapsed = Unix.gettimeofday () -. start_time in
    Log.debug (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
    AlgoLog.show_unrealizability_witnesses ~ctx unknowns eqns new_witnesses;
    new_witnesses
  in
  AsyncSmt.(cancellable_task (make_solver !Config.verification_solver) task)
;;

(* ============================================================================================= *)
(*                            CLASSIFYING SPURIOUS COUNTEREXAMPLES                               *)
(* ============================================================================================= *)

let add_cause (ctx : witness_stat) (cause : spurious_cause) =
  match ctx with
  | Valid -> Spurious [ cause ]
  | Spurious c ->
    if not (Caml.List.mem cause c) then Spurious (cause :: c) else Spurious c
  | Unknown -> Spurious [ cause ]
;;

(*               CLASSIFYING SPURIOUS COUNTEREXAMPLES - NOT IN REFERENCE IMAGE                   *)

(**
    Check that some witness is in the image of the function using bounded checking.
    *)
let check_image_sat ~(ctx : env) ~(p : PsiDef.t) witness
    : (SmtLib.solver_response * Stats.verif_method) Lwt.t * int Lwt.u
  =
  let f_compose_r t =
    let repr_of_v =
      if p.PsiDef.repr_is_identity then t else ctx >>- Reduce.reduce_pmrs p.PsiDef.repr t
    in
    ctx_reduce ctx (ctx >>- Reduce.reduce_pmrs p.PsiDef.reference repr_of_v)
  in
  (* Asynchronous solver task. *)
  let build_task (solver_instance, task_start) =
    let steps = ref 0 in
    (* A single check for a bounded term. *)
    let t_check accum t =
      (* Build equations of the form (f(r(t)) != (value of elimination var in model) *)
      let term_eqs =
        List.map witness.witness_eqn.eelim ~f:(fun (_, elimv) ->
            Terms.(f_compose_r t == (ctx >- Eval.in_model witness.witness_model elimv)))
      in
      let rec aux accum tlist =
        let f binder eqn =
          let* _ = binder in
          let* () = AsyncSmt.spush solver_instance in
          let* () =
            AsyncSmt.exec_all
              solver_instance
              (ctx >- Commands.decls_of_vars (ctx >- Analysis.free_variables eqn))
          in
          let* () = AsyncSmt.smt_assert solver_instance (ctx >- smt_of_term eqn) in
          let* res = AsyncSmt.check_sat solver_instance in
          let* () = AsyncSmt.spop solver_instance in
          Lwt.return res
        in
        match tlist with
        | [] -> accum
        | t0 :: tl ->
          let* accum' = f accum t0 in
          (match accum' with
          | Unsat -> Lwt.return SmtLib.Unsat
          | _ -> aux (Lwt.return accum') tl)
      in
      aux accum term_eqs
    in
    let t_decl =
      List.map ~f:snd (ctx >- Lang.SmtInterface.declare_datatype_of_rtype (get_alpha ctx))
    in
    let* _ = task_start in
    let* () =
      AsyncSmt.exec_all
        solver_instance
        (Commands.mk_preamble
           ~incremental:(String.is_prefix ~prefix:"CVC" solver_instance.s_name)
           ~logic:Logics.ALL (* TODO: fix this, we have datatypes leaking. *)
           ())
    in
    let* () = AsyncSmt.exec_all solver_instance t_decl in
    (* Run the bounded checking loop. *)
    let* res =
      let x =
        mk_var
          ctx.ctx
          (Variable.mk ~t:(Some (List.hd_exn p.target.pinput_typ)) ctx.ctx "_x")
      in
      ctx >- Expand.lwt_expand_loop steps t_check (Lwt.return x)
    in
    let* () = AsyncSmt.close_solver solver_instance in
    Lwt.return (res, Stats.BoundedChecking)
  in
  AsyncSmt.(cancellable_task (make_solver !Config.verification_solver) build_task)
;;

let check_image_unsat ~(ctx : env) ~(p : PsiDef.t) witness
    : (SmtLib.solver_response * Stats.verif_method) Lwt.t * int Lwt.u
  =
  let ref_tin = PMRS.extract_rec_input_typ p.reference in
  let f_compose_r t =
    let repr_of_v =
      if p.PsiDef.repr_is_identity
      then t
      else (
        match RType.unify_one ref_tin (type_of (Terms.typed ctx.ctx t)) with
        | Ok _ -> t
        | _ -> mk_app_v ctx.ctx p.PsiDef.repr.pvar [ t ])
    in
    (* Don't add the PMRS pargs in the application, they are declared "globally" in the problem. *)
    mk_app_v ctx.ctx p.PsiDef.reference.pvar [ repr_of_v ]
  in
  let build_task (solver, task_start) =
    let* _ = task_start in
    let* () =
      AsyncSmt.exec_all
        solver
        (Commands.mk_preamble
           ~induction:true
           ~incremental:false
           ~logic:
             (ctx
             >- SmtLogic.infer_logic
                  ~quantifier_free:false
                  ~for_induction:true
                  ~with_uninterpreted_functions:true
                  ~logic_infos:(Common.ProblemDefs.PsiDef.logics p)
                  [])
           ())
    in
    (* Declare Tinv, repr and reference functions. *)
    let* () =
      Lwt_list.iter_s
        (fun x ->
          let* _ = AsyncSmt.exec_command solver x in
          Lwt.return ())
        ((ctx >>- smt_of_pmrs p.PsiDef.reference)
        @ if p.PsiDef.repr_is_identity then [] else ctx >>- smt_of_pmrs p.PsiDef.repr)
    in
    let fv, formula =
      (* Substitute recursion elimination by recursive calls. *)
      let eqns =
        List.concat_map
          ~f:(fun (orig_rec_var, elimv) ->
            match elimv.tkind with
            | TTup comps ->
              List.mapi comps ~f:(fun i t ->
                  Terms.(
                    ctx
                    >- Eval.in_model witness.witness_model t
                    == mk_sel ctx.ctx (f_compose_r orig_rec_var) i))
            | _ ->
              Terms.
                [ ctx
                  >- Eval.in_model witness.witness_model elimv
                  == f_compose_r orig_rec_var
                ])
          witness.witness_eqn.eelim
      in
      ( VarSet.union_list
          (List.map witness.witness_eqn.eelim ~f:(fun (x, _) ->
               ctx >- Analysis.free_variables x))
      , (* Assert that the variables have the values assigned by the model. *)
        SmtLib.mk_assoc_and (List.map ~f:(ctx >- smt_of_term) eqns) )
    in
    let* _ =
      AsyncSmt.smt_assert
        solver
        (SmtLib.mk_exists (ctx >- sorted_vars_of_vars fv) formula)
    in
    let* resp = AsyncSmt.check_sat solver in
    let* () = AsyncSmt.close_solver solver in
    Lwt.return (resp, Stats.Induction)
  in
  AsyncSmt.(cancellable_task (AsyncSmt.make_solver "cvc") build_task)
;;

(**
  [check_witness_in_image ~p witness] checks whether the recursion elimination's variables values in the
  model of [witness] are in the image of (p.PsiDef.reference o p.PsiDef.repr).
*)
let check_witness_in_image
    ?(ignore_unknown = false)
    ~(ctx : env)
    ~(p : PsiDef.t)
    (witness : witness)
    : witness Lwt.t
  =
  Log.verbose_msg
    Fmt.(
      str
        "Checking whether witness is in the image of %s..."
        p.PsiDef.reference.pvar.vname);
  (* Parallel calls to induction solver and bounded checking procedure. *)
  let parallel_solver_calls () =
    let task_counter = ref 2 in
    let%lwt resp, vmethod =
      if ctx >- Analysis.is_bounded witness.witness_eqn.eterm
      then Lwt.return (SmtLib.Sat, Stats.Induction)
      else (
        try
          (* This call is expected to respond "unsat" when terminating. *)
          let pr1, resolver1 = check_image_sat ~ctx ~p witness in
          let pr1 = wait_on_failure task_counter pr1 in
          (* This call is expected to respond "sat" when terminating. *)
          let pr2, resolver2 = check_image_unsat ~ctx ~p witness in
          let pr2 = wait_on_failure task_counter pr2 in
          Lwt.wakeup resolver2 1;
          Lwt.wakeup resolver1 1;
          (* The first call to return is kept, the other one is ignored. *)
          Lwt.pick [ pr1; pr2 ]
        with
        | End_of_file ->
          Log.error_msg "Solvers terminated unexpectedly  ⚠️ .";
          Log.error_msg "Please inspect logs.";
          Lwt.return (SmtLib.Unknown, Stats.Induction))
    in
    ctx >- AlgoLog.image_witness_class p witness resp vmethod;
    match resp with
    | Sat -> Lwt.return { witness with witness_stat = Valid }
    | Unsat ->
      Lwt.return
        { witness with witness_stat = add_cause witness.witness_stat NotInReferenceImage }
    | _ ->
      Lwt.return
        (if ignore_unknown then witness else { witness with witness_stat = Unknown })
  in
  (* Onyl call bounded checking procedure. Saves calling a solver. *)
  let only_bounded_call () =
    let%lwt resp, vmethod =
      if ctx >- Analysis.is_bounded witness.witness_eqn.eterm
      then Lwt.return (SmtLib.Sat, Stats.BoundedChecking)
      else (
        try
          (* This call is expected to respond "unsat" when terminating. *)
          let pr1, resolver1 = check_image_sat ~ctx ~p witness in
          Lwt.wakeup resolver1 1;
          (* The first call to return is kept, the other one is ignored. *)
          pr1
        with
        | End_of_file ->
          Log.error_msg "Solvers terminated unexpectedly  ⚠️ .";
          Log.error_msg "Please inspect logs.";
          Lwt.return (SmtLib.Unknown, Stats.BoundedChecking))
    in
    ctx >- AlgoLog.image_witness_class p witness resp vmethod;
    match resp with
    | Sat -> Lwt.return { witness with witness_stat = Valid }
    | Unsat ->
      Lwt.return
        { witness with witness_stat = add_cause witness.witness_stat NotInReferenceImage }
    | _ ->
      Lwt.return
        (if ignore_unknown then witness else { witness with witness_stat = Unknown })
  in
  if !Config.only_bounded_check then only_bounded_call () else parallel_solver_calls ()
;;

(* ============================================================================================= *)
(*               CLASSIFYING SPURIOUS COUNTEREXAMPLES - DOES NOT SAT TINV                        *)

let rec find_original_var_and_proj v (og, elimv) =
  match elimv.tkind with
  | TVar v' ->
    (* Elimination term is just one variable. *)
    if v.vid = v'.vid then Some (og, -1) else None
  | TTup vars ->
    (* Elimination term is a tuple. Find which component we have. *)
    List.find_mapi vars ~f:(fun i t ->
        Option.map (find_original_var_and_proj v (og, t)) ~f:(fun (og, _) -> og, i))
  | _ -> None
;;

let mk_model_sat_asserts ~fctx ~ctx witness f_o_r instantiate =
  let f v =
    let v_val = Map.find_exn witness.witness_model v in
    match List.find_map ~f:(find_original_var_and_proj v) witness.witness_eqn.eelim with
    | Some (original_recursion_var, proj) ->
      (match original_recursion_var.tkind with
      | TVar ov when Option.is_some (instantiate ov) ->
        let instantiation = Option.value_exn (instantiate ov) in
        if proj >= 0
        then (
          let t = Reduce.reduce_term ~fctx ~ctx (mk_sel ctx (f_o_r instantiation) proj) in
          smt_of_term ~ctx Terms.(t == v_val))
        else (
          let t = f_o_r instantiation in
          smt_of_term ~ctx Terms.(t == v_val))
      | _ ->
        Log.error_msg
          Fmt.(
            str "Warning: skipped instantiating %a." (pp_term ctx) original_recursion_var);
        SmtLib.mk_true)
    | None -> smt_of_term ~ctx Terms.(mk_var ctx v == v_val)
  in
  List.map ~f (Map.keys witness.witness_model)
;;

(** [check_tinv_unsat ~ctx ~p tinv c] checks whether the counterexample [c] satisfies the predicate
  [tinv] in the synthesis problem [p]. The function returns a promise of a solver response and
  the resolver associated to that promise (the promise is cancellable).
  If the solver response is unsat, then there is a proof that the counterexample [c] does not
  satisfy the predicate [tinv]. In general, if the reponse is not unsat, the solver either
  stalls or returns unknown.
*)
let check_tinv_unsat ~(ctx : env) ~(p : PsiDef.t) (tinv : PMRS.t) (witness : witness)
    : (SmtLib.solver_response * Stats.verif_method) Lwt.t * int Lwt.u
  =
  let ref_tin = PMRS.extract_rec_input_typ p.reference in
  let build_task (cvc4_instance, task_start) =
    let* _ = task_start in
    (* Problem components. *)
    let existential_vars = ctx >- Analysis.free_variables witness.witness_eqn.eterm in
    let pmrs_decls =
      (ctx >>- smt_of_pmrs tinv)
      @ (ctx >>- smt_of_pmrs p.PsiDef.reference)
      @ if p.PsiDef.repr_is_identity then [] else ctx >>- smt_of_pmrs p.PsiDef.repr
    in
    let f_compose_r t =
      let repr_of_v =
        if p.PsiDef.repr_is_identity
        then t
        else (
          match RType.unify_one ref_tin (type_of (Terms.typed ctx.ctx t)) with
          | Ok _ -> t
          | _ -> mk_app_v ctx.ctx p.PsiDef.repr.pvar [ t ])
      in
      (* Don't add the PMRS pargs in the application, they are declared "globally" in the problem. *)
      mk_app_v ctx.ctx p.PsiDef.reference.pvar [ repr_of_v ]
    in
    (* Create the formula. *)
    let formula =
      (* Assert that Tinv(t) is true. *)
      let term_sat_tinv = mk_app_v ctx.ctx tinv.pvar [ witness.witness_eqn.eterm ] in
      (* Substitute recursion elimnation by recursive calls. *)
      let subs =
        List.concat_map
          ~f:(fun (orig_rec_var, elimv) ->
            match elimv.tkind with
            | TTup comps ->
              List.mapi comps ~f:(fun i t ->
                  t, mk_sel ctx.ctx (f_compose_r orig_rec_var) i)
            | _ -> [ elimv, f_compose_r orig_rec_var ])
          witness.witness_eqn.eelim
      in
      (* Assert that the preconditions hold. *)
      let preconds =
        Option.to_list (Option.map ~f:(substitution subs) witness.witness_eqn.eprecond)
      in
      (* Assert that the variables have the values assigned by the model. *)
      let model_sat =
        mk_model_sat_asserts witness f_compose_r (fun t -> Some (mk_var ctx.ctx t))
      in
      let formula_body =
        SmtLib.mk_assoc_and
          (List.map ~f:(ctx >- smt_of_term) (term_sat_tinv :: preconds)
          @ (ctx >>- model_sat))
      in
      SmtLib.mk_exists (ctx >- sorted_vars_of_vars existential_vars) formula_body
    in
    (* Start solving... *)
    let preamble =
      let logic =
        ctx
        >- SmtLogic.infer_logic
             ~quantifier_free:false
             ~for_induction:true
             ~with_uninterpreted_functions:true
             ~logic_infos:(Common.ProblemDefs.PsiDef.logics p)
             []
      in
      Commands.mk_preamble ~induction:true ~logic ()
    in
    let* () =
      AsyncSmt.exec_all cvc4_instance (preamble @ pmrs_decls @ [ mk_assert formula ])
    in
    let* resp = AsyncSmt.check_sat cvc4_instance in
    let* () = AsyncSmt.close_solver cvc4_instance in
    Lwt.return (resp, Stats.Induction)
  in
  AsyncSmt.(cancellable_task (AsyncSmt.make_solver "cvc") build_task)
;;

(** [check_tinv_sat ~ctx ~p tinv witness] checks whether the counterexample [witness] satisfies
    the invariant [tinv] (a PMRS). The function returns a pair of a promise of a solver
    response and a resolver for that promise. The promise is cancellable.
 *)
let check_tinv_sat ~(ctx : env) ~(p : PsiDef.t) (tinv : PMRS.t) (witness : witness)
    : (SmtLib.solver_response * Stats.verif_method) Lwt.t * int Lwt.u
  =
  let f_compose_r t =
    let repr_of_v =
      if p.PsiDef.repr_is_identity then t else ctx >>- Reduce.reduce_pmrs p.PsiDef.repr t
    in
    ctx_reduce ctx (ctx >>- Reduce.reduce_pmrs p.PsiDef.reference repr_of_v)
  in
  let initial_t = witness.witness_eqn.eterm in
  let task (solver, starter) =
    let steps = ref 0 in
    let t_check binder t =
      let tinv_t = ctx >>- Reduce.reduce_pmrs tinv t in
      let rec_instantation =
        Option.value ~default:VarMap.empty (ctx >- Matching.matches t ~pattern:initial_t)
      in
      let preconds =
        let subs =
          List.map
            ~f:(fun (orig_rec_var, elimv) ->
              match orig_rec_var.tkind with
              | TVar rec_var when Map.mem rec_instantation rec_var ->
                elimv, f_compose_r (Map.find_exn rec_instantation rec_var)
              | _ -> failwith "all elimination variables should be substituted.")
            witness.witness_eqn.eelim
        in
        Option.to_list
          (Option.map
             ~f:(fun t -> ctx >- smt_of_term (substitution subs t))
             witness.witness_eqn.eprecond)
      in
      (* Assert that the variables have the values assigned by the model. *)
      let model_sat =
        ctx >>- mk_model_sat_asserts witness f_compose_r (Map.find rec_instantation)
      in
      let vars =
        VarSet.union_list
          (Option.(
             to_list
               (map
                  ~f:(fun v -> ctx >- Analysis.free_variables v)
                  witness.witness_eqn.eprecond))
          @ [ witness.witness_vars
            ; ctx >- Analysis.free_variables t
            ; ctx >- Analysis.free_variables (f_compose_r t)
            ])
      in
      (* Start sequence of solver commands, bind on accum. *)
      let* _ = binder in
      let* () = AsyncSmt.spush solver in
      let* () = AsyncSmt.exec_all solver (ctx >- Commands.decls_of_vars vars) in
      (* Assert that Tinv(t) *)
      let* () = AsyncSmt.smt_assert solver (ctx >- smt_of_term tinv_t) in
      (* Assert that term satisfies model. *)
      let* () = AsyncSmt.smt_assert solver (SmtLib.mk_assoc_and (preconds @ model_sat)) in
      (* Assert that preconditions hold. *)
      let* resp = AsyncSmt.check_sat solver in
      let* () = AsyncSmt.spop solver in
      Lwt.return resp
    in
    let* _ = starter in
    let* () =
      let logic =
        ctx >- SmtLogic.infer_logic ~logic_infos:(Common.ProblemDefs.PsiDef.logics p) []
      in
      AsyncSmt.exec_all
        solver
        (Commands.mk_preamble
           ~incremental:(String.is_prefix ~prefix:"CVC" solver.s_name)
           ~logic
           ())
    in
    let* res =
      ctx >- Expand.lwt_expand_loop steps t_check (Lwt.return witness.witness_eqn.eterm)
    in
    let* () = AsyncSmt.close_solver solver in
    Lwt.return (res, Stats.BoundedChecking)
  in
  AsyncSmt.(cancellable_task (make_solver !Config.verification_solver) task)
;;

let satisfies_tinv ~(ctx : env) ~(p : PsiDef.t) (tinv : PMRS.t) (witness : witness)
    : witness Lwt.t
  =
  let resp_handler resp =
    Lwt.return
      Smtlib.SmtLib.(
        match resp with
        | Sat -> { witness with witness_stat = Valid }
        | Unsat ->
          { witness with
            witness_stat = add_cause witness.witness_stat ViolatesTargetRequires
          }
        | Error _ | Unknown -> { witness with witness_stat = Unknown }
        | _ -> failwith "Unexpected response.")
  in
  let parallel_solver_calls () =
    let task_counter = ref 2 in
    let%lwt resp, vmethod =
      try
        (* This call is expected to respond "unsat" when terminating. *)
        let pr1, resolver1 = check_tinv_unsat ~ctx ~p tinv witness in
        let pr1 = wait_on_failure task_counter pr1 in
        (* This call is expected to respond "sat" when terminating. *)
        let pr2, resolver2 = check_tinv_sat ~ctx ~p tinv witness in
        let pr2 = wait_on_failure task_counter pr2 in
        Lwt.wakeup resolver2 1;
        Lwt.wakeup resolver1 1;
        Lwt.pick [ pr1; pr2 ]
      with
      | End_of_file ->
        Log.error_msg "Solvers terminated unexpectedly  ⚠️";
        Log.error_msg "Please inspect logs.";
        Lwt.return (SmtLib.Unknown, Stats.Induction)
    in
    ctx >- AlgoLog.requires_witness_class tinv witness resp vmethod;
    resp_handler resp
  in
  let only_bounded_call () =
    let%lwt resp, vmethod =
      try
        let pr1, resolver1 = check_tinv_sat ~ctx ~p tinv witness in
        Lwt.wakeup resolver1 1;
        pr1
      with
      | End_of_file ->
        Log.error_msg "Solvers terminated unexpectedly  ⚠️";
        Log.error_msg "Please inspect logs.";
        Lwt.return (SmtLib.Unknown, Stats.Induction)
    in
    ctx >- AlgoLog.requires_witness_class tinv witness resp vmethod;
    resp_handler resp
  in
  if !Config.only_bounded_check then only_bounded_call () else parallel_solver_calls ()
;;

(** Classify counterexamples into positive or negative counterexamples with respect
    to the Tinv predicate in the problem.
*)
let classify_witnesss ~(ctx : env) ~(p : PsiDef.t) (witnesss : witness list)
    : witness list Lwt.t
  =
  let classify_with_tinv tinv witnesss =
    (* TODO: DT_LIA for z3, DTLIA for cvc4... Should write a type to represent logics. *)
    let f (witness : witness) = satisfies_tinv ~ctx ~p tinv witness in
    Lwt_list.map_s f witnesss
  in
  let classify_wrt_ref b =
    Lwt_list.map_s (check_witness_in_image ~ctx ~ignore_unknown:b ~p)
  in
  Log.start_section "Classify counterexamples...";
  (* First pass ignoring unknowns. *)
  (* let witnesss = classify_wrt_ref true witnesss in *)
  let witnesss_c1 =
    match p.tinv with
    | Some tinv -> classify_with_tinv tinv witnesss
    | None -> Lwt.return witnesss
  in
  let%lwt witnesss_c2 =
    if Option.is_some p.tinv
    then (* TODO find a way to do both classifications efficiently. *)
      witnesss_c1
    else Lwt.bind witnesss_c1 (classify_wrt_ref false)
  in
  Log.end_section ();
  Lwt.return witnesss_c2
;;
