open Lwt
open AState
open Base
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

let smt_unsatisfiability_check (unknowns : VarSet.t) (eqns : equation list) : unit =
  let free_vars =
    let f fvs eqn =
      let precond, lhs, rhs = eqn.eprecond, eqn.elhs, eqn.erhs in
      let set' =
        VarSet.union_list
          [ Analysis.free_variables lhs
          ; Analysis.free_variables rhs
          ; Option.value_map precond ~f:Analysis.free_variables ~default:VarSet.empty
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
        let lhs = smt_of_term eqn.elhs in
        let rhs = smt_of_term eqn.erhs in
        let pre = Option.map ~f:smt_of_term eqn.eprecond in
        match pre with
        | Some precondition -> SmtLib.(mk_or (mk_not precondition) (mk_eq lhs rhs))
        | None -> SmtLib.mk_eq lhs rhs
      in
      List.map ~f eqns
    in
    ( SmtLib.(mk_forall (sorted_vars_of_vars free_vars) (mk_assoc_and eqns_constraints))
    , List.map ~f:(fun eqn -> Terms.(eqn.elhs == eqn.erhs)) eqns )
  in
  let z3 = SyncSmt.make_z3_solver () in
  let preamble =
    Commands.mk_preamble
      ~logic:
        (SmtLogic.infer_logic
           ~with_uninterpreted_functions:true
           ~for_induction:false
           ~quantifier_free:false
           terms_for_logic_deduction)
      ()
  in
  SyncSmt.exec_all z3 (preamble @ Commands.decls_of_vars unknowns);
  SyncSmt.smt_assert z3 constraint_of_eqns;
  let resp = SyncSmt.check_sat z3 in
  (match resp with
  | Unsat ->
    Utils.Stats.(add_unrealizability_method USmtProof);
    Log.debug
      Fmt.(
        fun fmt () ->
          pf
            fmt
            "Z3 query answer for unsatisfiability of synthesis problem: %a"
            SyncSmt.pp_solver_response
            Unsat)
  | x ->
    Log.error
      Fmt.(
        fun fmt () ->
          pf fmt "Z3 unsat check failed with %a@." SyncSmt.pp_solver_response x));
  SyncSmt.close_solver z3
;;

let merge_all (cl : unrealizability_ctex list) : unrealizability_ctex list =
  let same_origin orig other =
    Terms.equal other.ci.ctex_eqn.eterm orig.ci.ctex_eqn.eterm
    && Terms.equal other.cj.ctex_eqn.eterm orig.cj.ctex_eqn.eterm
  in
  let complete_model (cm : ctex) (other_m : ctex) =
    Map.fold other_m.ctex_model ~init:cm ~f:(fun ~key:v_of_other ~data cm ->
        if Set.mem cm.ctex_vars v_of_other && Map.mem cm.ctex_model v_of_other
        then cm
        else
          { cm with
            ctex_vars = Set.add cm.ctex_vars v_of_other
          ; ctex_model = Map.set cm.ctex_model ~key:v_of_other ~data
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

let reinterpret_model vseti (m0i, m0j') var_subst =
  let mi, mj =
    List.fold var_subst ~init:(VarMap.empty, VarMap.empty) ~f:(fun (mi, mj) (v, v') ->
        let primed_name = v'.vname in
        Variable.free v';
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

let unrealizability_ctex_of_constmap (i, j) (eqn_i, eqn_j) (vseti, vsetj) var_subst model =
  let m0i, m0j =
    Map.partitioni_tf
      ~f:(fun ~key ~data:_ -> Option.is_some (VarSet.find_by_name vseti key))
      model
  in
  (* Remap the names to ids of the original variables in m' *)
  let m_i, m_j = reinterpret_model vseti (m0i, m0j) var_subst in
  let vset = Set.union vseti vsetj in
  let ctex_i : ctex =
    { ctex_eqn = eqn_i; ctex_model = m_i; ctex_vars = vset; ctex_stat = Unknown }
  in
  let ctex_j : ctex =
    { ctex_eqn = eqn_j; ctex_model = m_j; ctex_vars = vset; ctex_stat = Unknown }
  in
  { i; j; ci = ctex_i; cj = ctex_j }
;;

let skeleton_match ~unknowns (e1 : term) (e2 : term) : (term * term) list option =
  let args1, e1' = Matching.skeletize ~functions:unknowns e1
  and args2, e2' = Matching.skeletize ~functions:unknowns e2 in
  match Matching.matches ~boundvars:unknowns ~pattern:e1' e2' with
  | Some subs ->
    let f (v1, packedv) =
      match packedv.tkind with
      | TVar v2 -> Option.both (Map.find args1 v1) (Map.find args2 v2)
      | _ -> None
    in
    all_or_none (List.map ~f (Map.to_alist subs))
  | None -> None
;;

let components_of_unrealizability ~unknowns (eqn1 : equation) (eqn2 : equation)
    : ((term * term) list * (term * term)) option
  =
  let validate terms =
    List.for_all terms ~f:(fun (t, t') ->
        Set.is_empty (Set.inter (Analysis.free_variables t) unknowns)
        && Set.is_empty (Set.inter (Analysis.free_variables t') unknowns))
  in
  match skeleton_match ~unknowns eqn1.erhs eqn2.erhs with
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

let gen_info (eqn_i, eqn_j) unknowns =
  let fv e =
    VarSet.union_list
      (List.map e.eelim ~f:(fun (_, elim) -> Analysis.free_variables elim)
      @ [ Set.filter
            ~f:(fun v -> RType.is_base (Variable.vtype_or_new v))
            (Analysis.free_variables e.eterm)
        ]
      @ [ Analysis.free_variables e.elhs
        ; Analysis.free_variables e.erhs
        ; Option.value_map ~default:VarSet.empty ~f:Analysis.free_variables e.eprecond
        ])
  in
  let vseti = Set.diff (fv eqn_i) unknowns
  and vsetj = Set.diff (fv eqn_j) unknowns in
  let var_subst = VarSet.prime vsetj in
  let vsetj' = VarSet.of_list (List.map ~f:snd var_subst) in
  let sub = List.map ~f:(fun (v, v') -> mk_var v, mk_var v') var_subst in
  vseti, vsetj, vsetj', sub, var_subst
;;

(** Check if system of equations defines a functionally realizable synthesis problem.
  If any equation defines an unsolvable problem, an unrealizability_ctex is added to the
  list of counterexamples to be returned.
  If the returned list is empty, the problem may be solvable/realizable.
  If the returned list is not empty, the problem is not solvable / unrealizable.
*)
let check_unrealizable (unknowns : VarSet.t) (eqns : equation_system)
    : unrealizability_ctex list Lwt.t * int Lwt.u
  =
  Log.debug (fun f () -> Fmt.(pf f "Checking unrealizability..."));
  let start_time = Unix.gettimeofday () in
  let task (solver, binder) =
    let* _ = binder in
    let* () = AsyncSmt.load_min_max_defs solver in
    (* Main part of the check, applied to each equation in eqns. *)
    let check_eqn_accum (ctexs : unrealizability_ctex list Lwt.t) ((i, eqn_i), (j, eqn_j))
      =
      let* ctexs = ctexs in
      let vseti, vsetj, vsetj', sub, var_subst = gen_info (eqn_i, eqn_j) unknowns in
      (* Extract the arguments of the rhs, if it is a proper skeleton. *)
      match components_of_unrealizability ~unknowns eqn_i eqn_j with
      | None -> return ctexs (* If we cannot match the expected structure, skip it. *)
      | Some (rhs_args_ij, (lhs_i, lhs_j)) ->
        let lhs_diff =
          let projs = projection_eqns lhs_i (substitution sub lhs_j) in
          List.map ~f:(fun (ei, ej) -> Terms.(~!(ei == ej))) projs
        in
        (* (push). *)
        let* () = AsyncSmt.spush solver in
        (* Declare the variables. *)
        let* () =
          AsyncSmt.exec_all solver (Commands.decls_of_vars (Set.union vseti vsetj'))
        in
        (* Assert preconditions, if they exist. *)
        let* () =
          match eqn_i.eprecond with
          | Some pre_i ->
            let* _ =
              AsyncSmt.exec_all
                solver
                (Commands.decls_of_vars (Analysis.free_variables pre_i))
            in
            AsyncSmt.smt_assert solver (smt_of_term pre_i)
          | None -> return ()
        in
        (* Assert the precondition for j *)
        let* () =
          match eqn_j.eprecond with
          | Some pre_j ->
            let* _ =
              AsyncSmt.exec_all
                solver
                (Commands.decls_of_vars (Analysis.free_variables pre_j))
            in
            AsyncSmt.smt_assert solver (smt_of_term (substitution sub pre_j))
          | None -> return ()
        in
        (* Assert that the lhs of i and j must be different. **)
        let* () =
          Lwt_list.iter_s
            (fun eqn -> AsyncSmt.smt_assert solver (smt_of_term eqn))
            lhs_diff
        in
        (* Assert that the rhs must be equal. *)
        let* _ =
          Lwt_list.iter_s
            (fun (rhs_i_arg_term, rhs_j_arg_term) ->
              let rhs_eqs =
                projection_eqns rhs_i_arg_term (substitution sub rhs_j_arg_term)
              in
              Lwt_list.iter_s
                (fun (lhs, rhs) ->
                  AsyncSmt.smt_assert solver (smt_of_term Terms.(lhs == rhs)))
                rhs_eqs)
            rhs_args_ij
        in
        (* Check sat and get model. *)
        let* new_ctexs =
          let* resp = AsyncSmt.check_sat solver in
          match resp with
          | Sat ->
            let* model_sexps = AsyncSmt.get_model solver in
            (match model_sexps with
            | SExps s ->
              let model = model_to_constmap (SExps s) in
              (* Search for a few additional models. *)
              let* other_models =
                if !Config.Optims.fuzzing_count > 0
                then
                  request_different_models_async
                    (return model)
                    !Config.Optims.fuzzing_count
                    solver
                else return []
              in
              let new_ctexs =
                List.map
                  ~f:
                    (unrealizability_ctex_of_constmap
                       (i, j)
                       (eqn_i, eqn_j)
                       (vseti, vsetj)
                       var_subst)
                  (model :: other_models)
              in
              return (new_ctexs @ ctexs)
            | _ -> return ctexs)
          | _ -> return ctexs
        in
        let+ () = AsyncSmt.spop solver in
        new_ctexs
    in
    let* ctexs =
      Lwt.map
        merge_all
        (List.fold
           ~f:check_eqn_accum
           ~init:(return [])
           (List.mapi ~f:(fun i eqn -> i, eqn) eqns |> combinations))
    in
    let+ _ = AsyncSmt.close_solver solver in
    let elapsed = Unix.gettimeofday () -. start_time in
    Log.debug (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
    AlgoLog.show_unrealizability_witnesses unknowns eqns ctexs;
    ctexs
  in
  AsyncSmt.(cancellable_task (make_solver "z3") task)
;;

(* ============================================================================================= *)
(*                            CLASSIFYING SPURIOUS COUNTEREXAMPLES                               *)
(* ============================================================================================= *)

let add_cause (ctx : ctex_stat) (cause : spurious_cause) =
  match ctx with
  | Valid -> Spurious [ cause ]
  | Spurious c ->
    if not (Caml.List.mem cause c) then Spurious (cause :: c) else Spurious c
  | Unknown -> Spurious [ cause ]
;;

(*               CLASSIFYING SPURIOUS COUNTEREXAMPLES - NOT IN REFERENCE IMAGE                   *)

let check_image_sat ~p ctex : (Stats.verif_method * SmtLib.solver_response) Lwt.t * int u =
  let f_compose_r t =
    let repr_of_v =
      if p.psi_repr_is_identity then t else Reduce.reduce_pmrs p.psi_repr t
    in
    Reduce.reduce_term (Reduce.reduce_pmrs p.psi_reference repr_of_v)
  in
  (* Asynchronous solver task. *)
  let build_task (solver_instance, task_start) =
    let steps = ref 0 in
    (* A single check for a bounded term. *)
    let t_check accum t =
      (* Build equations of the form (f t) != (value of elimination var in model) *)
      let term_eqs =
        List.map ctex.ctex_eqn.eelim ~f:(fun (_, elimv) ->
            Terms.(f_compose_r t == Eval.in_model ctex.ctex_model elimv))
      in
      let rec aux accum tlist =
        let f binder eqn =
          let* _ = binder in
          let* () = AsyncSmt.spush solver_instance in
          let* () =
            AsyncSmt.exec_all
              solver_instance
              (Commands.decls_of_vars (Analysis.free_variables eqn))
          in
          let* () = AsyncSmt.smt_assert solver_instance (smt_of_term eqn) in
          let* res = AsyncSmt.check_sat solver_instance in
          let* () = AsyncSmt.spop solver_instance in
          return res
        in
        match tlist with
        | [] -> accum
        | t0 :: tl ->
          let* accum' = f accum t0 in
          (match accum' with
          | Unsat -> return SmtLib.Unsat
          | _ -> aux (return accum') tl)
      in
      aux accum term_eqs
    in
    let t_decl =
      List.map ~f:snd (Lang.SmtInterface.declare_datatype_of_rtype !AState._alpha)
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
      Expand.lwt_expand_loop
        steps
        t_check
        (return (TermSet.singleton ctex.ctex_eqn.eterm))
    in
    let* () = AsyncSmt.close_solver solver_instance in
    return (Stats.BoundedChecking, res)
  in
  AsyncSmt.(cancellable_task (make_solver "z3") build_task)
;;

let check_image_unsat ~p ctex : (Stats.verif_method * SmtLib.solver_response) t * int u =
  let f_compose_r t =
    let repr_of_v =
      if p.psi_repr_is_identity then t else mk_app_v p.psi_repr.pvar [ t ]
    in
    mk_app_v
      p.psi_reference.pvar
      (List.map ~f:mk_var p.psi_reference.pargs @ [ repr_of_v ])
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
             (SmtLogic.infer_logic
                ~quantifier_free:false
                ~for_induction:true
                ~with_uninterpreted_functions:true
                ~logic_infos:(AState.psi_def_logics p)
                [])
           ())
    in
    (* Declare Tinv, repr and reference functions. *)
    let* () =
      Lwt_list.iter_p
        (fun x ->
          let* _ = AsyncSmt.exec_command solver x in
          return ())
        (smt_of_pmrs p.psi_reference
        @ if p.psi_repr_is_identity then [] else smt_of_pmrs p.psi_repr)
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
                    Eval.in_model ctex.ctex_model t == mk_sel (f_compose_r orig_rec_var) i))
            | _ ->
              Terms.[ Eval.in_model ctex.ctex_model elimv == f_compose_r orig_rec_var ])
          ctex.ctex_eqn.eelim
      in
      ( VarSet.union_list
          (List.map ctex.ctex_eqn.eelim ~f:(fun (x, _) -> Analysis.free_variables x))
      , (* Assert that the variables have the values assigned by the model. *)
        SmtLib.mk_assoc_and (List.map ~f:smt_of_term eqns) )
    in
    let* _ =
      let fv = Set.union fv (VarSet.of_list p.psi_reference.pargs) in
      AsyncSmt.smt_assert solver (SmtLib.mk_exists (sorted_vars_of_vars fv) formula)
    in
    let* resp = AsyncSmt.check_sat solver in
    let* () = AsyncSmt.close_solver solver in
    return (Stats.Induction, resp)
  in
  AsyncSmt.(cancellable_task (AsyncSmt.make_solver "cvc") build_task)
;;

(**
  [check_ctex_in_image ~p ctex] checks whether the recursion elimination's variables values in the
  model of [ctex] are in the image of (p.psi_reference o p.psi_repr).
*)
let check_ctex_in_image ?(ignore_unknown = false) ~(p : psi_def) (ctex : ctex) : ctex =
  Log.verbose_msg
    Fmt.(str "Checking whether ctex is in the image of %s..." p.psi_reference.pvar.vname);
  let vmethod, resp =
    if Analysis.is_bounded ctex.ctex_eqn.eterm
    then Stats.Induction, SmtLib.Sat
    else (
      try
        Lwt_main.run
          ((* This call is expected to respond "unsat" when terminating. *)
           let pr1, resolver1 = check_image_sat ~p ctex in
           (* This call is expected to respond "sat" when terminating. *)
           let pr2, resolver2 = check_image_unsat ~p ctex in
           Lwt.wakeup resolver2 1;
           Lwt.wakeup resolver1 1;
           (* The first call to return is kept, the other one is ignored. *)
           Lwt.pick [ pr1; pr2 ])
      with
      | End_of_file ->
        Log.error_msg "Solvers terminated unexpectedly  ⚠️ .";
        Log.error_msg "Please inspect logs.";
        Stats.Induction, SmtLib.Unknown)
  in
  AlgoLog.image_ctex_class p ctex resp vmethod;
  match resp with
  | Sat -> ctex
  | Unsat -> { ctex with ctex_stat = add_cause ctex.ctex_stat NotInReferenceImage }
  | _ -> if ignore_unknown then ctex else { ctex with ctex_stat = Unknown }
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

let mk_model_sat_asserts ctex f_o_r instantiate =
  let f v =
    let v_val = Map.find_exn ctex.ctex_model v in
    match List.find_map ~f:(find_original_var_and_proj v) ctex.ctex_eqn.eelim with
    | Some (original_recursion_var, proj) ->
      (match original_recursion_var.tkind with
      | TVar ov when Option.is_some (instantiate ov) ->
        let instantiation = Option.value_exn (instantiate ov) in
        if proj >= 0
        then (
          let t = Reduce.reduce_term (mk_sel (f_o_r instantiation) proj) in
          smt_of_term Terms.(t == v_val))
        else (
          let t = f_o_r instantiation in
          smt_of_term Terms.(t == v_val))
      | _ ->
        Log.error_msg
          Fmt.(str "Warning: skipped instantiating %a." pp_term original_recursion_var);
        SmtLib.mk_true)
    | None -> smt_of_term Terms.(mk_var v == v_val)
  in
  List.map ~f (Map.keys ctex.ctex_model)
;;

(** [check_tinv_unsat ~p tinv c] checks whether the counterexample [c] satisfies the predicate
  [tinv] in the synthesis problem [p]. The function returns a promise of a solver response and
  the resolver associated to that promise (the promise is cancellable).
  If the solver response is unsat, then there is a proof that the counterexample [c] does not
  satisfy the predicate [tinv]. In general, if the reponse is not unsat, the solver either
  stalls or returns unknown.
*)
let check_tinv_unsat ~(p : psi_def) (tinv : PMRS.t) (ctex : ctex)
    : (Stats.verif_method * SmtLib.solver_response) t * int Lwt.u
  =
  let build_task (cvc4_instance, task_start) =
    let* _ = task_start in
    (* Problem components. *)
    let fv =
      Set.union
        (Analysis.free_variables ctex.ctex_eqn.eterm)
        (VarSet.of_list p.psi_reference.pargs)
    in
    let pmrs_decls =
      smt_of_pmrs tinv
      @ smt_of_pmrs p.psi_reference
      @ if p.psi_repr_is_identity then [] else smt_of_pmrs p.psi_repr
    in
    let f_compose_r t =
      let repr_of_v =
        if p.psi_repr_is_identity then t else mk_app_v p.psi_repr.pvar [ t ]
      in
      mk_app_v
        p.psi_reference.pvar
        (List.map ~f:mk_var p.psi_reference.pargs @ [ repr_of_v ])
    in
    (* Create the formula. *)
    let formula =
      (* Assert that Tinv(t) is true. *)
      let term_sat_tinv = mk_app_v tinv.pvar [ ctex.ctex_eqn.eterm ] in
      (* Substitute recursion elimnation by recursive calls. *)
      let subs =
        List.concat_map
          ~f:(fun (orig_rec_var, elimv) ->
            match elimv.tkind with
            | TTup comps ->
              List.mapi comps ~f:(fun i t -> t, mk_sel (f_compose_r orig_rec_var) i)
            | _ -> [ elimv, f_compose_r orig_rec_var ])
          ctex.ctex_eqn.eelim
      in
      (* Assert that the preconditions hold. *)
      let preconds =
        Option.to_list (Option.map ~f:(substitution subs) ctex.ctex_eqn.eprecond)
      in
      (* Assert that the variables have the values assigned by the model. *)
      let model_sat = mk_model_sat_asserts ctex f_compose_r (fun t -> Some (mk_var t)) in
      let formula_body =
        SmtLib.mk_assoc_and
          (List.map ~f:smt_of_term (term_sat_tinv :: preconds) @ model_sat)
      in
      SmtLib.mk_exists (sorted_vars_of_vars fv) formula_body
    in
    (* Start solving... *)
    let preamble =
      let logic =
        SmtLogic.infer_logic
          ~quantifier_free:false
          ~for_induction:true
          ~with_uninterpreted_functions:true
          ~logic_infos:(AState.psi_def_logics p)
          []
      in
      Commands.mk_preamble ~induction:true ~logic ()
    in
    let* () =
      AsyncSmt.exec_all cvc4_instance (preamble @ pmrs_decls @ [ mk_assert formula ])
    in
    let* resp = AsyncSmt.check_sat cvc4_instance in
    let* () = AsyncSmt.close_solver cvc4_instance in
    return (Stats.Induction, resp)
  in
  AsyncSmt.(cancellable_task (AsyncSmt.make_solver "cvc") build_task)
;;

(** [check_tinv_sat ~p tinv ctex] checks whether the counterexample [ctex] satisfies
    the invariant [tinv] (a PMRS). The function returns a pair of a promise of a solver
    response and a resolver for that promise. The promise is cancellable.
 *)
let check_tinv_sat ~(p : psi_def) (tinv : PMRS.t) (ctex : ctex)
    : (Stats.verif_method * SmtLib.solver_response) t * int Lwt.u
  =
  let f_compose_r t =
    let repr_of_v =
      if p.psi_repr_is_identity then t else Reduce.reduce_pmrs p.psi_repr t
    in
    Reduce.reduce_term (Reduce.reduce_pmrs p.psi_reference repr_of_v)
  in
  let initial_t = ctex.ctex_eqn.eterm in
  let task (solver, starter) =
    let steps = ref 0 in
    let t_check binder t =
      let tinv_t = Reduce.reduce_pmrs tinv t in
      let rec_instantation =
        Option.value ~default:VarMap.empty (Matching.matches t ~pattern:initial_t)
      in
      let preconds =
        let subs =
          List.map
            ~f:(fun (orig_rec_var, elimv) ->
              match orig_rec_var.tkind with
              | TVar rec_var when Map.mem rec_instantation rec_var ->
                elimv, f_compose_r (Map.find_exn rec_instantation rec_var)
              | _ -> failwith "all elimination variables should be substituted.")
            ctex.ctex_eqn.eelim
        in
        Option.to_list
          (Option.map
             ~f:(fun t -> smt_of_term (substitution subs t))
             ctex.ctex_eqn.eprecond)
      in
      (* Assert that the variables have the values assigned by the model. *)
      let model_sat = mk_model_sat_asserts ctex f_compose_r (Map.find rec_instantation) in
      let vars =
        VarSet.union_list
          (Option.(to_list (map ~f:Analysis.free_variables ctex.ctex_eqn.eprecond))
          @ [ ctex.ctex_vars
            ; Analysis.free_variables t
            ; Analysis.free_variables (f_compose_r t)
            ])
      in
      (* Start sequence of solver commands, bind on accum. *)
      let* _ = binder in
      let* () = AsyncSmt.spush solver in
      let* () = AsyncSmt.exec_all solver (Commands.decls_of_vars vars) in
      (* Assert that Tinv(t) *)
      let* () = AsyncSmt.smt_assert solver (smt_of_term tinv_t) in
      (* Assert that term satisfies model. *)
      let* () = AsyncSmt.smt_assert solver (SmtLib.mk_assoc_and (preconds @ model_sat)) in
      (* Assert that preconditions hold. *)
      let* resp = AsyncSmt.check_sat solver in
      let* () = AsyncSmt.spop solver in
      return resp
    in
    let* _ = starter in
    let* () =
      AsyncSmt.exec_all
        solver
        (Commands.mk_preamble
           ~incremental:(String.is_prefix ~prefix:"CVC" solver.s_name)
           ~logic:(SmtLogic.infer_logic ~logic_infos:(AState.psi_def_logics p) [])
           ())
    in
    let* res =
      Expand.lwt_expand_loop
        steps
        t_check
        (return (TermSet.singleton ctex.ctex_eqn.eterm))
    in
    let* () = AsyncSmt.close_solver solver in
    return (Stats.BoundedChecking, res)
  in
  AsyncSmt.(cancellable_task (make_solver "z3") task)
;;

let satisfies_tinv ~(p : psi_def) (tinv : PMRS.t) (ctex : ctex) : ctex =
  let vmethod, resp =
    try
      Lwt_main.run
        ((* This call is expected to respond "unsat" when terminating. *)
         let pr1, resolver1 = check_tinv_unsat ~p tinv ctex in
         (* This call is expected to respond "sat" when terminating. *)
         let pr2, resolver2 = check_tinv_sat ~p tinv ctex in
         Lwt.wakeup resolver2 1;
         Lwt.wakeup resolver1 1;
         (* The first call to return is kept, the other one is ignored. *)
         Lwt.pick [ pr1; pr2 ])
    with
    | End_of_file ->
      Log.error_msg "Solvers terminated unexpectedly  ⚠️ .";
      Log.error_msg "Please inspect logs.";
      Stats.Induction, SmtLib.Unknown
  in
  AlgoLog.requires_ctex_class tinv ctex resp vmethod;
  match resp with
  | Sat -> { ctex with ctex_stat = Valid }
  | Unsat -> { ctex with ctex_stat = add_cause ctex.ctex_stat ViolatesTargetRequires }
  | _ -> ctex
;;

(** Classify counterexamples into positive or negative counterexamples with respect
    to the Tinv predicate in the problem.
*)
let classify_ctexs ~(p : psi_def) (ctexs : ctex list) : ctex list =
  let classify_with_tinv tinv ctexs =
    (* TODO: DT_LIA for z3, DTLIA for cvc4... Should write a type to represent logics. *)
    let f (ctex : ctex) = satisfies_tinv ~p tinv ctex in
    List.map ~f ctexs
  in
  let classify_wrt_ref b = List.map ~f:(check_ctex_in_image ~ignore_unknown:b ~p) in
  Log.start_section "Classify counterexamples...";
  (* First pass ignoring unknowns. *)
  (* let ctexs = classify_wrt_ref true ctexs in *)
  let ctexs_c1, ignore_further_unknowns =
    match p.psi_tinv with
    | Some tinv ->
      (* If there is some tinv, we may ignore unknowns in further classification steps. *)
      classify_with_tinv tinv ctexs, true
    | None ->
      (* Otherwise don't ignore anything. *)
      ctexs, false
  in
  let ctexs_c2 =
    if ignore_further_unknowns
    then (* TODO: there are some bugs with classification.. *)
      ctexs_c1
    else classify_wrt_ref ignore_further_unknowns ctexs_c1
  in
  Log.end_section ();
  ctexs_c2
;;
