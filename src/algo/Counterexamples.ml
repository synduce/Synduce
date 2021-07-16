open Lwt
open AState
open Base
open Lang
open Lang.Term
open Projection
open SmtInterface
open Smtlib
open Utils

(* ============================================================================================= *)
(*                               CHECKING UNREALIZABILITY                                        *)
(* ============================================================================================= *)

type unrealizability_ctex = { i : int; j : int; ci : ctex; cj : ctex }
(** A counterexample to realizability is a pair of models: a pair of maps from variable ids to terms. *)

let pp_unrealizability_ctex (frmt : Formatter.t) (uc : unrealizability_ctex) : unit =
  let pp_model frmt model =
    (* Print as comma-separated list of variable -> term *)
    Fmt.(
      list ~sep:comma
        (pair ~sep:Utils.rightarrow (option ~none:(fun fmt () -> pf fmt "?") Variable.pp) pp_term))
      frmt
      (List.map
         ~f:(fun (vid, t) -> (VarSet.find_by_id uc.ci.ctex_vars vid, t))
         (Map.to_alist model))
  in
  Fmt.(
    pf frmt "@[M<%i> = [%a]@]@;@[M'<%i> = [%a]@]" uc.i pp_model uc.ci.ctex_model uc.j pp_model
      uc.cj.ctex_model)

let reinterpret_model (m0i, m0j') var_subst =
  List.fold var_subst
    ~init:(Map.empty (module Int), Map.empty (module Int))
    ~f:(fun (m, m') (v, v') ->
      let primed_name = v'.vname in
      Variable.free v';
      match Map.find m0i v.vname with
      | Some data -> (
          let new_m = Map.set m ~key:v.vid ~data in
          match Map.find m0j' primed_name with
          | Some data -> (new_m, Map.set m' ~key:v.vid ~data)
          | None -> (new_m, m'))
      | None -> (
          match Map.find m0j' primed_name with
          | Some data -> (m, Map.set m' ~key:v.vid ~data)
          | None -> (m, m')))

let unrealizability_ctex_of_constmap (i, j) (eqn_i, eqn_j) (vseti, vsetj) var_subst model =
  let m0i, m0j =
    Map.partitioni_tf ~f:(fun ~key ~data:_ -> Option.is_some (VarSet.find_by_name vseti key)) model
  in
  (* Remap the names to ids of the original variables in m' *)
  let m_i, m_j = reinterpret_model (m0i, m0j) var_subst in
  let vset = Set.union vseti vsetj in
  let ctex_i : ctex = { ctex_eqn = eqn_i; ctex_vars = vset; ctex_model = m_i } in
  let ctex_j : ctex = { ctex_eqn = eqn_j; ctex_vars = vset; ctex_model = m_j } in
  { i; j; ci = ctex_i; cj = ctex_j }

(** Check if system of equations defines a functionally realizable synthesis problem.
  If any equation defines an unsolvable problem, an unrealizability_ctex is added to the
  list of counterexamples to be returned.
  If the returned list is empty, the problem may be solvable/realizable.
  If the returned list is not empty, the problem is not solvable / unrealizable.
*)
let check_unrealizable (unknowns : VarSet.t) (eqns : equation_system) : unrealizability_ctex list =
  Log.debug (fun f () -> Fmt.(pf f "Checking unrealizability..."));
  let start_time = Unix.gettimeofday () in
  let solver = Solvers.make_z3_solver () in
  Solvers.load_min_max_defs solver;
  (* Main part of the check, applied to each equation in eqns. *)
  let check_eqn_accum (ctexs : unrealizability_ctex list) ((i, eqn_i), (j, eqn_j)) =
    let fv e =
      VarSet.union_list
        [
          Analysis.free_variables e.elhs;
          Analysis.free_variables e.erhs;
          Option.value_map ~default:VarSet.empty ~f:Analysis.free_variables e.eprecond;
        ]
    in
    let vseti = Set.diff (fv eqn_i) unknowns and vsetj = Set.diff (fv eqn_j) unknowns in
    let var_subst = VarSet.prime vsetj in
    let vsetj' = VarSet.of_list (List.map ~f:snd var_subst) in
    let sub = List.map ~f:(fun (v, v') -> (mk_var v, mk_var v')) var_subst in
    (* Extract the arguments of the rhs, if it is a call to an unknown. *)
    let maybe_rhs_args =
      match (eqn_i.erhs.tkind, eqn_j.erhs.tkind) with
      | TApp ({ tkind = TVar f_v_i; _ }, args_i), TApp ({ tkind = TVar f_v_j; _ }, args_j) ->
          if
            Set.mem unknowns f_v_i && Set.mem unknowns f_v_j
            && Variable.(f_v_i = f_v_j)
            && List.length args_i = List.length args_j
          then
            let fv_args =
              VarSet.union_list (List.map ~f:Analysis.free_variables (args_i @ args_j))
            in
            (* Check there are no unknowns in the args. *)
            if Set.are_disjoint fv_args unknowns then Some (args_i, args_j) else None
          else None
      | TVar v_i, TVar v_j when Set.mem unknowns v_i && Set.mem unknowns v_j -> Some ([], [])
      | _ ->
          Log.debug_msg
            Fmt.(str "Unrealizability check: %a is not a function application." pp_term eqn_i.erhs);
          None
    in
    match maybe_rhs_args with
    | None -> ctexs (* If we cannot match the expected structure, skip it. *)
    | Some (rhs_args_i, rhs_args_j) ->
        (* (push). *)
        Solvers.spush solver;
        (* Declare the variables. *)
        Solvers.declare_all solver (decls_of_vars (Set.union vseti vsetj'));
        (* Assert preconditions, if they exist. *)
        (match eqn_i.eprecond with
        | Some pre_i ->
            Solvers.declare_all solver (decls_of_vars (Analysis.free_variables pre_i));
            Solvers.smt_assert solver (smt_of_term pre_i)
        | None -> ());
        (match eqn_j.eprecond with
        | Some pre_j ->
            Solvers.declare_all solver (decls_of_vars (Analysis.free_variables pre_j));
            Solvers.smt_assert solver (smt_of_term (substitution sub pre_j))
        | None -> ());
        (* Assert that the lhs of i and j must be different. **)
        let lhs_diff =
          let projs = projection_eqns eqn_i.elhs (substitution sub eqn_j.elhs) in
          List.map ~f:(fun (lhs, rhs) -> mk_un Not (mk_bin Eq lhs rhs)) projs
        in
        List.iter lhs_diff ~f:(fun eqn -> Solvers.smt_assert solver (smt_of_term eqn));
        (* Assert that the rhs must be equal. *)
        List.iter2_exn rhs_args_i rhs_args_j ~f:(fun rhs_i_arg_term rhs_j_arg_term ->
            let rhs_eqs = projection_eqns rhs_i_arg_term (substitution sub rhs_j_arg_term) in
            List.iter rhs_eqs ~f:(fun (lhs, rhs) ->
                Solvers.smt_assert solver (smt_of_term (mk_bin Eq lhs rhs))));
        (* Check sat and get model. *)
        let new_ctexs =
          match Solvers.check_sat solver with
          | Sat -> (
              match Solvers.get_model solver with
              | SExps s ->
                  let model = model_to_constmap (SExps s) in
                  (* Search for a few additional models. *)
                  let other_models =
                    if !Config.fuzzing_count > 0 then
                      request_different_models model !Config.fuzzing_count solver
                    else []
                  in
                  let new_ctexs =
                    List.map
                      ~f:
                        (unrealizability_ctex_of_constmap (i, j) (eqn_i, eqn_j) (vseti, vsetj)
                           var_subst)
                      (model :: other_models)
                  in
                  new_ctexs @ ctexs
              | _ -> ctexs)
          | _ -> ctexs
        in
        Solvers.spop solver;
        new_ctexs
  in
  let eqns_indexed = List.mapi ~f:(fun i eqn -> (i, eqn)) eqns in
  let ctexs = List.fold ~f:check_eqn_accum ~init:[] (combinations eqns_indexed) in
  Solvers.close_solver solver;
  let elapsed = Unix.gettimeofday () -. start_time in
  Log.debug (fun f () -> Fmt.(pf f "... finished in %3.4fs" elapsed));
  Log.debug (fun f () ->
      match ctexs with
      | [] -> Fmt.pf f "No counterexample to realizability found."
      | _ :: _ ->
          Fmt.(
            pf f
              "@[Counterexamples found!@;\
               @[<hov 2>❔ Equations:@;\
               @[<v>%a@]@]@;\
               @[<hov 2>❔ Counterexample models:@;\
               @[<v>%a@]@]@]"
              (list ~sep:sp (box (pair ~sep:colon int (box pp_equation))))
              eqns_indexed
              (list ~sep:sep_and pp_unrealizability_ctex)
              ctexs));
  ctexs

(** [check_tinv_unsat ~p tinv c] checks whether the counterexample [c] satisfies the predicate
  [tinv] in the synthesis problem [p]. The function returns a promise of a solver response and
  the resolver associated to that promise (the promise is cancellable).
  If the solver response is unsat, then there is a proof that the counterexample [c] does not
  satisfy the predicate [tinv]. In general, if the reponse is not unsat, the solver either
  stalls or returns unknown.
*)
let check_tinv_unsat ~(p : psi_def) (tinv : PMRS.t) (ctex : ctex) :
    Solvers.Asyncs.response * int Lwt.u =
  let open Solvers in
  let build_task (cvc4_instance, task_start) =
    let%lwt _ = task_start in
    let%lwt () = Asyncs.set_logic cvc4_instance "ALL" in
    let%lwt () = Asyncs.set_option cvc4_instance "quant-ind" "true" in
    let%lwt () =
      if !Config.induction_proof_tlimit >= 0 then
        Asyncs.set_option cvc4_instance "tlimit" (Int.to_string !Config.induction_proof_tlimit)
      else return ()
    in
    let%lwt () = Asyncs.load_min_max_defs cvc4_instance in
    (* Declare Tinv, repr and reference functions. *)
    let%lwt () =
      Lwt_list.iter_p
        (fun x ->
          let%lwt _ = Asyncs.exec_command cvc4_instance x in
          return ())
        (smt_of_pmrs tinv @ smt_of_pmrs p.psi_reference
        @ if p.psi_repr_is_identity then [] else smt_of_pmrs p.psi_repr)
    in
    let fv =
      Set.union (Analysis.free_variables ctex.ctex_eqn.eterm) (VarSet.of_list p.psi_reference.pargs)
    in
    let f_compose_r t =
      let repr_of_v = if p.psi_repr_is_identity then t else mk_app_v p.psi_repr.pvar [ t ] in
      mk_app_v p.psi_reference.pvar (List.map ~f:mk_var p.psi_reference.pargs @ [ repr_of_v ])
    in
    (* Create the formula. *)
    let formula =
      (* Assert that Tinv(t) is true. *)
      let term_sat_tinv = mk_app_v tinv.pvar [ ctex.ctex_eqn.eterm ] in
      (* Assert that the preconditions hold. *)
      let preconds =
        let subs =
          List.map
            ~f:(fun (orig_rec_var, elimv) -> (elimv, f_compose_r orig_rec_var))
            ctex.ctex_eqn.eelim
        in
        Option.to_list (Option.map ~f:(substitution subs) ctex.ctex_eqn.eprecond)
      in
      (* Assert that the variables have the values assigned by the model. *)
      let model_sat =
        let f v =
          let v_val = Map.find_exn ctex.ctex_model v.vid in
          match
            List.find
              ~f:(fun (_, elimv) -> match elimv.tkind with TVar v' -> v.vid = v'.vid | _ -> false)
              ctex.ctex_eqn.eelim
          with
          | Some (original_recursion_var, _) ->
              mk_bin Binop.Eq (f_compose_r original_recursion_var) v_val
          | None -> mk_bin Binop.Eq (mk_var v) v_val
        in
        List.map ~f (Set.elements ctex.ctex_vars)
      in
      SmtLib.mk_assoc_and (List.map ~f:smt_of_term (term_sat_tinv :: preconds @ model_sat))
    in
    let%lwt _ =
      Asyncs.smt_assert cvc4_instance (SmtLib.mk_exists (sorted_vars_of_vars fv) formula)
    in
    let%lwt resp = Asyncs.check_sat cvc4_instance in
    let%lwt () = Asyncs.close_solver cvc4_instance in
    return resp
  in
  Asyncs.(cancellable_task (Asyncs.make_cvc4_solver ()) build_task)

(** [check_tinv_sat ~p tinv ctex] checks whether the counterexample [ctex] satisfies
    the invariant [tinv] (a PMRS). The function returns a pair of a promise of a solver
    response and a resolver for that promise. The promise is cancellable.
 *)
let check_tinv_sat ~(p : psi_def) (tinv : PMRS.t) (ctex : ctex) :
    Solvers.Asyncs.response * int Lwt.u =
  let open Solvers in
  let f_compose_r t =
    let repr_of_v = if p.psi_repr_is_identity then t else Reduce.reduce_pmrs p.psi_repr t in
    Reduce.reduce_pmrs p.psi_reference repr_of_v
  in
  let initial_t = ctex.ctex_eqn.eterm in
  let task (solver, starter) =
    let steps = ref 0 in
    let rec check_bounded_sol accum terms =
      let f accum t =
        let tinv_t = Reduce.reduce_pmrs tinv t in
        let rec_instantation =
          Option.value ~default:VarMap.empty (Analysis.matches t ~pattern:initial_t)
        in
        let preconds =
          let subs =
            List.map
              ~f:(fun (orig_rec_var, elimv) ->
                match orig_rec_var.tkind with
                | TVar rec_var when Map.mem rec_instantation rec_var ->
                    (elimv, f_compose_r (Map.find_exn rec_instantation rec_var))
                | _ -> failwith "all elimination variables should be substituted.")
              ctex.ctex_eqn.eelim
          in
          Option.to_list
            (Option.map ~f:(fun t -> smt_of_term (substitution subs t)) ctex.ctex_eqn.eprecond)
        in
        (* Assert that the variables have the values assigned by the model. *)
        let model_sat =
          let f v =
            let v_val = Map.find_exn ctex.ctex_model v.vid in
            match
              List.find
                ~f:(fun (_, elimv) ->
                  match elimv.tkind with TVar v' -> v.vid = v'.vid | _ -> false)
                ctex.ctex_eqn.eelim
            with
            | Some (original_recursion_var, _) -> (
                match original_recursion_var.tkind with
                | TVar ov when Map.mem rec_instantation ov ->
                    let instantiation = Map.find_exn rec_instantation ov in
                    smt_of_term (mk_bin Binop.Eq (f_compose_r instantiation) v_val)
                | _ -> SmtLib.mk_true)
            | None -> smt_of_term (mk_bin Binop.Eq (mk_var v) v_val)
          in
          List.map ~f (Set.elements ctex.ctex_vars)
        in
        let vars =
          Set.union (Analysis.free_variables t) (Analysis.free_variables (f_compose_r t))
        in
        (* Start sequence of solver commands, bind on accum. *)
        let%lwt _ = accum in
        let%lwt () = Asyncs.spush solver in
        let%lwt () = Asyncs.declare_all solver (SmtInterface.decls_of_vars vars) in
        (* Assert that Tinv(t) *)
        let%lwt () = Asyncs.smt_assert solver (smt_of_term tinv_t) in
        (* Assert that term satisfies model. *)
        let%lwt () = Asyncs.smt_assert solver (SmtLib.mk_assoc_and (preconds @ model_sat)) in
        (* Assert that preconditions hold. *)
        let%lwt resp = Asyncs.check_sat solver in
        let%lwt () = Asyncs.spop solver in
        return resp
      in
      match terms with
      | [] -> accum
      | t0 :: tl -> (
          let%lwt accum' = f accum t0 in
          match accum' with Sat -> return SmtLib.Sat | _ -> check_bounded_sol (return accum') tl)
    in
    let rec expand_loop u =
      let%lwt u = u in
      match (Set.min_elt u, !steps < !Config.num_expansions_check) with
      | Some t0, true -> (
          let tset, u' = Expand.simple t0 in
          let%lwt check_result = check_bounded_sol (return SmtLib.Unknown) (Set.elements tset) in
          steps := !steps + Set.length tset;
          match check_result with
          | Sat -> return SmtLib.Sat
          | _ -> expand_loop (return (Set.union (Set.remove u t0) u')))
      | None, true ->
          Log.verbose_msg "Bounded checking is complete.";
          (* All expansions have been checked. *)
          return SmtLib.Unsat
      | _, false ->
          (* Check reached limit. *)
          if !Config.no_bounded_sat_as_unsat then return SmtLib.Unsat else return SmtLib.Unknown
    in

    let%lwt _ = starter in
    (* TODO : logic *)
    let%lwt () = Asyncs.set_logic solver "LIA" in
    let%lwt () = Asyncs.load_min_max_defs solver in
    let%lwt res = expand_loop (return (TermSet.singleton ctex.ctex_eqn.eterm)) in
    let%lwt () = Asyncs.close_solver solver in
    return res
  in
  Asyncs.(cancellable_task (make_z3_solver ()) task)

let satisfies_tinv ~(p : psi_def) (tinv : PMRS.t) (ctex : ctex) :
    [ `Fst of ctex | `Snd of ctex | `Trd of ctex ] =
  let resp =
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
    with End_of_file ->
      Log.error_msg "Solvers terminated unexpectedly  ⚠️ .";
      Log.error_msg "Please inspect logs.";
      SmtLib.Unknown
  in
  Log.verbose (fun frmt () ->
      if Solvers.is_unsat resp then
        Fmt.(pf frmt "(%a)@;<1 4>does not satisfy \"%s\"." (box pp_ctex) ctex tinv.pvar.vname)
      else if Solvers.is_sat resp then
        Fmt.(pf frmt "(%a) satisfies %s." (box pp_ctex) ctex tinv.pvar.vname)
      else Fmt.(pf frmt "%s-satisfiability of (%a) is unknown." tinv.pvar.vname (box pp_ctex) ctex));
  match resp with Sat -> `Fst ctex | Unsat -> `Snd ctex | _ -> `Trd ctex

(** Classify counterexamples into positive or negative counterexamples with respect
    to the Tinv predicate in the problem.
*)
let classify_ctexs ~(p : psi_def) (ctexs : ctex list) : ctex list * ctex list * ctex list =
  let classify_with_tinv tinv =
    (* TODO: DT_LIA for z3, DTLIA for cvc4... Should write a type to represent logics. *)
    let f (ctex : ctex) = satisfies_tinv ~p tinv ctex in
    let positives, negatives, unknowns = List.partition3_map ~f ctexs in
    (positives, negatives, unknowns)
  in
  match p.psi_tinv with Some tinv -> classify_with_tinv tinv | None -> ([], [], [])
