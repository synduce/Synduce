open AState
open Base
open Lang
open Lang.Term
open Syguslib.Sygus
open Utils
open Env

let rec refinement_loop
    ?(major = true)
    ~(ctx : env)
    (tctx : ThreadContext.t)
    (p : PsiDef.t)
    (lstate_in : refinement_loop_state)
    : solver_response segis_response
  =
  (* Check there is no termination order. *)
  ThreadContext.check tctx;
  let tsize, usize = Set.length lstate_in.t_set, Set.length lstate_in.u_set in
  if major
  then (
    Int.incr refinement_steps;
    secondary_refinement_steps := 0;
    Stats.log_new_major_step ~tsize ~usize ())
  else Int.incr secondary_refinement_steps;
  (* Output status information before entering process. *)
  let elapsed = Stats.get_glob_elapsed () in
  if !Config.info
  then AlgoLog.show_steps tsize usize
  else AlgoLog.show_stat elapsed tsize usize;
  (* Add lemmas interactively if the option is set. *)
  let lstate =
    if !Config.interactive_lemmas
    then ctx >>- Lemmas.Interactive.add_lemmas ~p lstate_in
    else lstate_in
  in
  (* First, generate the set of constraints corresponding to the set of terms t_set. *)
  let eqns, lifting =
    Equations.make
      ~ctx
      ~p
      ~term_state:lstate.term_state
      ~lifting:lstate.lifting
      lstate.t_set
  in
  (* Check there is no termination order. *)
  ThreadContext.check tctx;
  (* The solve the set of constraints with the assumption equations. *)
  let synth_time, (s_resp, solution) =
    Stats.timed (fun () -> Equations.solve ctx ~p (eqns @ lstate.assumptions))
  in
  match s_resp, solution with
  | RSuccess _, First sol ->
    (* Synthesis has succeeded, now we need to verify the solution. *)
    (try
       (* The solution is verified with a bounded check.  *)
       let verif_time, check_r =
         Stats.timed (fun () -> Verify.check_solution ~ctx ~p lstate sol)
       in
       match check_r with
       | `Ctexs (t_set, u_set) ->
         (* If check_r is some new set of MR-terms t_set, and terms u_set, this means
               verification failed. The generalized counterexamples have been added to new_t_set,
               which is also a superset of t_set.
            *)
         ctx >- AlgoLog.show_counterexamples lstate t_set;
         Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:usize false;
         let lstate =
           if !Config.Optims.make_partial_correctness_assumption
           then Equations.update_assumptions ~ctx ~p lstate sol t_set
           else lstate
         in
         (* Continue looping with the new sets. *)
         refinement_loop ~ctx ~major:true tctx p { lstate with t_set; u_set; lifting }
       | `Incorrect_assumptions ->
         if !Config.Optims.use_syntactic_definitions
            || !Config.Optims.make_partial_correctness_assumption
         then (
           (* The tool might have made some incorrect assumptions. *)
           AlgoLog.msg_too_many_opts ();
           Stats.log_major_step_end
             ~failure_step:true
             ~synth_time
             ~verif_time
             ~t:tsize
             ~u:usize
             false;
           Config.Optims.turn_off_eager_optims ();
           refinement_loop ~ctx ~major tctx p lstate_in)
         else Failed RFail
       | `Correct ->
         (* This case happens when verification succeeded.
               Store the equation system, return the solution. *)
         Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:usize true;
         AState.solved_eqn_system := Some eqns;
         Log.print_ok ();
         Realizable
           { soln_rec_scheme = p.PsiDef.target
           ; soln_implems = ctx >- Analysis.rename_nicely sol
           }
     with
    | Failure s ->
      Log.error_msg Fmt.(str "Failure: %s" s);
      Log.error_msg "Solution cannot be proved correct, solver failed.";
      Failed RFail
    | e -> raise e)
  | RUnknown, _ -> Failed RUnknown
  | _ as synt_failure_info ->
    (* On synthesis failure, start by trying to synthesize lemmas. *)
    (match
       Stats.timed (fun () ->
           ctx >>- Lemmas.synthesize_lemmas ~p synt_failure_info lstate)
     with
    | lsynt_time, Ok (First new_lstate) ->
      Stats.log_minor_step ~synth_time ~auxtime:lsynt_time false;
      refinement_loop ~ctx ~major:false tctx p new_lstate
    | lsynt_time, Ok (Second ctexs)
      when !Config.Optims.attempt_lifting
           && ctx >- Lifting.lifting_count p < !Config.Optims.max_lifting_attempts ->
      (* If all no counterexample is spurious, lemma synthesis fails, we need lifting. *)
      (match ctx >- Lifting.scalar ~p lstate synt_failure_info with
      | Ok (p', lstate') ->
        Lifting.msg_lifting ();
        Stats.log_minor_step ~synth_time ~auxtime:lsynt_time true;
        refinement_loop ~ctx ~major:false tctx p' lstate'
      | Error r' ->
        (* Infeasible is not a failure! *)
        (match r' with
        | RInfeasible ->
          Stats.log_major_step_end ~synth_time ~verif_time:0. ~t:tsize ~u:usize false;
          Unrealizable ctexs
        | _ -> Failed r'))
    | _, Ok (Second ctexs) ->
      (* Infeasible is not a failure! When the sygus solver answers infeasible,
        we do not have witnesses of unrealizability.
       *)
      Stats.log_major_step_end ~synth_time ~verif_time:0. ~t:tsize ~u:usize false;
      Unrealizable ctexs
    | _ -> Failed RFail)
;;

let se2gis ~(ctx : env) (tctx : ThreadContext.t) (p : PsiDef.t) =
  (* Initialize sets with the most general terms. *)
  let t_set, u_set =
    if !Config.Optims.simple_init
    then (
      let x0 =
        mk_var
          ctx.ctx
          (Variable.mk ctx.ctx ~t:(Some !AState._theta) (Alpha.fresh ctx.ctx.names))
      in
      let s = TermSet.of_list (ctx >- Analysis.expand_once x0) in
      Set.partition_tf ~f:(ctx >>- Expand.is_mr_all p) s)
    else (
      let init_set = MGT.most_general_terms ctx.functions ctx.ctx p.PsiDef.target in
      Set.fold init_set ~init:(TermSet.empty, TermSet.empty) ~f:(fun (t, u) mgt ->
          let t', u' = ctx >>- Expand.to_maximally_reducible p mgt in
          Set.union t t', Set.union u u'))
  in
  Log.debug (fun frmt () ->
      Fmt.(
        pf
          frmt
          "@[<hov 2>INIT = %a@]"
          (list ~sep:comma (pp_term ctx.ctx))
          (Set.elements t_set)));
  if Set.is_empty t_set
  then (
    Log.error_msg "Empty set of terms for equation system.";
    Failed RFail)
  else (
    refinement_steps := 0;
    refinement_loop
      ~ctx
      tctx
      p
      { t_set
      ; u_set
      ; term_state = Lemmas.empty_term_state
      ; lifting = Lifting.empty_lifting
      ; assumptions = []
      })
;;

(* ============================================================================================= *)
(*                                                 MAIN ENTRY POINTS                             *)
(* ============================================================================================= *)

let solve_problem ~(ctx : env) (tctx : ThreadContext.t) (synthesis_problem : PsiDef.t)
    : solver_response segis_response
  =
  (* Solve the problem using portofolio of techniques. *)
  try se2gis ~ctx tctx synthesis_problem with
  | ThreadContext.Escape ->
    Utils.Log.debug_msg "se2gis run was terminated early.";
    Failed RFail
;;

let find_and_solve_problem
    ~(ctx : env)
    (tctx : ThreadContext.t)
    (psi_comps : (string * string * string) option)
    (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t)
    : (PsiDef.t * Syguslib.Sygus.solver_response segis_response) list
  =
  (*  Find problem components *)
  let target_fname, spec_fname, repr_fname =
    match psi_comps with
    | Some names -> names
    | None ->
      Utils.Log.debug_msg "Using default names.";
      "target", "spec", "repr"
  in
  let top_userdef_problem =
    ctx
    >>- ProblemFinder.find_problem_components (target_fname, spec_fname, repr_fname) pmrs
  in
  let main_algo =
    if !Config.Optims.use_segis (* Symbolic CEGIS. *)
    then fun t -> Baselines.algo_segis ~ctx ~t
    else if !Config.Optims.use_cegis (* Concrete CEGIS. *)
    then
      fun t ->
      Baselines.algo_cegis ~ctx ~t
      (* Default algorithm: best combination of techniques. *)
    else solve_problem ~ctx
  in
  [ top_userdef_problem, main_algo tctx top_userdef_problem ]
;;
