open Base
open Common
open Env
open ProblemDefs
open Lang
open Term
open Syguslib.Sygus
open Utils

let rec refinement_loop
    ?(major = true)
    ~(ctx : env)
    (p : PsiDef.t)
    (t_lstate_in : refinement_loop_state Lwt.t)
    : solver_response segis_response Lwt.t
  =
  (* Check there is no termination order. *)
  let%lwt lstate_in = t_lstate_in in
  let tsize, usize = Set.length lstate_in.t_set, Set.length lstate_in.u_set in
  if major
  then (
    incr_refinement ctx;
    ctx.secondary_refinement_steps := 0;
    Stats.log_new_major_step ~tsize ~usize ())
  else incr_secondary_refinement ctx;
  (* Output status information before entering process. *)
  let elapsed = Stats.get_glob_elapsed () in
  if !Config.info
  then AlgoLog.show_steps "se2gis" ctx tsize usize
  else if !Config.Optims.max_solutions <= 0
  then AlgoLog.show_stat_refinement_step ctx elapsed tsize usize;
  (* Add lemmas interactively if the option is set. *)
  let () =
    if !Config.interactive_lemmas
    then LemmasInteractive.add_lemmas ~ctx ~p lstate_in
    else ()
  in
  (* First, generate the set of constraints corresponding to the set of terms t_set. *)
  let eqns, lifting =
    Equations.make
      ~count_reused_predicates:true
      ~ctx
      ~p
      ~lifting:lstate_in.lifting
      lstate_in.t_set
  in
  (* The solve the set of constraints with the assumption equations. *)
  let%lwt synth_time, (s_resp, solution) =
    Stats.lwt_timed (fun a ->
        Lwt.bind a (fun _ -> Equations.solve ctx ~p (eqns @ lstate_in.assumptions)))
  in
  match s_resp, solution with
  | RSuccess _, First sol ->
    (try
       success_case ~ctx ~p ~synth_time lstate_in (tsize, usize) lifting (eqns, sol)
     with
    | Failure s ->
      Log.error_msg Fmt.(str "Failure: %s" s);
      Log.error_msg "Solution cannot be proved correct, solver failed.";
      failure_case "proving step" RFail
    | e -> raise e)
  (* Synthesis returned unknown, which is interpreted as failure. *)
  | RUnknown, _ ->
    Log.error_msg Fmt.(str "Unknown");
    failure_case "candidate solution synthesis" RUnknown
  (* Synthesis failed, the loop returns failure. *)
  | RFail, Second [] ->
    Log.error_msg Fmt.(str "Failure.");
    failure_case "candidate solution synthesis" RFail
  (* Synthesis returns some information about unrealizability. *)
  | _ as synt_failure_info ->
    (* On synthesis failure, start by trying to synthesize lemmas. *)
    (match%lwt
       Stats.lwt_timed (fun a ->
           Lwt.bind a (fun _ ->
               LemmaSynthesis.synthesize_lemmas ~ctx ~p synt_failure_info lstate_in))
     with
    (* Some unrealizability witnesses were spurious and lemmas were synthesized.
     The refinement loop continues. *)
    | lsynt_time, Ok (First new_lstate) ->
      Stats.log_minor_step ~synth_time ~auxtime:lsynt_time false;
      refinement_loop ~ctx ~major:false p (Lwt.return new_lstate)
    (* The witnesses were not spurious, so the problem is unrealizable. We can attempt lifting.
        First, we root cause why the problem is unrealizable.
      *)
    | lsynt_time, Ok (Second witnesses) ->
      (match RootCausing.find_repair ~ctx ~p witnesses with
      | Lift
        when !Config.Optims.attempt_lifting
             && Lifting.lifting_count ~ctx p < !Config.Optims.max_lifting_attempts ->
        (match Lifting.scalar ~ctx ~p lstate_in synt_failure_info with
        | Ok (p', lstate') ->
          Lifting.msg_lifting ();
          Stats.log_minor_step ~synth_time ~auxtime:lsynt_time true;
          refinement_loop ~ctx ~major:false p' (Lwt.return lstate')
        | Error r' ->
          (* Infeasible is not a failure! *)
          (match r' with
          | RInfeasible ->
            unrealizable_w_witnesses_case (synth_time, 0.) (tsize, usize) Lift witnesses
          | _ -> failure_case "lifting" r'))
      | repair ->
        (* The problem is infeasible, and we have witnesses for it (and possiblt a repair) *)
        unrealizable_w_witnesses_case (synth_time, 0.) (tsize, usize) repair witnesses)
    | _ ->
      (* The problem is infeasible. When the sygus solver answers infeasible,
        we do not have witnesses of unrealizability.
       *)
      failure_case "lemma synthesis" RFail)

and failure_case s r = Lwt.return (Failed (s, r))

and unrealizable_w_witnesses_case (synth_time, verif_time) (tsize, usize) repair witnesses
  =
  Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:usize false;
  Lwt.return (Unrealizable (repair, witnesses))

and success_case ~ctx ~p ~synth_time lstate_in (tsize, usize) lifting (eqns, sol) =
  (* The solution is verified with a bounded check.  *)
  let%lwt verif_time, check_r =
    Stats.lwt_timed (fun a ->
        Lwt.bind a (fun _ -> Verify.check_solution ~ctx ~p lstate_in sol))
  in
  match check_r with
  | `witnesss (t_set, u_set) ->
    (* If check_r is some new set of MR-terms t_set, and terms u_set, this means
                  verification failed. The generalized counterexamples have been added to new_t_set,
                  which is also a superset of t_set.
               *)
    ctx >- AlgoLog.show_counterexamples lstate_in t_set;
    Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:usize false;
    let lstate =
      if !Config.Optims.make_partial_correctness_assumption
      then Equations.update_assumptions ~ctx ~p lstate_in sol t_set
      else lstate_in
    in
    (* Continue looping with the new sets. *)
    refinement_loop ~ctx ~major:true p (Lwt.return { lstate with t_set; u_set; lifting })
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
      refinement_loop ~ctx ~major:true p (Lwt.return lstate_in))
    else failure_case "candidate solution verification" RFail
  | `Correct ->
    (* This case happens when verification succeeded.
                  Store the equation system, return the solution. *)
    Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:usize true;
    Common.ProblemDefs.solved_eqn_system := Some eqns;
    Log.print_ok ();
    Lwt.return
      (Realizable
         { soln_rec_scheme = p.PsiDef.target
         ; soln_implems = ctx >- Analysis.rename_nicely sol
         })
;;

let se2gis ~(ctx : env) (p : PsiDef.t) : solver_response segis_response Lwt.t =
  let%lwt _ = Lwt.return () in
  (* Initialize sets with the most general terms. *)
  let t_set, u_set =
    if !Config.Optims.simple_init
    then (
      let x0 =
        mk_var
          ctx.ctx
          (Variable.mk ctx.ctx ~t:(Some (get_theta ctx)) (Alpha.fresh ctx.ctx.names))
      in
      let s = TermSet.of_list (ctx >- Analysis.expand_once x0) in
      Set.partition_tf ~f:(Expand.is_mr_all ~ctx p) s)
    else (
      let init_set = MGT.most_general_terms ctx p.PsiDef.target in
      Set.fold init_set ~init:(TermSet.empty, TermSet.empty) ~f:(fun (t, u) mgt ->
          let t', u' = Expand.to_maximally_reducible ~ctx p mgt in
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
    Lwt.return (Failed ("empty set of terms", RFail)))
  else (
    ctx.refinement_steps := 0;
    refinement_loop
      ~ctx
      p
      (Lwt.return { t_set; u_set; lifting = Lifting.empty_lifting; assumptions = [] }))
;;

(* ============================================================================================= *)
(*                                                 MAIN ENTRY POINTS                             *)
(* ============================================================================================= *)

let solve_problem ~(ctx : env) (synthesis_problem : PsiDef.t)
    : solver_response segis_response Lwt.t
  =
  (* Solve the problem using portofolio of techniques. *)
  se2gis ~ctx synthesis_problem
;;

let find_and_solve_problem
    ~(ctx : env)
    ~(filename : string)
    (psi_comps : (string * string * string) option)
    (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t)
    : (PsiDef.t * Syguslib.Sygus.solver_response segis_response) list Lwt.t
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
    Common.ProblemFinder.find_problem_components
      ~filename
      ~ctx
      (target_fname, spec_fname, repr_fname)
      pmrs
  in
  let main_algo =
    if !Config.Optims.use_segis (* Symbolic CEGIS. *)
    then Baselines.algo_segis ~ctx
    else if !Config.Optims.use_cegis (* Concrete CEGIS. *)
    then
      Baselines.algo_cegis ~ctx
      (* Default algorithm: best combination of techniques. *)
    else solve_problem ~ctx
  in
  let%lwt sol_or_witnesss = main_algo top_userdef_problem in
  Lwt.return [ top_userdef_problem, sol_or_witnesss ]
;;
