(** [Baselines] contains two alternative synthesis algorithms:
  [segis_loop] is a refinement loop using symbolic counterexample without partial bounding,
  [cegis_loop] is a refinement loop using concrete couternexamples (and therefore fully
  bounded counterexamples).
*)

open Base
open Common
open Env
open Lang
open Lang.Term
open Utils
open Common.ProblemDefs
open Syguslib.Sygus

(* ============================================================================================= *)
(*                             SEGIS REFINEMENT LOOP                                            *)
(* ============================================================================================= *)

(** [segis_loop p t_set] solves the synthesis problem defined by [p] starting with the set of
    equations [Equations.make t]. The terms in [t] must be bounded terms, and only bounded terms
    will be taken as counterexamples during the refinement loop.
    Use the option [--segis] in the executable to use this synthesis algorithm.
*)
let rec segis_loop ~(ctx : env) (p : PsiDef.t) (t_set : TermSet.t)
    : solver_response segis_response Lwt.t
  =
  incr_refinement ctx;
  if get_refinement_steps ctx > !Config.refinement_rounds_warning_limit
     && Config.Optims.some_eager_optim_on ()
  then (
    Log.info (fun frmt () ->
        Fmt.pf
          frmt
          "Turning off eager optimizations after %i rounds."
          (get_refinement_steps ctx));
    Config.Optims.turn_off_eager_optims ());
  let elapsed = Stats.get_glob_elapsed () in
  Log.info (fun frmt () -> Fmt.pf frmt "Refinement step %i." (get_refinement_steps ctx));
  let tsize = Set.length t_set
  and usize = 0 in
  Stats.log_new_major_step ~tsize ~usize ();
  (* Show intemediate refinement step information depending on mode. *)
  if !Config.info
  then AlgoLog.show_steps "segis" ctx tsize usize
  else if !Config.Optims.max_solutions <= 0
  then AlgoLog.show_stat_refinement_step ctx elapsed tsize usize;
  Log.debug_msg
    Fmt.(str "<SEGIS> Start refinement loop with %i terms in T." (Set.length t_set));
  (* Start of the algorithm. *)
  let eqns, _ =
    Equations.make
      ~count_reused_predicates:false
      ~ctx
      ~force_replace_off:true
      ~p
      ~lifting:Lifting.empty_lifting
      t_set
  in
  let%lwt synth_time, (s_resp, solution) =
    Stats.lwt_timed (fun a -> Lwt.bind a (fun _ -> Equations.solve ctx ~p eqns))
  in
  match s_resp, solution with
  | RSuccess _, First sol ->
    (match%lwt
       Stats.lwt_timed (fun a -> Lwt.bind a (fun _ -> Verify.bounded_check ~ctx ~p sol))
     with
    (* A symbolic counterexample term is returned. *)
    | verif_time, Some eqn ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:0 false;
      Log.debug (fun frmt () ->
          Fmt.(
            pf
              frmt
              "@[<hov 2><SEGIS> Counterexample term:@;@[<hov 2>%a@]"
              (pp_term ctx.ctx)
              eqn.eterm));
      segis_loop ~ctx p (Set.add t_set eqn.eterm)
    | verif_time, None ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:0 true;
      Log.print_ok ();
      Lwt.return (Realizable { soln_rec_scheme = p.PsiDef.target; soln_implems = sol }))
  | RInfeasible, Second witnesss ->
    Stats.log_major_step_end ~synth_time ~verif_time:0. ~t:tsize ~u:0 false;
    Log.info
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "@[<hov 2><SEGIS> This problem has no solution. Counterexample set:@;%a@]"
            (list ~sep:sp (pp_term ctx.ctx))
            (Set.elements t_set));
    Lwt.return (Unrealizable (NoRepair, witnesss))
  | RFail, _ ->
    Log.error_msg "<SEGIS> SyGuS solver failed to find a solution.";
    Lwt.return (Failed ("segis", RFail))
  | RUnknown, _ ->
    Log.error_msg "<SEGIS> SyGuS solver returned unknown.";
    failure_case "segis" RUnknown
  | _ -> failure_case "segis" s_resp

and failure_case s r = Lwt.return (Failed (s, r))

let algo_segis ~(ctx : env) (p : PsiDef.t) =
  let t_set =
    TermSet.of_list Env.(ctx >- Analysis.terms_of_max_depth 1 (get_theta ctx))
  in
  ctx.refinement_steps := 0;
  segis_loop ~ctx p t_set
;;

(* ============================================================================================= *)
(*                             CEGIS REFINEMENT LOOP                                            *)
(* ============================================================================================= *)

(** [cegis_loop p t_set] solves the synthesis problem defined by [p] starting with the set of
    equations [Equations.make t]. The terms in [t] must be *concrete* terms, and only concrete terms
    will be taken as counterexamples during the refinement loop: this is a concrete CEGIS algorithm.
    Use the option [--cegis] in the executable to use this synthesis algorithm.
*)
let rec cegis_loop ~(ctx : Env.env) (p : PsiDef.t) (t_set : TermSet.t)
    : solver_response segis_response Lwt.t
  =
  incr_refinement ctx;
  if get_refinement_steps ctx > !Config.refinement_rounds_warning_limit
  then (
    Log.info (fun frmt () ->
        Fmt.pf
          frmt
          "Turning off eager optimizations after %i rounds."
          (get_refinement_steps ctx));
    Config.Optims.turn_off_eager_optims ());
  let elapsed = Stats.get_glob_elapsed () in
  Log.info (fun frmt () -> Fmt.pf frmt "Refinement step %i." (get_refinement_steps ctx));
  let tsize = Set.length t_set in
  Stats.log_new_major_step ~tsize ~usize:0 ();
  if !Config.info
  then AlgoLog.show_steps "cegis" ctx tsize 0
  else if !Config.Optims.max_solutions <= 0
  then AlgoLog.show_stat_refinement_step ctx elapsed tsize 0;
  Log.debug_msg
    Fmt.(str "<CEGIS> Start refinement loop with %i terms in T." (Set.length t_set));
  (* Start of the algorithm. *)
  let eqns, _ =
    Equations.make
      ~count_reused_predicates:false
      ~ctx
      ~force_replace_off:true
      ~p
      ~lifting:Lifting.empty_lifting
      t_set
  in
  let%lwt synth_time, (s_resp, solution) =
    Stats.lwt_timed (fun a -> Lwt.bind a (fun () -> Equations.solve ctx ~p eqns))
  in
  match s_resp, solution with
  | RSuccess _, First sol ->
    (match%lwt
       Stats.lwt_timed (fun a ->
           Lwt.bind a (fun _ ->
               Verify.bounded_check ~ctx ~use_concrete_witness:true ~p sol))
     with
    (* A concrete conterexample term is returned. *)
    | verif_time, Some eqn ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:(Set.length t_set) ~u:0 false;
      Log.debug (fun frmt () ->
          Fmt.(
            pf
              frmt
              "@[<hov 2><CEGIS> Counterexample term:@;@[<hov 2>%a@]"
              (pp_term ctx.ctx)
              eqn.eterm));
      cegis_loop ~ctx p (Set.add t_set eqn.eterm)
    | verif_time, None ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:0 true;
      Log.print_ok ();
      Lwt.return (Realizable { soln_rec_scheme = p.PsiDef.target; soln_implems = sol }))
  | RInfeasible, Second witnesss ->
    Stats.log_major_step_end ~synth_time ~verif_time:0. ~t:tsize ~u:0 false;
    Log.info
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "@[<hov 2><CEGIS> This problem has no solution. Counterexample set:@;%a@]"
            (list ~sep:sp (pp_term ctx.ctx))
            (Set.elements t_set));
    Lwt.return (Unrealizable (NoRepair, witnesss))
  | RFail, _ ->
    Log.error_msg "<CEGIS> SyGuS solver failed to find a solution.";
    Lwt.return (Failed ("cegis", RFail))
  | RUnknown, _ ->
    Log.error_msg "<CEGIS> SyGuS solver returned unknown.";
    Lwt.return (Failed ("cegis", RUnknown))
  | _ -> Lwt.return (Failed ("cegis", s_resp))
;;

let algo_cegis ~(ctx : Env.env) (p : PsiDef.t) : solver_response segis_response Lwt.t =
  let t_set =
    TermSet.of_list
      (List.map
         ~f:(Analysis.concretize ~ctx:ctx.ctx)
         Env.(ctx >- Analysis.terms_of_max_depth 1 (get_theta ctx)))
  in
  ctx.refinement_steps := 0;
  cegis_loop ~ctx p t_set
;;
