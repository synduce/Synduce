(** [Baselines] contains two alternative synthesis algorithms:
  [segis_loop] is a refinement loop using symbolic counterexample without partial bounding,
  [cegis_loop] is a refinement loop using concrete couternexamples (and therefore fully
  bounded counterexamples).
*)

open Base
open Lang
open Lang.Term
open Utils
open AState
open Syguslib.Sygus

(* ============================================================================================= *)
(*                             SEGIS REFINEMENT LOOP                                            *)
(* ============================================================================================= *)

(** [segis_loop p t_set] solves the synthesis problem defined by [p] starting with the set of
    equations [Equations.make t]. The terms in [t] must be bounded terms, and only bounded terms
    will be taken as counterexamples during the refinement loop.
    Use the option [--segis] in the executable to use this synthesis algorithm.
*)
let rec segis_loop (ctx : Context.t) (p : PsiDef.t) (t_set : TermSet.t)
    : solver_response segis_response
  =
  Int.incr refinement_steps;
  if !refinement_steps > !Config.refinement_rounds_warning_limit
     && Config.Optims.some_eager_optim_on ()
  then (
    Log.info (fun frmt () ->
        Fmt.pf frmt "Turning off eager optimizations after %i rounds." !refinement_steps);
    Config.Optims.turn_off_eager_optims ());
  let elapsed = Stats.get_glob_elapsed () in
  Log.info (fun frmt () -> Fmt.pf frmt "Refinement step %i." !refinement_steps);
  let tsize = Set.length t_set
  and usize = 0 in
  Stats.log_new_major_step ~tsize ~usize ();
  if !Config.info
  then AlgoLog.show_steps tsize usize
  else AlgoLog.show_stat elapsed tsize usize;
  Log.debug_msg
    Fmt.(str "<SEGIS> Start refinement loop with %i terms in T." (Set.length t_set));
  (* Start of the algorithm. *)
  let eqns, _ =
    Equations.make
      ~force_replace_off:true
      ~p
      ~term_state:Lemmas.empty_term_state
      ~lifting:Lifting.empty_lifting
      t_set
  in
  let synth_time, (s_resp, solution) =
    Stats.timed (fun () -> Equations.solve ctx ~p eqns)
  in
  match s_resp, solution with
  | RSuccess _, First sol ->
    (match Stats.timed (fun () -> Verify.bounded_check ~p sol) with
    (* A symbolic counterexample term is returned. *)
    | verif_time, Some eqn ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:0 false;
      Log.debug (fun frmt () ->
          Fmt.(
            pf
              frmt
              "@[<hov 2><SEGIS> Counterexample term:@;@[<hov 2>%a@]"
              pp_term
              eqn.eterm));
      segis_loop ctx p (Set.add t_set eqn.eterm)
    | verif_time, None ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:0 true;
      Log.print_ok ();
      Realizable { soln_rec_scheme = p.PsiDef.target; soln_implems = sol })
  | RInfeasible, Second ctexs ->
    Stats.log_major_step_end ~synth_time ~verif_time:0. ~t:tsize ~u:0 false;
    Log.info
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "@[<hov 2><SEGIS> This problem has no solution. Counterexample set:@;%a@]"
            (list ~sep:sp pp_term)
            (Set.elements t_set));
    Unrealizable ctexs
  | RFail, _ ->
    Log.error_msg "<SEGIS> SyGuS solver failed to find a solution.";
    Failed RFail
  | RUnknown, _ ->
    Log.error_msg "<SEGIS> SyGuS solver returned unknown.";
    Failed RUnknown
  | _ -> Failed s_resp
;;

let algo_segis (ctx : Context.t) (p : PsiDef.t) =
  let t_set = TermSet.of_list (Analysis.terms_of_max_depth 1 !AState._theta) in
  refinement_steps := 0;
  segis_loop (Context.subctx ctx "segis") p t_set
;;

(* ============================================================================================= *)
(*                             CEGIS REFINEMENT LOOP                                            *)
(* ============================================================================================= *)

(** [cegis_loop p t_set] solves the synthesis problem defined by [p] starting with the set of
    equations [Equations.make t]. The terms in [t] must be *concrete* terms, and only concrete terms
    will be taken as counterexamples during the refinement loop: this is a concrete CEGIS algorithm.
    Use the option [--cegis] in the executable to use this synthesis algorithm.
*)
let rec cegis_loop (ctx : Context.t) (p : PsiDef.t) (t_set : TermSet.t)
    : solver_response segis_response
  =
  Int.incr refinement_steps;
  if !refinement_steps > !Config.refinement_rounds_warning_limit
  then (
    Log.info (fun frmt () ->
        Fmt.pf frmt "Turning off eager optimizations after %i rounds." !refinement_steps);
    Config.Optims.turn_off_eager_optims ());
  let elapsed = Stats.get_glob_elapsed () in
  Log.info (fun frmt () -> Fmt.pf frmt "Refinement step %i." !refinement_steps);
  let tsize = Set.length t_set in
  Stats.log_new_major_step ~tsize ~usize:0 ();
  if !Config.info then AlgoLog.show_steps tsize 0 else AlgoLog.show_stat elapsed tsize 0;
  Log.debug_msg
    Fmt.(str "<CEGIS> Start refinement loop with %i terms in T." (Set.length t_set));
  (* Start of the algorithm. *)
  let eqns, _ =
    Equations.make
      ~force_replace_off:true
      ~p
      ~term_state:Lemmas.empty_term_state
      ~lifting:Lifting.empty_lifting
      t_set
  in
  let synth_time, (s_resp, solution) =
    Stats.timed (fun () -> Equations.solve ctx ~p eqns)
  in
  match s_resp, solution with
  | RSuccess _, First sol ->
    (match
       Stats.timed (fun () -> Verify.bounded_check ~use_concrete_ctex:true ~p sol)
     with
    (* A concrete conterexample term is returned. *)
    | verif_time, Some eqn ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:(Set.length t_set) ~u:0 false;
      Log.debug (fun frmt () ->
          Fmt.(
            pf
              frmt
              "@[<hov 2><CEGIS> Counterexample term:@;@[<hov 2>%a@]"
              pp_term
              eqn.eterm));
      cegis_loop ctx p (Set.add t_set eqn.eterm)
    | verif_time, None ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:0 true;
      Log.print_ok ();
      Realizable { soln_rec_scheme = p.PsiDef.target; soln_implems = sol })
  | RInfeasible, Second ctexs ->
    Stats.log_major_step_end ~synth_time ~verif_time:0. ~t:tsize ~u:0 false;
    Log.info
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "@[<hov 2><CEGIS> This problem has no solution. Counterexample set:@;%a@]"
            (list ~sep:sp pp_term)
            (Set.elements t_set));
    Unrealizable ctexs
  | RFail, _ ->
    Log.error_msg "<CEGIS> SyGuS solver failed to find a solution.";
    Failed RFail
  | RUnknown, _ ->
    Log.error_msg "<CEGIS> SyGuS solver returned unknown.";
    Failed RUnknown
  | _ -> Failed s_resp
;;

let algo_cegis (ctx : Context.t) (p : PsiDef.t) =
  let t_set =
    TermSet.of_list
      (List.map ~f:Analysis.concretize (Analysis.terms_of_max_depth 1 !AState._theta))
  in
  refinement_steps := 0;
  cegis_loop (Context.subctx ctx "cegis") p t_set
;;
