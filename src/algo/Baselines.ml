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
let rec segis_loop (p : psi_def) (t_set : TermSet.t) =
  Int.incr refinement_steps;
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
  let synth_time, (s_resp, solution) = Stats.timed (fun () -> Equations.solve ~p eqns) in
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
      segis_loop p (Set.add t_set eqn.eterm)
    | verif_time, None ->
      Stats.log_major_step_end ~synth_time ~verif_time ~t:tsize ~u:0 true;
      Log.print_ok ();
      Ok { soln_rec_scheme = p.psi_target; soln_implems = sol })
  | RFail, _ ->
    Log.error_msg "<SEGIS> SyGuS solver failed to find a solution.";
    Error RFail
  | RInfeasible, _ ->
    Stats.log_major_step_end ~synth_time ~verif_time:0. ~t:tsize ~u:0 false;
    Log.info
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "@[<hov 2><SEGIS> This problem has no solution. Counterexample set:@;%a@]"
            (list ~sep:sp pp_term)
            (Set.elements t_set));
    Error RInfeasible
  | RUnknown, _ ->
    Log.error_msg "<SEGIS> SyGuS solver returned unknown.";
    Error RUnknown
  | _ -> Error s_resp
;;

let algo_segis (p : psi_def) =
  (* In segis, checking for unrealizability may return errors. *)
  (* Config.check_unrealizable := false; *)
  Config.check_unrealizable_smt_unsatisfiable := false;
  let t_set = TermSet.of_list (Analysis.terms_of_max_depth 1 !AState._theta) in
  refinement_steps := 0;
  segis_loop p t_set
;;

(* ============================================================================================= *)
(*                             CEGIS REFINEMENT LOOP                                            *)
(* ============================================================================================= *)

(** [cegis_loop p t_set] solves the synthesis problem defined by [p] starting with the set of
    equations [Equations.make t]. The terms in [t] must be *concrete* terms, and only concrete terms
    will be taken as counterexamples during the refinement loop: this is a concrete CEGIS algorithm.
    Use the option [--cegis] in the executable to use this synthesis algorithm.
*)
let rec cegis_loop (p : psi_def) (t_set : TermSet.t) =
  Int.incr refinement_steps;
  let elapsed = Stats.get_glob_elapsed () in
  Log.info (fun frmt () -> Fmt.pf frmt "Refinement step %i." !refinement_steps);
  (if not !Config.info
  then
    Fmt.(
      pf
        stdout
        "%i,%3.3f,%3.3f,%i,0@."
        !refinement_steps
        (Stats.get_glob_elapsed ())
        elapsed
        (Set.length t_set)));
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
  let s_resp, solution = Equations.solve ~p eqns in
  match s_resp, solution with
  | RSuccess _, First sol ->
    (match Verify.bounded_check ~use_concrete_ctex:true ~p sol with
    (* A concrete conterexample term is returned. *)
    | Some eqn ->
      Log.debug (fun frmt () ->
          Fmt.(
            pf
              frmt
              "@[<hov 2><CEGIS> Counterexample term:@;@[<hov 2>%a@]"
              pp_term
              eqn.eterm));
      cegis_loop p (Set.add t_set eqn.eterm)
    | None ->
      Log.print_ok ();
      Ok { soln_rec_scheme = p.psi_target; soln_implems = sol })
  | RFail, _ ->
    Log.error_msg "<CEGIS> SyGuS solver failed to find a solution.";
    Error RFail
  | RInfeasible, _ ->
    Log.info
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "@[<hov 2><CEGIS> This problem has no solution. Counterexample set:@;%a@]"
            (list ~sep:sp pp_term)
            (Set.elements t_set));
    Error RInfeasible
  | RUnknown, _ ->
    Log.error_msg "<CEGIS> SyGuS solver returned unknown.";
    Error RUnknown
  | _ -> Error s_resp
;;

let algo_cegis (p : psi_def) =
  let t_set =
    TermSet.of_list
      (List.map ~f:Analysis.concretize (Analysis.terms_of_max_depth 1 !AState._theta))
  in
  refinement_steps := 0;
  cegis_loop p t_set
;;
