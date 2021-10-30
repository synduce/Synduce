open Base
open Lang
open Lang.Term
open Utils
open AState
open Syguslib.Sygus

(* ============================================================================================= *)
(*                             ACEGIS REFINEMENT LOOP                                            *)
(* ============================================================================================= *)

let rec acegis_loop (p : psi_def) (t_set : TermSet.t) =
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
        !Stats.verif_time
        elapsed
        (Set.length t_set)));
  Log.debug_msg
    Fmt.(str "<ACEGIS> Start refinement loop with %i terms in T." (Set.length t_set));
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
    (match Verify.bounded_check ~p sol with
    (* A symbolic counterexample term is returned. *)
    | Some eqn ->
      Log.debug (fun frmt () ->
          Fmt.(
            pf
              frmt
              "@[<hov 2><ACEGIS> Counterexample term:@;@[<hov 2>%a@]"
              pp_term
              eqn.eterm));
      acegis_loop p (Set.add t_set eqn.eterm)
    | None ->
      Log.print_ok ();
      Ok { soln_rec_scheme = p.psi_target; soln_implems = sol })
  | RFail, _ ->
    Log.error_msg "<ACEGIS> SyGuS solver failed to find a solution.";
    Error RFail
  | RInfeasible, _ ->
    Log.info
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "@[<hov 2><ACEGIS> This problem has no solution. Counterexample set:@;%a@]"
            (list ~sep:sp pp_term)
            (Set.elements t_set));
    Error RInfeasible
  | RUnknown, _ ->
    Log.error_msg "<ACEGIS> SyGuS solver returned unknown.";
    Error RUnknown
  | _ -> Error s_resp
;;

let algo_acegis (p : psi_def) =
  let t_set = TermSet.of_list (Analysis.terms_of_max_depth 1 !AState._theta) in
  refinement_steps := 0;
  acegis_loop p t_set
;;

(* ============================================================================================= *)
(*                             CCEGIS REFINEMENT LOOP                                            *)
(* ============================================================================================= *)

let rec ccegis_loop (p : psi_def) (t_set : TermSet.t) =
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
    Fmt.(str "<CCEGIS> Start refinement loop with %i terms in T." (Set.length t_set));
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
              "@[<hov 2><CCEGIS> Counterexample term:@;@[<hov 2>%a@]"
              pp_term
              eqn.eterm));
      ccegis_loop p (Set.add t_set eqn.eterm)
    | None ->
      Log.print_ok ();
      Ok { soln_rec_scheme = p.psi_target; soln_implems = sol })
  | RFail, _ ->
    Log.error_msg "<CCEGIS> SyGuS solver failed to find a solution.";
    Error RFail
  | RInfeasible, _ ->
    Log.info
      Fmt.(
        fun frmt () ->
          pf
            frmt
            "@[<hov 2><CCEGIS> This problem has no solution. Counterexample set:@;%a@]"
            (list ~sep:sp pp_term)
            (Set.elements t_set));
    Error RInfeasible
  | RUnknown, _ ->
    Log.error_msg "<CCEGIS> SyGuS solver returned unknown.";
    Error RUnknown
  | _ -> Error s_resp
;;

let algo_ccegis (p : psi_def) =
  let t_set =
    TermSet.of_list
      (List.map ~f:Analysis.concretize (Analysis.terms_of_max_depth 1 !AState._theta))
  in
  refinement_steps := 0;
  ccegis_loop p t_set
;;
