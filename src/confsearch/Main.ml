open Base
open Common
open Env
open ProblemDefs
open Lang
open Utils

let find_and_solve_problem
    ~(ctx : env)
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
    ProblemFinder.find_problem_components ~ctx (target_fname, spec_fname, repr_fname) pmrs
  in
  let main_algo =
    if !Config.Optims.use_segis (* Symbolic CEGIS. *)
    then Se2gis.Baselines.algo_segis
    else if !Config.Optims.use_cegis (* Concrete CEGIS. *)
    then
      Se2gis.Baselines.algo_cegis
      (* Default algorithm: best combination of techniques. *)
    else Se2gis.Main.solve_problem
  in
  (* Solve the problem. *)
  let problems =
    if !Config.Optims.max_solutions < 0
    then [ top_userdef_problem ]
    else ctx >- PEnum.enumerate_p top_userdef_problem
  in
  let rec f (i, sols, fails) l =
    if List.length sols >= max 1 !Config.Optims.max_solutions
    then sols, fails
    else (
      match l with
      | low_problem :: tl ->
        ProblemFinder.update_context ~ctx low_problem;
        ctx >- AlgoLog.show_new_rskel i low_problem;
        let maybe_solution = main_algo ~ctx low_problem in
        (* Print state and save. *)
        LogJson.save_stats_and_restart low_problem.id;
        (match maybe_solution with
        | Realizable soln -> f (i + 1, (low_problem, Realizable soln) :: sols, fails) tl
        | _ as res -> f (i + 1, sols, (low_problem, res) :: fails) tl)
      | _ -> sols, fails)
  in
  let solutions, unrealized_or_failed = f (1, [], []) problems in
  List.rev solutions @ List.rev unrealized_or_failed
;;
