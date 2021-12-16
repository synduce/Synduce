open Syguslib

(* Solve a synthesis problem, as defined by a structure of type [psi_def]. The method
may use the best portoflio of techniques to solve the problem, with the goal to find whether
the problem is realizable or not.
 *)
val solve_problem : AState.psi_def -> Sygus.solver_response AState.segis_response

(**
  [find_and_solve_problem (Some (target, reference, representation))] solves the synthesis problem
  associated with the target function named [target], the reference function named
  [reference] and the representation function named [representation] that have
  been parsed in the file.
  If None is passed as argument, the default values are ("target", "spec", "repr").
  If the functions cannot be found, it will exit the program.
  To allow the program to search for better recursion skeletons, use
  [ManyProgramRefinement.find_and_solve_problem]
*)
val find_and_solve_problem
  :  (string * string * string) option
  -> (string, Lang.PMRS.t, Base.String.comparator_witness) Base.Map.t
  -> (AState.psi_def * Syguslib.Sygus.solver_response AState.segis_response) list
