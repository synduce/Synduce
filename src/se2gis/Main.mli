open Common
open ProblemDefs
open Syguslib
open Lang

(* Solve a synthesis problem, as defined by a structure of type [psi_def]. The method
may use the best portoflio of techniques to solve the problem, with the goal to find whether
the problem is realizable or not.
 *)
val solve_problem : ctx:Env.env -> PsiDef.t -> Sygus.solver_response segis_response Lwt.t

(**
  [find_and_solve_problem ctx (Some (target, reference, representation))] solves the synthesis problem
  associated with the target function named [target], the reference function named
  [reference] and the representation function named [representation] that have
  been parsed in the file.
  If None is passed as argument, the default values are ("target", "spec", "repr").
  If the functions cannot be found, it will exit the program.
  To allow the program to search for better recursion skeletons, use
  [ManyProgramRefinement.find_and_solve_problem]
*)
val find_and_solve_problem
  :  ctx:Env.env
  -> filename:string
  -> (string * string * string) option
  -> (string, PMRS.t, Base.String.comparator_witness) Base.Map.t
  -> (PsiDef.t * Syguslib.Sygus.solver_response segis_response) list Lwt.t
