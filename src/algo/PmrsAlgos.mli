open Base
open Lang
open Syguslib

(* Solve a synthesis problem.
    `solve_problem (target, reference, repr) functions` solves the synthesis problem
    that consists in implementing the unknowns found in the pmrs named target in the
    functions such that it is equivalent to (reference ○ repr).
    If None is passed as first argument, the function looks for PMRS named "target",
    "spec" and "repr" in the `functions` map.
 *)
val solve_problem :
  (string * string * string) option ->
  (string, PMRS.t, String.comparator_witness) Map.t ->
  AState.psi_def * (AState.soln, Sygus.solver_response) Result.t
