open Base
open Lang.Term

val check_solution :
  ?use_acegis:bool ->
  p:AState.psi_def ->
  TermSet.t * TermSet.t ->
  (string * variable list * term) list ->
  ((term, Terms.comparator_witness) Set.t * (term, Terms.comparator_witness) Set.t) option
(**
   `check_solution ~p (t,u) soln`

   Checks if ᴪ(p, soln) is valid, assuming `soln` has been synthesized using the term set `t` with
   expansion continuation `u`.
   Returns the set (t,u) for the next step in the ctex-guided refinement loop.
   This bounded checking procedure takes advantage of the only partial bounding, and does
   not perform bounded checking by expanding unnecessary terms.
*)

val bounded_check :
  ?concrete_ctex:bool ->
  p:AState.psi_def ->
  (string * variable list * term) list ->
  Equations.equation option
(** Perform a bounded check of the solution. As opposed to check_solution this does not take advantage
    of partial bounding techniques.
    Consequently, it does not require sets of terms as arguments but only a problem definition and a solution.
    Returns the first equation for which checking has failed (the first element is the counterexample).
    Returns None if the check passed.
*)
(*
type unrealizability_ctex =
  VarSet.t * (int, term, Int.comparator_witness) Map.t * (int, term, Int.comparator_witness) Map.t
(** A counterexample to realizability is a pair of models: a pair of maps from variable ids to terms. *)

val check_realizable : VarSet.t -> Equations.equation list -> unrealizability_ctex list
(** Check if system of equations defines a functionally realizable synthesis problem.
  If any equation defines an unsolvable problem, an unrealizability_ctex is added to the
  list of counterexamples to be returned.
  If the returned list is empty, the problem may be solvable.
  If the returned list is not empty, the problem is not solvable.
*) *)
