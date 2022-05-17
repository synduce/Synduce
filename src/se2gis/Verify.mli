open Common
open ProblemDefs
open Base
open Lang
open Lang.Term

(**
   `check_solution ~p (t,u) soln`

   Checks if ᴪ(p, soln) is valid, assuming `soln` has been synthesized using the term set `t` with
   expansion continuation `u`.
   Returns the set (t,u) for the next step in the witness-guided refinement loop.
   This bounded checking procedure takes advantage of the only partial bounding, and does
   not perform bounded checking by expanding unnecessary terms.
*)
val check_solution
  :  ctx:Env.env
  -> p:Common.ProblemDefs.PsiDef.t
  -> refinement_loop_state
  -> (string * variable list * term) list
  -> [ `Incorrect_assumptions
     | `witnesss of
       (term, Terms.comparator_witness) Set.t * (term, Terms.comparator_witness) Set.t
     | `Correct
     ]
     Lwt.t

(** Perform a bounded check of the solution. As opposed to check_solution this does not take advantage
    of partial bounding techniques.
    Consequently, it does not require sets of terms as arguments but only a problem definition and a solution.
    Returns the first equation for which checking has failed (the first element is the counterexample).
    Returns None if the check passed.
*)
val bounded_check
  :  ?use_concrete_witness:bool
  -> ctx:Env.env
  -> p:Common.ProblemDefs.PsiDef.t
  -> (string * variable list * term) list
  -> Common.ProblemDefs.equation option Lwt.t

(**
  Solve an equation of the form f(x1,..,xn) = c where f is a recursive function and t is a constant.
  Returns Some list of terms, one for each input of the function f, if the equation admits a solution.
  Returns None if the equation has no solution.
 *)
val invert : ctx:Env.env -> PMRS.t -> Constant.t -> term list option Lwt.t
