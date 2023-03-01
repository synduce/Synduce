open Common
open ProblemDefs
open Env

type multi_soln_result =
  { r_best : (env * PsiDef.t * Syguslib.Sygus.solver_response segis_response) option
  ; r_all : (env * PsiDef.t * Syguslib.Sygus.solver_response segis_response) list
  ; r_subconf_count : int
  ; r_final_state : ConfGraph.state
  }

(** [find_and_solver_problem (target,spec,repr) globals] finds the PMRS
    components in the top-level functions [globals] with names [target], [spec]
    and [repr], and solves the synthesis problem associated with it. In order to
    solve it, it may attempt to modify the problem by changing the inputs of the
    unknowns or changing their output domain.
    Returns an integer representing the size of the possible search space and a list
    of results that are either solutions, unrealizablity results or failures.
*)
val find_and_solve_problem
  :  ctx:env
  -> filename:string
  -> (string * string * string) option
  -> (string, Lang.PMRS.t, Base.String.comparator_witness) Base.Map.t
  -> multi_soln_result
