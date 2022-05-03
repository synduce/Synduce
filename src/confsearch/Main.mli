open Common

(** [find_and_solver_problem (target,spec,repr) globals] finds the PMRS
    components in the top-level functions [globals] with names [target], [spec]
    and [repr], and solves the synthesis problem associated with it. In order to
    solve it, it may attempt to modify the problem by changing the inputs of the
    unknowns or changing their output domain.
    Returns an integer representing the size of the possible search space and a list
    of results that are either solutions, unrealizablity results or failures.
*)
val find_and_solve_problem
  :  ctx:Env.env
  -> (string * string * string) option
  -> (string, Lang.PMRS.t, Base.String.comparator_witness) Base.Map.t
  -> int
     * (Env.env
       * ProblemDefs.PsiDef.t
       * Syguslib.Sygus.solver_response ProblemDefs.segis_response)
       list
