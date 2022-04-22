open Common

(** [find_and_solver_problem (target,spec,repr) globals] finds the PMRS
    components in the top-level functions [globals] with names [target], [spec]
    and [repr], and solves the synthesis problem associated with it. In oder to
    solve it, it may attempt to modify the problem by changing the inputs of the
    unknowns or changing their output domain.
*)
val find_and_solve_problem
  :  ctx:Env.env
  -> (string * string * string) option
  -> (string, Lang.PMRS.t, Base.String.comparator_witness) Base.Map.t
  -> (Env.env
     * ProblemDefs.PsiDef.t
     * Syguslib.Sygus.solver_response ProblemDefs.segis_response)
     list
