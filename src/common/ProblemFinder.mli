open Base
open Lang

val find_problem_components
  :  ctx:Env.env
  -> string * string * string
  -> (string, PMRS.t, String.comparator_witness) Map.t
  -> ProblemDefs.PsiDef.t

val update_context : ctx:Env.env -> ProblemDefs.PsiDef.t -> unit
