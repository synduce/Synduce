open Base
open Lang

val find_problem_components
  :  ctx:Env.env
  -> filename:string
  -> string * string * string
  -> (string, PMRS.t, String.comparator_witness) Map.t
  -> ProblemDefs.PsiDef.t

(** Update an environment:
  - infer the types for the problem definition.
  - update the symbol and type tables inside the context.
*)
val update_context : ctx:Env.env -> ProblemDefs.PsiDef.t -> unit
