open AState
open Base

val find_problem_components
  :  string * string * string
  -> (string, Lang.PMRS.t, String.comparator_witness) Map.t
  -> PsiDef.t

val update_context : PsiDef.t -> unit
