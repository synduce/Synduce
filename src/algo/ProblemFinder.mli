open AState
open Base
open Lang

val find_problem_components
  :  fctx:PMRS.Functions.ctx
  -> ctx:Term.Context.t
  -> string * string * string
  -> (string, PMRS.t, String.comparator_witness) Map.t
  -> PsiDef.t

val update_context : ctx:Term.Context.t -> fctx:PMRS.Functions.ctx -> PsiDef.t -> unit
