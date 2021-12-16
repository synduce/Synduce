open AState
open Base

val find_problem_components
  :  string * string * string
  -> (string, Lang.PMRS.t, String.comparator_witness) Map.t
  -> psi_def

val update_context : psi_def -> unit
