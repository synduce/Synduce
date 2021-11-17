open Lang
open AState

val next_lifing_type : Lang.RType.t list ref
val msg_lifting : unit -> unit
val empty_lifting : lifting
val is_empty_lifting : lifting -> bool
val alpha_component_count : unit -> int
val decompose_t : psi_def -> Term.term -> (int * Term.term) option
val recompose_t : psi_def -> Term.term -> int -> Term.term
val get_mapped_value : p:psi_def -> lifting -> Term.term -> Term.term option

val interactive_add_lifting_expression
  :  p:psi_def
  -> lifting
  -> Term.term
  -> int
  -> lifting * Term.term option

val replace_boxed_expressions : p:psi_def -> lifting -> Term.term -> Term.term
val is_lifted : psi_def -> bool
val lifting_count : psi_def -> int
val lifting_types : psi_def -> RType.t list
val proj_to_non_lifting : psi_def -> Term.term option
val is_proj_function : psi_def -> Term.term -> bool
val proj_to_lifting : psi_def -> (Term.term -> Term.term) option
val compose_parts : psi_def -> Term.term option

val deduce_lifting_expressions
  :  p:psi_def
  -> lifting
  -> Term.term option
  -> Term.term
  -> Term.term
  -> lifting

val scalar
  :  p:psi_def
  -> refinement_loop_state
  -> Syguslib.Sygus.solver_response * ('a, unrealizability_ctex list) Base.Either.t
  -> (psi_def * refinement_loop_state, Syguslib.Sygus.solver_response) result
