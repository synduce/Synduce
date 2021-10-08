open Lang

val next_lifing_type : Lang.RType.t list ref
val msg_lifting : unit -> unit
val empty_lifting : AState.lifting
val is_empty_lifting : AState.lifting -> bool
val alpha_component_count : unit -> int
val decompose_t : AState.psi_def -> Term.term -> (int * Term.term) option
val recompose_t : AState.psi_def -> Term.term -> int -> Term.term
val get_mapped_value : p:AState.psi_def -> AState.lifting -> Term.term -> Term.term option

val interactive_add_lifting_expression
  :  p:AState.psi_def
  -> AState.lifting
  -> Term.term
  -> int
  -> AState.lifting * Term.term option

val replace_boxed_expressions
  :  p:AState.psi_def
  -> AState.lifting
  -> Term.term
  -> Term.term

val is_lifted : AState.psi_def -> bool
val lift_count : AState.psi_def -> int
val proj_to_non_lifting : AState.psi_def -> Term.term option
val is_proj_function : AState.psi_def -> Term.term -> bool
val proj_to_lifting : AState.psi_def -> (Term.term -> Term.term) option
val compose_parts : AState.psi_def -> Term.term option

val deduce_lifting_expressions
  :  p:AState.psi_def
  -> AState.lifting
  -> Term.term option
  -> Term.term
  -> Term.term
  -> AState.lifting

val scalar
  :  p:AState.psi_def
  -> AState.refinement_loop_state
  -> 'a
  -> ( AState.psi_def * AState.refinement_loop_state
     , Syguslib.Sygus.solver_response )
     result
