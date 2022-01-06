open Lang
open AState

val next_lifing_type : Lang.RType.t list ref
val msg_lifting : unit -> unit
val empty_lifting : lifting
val is_empty_lifting : lifting -> bool
val alpha_component_count : unit -> int
val decompose_t : PsiDef.t -> Term.term -> (int * Term.term) option
val recompose_t : PsiDef.t -> Term.term -> int -> Term.term
val get_mapped_value : p:PsiDef.t -> lifting -> Term.term -> Term.term option

val interactive_add_lifting_expression
  :  p:PsiDef.t
  -> lifting
  -> Term.term
  -> int
  -> lifting * Term.term option

val replace_boxed_expressions : p:PsiDef.t -> lifting -> Term.term -> Term.term
val is_lifted : PsiDef.t -> bool
val lifting_count : PsiDef.t -> int
val lifting_types : PsiDef.t -> RType.t list
val proj_to_non_lifting : PsiDef.t -> Term.term option
val is_proj_function : PsiDef.t -> Term.term -> bool
val proj_to_lifting : PsiDef.t -> (Term.term -> Term.term) option
val compose_parts : PsiDef.t -> Term.term option

val deduce_lifting_expressions
  :  p:PsiDef.t
  -> lifting
  -> Term.term option
  -> Term.term
  -> Term.term
  -> lifting

val scalar
  :  p:PsiDef.t
  -> refinement_loop_state
  -> Syguslib.Sygus.solver_response * ('a, unrealizability_ctex list) Base.Either.t
  -> (PsiDef.t * refinement_loop_state, Syguslib.Sygus.solver_response) result
