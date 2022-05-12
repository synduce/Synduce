open Lang
open Common
open ProblemDefs
open Env
open Term

val next_lifing_type : Lang.RType.t list ref
val msg_lifting : unit -> unit
val empty_lifting : lifting
val is_empty_lifting : lifting -> bool
val alpha_component_count : env -> int

(** Get the argument and projection index of a term when it is the projection of
  the target function applied to some term.
*)
val decompose_t : ctx:env -> PsiDef.t -> term -> (int * term) option

(** Get the i-th projection of applying the target function to a term. *)
val recompose_t : ctx:Context.t -> PsiDef.t -> Term.term -> int -> Term.term

val get_mapped_value : ctx:env -> p:PsiDef.t -> lifting -> term -> term option

(** Interactively add expressions for the value of the lifting. *)
val interactive_add_lifting_expression
  :  ctx:env
  -> p:PsiDef.t
  -> lifting
  -> term
  -> int
  -> lifting * term option

val replace_boxed_expressions : ctx:env -> p:PsiDef.t -> lifting -> Term.term -> Term.term

(** Check whether the problem is lifted in the current context.  *)
val is_lifted : ctx:env -> PsiDef.t -> bool

(** Return the number of liftings in the current context. 0 if no lifting. *)
val lifting_count : ctx:env -> PsiDef.t -> int

(** Returns the list of types that correspond to the current lifting. Empty list if no lifting. *)
val lifting_types : ctx:env -> PsiDef.t -> RType.t list

(** Project to the non-lifting components. *)
val proj_to_non_lifting : ctx:env -> PsiDef.t -> Term.term option

val is_proj_function : ctx:env -> PsiDef.t -> Term.term -> bool

(** Project to the lifting components. *)
val proj_to_lifting : ctx:env -> PsiDef.t -> (Term.term -> Term.term) option

val compose_parts : ctx:env -> PsiDef.t -> Term.term option

(** Deduce the lifting expresions needed to solve lhs = rhs *)
val deduce_lifting_expressions
  :  ctx:env
  -> p:PsiDef.t
  -> lifting
  -> Term.term option
  -> lhs:Term.term
  -> rhs:Term.term
  -> lifting

val scalar
  :  ctx:env
  -> p:PsiDef.t
  -> refinement_loop_state
  -> Syguslib.Sygus.solver_response * ('a, unrealizability_witness list) Base.Either.t
  -> (PsiDef.t * refinement_loop_state, Syguslib.Sygus.solver_response) result
