open Lang
open Base

val subst_recursive_calls :
  AState.psi_def -> Term.term list -> (Term.term * Term.term) list * Term.TermSet.t

val maximally_reduced_app : AState.psi_def -> Term.term -> Term.term list -> bool

val nonreduced_terms :
  AState.psi_def -> Term.VarSet.t -> Term.term -> (Term.variable * Term.term list) list

val nonreduced_terms_all : AState.psi_def -> Term.term -> (Term.variable * Term.term list) list

val replace_rhs_of_main : ?for_mr:bool -> AState.psi_def -> PMRS.t -> Term.term -> Term.term
(** Replace subterms that correspond to the right-hand side of the main rule to avoid capturing f(v)
    when v is a variable. *)

val replace_rhs_of_mains : AState.psi_def -> Term.term -> Term.term
(** Applies `replace_rhs_of_main` with all main functions in the problem definition (psi_def)
*)

val simple :
  ?max_height:int ->
  Term.term ->
  (Term.term, Term.Terms.comparator_witness) Set.t
  * (Term.term, Term.Terms.comparator_witness) Set.t
(**
Simple term expansion procedure. [simple t] expands the term [t] into two sets of terms, the first
set is a set of *bounded* terms, the second set is a set of unbounded terms. In an algorithm, one
should keep track of the unbounded terms as well and recursively expand them as necessary.
*)

val is_mr : AState.psi_def -> PMRS.t -> Term.term -> Term.VarSet.t -> bool

val is_mr_all : AState.psi_def -> Term.term -> bool

val to_maximally_reducible : AState.psi_def -> Term.term -> Term.TermSet.t * Term.TermSet.t
