open Lang
open Base
open SmtInterface

val mk_recursion_elimination_term
  :  ctx:Env.env
  -> ProblemDefs.PsiDef.t
  -> (Term.term * Term.term) option

val subst_recursive_calls
  :  ctx:Env.env
  -> ProblemDefs.PsiDef.t
  -> Term.term list
  -> (Term.term * Term.term) list * TermSet.t

val maximally_reduced_app : ProblemDefs.PsiDef.t -> Term.term -> Term.term list -> bool

val nonreduced_terms
  :  ProblemDefs.PsiDef.t
  -> Term.VarSet.t
  -> Term.term
  -> (Term.variable * Term.term list) list

val nonreduced_terms_all
  :  ProblemDefs.PsiDef.t
  -> Term.term
  -> (Term.variable * Term.term list) list

(** Replace subterms that correspond to the right-hand side of the main rule to avoid capturing f(v)
    when v is a variable. *)
val replace_rhs_of_main
  :  ?verbose:bool
  -> ?for_mr:bool
  -> ctx:Term.Context.t
  -> ProblemDefs.PsiDef.t
  -> PMRS.t
  -> Term.term
  -> Term.term

(** Applies `replace_rhs_of_main` with all main functions in the problem definition (psi_def)
*)
val replace_rhs_of_mains
  :  ctx:Term.Context.t
  -> ProblemDefs.PsiDef.t
  -> Term.term
  -> Term.term

(**
Simple term expansion procedure. [simple ~ctx t] expands the term [t] into two sets of terms, the first
set is a set of *bounded* terms, the second set is a set of unbounded terms. In an algorithm, one
should keep track of the unbounded terms as well and recursively expand them as necessary.
*)
val simple
  :  ?verbose:bool
  -> ?max_height:int
  -> ctx:Term.Context.t
  -> Term.term
  -> (Term.term, Term.Terms.comparator_witness) Set.t
     * (Term.term, Term.Terms.comparator_witness) Set.t

(** Heuristic to bound terms. Unbounded symbols will be bound using some arbitrary expansion. *)
val make_bounded : ctx:Term.Context.t -> Term.term -> Term.term

val is_mr
  :  ctx:Env.env
  -> ProblemDefs.PsiDef.t
  -> PMRS.t
  -> Term.term
  -> Term.VarSet.t
  -> bool

val is_mr_all : ctx:Env.env -> ProblemDefs.PsiDef.t -> Term.term -> bool

val to_maximally_reducible
  :  ctx:Env.env
  -> ProblemDefs.PsiDef.t
  -> Term.term
  -> TermSet.t * TermSet.t

(**
  `expand_all ~fctx ~ctx p (t,u)` expands all terms in `u` to at least one MR-term according to
    `p`. If the pair (t,u) is a boundary then the pair of sets returned will also be a boundary.
*)
val expand_all
  :  ?fuel:float
  -> ctx:Env.env
  -> ProblemDefs.PsiDef.t
  -> TermSet.t * TermSet.t
  -> (TermSet.t * TermSet.t, unit) Result.t

val expand_fast : ctx:Term.Context.t -> Term.term -> Term.term list

(** [lwt_expand_loop] provides basic functionality for bounded checking.
    [lwt_expand_loop steps f start] will run a bounded checking loop, starting with the term
    set [start] and performing [!Lib.Config.num_expansions_check] steps, applying the
    checking function [f] for each term obtained during the expansion of the term in the set
    [start] into bounded terms.

    If a check returns an answer for which [r_stop] is true, then the function returns
    this answer. By default [r_stop] checks whether the answer is [SmtLib.Sat].

    If every check returns UNSAT, then the procedure returns [SmtLib.Unknown], unless
    [!Config.no_bounded_sat_as_unsat] is true, in which case it returns [r_complete].
    By default, [r_complete] is [SmtLib.Unsat].

    If the procedure has explored all possible expansions of the terms in the input set,
    then it returns [r_complete] (all the check having returned UNSAT for all
    possible terms).

    @param r_stop is a function that returns true whenever the loop should stop, given
    the solver response to a call to f.
    @param r_complete is the answer that should be given if all the

*)
val lwt_expand_loop
  :  int ref
  -> (AsyncSmt.response -> Term.term -> AsyncSmt.response)
  -> ?r_stop:(Smtlib.SmtLib.solver_response -> bool)
  -> ?r_complete:Smtlib.SmtLib.solver_response
  -> ctx:Term.Context.t
  -> Term.term Lwt.t
  -> AsyncSmt.response
