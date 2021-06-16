open Smtlib
open Term

val rtype_of_smtSort : SmtLib.smtSort -> RType.t option
(** `rtype_of_smtSort smt_sort` return `Some t` if `t` is a type representing the smt sort
      `smt_sort`, and `None` if no such type exists.
*)

val sort_of_rtype : RType.t -> SmtLib.smtSort

val smt_of_term : term -> SmtLib.smtTerm
(** `smt_of_term t` generates a smt-term from the term t. If t is not a valid term, raises errors.
      TODO: return a Result.t instead of failing.
 *)

val sorted_vars_of_vars : VarSet.t -> SmtLib.smtSortedVar list
(**
      Convert a set of variables into a list of smt sorted vars.
 *)

val smt_of_pmrs : PMRS.t -> SmtLib.command list
(**
      Convert a PMRS into a set of commands. Once the commands have been passed to the solver,
      the PMRS can be used as a function in the solver.
*)

val term_of_smt :
  (string, variable, Base.String.comparator_witness) Base.Map.t -> SmtLib.smtTerm -> term
(** Convert a term to a smt-term in an environment (a map from string to variables). *)

type term_model = (string, term, Base.String.comparator_witness) Base.Map.t
(** A term model: a map from string (variable names) to terms. In most cases these
    terms will be constants.  *)

val model_to_constmap : SmtLib.solver_response -> term_model
(** Translate a solver response (a Smtlib.Solvers.solver_response) to a term-model. Returns an empty
      map if the response is not a SExps (a list of s-expressions) that define a model obtained
      through (get-model).
*)

val model_to_subst : VarSet.t -> SmtLib.solver_response -> (term * term) list
(** Translate a solver response (a Smtlib.Solvers.solver_response) to a term substitution list.
      Returns an empty list if the response is not a SExps (a list of s-expressions) that defines a
      model obtained through (get-model).
*)

val decls_of_vars : VarSet.t -> SmtLib.command list
(** Generate a list of commands from a set of variables. The list of commands may then be passsed to
     a Smtlib.Solvers.online_solver instance through the Smtlib.Solvers.declare_all command.
*)
