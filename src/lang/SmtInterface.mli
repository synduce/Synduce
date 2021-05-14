open Smtlib
open Term

(* `rtype_of_smtSort smt_sort` return `Some t` if `t` is a type representing the
    smt sort `smt_sort`, and `None` if no such type exists.
*)
val rtype_of_smtSort : SmtLib.smtSort -> RType.t option

(* `smt_of_term t` generates a smt-term from the term t. If t is not a valid term,
    raises errors.
    TODO: return a Result.t instead of failing.
 *)
val smt_of_term : term -> SmtLib.smtTerm

(* In an environment, and identifier is assigned a specific kind for parsing. *)
type id_kind =
  (* A type constructor (e.g. Cons, Node, ...) *)
  | ICstr of string
  (* A variable. *)
  | IVar of variable
  (* A binary operator. *)
  | IBinop of Binop.t
  (* A unary operator. *)
  | IUnop of Unop.t
  (* The identified is undefined in the current environment. *)
  | INotDef

(* Returns the kind of a string identified given a map from variable names to variables. *)
val id_kind_of_s : (string, variable, 'a) Base.Map.t -> string -> id_kind

(* Convert a term to a smt-term in an environment (a map from string to variables). *)
val term_of_smt :
  (string, variable, Base.String.comparator_witness) Base.Map.t -> SmtLib.smtTerm -> term

(* A term model: a map from string (variable names) to terms. In most cases these
    terms will be constants.  *)
type term_model = (string, term, Base.String.comparator_witness) Base.Map.t

(* Translate a solver response (a Smtlib.Solvers.solver_response) to a term-model.
    Returns an empty map if the response is not a SExps (a list of s-expressions) that
    define a model obtained through (get-model).
*)
val model_to_constmap : SmtLib.solver_response -> term_model

(* Generate a list of commands from a set of variables.
   The list of commands may then be passsed to a Smtlib.Solvers.online_solver instance
   through the Smtlib.Solvers.declare_all command.
*)
val decls_of_vars : VarSet.t -> SmtLib.command list
