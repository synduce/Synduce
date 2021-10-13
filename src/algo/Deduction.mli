open Lang
open Rewriter

val as_unknown_app
  :  ?match_functions:(Term.term -> bool)
  -> unknowns:Term.VarSet.t
  -> Term.term
  -> Term.term list option

module Solver : sig
  val presolve_equations
    :  unknowns:Term.VarSet.t
    -> (Term.term * Term.term option * Term.term * Term.term) list
    -> Term.variable
    -> Skeleton.t option

  val functional_equation
    :  func_side:Term.term list
    -> lemma:Term.term option
    -> Term.term
    -> (Term.variable * Term.VarSet.t) list
    -> (Term.variable * Term.term) list * Expression.t option
end
