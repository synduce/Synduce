open Lang
open Rewriter

module Solver : sig
  val functionalize :
    args:Expression.t list ->
    lemma:Expression.t option ->
    Expression.t ->
    (int * IS.t) list ->
    ((int * Expression.t) list, (int * Expression.t) list * Expression.t) Result.t

  val functional_equation :
    func_side:Term.term list ->
    lemma:Term.term option ->
    Term.term ->
    (Term.variable * Term.VarSet.t) list ->
    (Term.variable * Term.term) list
end
