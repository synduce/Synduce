open Lang

val as_unknown_app
  :  ?match_functions:(Term.term -> bool)
  -> unknowns:Term.VarSet.t
  -> Term.term
  -> Term.term list option

val subexpressions_without_boxes
  :  Expression.t
  -> (Expression.t, Expression.comparator_witness) Base.Set.t

module Solver : sig
  val presolve_equations
    :  orig_ctx:Term.Context.t
    -> ?ctx:EProps.RContext.t
    -> xi:Term.variable
    -> (Term.term * Term.term option * Term.term * Term.term) list
    -> [> `First of string * Term.variable list * Term.term
       | `Second of Skeleton.t
       | `Third
       ]
       Lwt.t

  val functional_equation
    :  ctx:Common.Env.env
    -> func_side:Term.term list
    -> lemma:Term.term option
    -> Term.term
    -> (Term.variable * Term.VarSet.t) list
    -> (Term.variable * Term.term) list * Expression.t option * EProps.RContext.t
end
