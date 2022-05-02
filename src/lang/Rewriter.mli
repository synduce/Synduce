open Base

val factorize : Expression.t -> Expression.t
val distrib : Term.Operator.t -> Expression.t list -> Expression.t
val expand : Expression.t -> Expression.t
val eequals : Expression.t -> Expression.t -> bool
val rewrite_with_lemma : Expression.t -> Expression.t -> Expression.t list

val match_as_subexpr
  :  ?lemma:Expression.t option
  -> Expression.boxkind
  -> Expression.t
  -> of_:Expression.t
  -> Expression.t option

val simplify_term : ctx:Term.Context.t -> Term.term -> Term.term
