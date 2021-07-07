val is_boxable_expr : int * Lang.Rewriter.IS.t -> Lang.Rewriter.Expression.t -> bool

module Solver : sig
  val functional_equation :
    func_side:Lang.Term.term list ->
    Lang.Term.term ->
    (Lang.Term.variable * Lang.Term.VarSet.t) list ->
    (Lang.Term.variable * Lang.Term.term) list
end
