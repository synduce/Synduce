open Term

val until_irreducible : (term -> term * bool) -> term -> term
(**
  `until_irreducible f t` applies `f` to `t` until `t` is unchanged by application of `f` or we have
  reached the limit of rewrites Reduce._MAX.
  @param f A function that returns a pair of a term and a boolean. It should be written such that
  `f x = x', b => (b = (x != x'))`. The boolean indicates whether the term has be changed.
  @param t The term that will be rewritten using `f`.
  *)

val rule_lookup :
  ('a, variable * variable list * pattern option * term, 'b) Base.Map.t ->
  variable ->
  term list ->
  term list
(**
    `rule_lookup rule_map f args` searches for a rewrite rule in rule_map whose head matches
    `f args`. For each matched rule, it returns the rhs of the rule with the substitutions
    corresponding to apply `f args` applied. If no rule is matched, returns an empty list.
*)

val reduce_term : ?unboxing:bool -> term -> term
(** `reduce_term t` reduces the term `t` using lambda-calculus reduction rules.
    (let x = e in e' is equivalent to (λx.e') e).
    Function symbolds in applications are resolved by looking up functions in the
    current environment.
*)

val reduce_pmrs : PMRS.t -> term -> term
(**
    `reduce_pmrs p t` is a shortcut for `reduce_term (mk_app (mk_var p.pmain_symb) [t])`
    when `p` is in the current environment.
*)

val calc_term : term -> term list
(**
    Same as `reduce_term` but returns a list of terms, one for each reduction step using
    a pmrs rule.
 *)

val instantiate_with_solution : PMRS.t -> (string * variable list * term) list -> PMRS.t
(**
    `instantiate_with_solution p defs` returns the PMRS p, in which the unknowns have been
    replaced by their definition defined in defs.
    @param p A PMRS with unknowns.
    @param defs Definitions for the unknown in the PMRS, in the form of a list of triples
    (unknown function name, arguments, body of the unknowns)
*)

val is_identity : PMRS.t -> bool
(** Returns true if the PMRS is equivalent to the identity function.
    Uses the reduction procedures to check wether `f x = x`, symbolically.
 *)
