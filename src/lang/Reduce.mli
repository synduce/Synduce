open Term

module CondTree : sig
  type 'a t =
    | CBr of 'a
    | CIf of term * 'a t * 'a t

  val to_term : term t -> term
  val of_term : term -> term t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val all_or_none : 'a option t -> 'a t option
end

type func_resolution =
  | FRFun of fpattern list * term
  | FRPmrs of PMRS.t
  | FRNonT of PMRS.t
  | FRUnknown

(** Resolve the funcion definiton of a term in an environment. *)
val resolve_func : PMRS.Functions.ctx -> Context.t -> term -> func_resolution

(** Project irreducible terms into tuples. *)
val project_irreducible_terms : Context.t -> term -> term

(**
  [until_irreducible f t] applies [f] to [t] until [t] is unchanged by application of [f] or we have
  reached the limit of rewrites Reduce._MAX.
  @param f A function that returns a pair of a term and a boolean. It should be written such that
  [f x = x', b => (b = (x != x'))]. The boolean indicates whether the term has be changed.
  @param t The term that will be rewritten using [f].
  *)
val until_irreducible : (term -> term * bool) -> term -> term

(**
    [rule_lookup ctx rule_map f args] searches for a rewrite rule in [rule_map] whose head matches
    [f args]. For each matched rule, it returns the rhs of the rule with the substitutions
    corresponding to apply [f args] applied. If no rule is matched, returns an empty list.
*)
val rule_lookup
  :  Context.t
  -> ('a, variable * variable list * pattern option * term, 'b) Base.Map.t
  -> variable
  -> term list
  -> term list

(** [reduce_term ~fctx ~ctx t] reduces the term [t] using lambda-calculus reduction rules.
    (let x = e in e' is equivalent to (λx.e') e).
    Function symbolds in applications are resolved by looking up functions in the
    current function contexts [fctx] or context [ctx].
*)
val reduce_term
  :  ?projecting:bool
  -> ?unboxing:bool
  -> fctx:PMRS.Functions.ctx
  -> ctx:Context.t
  -> term
  -> term

(**
    [reduce_pmrs ~fctx ~ctx p t] is a shortcut for [reduce_term ~fctx ~ctx (mk_app (mk_var p.pmain_symb) [t])]
    when [p] is in the current environment.
*)
val reduce_pmrs : fctx:PMRS.Functions.ctx -> ctx:Context.t -> PMRS.t -> term -> term

(**
    Same as `reduce_term` but returns a list of terms, one for each reduction step using
    a pmrs rule.
 *)
val calc_term : fctx:PMRS.Functions.ctx -> ctx:Context.t -> term -> term list

(**
    ]instantiate_with_solution ~fctx ~ctx p defs] returns the PMRS [p], in which the unknowns have been
    replaced by their definition defined in defs.
    @param fctx The function context.
    @param ctx The current context with types and names.
    @param p A PMRS with unknowns.
    @param defs Definitions for the unknown in the PMRS, in the form of a list of triples
    (unknown function name, arguments, body of the unknowns)
*)
val instantiate_with_solution
  :  fctx:PMRS.Functions.ctx
  -> ctx:Context.t
  -> PMRS.t
  -> (string * variable list * term) list
  -> PMRS.t

(** Returns [true] if the PMRS is equivalent to the identity function.
    Uses the reduction procedures to check wether [f x = x], symbolically.
 *)
val is_identity : fctx:PMRS.Functions.ctx -> ctx:Context.t -> PMRS.t -> bool
