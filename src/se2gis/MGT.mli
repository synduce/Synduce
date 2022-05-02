(** Returns the set of most general terms for a synthesis problems: a set of terms expanded from a
    variable of the target input type. This set can be used to initialize the refinement loop.  *)
val most_general_terms : Common.Env.env -> Lang.PMRS.t -> Lang.TermSet.t
