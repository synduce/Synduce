open Lang
open Term

val find : ctx:Env.env -> key:term -> ProblemDefs.term_info list option
val get : ctx:Env.env -> p:Psi.t -> term -> term option
val get_with_precond : ctx:Env.env -> p:Psi.t -> key:term * term option -> term option

val change
  :  ctx:Env.env
  -> key:term
  -> split:term option
  -> (ProblemDefs.term_info -> ProblemDefs.term_info)
  -> unit

val add_direct : ctx:Env.env -> key:Expression.t -> data:ProblemDefs.term_info -> unit
val add : ctx:Env.env -> key:term -> data:ProblemDefs.term_info -> unit
val set : ctx:Env.env -> key:term -> data:ProblemDefs.term_info list -> unit

val fold
  :  ctx:Env.env
  -> init:'a
  -> f:(key:Expression.t -> data:ProblemDefs.term_info list -> 'a -> 'a)
  -> 'a
