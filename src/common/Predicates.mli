open Lang
open Term
open Env
open ProblemDefs

val find : ctx:Env.env -> key:term -> (term_info * cond_lemma list) option

val find_lemma_info
  :  ctx:Env.env
  -> term * term option
  -> (term_info * cond_lemma option) option

val get : ?count_reuse:bool -> ctx:Env.env -> p:Psi.t -> term -> term option
val get_with_precond : ctx:Env.env -> p:Psi.t -> key:term * term option -> term option

val change
  :  ctx:Env.env
  -> key:term
  -> split:term option
  -> (term_info -> cond_lemma -> cond_lemma Lwt.t)
  -> unit Lwt.t

val add_direct : ctx:Env.env -> key:Expression.t -> data:term_info * cond_lemma -> unit
val add : ctx:Env.env -> key:term -> data:term_info * cond_lemma -> unit
val set : ctx:Env.env -> key:term -> data:term_info * cond_lemma list -> unit

val fold
  :  ctx:Env.env
  -> init:'a
  -> f:(key:Expression.t -> data:term_info * cond_lemma list -> 'a -> 'a)
  -> 'a
