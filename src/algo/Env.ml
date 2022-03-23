open Lang

type env =
  { functions : PMRS.Functions.ctx
  ; ctx : Term.Context.t
  }

let group (ctx : Term.Context.t) (functions : PMRS.Functions.ctx) = { functions; ctx }

let env_copy (c : env) =
  { functions = PMRS.Functions.copy c.functions; ctx = Term.Context.copy c.ctx }
;;

(** Execute a function with a full context. *)
let ( >>> ) (c : env) f = f ~ctx:c

(** Execute a function with a term context from a full context.  *)
let ( >- ) (c : env) f = f ~ctx:c.ctx

(** Execute a function with a term context from a full context, without a labelled argument. *)
let ( @>- ) c f = f c.ctx

(** Execute a function with a term context and function context from a full context.  *)
let ( >>- ) (c : env) f = f ~fctx:c.functions ~ctx:c.ctx

(** Reduce a term with a context.  *)
let ctx_reduce (c : env) = Reduce.reduce_term ~ctx:c.ctx ~fctx:c.functions

(** Generate a fresh variable name in an environment. *)
let ( !> ) (c : env) = Alpha.fresh c.ctx.names
