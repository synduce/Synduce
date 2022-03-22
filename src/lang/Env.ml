type env =
  { functions : PMRS.Functions.ctx
  ; ctx : Term.Context.t
  }

let group (ctx : Term.Context.t) (functions : PMRS.Functions.ctx) = { functions; ctx }
let ( >- ) (c : env) f = f ~ctx:c.ctx
let ( >+ ) c f = f c.ctx
let ( >>- ) (c : env) f = f ~fctx:c.functions ~ctx:c.ctx
let ctx_reduce (c : env) = Reduce.reduce_term ~ctx:c.ctx ~fctx:c.functions
