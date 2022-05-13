open Base
open Lang

type env =
  { functions : PMRS.Functions.ctx
  ; ctx : Term.Context.t
  ; refinement_steps : int ref
  ; secondary_refinement_steps : int ref (* Problem types. *)
  ; alpha : RType.t ref
        (** The type D in the paper: the output type of the reference and the target recursion skeleton. The
    first element is the pure type output type of the functions, and the second element is an
    optional term that represents the additional predicate on the output of the reference function.
    The term is assumed to define a function (fun (free variables of term) -> term).
*)
  ; tau : RType.t ref
        (** The type τ in the paper, input type of the reference function.  *)
  ; theta : RType.t ref
        (** The type θ in the paper, input type of the target recursion skeleton.  *)
  }

let group (ctx : Term.Context.t) (functions : PMRS.Functions.ctx) =
  { functions
  ; ctx
  ; refinement_steps = ref 0
  ; secondary_refinement_steps = ref 0
  ; alpha = ref RType.TInt
  ; tau = ref RType.TInt
  ; theta = ref RType.TInt
  }
;;

let env_copy (c : env) =
  { functions = PMRS.Functions.copy c.functions
  ; ctx = Term.Context.copy c.ctx
  ; refinement_steps = ref !(c.refinement_steps)
  ; secondary_refinement_steps = ref !(c.secondary_refinement_steps)
  ; alpha = ref !(c.alpha)
  ; tau = ref !(c.tau)
  ; theta = ref !(c.theta)
  }
;;

let env_create () =
  { functions = PMRS.Functions.create ()
  ; ctx = Term.Context.create ()
  ; refinement_steps = ref 0
  ; secondary_refinement_steps = ref 0
  ; alpha = ref RType.TInt
  ; tau = ref RType.TInt
  ; theta = ref RType.TInt
  }
;;

(** Execute a function with a full context. *)
let ( >>> ) (c : env) f = f ~ctx:c

(** Execute a function with a term context from a full context.  *)
let ( >- ) (c : env) f = f ~ctx:c.ctx

(** Execute a function with a function context from a full context.  *)
let ( -< ) (c : env) f = f ~fctx:c.ctx

(** Execute a function with a term context from a full context, without a labelled argument. *)
let ( @>- ) c f = f c.ctx

(** Execute a function with a term context and function context from a full context.  *)
let ( >>- ) (c : env) f = f ~fctx:c.functions ~ctx:c.ctx

(** Reduce a term with a context.  *)
let ctx_reduce (c : env) = Reduce.reduce_term ~ctx:c.ctx ~fctx:c.functions

(** Generate a fresh variable name in an environment. *)
let ( !> ) (c : env) = Alpha.fresh c.ctx.names

(** Increment the refinement step count. *)
let incr_refinement (c : env) = Int.incr c.refinement_steps

(** Increment the secondary refinement step count. *)
let incr_secondary_refinement (c : env) = Int.incr c.secondary_refinement_steps

let get_refinement_steps (c : env) = !(c.refinement_steps)
let get_secondary_refinement_steps (c : env) = !(c.secondary_refinement_steps)
let get_theta (env : env) = !(env.theta)
let get_tau (env : env) = !(env.tau)
let get_alpha (env : env) = !(env.alpha)

(* Shortcuts *)
let var_type (c : env) = Term.Variable.vtype_or_new c.ctx
