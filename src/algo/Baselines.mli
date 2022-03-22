open AState

(** The symbolic SEGIS algorithm.  *)
val algo_segis
  :  ctx:Lang.Env.env
  -> t:ThreadContext.t
  -> PsiDef.t
  -> Syguslib.Sygus.solver_response segis_response

(** The concrete CEGIS algorithm. *)
val algo_cegis
  :  ctx:Lang.Env.env
  -> t:ThreadContext.t
  -> PsiDef.t
  -> Syguslib.Sygus.solver_response segis_response
