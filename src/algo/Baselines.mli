open AState

(** The symbolic SEGIS algorithm.  *)
val algo_segis : ctx:Env.env -> PsiDef.t -> Syguslib.Sygus.solver_response segis_response

(** The concrete CEGIS algorithm. *)
val algo_cegis : ctx:Env.env -> PsiDef.t -> Syguslib.Sygus.solver_response segis_response
