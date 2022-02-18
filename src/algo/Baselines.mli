open AState

(** The symbolic SEGIS algorithm.  *)
val algo_segis : Context.t -> PsiDef.t -> Syguslib.Sygus.solver_response segis_response

(** The concrete CEGIS algorithm. *)
val algo_cegis : Context.t -> PsiDef.t -> Syguslib.Sygus.solver_response segis_response
