open Lang

val clear : unit -> unit
(** This functions enumerate possible extensions of an input problem. *)
val enumerate_p : ctx:Term.Context.t -> Common.ProblemDefs.PsiDef.t -> Common.ProblemDefs.PsiDef.t list
