open Term

(** Simplifies a term using rules rewriting terms containing constants.
    If all the leaves of the term are constants, evaluates the term to
    a constant.
*)
val simplify : term -> term

val in_model : ?no_simplify:bool -> ctx:Context.t -> term VarMap.t -> term -> term
