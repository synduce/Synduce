val cvc_message : unit -> unit
val start_message : string -> bool -> unit

val prep_final_json
  :  is_ocaml_syntax:bool
  -> ctx:Common.Env.env
  -> string ref
  -> Common.ProblemDefs.PsiDef.t
  -> (Common.ProblemDefs.soln, Common.ProblemDefs.unrealizability_ctex list) Base.Either.t
  -> float
  -> float
  -> Yojson.t

val on_success
  :  is_ocaml_syntax:bool
  -> ctx:Common.Env.env
  -> string ref
  -> Common.ProblemDefs.PsiDef.t
  -> (Common.ProblemDefs.soln, Common.ProblemDefs.unrealizability_ctex list) Base.Either.t
  -> Yojson.t

val print_usage : unit -> 'a
