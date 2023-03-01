val cvc_message : unit -> unit
val start_message : string -> bool -> unit

val on_success
  :  ?print_unrealizable:bool
  -> is_ocaml_syntax:bool
  -> ctx:Common.Env.env
  -> string ref
  -> Common.ProblemDefs.PsiDef.t
  -> Syguslib.Sygus.solver_response Common.ProblemDefs.segis_response
  -> Yojson.t

val on_failure
  :  ?is_ocaml_syntax:bool
  -> ctx:Common.Env.env
  -> Common.ProblemDefs.PsiDef.t
  -> Yojson.t

val print_stats_coverage : Confsearch.Main.multi_soln_result -> int * int * int -> unit
val print_usage : unit -> 'a
