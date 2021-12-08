val refinement_steps_summary : unit -> Yojson.t
val save_stats_and_restart : int -> unit
val get_solver_stats : int -> Yojson.t option
val get_summary : int -> Yojson.t option
val get_simple_stats : int -> (float * float) option
