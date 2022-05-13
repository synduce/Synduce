val timed : (unit -> 'a) -> float * 'a
val lwt_timed : (unit Lwt.t -> 'a Lwt.t) -> (float * 'a) Lwt.t

type proc_time_info =
  { pti_start : float
  ; pti_status : int
  ; pti_elapsed : float
  ; pti_last : float
  ; pti_exited : bool
  }

val get_solver_pids : unit -> (string * int list) list
val log_proc_start : int -> unit
val log_proc_restart : int -> unit
val log_alive : int -> unit
val get_alive : unit -> (string * int) list
val log_proc_quit : ?status:int -> int -> unit
val get_elapsed : int -> float
val glob_start : unit -> unit
val get_glob_elapsed : unit -> float
val verif_time : float ref
val add_verif_time : float -> unit
val log_solver_start : int -> string -> unit
val refinement_log : (string * Yojson.t) list Base.Stack.t
val major_step_counter : int ref
val log_new_major_step : tsize:int -> usize:int -> unit -> unit

val log_major_step_end
  :  ?failure_step:bool
  -> synth_time:float
  -> verif_time:float
  -> t:int
  -> u:int
  -> bool
  -> unit

type verif_method =
  | BoundedChecking
  | Induction

val verif_method_to_str : verif_method -> string
val counterexample_classification_method : verif_method option ref
val update_counterexample_classification_method : verif_method -> unit
val last_lemma_synthesized : (string * string) option ref
val last_lemma_proved : verif_method option ref
val set_lemma_synthesized : string -> string -> unit
val set_last_lemma_proof_method : verif_method -> unit
val log_minor_step : synth_time:float -> auxtime:float -> bool -> unit

(** Reset all the algorithm counters. Call between two different configurations. *)
val restart : unit -> unit

(** A counter for the number of times a configuration is unrealizable according to the cache. *)
val num_unr_cache_hits : int ref

(** Boolean indicates whether counfiguration exploration lead to the original configuartion given
    by the user.
*)
val orig_solution_hit : bool ref

(** The number of times a lemma that has been created for a different configuration is reused.
*)
val num_foreign_lemma_uses : int ref
