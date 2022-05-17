(**
    This module contains functions to interface with syntax-guided synthesis solvers using
    the SMTLIB language defined in
    The synchronous and asynchronous solvers defined here are functors parametric on Logger and
    Statistics modules to automate logging on some output and collecting statistics
    on solver usage.
*)

(** {1 Log and Stats modules } *)

(** The Logger module must provide some basic logging functionality,
    including error, debug and verbose messages. One can also set
    log_queries to true with a file, which allows the solver to write
    queries to a separate file.
 *)
module type Logger = sig
  val error : (Format.formatter -> unit -> unit) -> unit
  val debug : (Format.formatter -> unit -> unit) -> unit
  val verb : (Format.formatter -> unit -> unit) -> unit
  val log_file : string
  val verbose : bool
  val log_queries : bool
end

(** An empty logger that ignores messages, and can be used to construct a solver if
    no logging is desired. *)
module EmptyLog : Logger

(** The statistics modules allows logging start/termination time of subprocesses used
    to solve the syntax-guided synthesis instances. It should also provide a function that
    returns the time elapsed in the program.
*)
module type Statistics = sig
  val log_proc_start : int -> unit
  val log_solver_start : int -> string -> unit
  val log_proc_restart : int -> unit
  val log_alive : int -> unit
  val log_proc_quit : ?status:int -> int -> unit
  val get_elapsed : int -> float
end

(** An empty Stats module that does nothing. *)
module NoStat : Statistics

(** {1 Blocking solver} *)
module Synchronous : functor (Log : Logger) (Stats : Statistics) -> sig
  type solver_response = SmtLib.solver_response

  val is_sat : solver_response -> bool
  val is_unsat : solver_response -> bool

  type online_solver =
    { s_name : string
    ; s_pid : int
    ; s_inputc : out_channel
    ; s_outputc : in_channel
    ; mutable s_online : bool
    ; mutable s_scope_level : int
    ; s_declared : (string, int) Base.Hashtbl.t
    ; s_log_file : string
    ; s_log_outc : out_channel
    }

  val open_log : unit -> unit
  val log : ?solver:online_solver option -> SmtLib.command -> unit
  val already_declared : online_solver -> SmtLib.smtSymbol -> bool
  val solver_declare : online_solver -> SmtLib.smtSymbol -> unit
  val exec_command : online_solver -> SmtLib.command -> solver_response
  val make_solver : ?hint:string -> name:string -> string -> string list -> online_solver
  val close_solver : online_solver -> unit
  val call_solver : online_solver -> SmtLib.command list -> solver_response

  exception SolverError of Ppx_sexp_conv_lib.Sexp.t list

  val solver_response_errors : solver_response -> Ppx_sexp_conv_lib.Sexp.t list
  val pp_solver_response : Format.formatter -> solver_response -> unit
  val check_sat : online_solver -> solver_response
  val exec_all : online_solver -> SmtLib.command list -> unit
  val get_model : online_solver -> solver_response
  val load_min_max_defs : online_solver -> unit
  val set_logic : online_solver -> Logics.logic -> unit
  val set_option : online_solver -> string -> string -> unit
  val smt_assert : online_solver -> SmtLib.smtTerm -> unit
  val spop : online_solver -> unit
  val spush : online_solver -> unit
end

module Asyncs : functor (Log : Logger) (Stats : Statistics) -> sig
  type response = SmtLib.solver_response Lwt.t

  type solver =
    { s_name : string
    ; s_pinfo : Lwt_process.process
    ; s_pid : int
    ; s_inputc : Lwt_io.output_channel
    ; s_outputc : Lwt_io.input_channel
    ; mutable s_scope_level : int
    ; s_declared : (string, int) Base.Hashtbl.t
    ; s_log_file : string
    ; s_log_outc : out_channel
    }

  val async_online_solvers : (int * solver) list ref
  val _last_resp : (int, int * Ppx_sexp_conv_lib.Sexp.t) Base.Hashtbl.t
  val solver_verbose_response_summary : solver -> Ppx_sexp_conv_lib.Sexp.t -> unit
  val solver_verbose_pp_last_resp_count : solver -> unit
  val already_declared : solver -> SmtLib.smtSymbol -> bool
  val solver_read : solver -> response
  val solver_write : solver -> SmtLib.command -> unit Lwt.t
  val solver_declare : solver -> SmtLib.smtSymbol -> unit
  val open_log : unit -> unit
  val log : ?solver:solver option -> SmtLib.command -> unit
  val exec_command : solver -> SmtLib.command -> response

  val make_solver
    :  ?hint:string
    -> name:string
    -> string
    -> string list
    -> solver * int Lwt.t * int Lwt.u

  val solver_make_cancellable : solver -> 'a Lwt.t -> unit
  val close_solver : solver -> unit Lwt.t
  val check_sat : solver -> response
  val exec_all : solver -> SmtLib.command list -> unit Lwt.t
  val get_model : solver -> response
  val load_min_max_defs : solver -> unit Lwt.t
  val set_logic : solver -> Logics.logic -> unit Lwt.t
  val set_option : solver -> string -> string -> unit Lwt.t
  val smt_assert : solver -> SmtLib.smtTerm -> unit Lwt.t
  val spop : solver -> unit Lwt.t
  val spush : solver -> unit Lwt.t

  val cancellable_task
    :  solver * int Lwt.t * int Lwt.u
    -> (solver * int Lwt.t -> 'b Lwt.t)
    -> 'b Lwt.t * int Lwt.u
end
