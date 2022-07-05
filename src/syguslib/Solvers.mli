(**
    This module contains functions to interface with syntax-guided synthesis solvers using
    the SyGuS Language Standard Version 2 or 1 defined in {!module-Sygus}.
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

(** A module to provide the system configuration for syntax-guided synthesis solver.
    It indicates the path to the solver executables on the system.
    CVC4 and CVC5 are treted as one solver, with the boolean [use_cvc5 ()] setting
    which version of CVC to use.
*)
module type SolverSystemConfig = sig
  val cvc_binary_path : unit -> string
  val dryadsynth_binary_path : unit -> string
  val eusolver_binary_path : unit -> string
  val using_cvc5 : unit -> bool
end

type solver_instance =
  { s_name : string
  ; s_pid : int
  ; s_input_file : string
  ; s_output_file : string
  ; s_process : Lwt_process.process_out
  ; s_out_fd : Unix.file_descr
  }

val online_solvers : (int * solver_instance) list ref
val mk_tmp_sl : string -> string
val commands_to_file : Sygus.program -> string -> unit

module SygusSolver : functor
  (Stats : Statistics)
  (Log : Logger)
  (Config : SolverSystemConfig)
  -> sig
  type t =
    | CVC
    | DryadSynth
    | EUSolver

  val default_solver : t ref
  val binary_path : t -> string
  val executable_name : t -> string
  val sname : t -> string
  val print_options : Format.formatter -> string list -> unit
  val fetch_solution : int -> string -> Sygus.solver_response
end

(** A module encapsulating function to execute SyGuS solvers asynchronously using Lwt.
  *)
module LwtSolver : functor
  (Stats : Statistics)
  (Log : Logger)
  (Config : SolverSystemConfig)
  -> sig
  module CoreSolver : sig
    type t = SygusSolver(Stats)(Log)(Config).t =
      | CVC
      | DryadSynth
      | EUSolver

    val default_solver : t ref
  end

  val solver_make_cancellable : solver_instance -> 'a Lwt.t -> unit

  val exec_solver
    :  ?timeout:float option
    -> ?solver_kind:CoreSolver.t
    -> ?options:string list
    -> string * string
    -> solver_instance * Sygus.solver_response option Lwt.t * int Lwt.u

  val solve_commands
    :  ?timeout:float option
    -> ?solver_kind:CoreSolver.t
    -> Sygus.program
    -> Sygus.solver_response option Lwt.t * int Lwt.u
end

(** A module encapsulating function to execute SyGuS solvers synchronously.
  *)
module SyncSolver : functor
  (Stats : Statistics)
  (Log : Logger)
  (Config : SolverSystemConfig)
  -> sig
  module CoreSolver : sig
    type t = SygusSolver(Stats)(Log)(Config).t =
      | CVC
      | DryadSynth
      | EUSolver

    val default_solver : t ref
  end

  (** Kill a solver, given its process id. Ignores errors.  *)
  val kill_solver : int -> unit

  (** Execute a solver on a given sygus set of commands. [exec_solver ~solver_kind ~options ~pid
      ~error_log (inputfile, outputfile)] executes the solver [~solver_kind] (defaults to CVC) with
      options [~options] and stores the process id of the solver process in [~pid] as soon as the
      process is started. The solver is executed by printing the commands in [inputfile] and
      outputting the solver response in [outputfile].
  *)
  val exec_solver
    :  ?solver_kind:CoreSolver.t
    -> ?options:string list
    -> ?pid:int ref
    -> ?error_log:string option
    -> string * string
    -> Sygus.solver_response

  (** [solve_commands ~solver_kind ~pid program] solvers the set of commands in [program] using the
    solver [solver_kind] (default to CVC) and stores the process id of the solver in [pid]. The user
    can use [pid] as a handle to kill the process if necessary.
  *)
  val solve_commands
    :  ?solver_kind:CoreSolver.t
    -> ?pid:int ref
    -> Sygus.program
    -> Sygus.solver_response
end
