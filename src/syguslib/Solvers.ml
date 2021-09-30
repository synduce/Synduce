open Base
open Sygus
open Sexplib
open Lwt_process
module OC = Stdio.Out_channel
module IC = Stdio.In_channel
open Lwt.Syntax

(* Logging utilities. *)

(** Logger for the solvers. A solver might log error, debug and verbose messages.
*)
module type Logger = sig
  val error : (Formatter.t -> unit -> unit) -> unit

  val debug : (Formatter.t -> unit -> unit) -> unit

  val verb : (Formatter.t -> unit -> unit) -> unit

  val log_file : string

  val verbose : bool

  val log_queries : bool
end

(** An empty logging module that can be used to create silent solver instances.  *)
module EmptyLog : Logger = struct
  let error _ = ()

  let debug _ = ()

  let verb _ = ()

  let log_file = "tmp"

  let verbose = false

  let log_queries = false
end

(** A module signature to log satistics about solver usage. *)
module type Statistics = sig
  val log_proc_start : int -> unit

  val log_solver_start : int -> string -> unit

  val log_proc_restart : int -> unit

  val log_alive : int -> unit

  val log_proc_quit : ?status:int -> int -> unit

  val get_elapsed : int -> float
end

module NoStat : Statistics = struct
  let log_proc_start _ = ()

  let log_solver_start _ _ = ()

  let log_proc_restart _ = ()

  let log_alive _ = ()

  let log_proc_quit ?(status = 0) _ = ignore status

  let get_elapsed _ = 0.
end

module type SolverSystemConfig = sig
  val cvc_binary_path : unit -> string

  val dryadsynth_binary_path : unit -> string

  val eusolver_binary_path : unit -> string

  val using_cvc5 : unit -> bool
end

type solver_instance = {
  s_name : string;
  s_pid : int;
  s_input_file : string;
  s_output_file : string;
  s_process : process_out;
}

let online_solvers : (int * solver_instance) list ref = ref []

let mk_tmp_sl prefix = Caml.Filename.temp_file prefix ".sl"

let commands_to_file (commands : program) (filename : string) =
  let out_chan = OC.create filename in
  let fout = Stdlib.Format.formatter_of_out_channel out_chan in
  Fmt.set_utf_8 fout false;
  Stdlib.Format.pp_set_margin fout 100;
  List.iter commands ~f:(fun c ->
      SyCommand.pp_hum fout c;
      Fmt.(pf fout "@."));
  OC.close out_chan

module SygusSolver (Stats : Statistics) (Log : Logger) (Config : SolverSystemConfig) = struct
  type t = CVC | DryadSynth | EUSolver

  let default_solver = ref CVC

  let binary_path = function
    | CVC -> Config.cvc_binary_path ()
    | DryadSynth -> Config.dryadsynth_binary_path ()
    | EUSolver -> Config.eusolver_binary_path ()

  let executable_name x =
    let using_cvc5 = Config.using_cvc5 () in
    match x with
    | CVC -> if using_cvc5 then "cvc5" else "cvc4"
    | DryadSynth -> Caml.Filename.basename (Config.dryadsynth_binary_path ())
    | EUSolver -> Caml.Filename.basename (Config.eusolver_binary_path ())

  let sname = function CVC -> "CVC-SyGuS" | DryadSynth -> "DryadSynth" | EUSolver -> "EUSolver"

  let print_options (frmt : Formatter.t) =
    Fmt.(list ~sep:sp (fun fmt opt -> pf fmt "--%s" opt) frmt)

  let fetch_solution pid filename =
    Log.debug Fmt.(fun fmt () -> pf fmt "Fetching solution in %s" filename);
    let r = reponse_of_sexps (Sexp.input_sexps (Stdio.In_channel.create filename)) in
    Stats.log_proc_quit pid;
    r

  let solver_make_cancellable (s : solver_instance) (p : 'a Lwt.t) : unit =
    (* IF task is cancelled, kill the solver.  *)
    Lwt.on_cancel p (fun () ->
        match s.s_process#state with
        | Lwt_process.Exited _ -> ()
        | Running ->
            Stats.log_proc_quit s.s_process#pid;
            Log.debug
              Fmt.(
                fun fmt () ->
                  pf fmt "Terminating solver %s (PID : %i) (log: %s)" s.s_name s.s_pid
                    s.s_output_file);
            s.s_process#terminate)

  let exec_solver ?(solver_kind = !default_solver) ?(options = [])
      ((inputfile, outputfile) : string * string) :
      solver_instance * solver_response option Lwt.t * int Lwt.u =
    let command =
      (binary_path solver_kind, Array.of_list (executable_name solver_kind :: inputfile :: options))
    in
    let out_fd = Unix.openfile outputfile [ Unix.O_RDWR; Unix.O_TRUNC; Unix.O_CREAT ] 0o644 in
    let process = open_process_out ~stdout:(`FD_move out_fd) command in
    let solver =
      {
        s_name = sname solver_kind;
        s_pid = process#pid;
        s_output_file = outputfile;
        s_input_file = inputfile;
        s_process = process;
      }
    in
    Stats.log_solver_start solver.s_pid solver.s_name;
    Log.debug
      Fmt.(
        fun fmt () ->
          pf fmt "%s (pid : %i) solving %s -> %s" solver.s_name solver.s_pid inputfile outputfile);
    try
      let t, r = Lwt.task () in
      ( solver,
        Lwt.bind t (fun _ ->
            let* status = process#status in
            match status with
            | Unix.WEXITED 0 -> Lwt.return (Some (fetch_solution solver.s_pid outputfile))
            | Unix.WEXITED i ->
                Log.error
                  Fmt.(
                    fun fmt () ->
                      pf fmt "Solver %s (pid : %i) exited with code %i." solver.s_name solver.s_pid
                        i);
                Lwt.return None
            | Unix.WSIGNALED i ->
                Log.error Fmt.(fun fmt () -> pf fmt "Solver signaled with code %i." i);
                Lwt.return None (* TODO error messages. *)
            | Unix.WSTOPPED i ->
                Log.error Fmt.(fun fmt () -> pf fmt "Solver stopped with code %i." i);
                Lwt.return None),
        r )
    with Sys_error s -> failwith ("couldn't talk to solver, double-check path. Sys_error " ^ s)

  let solve_commands (p : program) : solver_response option Lwt.t * int Lwt.u =
    let inputfile = mk_tmp_sl "in_" in
    let outputfile = mk_tmp_sl "out_" in
    commands_to_file p inputfile;
    let s, t, r = exec_solver (inputfile, outputfile) in
    solver_make_cancellable s t;
    (t, r)
end
