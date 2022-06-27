open Core
open Sexplib
open SmtLib
module OC = Stdio.Out_channel
module IC = Stdio.In_channel

let log_out = ref None

let pp_link frmt target =
  Fmt.(styled `Underline (styled (`Fg `Blue) string)) frmt ("file://" ^ target)
;;

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
  let log_file = Caml.Filename.temp_file "tmp" ".log"
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

module Synchronous (Log : Logger) (Stats : Statistics) = struct
  (* Solver response and other types. *)
  type solver_response = SmtLib.solver_response

  let is_sat s =
    match s with
    | Sat -> true
    | _ -> false
  ;;

  let is_unsat s =
    match s with
    | Unsat -> true
    | _ -> false
  ;;

  (** Type information about a solver.  *)
  type online_solver =
    { s_name : string (** The name of the solver. *)
    ; s_pid : int (** The process id of the solver.  *)
    ; s_inputc : OC.t (** The input of the solver (an output channel for the user).  *)
    ; s_outputc : IC.t (** The output of the sovler (an input channel for the user).  *)
    ; mutable s_online : bool (** Whether the solver is online.  *)
    ; mutable s_scope_level : int
          (** The scope level the solver is curently in. The scope level
     changes when push and pop operations are used.  *)
    ; s_declared : (string, int) Hashtbl.t
          (** The table of symbols declared in the solver at each scope level.  *)
    ; s_log_file : string
          (** The file that hols the log of the SMT commands issued to the solver.  *)
    ; s_log_outc : OC.t (** The output channel for logging from the solver. *)
    }

  (* Logging utilities. *)

  let open_log () = log_out := Some (OC.create Log.log_file)

  let log ?(solver = None) c =
    match solver with
    | Some s -> write_command s.s_log_outc c
    | None ->
      (match !log_out with
      | Some oc -> write_command oc c
      | None ->
        open_log ();
        (match !log_out with
        | Some oc -> write_command oc c
        | None -> Log.(error (fun fmt () -> Fmt.pf fmt "Failed to open log file."))))
  ;;

  let close_solver solver =
    Stats.log_proc_quit solver.s_pid;
    let elapsed = Stats.get_elapsed solver.s_pid in
    Log.debug
      Fmt.(
        fun fmt () ->
          pf
            fmt
            "Closing %s (spent %.3fs), log can be found in %a"
            solver.s_name
            elapsed
            (styled (`Fg `Blue) pp_link)
            solver.s_log_file);
    solver.s_online <- false;
    (try OC.output_string solver.s_inputc (Sexp.to_string (sexp_of_command mk_exit)) with
    | _ -> ());
    try
      IC.close solver.s_outputc;
      OC.close solver.s_log_outc
    with
    | _ -> ()
  ;;

  let solver_write (solver : online_solver) (c : command) : bool =
    if solver.s_online
    then (
      try
        write_command solver.s_inputc c;
        OC.output_char solver.s_inputc '\n';
        OC.flush solver.s_inputc;
        true
      with
      | Sys_error s ->
        Log.error
          Fmt.(fun fmt () -> pf fmt "Cannot write to solve (%s). Ignoring command." s);
        (* Tag the solver as offline  *)
        close_solver solver;
        false)
    else false
  ;;

  let solver_read (solver : online_solver) : solver_response =
    if solver.s_online
    then (
      let l =
        try Ok (Sexp.input_sexp solver.s_outputc) with
        | End_of_file -> Error (Sexp.List [ Atom "solver_read"; Atom "end of file" ])
        | Sys_error _ ->
          Error (Sexp.List [ Atom "solver_read"; Atom "Couldn't read solver answer." ])
      in
      match Result.map ~f:(fun l -> parse_response [ l ]) l with
      | Ok r -> r
      | Error sexps ->
        Log.error (fun fmt () -> Sexp.pp fmt (List [ Atom "error"; sexps ]));
        SExps [ Atom "error"; sexps ])
    else SExps [ Atom "error"; Atom "solver_offline" ]
  ;;

  let already_declared (solver : online_solver) (s : smtSymbol) : bool =
    match Hashtbl.find solver.s_declared (str_of_symb s) with
    | Some _ ->
      (* Do not write command if variable is already declared. *)
      true
    | None -> false
  ;;

  let solver_declare (solver : online_solver) (s : smtSymbol) : unit =
    Hashtbl.set solver.s_declared ~key:(str_of_symb s) ~data:solver.s_scope_level
  ;;

  let exec_command (solver : online_solver) (c : command) : solver_response =
    (* Guard execution of command for declarations and keep track of scope level and declared
       variables.
    *)
    let do_exec =
      match c with
      | DefineSmtSort (s, _, _)
      | DeclareDatatype (s, _)
      | DeclareConst (s, _)
      | DefineFunRec (s, _, _, _)
      | DefineFun (s, _, _, _)
      | DeclareFun (s, _, _) ->
        if already_declared solver s
        then false
        else (
          solver_declare solver s;
          true)
      | DefineFunsRec (decls, _) ->
        if List.exists ~f:(fun (a, _, _) -> already_declared solver a) decls
        then false
        else (
          List.iter ~f:(fun (a, _, _) -> solver_declare solver a) decls;
          true)
      | DeclareDatatypes (sl, _) ->
        if List.exists ~f:(fun (a, _) -> already_declared solver a) sl
        then false
        else (
          List.iter ~f:(fun (a, _) -> solver_declare solver a) sl;
          true)
      | Push i ->
        solver.s_scope_level <- solver.s_scope_level + i;
        true
      | Pop i ->
        solver.s_scope_level <- solver.s_scope_level - i;
        Hashtbl.filter_inplace solver.s_declared ~f:(fun level ->
            level <= solver.s_scope_level);
        true
      | _ -> true
    in
    if not solver.s_online
    then Error (Fmt.str "Solver is offline")
    else if do_exec
    then (
      if Log.log_queries then log ~solver:(Some solver) c;
      if solver_write solver c
      then solver_read solver
      else
        SExps
          [ Atom "error"; List [ Atom "exec_command"; Atom "Could not talk to solver" ] ])
    else Error (Fmt.str "Variable already declared")
  ;;

  (* keep track of all solvers we spawn, so we can close our read/write
     FDs when the solvers exit *)
  let online_solvers : (int * online_solver) list ref = ref []

  let handle_sigchild (_ : int) : unit =
    if List.length !online_solvers = 0
    then ignore @@ Caml_unix.wait ()
    else (
      let pid =
        let pid, msg = Caml_unix.wait () in
        if Log.verbose
        then Log.debug (fun fmt () -> Fmt.pf fmt "Solver (pid %d) exited!" pid);
        (match msg with
        | Caml_unix.WEXITED i ->
          if not (i = 0)
          then Log.debug Fmt.(fun fmt () -> pf fmt "%i returned non-zero error: %i" pid i)
        | Caml_unix.WSIGNALED s ->
          Log.debug Fmt.(fun fmt () -> pf fmt "%i signalled with: %i" pid s)
        | Caml_unix.WSTOPPED s ->
          Log.debug Fmt.(fun fmt () -> pf fmt "%i stopped with code: %i" pid s));
        pid
      in
      match List.Assoc.find !online_solvers ~equal:( = ) pid with
      | Some solver ->
        if Log.verbose
        then
          Log.debug
            Fmt.(
              fun fmt () ->
                pf fmt "Solver %s log is in: %a" solver.s_name pp_link solver.s_log_file);
        OC.close_no_err solver.s_inputc;
        solver.s_online <- false;
        (try IC.close solver.s_outputc with
        | _ -> ())
      | None -> ())
  ;;

  let () = Caml.Sys.set_signal Caml.Sys.sigchld (Caml.Sys.Signal_handle handle_sigchild)

  let make_solver ?(hint = "") ~(name : string) (path : string) (options : string list)
      : online_solver
    =
    let open Core_unix in
    let pinfo = create_process ~prog:path ~args:options in
    (* If the ocaml ends of the pipes aren't marked close-on-exec, they
       will remain open in the fork/exec'd solver process, and the solver won't exit
       when our main ocaml process ends. *)
    set_close_on_exec pinfo.stdout;
    set_close_on_exec pinfo.stderr;
    (* The stdout of the solver is our input channel. *)
    let in_chan = in_channel_of_descr pinfo.stdout in
    (* The stdin of the solver is our output channel.  *)
    let out_chan = out_channel_of_descr pinfo.stdin in
    OC.set_binary_mode out_chan false;
    IC.set_binary_mode in_chan false;
    let log_file = Caml.Filename.temp_file (name ^ "_") ".smt2" in
    let solver =
      { s_name = name
      ; s_pid = Pid.to_int pinfo.pid
      ; s_inputc = out_chan
      ; s_outputc = in_chan
      ; s_online = true
      ; s_declared = Hashtbl.create (module String)
      ; s_scope_level = 0
      ; s_log_file = log_file
      ; s_log_outc = OC.create log_file
      }
    in
    Stats.log_solver_start solver.s_pid name;
    online_solvers := (Pid.to_int pinfo.pid, solver) :: !online_solvers;
    Log.debug
      Fmt.(
        fun fmt () ->
          pf
            fmt
            "%sSolver %s started:  pid: %i log: %a"
            hint
            solver.s_name
            solver.s_pid
            pp_link
            solver.s_log_file);
    try
      match exec_command solver mk_print_success with
      | Success -> solver
      | _ as resp ->
        Log.error Fmt.(fun fmt () -> pf fmt "Solver: %a" pp_solver_response resp);
        failwith "could not configure solver to :print-success"
    with
    | Sys_error s ->
      failwith ("couldn't talk to solver, double-check path. Sys_error " ^ s)
  ;;

  (** Returns empty response if commands is empty, otherwise, executes the LAST command in the
    command list.
*)
  let call_solver solver commands =
    match List.last commands with
    | Some c -> exec_command solver c
    | None ->
      Log.(error Fmt.(fun fmt () -> pf fmt "Called solver without any command."));
      SExps []
  ;;

  (* Helpers for solver calls to check simple formula satisfiability,
     simplify an expression, get unsat cores.
  *)
  exception SolverError of Sexp.t list

  let solver_response_errors (response : solver_response) =
    let rec is_error_sexp sexp =
      match sexp with
      | Sexp.Atom _ -> []
      | List (Atom "error" :: _) -> [ sexp ]
      | List ls -> Caml.List.flatten (List.map ~f:is_error_sexp ls)
    in
    match response with
    | SExps l -> Caml.List.flatten (List.map ~f:is_error_sexp l)
    | _ -> []
  ;;

  let pp_solver_response = SmtLib.pp_solver_response

  (* === Command helpers ===  *)

  let check_sat s = exec_command s mk_check_sat

  let exec_all s commands =
    List.iter ~f:(fun decl -> ignore (exec_command s decl)) commands
  ;;

  let get_model s = exec_command s GetModel

  let load_min_max_defs s =
    ignore (exec_command s mk_max_def);
    ignore (exec_command s mk_min_def)
  ;;

  let set_logic solver logic = ignore (exec_command solver (mk_set_logic logic))

  let set_option solver option_id option_value =
    ignore (exec_command solver (SetOption (option_id, option_value)))
  ;;

  let smt_assert s term = ignore (exec_command s (mk_assert term))
  let spop solver = ignore (exec_command solver (mk_pop 1))
  let spush solver = ignore (exec_command solver (mk_push 1))
end

(* ============================================================================================= *)
(*                          Async solver for parallel solving                                    *)
(* ============================================================================================= *)

(** Async versions of the solver operations for use in concurrent threads.  *)
module Asyncs (Log : Logger) (Stats : Statistics) = struct
  open Lwt

  type response = solver_response t

  type solver =
    { s_name : string
    ; s_pinfo : Lwt_process.process
    ; s_pid : int
    ; s_inputc : Lwt_io.output_channel
    ; s_outputc : Lwt_io.input_channel
    ; mutable s_scope_level : int
    ; s_declared : (string, int) Hashtbl.t
    ; s_log_file : string
    ; s_log_outc : OC.t
    }

  let async_online_solvers : (int * solver) list ref = ref []
  let _last_resp : (int, int * Sexp.t) Hashtbl.t = Hashtbl.create (module Int)

  let solver_verbose_response_summary solver s =
    (* For verbose debugging, print non-sucess messages.
         Declarations errors are ignored in Z3, it might be useful to see them.
    *)
    match s with
    | Sexp.Atom "success" -> ()
    | Sexp.List (Sexp.Atom "error" :: err_msgs) ->
      List.iter
        ~f:(fun err ->
          Log.error (fun fmt () ->
              Fmt.pf fmt "%s error: %a" solver.s_name Sexp.pp_hum err))
        err_msgs
    | _ ->
      let resp_ind, prev_resp =
        Option.value ~default:(0, Sexp.Atom "none") (Hashtbl.find _last_resp solver.s_pid)
      in
      let changed =
        if Sexp.equal prev_resp s
        then (
          Hashtbl.set _last_resp ~key:solver.s_pid ~data:(resp_ind + 1, s);
          false)
        else (
          Hashtbl.set _last_resp ~key:solver.s_pid ~data:(0, s);
          true)
      in
      if changed
      then (
        if resp_ind > 0
        then
          Log.verb (fun frmt () ->
              Fmt.pf frmt "%6s [%6i] ... %i more." solver.s_name solver.s_pid resp_ind);
        Log.verb (fun frmt () ->
            Fmt.pf frmt "%6s [%6i] 📢@; @[%a@]" solver.s_name solver.s_pid Sexp.pp_hum s))
      else ()
  ;;

  let solver_verbose_pp_last_resp_count solver =
    let resp_ind, s =
      Option.value ~default:(0, Sexp.Atom "none") (Hashtbl.find _last_resp solver.s_pid)
    in
    if resp_ind > 0
    then
      Log.verb (fun frmt () ->
          Fmt.pf
            frmt
            "%6s [%6i] ... %i more \"%a\" answers."
            solver.s_name
            solver.s_pid
            resp_ind
            Sexp.pp_hum
            s)
  ;;

  let already_declared (solver : solver) (s : smtSymbol) : bool =
    match Hashtbl.find solver.s_declared (str_of_symb s) with
    | Some _ ->
      (* Do not write command if variable is already declared. *)
      true
    | None -> false
  ;;

  let solver_read (solver : solver) : response =
    try
      (* Read lines until concatenation of lines can be parsed as a S-Expression. *)
      let rec read_until_sexp buf =
        let%lwt s = Lwt_io.read_line solver.s_outputc in
        Buffer.add_string buf s;
        try return (Sexp.of_string (String.strip (Buffer.contents buf))) with
        | Failure _ -> read_until_sexp buf
      in
      let%lwt s = read_until_sexp (Buffer.create 10) in
      solver_verbose_response_summary solver s;
      return (parse_response [ s ])
    with
    | _ -> return (Error "Error reading solver response.")
  ;;

  let solver_write (solver : solver) (c : command) : unit t =
    let comm_s = Sexp.to_string_hum (sexp_of_command c) in
    let%lwt () = Lwt_io.write_line solver.s_inputc comm_s in
    Lwt_io.flush solver.s_inputc
  ;;

  let solver_declare (solver : solver) (s : smtSymbol) : unit =
    Hashtbl.set solver.s_declared ~key:(str_of_symb s) ~data:solver.s_scope_level
  ;;

  let open_log () = log_out := Some (OC.create Log.log_file)

  let log ?(solver = None) c =
    match solver with
    | Some s ->
      (try write_command s.s_log_outc c with
      | _ -> ())
    | None ->
      (match !log_out with
      | Some oc -> write_command oc c
      | None ->
        open_log ();
        (match !log_out with
        | Some oc -> write_command oc c
        | None -> Log.(error Fmt.(fun fmt () -> pf fmt "Failed to open log file."))))
  ;;

  let exec_command (solver : solver) (c : command) : response =
    (* Guard execution of command for declarations and keep track of scope level and declared
       variables.
    *)
    let do_exec =
      match c with
      | DefineSmtSort (s, _, _)
      | DeclareDatatype (s, _)
      | DeclareConst (s, _)
      | DefineFunRec (s, _, _, _)
      | DefineFun (s, _, _, _)
      | DeclareFun (s, _, _) ->
        if already_declared solver s
        then false
        else (
          solver_declare solver s;
          true)
      | DefineFunsRec (decls, _) ->
        if List.exists ~f:(fun (a, _, _) -> already_declared solver a) decls
        then false
        else (
          List.iter ~f:(fun (a, _, _) -> solver_declare solver a) decls;
          true)
      | DeclareDatatypes (sl, _) ->
        if List.exists ~f:(fun (a, _) -> already_declared solver a) sl
        then false
        else (
          List.iter ~f:(fun (a, _) -> solver_declare solver a) sl;
          true)
      | Push i ->
        solver.s_scope_level <- solver.s_scope_level + i;
        true
      | Pop i ->
        solver.s_scope_level <- solver.s_scope_level - i;
        Hashtbl.filter_inplace solver.s_declared ~f:(fun level ->
            level <= solver.s_scope_level);
        true
      | _ -> true
    in
    if do_exec
    then (
      if Log.log_queries then log ~solver:(Some solver) c;
      let%lwt () = solver_write solver c in
      solver_read solver)
    else return Success
  ;;

  let make_solver ?(hint = "") ~(name : string) (path : string) (options : string list)
      : solver * int t * int u
    =
    let pinfo = Lwt_process.open_process (path, Array.of_list options) in
    (* Lwt_unix.set_close_on_exec;
    Lwt_unix.set_close_on_exec pinfo#stderr; *)
    let log_file = Caml.Filename.temp_file (name ^ "_") ".smt2" in
    let solver =
      { s_name = name
      ; s_pinfo = pinfo
      ; s_pid = pinfo#pid
      ; s_inputc = pinfo#stdin
      ; s_outputc = pinfo#stdout
      ; s_declared = Hashtbl.create (module String)
      ; s_scope_level = 0
      ; s_log_file = log_file
      ; s_log_outc = OC.create log_file
      }
    in
    Stats.log_solver_start solver.s_pid name;
    async_online_solvers := (pinfo#pid, solver) :: !async_online_solvers;
    (* The solver returned is bound to a task that can be cancelled. *)
    try
      let (m, task_r) : int t * int u = Lwt.task () in
      Log.debug
        Fmt.(
          fun fmt () ->
            pf
              fmt
              "%sSolver %s started:  pid: %i log: %a"
              hint
              solver.s_name
              solver.s_pid
              pp_link
              solver.s_log_file);
      ( solver
      , Lwt.bind m (fun i ->
            let%lwt r = exec_command solver mk_print_success in
            match r with
            | Success -> return i
            | _ -> failwith "could not configure solver to :print-success")
      , task_r )
    with
    | Sys_error s ->
      failwith ("couldn't talk to solver, double-check path. Sys_error " ^ s)
  ;;

  let close_solver (solver : solver) : unit t =
    Stats.log_proc_quit solver.s_pid;
    let elapsed = Stats.get_elapsed solver.s_pid in
    solver_verbose_pp_last_resp_count solver;
    OC.close solver.s_log_outc;
    Log.debug
      Fmt.(
        fun fmt () ->
          pf
            fmt
            "Closing %s (spent %.3fs), log can be found in %a"
            solver.s_name
            elapsed
            pp_link
            solver.s_log_file);
    let%lwt _ = exec_command solver mk_exit in
    let%lwt () = Lwt_io.close solver.s_outputc in
    let%lwt () = Lwt_io.close solver.s_inputc in
    return ()
  ;;

  let solver_make_cancellable (s : solver) (p : 'a t) : unit =
    (* IF task is cancelled, kill the solver.  *)
    let callc () =
      match s.s_pinfo#state with
      | Lwt_process.Exited _ -> ()
      | Running ->
        Stats.log_proc_quit s.s_pid;
        Log.debug
          Fmt.(
            fun fmt () ->
              pf
                fmt
                "Terminating solver %s (PID : %i) (log: %a)"
                s.s_name
                s.s_pid
                pp_link
                s.s_log_file);
        s.s_pinfo#terminate;
        Lwt.(async (fun () -> map (fun _ -> ()) s.s_pinfo#close));
        Lwt.(async (fun () -> Lwt_io.close s.s_outputc));
        Lwt.(async (fun () -> Lwt_io.close s.s_inputc));
        ()
    in
    Lwt.on_cancel p callc
  ;;

  let check_sat s : response = exec_command s mk_check_sat

  let exec_all (s : solver) (decls : command list) : unit t =
    List.fold
      ~f:(fun _ decl ->
        let%lwt resp = exec_command s decl in
        match resp with
        | Error s -> raise (Failure s)
        | _ -> return ())
      decls
      ~init:(return ())
  ;;

  let get_model (s : solver) : response = exec_command s GetModel

  let load_min_max_defs (s : solver) : unit t =
    exec_command s mk_max_def
    >>= (fun _ -> exec_command s mk_min_def)
    >>= fun _ -> return ()
  ;;

  let set_logic solver logic : unit t =
    let%lwt _ = exec_command solver (mk_set_logic logic) in
    return ()
  ;;

  let set_option (solver : solver) (option_id : string) (option_value : string) : unit t =
    let%lwt _ = exec_command solver (SetOption (option_id, option_value)) in
    return ()
  ;;

  let smt_assert (s : solver) (term : smtTerm) : unit Lwt.t =
    let%lwt _ = exec_command s (mk_assert term) in
    Stats.log_alive s.s_pid;
    return ()
  ;;

  let spop (solver : solver) : unit Lwt.t =
    Stats.log_alive solver.s_pid;
    let%lwt _ = exec_command solver (mk_pop 1) in
    return ()
  ;;

  let spush (solver : solver) : unit Lwt.t =
    Stats.log_alive solver.s_pid;
    let%lwt _ = exec_command solver (mk_push 1) in
    return ()
  ;;

  let cancellable_task
      ((s, starter, resolver) : solver * int t * int u)
      (task_func : solver * 'a t -> 'b t)
      : 'b t * int u
    =
    let task = task_func (s, starter) in
    solver_make_cancellable s task;
    task, resolver
  ;;
end
