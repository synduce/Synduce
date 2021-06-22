open Core
open Sexplib
open SmtLib
open Utils
module OC = Stdio.Out_channel
module IC = Stdio.In_channel

(* Solver response and other types. *)
type solver_response = SmtLib.solver_response

let is_sat s = match s with Sat -> true | _ -> false

let is_unsat s = match s with Unsat -> true | _ -> false

type online_solver = {
  s_name : string;
  s_pid : int;
  s_inputc : OC.t;
  s_outputc : IC.t;
  mutable s_scope_level : int;
  s_declared : (string, int) Hashtbl.t;
  s_log_file : string;
  s_log_outc : OC.t;
}

(* Logging utilities. *)

let log_out = ref None

let open_log () = log_out := Some (OC.create !Config.smt_solver_log_file)

let log ?(solver = None) c =
  match solver with
  | Some s -> write_command s.s_log_outc c
  | None -> (
      match !log_out with
      | Some oc -> write_command oc c
      | None -> (
          open_log ();
          match !log_out with
          | Some oc -> write_command oc c
          | None -> Log.(error (wrap "Failed to open log file."))))

let solver_write (solver : online_solver) (c : command) : unit =
  write_command solver.s_inputc c;
  OC.output_char solver.s_inputc '\n';
  OC.flush solver.s_inputc

let solver_read (solver : online_solver) : solver_response =
  let l =
    try Ok (Sexp.input_sexp solver.s_outputc)
    with Sys_error _ ->
      Error (Sexp.List [ Atom "solver_read"; Atom "Couldn't read solver answer." ])
  in
  match Result.map ~f:(fun l -> parse_response [ l ]) l with
  | Ok r -> r
  | Error sexps -> SExps [ Atom "error"; sexps ]

let already_declared (solver : online_solver) (s : smtSymbol) : bool =
  match Hashtbl.find solver.s_declared (str_of_symb s) with
  | Some _ ->
      (* Do not write command if variable is already declared. *)
      true
  | None -> false

let solver_declare (solver : online_solver) (s : smtSymbol) : unit =
  Hashtbl.set solver.s_declared ~key:(str_of_symb s) ~data:solver.s_scope_level

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
        if already_declared solver s then false
        else (
          solver_declare solver s;
          true)
    | DefineFunsRec (decls, _) ->
        if List.exists ~f:(fun (a, _, _) -> already_declared solver a) decls then false
        else (
          List.iter ~f:(fun (a, _, _) -> solver_declare solver a) decls;
          true)
    | DeclareDatatypes (sl, _) ->
        if List.exists ~f:(fun (a, _) -> already_declared solver a) sl then false
        else (
          List.iter ~f:(fun (a, _) -> solver_declare solver a) sl;
          true)
    | Push i ->
        solver.s_scope_level <- solver.s_scope_level + i;
        true
    | Pop i ->
        solver.s_scope_level <- solver.s_scope_level - i;
        Hashtbl.filter_inplace solver.s_declared ~f:(fun level -> level <= solver.s_scope_level);
        true
    | _ -> true
  in
  if do_exec then (
    if !Config.smt_log_queries then log ~solver:(Some solver) c;
    solver_write solver c;
    solver_read solver)
  else Error (Fmt.str "Variable already declared")

(* keep track of all solvers we spawn, so we can close our read/write
   FDs when the solvers exit *)
let online_solvers : (int * online_solver) list ref = ref []

let handle_sigchild (_ : int) : unit =
  if List.length !online_solvers = 0 then ignore @@ Unix.wait (`Group (Pid.of_int (-1)))
  else
    let pid =
      let pid, msg = Unix.wait (`Group (Pid.of_int (-1))) in
      let pid = Pid.to_int pid in
      if !Config.smt_solve_verbose then Log.error_msg (Fmt.str "Solver (pid %d) exited!" pid);
      (match msg with
      | Ok _ -> ()
      | Error (`Exit_non_zero i) ->
          if !Config.smt_solve_verbose then Log.error_msg (Fmt.str "Non-zero error = %i" i)
      | Error (`Signal sign) ->
          if !Config.smt_solve_verbose then
            Log.error_msg (Fmt.str "Signal : %s" (Signal.to_string sign)));
      pid
    in
    match List.Assoc.find !online_solvers ~equal:( = ) pid with
    | Some solver ->
        if !Config.smt_solve_verbose then
          Log.debug_msg (Fmt.str "Solver %s log is in: %s" solver.s_name solver.s_log_file);
        IC.close solver.s_outputc;
        OC.close solver.s_inputc
    | None -> ()

let () = Caml.Sys.set_signal Caml.Sys.sigchld (Caml.Sys.Signal_handle handle_sigchild)

let make_solver ~(name : string) (path : string) (options : string list) : online_solver =
  let open Unix in
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
    {
      s_name = name;
      s_pid = Pid.to_int pinfo.pid;
      s_inputc = out_chan;
      s_outputc = in_chan;
      s_declared = Hashtbl.create (module String);
      s_scope_level = 0;
      s_log_file = log_file;
      s_log_outc = OC.create log_file;
    }
  in
  online_solvers := (Pid.to_int pinfo.pid, solver) :: !online_solvers;
  try
    match exec_command solver mk_print_success with
    | Success -> solver
    | _ -> failwith "could not configure solver to :print-success"
  with Sys_error s -> failwith ("couldn't talk to solver, double-check path. Sys_error " ^ s)

let close_solver solver =
  Log.debug
    Fmt.(
      fun fmt () ->
        pf fmt "Closing %s, log can be found in %a" solver.s_name
          (styled (`Fg `Blue) string)
          solver.s_log_file);
  OC.output_string solver.s_inputc (Sexp.to_string (sexp_of_command mk_exit));
  IC.close solver.s_outputc;
  OC.close solver.s_log_outc

(** Returns empty response if commands is empty, otherwise, executes the LAST command in the
    command list.
*)
let call_solver solver commands =
  match List.last commands with
  | Some c -> exec_command solver c
  | None ->
      Log.(error_msg "Called solver without any command.");
      SExps []

(* TODO: change the path of the solver. *)

(** Create a process with a Z3 solver. *)
let make_z3_solver () = make_solver ~name:"Z3" Utils.Config.z3_binary_path [ "-in"; "-smt2" ]

(** Create a process with a CVC4 solver. *)
let make_cvc4_solver () =
  make_solver ~name:"CVC4" Utils.Config.cvc4_binary_path [ "--lang=smt2.6"; "--incremental" ]

let call_solver_default solver commands =
  match solver with
  | Some s -> call_solver s commands
  | None ->
      let s = make_z3_solver () in
      let r = call_solver s commands in
      close_solver s;
      r

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
  match response with SExps l -> Caml.List.flatten (List.map ~f:is_error_sexp l) | _ -> []

let pp_solver_response = SmtLib.pp_solver_response

(* === Command helpers ===  *)

let check_sat s = exec_command s mk_check_sat

let declare_all s decls = List.iter ~f:(fun decl -> ignore (exec_command s decl)) decls

let get_model s = exec_command s GetModel

let load_min_max_defs s =
  ignore (exec_command s mk_max_def);
  ignore (exec_command s mk_min_def)

let set_logic solver logic_id = ignore (exec_command solver (SetLogic (SSimple logic_id)))

let set_option solver option_id option_value =
  ignore (exec_command solver (SetOption (option_id, option_value)))

let smt_assert s term = ignore (exec_command s (mk_assert term))

let spop solver = ignore (exec_command solver (mk_pop 1))

let spush solver = ignore (exec_command solver (mk_push 1))

(* ============================================================================================= *)
(*                          Async solver for parallel solving                                    *)
(* ============================================================================================= *)

(** Async versions of the solver operations for use in concurrent threads.  *)
module Asyncs = struct
  open Lwt

  type response = solver_response t

  type solver = {
    s_name : string;
    s_pinfo : Lwt_process.process;
    s_pid : int;
    s_inputc : Lwt_io.output_channel;
    s_outputc : Lwt_io.input_channel;
    mutable s_scope_level : int;
    s_declared : (string, int) Hashtbl.t;
    s_log_file : string;
    s_log_outc : OC.t;
  }

  let online_solvers : (int * solver) list ref = ref []

  let already_declared (solver : solver) (s : smtSymbol) : bool =
    match Hashtbl.find solver.s_declared (str_of_symb s) with
    | Some _ ->
        (* Do not write command if variable is already declared. *)
        true
    | None -> false

  let solver_read (solver : solver) : response =
    try
      (* Read lines until concatenation of lines can be parsed as a S-Expression. *)
      let rec read_until_sexp buf =
        let%lwt s = Lwt_io.read_line solver.s_outputc in
        Buffer.add_string buf s;
        try return (Sexp.of_string (String.strip (Buffer.contents buf)))
        with Failure _ -> read_until_sexp buf
      in
      let%lwt s = read_until_sexp (Buffer.create 10) in
      (* For verbose debugging, print non-sucess messages.
         Declarations errors are ignored in Z3, it might be useful to see them.
      *)
      (match s with
      | Sexp.Atom "success" -> ()
      | _ ->
          Log.verbose (fun frmt () ->
              Fmt.pf frmt "%6s [%6i] 📢@; @[%a@]" solver.s_name solver.s_pid Sexp.pp_hum s));
      return (parse_response [ s ])
    with _ -> return (Error "Error reading solver response.")

  let solver_write (solver : solver) (c : command) : unit t =
    let comm_s = Sexp.to_string_hum (sexp_of_command c) in
    let%lwt () = Lwt_io.write_line solver.s_inputc comm_s in
    Lwt_io.flush solver.s_inputc

  let solver_declare (solver : solver) (s : smtSymbol) : unit =
    Hashtbl.set solver.s_declared ~key:(str_of_symb s) ~data:solver.s_scope_level

  let open_log () = log_out := Some (OC.create !Config.smt_solver_log_file)

  let log ?(solver = None) c =
    match solver with
    | Some s -> write_command s.s_log_outc c
    | None -> (
        match !log_out with
        | Some oc -> write_command oc c
        | None -> (
            open_log ();
            match !log_out with
            | Some oc -> write_command oc c
            | None -> Log.(error (wrap "Failed to open log file."))))

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
          if already_declared solver s then false
          else (
            solver_declare solver s;
            true)
      | DefineFunsRec (decls, _) ->
          if List.exists ~f:(fun (a, _, _) -> already_declared solver a) decls then false
          else (
            List.iter ~f:(fun (a, _, _) -> solver_declare solver a) decls;
            true)
      | DeclareDatatypes (sl, _) ->
          if List.exists ~f:(fun (a, _) -> already_declared solver a) sl then false
          else (
            List.iter ~f:(fun (a, _) -> solver_declare solver a) sl;
            true)
      | Push i ->
          solver.s_scope_level <- solver.s_scope_level + i;
          true
      | Pop i ->
          solver.s_scope_level <- solver.s_scope_level - i;
          Hashtbl.filter_inplace solver.s_declared ~f:(fun level -> level <= solver.s_scope_level);
          true
      | _ -> true
    in
    if do_exec then (
      if !Config.smt_log_queries then log ~solver:(Some solver) c;
      let%lwt () = solver_write solver c in
      solver_read solver)
    else return (Error (Fmt.str "Variable already declared"))

  let make_solver ~(name : string) (path : string) (options : string list) : solver * int t * int u
      =
    let pinfo = Lwt_process.open_process (path, Array.of_list options) in
    (* If the ocaml ends of the pipes aren't marked close-on-exec, they
       will remain open in the fork/exec'd solver process, and the solver won't exit
       when our main ocaml process ends. *)
    (* set_close_on_exec pinfo#stdout; *)
    (* set_close_on_exec pinfo.stderr; *)
    let log_file = Caml.Filename.temp_file (name ^ "_") ".smt2" in
    let solver =
      {
        s_name = name;
        s_pinfo = pinfo;
        s_pid = pinfo#pid;
        s_inputc = pinfo#stdin;
        s_outputc = pinfo#stdout;
        s_declared = Hashtbl.create (module String);
        s_scope_level = 0;
        s_log_file = log_file;
        s_log_outc = OC.create log_file;
      }
    in
    online_solvers := (pinfo#pid, solver) :: !online_solvers;
    (* The solver returned is bound to a task that can be cancelled. *)
    try
      let (m, task_r) : int t * int u = Lwt.task () in
      ( solver,
        Lwt.bind m (fun i ->
            let%lwt r = exec_command solver mk_print_success in
            match r with
            | Success -> return i
            | _ -> failwith "could not configure solver to :print-success"),
        task_r )
    with Sys_error s -> failwith ("couldn't talk to solver, double-check path. Sys_error " ^ s)

  let make_z3_solver () = make_solver ~name:"Z3" Config.z3_binary_path [ "z3"; "-in"; "-smt2" ]

  (** Create a process with a CVC4 solver. *)
  let make_cvc4_solver () =
    make_solver ~name:"CVC4" Config.cvc4_binary_path [ "cvc4"; "--lang=smt2.6"; "--incremental" ]

  let solver_make_cancellable (s : solver) (p : 'a t) : unit =
    (* IF task is cancelled, kill the solver.  *)
    Lwt.on_cancel p (fun () ->
        match s.s_pinfo#state with
        | Lwt_process.Exited _ -> ()
        | Running ->
            Log.debug_msg Fmt.(str "Terminating solver %s (PID : %i)" s.s_name s.s_pid);
            s.s_pinfo#terminate)

  let close_solver (solver : solver) : unit t =
    Log.debug
      Fmt.(
        fun fmt () ->
          pf fmt "Closing %s, log can be found in %a" solver.s_name
            (styled (`Fg `Blue) string)
            solver.s_log_file);
    let%lwt _ = exec_command solver mk_exit in
    let%lwt () = Lwt_io.close solver.s_outputc in
    return (OC.close solver.s_log_outc)

  let check_sat s : response = exec_command s mk_check_sat

  let declare_all (s : solver) (decls : command list) : unit t =
    List.fold
      ~f:(fun _ decl ->
        let%lwt _ = exec_command s decl in
        return ())
      decls ~init:(return ())

  let get_model (s : solver) : response = exec_command s GetModel

  let load_min_max_defs (s : solver) : unit t =
    (exec_command s mk_max_def >>= fun _ -> exec_command s mk_min_def) >>= fun _ -> return ()

  let set_logic solver logic_id : unit t =
    let%lwt _ = exec_command solver (SetLogic (SSimple logic_id)) in
    return ()

  let set_option (solver : solver) (option_id : string) (option_value : string) : unit t =
    let%lwt _ = exec_command solver (SetOption (option_id, option_value)) in
    return ()

  let smt_assert (s : solver) (term : smtTerm) : unit Lwt.t =
    let%lwt _ = exec_command s (mk_assert term) in
    return ()

  let spop (solver : solver) : unit Lwt.t =
    let%lwt _ = exec_command solver (mk_pop 1) in
    return ()

  let spush (solver : solver) : unit Lwt.t =
    let%lwt _ = exec_command solver (mk_push 1) in
    return ()

  let cancellable_task ((s, starter, resolver) : solver * int t * int u) task_func :
      response * int u =
    let task = task_func (s, starter) in
    solver_make_cancellable s task;
    (task, resolver)
end
