open Base
open Sexplib
open SmtLib
open Utils
module OC = Stdio.Out_channel
module IC = Stdio.In_channel

(* Logging utilities. *)
let log_queries = ref true

let solve_verbose = ref false

let log_file = "/tmp/solver.smt2"

let log_out = ref None

let open_log () = log_out := Some (OC.create log_file)

let log c =
  match !log_out with
  | Some oc -> write_command oc c
  | None -> (
      open_log ();
      match !log_out with
      | Some oc -> write_command oc c
      | None -> Log.(error (wrap "Failed to open log file.")))

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
}

let solver_write (solver : online_solver) (c : command) : unit =
  write_command solver.s_inputc c;
  OC.output_char solver.s_inputc '\n';
  OC.flush solver.s_inputc

let solver_read (solver : online_solver) : solver_response =
  let l =
    try Sexp.input_sexp solver.s_outputc
    with Sys_error _ ->
      failwith Fmt.(str "%s:%s:%s" Caml.__FILE__ "solver_read" "Couldn't read solver answer.")
  in
  parse_response [ l ]

let exec_command (solver : online_solver) (c : command) =
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
    | DeclareFun (s, _, _) -> (
        match Hashtbl.find solver.s_declared (str_of_symb s) with
        | Some _ ->
            (* Do not write command if variable is already declared. *)
            false
        | None ->
            Hashtbl.set solver.s_declared ~key:(str_of_symb s) ~data:solver.s_scope_level;
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
    if !log_queries then log c;
    solver_write solver c;
    solver_read solver)
  else Error (Fmt.str "Variable already declared")

(* keep track of all solvers we spawn, so we can close our read/write
   FDs when the solvers exit *)
let online_solvers : (int * online_solver) list ref = ref []

let handle_sigchild (_ : int) : unit =
  if List.length !online_solvers = 0 then ignore @@ Unix.waitpid [] (-1)
  else
    let pid, _ = Unix.waitpid [] (-1) in
    (if !solve_verbose then Fmt.(pf stdout "[WARNING] Solver (pid %d) exited!@." pid));
    match List.Assoc.find !online_solvers ~equal:( = ) pid with
    | Some solver ->
        IC.close solver.s_outputc;
        OC.close solver.s_inputc
    | None -> ()

let () = Caml.Sys.set_signal Caml.Sys.sigchld (Caml.Sys.Signal_handle handle_sigchild)

let make_solver ~(name : string) (path : string) (options : string array) : online_solver =
  let open Unix in
  let solver_stdin, solver_stdin_writer = pipe () in
  let solver_stdout_reader, solver_stdout = pipe () in
  (* If the ocaml ends of the pipes aren't marked close-on-exec, they
     will remain open in the fork/exec'd z3 process, and z3 won't exit
     when our main ocaml process ends. *)
  set_close_on_exec solver_stdin_writer;
  set_close_on_exec solver_stdout_reader;
  let pid =
    create_process path (Array.append [| path |] options) solver_stdin solver_stdout stderr
  in
  let in_chan = in_channel_of_descr solver_stdout_reader in
  let out_chan = out_channel_of_descr solver_stdin_writer in
  OC.set_binary_mode out_chan false;
  IC.set_binary_mode in_chan false;
  let solver =
    {
      s_name = name;
      s_pid = pid;
      s_inputc = out_chan;
      s_outputc = in_chan;
      s_declared = Hashtbl.create (module String);
      s_scope_level = 0;
    }
  in
  online_solvers := (pid, solver) :: !online_solvers;
  try
    match exec_command solver mk_print_success with
    | Success -> solver
    | _ -> failwith "could not configure solver to :print-success"
  with Sys_error s -> failwith ("couldn't talk to solver, double-check path. Sys_error " ^ s)

let close_solver solver =
  OC.output_string solver.s_inputc (Sexp.to_string (sexp_of_command mk_exit));
  IC.close solver.s_outputc

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
let make_z3_solver () = make_solver ~name:"Z3" Utils.Config.z3_binary_path [| "-in"; "-smt2" |]

(** Create a process with a CVC4 solver. *)
let make_cvc4_solver () =
  make_solver ~name:"CVC4" Utils.Config.cvc4_binary_path [| "--lang=smt2.6"; "--incremental" |]

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

let smt_assert s term = ignore (exec_command s (mk_assert term))

let spop solver = ignore (exec_command solver (mk_pop 1))

let spush solver = ignore (exec_command solver (mk_push 1))
