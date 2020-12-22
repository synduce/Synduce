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
let log_out  = ref None

let open_log () =
  log_out := Some (OC.create log_file)


let log c =
  match !log_out with
  | Some oc -> write_command oc c
  | None -> open_log ();
    (match !log_out with
     | Some oc -> write_command oc c
     | None -> Log.(error (wrap "Failed to open log file.")))



(* Solver response and other types. *)
type solver_response = SmtLib.solver_response

let is_sat s = match s with Sat -> true | _ -> false
let is_unsat s = match s with Unsat -> true | _ -> false

type online_solver = { pid : int; inputc : OC.t; outputc : IC.t; declared : String.t Hash_set.t }


let solver_write (solver : online_solver) (c : command) : unit =
  write_command solver.inputc c;
  OC.output_char solver.inputc '\n';
  OC.flush solver.inputc


let solver_read  (solver : online_solver) : solver_response =
  let l =
    try Sexp.input_sexp solver.outputc
    with Sys_error _ ->
      failwith Fmt.(str "%s:%s:%s" Caml.__FILE__ "solver_read" "Couldn't read solver answer.")
  in
  parse_response [l]


let exec_command (solver : online_solver) (c : command) =
  if !log_queries then log c;
  (match c with
   | DefineSmtSort (s, _, _)
   | DeclareDatatype (s, _)
   | DeclareConst (s, _)
   | DefineFunRec ((s, _, _,_))
   | DefineFun ((s, _, _,_))
   | DeclareFun (s, _, _) -> Hash_set.add solver.declared (str_of_symb s)
   | _ -> ());
  solver_write solver c;
  solver_read solver

let silent_command (solver : online_solver) (c : command) =
  if !log_queries then log c;
  solver_write solver c;
  ignore(solver_read solver)

(* keep track of all solvers we spawn, so we can close our read/write
   FDs when the solvers exit *)
let online_solvers : (int * online_solver) list ref = ref []


let handle_sigchild (_ : int) : unit =
  if List.length !online_solvers = 0
  then ignore @@ Unix.waitpid [] (-1)
  else
    let (pid, _) = Unix.waitpid [] (-1) in
    if !solve_verbose then Fmt.(pf stdout "[WARNING] Solver (pid %d) exited!@." pid);
    match List.Assoc.find !online_solvers ~equal:(=) pid with
    | Some solver -> IC.close solver.outputc; OC.close solver.inputc
    | None -> ()


let () =
  Caml.Sys.set_signal Caml.Sys.sigchld (Caml.Sys.Signal_handle handle_sigchild)

let make_solver (path : string) : online_solver =
  let open Unix in
  let (z3_stdin, z3_stdin_writer) = pipe () in
  let (z3_stdout_reader, z3_stdout) = pipe () in
  (* If the ocaml ends of the pipes aren't marked close-on-exec, they
     will remain open in the fork/exec'd z3 process, and z3 won't exit
     when our main ocaml process ends. *)
  set_close_on_exec z3_stdin_writer;
  set_close_on_exec z3_stdout_reader;
  let pid = create_process path [| path; "-in"; "-smt2" |]
      z3_stdin z3_stdout stderr in
  let in_chan = in_channel_of_descr z3_stdout_reader in
  let out_chan = out_channel_of_descr z3_stdin_writer in
  OC.set_binary_mode out_chan false;
  IC.set_binary_mode in_chan false;
  let solver =
    { pid = pid; inputc = out_chan; outputc = in_chan; declared = Hash_set.create (module String) }
  in
  online_solvers := (pid, solver) :: !online_solvers;
  try
    match exec_command solver mk_print_success with
    | SExps [Sexp.Atom "success"] -> solver
    | _ -> failwith "could not configure solver to :print-success"
  with
    Sys_error s ->
    failwith ("couldn't talk to solver, double-check path. Sys_error "^s)

let close_solver solver = ignore @@ exec_command solver mk_exit


(** Returns empty response if commands is empty, otherwise, executes the LAST command in the
    command list.
*)
let call_solver solver commands =
  match List.last commands with
  | Some c -> exec_command solver c
  | None -> Log.(error_msg "Called solver without any command."); SExps []

(** Create a process with a Z3 sovler. *)
(* TODO: change the path of the solver. *)
let make_z3_solver () = make_solver Utils.Config.z3_binary_path

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
    | List (Atom "error" :: _) -> [sexp]
    | List ls -> Caml.List.flatten (List.map  ~f:is_error_sexp ls)
  in
  match response with
  | SExps l ->
    Caml.List.flatten (List.map ~f:is_error_sexp l)
  | _ -> []


let pp_solver_response = SmtLib.pp_solver_response


let declare_all s decls =
  List.iter ~f:(fun decl -> ignore(exec_command s decl)) decls
(*   (remove_duplicate_decls s.declared decls) *)

let load_min_max_defs s =
  silent_command s mk_max_def;
  silent_command s mk_min_def

let smt_assert s term =
  ignore(exec_command s (Assert term))

let check_sat s =
  exec_command s CheckSat

let get_model s =
  exec_command s GetModel

let spush solver =
  silent_command solver (mk_push 1)

let spop solver =
  silent_command solver (mk_pop 1)
