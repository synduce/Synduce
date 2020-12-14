open Base
open Sygus
open Sexplib
open Lwt_process
open Utils

module OC = Stdio.Out_channel
module IC = Stdio.In_channel

(* Logging utilities. *)
let log_queries = ref true

let solver_verbose = ref false


let err_msg s = Fmt.(pf stderr "[ERROR] %s" s)

let tmp_folder = ref "/tmp"

let log_file () = !tmp_folder^"log.sl"


let log_out = ref None


let open_log () =
  log_out := Some (OC.create (log_file ()))


let log c =
  match !log_out with
  | Some oc -> write_command oc c
  | None -> err_msg "Failed to open log file."


type solver_instance = {
  pid : int;
  inputc : OC.t;
  outputc : IC.t;
  decls : String.t Hash_set.t;
}



let online_solvers : (int * solver_instance) list ref = ref []


module CVC4 = struct

  let print_options (frmt : Formatter.t) =
    Fmt.(list ~sep:sp (fun fmt opt -> pf fmt "--%s" opt) frmt)


  let _solver_call
      ?(options = [])
      (inputfile, outputfile : string * string) : string * process_out =
    let racket_command =
      shell Fmt.(str "%s %a %s" Config.cvc4_binary_path print_options options inputfile)
    in
    let out_fd =
      Unix.openfile outputfile [Unix.O_RDWR; Unix.O_TRUNC; Unix.O_CREAT] 0o644
    in
    outputfile, open_process_out ~stdout:(`FD_move out_fd) racket_command


  let fetch_solution filename =
    Log.debug_msg Fmt.(str "Fetching solution in %s" filename);
    reponse_of_sexps (Sexp.input_sexps (Stdio.In_channel.create filename))


  let exec_solver (filenames : (string * string) list) =
    let processes = List.map ~f:_solver_call filenames in
    let proc_status this _ otf =
      let sol =
        try
          fetch_solution otf
        with Sys_error s ->
          Log.error_msg Fmt.(str "Sys_error (%a)" string s);
          RFail
      in
      if is_infeasible sol || is_failed sol then None
      else
        (List.iter
           ~f:(fun (_, proc) ->
               if not (equal this#pid proc#pid) then
                 (proc#terminate;
                  Log.debug_msg Fmt.(str "Killed %a early" int (proc#pid))))
           processes;
         Some sol)
    in
    let cp =
      List.map processes
        ~f:(fun (otf, proc) -> Lwt.map (fun x -> proc_status proc x otf) proc#status)
    in
    Lwt_main.run (Lwt.all cp)

end

