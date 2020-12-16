open Core
open Syguslib.Sygus
open Syguslib.Solvers
open Utils
open Fmt



let test_from_input_file ?(print=true) file =
  let input_filename = Config.base ("tests/inputs/"^file) in
  let output_filename = Config.base "tests/outputs/"^file^"-res.txt" in
  let file_contents = Stdio.In_channel.create input_filename in
  let sexp = Sexplib.Sexp.input_sexps file_contents in
  let _ = program_of_sexp_list sexp in
  let tstart = Unix.time () in
  let response =
    CVC4.exec_solver (input_filename, output_filename)
  in
  let elapsed = (Unix.time ()) -. tstart in
  Unix.remove output_filename;
  Log.info Fmt.(fun fmt () -> pf fmt "> %s result in %1.3fs:" file elapsed);
  match response with
  | Some (RSuccess synthesized_funcs) ->
    let commands = List.map ~f:(fun (a,b,c,d) -> CDefineFun (a,b,c,d)) synthesized_funcs in
    if print then
      Log.(info (fun fmt () -> pf fmt "%a" (list SyCommand.pp_hum) commands))
    else ()
  | Some RFail -> Log.(info (wrap "Synthesis failure."))
  | Some RInfeasible -> Log.(info (wrap "Unfeasible."))
  | Some RUnknown -> Log.(info (wrap "Unknown."))
  | _ -> ()

;;
test_from_input_file "ex1.sl";
test_from_input_file "ex2.sl";
test_from_input_file "ex3.sl";
test_from_input_file "ex4.sl";
test_from_input_file "ex5.sl";
test_from_input_file ~print:false "fg_max14.sl";
test_from_input_file "small_linear_func.sl";
test_from_input_file "ex_from.sl"