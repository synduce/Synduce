open Base
open Syguslib.Sygus
open Syguslib.Solvers
open Utils
open Fmt



let test_from_input_file file =
  let input_filename = Config.base ("tests/inputs/"^file) in
  let output_filename = Config.base "tests/outputs/"^file^"-res.txt" in
  let file_contents = Stdio.In_channel.create input_filename in
  let sexp = Sexplib.Sexp.input_sexps file_contents in
  let _ = program_of_sexp_list sexp in
  let responses =
    CVC4.exec_solver [input_filename, output_filename]
  in
  List.iter responses
    ~f:(function
        | Some (RSuccess synthesized_funcs) ->
          let commands = List.map ~f:(fun (a,b,c,d) -> CDefineFun (a,b,c,d)) synthesized_funcs in
          Log.(info (fun fmt () -> pf fmt "> %s result:@.%a" file (list Command.pp_hum) commands))
        | Some RFail -> Log.(info (wrap "Synthesis failure."))
        | Some RInfeasible -> Log.(info (wrap "Unfeasible."))
        | Some RUnknown -> Log.(info (wrap "Unknown."))
        | _ -> ())


;;
test_from_input_file "ex1.sl";
test_from_input_file "ex2.sl";
test_from_input_file "ex3.sl";

test_from_input_file "ex4.sl"