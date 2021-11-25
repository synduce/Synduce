open Base
open Fmt
open Lib.Lang
open Lib.Parsers
open Lib.Utils

let parse_only = ref false

let main () =
  let filename = ref None in
  let options = Config.options ToolMessages.print_usage parse_only in
  Getopt.parse_cmdline options (fun s -> filename := Some s);
  let filename =
    match !filename with
    | Some f -> ref f
    | None -> ToolMessages.print_usage ()
  in
  Config.problem_name := Caml.Filename.basename (Caml.Filename.chop_extension !filename);
  set_style_renderer stdout `Ansi_tty;
  Caml.Format.set_margin 100;
  (match !SygusInterface.SygusSolver.default_solver with
  | CVC -> ToolMessages.cvc_message ()
  | EUSolver -> failwith "EUSolver unsupported."
  | DryadSynth -> Syguslib.Sygus.use_v1 := true);
  Lib.Utils.Stats.glob_start ();
  (* Parse input file. *)
  let is_ocaml_syntax = Caml.Filename.check_suffix !filename ".ml" in
  let prog, psi_comps =
    if is_ocaml_syntax then parse_ocaml !filename else parse_pmrs !filename
  in
  (* Populate types.  *)
  let _ = seek_types prog in
  (* Translate the Caml or PRMS file into pmrs representation. *)
  let all_pmrs =
    try translate prog with
    | e ->
      if !Config.show_vars then Term.Variable.print_summary stdout ();
      raise e
  in
  if !parse_only then Caml.exit 1;
  (* Solve the problem proper. *)
  (match Algo.Refinement.solve_problem psi_comps all_pmrs with
  | pb, Realizable soln ->
    ToolMessages.on_success ~is_ocaml_syntax filename pb (Either.First soln)
  | pb, Unrealizable ctexs ->
    ToolMessages.on_success ~is_ocaml_syntax filename pb (Either.Second ctexs)
  | _, Failed _ ->
    Utils.Log.error_msg "Failed to find a solution or a witness of unrealizability");
  if !Config.show_vars then Term.Variable.print_summary stdout ()
;;

main ()
