open Base
open Fmt
open Lib
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
  (match !SygusInterface.SygusSolver.CoreSolver.default_solver with
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
  let outputs = Many.find_and_solve_problem psi_comps all_pmrs in
  let f (pb, soln) =
    Algo.AState.(
      match pb, soln with
      | pb, Realizable soln ->
        ( pb.PsiDef.id
        , ToolMessages.on_success ~is_ocaml_syntax filename pb (Either.First soln) )
      | pb, Unrealizable ctexs ->
        ( pb.PsiDef.id
        , ToolMessages.on_success ~is_ocaml_syntax filename pb (Either.Second ctexs) )
      | _, Failed _ ->
        Utils.Log.error_msg "Failed to find a solution or a witness of unrealizability";
        failwith "Solving failure")
  in
  let json_out =
    match outputs with
    | [ a ] -> snd (f a)
    | _ ->
      let subproblem_jsons = List.map ~f outputs in
      `Assoc
        (List.map subproblem_jsons ~f:(fun (psi_id, json) ->
             Fmt.(str "problem_%i" psi_id), json))
  in
  (if !Config.json_out
  then
    if !Config.json_progressive
    then (
      Yojson.to_channel ~std:true Stdio.stdout json_out;
      Stdio.(Out_channel.flush stdout))
    else Fmt.(pf stdout "%a@." (Yojson.pretty_print ~std:false) json_out));
  if !Config.show_vars then Term.Variable.print_summary stdout ()
;;

main ()
