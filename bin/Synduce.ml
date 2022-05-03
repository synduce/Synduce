open Base
open Fmt
open Lib
open Lib.Lang
open Lib.Parsers
open Lib.Utils
open Common.Env

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
  (* Get problem name from file name, or exit if we don't recognize. *)
  (try
     match Caml.Filename.extension !filename with
     | ".ml" | ".pmrs" ->
       Config.problem_name
         := Caml.Filename.basename (Caml.Filename.chop_extension !filename)
     | _ -> raise (Invalid_argument "wrong extension")
   with
  | Invalid_argument _ ->
    Log.error_msg "Filename must end with extension .ml or .pmrs";
    Caml.exit (-1));
  set_style_renderer stdout `Ansi_tty;
  Caml.Format.set_margin 100;
  (match !SygusInterface.SygusSolver.CoreSolver.default_solver with
  | CVC -> ToolMessages.cvc_message ()
  | EUSolver -> failwith "EUSolver unsupported."
  | DryadSynth -> Syguslib.Sygus.use_v1 := true);
  Lib.Utils.Stats.glob_start ();
  (* Parse input file. *)
  let is_ocaml_syntax = Caml.Filename.check_suffix !filename ".ml" in
  ToolMessages.start_message !filename is_ocaml_syntax;
  let prog, psi_comps =
    if is_ocaml_syntax then parse_ocaml !filename else parse_pmrs !filename
  in
  (* Main context *)
  let ctx = group (Term.Context.create ()) (PMRS.Functions.create ()) in
  (* Populate types.  *)
  let _ = ctx >- seek_types prog in
  (* Translate the Caml or PRMS file into pmrs representation. *)
  let all_pmrs =
    try ctx >>- translate prog with
    | e ->
      if !Config.show_vars then Term.Variable.print_summary stdout ctx.ctx;
      raise e
  in
  if !parse_only then Caml.exit 1;
  (* Solve the problem proper. *)
  let num_configurations, outputs =
    ctx >>> Many.find_and_solve_problem psi_comps all_pmrs
  in
  let n_out = List.length outputs in
  let print_unrealizable = !Config.print_unrealizable_configs || n_out < 2 in
  let check_output u_count (ctx, pb, soln) =
    Common.ProblemDefs.(
      match pb, soln with
      | pb, Realizable soln ->
        ( pb.PsiDef.id
        , ctx >>> ToolMessages.on_success ~is_ocaml_syntax filename pb (Either.First soln)
        )
      | pb, Unrealizable ctexs ->
        Int.incr u_count;
        ( pb.PsiDef.id
        , ctx
          >>> ToolMessages.on_success
                ~print_unrealizable
                ~is_ocaml_syntax
                filename
                pb
                (Either.Second ctexs) )
      | _, Failed _ ->
        Log.error_msg "Failed to find a solution or a witness of unrealizability";
        failwith "Solving failure")
  in
  let json_out =
    let u_count = ref 0 in
    let json =
      match outputs with
      | [ a ] -> snd (check_output u_count a)
      | _ ->
        let subproblem_jsons = List.map ~f:(check_output u_count) outputs in
        let results =
          List.map subproblem_jsons ~f:(fun (psi_id, json) ->
              Fmt.(str "problem_%i" psi_id), json)
        in
        `Assoc (("total_configurations", `Int num_configurations) :: results)
    in
    if n_out > 1
    then
      Log.info
        Fmt.(
          fun fmt () ->
            pf
              fmt
              "%i configurations solved, %i solutions (%i unrealizable)."
              n_out
              (n_out - !u_count)
              !u_count);
    json
  in
  (if !Config.json_out
  then
    if !Config.json_progressive || !Config.compact
    then (
      Yojson.to_channel ~std:true Stdio.stdout json_out;
      Stdio.(Out_channel.flush stdout))
    else Fmt.(pf stdout "%a@." (Yojson.pretty_print ~std:false) json_out));
  if !Config.show_vars then Term.Variable.print_summary stdout ctx.ctx
;;

main ()
