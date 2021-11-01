open Base
open Fmt
open Utils

let prep_final_json
    ~(is_ocaml_syntax : bool)
    (source_filename : string ref)
    (pb : Algo.AState.psi_def)
    (soln : Algo.AState.soln)
    : Yojson.t
  =
  let _ = is_ocaml_syntax, source_filename, pb, soln in
  let solvers = Utils.LogJson.solvers_summary () in
  let refinement_steps = Utils.LogJson.refinement_steps_summary () in
  `Assoc [ "solver-usage", solvers; "refinement-steps", refinement_steps ]
;;

let on_success
    ~(is_ocaml_syntax : bool)
    (source_filename : string ref)
    (pb : Algo.AState.psi_def)
    (soln : Algo.AState.soln)
    : unit
  =
  let elapsed = Stats.get_glob_elapsed () in
  let verif_ratio = 100.0 *. (!Stats.verif_time /. elapsed) in
  Log.(verbose print_solvers_summary);
  Log.info
    Fmt.(
      fun frmt () ->
        pf
          frmt
          "Solution found in %4.4fs (%3.1f%% verifying):@.%a@]"
          elapsed
          verif_ratio
          (box (Algo.AState.pp_soln ~use_ocaml_syntax:is_ocaml_syntax))
          soln);
  (* If output specified, write the solution in file. *)
  (match Config.get_output_file !source_filename with
  | Some out_file ->
    Utils.Log.to_file out_file (fun frmt () ->
        (box (Algo.AState.pp_soln ~use_ocaml_syntax:is_ocaml_syntax)) frmt soln)
  | None -> ());
  (* If specified, output a Dafny proof skeleton. *)
  if !Config.generate_proof
  then
    Codegen.(
      Generation.gen_proof
        (Commons.problem_descr_of_psi_def pb, Some soln)
        !Config.proof_generation_file)
  else ();
  (* If no info required, output timing information. *)
  if (not !Config.info) && !Config.timings
  then (
    Fmt.(
      pf stdout "%i,%.4f,%.4f@." !Algo.AState.refinement_steps !Stats.verif_time elapsed);
    Fmt.(pf stdout "success@."));
  if !Config.json_out
  then (
    let json = prep_final_json ~is_ocaml_syntax source_filename pb soln in
    Fmt.(pf stdout "%a@." (Yojson.pretty_print ~std:true) json))
;;
