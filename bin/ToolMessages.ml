(**  This module packages a few messages printed by the tool. *)

open Base
open Fmt
open Utils

let cvc_message () =
  Utils.Log.debug_msg
    (if Config.using_cvc5 () then "Using CVC5 ✔" else "Using CVC4. Please install CVC5.")
;;

let prep_final_json
    ~(is_ocaml_syntax : bool)
    (source_filename : string ref)
    (pb : Algo.AState.psi_def)
    (soln : Algo.AState.soln)
    (elapsed : float)
    : Yojson.t
  =
  let _ = is_ocaml_syntax, source_filename, pb, soln in
  let solvers = Utils.LogJson.solvers_summary () in
  let refinement_steps = Utils.LogJson.refinement_steps_summary () in
  `Assoc
    [ "total_elapsed", `Float elapsed
    ; "solver-usage", solvers
    ; "refinement-steps", refinement_steps
    ; ( "solution"
      , `String
          (Fmt.str
             "%a"
             (box (Algo.AState.pp_soln ~use_ocaml_syntax:is_ocaml_syntax))
             soln) )
    ]
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
    let json = prep_final_json ~is_ocaml_syntax source_filename pb soln elapsed in
    if !Config.json_progressive
    then (
      Yojson.to_channel ~std:true Stdio.stdout json;
      Stdio.(Out_channel.flush stdout))
    else Fmt.(pf stdout "%a@." (Yojson.pretty_print ~std:false) json))
;;

(** Print a summary of the options available. The options are in the Lib.Utils.Config module.  *)
let print_usage () =
  pf stdout "Usage : Synduce [options] input_file@.";
  pf
    stdout
    "Options:\n\
    \    -d --debug                     Print debugging info.\n\
    \    -h --help                      Print this message.\n\
    \    -i --info-off                  Print timing information only.\n\
    \    -j --json                      Output in JSON format on stdout.\n\
    \       --json-progress             JSON messages at each new refinement loop \
     iteration.\n\
    \    -m --style-math                Print math-style.\n\
    \    -o --output=PATH               Output solution in folder PATH.\n\
    \    -v --verbose                   Print verbose.\n\
    \       --fuzzing=NUM               Fuzz SMT solutions on some problems.(Default=0, \
     no fuzzing).\n\
    \  Otimizations off/on:\n\
    \    -s --no-splitting              Do not split systems into subsystems.\n\
    \       --no-syndef                 Do not use syntactic definitions.\n\
    \    -t --no-detupling              Turn off detupling.\n\
    \    -c --simple-init               Initialize T naively.\n\
    \       --acegis                    Use the Abstract CEGIS algorithm. Turns bmc on.\n\
    \       --ccegis                    Use the Concrete CEGIS algorithm. Turns bmc on.\n\
    \       --no-assumptions            Don't  partial correctness assumptions.\n\
    \       --no-simplify               Don't simplify equations with partial evaluation.\n\
    \       --no-gropt                  Don't optimize grammars (level 0)\n\
    \       --set-gropt=NUM             Set grammar optimization level (NUM=0,1 or 2)\n\
    \       --no-lifting                Don't attempt lifting.\n\
    \       --max-lifting=NUM           Set how many times Synduce attempts to lift.\n\
    \    -u --no-check-unrealizable     Do not check if synthesis problems are \
     functionally realizable.\n\
    \       --sysfe-opt-off             Turn off optimizations to solve systems of \
     equations in parallel\n\
    \  Bounded checking:\n\
    \       --use-bmc                   Use acegis bounded model checking (bmc mode).\n\
    \    -b --bmc=MAX_DEPTH             Maximum depth of terms for bounded model \
     checking, in bmc mode.\n\
    \    -n --verification=NUM          Number of expand calls for bounded model \
     checking, in opt mode.\n\
    \  Background solver parameters:\n\
    \       --ind-tlimit=TIMEOUT        Set the solver to timeout after TIMEOUT ms when \
     doing an induction proof.\n\
    \       --cvc4                      Use CVC4 instead of CVC5 if both are available.\n\
    \       --cvc5                      Always use CVC5 instead of CVC4 if both are \
     available.\n\
    \       --verif-with=SOLVER         Don't use z3 for verification, use SOLVER \
     (=z3,cvc4,cvc5,yices)\n\
    \  Debugging:\n\
    \  -I   --interactive               Request additional lemmas interactively.\n\
    \  -J   --interactive-lifting       Request expressions for lifting.\n\
    \  -L   --interactive-loop          Request lemmas interactively in a loop.\n\
    \  -X   --classify-ctex             Manually classify ctex as pos or neg.\n\
    \  -N   --no-sat-as-unsat           No sat found in bounded checking is accepted as \
     unsat.\n\
    \  -B   --bounded-lemma-check       Use depth-bounded check to verify lemma \
     candidates and generate positive examples for lemma synth.\n\
    \  -C   --interactive-check-lemma   Manually set if a lemma is true and, if not, \
     give counterexample.\n\
    \       --parse-only                Just parse the input.\n\
    \       --show-vars                 Print variables and their types at the end.\n\
    \       --generate-benchmarks=DIR   Save SyGuS problems in DIR, including problems \
     that are provably unrealizable.\n\
    \       --generate-proof=FILE       Save a Dafny proof skeleton in file \
     (experimental).\n\
    \       --check-smt-unrealizable    Check unrealizability using a SMT query directly.\n\
    \     -> Try:\n\
     ./Synduce benchmarks/list/mps.ml@.";
  Caml.exit 0
;;
