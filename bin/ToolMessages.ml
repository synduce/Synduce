(**  This module packages a few messages printed by the tool. *)

open Base
open Fmt
open Utils
open Common.ProblemDefs
open Common.Env

(* ============================================================================================= *)
(*                              TOOL MESSAGESAND JSON OUTPUT                                     *)
(* ============================================================================================= *)

let cvc_message () =
  Utils.Log.debug_msg
    (if Config.using_cvc5 () then "Using CVC5 ✔" else "Using CVC4. Please install CVC5.")
;;

let prep_final_json
    ~(is_ocaml_syntax : bool)
    ~(ctx : env)
    (source_filename : string ref)
    (pb : PsiDef.t)
    (soln : (soln, unrealizability_ctex list) Either.t)
    (elapsed : float)
    (verif : float)
    : Yojson.t
  =
  let _ = is_ocaml_syntax, source_filename, pb, soln in
  let solvers =
    Option.value ~default:(`String "unknown") (Utils.LogJson.get_solver_stats pb.id)
  in
  let refinement_steps =
    Option.value ~default:(`String "unknown") (Utils.LogJson.get_summary pb.id)
  in
  let algo =
    if !Config.Optims.use_segis
    then "SEGIS"
    else if !Config.Optims.use_cegis
    then "CEGIS"
    else "SE2GIS"
  in
  let soln_or_refutation =
    match soln with
    | Either.First soln ->
      [ ( "solution"
        , `String
            (Fmt.str
               "%a"
               (box (ctx >- Common.ProblemDefs.pp_soln ~use_ocaml_syntax:is_ocaml_syntax))
               soln) )
      ; "unrealizable", `Bool false
      ]
    | Either.Second _ -> [ "unrealizable", `Bool true ]
  in
  `Assoc
    ([ "algorithm", `String algo
     ; "total_elapsed", `Float elapsed
     ; "verif_elapsed", `Float verif
     ; "solver-usage", solvers
     ; "refinement-steps", refinement_steps
     ; "id", `Int pb.id
     ]
    @ soln_or_refutation)
;;

let on_success
    ~(is_ocaml_syntax : bool)
    ~(ctx : env)
    (source_filename : string ref)
    (pb : PsiDef.t)
    (result : (soln, unrealizability_ctex list) Either.t)
    : Yojson.t
  =
  let elapsed, verif_time =
    Option.value
      ~default:(Stats.get_glob_elapsed (), !Stats.verif_time)
      (LogJson.get_simple_stats pb.id)
  in
  let verif_ratio = 100.0 *. (verif_time /. elapsed) in
  Log.sep ();
  Log.(info (print_solvers_summary pb.id));
  (* Print the solution. *)
  (match result with
  | Either.First soln ->
    Log.info (fun frmt () ->
        pf
          frmt
          "Solution found in %4.4fs (%3.1f%% verifying):@.%a@]"
          elapsed
          verif_ratio
          (box (ctx >- Common.ProblemDefs.pp_soln ~use_ocaml_syntax:is_ocaml_syntax))
          soln)
  | Either.Second ctexs ->
    Log.(
      info (fun frmt () ->
          pf frmt "No solution: problem is unrealizable (found answer in %4.4fs)." elapsed));
    Log.(info (fun frmt () -> pf frmt "%a" (ctx >- Lang.PMRS.pp ~short:false) pb.target));
    ctx >>> ToolExplain.when_unrealizable pb ctexs);
  (* If output specified, write the solution in file. *)
  (match result with
  | Either.First soln ->
    (match Config.get_output_file !source_filename with
    | Some out_file ->
      Utils.Log.to_file out_file (fun frmt () ->
          (box (ctx >- Common.ProblemDefs.pp_soln ~use_ocaml_syntax:is_ocaml_syntax))
            frmt
            soln)
    | None -> ());
    (* If specified, output a Dafny proof skeleton. *)
    if !Config.generate_proof
    then
      Codegen.(
        Generation.gen_proof
          ~ctx
          (ctx >- Commons.problem_descr_of_psi_def pb, Some soln)
          !Config.proof_generation_file)
    else ()
  | _ -> ());
  (* If no info required, output timing information. *)
  if (not !Config.info) && !Config.timings
  then (
    Fmt.(pf stdout "%i,%.4f,%.4f@." (get_refinement_steps ctx) verif_time elapsed);
    Fmt.(pf stdout "success@."));
  ctx
  >>> prep_final_json ~is_ocaml_syntax source_filename pb result elapsed !Stats.verif_time
;;

(** Print a summary of the options available. The options are in the Lib.Utils.Config module.  *)
let print_usage () =
  pf stdout "Usage : Synduce [options] input_file@.";
  pf
    stdout
    "Options:\n\
    \  Output and input options:\n\
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
    \  Features off/on:\n\
    \    -s --max-solutions             Set maximum number of solutions\n\
    \                                   must be > 0 (no option means solve sketch \
     directly)\n\
    \       --no-splitting              Do not split systems into subsystems.\n\
    \       --no-syndef                 Do not use syntactic definitions.\n\
    \       --no-rew                    Do not use rewrite solver.\n\
    \    -t --no-detupling              Turn off detupling.\n\
    \    -c --simple-init               Initialize T naively.\n\
    \    -l --lemma-sketch              Sketch lemmas in synthesis.\n\
    \       --segis                    Use the Abstract CEGIS algorithm. Turns bmc on.\n\
    \       --cegis                    Use the Concrete CEGIS algorithm. Turns bmc on.\n\
    \       --no-assumptions            Don't  partial correctness assumptions.\n\
    \       --no-simplify               Don't simplify equations with partial evaluation.\n\
    \       --no-gropt                  Don't optimize grammars (level 0)\n\
    \       --gropt-level=NUM           Set grammar optimization level (NUM=0,1 or 2)\n\
    \       --no-lifting                Don't attempt lifting.\n\
    \       --max-lifting=NUM           Set how many times Synduce attempts to lift.\n\
    \    -u --no-check-unrealizable     Do not check if synthesis problems are \
     functionally realizable.\n\
    \       --sysfe-opt-off             Turn off optimizations to solve systems of \
     equations in parallel\n\
    \  Bounded checking:\n\
    \       --use-bmc                   Use segis bounded model checking (bmc mode).\n\
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
    \       --interactive-check-lemma   Manually set if a lemma is true and, if not, \
     give counterexample.\n\
    \       --parse-only                Just parse the input.\n\
    \       --show-vars                 Print variables and their types at the end.\n\
    \       --generate-benchmarks=DIR   Save SyGuS problems in DIR, including problems \
     that are provably unrealizable.\n\
    \       --generate-proof=FILE       Save a Dafny proof skeleton in file \
     (experimental).\n\
    \  -C   --check-smt-unrealizable    Check unrealizability using a SMT query directly.\n\
    \     -> Try:\n\
     ./Synduce benchmarks/list/mps.ml@.";
  Caml.exit 0
;;
