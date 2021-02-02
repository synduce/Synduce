open Lang
open Base
open Getopt
open Fmt
open Parsers
module Config = Lib.Utils.Config

let parse_only = ref false


let print_usage () =
  pf stdout ("Usage : atropos [options] input_file@.");
  pf stdout
  "Options:
    -h --help                      Print this message.
    -d --debug                     Print debugging information.
    -v --verbose                   Print verbose.
    -i --info-off                  Turn off information messages. Print time and refinement steps.
  Turning optimizations off:
    -s --no-splitting              Do not split systems into subsystems.
       --no-syndef                 Do not use syntactic definitions.
    -t --no-detupling              Turn off detupling.
    -c --simple-init               Initialize T naively.
       --acegis                    Use the Abstract CEGIS algorithm. Turns bmc on.
       --ccegis                    Use the Concrete CEGIS algorithm. Turns bmc on.
  Bounded checking:
       --use-bmc                   Use acegis bounded model checking (bmc mode).
    -b --bmc                       Maximum depth of terms for bounded model checking, in bmc mode.
    -v --verification              Number of expand calls for bounded model checking, in opt mode.
  Debugging:
       --parse-only                Just parse the input.
       --show-vars                 Print variables and their types at the end.
-> Try:
./atropos benchmarks/list/mpshom.pmrs@.";
  Caml.exit 0

let options = [
  ('h', "help", (Some print_usage), None);
  ('v', "verbose", (set Config.verbose true), None);
  ('d', "debug", (set Config.debug true), None);
  ('i', "info-off", (set Config.info false), None);
  ('\000', "show-vars", (set Config.show_vars true), None);
  ('\000', "acegis", (set Config.use_acegis true), None);
  ('\000', "ccegis", (set Config.use_ccegis true), None);
  ('\000', "use-bmc", (set Config.use_bmc true), None);
  ('\000', "parse-only", (set parse_only true), None);
  ('c', "simple-init", (set Config.simple_init true), None);
  ('t',"no-detupling", (set Config.detupling_on false), None);
  ('\000',"no-syndef", (set Config.syndef_on false), None);
  ('s',"no-splitting", (set Config.split_solve_on false), None);
  ('n',"verification", None, Some (Config.set_num_expansions_check));
  ('b',"bmc", None, Some (Config.set_check_depth));
  ('\000',"use-dryadsynth", (set Syguslib.Solvers.SygusSolver.default_solver DryadSynth), None)
]



let main () =
  let filename = ref None in
  parse_cmdline options (fun s -> filename := Some s);
  let filename = match !filename with Some f -> ref f | None -> print_usage () in
  set_style_renderer stdout `Ansi_tty;
  Caml.Format.set_margin 100;
  (match !Syguslib.Solvers.SygusSolver.default_solver with
   | CVC4 -> ()
   | EUSolver -> failwith "EUSolver unsupported."
   | DryadSynth -> Syguslib.Sygus.use_v1 := true);
  let start_time = Unix.gettimeofday () in
  Config.glob_start := start_time;
  (* Parse input file. *)
  let is_ocaml_syntax = Caml.Filename.check_suffix !filename ".ml" in
  let prog, psi_comps =
    if is_ocaml_syntax then
      parse_ocaml !filename
    else
      parse_pmrs !filename
  in
  let _ = seek_types prog in
  let all_pmrs = translate prog in
  if !parse_only then Caml.exit 1;
  (match Algo.PmrsAlgos.solve_problem psi_comps all_pmrs with
   | Ok target ->
     let elapsed = Unix.gettimeofday () -. start_time in
     let verif_ratio = 100.0 *. (!Config.verif_time /. elapsed) in
     Utils.Log.info Fmt.(fun frmt () -> pf frmt "Solution found in %4.4fs (%3.1f%% verifying):@.%a@]"
     elapsed verif_ratio (box (if is_ocaml_syntax then PMRS.pp_ocaml else PMRS.pp)) target);
     (* If no info required, output timing information. *)
     if not !Config.info then
       Fmt.(pf stdout "%i,%.4f,%.4f@." !Algo.PmrsAlgos.refinement_steps !Config.verif_time elapsed)

   | Error _ -> Utils.Log.error_msg "No solution found.");
  if !Config.show_vars then Term.Variable.print_summary stdout ()

;;
main ()