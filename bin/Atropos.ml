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
       --replacing-recursion-off   Do not use reference function in nested calls.
    -s --split-solving-off         Do not split systems into subsystems.
       --stratifying-off           Do not use syntactic definitions.
    -t --detupling-off             Turn off detupling.
       --use-naive                 Use the naive algorithm. Turns bmc on.
  Bounded checking:
       --use-bmc                   Use naive bounded model checking (bmc mode).
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
  ('t',"detupling-off", (set Config.detupling_on false), None);
  ('i', "info-off", (set Config.info false), None);
  ('\000', "use-naive", (set Config.use_naive true), None);
  ('\000', "use-bmc", (set Config.use_bmc true), None);
  ('\000', "parse-only", (set parse_only true), None);
  ('\000',"replacing-recursion-off", (set Config.replace_recursion false), None);
  ('\000', "show-vars", (set Config.show_vars true), None);
  ('\000',"stratifying-off", (set Config.stratify_on false), None);
  ('s',"split-solving-off", (set Config.split_solve_on false), None);
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
  let prog = parse_pmrs !filename in
  let _ = seek_types prog in
  let all_pmrs = translate prog in
  if !parse_only then Caml.exit 1;
  (match Algo.PmrsAlgos.solve_problem all_pmrs with
   | Ok target ->
     let elapsed = Unix.gettimeofday () -. start_time in
     Utils.Log.info Fmt.(fun frmt () -> pf frmt "Solution found in %4.4fs:@;%a@]" elapsed (box PMRS.pp) target);
     (* If no info required, output timing information. *)
     if not !Config.info then
       Fmt.(pf stdout "%i,%.4f@." !Algo.PmrsAlgos.refinement_steps elapsed)

   | Error _ -> Utils.Log.error_msg "No solution found.");
  if !Config.show_vars then Term.Variable.print_summary stdout ()

;;
main ()