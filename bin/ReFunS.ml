open Lang
open Base
open Getopt
open Fmt
open Parsers
module Config = Lib.Utils.Config

let parse_only = ref false

let options = [
  ('v', "verbose", (set Config.verbose true), None);
  ('d', "debug", (set Config.debug true), None);
  ('t',"detupling-off", (set Config.detupling_on false), None);
  ('i', "info-off", (set Config.info false), None);
  ('\000', "use-naive", (set Config.use_naive true), None);
  ('\000', "parse-only", (set parse_only true), None);
  ('\000',"replacing-recursion-off", (set Config.replace_recursion false), None);
  ('\000', "show-vars", (set Config.show_vars true), None);
  ('\000',"stratifying-off", (set Config.stratify_on false), None);
  ('s',"split-solving-off", (set Config.split_solve_on false), None);
  ('n',"verification", None, Some (Config.set_num_expansions_check));
  ('\000',"use-dryadsynth", (set Syguslib.Solvers.SygusSolver.default_solver DryadSynth), None)
]


let main () =
  let filename = ref "" in
  parse_cmdline options (fun s -> filename := s);
  set_style_renderer stdout `Ansi_tty;
  Caml.Format.set_margin 100;
  (match !Syguslib.Solvers.SygusSolver.default_solver with
   | CVC4 -> ()
   | EUSolver -> failwith "EUSolver unsupported."
   | DryadSynth -> Syguslib.Sygus.use_v1 := true);
  let start_time = Unix.gettimeofday () in
  let prog = parse_pmrs !filename in
  let _ = seek_types prog in
  let all_pmrs = translate prog in
  if !parse_only then Caml.exit 1;
  try
    (match Algo.PmrsAlgos.solve_problem all_pmrs with
     | Ok target ->
       let elapsed = Unix.gettimeofday () -. start_time in
       Utils.Log.info Fmt.(fun frmt () -> pf frmt "Solution found in %4.4fs:@;%a@]" elapsed (box PMRS.pp) target);
       (* If no info required, output timing information. *)
       if not !Config.info then
         Fmt.(pf stdout "%i,%.4fs@." !Algo.PmrsAlgos.loop_counter elapsed)

     | Error _ -> Utils.Log.error_msg "No solution found.")
  with s -> (if !Config.show_vars then Term.Variable.print_summary stdout (); raise s)

;;
main ()