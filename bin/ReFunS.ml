open Lang
open Base
open Getopt
open Fmt
open Parsers
module Config = Lib.Utils.Config


let options = [
  ('v', "verbose", (set Config.verbose true), None);
  ('d', "debug", (set Config.debug true), None);
  ('i', "info-off", (set Config.info false), None);
  ('s', "show-type", (set Config.show_vars true), None);
]


let main () =
  let filename = ref "" in
  parse_cmdline options (fun s -> filename := s);
  set_style_renderer stdout `Ansi_tty;
  Caml.Format.set_margin 100;
  let start_time = Unix.gettimeofday () in
  let prog = parse_pmrs !filename in
  let _ = seek_types prog in
  let all_pmrs = translate prog in
  try
    (match Algo.PmrsAlgos.solve_problem all_pmrs with
     | Ok target ->
       let elapsed = Unix.gettimeofday () -. start_time in
       Utils.Log.info Fmt.(fun frmt () -> pf frmt "Solution found in %4.4fs:@;%a@]" elapsed (box PMRS.pp) target);
       (* If no info required, output timing information. *)
       if not !Config.info then Fmt.(pf stdout "%s,%i,%.4fs@." (Caml.Filename.basename(!filename)) !Algo.PmrsAlgos.loop_counter elapsed)

     | Error _ -> Utils.Log.error_msg "No solution found.")
  with s -> (if !Config.show_vars then Term.Variable.print_summary stdout (); raise s)

;;
main ()