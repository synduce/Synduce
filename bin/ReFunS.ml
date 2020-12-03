open Base
open Getopt
open Fmt
open Parsers
open Algo.PmrsAlgos

module Config = Lib.Utils.Config

let options = [
  ('v', "verbose", (set Config.verbose true), None);
]

let main () =
  let filename = ref "" in
  parse_cmdline options (fun s -> filename := s);
  set_style_renderer stdout `Ansi_tty;
  Caml.Format.set_margin 100;
  let prog = parse_pmrs !filename in
  let _ = seek_types prog in
  let plist = translate prog in
  let do_prog p =
    Lang.(
      let mgt = most_general_terms p in
      pf stdout "%a@." PMRScheme.pp_pmrs p;
      if List.length mgt > 0 then
        pf stdout "Most general terms: %a@."
          (list Term.pp_term) mgt)
  in
  List.iter plist  ~f:do_prog;
  Lang.Term.Variable.print_summary stdout ()
;;
main ()