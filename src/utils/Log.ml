open Fmt
open Lexing
open Base

(* ============================================================================================= *)
(*        Printing errors in the input program, with some context on where the error appears.    *)
(* ============================================================================================= *)

let reference_text = ref ""

let extract text (pos1, pos2) : string =
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let len = ofs2 - ofs1 in
  try String.sub text ~pos:ofs1 ~len with
  | Invalid_argument _ -> "???"
;;

let compress text = Str.global_replace (Str.regexp "[ \t\n\r]+") " " text

let shorten k text =
  let n = String.length text in
  if n <= (2 * k) + 3
  then text
  else String.sub text ~pos:0 ~len:k ^ "..." ^ String.sub text ~pos:(n - k) ~len:k
;;

let log_located (frmt : Formatter.t) (location : position * position) s x =
  let _start, _end = location in
  let start_col = _start.pos_cnum - _start.pos_bol
  and end_col = _end.pos_cnum - _end.pos_bol
  and start_line = _start.pos_lnum
  and end_lin = _end.pos_lnum in
  Fmt.(
    pf
      frmt
      "@[<v 2>%s (%i:%i)-(%i:%i)@;%a@]"
      _start.pos_fname
      start_line
      start_col
      end_lin
      end_col
      s
      x)
;;

let ( @! ) (msg : Sexp.t) (loc : position * position) =
  let _start, _end = loc in
  let start_col = _start.pos_cnum - _start.pos_bol
  and end_col = _end.pos_cnum - _end.pos_bol
  and start_line = _start.pos_lnum
  and end_lin = _end.pos_lnum in
  let locstring =
    str "%s (%i:%i)-(%i:%i)" _start.pos_fname start_line start_col end_lin end_col
  in
  Sexp.List [ Atom locstring; msg ]
;;

let width = 40

let range text (loc : position * position) : string =
  (* Extract the start and positions of this stack element. *)
  let pos1, pos2 = loc in
  (* Get the underlying source text fragment. *)
  let fragment = extract text (pos1, pos2) in
  (* Sanitize it and limit its length. Enclose it in single quotes. *)
  "'" ^ shorten width (compress fragment) ^ "'"
;;

let log_with_excerpt
    (frmt : Formatter.t)
    (ttext : string)
    (location : position * position)
    s
    x
  =
  let _start, _end = location in
  let start_col = _start.pos_cnum - _start.pos_bol
  and end_col = _end.pos_cnum - _end.pos_bol
  and start_line = _start.pos_lnum
  and end_lin = _end.pos_lnum in
  Fmt.(
    pf
      frmt
      "@[<h 8>%s (%i:%i)-(%i:%i): %s@;%a@]"
      _start.pos_fname
      start_line
      start_col
      end_lin
      end_col
      (range ttext location)
      s
      x)
;;

(* ============================================================================================= *)
(*                            General purpose printing functions with helpers.                   *)
(* ============================================================================================= *)

let wrap (s : string) fmt () = string fmt s
let wrap1 f s t fmt () = pf fmt f s t
let wrap2 f s1 t1 s2 t2 fmt () = pf fmt f s1 t1 s2 t2
let wrapn fmt () = pf fmt
let indent = ref 0
let section_indent () = String.of_char_list (List.init !indent ~f:(fun _ -> '\t'))

let error (msg : Formatter.t -> unit -> unit) : unit =
  if !Config.info
  then
    pf
      Fmt.stdout
      "%s@[<h 8>%a@;%a@]@."
      (section_indent ())
      (styled (`Bg `Red) string)
      "[ERROR]"
      msg
      ()
  else ()
;;

let error_msg (msg : string) = error (fun fmt () -> pf fmt "%s" msg)
let fatal () = failwith "Fatal error. See messages."

let loc_fatal_errmsg loc msg =
  error (fun f () -> log_with_excerpt f !reference_text loc Fmt.string msg);
  fatal ()
;;

(** Info messaging is the lowest verbose output, activated by default.  *)
let info (msg : Formatter.t -> unit -> unit) : unit =
  if !Config.info
  then
    pf
      Fmt.stdout
      "%s@[<h 8>%a@;%a@]@."
      (section_indent ())
      (styled (`Bg `Blue) string)
      " INFO :"
      msg
      ()
  else ()
;;

let hline = String.concat (List.init 100 ~f:(fun _ -> "-"))

let sep ?(i = None) () =
  if !Config.info
  then (
    match i with
    | Some i ->
      let is = Int.to_string i in
      let hline = String.drop_suffix hline (String.length is + 3) in
      pf Fmt.stdout "@.%s[%s]-" hline is
    | None -> pf Fmt.stdout "@.%s" hline)
  else ()
;;

(** Debug messaging is used mostly for printing status of interactions with external solvers.  *)
let debug (msg : Formatter.t -> unit -> unit) : unit =
  if !Config.debug
  then
    pf
      Fmt.stdout
      "%s@[<h 8>%a@;%a@]@."
      (section_indent ())
      (styled (`Fg `Black) (styled (`Bg `Yellow) string))
      "!DEBUG!"
      msg
      ()
  else ()
;;

let debug_msg (msg : string) =
  debug (fun fmt () -> pf fmt "%s" (String.prefix msg !Config.debug_msg_max_chars))
;;

let print_ok () =
  if !Config.debug
  then pf Fmt.stdout "%a@." (styled (`Fg `Black) (styled (`Bg `Green) string)) "  OK   "
  else ()
;;

(** Verbose output for printing status of internal methods.  *)
let verb msg =
  if !Config.verbose
  then
    pf
      Fmt.stdout
      "%s@[<h 8>%a@;@[%a@]@]@."
      (section_indent ())
      (styled (`Fg `Black) (styled (`Bg `Cyan) string))
      " VERB <"
      msg
  else fun _ -> ()
;;

let verbose msg = if !Config.verbose then verb msg () else ()
let verbose_msg msg = verbose (fun fmt () -> pf fmt "%s" msg)

let start_section s =
  info (fun fmt () -> pf fmt "%s" s);
  Int.incr indent
;;

let end_section () = Int.decr indent

(* ============================================================================================= *)
(*                            Printing info on solver sub-processes                              *)
(* ============================================================================================= *)
let status_printer () =
  while true do
    Unix.sleepf 0.05;
    let ids = Stats.get_alive () in
    let s =
      String.concat
        ~sep:" "
        (List.map ids ~f:(fun (sname, sid) ->
             "(" ^ sname ^ " : " ^ Int.to_string sid ^ ")"))
    in
    Caml.Format.printf "Running: %s\r" s;
    Caml.flush Caml.stdout;
    ()
  done
;;

(* ============================================================================================= *)
(*                            Miscelleanous                                                      *)
(* ============================================================================================= *)

(** Print a message function to a file. *)
let to_file (file : string) (msg : Formatter.t -> unit -> unit) : unit =
  let oc = Stdio.Out_channel.create file in
  let frmt = Caml.Format.formatter_of_out_channel oc in
  msg frmt ();
  Stdio.Out_channel.close oc
;;

let print_solvers_summary (i : int) (frmt : Formatter.t) () : unit =
  let open Fmt in
  let json_data = LogJson.get_solver_stats i in
  match json_data with
  | Some data ->
    (match data with
    | `Assoc solver_stats ->
      let total_solvers_time = ref 0.0 in
      let total_instances = ref 0 in
      let f (key, data) =
        match data with
        | `Assoc [ ("total-time", `Float total_time); ("num-instances", `Int instances) ]
          ->
          total_solvers_time := !total_solvers_time +. total_time;
          total_instances := !total_instances + instances;
          key, total_time, instances
        | _ -> key, -1.0, -1
      in
      let l = List.map ~f solver_stats in
      let pp frmt (key, time, instances) =
        Fmt.(pf frmt "@[<v 0>%-10s [%4i instances] %.3fs @]" key instances time)
      in
      pf
        frmt
        "@[<v 0>Total time spent in solvers:@;@[%a@]@;> %-8s [%4i instances]: %.3fs@]@;"
        (list ~sep:(fun fmt () -> pf fmt "@;<80 0>") pp)
        l
        "TOTAL"
        !total_instances
        !total_solvers_time
    | _ -> ())
  | _ -> ()
;;
