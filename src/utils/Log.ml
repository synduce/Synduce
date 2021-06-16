open Fmt
open Lexing
open Base

let reference_text = ref ""

let extract text (pos1, pos2) : string =
  let ofs1 = pos1.pos_cnum and ofs2 = pos2.pos_cnum in
  let len = ofs2 - ofs1 in
  try String.sub text ~pos:ofs1 ~len with Invalid_argument _ -> "???"

let compress text = Str.global_replace (Str.regexp "[ \t\n\r]+") " " text

let shorten k text =
  let n = String.length text in
  if n <= (2 * k) + 3 then text
  else String.sub text ~pos:0 ~len:k ^ "..." ^ String.sub text ~pos:(n - k) ~len:k

let log_located (frmt : Formatter.t) (location : position * position) s x =
  let _start, _end = location in
  let start_col = _start.pos_cnum - _start.pos_bol
  and end_col = _end.pos_cnum - _end.pos_bol
  and start_line = _start.pos_lnum
  and end_lin = _end.pos_lnum in
  Fmt.(
    pf frmt "@[<v 2>%s (%i:%i)-(%i:%i)@;%a@]" _start.pos_fname start_line start_col end_lin end_col
      s x)

let ( @! ) (msg : Sexp.t) (loc : position * position) =
  let _start, _end = loc in
  let start_col = _start.pos_cnum - _start.pos_bol
  and end_col = _end.pos_cnum - _end.pos_bol
  and start_line = _start.pos_lnum
  and end_lin = _end.pos_lnum in
  let locstring = str "%s (%i:%i)-(%i:%i)" _start.pos_fname start_line start_col end_lin end_col in
  Sexp.List [ Atom locstring; msg ]

let width = 40

let range text (loc : position * position) : string =
  (* Extract the start and positions of this stack element. *)
  let pos1, pos2 = loc in
  (* Get the underlying source text fragment. *)
  let fragment = extract text (pos1, pos2) in
  (* Sanitize it and limit its length. Enclose it in single quotes. *)
  "'" ^ shorten width (compress fragment) ^ "'"

let log_with_excerpt (frmt : Formatter.t) (ttext : string) (location : position * position) s x =
  let _start, _end = location in
  let start_col = _start.pos_cnum - _start.pos_bol
  and end_col = _end.pos_cnum - _end.pos_bol
  and start_line = _start.pos_lnum
  and end_lin = _end.pos_lnum in
  Fmt.(
    pf frmt "@[<v 2>%s (%i:%i)-(%i:%i): %s@;%a@]" _start.pos_fname start_line start_col end_lin
      end_col (range ttext location) s x)

let wrap (s : string) fmt () = string fmt s

let wrap1 f s t fmt () = pf fmt f s t

let wrap2 f s1 t1 s2 t2 fmt () = pf fmt f s1 t1 s2 t2

let error (msg : Formatter.t -> unit -> unit) : unit =
  pf Fmt.stdout "@[<hov 2>%a@;%a@]@." (styled (`Bg `Red) string) "[ERROR]" msg ()

let error_msg (msg : string) = error (fun fmt () -> pf fmt "%s" msg)

let fatal () = failwith "Fatal error. See messages."

let loc_fatal_errmsg loc msg =
  error (fun f () -> log_with_excerpt f !reference_text loc Fmt.string msg);
  fatal ()

let info (msg : Formatter.t -> unit -> unit) : unit =
  if !Config.info then
    pf Fmt.stdout "@[<hov 4>%a@;%a@]@." (styled (`Bg `Blue) string) " INFO :" msg ()
  else ()

let debug (msg : Formatter.t -> unit -> unit) : unit =
  if !Config.debug then
    pf Fmt.stdout "@[<hov 2>%a@;%a@]@."
      (styled (`Fg `Black) (styled (`Bg `Yellow) string))
      "!DEBUG!" msg ()
  else ()

let debug_msg (msg : string) =
  debug (fun fmt () -> pf fmt "%s" (String.prefix msg !Config.debug_msg_max_chars))

let print_ok () =
  if !Config.debug then
    pf Fmt.stdout "%a@." (styled (`Fg `Black) (styled (`Bg `Green) string)) "  OK   "
  else ()

let verbose msg =
  if !Config.verbose then
    pf Fmt.stdout "@[<hov 2>%a@;%a@]@."
      (styled (`Fg `Black) (styled (`Bg `Cyan) string))
      " VERB <" msg ()
  else ()

let verbose_msg msg =
  if !Config.verbose then
    pf Fmt.stdout "@[<hov 2>%a@;%s@]@."
      (styled (`Fg `Black) (styled (`Bg `Cyan) string))
      " VERB <" msg
  else ()

let to_file (file : string) (msg : Formatter.t -> unit -> unit) : unit =
  let oc = Stdio.Out_channel.create file in
  let frmt = Caml.Format.formatter_of_out_channel oc in
  msg frmt ()
