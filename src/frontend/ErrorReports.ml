open Base
open Lexing
open Pmrs_parser.MenhirInterpreter
module S = MenhirLib.General

let debug = false

let extract text (pos1, pos2) : string =
  let ofs1 = pos1.pos_cnum
  and ofs2 = pos2.pos_cnum in
  let len = ofs2 - ofs1 in
  try String.sub text ~pos:ofs1 ~len with
  | Invalid_argument _ ->
    (* In principle, this should not happen, but if it does, let's make this
       a non-fatal error. *)
    if debug then assert false else "???"
;;

let compress text = Str.global_replace (Str.regexp "[ \t\n\r]+") " " text

(* -------------------------------------------------------------------------- *)

(* [sanitize text] eliminates any special characters from the text [text].
   They are (arbitrarily) replaced with a single dot character. *)

let sanitize text =
  String.map
    ~f:(fun c ->
      if Char.get_digit_exn c < 32 || Char.get_digit_exn c >= 127 then '.' else c)
    text
;;

(* -------------------------------------------------------------------------- *)

(* [shorten k text] limits the length of [text] to [2k+3] characters. If the
   text is too long, a fragment in the middle is replaced with an ellipsis. *)

let shorten k text =
  let n = String.length text in
  if n <= (2 * k) + 3
  then text
  else String.sub text ~pos:0 ~len:k ^ "..." ^ String.sub text ~pos:(n - k) ~len:k
;;

(* -------------------------------------------------------------------------- *)

(* [stack checkpoint] extracts the parser's stack out of a checkpoint. *)

let stack checkpoint =
  match checkpoint with
  | HandlingError env -> stack env
  | _ -> assert false
;;

(* this cannot happen, I promise *)

(* -------------------------------------------------------------------------- *)

(* [state checkpoint] extracts the number of the current state out of a
   parser checkpoint. *)

let state checkpoint : int =
  match Lazy.force (stack checkpoint) with
  | S.Nil ->
    (* Hmm... The parser is in its initial state. Its number is
       usually 0. This is a BIG HACK. TEMPORARY *)
    0
  | S.Cons (Element (s, _, _, _), _) -> number s
;;

(* -------------------------------------------------------------------------- *)

(* TEMPORARY move to MenhirLib.General *)

let rec drop n (xs : 'a S.stream) : 'a S.stream =
  match n, xs with
  | 0, _ | _, (lazy S.Nil) -> xs
  | _, (lazy (S.Cons (_, xs))) -> drop (n - 1) xs
;;

(* -------------------------------------------------------------------------- *)

(* [element checkpoint i] returns the [i]-th cell of the parser stack. The index
   [i] is 0-based. [i] should (ideally) be within bounds; we raise [Not_found]
   if it isn't. *)

let element checkpoint i : element =
  match Lazy.force (drop i (stack checkpoint)) with
  | S.Nil ->
    (* [i] is out of range. This could happen if the handwritten error
       messages are out of sync with the grammar, or if a mistake was
       made. We fail in a non-fatal way. *)
    raise Caml.Not_found
  | S.Cons (e, _) -> e
;;

(* -------------------------------------------------------------------------- *)

(* [range text e] converts the stack element [e] to the fragment of the source
   text that corresponds to this stack element. The fragment is placed within
   single quotes and shortened if it is too long. We also ensure that it does
   not contain any special characters. *)

let width = 30

let range text (e : element) : string =
  (* Extract the start and positions of this stack element. *)
  let (Element (_, _, pos1, pos2)) = e in
  (* Get the underlying source text fragment. *)
  let fragment = extract text (pos1, pos2) in
  (* Sanitize it and limit its length. Enclose it in single quotes. *)
  "'" ^ shorten width (sanitize (compress fragment)) ^ "'"
;;

(* -------------------------------------------------------------------------- *)

(* We allow an error message to contain the special form $i, where is a 0-based
   index into the stack. We replace it with the fragment of the source text that
   corresponds to this stack entry. *)

let fragment text checkpoint message =
  try
    let i = Int.of_string (Str.matched_group 1 message) in
    range text (element checkpoint i)
  with
  | Failure _ ->
    (* In principle, this should not happen, but if it does, let's cover up
       for our internal error. *)
    if debug then assert false else "???"
  | Caml.Not_found ->
    (* In principle, this should not happen, but if it does, let's cover up
       for our internal error. *)
    if debug then assert false else "???"
;;

let fragments text checkpoint (message : string) : string =
  Str.global_substitute (Str.regexp "\\$\\([0-9]+\\)") (fragment text checkpoint) message
;;

let report text checkpoint : string =
  let s : int = state checkpoint in
  (* Choose an error message, based on the state number [s].
     Then, customize it, based on dynamic information. *)
  let message =
    try Syntax_messages.message s |> fragments text checkpoint with
    | Caml.Not_found ->
      (* If the state number cannot be found -- which, in principle,
         should not happen, since our list of erroneous states is
         supposed to be complete! -- produce a generic message. *)
      Printf.sprintf
        "This is an unknown syntax error (%d).\n\
         Please report this problem to the compiler vendor.\n"
        s
  in
  (* Construct the full error message. *)
  Fmt.str "Syntax error:@;%s" message
;;
