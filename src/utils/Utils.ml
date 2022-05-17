open Base
open Sexplib
module Config = Config
module Concurrency = Concurrency
module Stats = Stats
module Log = Log
module LogJson = LogJson
module IntMap = Map.M (Int)
module StringMap = Map.M (Int)

type z = Z : z
type 'n s = S : 'n -> 'n s

let identity x = x
let first (a, _) = a
let second (_, b) = b
let ( <| ) (f1 : 'a -> 'b) (f2 : 'c -> 'a) x = f1 (f2 x)

let rec product (l : 'a list list) : 'a list list =
  let rec aux ~acc l1 l2 =
    match l1, l2 with
    | [], _ | _, [] -> acc
    | h1 :: t1, h2 :: t2 ->
      let acc = (h1 :: h2) :: acc in
      let acc = aux ~acc t1 l2 in
      aux ~acc [ h1 ] t2
  in
  match l with
  | [] -> []
  | [ l1 ] -> List.map ~f:(fun x -> [ x ]) l1
  | l1 :: tl ->
    let tail_product = product tl in
    aux ~acc:[] l1 tail_product
;;

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Fmt.(pf stdout "ELAPSED: %f s@." (Unix.gettimeofday () -. t));
  res
;;

let ( >>?| ) = Option.( >>| )
let ( >>!| ) = Result.( >>| )
let ( >>=? ) = Option.( >>= )
let ( >>=! ) = Result.( >>= )
let repeat n f ~init = List.fold ~init ~f:(fun acc _ -> f acc) (List.range 0 n)
let satom a = Sexp.Atom a
let slist a = Sexp.List a
let blast x = Result.map_error ~f:List.concat (Result.combine_errors x)
let pair a b = a, b
let index_list = List.mapi ~f:pair

let sub_list (l : 'a list) (il : int list) =
  let rec aux l il a n =
    match l, il with
    | [], _ | _ :: _, [] -> a
    | l_hd :: l_tl, il_hd :: il_tl ->
      if il_hd < 0
      then aux l_tl il_tl a (n + 1)
      else if il_hd = n
      then aux l_tl il_tl (a @ [ l_hd ]) (n + 1)
      else aux l_tl il a (n + 1)
  in
  aux l (List.sort ~compare il) [] 0
;;

let trim (s : string) = Str.global_replace (Str.regexp "[\r\n\t ]") "" s

(* ============================================================================================= *)
(*                  PRETTY PRINTING HELPERS                                                      *)
(* ============================================================================================= *)
let ast frmt () = Fmt.(pf frmt "@;*@;")
let colon frmt () = Fmt.(pf frmt "@;:@;")
let vbar frmt () = Fmt.(pf frmt "@;|@;")
let dot frmt () = Fmt.(pf frmt ".")
let rightarrow frmt () = Fmt.(pf frmt " ⟶ @;")
let leftarrow frmt () = Fmt.(pf frmt " <-@;")
let sep_and : Formatter.t -> unit -> unit = Fmt.any "@;and@;"
let pp_link frmt target = Fmt.pf frmt "file://%s" target

(** list_or_space prints the list using f for each element, and sep for the separator.
  If the list is empty, prints a space.
  If the list is not empty, the printed list is surrounded by two spaces.
*)
let list_or_space ~sep ~f frmt li =
  match li with
  | [] -> Fmt.pf frmt " "
  | _ as l -> Fmt.(pf frmt " %a " (list ~sep f) l)
;;

(** option_or_space prints the option using f for the data, and sep for the separator.
  If the option is None, prints a space.
  If the option is not None, the printed data is surrounded by two spaces.
*)
let option_or_space ~f frmt o =
  match o with
  | Some x -> Fmt.(pf frmt "%a" f x)
  | None -> Fmt.(pf frmt " ")
;;

let to_subscript_unicode (i : int) =
  let f c =
    match c with
    | '1' -> "₁"
    | '2' -> "₂"
    | '3' -> "₃"
    | '4' -> "₄"
    | '5' -> "₅"
    | '6' -> "₆"
    | '7' -> "₇"
    | '8' -> "₈"
    | '9' -> "₉"
    | '0' -> "₀"
    | _ -> String.of_char c
  in
  String.concat_map ~f (Int.to_string i)
;;

(* ============================================================================================= *)
(*                  LISTS HELPERS                                                                *)
(* ============================================================================================= *)
let list_map_snd (l : ('a * 'b) list) ~(f : 'b -> 'c) : ('a * 'c) list =
  List.map ~f:(fun (a, b) -> a, f b) l
;;

let cartesian_nary_product (elts : 'a list list) : 'a list list =
  let f acc l =
    List.concat
      (List.map l ~f:(fun elt -> List.map acc ~f:(fun acc_l -> acc_l @ [ elt ])))
  in
  match elts with
  | hd :: tl -> List.fold ~f ~init:(List.map ~f:(fun x -> [ x ]) hd) tl
  | [] -> []
;;

let combinations (l : 'a list) : ('a * 'a) list =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (acc @ List.map ~f:(fun x -> hd, x) (hd :: tl)) tl
  in
  aux [] l
;;

let all_or_none (l : 'a option list) : 'a list option =
  let l' = Option.all l in
  match l' with
  | Some _l -> if List.length _l = List.length l then l' else None
  | None -> None
;;

let subsets l =
  let rec aux x = function
    | [] -> x
    | hd :: tl -> aux (([ hd ] :: x) @ List.map ~f:(fun l -> hd :: l) x) tl
  in
  aux [] l
;;

type 'a continue_or_stop =
  | Continue of 'a
  | Stop of 'a

let lwt_until
    ~(f : 'a -> 'b -> 'a continue_or_stop Lwt.t)
    ~(init : 'a Lwt.t)
    (l : 'b list)
    : 'a Lwt.t
  =
  let rec aux c = function
    | hd :: tl ->
      let c' = Lwt.bind c (fun x -> f x hd) in
      Lwt.bind c' (function
          | Continue x -> aux (Lwt.return x) tl
          | Stop x -> Lwt.return x)
    | [] -> c
  in
  aux init l
;;

(* ============================================================================================= *)
(*                  FILE MANAGEMENT HELPERS                                                      *)
(* ============================================================================================= *)

let relative_to_root (filename : string) =
  let curdir = Caml.Filename.current_dir_name in
  Str.string_after filename (String.length curdir)
;;
