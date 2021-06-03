open Base
open Sexplib
module Config = Config
module Log = Log
module IntMap = Map.M (Int)
module StringMap = Map.M (Int)

type z = Z : z

type 'n s = S : 'n -> 'n s

let first (a, _) = a

let second (_, b) = b

let ( <| ) (f1 : 'a -> 'b) (f2 : 'c -> 'a) x = f1 (f2 x)

let rec product (l : 'a list list) : 'a list list =
  let rec aux ~acc l1 l2 =
    match (l1, l2) with
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

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Fmt.(pf stdout "ELAPSED: %f s@." (Unix.gettimeofday () -. t));
  res

let ( >>?| ) = Option.( >>| )

let ( >>!| ) = Result.( >>| )

let ( >>=? ) = Option.( >>= )

let ( >>=! ) = Result.( >>= )

let repeat n f ~init = List.fold ~init ~f:(fun acc _ -> f acc) (List.range 0 n)

let satom a = Sexp.Atom a

let slist a = Sexp.List a

let blast x = Result.map_error ~f:List.concat (Result.combine_errors x)

let pair a b = (a, b)

(* ============================================================================================= *)
(*                  PRETTY PRINTING HELPERS                                                      *)
(* ============================================================================================= *)
let ast frmt () = Fmt.(pf frmt "@;*@;")

let colon frmt () = Fmt.(pf frmt "@;:@;")

let rightarrow frmt () = Fmt.(pf frmt " ->@;")

let leftarrow frmt () = Fmt.(pf frmt " <-@;")

let sep_and : Formatter.t -> unit -> unit = Fmt.any "@;and@;"

let list_or_space ~sep ~f frmt li =
  match li with [] -> Fmt.pf frmt " " | _ as l -> Fmt.(pf frmt " %a " (list ~sep f) l)

let option_or_space ~f frmt o =
  match o with Some x -> Fmt.(pf frmt "%a" f x) | None -> Fmt.(pf frmt " ")

(* ============================================================================================= *)
(*                  LISTS HELPERS                                                                *)
(* ============================================================================================= *)

let cartesian_nary_product (elts : 'a list list) : 'a list list =
  let f acc l =
    List.concat (List.map l ~f:(fun elt -> List.map acc ~f:(fun acc_l -> elt :: acc_l)))
  in
  match elts with hd :: tl -> List.fold ~f ~init:(List.map ~f:(fun x -> [ x ]) hd) tl | [] -> []

let combinations (l : 'a list) : ('a * 'a) list =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (acc @ List.map ~f:(fun x -> (hd, x)) (hd :: tl)) tl
  in
  aux [] l

let all_or_none (l : 'a option list) : 'a list option =
  let l' = Option.all l in
  match l' with Some _l -> if List.length _l = List.length l then l' else None | None -> None

let relative_to_root (filename : string) =
  let curdir = Caml.Filename.current_dir_name in
  Str.string_after filename (String.length curdir)
