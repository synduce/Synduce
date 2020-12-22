open Base
open Sexplib

module Config = Config
module Log = Log
module IntMap = Map.M(Int)
module StringMap = Map.M(Int)

type z = Z : z

type 'n s = S : 'n -> 'n s

let first (a,_) = a
let second (_, b) = b

let rec product (l : 'a list list) : 'a list list  =
  let rec aux ~acc l1 l2 = match l1, l2 with
    | [], _ | _, [] -> acc
    | h1::t1, h2::t2 ->
      let acc = (h1::h2)::acc in
      let acc = (aux ~acc t1 l2) in
      aux ~acc [h1] t2
  in match l with
  | [] -> []
  | [l1] -> List.map ~f:(fun x -> [x]) l1
  | l1::tl ->
    let tail_product = product tl in
    aux ~acc:[] l1 tail_product

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Fmt.(pf stdout "ELAPSED: %f s@." (Unix.gettimeofday () -. t));
  res


let (>>?|) = Option.(>>|)
let (>>!|) = Result.(>>|)
let (>>=?) = Option.(>>=)
let (>>=!) = Result.(>>=)

let repeat n f ~init = List.fold ~init ~f:(fun acc _ -> f acc) (List.range 0 n)

let satom a = Sexp.Atom a
let slist  a = Sexp.List a

let blast = fun x -> Result.map_error ~f:List.concat (Result.combine_errors x)

let pair a b = (a,b)

let ast frmt () = Fmt.(pf frmt "@;*@;")

let cartesian_nary_product (elts : ('a list) list) : ('a list) list =
  let f acc l =
    List.concat
      (List.map l ~f:(fun elt -> List.map acc ~f:(fun acc_l -> elt :: acc_l)))
  in
  match elts with
  | hd :: tl -> List.fold ~f ~init:(List.map ~f:(fun x -> [x]) hd) tl
  | [] -> []

let all_or_none (l : ('a option) list) : ('a list) option =
  let rec aux a l =
    Option.bind a
      ~f:(fun y -> match l with
          | [] -> a
          | (Some x) :: tl -> aux (Some (x :: y)) tl
          | None :: _ -> None)
  in
  match l with
  | [] -> Some []
  | None :: _ -> None
  | (Some a) :: tl -> aux (Some [a]) tl