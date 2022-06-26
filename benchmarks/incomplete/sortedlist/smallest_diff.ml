(** @synduce -s 2 -NB *)

type clist =
  | Elt of int
  | Cons of int * clist

let rec is_sorted = function
  | Elt x -> true
  | Cons (hd, tl) -> aux hd tl

and aux prev = function
  | Elt x -> prev >= x
  | Cons (hd, tl) -> prev >= hd && aux hd tl
;;

(* Reference function : quadratic time. *)
let rec ldiff = function
  | Elt x -> 0, x
  | Cons (hd, tl) ->
    let r, s = ldiff tl in
    min (min_diff_with hd tl) r, hd

and min_diff_with x = function
  | Elt y -> -abs (x - y)
  | Cons (hd, tl) -> min (-abs (x - hd)) (min_diff_with hd tl)
;;

(* Synthesize a linear function. *)
let rec amax = function
  | Elt x -> [%synt g0] x
  | Cons (hd, tl) -> [%synt join] hd (amax tl)
  [@@requires is_sorted]
;;

assert (amax = ldiff)
