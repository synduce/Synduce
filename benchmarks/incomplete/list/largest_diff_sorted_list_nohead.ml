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
  | Elt x -> 0
  | Cons (hd, tl) ->
    let r = ldiff tl in
    max (max_diff_with hd tl) r

and max_diff_with x = function
  | Elt y -> abs (x - y)
  | Cons (hd, tl) -> max (abs (x - hd)) (max_diff_with hd tl)
;;

(* Synthesize a linear function. *)
let rec amax = function
  | Elt x -> [%synt g0] x
  | Cons (hd, tl) -> [%synt join] hd (amax tl)
  [@@requires is_sorted]
;;

assert (amax = ldiff)
