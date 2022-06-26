(** @synduce -s 2 -NB -n 20 --no-lifting *)

type clist =
  | Elt of int
  | Cons of int * clist

let rec is_sorted = function
  | Elt x -> true
  | Cons (hd, tl) -> hd > 0 && aux hd tl

and aux prev = function
  | Elt x -> prev > x && x > 0
  | Cons (hd, tl) -> prev > hd && hd > 0 && aux hd tl
;;

(* Reference function : quadratic time. *)
let rec ldiff = function
  | Elt x -> 0, x, x
  | Cons (hd, tl) ->
    let amax, amin, al = ldiff tl in
    let newmax = max amax hd in
    let newmin = min amin hd in
    newmax, newmin, max al (newmax - newmin)
;;

(* Synthesize a linear function. *)
let rec ldiff2 = function
  | Elt x -> [%synt g0] x
  | Cons (hd, tl) -> [%synt join] hd (last tl)
  [@@requires is_sorted]

and last = function
  | Elt x -> x
  | Cons (hd, tl) -> last tl
;;

assert (ldiff2 = ldiff)
