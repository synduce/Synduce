(** @synduce -s 2 -NB *)

type 'a clist =
  | Elt of 'a
  | Cons of 'a * 'a clist

let rec is_sorted = function
  | Elt x -> true
  | Cons (hd, tl) -> aux hd tl

and aux prev = function
  | Elt x -> prev >= x
  | Cons (hd, tl) -> prev >= hd && aux hd tl
;;

let rec maxs = function
  | Elt x -> x
  | Cons (hd, tl) -> max hd (maxs tl)
;;

let rec amax = function
  | Elt x -> [%synt base]
  | Cons (hd, tl) -> [%synt join]
  [@@requires is_sorted]
;;

assert (amax = maxs)
