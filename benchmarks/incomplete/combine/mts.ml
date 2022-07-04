(** @synduce  -s 2 -NB *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec spec l = sum l, mts l

and sum = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl

and mts = function
  | Nil -> 0
  | Cons (hd, tl) -> max (hd + sum tl) (mts tl)
;;

let rec target t = f [%synt s0] t

and f s = function
  | Nil -> s
  | Cons (hd, tl) -> f ([%synt oplus] hd s) tl
;;
