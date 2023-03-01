(** @synduce -s 2 -NB *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec spec l = sum l, mts 0 l, mps l

and mts s = function
  | Nil -> s
  | Cons (hd, tl) -> mts (max (s + hd) 0) tl

and sum = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl

and mps = function
  | Nil -> 0
  | Cons (hd, tl) -> max (mps tl + hd) 0
  [@@ensures fun (x, y, z) -> y >= 0 && z >= 0]
;;

let rec target = function
  | Nil -> [%synt s0]
  | Cons (hd, tl) -> [%synt oplus] hd (target tl)
;;
