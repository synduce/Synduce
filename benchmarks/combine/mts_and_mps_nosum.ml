(** @synduce --max-lifting=1 *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec spec l = mts 0 l, mps l [@@ensures fun (y, z) -> y >= 0 && z >= 0]

and mts s = function
  | Nil -> s
  | Cons (hd, tl) -> mts (max (s + hd) 0) tl

and mps = function
  | Nil -> 0
  | Cons (hd, tl) -> max (mps tl + hd) 0
;;

let rec target = function
  | Nil -> [%synt s0]
  | Cons (hd, tl) -> [%synt oplus] hd (target tl)
;;
