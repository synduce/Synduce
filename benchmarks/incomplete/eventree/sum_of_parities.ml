(** @synduce -s 2 -NB *)

type 'a tree =
  | Elt of 'a
  | Node of 'a * 'a tree * 'a tree

let rec is_even = function
  | Elt a -> a mod 2 = 0
  | Node (a, l, r) -> a mod 2 = 0 && is_even l && is_even r
;;

let rec spec = function
  | Elt a -> a mod 2
  | Node (a, l, r) -> (a mod 2) + spec l + spec r
  [@@ensures fun x -> x >= 0]
;;

let rec target = function
  | Elt a -> [%synt c0]
  | Node (a, l, r) -> [%synt c1]
  [@@requires is_even]
;;
