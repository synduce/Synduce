(** @synduce -s 2 -NB *)

type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree

let rec balanced = function
  | Nil -> true
  | Node (a, b, c) -> aux b = aux c && balanced b && balanced c

and aux = function
  | Nil -> 0
  | Node (a, b, c) -> 1 + max (aux b) (aux c)
  [@@ensures fun x -> x >= 0]
;;

let rec height = function
  | Nil -> 0
  | Node (a, l, r) -> 1 + max (height l) (height r)
;;

let rec target = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> [%synt f0] (target l)
  [@@requires balanced]
;;

assert (target = height)
