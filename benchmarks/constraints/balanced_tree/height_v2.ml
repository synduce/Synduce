(* Run with --no-lifting -dvN *)

type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree

let rec balanced = function
  | Nil -> true
  | Node (a, b, c) -> aux b = aux c && balanced b && balanced c

and aux = function
  | Nil -> 0
  | Node (a, b, c) -> 1 + max (aux b) (aux c)
;;

let rec height = function
  | Nil -> 0
  | Node (a, l, r) -> 1 + max (height l) (height r)
;;

(* A small variation of height. *)
let rec target = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> a + [%synt f0] a (target l)
  [@@requires balanced]
;;

assert (target = height)
