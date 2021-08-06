(* Run with --no-lifting -dvN *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec balanced = function
  | Nil -> true
  | Node (a, b, c) -> height b = height c && balanced b && balanced c

and height = function
  | Nil -> 0
  | Node (a, b, c) -> 1 + max (height b) (height c)
  [@@ensures fun x -> x >= 0]

let rec count = function Nil -> 0 | Node (a, l, r) -> 1 + count l + count r

let rec target = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> [%synt f0] a (target l)
  [@@requires balanced]

;;
assert (target = count)
