(* Sum elements larger than parameter. *)

(* Type of binary search trees with integer keys.*)
type 'a bst =
  | BstNil : 'a bst
  | BstNode : ('a[@param x]) * ('a[@dep _v < x]) bst * ('a[@dep _v > x]) bst -> 'a bst

(* Type of simple trees. *)
type 'a tree = Nil : 'a tree | Node : 'a * 'a tree * 'a tree -> 'a tree

let rec sumlgt x =
  let rec f = function BstNil -> 0 | BstNode (a, b, c) -> if a > x then a + f c + f b else f c in
  f

let rec h x = function Nil -> [%synt s0] | Node (a, l, r) -> [%synt join] x a (h x l) (h x r)
