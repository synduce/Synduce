(** @synduce -s 2 -NB *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

type 'a tree_memo =
  | MLeaf of 'a
  | MNode of int * 'a * 'a tree_memo * 'a tree_memo

let rec is_memo = function
  | MLeaf x -> true
  | MNode (n, a, l, r) -> n = 1 + memo l + memo r && is_memo l && is_memo r

and memo = function
  | MLeaf x -> 1
  | MNode (n, a, l, r) -> n
;;

let rec repr = function
  | MLeaf a -> Leaf a
  | MNode (n, a, tl, tr) -> Node (a, repr tl, repr tr)
;;

let rec spec = function
  | Leaf a -> 1
  | Node (a, l, r) -> 1
;;

let rec target = function
  | MLeaf a -> 1
  | MNode (n, a, l, r) -> if n < 0 then 2 else [%synt xi_1] n
  [@@requires is_memo]
;;
