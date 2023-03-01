(** @synduce -s 2 -NB *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

type 'a tree_memo =
  | MLeaf of 'a
  | MNode of int * 'a * 'a tree_memo * 'a tree_memo

let rec tree_min = function
  | Leaf x -> x
  | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))
;;

let rec tree_max = function
  | Leaf x -> x
  | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))
;;

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
  | Node (a, l, r) -> 1 + spec l + spec r
  [@@ensures fun x -> x >= 1]
;;

let rec target = function
  | MLeaf a -> [%synt xi_0]
  | MNode (n, a, l, r) -> [%synt xi_1] n
  [@@requires is_memo]
;;
