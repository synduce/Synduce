(** @synduce -s 2 -NB --no-lifting *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

type 'a tree_memo =
  | MLeaf of 'a
  | MNode of int * 'a * 'a tree_memo * 'a tree_memo

let rec tree_max = function
  | MLeaf x -> x
  | MNode (n, a, l, r) -> max a (max (tree_max l) (tree_max r))
;;

let rec is_memo = function
  | MLeaf x -> true
  | MNode (n, a, l, r) ->
    n >= tree_max l && n >= tree_max r && n >= a && is_memo l && is_memo r
;;

let rec repr = function
  | MLeaf a -> Leaf a
  | MNode (n, a, tl, tr) -> Node (a, repr tl, repr tr)
;;

let spec x t =
  let rec f = function
    | Leaf a -> x = a
    | Node (a, l, r) -> x = a || f l || f r
  in
  f t
;;

let target x t =
  let rec g = function
    | MLeaf a -> [%synt s0] x a
    | MNode (n, a, l, r) -> if x > n then [%synt c0] x a else [%synt f1] x a (g l) (g r)
  in
  g t
  [@@requires is_memo]
;;
