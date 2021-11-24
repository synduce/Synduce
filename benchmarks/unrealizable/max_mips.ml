(** @synduce --no-lifting -NB -n 20 *)

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

(* The memo trees memoize the max value in a subtree. *)
let rec is_memo = function
  | MLeaf x -> true
  | MNode (n, a, l, r) ->
    n >= tree_max l && n >= tree_max r && n >= a && is_memo l && is_memo r
;;

let rec repr = function
  | MLeaf a -> Leaf a
  | MNode (n, a, tl, tr) -> Node (a, repr tl, repr tr)
;;

let rec spec t = f (0, 0) t [@@ensures fun (x, y) -> y >= 0]

and f s = function
  | Leaf a ->
    let sum, m1 = s in
    sum + a, max (sum + a) m1
  | Node (a, l, r) ->
    let sum, m1 = f s l in
    f (sum + a, max (sum + a) m1) r
;;

let rec target = function
  | MLeaf a -> [%synt s0] a
  | MNode (n, a, l, r) ->
    if n < 0 then [%synt c0] else [%synt join] a (target l) (target r)
  [@@requires is_memo]
;;
