(** @synduce --no-lifting -NB -n 10 *)

(* The type of binary trees. *)
type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

(* A function returning the minimum of a tree. *)
let rec tree_min = function
  | Leaf x -> x
  | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))
;;

(* A function returning the maximum of a tree. *)
let rec tree_max = function
  | Leaf x -> x
  | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))
;;

(* The data invariant is_bst *)
let rec is_bst = function
  | Leaf x -> true
  | Node (a, l, r) -> a > tree_max l && a < tree_min r && is_bst l && is_bst r
;;

let repr x = x

(* The function count the number of elements in the tree between hi and lo. *)
let countbtw lo hi t =
  let rec f = function
    | Leaf a -> if hi > a && a > lo then 1 else 0
    | Node (a, l, r) -> if hi > a && a > lo then 1 + f l + f r else f l + f r
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

(* This is our target recursion skeleton.  *)
let target lo hi t =
  let rec g = function
    | Leaf a -> [%synt g0] hi lo a
    | Node (a, l, r) ->
      if a <= lo
      then [%synt g1] (g r)
      else if a >= hi
      then [%synt g2] (g l)
      else [%synt g3] a hi lo (g l) (g r)
  in
  g t
  [@@requires is_bst]
;;

assert (target = countbtw)
