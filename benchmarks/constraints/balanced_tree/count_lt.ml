(* Run with --no-lifting -dvN *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec tree_min = function
  | Leaf x -> x
  | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))
;;

let rec tree_max = function
  | Leaf x -> x
  | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))
;;

let rec bst_balanced = function
  | Leaf a -> true
  | Node (a, b, c) ->
    a >= tree_max b
    && a <= tree_min c
    && aux b = aux c
    && bst_balanced b
    && bst_balanced c

and aux = function
  | Leaf x -> 0
  | Node (a, b, c) -> 1 + max (aux b) (aux c)
;;

let repr x = x

let spec x t =
  let rec f = function
    | Leaf a -> if a < x then 1 else 0
    | Node (a, l, r) -> if a < x then 1 + f l + f r else f l + f r
  in
  f t
  [@@ensures fun x -> x >= 0]
;;

let target y t =
  let rec g = function
    | Leaf a -> [%synt xi_0] y a
    | Node (a, l, r) -> if a < y then [%synt xi_1] (size l) (g r) else [%synt xi_2] (g l)
  and size = function
    | Leaf a -> 1
    | Node (a, l, r) -> 1 + size l + size r
  in
  g t
  [@@requires bst_balanced]
;;

assert (target = spec)
