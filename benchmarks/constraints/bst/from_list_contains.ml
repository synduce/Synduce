type 'a list = Elt of 'a | Cons of 'a * 'a list

type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

let rec repr = function Leaf a -> Elt a | Node (a, l, r) -> merge (Cons (a, repr l)) (repr r)

and merge l = function
  | Elt a -> Cons (a, l)
  | Cons (a, l1) ->
      let l2 = merge l l1 in
      Cons (a, l2)

let rec tree_min = function Leaf x -> x | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))

let rec tree_max = function Leaf x -> x | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))

let rec is_bst = function
  | Leaf x -> true
  | Node (a, l, r) -> a >= tree_max l && a <= tree_min r && is_bst l && is_bst r

let spec x t =
  let rec f = function Elt a -> a = x | Cons (a, l) -> a = x || f l in
  f t

let target y t =
  let rec g = function
    | Leaf a -> [%synt xi_0] y a
    | Node (a, l, r) -> if y < a then [%synt xi_1] (g l) else [%synt xi_2] y a (g l) (g r)
  in
  g t
  [@@requires is_bst]
