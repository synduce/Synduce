type 'b list = Elt of 'b | Cons of 'b * 'b list

type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

let rec repr = function Leaf a -> Elt a | Node (a, l, r) -> merge (Cons (a, repr l)) (repr r)

and merge l = function
  | Elt a -> Cons (a, l)
  | Cons (a, m) ->
      let r = merge l m in
      Cons (a, r)

let rec tree_min = function Leaf x -> x | Node (a, l, r) -> min a (min (tree_min l) (tree_min r))

let rec tree_max = function Leaf x -> x | Node (a, l, r) -> max a (max (tree_max l) (tree_max r))

let rec is_bst = function
  | Leaf x -> true
  | Node (a, l, r) -> a >= tree_max l && a <= tree_min r && is_bst l && is_bst r

let rec spec = function Elt a -> a | Cons (a, l) -> max a (spec l)

let rec target = function Leaf a -> [%synt xi_0] a | Node (a, l, r) -> [%synt xi_1] a (target r)
  [@@requires is_bst]
