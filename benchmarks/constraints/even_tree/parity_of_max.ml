type 'a tree = Elt of 'a | Node of 'a * 'a tree * 'a tree

let rec is_even = function
  | Elt a -> a mod 2 = 0
  | Node (a, l, r) -> a mod 2 = 0 && is_even l && is_even r

let rec spec t = tree_max t mod 2

and tree_max = function
  | Elt a -> a
  | Node (a, l, r) ->
      let lmax = tree_max l in
      let rmax = tree_max r in
      if a > lmax && a > rmax then a mod 2 else if lmax > rmax then lmax mod 2 else rmax mod 2

let rec target = function Elt a -> [%synt c0] | Node (a, l, r) -> [%synt c1] [@@requires is_even]
