type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec repr  = function x -> x

let rec sum = function t -> f 0 t
and f s =
  function
  | Nil -> s
  | Node(a, l, r) -> let sum = f s l in f (sum + a) r

let rec hsum =
  function
  | Nil -> s0
  | Node(a, l, r) -> join a (hsum l) (hsum r)
  [@defining join s0]
  [@equiv sum repr]