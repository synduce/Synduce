type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

type 'a tree_m = Leaf' of 'a | Node' of 'a * int * 'a tree_m * 'a tree_m

let rec repr = function Leaf' a -> Leaf a | Node' (a, _i, l, r) -> Node (a, repr l, repr r)

let rec pred = function
  | Leaf' _a -> true
  | Node' (a, n, l, r) -> n = max a (max (aux l) (aux r)) && pred l && pred r

and aux = function Leaf' a -> a | Node' (_a, n, _l, _r) -> n

(** Reference function : contains.
  ⚠️  This syntax is not yet supported (PMRS with a parameter).
  *)
let spec x t =
  let rec aux = function Leaf a -> a == x | Node (a, l, r) -> a == x || aux l || aux r in
  aux t

let target x t =
  let rec aux = function
    | Leaf' a -> a
    | Node' (a, n, l, r) -> if n < x then [%synt x1] else [%synt x2] x a (aux l) (aux r)
  in
  aux t
(* To be continued... *)
