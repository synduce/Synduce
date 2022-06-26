(** @synduce -s 2 -NB *)

type 'a tree =
  | Node of 'a * 'a tree * 'a tree
  | Leaf of 'a

let rec spec = function
  | Leaf x -> x
  | Node (a, l, r) -> max (f a l) (f a r)

and f s = function
  | Leaf x -> max s x
  | Node (a, l, r) -> (fun m -> max a (f m l)) (f s r)
;;

let rec target = function
  | Leaf x -> [%synt f0]
  | Node (a, l, r) -> [%synt join]
;;

let repr x = x
