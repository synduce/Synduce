(** @synduce -s 2 -NB *)

type 'a tree =
  | Node of 'a * 'a tree * 'a tree
  | Leaf of 'a

let rec spec = function
  | Leaf x -> x
  | Node (a, l, r) -> (fun m -> f m r) (f a l)

and f s = function
  | Node (a, l, r) -> max a (f (f s r) l)
  | Leaf x -> max x s
;;

let rec target = function
  | Leaf x -> [%synt f0]
  | Node (a, l, r) -> [%synt join]
;;

let repr x = x
