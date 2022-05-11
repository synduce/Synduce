(* This example is adapted from:
 The third homomorphism theorem on trees: upwards and downwards
 lead to divide-and-conquer *)
type 'a tree =
  | Node of 'a * 'a tree * 'a tree
  | Nil

let rec spec t = mp 0 t

and mp s = function
  | Nil -> s
  | Node (a, l, r) -> (fun m -> max m (mp (s + a) l)) (mp (s + a) r)
;;

let rec target = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> [%synt join] a (target l) (target r)
;;

let repr x = x
