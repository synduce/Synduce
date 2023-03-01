(** @synduce -s 2 -NB *)

type 'a tree =
  | Node of 'a * 'a tree * 'a tree
  | Leaf of 'a

let rec spec = function
  | Leaf x -> x
  | Node (a, l, r) ->
    let m = h a l in
    h m r

and h s = function
  | Leaf x -> min x s
  | Node (a, l, r) ->
    let m = h (min a s) l in
    h m r
;;

let rec target = function
  | Leaf x -> [%synt f0]
  | Node (a, l, r) -> [%synt join]
;;

let repr x = x
