(** @synduce -s 2 -NB *)

type 'a tree =
  | Node of 'a * 'a tree * 'a tree
  | Leaf of 'a

let rec spec = function
  | Leaf x -> x, x
  | Node (a, l, r) ->
    let amin, amax = g (a, a) l in
    g (amin, amax) r

and g s = function
  | Leaf x ->
    let amin, amax = s in
    min amin x, max amax x
  | Node (a, l, r) ->
    let amin, amax = s in
    g (g (min amin a, max amax a) l) r
;;

let rec target = function
  | Leaf x -> [%synt f0]
  | Node (a, l, r) -> [%synt join]
;;

let repr x = x
