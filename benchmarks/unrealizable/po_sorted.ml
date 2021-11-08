(** @synduce --no-lifting *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec spec = function
  | Leaf x -> x, true
  | Node (a, l, r) ->
    let lh, lis = spec l in
    let rh, ris = h (lh, lis) r in
    a, a < rh && lis && ris

and h s = function
  | Leaf x ->
    let y, b = s in
    x, b && x < y
  | Node (a, l, r) ->
    let lh, lis = h s l in
    let rh, ris = h (lh, lis) r in
    a, a < rh && lis && ris
;;

let rec target = function
  | Leaf a -> a, true
  | Node (a, l, r) -> [%synt join] a (target l) (target r)
;;

let repr x = x
