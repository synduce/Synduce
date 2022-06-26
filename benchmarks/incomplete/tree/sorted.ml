(** @synduce -s 2 -NB *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec f = function
  | Leaf x -> x, true
  | Node (a, l, r) ->
    let lh, lis = f l in
    let rh, ris = f r in
    a, (lh < a && a < rh) && lis && ris
;;

let rec h = function
  | Leaf a -> [%synt f0] a
  | Node (a, l, r) -> [%synt join] a (h l) (h r)
;;

assert (h = f)
