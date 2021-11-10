(** @synduce -NB  --no-lifting *)

type list =
  | Cons of int * int * list
  | Elt of int * int

let rec sorted_by_start = function
  | Elt (x, y) -> true
  | Cons (a, b, l) -> a < b && a < head l && sorted_by_start l

and head = function
  | Elt (a, b) -> a
  | Cons (a, b, l) -> a
;;

let rec spec = function
  | Elt (a, b) -> false, a, b
  | Cons (a, b, l) ->
    let w, x, y = spec l in
    interwith a b l || w, a, b

and interwith a b = function
  | Elt (d, c) -> (not (c < a)) && not (b < d)
  | Cons (d, c, l) -> ((not (c < a)) && not (b < d)) || interwith a b l
;;

let rec target = function
  | Elt (a, b) -> [%synt f0] a b
  | Cons (a, b, l) -> [%synt f1] a b (target l)
  [@@requires sorted_by_start]
;;
