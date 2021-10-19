type tree =
  | Nil
  | Node of int * tree * tree

type htree =
  | HNil
  | HNode of int * int * htree * htree

let rec repr = function
  | Nil -> HNil
  | Node (a, l, r) -> HNode (a, 1 + max (height l) (height r), repr l, repr r)

and height = function
  | Nil -> 0
  | Node (_, l, r) -> 1 + max (height l) (height r)
;;

let rec spec t = f 0 t

and f s = function
  | HNil -> s
  | HNode (a, h, l, r) ->
    let sum = f s l in
    f (max h (sum + a)) r
;;

let rec target = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> [%synt join] a (target l) (target r)
;;

(** If no assertion is used, the tool search for function names repr, spec and target. *)
