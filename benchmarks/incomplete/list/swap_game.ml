(** @synduce -NB --no-lifting -s 2 -I ConsList.ml,ConcatList.ml *)

open ConsList
open ConcatList
(* This benchmark has no solution, even with a different configuration. *)

let rec repr = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> repr l1
  | Single a -> Cons (a, repr l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let rec spec = function
  | Nil -> 0, 1
  | Cons (hd, tl) ->
    let a, b = spec tl in
    hd, a
;;

let rec target = function
  | CNil -> [%synt f0]
  | Single x -> [%synt f1] x
  | Concat (x, y) -> [%synt f2] (target x) (target y)
;;
