(** @synduce -NB --no-lifting *)

type list =
  | Nil
  | Cons of int * list

type clist =
  | Empty
  | Single of int
  | Concat of clist * clist

let rec repr = function
  | Empty -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | Empty -> repr l1
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
  | Empty -> [%synt f0]
  | Single x -> [%synt f1] x
  | Concat (x, y) -> [%synt f2] (target x)
;;
