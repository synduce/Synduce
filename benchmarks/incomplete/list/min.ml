(** @synduce -s 2 -NB *)

type 'a clist =
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

let rec spec = function
  | Elt a -> a
  | Cons (hd, tl) -> min hd (spec tl)
;;

let rec repr = function
  | Single a -> Elt a
  | Concat (x, y) -> dec y x

and dec l1 = function
  | Single a -> Cons (a, repr l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let rec target = function
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)
;;
