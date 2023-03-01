(** @synduce -s 2 -NB *)

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec spec = function
  | Nil -> 0
  | Cons (hd, tl) -> (if hd mod 2 = 0 then hd else 0) + spec tl
;;

let rec target = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0]
  | Concat (x, y) -> [%synt odot]
;;

let rec repr = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (l, y)) x
;;
