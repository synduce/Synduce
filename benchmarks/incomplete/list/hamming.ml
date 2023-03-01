(** @synduce -s 2 -NB *)

type 'a zipclist =
  | CNil
  | Single of 'a * 'a
  | Concat of 'a zipclist * 'a zipclist

type 'a ziplist =
  | Nil
  | Cons of 'a * 'a * 'a ziplist

let rec spec = function
  | Nil -> 0
  | Cons (hd1, hd2, tl) -> (if hd1 = hd2 then 0 else 1) + spec tl
;;

let rec target = function
  | CNil -> [%synt s0]
  | Single (a1, a2) -> [%synt f0]
  | Concat (x, y) -> [%synt odot]
;;

let rec repr = function
  | CNil -> Nil
  | Single (a, b) -> Cons (a, b, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single (a, b) -> Cons (a, b, repr l)
  | Concat (x, y) -> dec (Concat (l, y)) x
;;
