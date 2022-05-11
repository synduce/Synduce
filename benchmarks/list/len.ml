type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec f = function
  | Nil -> 0
  | Cons (hd, tl) -> 1 + f tl
;;

let rec h = function
  | CNil -> [%synt s_0]
  | Single a -> [%synt f_0] a
  | Concat (x, y) -> [%synt odot] (h x) (h y)
;;

let rec repr = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;

assert (h = repr @@ f)
