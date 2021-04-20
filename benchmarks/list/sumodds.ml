type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec spec l = f l

and f = function Nil -> 0 | Cons (hd, tl) -> (if hd mod 2 = 1 then hd else 0) + f tl

let rec target l = h l

and h = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt odot] (h x) (h y)

let rec repr l = c l

and c = function CNil -> Nil | Single a -> Cons (a, Nil) | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (l, y)) x
