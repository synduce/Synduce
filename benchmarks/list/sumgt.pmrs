type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let spec c l =
  let rec f = function Nil -> 0 | Cons (hd, tl) -> (if hd > c then hd else 0) + f tl in
  f l

let target c t =
  let rec h = function
    | CNil -> [%synt s0]
    | Single a -> [%synt f0] c a
    | Concat (x, y) -> [%synt odot] c (h x) (h y)
  in
  h t

let rec repr l = c l

and c = function CNil -> Nil | Single a -> Cons (a, Nil) | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (l, y)) x
