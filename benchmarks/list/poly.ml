type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec spec x l = f x l

and f x = function Nil -> (0, 1) | Cons (hd, tl) -> (fun (s, m) -> (s + (hd * m), x * m)) (f x tl)

let rec target x t = h x t

and h x = function
  | CNil -> [%synt s0]
  | Single a -> (a, x)
  | Concat (y, z) -> [%synt odot] x (h x y) (h x z)

let rec repr = function CNil -> Nil | Single a -> Cons (a, Nil) | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
