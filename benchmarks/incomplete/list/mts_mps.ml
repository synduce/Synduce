type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec f = function
  | Nil -> (0, 0, 0)
  | Cons (hd, tl) ->
      (fun (sum, mps, mts) -> (sum + hd, max (mps + hd) 0, max mts (sum + hd))) (f tl)
  [@@ensures fun (sum, mps, mts) -> mts >= 0 && mps >= 0 && mps >= sum && mts >= sum]

let rec h = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (y, z) -> [%synt odot] (h y) (h z)

let rec repr = function CNil -> Nil | Single a -> Cons (a, Nil) | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
