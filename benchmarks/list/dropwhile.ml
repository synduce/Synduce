(** @synduce --no-lifting  *)

type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec spec = function 
  | Nil -> (0, 0, true)
  | Cons (hd, tl) ->
    let i, pos, is_first_pos = spec tl in 
    let cond = not hd && is_first_pos in 
    (i+1, (if cond then i+1 else pos), cond)
  

let rec target =
  function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (y, z) -> [%synt odot] (target y) (target z)

let rec repr = function CNil -> Nil | Single a -> Cons (a, Nil) | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
