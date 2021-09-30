(** @synduce --no-lifting  *)

type 'a clist = Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Elt of 'a | Cons of 'a * 'a list

let rec spec = function 
  | Elt a -> (a, true, a)
  | Cons (hd, tl) ->
    let an, bn, head_l = spec tl in 
    (hd && an, ((not hd) || an) && bn, head_l)
  

let rec target =
  function
  | Single a -> [%synt f0] a
  | Concat (y, z) -> [%synt odot] (target y) (target z)

let rec repr = function Single a -> Elt a | Concat (x, y) -> dec y x

and dec l = function
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
