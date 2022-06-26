type 'a clist = Single of 'a | Concat of 'a clist * 'a clist

type 'a slist = Elt of 'a | Cons of 'a * 'a slist

let rec spec = function
  | Elt a -> (a, a, true)
  | Cons (hd, tl) ->
      let next, last, iss = spec tl in
      (hd, last, iss && hd < next)

let rec target = function
  | Single a -> [%synt f0] a
  | Concat (y, z) -> [%synt odot] (target y) (target z)

let rec repr = function Single a -> Elt a | Concat (x, y) -> dec y x

and dec l = function Single a -> Cons (a, repr l) | Concat (x, y) -> dec (Concat (y, l)) x
