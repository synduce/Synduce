type 'a clist = CSingle of 'a | Concat of 'a clist * 'a clist

type 'a list = Single of 'a | Cons of 'a * 'a list

let rec f = function
  | Single a -> (a, a, true)
  | Cons (hd, tl) -> (fun (last, amax, visible) -> (hd, max amax hd, hd > amax)) (f tl)

let rec h = function CSingle a -> [%synt f_0] a | Concat (x, y) -> [%synt odot] (h x) (h y)

let rec repr = function CSingle a -> Single a | Concat (x, y) -> dec y x

and dec l = function CSingle a -> Cons (a, repr l) | Concat (x, y) -> dec (Concat (l, y)) x

;;
assert (h = repr @@ f)
