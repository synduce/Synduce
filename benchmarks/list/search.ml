type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec clist_to_list = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_list l1
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x

let search a t =
  let rec f = function Nil -> false | Cons (hd, tl) -> if hd = a then true else f tl in
  f t

let hsearch a t =
  let rec h = function
    | CNil -> [%synt s0]
    | Single i -> [%synt f0] a i
    | Concat (x, y) -> [%synt odot] (h x) (h y)
  in
  h t

;;
assert (hsearch = clist_to_list @@ search)
