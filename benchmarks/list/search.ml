type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec search a = function Nil -> false | Cons (hd, tl) -> if hd = a then true else search a tl

let rec clist_to_list = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_list l1
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x

let rec hsearch a t =
  let main = function
    | CNil -> [%synt s0]
    | Single a -> [%synt f0] a
    | Concat (x, y) -> [%synt odot] (hsum x) (hsum y)
  in
  main t

;;
assert (hsearch = clist_to_list @@ search)
