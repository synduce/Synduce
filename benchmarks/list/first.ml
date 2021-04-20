(* The type of non-empty concat-lists *)
type clist = Single of int | Concat of clist * clist

(* The type of non-empty lists *)
type list = Elt of int | Cons of int * list

let rec spec = function Elt a -> a | Cons (hd, tl) -> hd

let rec target = function
  | Single a -> [%synt f_0] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)

let rec repr = function Single a -> Elt a | Concat (l1, l2) -> dec l2 l1

and dec l2 = function
  | Single a -> Cons (a, repr l2)
  | Concat (l11, l12) -> dec (Concat (l12, l2)) l11
