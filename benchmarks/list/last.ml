(* The type of non-empty concat-lists *)
type clist =
  | Single of int
  | Concat of clist * clist

(* The type of non-empty lists *)
type list =
  | Elt of int
  | Cons of int * list

let rec spec = function
  | Elt a -> a
  | Cons (hd, tl) -> spec tl
;;

let rec target = function
  | Single a -> [%synt f_0] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)
;;

let rec repr = function
  | Single a -> Elt a
  | Concat (x, y) -> dec y x

and dec l = function
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;
