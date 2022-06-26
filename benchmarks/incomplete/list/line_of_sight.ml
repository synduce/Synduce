(** @synduce -s 2 -NB *)

type 'a clist =
  | Elt of 'a
  | Cat of 'a clist * 'a clist

type 'a list =
  | Single of 'a
  | Cons of 'a * 'a list

let rec f = function
  | Single a -> a, a, true
  | Cons (hd, tl) -> (fun (last, amax, visible) -> hd, max amax hd, hd > amax) (f tl)
;;

let rec h = function
  | Elt a -> [%synt f_0] a
  | Cat (x, y) -> [%synt odot] (h x) (h y)
;;

let rec repr = function
  | Elt a -> Single a
  | Cat (x, y) -> dec y x

and dec l = function
  | Elt a -> Cons (a, repr l)
  | Cat (x, y) -> dec (Cat (y, l)) x
;;

assert (h = repr @@ f)
