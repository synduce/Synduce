(** @synduce -I ConsList.ml *)

open ConsList

let spec x l =
  let rec f = function
    | Elt a -> Cons (x, Elt a)
    | Cons (hd, tl) -> Cons (x, Cons (hd, tl))
  in
  f l
;;

let target x l =
  let rec g = function
    | Elt a -> [%synt f0] x a
    | Cons (hd, tl) -> [%synt odot] x hd (g tl)
  in
  g l
;;
