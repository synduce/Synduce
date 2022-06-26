(** @synduce -s 2 -NB *)

type 'a clist =
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

let rec maxcount = function
  | Elt a -> a, 1
  | Cons (hd, tl) ->
    let amax, acnt = maxcount tl in
    max amax hd, if hd > amax then 1 else acnt + if hd = amax then 1 else 0
;;

let rec clist_to_list = function
  | Single a -> Elt a
  | Concat (x, y) -> dec y x

and dec l1 = function
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let rec hom = function
  | Single a -> [%synt f0]
  | Concat (x, y) -> [%synt join]
;;

assert (hom = clist_to_list @@ maxcount)
