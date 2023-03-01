(** @synduce -s 2 -NB *)

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec mps = function
  | Nil -> 0, 0
  | Cons (hd, tl) ->
    let amps, asum = mps tl in
    max (amps + hd) 0, asum + hd
  [@@ensures fun (x, _) -> x >= 0]
;;

let rec clist_to_list = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_list l1
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let rec hom = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0]
  | Concat (x, y) -> [%synt join]
;;

assert (hom = clist_to_list @@ mps)
