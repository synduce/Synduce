(** @synduce --no-lifting *)

type list =
  | Nil
  | Cons of int * list

let rec list_all_positive = function
  | Nil -> true
  | Cons (hd, tl) -> hd > 0 && list_all_positive tl
;;

type clist =
  | CNil
  | Single of int
  | Concat of clist * clist

let rec all_positive = function
  | CNil -> true
  | Single x -> x >= 0
  | Concat (a, b) -> all_positive a && all_positive b
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

let rec mps_list = function
  | Nil -> 0
  | Cons (hd, tl) -> max (mps_list tl + hd) 0
;;

let rec h = function
  | CNil -> [%synt base_case]
  | Single x -> [%synt init] x
  | Concat (x, y) -> [%synt join] (h x) (h y)
  [@@requires all_positive]
;;

assert (h = clist_to_list @@ mps_list)
