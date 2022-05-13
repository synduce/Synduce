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

let rec clist_to_list = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_list l1
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let twosum goal t =
  let rec f1 = function
    | Nil -> -1, -1
    | Cons (hd, tl) ->
      let ok, value = f2 hd tl in
      if ok then hd, value else f1 tl
  and f2 hd = function
    | Nil -> false, -1
    | Cons (x, tl) -> if hd + x = goal then true, hd else f2 hd tl
  in
  f1 t
;;

let target x t =
  let rec h = function
    | Nil -> [%synt f] x
    | Cons (hd, tl) -> [%synt join] hd (h tl)
  in
  h t
  [@@requires list_all_positive]
;;

assert (target = twosum)
