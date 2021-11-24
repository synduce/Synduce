(** @synduce --no-lifting *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec has_dup = function
  | Nil -> 0
  | Cons (hd, tl) -> if dups_of hd tl > 0 then hd else has_dup tl

and dups_of x = function
  | Nil -> 0
  | Cons (hd, tl) ->
    let i = dups_of x tl in
    if hd = x then i + 1 else i
;;

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

let rec clist_to_list = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_list l1
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let rec target = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt join] (target x) (target y)
;;

assert (target = clist_to_list @@ has_dup)
