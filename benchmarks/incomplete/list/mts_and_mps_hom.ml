(** @synduce -s 2 -NB --no-lifting *)

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec spec = function
  | Nil -> 0, 0, 0
  | Cons (hd, tl) ->
    let sum, mps, mts = spec tl in
    let new_sum = sum + hd in
    let new_mps = max (mps + hd) 0 in
    let new_mts = max mts new_sum in
    new_sum, new_mps, new_mts
  [@@ensures fun (sum, mps, mts) -> mts >= 0 && mps >= 0 && mps >= sum && mts >= sum]
;;

let rec target = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0]
  | Concat (y, z) -> [%synt odot]
;;

let rec repr = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> Nil
  | Single a -> Cons (a, repr l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;
