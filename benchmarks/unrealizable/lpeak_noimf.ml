(** @synduce -NB -n 20 --no-lifting *)

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

(* This is the same benchmark as list/lpeak.ml but the ensures has been removed. *)
let rec lpeak = function
  | Nil -> 0, true, 0, 0
  | Cons (hd, tl) ->
    let cnt, aux1, aux2, lp = lpeak tl in
    let cnt2 = if hd > 0 then cnt + hd else 0 in
    let aux12 = aux1 && hd >= 0 in
    cnt2, aux12, (if aux12 then aux2 + hd else aux2), max lp cnt2
;;

let rec target = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)
;;

let rec repr = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | CNil -> repr l
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;

assert (target = repr @@ lpeak)
