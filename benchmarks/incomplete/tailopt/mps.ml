(** @synduce -s 2 -NB *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec spec = function
  | Nil -> 0, 0
  | Cons (hd, tl) ->
    let sum, mps = spec tl in
    sum + hd, max (mps + hd) 0
;;

let rec target l =
  let init = [%synt s0] in
  f init l

and f s = function
  | Nil -> s
  | Cons (hd, tl) ->
    let acc = [%synt oplus] in
    f acc tl
;;
