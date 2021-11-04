(* Origin type. *)
type list =
  | Nil
  | Cons of int * list

(* Invariant: the list is sorted in decreasing order. *)
let rec sorted_and_lt2 l = is_sorted l && len l >= 2

and len = function
  | Nil -> 0
  | Cons (hd, tl) -> 1 + len tl

and is_sorted = function
  | Nil -> true
  | Cons (hd, tl) -> hd > 0 && next_is_lt hd tl && is_sorted tl

and next_is_lt x = function
  | Nil -> true
  | Cons (hd, tl) -> x > hd
;;

(* Specification. *)
let rec spec = function
  | Nil -> 0, 0
  | Cons (hd, tl) ->
    let amax, sec_max = spec tl in
    max hd amax, max sec_max (min hd amax)
  [@@ensures fun (x, y) -> x >= y]
;;

(* Target function. *)
let rec target = function
  | Nil -> [%synt init]
  | Cons (hd, tl) -> [%synt ffst] hd (next tl)
  [@@requires sorted_and_lt2]

and next = function
  | Nil -> [%synt x]
  | Cons (hd, tl) -> [%synt fsnd] hd
;;
