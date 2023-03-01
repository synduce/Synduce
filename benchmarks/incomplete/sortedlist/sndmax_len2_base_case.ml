(** @synduce -s 2 -lNB -n 20 *)

(* Target type. *)
type list =
  | Nil
  | Cons2 of int * int * list

(* Origin type. *)
type simp_list =
  | SNil
  | SCons of int * simp_list

let rec repr = function
  | Nil -> SNil
  | Cons2 (hd1, hd2, tl) -> SCons (hd1, SCons (hd2, repr tl))
;;

(* Invariant: the list is sorted in decreasing order, and positive. *)
let rec is_sorted = function
  | Nil -> true
  | Cons2 (hd1, hd2, tl) ->
    hd1 > 0 && hd2 > 0 && hd1 > hd2 && next_is_lt hd2 tl && is_sorted tl

and next_is_lt x = function
  | Nil -> true
  | Cons2 (hd1, hd2, tl) -> x > hd1
;;

(* Specification. *)
let rec spec = function
  | SNil -> 0, 0
  | SCons (hd, tl) ->
    let amax, sec_max = spec tl in
    max hd amax, max sec_max (min hd amax)
  [@@ensures fun (x, y) -> x >= y]
;;

(* Target function. *)
let rec target = function
  | Nil -> [%synt init]
  | Cons2 (hd1, hd2, tl) -> [%synt odot] hd1 hd2
  [@@requires is_sorted]
;;
