(** @synduce -s 2 -NB -n 30 *)

(* Target type. *)
type clist =
  | Empty
  | E of int
  | Cat of clist * clist

(* Origin type. *)
type list =
  | Nil
  | Cons of int * list

(* Invariant: all elements are positive. *)
let rec allpos = function
  | Empty -> true
  | E x -> x > 0
  | Cat (x, y) -> allpos x && allpos y
;;

(* Representation function *)
let rec repr = function
  | Empty -> Nil
  | E x -> Cons (x, Nil)
  | Cat (x, y) -> dec y x

and dec l = function
  | Empty -> repr l
  | E a -> Cons (a, repr l)
  | Cat (x, y) -> dec (Cat (y, l)) x
;;

(* Invariant: the list is sorted in decreasing order. *)
let rec concat_list_sorted l = is_sorted (repr l)

and is_sorted = function
  | Nil -> true
  | Cons (hd, tl) -> lt_head hd tl && is_sorted tl

and lt_head x = function
  | Nil -> true
  | Cons (hd, tl) -> x >= hd
;;

(* Specification. *)
let rec spec = function
  | Nil -> 0, 0
  | Cons (hd, tl) ->
    let amax, sec_max = spec tl in
    max hd amax, max sec_max (min hd amax)
;;

(* Target function. *)
let rec target = function
  | Empty -> [%synt init]
  | E a -> [%synt base_case] a
  | Cat (x, y) -> [%synt odot] (target x) (target y)
  [@@requires concat_list_sorted]
;;
