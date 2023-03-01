(** @synduce -s 2 -NB -n 30 -l *)

(* Target type. *)
type clist =
  | Empty
  | Elt of int
  | Concat of clist * clist

(* Origin type. *)
type list =
  | Nil
  | Cons of int * list

(* Invariant: all elements are positive. *)
let rec allpos = function
  | Empty -> true
  | Elt x -> x > 0
  | Concat (x, y) -> allpos x && allpos y
;;

(* Representation function *)
let rec repr = function
  | Empty -> Nil
  | Elt x -> Cons (x, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | Empty -> repr l
  | Elt a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
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
  [@@ensures fun (x, y) -> x >= y]
;;

(* Target function. *)
let rec target = function
  | Empty -> [%synt init]
  | Elt a -> [%synt base_case]
  | Concat (x, y) -> [%synt odot]
  [@@requires concat_list_sorted]
;;
