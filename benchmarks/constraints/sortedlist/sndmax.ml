(** @synduce --no-lifting -NB -n 30 -l *)

(* Target type. *)
type clist =
  | Empty
  | E of int * int
  | Cat of clist * clist

(* Origin type. *)
type list =
  | Nil
  | Cons of int * list

(* Invariant: all elements are positive. *)
let rec allpos = function
  | Empty -> true
  | E (x, y) -> x > 0 && y > 0
  | Cat (x, y) -> allpos x && allpos y
;;

(* Representation function *)
let rec repr = function
  | Empty -> Nil
  | E (x, y) -> Cons (x, Cons (y, Nil))
  | Cat (x, y) -> dec y x

and dec l = function
  | Empty -> repr l
  | E (a, b) -> Cons (a, Cons (b, repr l))
  | Cat (x, y) -> dec (Cat (y, l)) x
;;

(* Invariant: the list is sorted in decreasing order. *)
let rec concat_list_sorted l = is_sorted (repr l)

and is_sorted = function
  | Nil -> true
  | Cons (hd, tl) -> next_is_lt hd tl && is_sorted tl

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
  | Empty -> [%synt init]
  | E (a, b) -> [%synt base_case] (a, b)
  | Cat (x, y) -> [%synt odot] (target x)
  [@@requires concat_list_sorted]
;;
