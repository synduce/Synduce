(** @synduce -s 2 -NB *)

type two_list = TwoLists of list * list

and list =
  | Elt of int
  | Cons of int * list

(* Invariant: sorted in decreasing order. *)
let rec is_sorted = function
  | TwoLists (x, y) -> is_sorted_l x && is_sorted_l y

and is_sorted_l = function
  | Elt x -> true
  | Cons (hd, tl) -> aux hd tl

and aux prev = function
  | Elt x -> prev >= x
  | Cons (hd, tl) -> prev >= hd && aux hd tl
;;

(* Reference function in quadratic time. *)
let rec is_intersection_nonempty = function
  | TwoLists (x, y) -> seek_common_elt x y

and seek_common_elt y = function
  | Elt a -> find a y
  | Cons (hd, tl) -> find hd y || is_intersection_nonempty (TwoLists (tl, y))

and find a = function
  | Elt b -> b = a
  | Cons (hd, tl) -> hd = a || find a tl
;;

(* Target assumes that lsits are sorted and takes advantage of it. *)
let rec target = function
  | TwoLists (x, y) -> seek x y
  [@@requires is_sorted]

and seek y = function
  | Elt a -> find2 a y
  | Cons (hd, tl) -> [%synt combine] (find2 hd y) (target (TwoLists (tl, y)))

and find2 a = function
  | Elt b -> [%synt base_case] a b
  | Cons (hd, tl) ->
    if a > hd then [%synt fstop] a hd else [%synt fcontinue] hd a (find2 a tl)
;;

assert (target = is_intersection_nonempty)
