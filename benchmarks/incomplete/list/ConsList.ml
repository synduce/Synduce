type 'a conslist =
  | Nil
  | Cons of 'a * 'a conslist

type 'a eltlist =
  | Elt of 'a
  | ECons of 'a * 'a eltlist

let rec eltlist_is_sorted_increasing = function
  | Elt x -> true
  | ECons (hd, tl) -> hd <= head tl && eltlist_is_sorted_increasing tl

and head = function
  | Elt x -> x
  | ECons (hd, tl) -> hd
;;

let rec eltlist_is_sorted_decreasing = function
  | Elt x -> true
  | ECons (hd, tl) -> hd >= head tl && eltlist_is_sorted_decreasing tl

and head = function
  | Elt x -> x
  | ECons (hd, tl) -> hd
;;

let rec list_is_sorted_increasing = function
  | Nil -> true
  | Cons (hd, tl) -> aux_incr hd tl

and aux_incr a = function
  | Nil -> true
  | Cons (hd, tl) -> a <= hd && aux_incr hd tl
;;

let rec list_is_sorted_decreasing = function
  | Nil -> true
  | Cons (hd, tl) -> aux_decr hd tl

and aux_decr a = function
  | Nil -> true
  | Cons (hd, tl) -> a >= hd && aux_decr hd tl
;;
