(** @synduce -I ConsList.ml -s 2 --no-lifting *)

open ConsList

(*
  This benchmark has two solutions:
  - the naive one, which doesn't take advantage of the list being sorted.
  - the optimal one, which only takes contant time.
  The algorithm reduces the search space from 8 configurations to 4.
*)

let rec spec = function
  | Elt x -> x
  | ECons (hd, tl) -> min hd (spec tl)
;;

let rec target = function
  | Elt x -> [%synt xi_0]
  | ECons (hd, tl) -> [%synt xi_1]
  [@@requires eltlist_is_sorted_increasing]
;;
