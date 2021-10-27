(** @synduce --no-lifting *)

type list =
  | Elt of int
  | Cons of int * list

type idx_list =
  | IElt of int
  | ICons of int * int * idx_list

let rec is_memo_sum = function
  | IElt x -> true
  | ICons (hd, idx, tl) -> idx = sum tl && is_memo_sum tl

and sum = function
  | IElt x -> x
  | ICons (hd, idx, tl) -> hd + sum tl
;;

let rec drop_sum_list = function
  | IElt x -> Elt x
  | ICons (hd, ids, tl) -> Cons (hd, drop_sum_list tl)
;;

let rec mts = function
  | Elt x -> if x > 0 then x, x, x else 0, 0, 0
  | Cons (hd, tl) ->
    let mts_tl, mps_tl, mss_tl = mts tl in
    let sum_tl = hsum tl in
    let new_mps = max (mps_tl + hd) 0 in
    max mss_tl sum_tl, new_mps, max new_mps mss_tl

and hsum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + hsum tl
;;

let rec target = function
  | IElt x -> [%synt base_case] x
  | ICons (hd, idx, tl) -> [%synt oplus] hd idx (target tl)
;;

assert (target = drop_sum_list @@ mts)
