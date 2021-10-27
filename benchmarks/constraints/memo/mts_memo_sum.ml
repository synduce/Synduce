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
  | Elt x -> if x > 0 then x else 0
  | Cons (hd, tl) ->
    let mts_tl = mts tl in
    max mts_tl (hd + hsum tl)

and hsum = function
  | Elt x -> x
  | Cons (hd, tl) -> hd + hsum tl
;;

let rec target = function
  | IElt x -> [%synt base_case] x
  | ICons (hd, idx, tl) -> [%synt oplus] hd idx (target tl)
  [@@requires is_memo_sum]
;;

assert (target = drop_sum_list @@ mts)
