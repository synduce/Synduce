(** @synduce --no-lifting -NB -n 30  *)

type list =
  | Elt of int
  | Cons of int * list

type idx_list =
  | IElt of int
  | ICons of int * int * idx_list

let rec is_sorted = function
  | IElt x -> true
  | ICons (hd, idx, tl) -> aux hd tl

and aux prev = function
  | IElt x -> prev < x
  | ICons (hd, idx, tl) -> prev < hd && aux hd tl
;;

let rec is_sorted_and_indexed = function
  | IElt x -> true
  | ICons (hd, idx, tl) -> aux hd tl && idx = 1 + len2 tl && is_sorted_and_indexed tl

and aux prev = function
  | IElt x -> prev > x
  | ICons (hd, idx, tl) -> prev > hd && aux hd tl

and len2 = function
  | IElt x -> 1
  | ICons (hd, idx, tl) -> 1 + len2 tl
;;

let rec index_list = function
  | Elt x -> IElt x
  | Cons (hd, tl) -> ICons (hd, length tl, index_list tl)

and length = function
  | Elt x -> 1
  | Cons (x, tl) -> 1 + length tl
;;

let rec drop_index_list = function
  | IElt x -> Elt x
  | ICons (hd, ids, tl) -> Cons (hd, drop_index_list tl)
;;

let rec count_lt0 = function
  | Elt x -> if x < 0 then 1 else 0
  | Cons (hd, tl) -> (if hd < 0 then 1 else 0) + count_lt0 tl

and len = function
  | Elt x -> 1
  | Cons (hd, tl) -> 1 + len tl
;;

let rec h = function
  | IElt x -> [%synt base_case] x
  | ICons (hd, idx, tl) ->
    if hd < 0 then [%synt rec_stop] hd idx else [%synt rec_cont] hd (h tl)
  [@@requires is_sorted_and_indexed]
;;

assert (h = drop_index_list @@ count_lt0)
