(** @synduce --no-lifting *)

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

let count_lt param l =
  let rec f = function
    | Elt x -> if x < param then 1 else 0
    | Cons (hd, tl) -> if hd < param then f tl else 0
  in
  f l
;;

let target param l =
  let rec h = function
    | IElt x -> [%synt base_case] param x
    | ICons (hd, idx, tl) ->
      if hd < param then [%synt rec_stop] idx else [%synt rec_cont] (h tl)
  in
  h l
  [@@requires is_sorted_and_indexed]
;;

assert (target = drop_index_list @@ count_lt)
