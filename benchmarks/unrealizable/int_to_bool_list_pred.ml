(** @synduce *)

type ilist = INil | ICons of int * ilist

type blist = BNil | BCons of bool * blist

let rec ilist_to_blist = function 
  | INil -> BNil
  | ICons(hd, tl) -> BCons(hd = 1, ilist_to_blist tl)

let rec pred1 = function
  | BCons(hd, tl) -> hd && pred1_aux tl 
  | BNil -> true 
  and pred1_aux = function
  | BNil  -> false 
  | BCons(hd, tl) -> (not hd) && pred1 tl

let rec target = 
  function 
  | INil -> [%synt c]
  | ICons(hd, tl) -> [%synt f] hd (target tl)
;;
assert (target = ilist_to_blist @@ pred1)
