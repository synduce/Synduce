(** @synduce *)

type ilist = INil | ICons of int * ilist

type blist = BNil | BCons of bool * blist

type silist = SiElt of int | SiCons of int * silist 

type sblist = SbElt of bool | SbCons of bool * sblist 

type clist = CNil | CElt of int | CConcat of clist * clist 



let rec clist_to_ilist = function CNil -> INil | CElt a -> ICons (a, INil) | CConcat (x, y) -> dec1 y x

and dec1 l = function
  | CNil -> clist_to_ilist l
  | CElt a -> ICons (a, clist_to_ilist l)
  | CConcat (x, y) -> dec1 (CConcat (y, l)) x

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
  | INil -> [%synt c1]
  | ICons(hd, tl) -> [%synt f] hd (target_aux tl)
and target_aux =
  function 
  | INil -> false
  | ICons(hd, tl) -> [%synt g] hd (target tl)
;;
assert (target = ilist_to_blist @@ pred1)
