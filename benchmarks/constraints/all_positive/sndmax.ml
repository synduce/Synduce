(** @synduce -NB -n 20 -l --no-lifting *)

type clist =
  | Empty
  | Elt of int
  | Concat of clist * clist

type list =
  | Nil
  | Cons of int * list

(* Invariant *)
let rec allpos = function
  | Empty -> true
  | Elt x -> x > 0
  | Concat (x, y) -> allpos x && allpos y
;;

let rec spec = function
  | Nil -> 0, 0
  | Cons (hd, tl) ->
    let amax, sec_max = spec tl in
    max hd amax, max sec_max (min hd amax)
  [@@ensures fun (x, y) -> x >= y && y >= 0]
;;

let rec target = function
  | Empty -> [%synt init]
  | Elt a -> [%synt base_case] a
  | Concat (x, y) -> [%synt odot] (target x) (target y)
  [@@requires allpos]
;;

let rec repr = function
  | Empty -> Nil
  | Elt x -> Cons (x, Nil)
  | Concat (x, y) -> dec y x

and dec l = function
  | Empty -> repr l
  | Elt a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;
