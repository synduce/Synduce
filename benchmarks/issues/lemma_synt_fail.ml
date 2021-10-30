(** @synduce --no-lifting *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

type 'a clist =
  | Emp
  | Single of 'a
  | Concat of 'a clist * 'a clist

let rec clist_to_list = function
  | Emp -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | Emp -> Nil
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

(** Predicate asserting that a concat-list is partitioned.  *)
let rec is_partitioned = function
  | Emp -> true
  | Single x -> true
  | Concat (x, y) -> gt0 x && lt0 y

and lt0 = function
  | Emp -> true
  | Single x -> x < 0
  | Concat (x, y) -> lt0 x && lt0 y

and gt0 = function
  | Emp -> true
  | Single x -> x > 0
  | Concat (x, y) -> gt0 x && gt0 y
;;

let rec spec = function
  | Nil -> 0
  | Cons (hd, tl) ->
    let mps = spec tl in
    max (mps + hd) 0
;;

let rec target = function
  | Emp -> [%synt init]
  | Single x -> [%synt base_case] x
  | Concat (l, r) -> [%synt join] (target l)
  [@@requires is_partitioned]
;;

assert (target = clist_to_list @@ spec)
