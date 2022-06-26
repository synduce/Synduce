(** @synduce -s 2 -NB  --no-lifting *)

(* Trees *)
type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

(* Lists *)
type 'a list =
  | Nil
  | Cons of 'a * 'a list

(* Representation function from tree to list *)
let rec repr = function
  | Empty -> Nil
  | Node (a, l, r) -> Cons (a, dec (repr r) l)

and dec li = function
  | Empty -> li
  | Node (a, ll, lr) -> Cons (a, dec (dec li lr) ll)
;;

let target x t =
  let rec g = function
    | Empty -> [%synt s0] x
    | Node (a, l, r) -> [%synt odot] x a (g l) (g r)
  in
  g t
;;

let spec x t =
  let rec f = function
    | Nil -> false
    | Cons (hd, tl) -> if hd = x then true else f tl
  in
  f t
;;
