(** @synduce -s 2 -NB *)

(* Trees *)
type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

(* Lists *)
type 'a list =
  | Nil
  | Cons of 'a * 'a list

(* Representation function from tree to list *)
(* pmrs repr =
    repr Empty -> Nil
    | repr Node(a, l, r) ->  dec Cons(a, repr l) r
    | dec li Empty ->  li
    | dec li Node(a, ll, lr) -> dec Cons(a, dec li ll) lr *)
let rec repr = function
  | Empty -> Nil
  | Node (a, l, r) -> Cons (a, dec (repr l) r)

and dec li = function
  | Empty -> li
  | Node (a, ll, lr) -> Cons (a, dec (dec li ll) lr)
;;

(* Target recursion scheme: homomorphism on trees. *)
let rec target = function
  | Empty -> [%synt s0]
  | Node (a, l, r) -> [%synt join] a (target l) (target r)
;;

(* A simple spec: search on lists. *)
let rec spec = function
  | Nil -> 0, 0
  | Cons (hd, tl) ->
    let x, y = spec tl in
    x + hd, max (x + hd) y
  [@@ensures fun (x, y) -> y > x && y > 0]
;;
