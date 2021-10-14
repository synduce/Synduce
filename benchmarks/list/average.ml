(** @synduce --no-lifting  *)

(*
  In this example, CVC4 fails on an almost syntactic definition.
  I have no idea why.
*)

type 'a clist =
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

let rec spec = function
  | Elt a -> 1, a
  | Cons (hd, tl) ->
    let n, avg = spec tl in
    n + 1, ((n * avg) + hd) / (n + 1)
  [@@ensures fun (i, _) -> i > 0]
;;

let rec target = function
  | Single a -> [%synt f0] a
  | Concat (y, z) -> [%synt odot] (target y) (target z)
;;

let rec repr = function
  | Single a -> Elt a
  | Concat (x, y) -> dec y x

and dec l = function
  | Single a -> Cons (a, repr l)
  | Concat (x, y) -> dec (Concat (y, l)) x
;;
