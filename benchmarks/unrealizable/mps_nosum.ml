(** @synduce --no-lifting *)

type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

(*
   The maximum prefix sum without the sum auxiliary.
   The function needs to be lifted for the problem to
   be solvable.
   See list/mps.ml for the version that includes the lifting.
*)
let rec mps = function
  | Nil -> 0
  | Cons (hd, tl) ->
    let _mps = mps tl in
    max (_mps + hd) 0
[@@ensures fun x -> x >= 0]
;;

let rec clist_to_list = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_list l1
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (y, l1)) x
;;

let rec hom = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt join] (hom x) (hom y)
;;

assert (hom = clist_to_list @@ mps)
