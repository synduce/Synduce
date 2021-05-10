(* This is a simple example where the target type is not really recursive,
   but it shows you can simply use it to desribe a different computation for
   the recursion skeleton.
   In this case, the list zipper is just a pair of two lists, the first list
   is reversed and the second list is the rest of the list.
   A zipper is useful to "point" at a particular element in the list or
   simualte a bidirectional traversal.
*)

(* The type of cons-lists *)
type 'a list = Nil | Cons of 'a * 'a list

(* The type of list zippers: a zipper is a pair of two lists (a, b). *)
type 'a list_zipper = Zip of 'a list * 'a list

(* Representation function: reconstructing a list from a zipper. *)
let rec repr = function Zip (a, b) -> conc b (rev a)

and conc x = function Nil -> x | Cons (hd, tl) -> Cons (hd, conc x tl)

and rev = function Nil -> Nil | Cons (hd, tl) -> conc (Cons (hd, Nil)) (rev tl)

(* Reference function: the sum on lists. *)
let rec list_sum = function Nil -> 0 | Cons (hd, tl) -> hd + list_sum tl

(* Target skeleton: the join is only a top-level constant time operation. *)
let rec zipper_sum = function Zip (a, b) -> [%synt join] (list_sum a) (list_sum b)

;;
assert (zipper_sum = repr @@ list_sum)
