type 'a clist =
  | CNil
  | Single of 'a
  | Concat of 'a clist * 'a clist

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec sum = function
  | Nil -> 0
  | Cons (hd, tl) -> hd + sum tl
  [@@ensures fun x -> x > 0]
;;

let rec clist_to_list = function
  | CNil -> Nil
  | Single a -> Cons (a, Nil)
  | Concat (x, y) -> dec y x

and dec l1 = function
  | CNil -> clist_to_list l1
  | Single a -> Cons (a, clist_to_list l1)
  | Concat (x, y) -> dec (Concat (l1, y)) x
;;

let rec allpos = function
  | CNil -> true
  | Single a -> a > 0
  | Concat (x, y) -> allpos x && allpos y
;;

(* Target function: synthesize hsum s.t. hsum(x) = sum(clist_to_list(x)) *)
let rec hsum = function
  | CNil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Concat (x, y) -> [%synt odot] (hsum x) (hsum y)
  [@@requires allpos]
;;
