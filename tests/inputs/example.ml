type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec sum =
    function
    | Nil -> 0
    | Cons(hd, tl) -> hd + (sum tl)

let rec clist_to_list  =
    function
    | CNil -> Nil
    | Single(a) -> Cons(a, Nil)
    | Concat(x, y) -> dec y x
and dec l1 =
      function
        | CNil -> clist_to_list l1
        | Single(a) -> Cons(a, clist_to_list l1)
        | Concat(x, y) -> dec (Concat(l1, y)) x

(* Target function: synthesize hsum s.t. hsum(x) = sum(clist_to_list(x)) *)
let rec hsum =
    function
    | CNil          -> s0
    | Single(a)    -> f0 a
    | Concat(x, y) -> odot (hsum x) (hsum y)
    [@defining s0 f0 odot]
    [@equiv sum clist_to_list ]
