type 'a clist = CNil | Single of 'a | Concat of 'a clist * 'a clist

type 'a list = Nil | Cons of 'a * 'a list

let rec sum [@atr ensures (fun x -> true)] =
    function
    | Nil -> 0
    | Cons(hd, tl) -> hd + (sum tl)

let rec clist_to_list  =
    function
    | CNil -> Nil
    | Single(a) -> Cons(a, Nil)
    | Concat(x, y) -> dec y x
and dec l1 x =
      match x with
        | CNil -> clist_to_list l1
        | Single(a) -> Cons(a, clist_to_list l1)
        | Concat(x, y) -> dec (Concat(l1, y)) x

(* Target function: synthesize hsum s.t. hsum(x) = sum(clist_to_list(x)) *)
let rec hsum [@atr equiv (clist_to_list |> sum)]=
    function
    | CNil          -> [%atr s0]
    | Single(a)    -> [%atr f0] a
    | Concat(x, y) -> [%atr join] (hsum x) (hsum y)