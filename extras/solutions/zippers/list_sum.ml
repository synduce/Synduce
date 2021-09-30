
let s0  = 0

let op x16 x17 = x17 + x16

let join x18 x19 = x18 + x19

let rec zipper_sum _x =
          match _x with Zip(a, b) -> join (rev_list_sum a) (list_sum b)
and rev_list_sum =
  function Nil -> s0 | Cons(hd, tl) -> op hd (rev_list_sum tl)

