
let op x4 x5 = (- x4) + x5

let s0  = - 0

let join x6 x7 = (- x6) + x7

let rec zipper_sum _x =
          match _x with Zip(a, b) -> join (rev_list_sum a) (list_sum b)
and rev_list_sum =
  function Nil -> s0 | Cons(hd, tl) -> op hd (rev_list_sum tl)

