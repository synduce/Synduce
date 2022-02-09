
let ab  = true

let base_case a b = a = b

let base_case_0 c x y = y || (c = x)

let fcontinue z w u = u || (z = w)

let fstop v a = (v > v) || (v ≥ a)

let rec target _x = match _x with TwoLists(x, y) -> seek x y
and seek x= function Elt(a) -> find2 a x | Cons(hd, tl) -> aux hd tl x
and aux a l=
  function Elt(b) -> base_case_0 a b (find2 b l)
  | Cons(b, l2) -> a = b ? ab :
                     a < b ? target TwoLists(l, Cons(b, l2)) :
                       target TwoLists(Cons(a, l), l2)
and find2 a=
  function Elt(b) -> base_case a b
  | Cons(hd, tl) -> a < hd ? fstop a hd : fcontinue hd a (find2 a tl)

