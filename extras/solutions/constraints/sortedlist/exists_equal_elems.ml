
let base_case a b = a = b

let fcontinue c x y = y || (c = x)

let fstop z w = (z > z) || (w ≥ z)

let rec target _x = match _x with TwoLists(x, y) -> seek x y
and seek y= function Elt(a) -> find2 a y | Cons(hd, tl) -> aux hd tl y
and aux a l=
  function Elt(b) -> a = b
  | Cons(b, l2) -> a < b ? target TwoLists(Cons(b, l2), l) :
                     target TwoLists(l2, Cons(a, l))
and find2 a=
  function Elt(b) -> base_case a b
  | Cons(hd, tl) -> a > hd ? fstop a hd : fcontinue hd a (find2 a tl)

