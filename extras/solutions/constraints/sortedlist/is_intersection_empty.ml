
let base_case x0 x1 = x1 = x0

let fcontinue x2 x3 x4 = (x3 = x2) || x4

let fstop x5 x6 = x6 > x5

let combine x7 x8 = x8 || x7

let rec target _x = match _x with TwoLists(x, y) -> seek x y
and seek y=
  function Elt(a) -> find2 a y
  | Cons(hd, tl) -> combine (find2 hd y) (target TwoLists(tl, y))
and find2 a=
  function Elt(b) -> base_case a b
  | Cons(hd, tl) -> a > hd ? fstop a hd : fcontinue hd a (find2 a tl)

