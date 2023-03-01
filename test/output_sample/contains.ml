
let xi_0 v6 a = v6 = a ? 1 : 0

let xi_1 b7 c7 x7 = x7

let xi_2 y7 z7 w7 = y7 = z7 ? 1 : w7

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x < a ? xi_1 x a (g l) : xi_2 x a (g r)

