
let xi_0 x6 x7 = x7 < x6 ? 1 : 0

let xi_1 x8 x9 = (1 + x8) + x9

let xi_2 x10 = x10

let rec g  =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> a < x ? xi_1 (g l) (g r) : xi_2 (g l)

