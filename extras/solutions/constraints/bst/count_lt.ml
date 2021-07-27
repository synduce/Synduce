
let xi_0 x3 x4 = x4 < x3 ? 1 : 0

let xi_1 x5 x6 = (1 + x5) + x6

let xi_2 x7 = x7

let rec g  =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> a < x ? xi_1 (g l) (g r) : xi_2 (g l)

