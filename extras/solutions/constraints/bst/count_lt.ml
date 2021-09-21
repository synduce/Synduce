
let xi_1 x10 x11 = (1 + x10) + x11

let xi_0 x8 x9 = x9 < x8 ? 1 : 0

let xi_2 x12 = x12

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> a < x ? xi_1 (g l) (g r) : xi_2 (g l)

