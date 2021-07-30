
let xi_0 x14 x15 = x15 < x14 ? 1 : 0

let xi_1 x16 x17 = (1 + x16) + x17

let xi_2 x18 = x18

let rec g  =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> a < x ? xi_1 (g l) (g r) : xi_2 (g l)

