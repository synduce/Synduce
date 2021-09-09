
let xi_0 x11 x12 = x12 = x11 ? 1 : 0

let xi_2 x13 x14 x15 x16 = x14 = x13 ? 1 : x15 = 1 ? 1 : x16 = 1 ? 1 : 0

let xi_1 x17 = x17

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x < a ? xi_1 (g l) : xi_2 x a (g l) (g r)

