
let xi_0  = 0

let xi_1 x10 x11 = x11 = x10 ? 1 : 0

let xi_2 x12 x13 x14 = x12 = x13 ? 1 : x14

let rec g  =
  function Nil(a) -> xi_0 | Leaf(a) -> xi_1 x a
  | Node(a, l, r) -> xi_2 x a (g l)

