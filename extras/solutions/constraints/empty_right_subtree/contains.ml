
let xi_0  = 0

let xi_1 x17 x18 = x18 = x17 ? 1 : 0

let xi_2 x19 x20 x21 = x19 = x20 ? 1 : x21

let rec g =
  function Nil(a) -> xi_0 | Leaf(a) -> xi_1 x a
  | Node(a, l, r) -> xi_2 x a (g l)

