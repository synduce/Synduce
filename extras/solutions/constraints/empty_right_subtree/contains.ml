
let xi_0  = 0

let xi_1 x40 x41 = x41 = x40 ? 1 : 0

let xi_2 x42 x43 x44 = x42 = x43 ? 1 : x44

let rec g =
  function Nil(a) -> xi_0 | Leaf(a) -> xi_1 x a
  | Node(a, l, r) -> xi_2 x a (g l)

