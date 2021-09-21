
let xi_2 x15 x16 x17 x18 = x16 = x15 ? 1 : x17 = 1 ? 1 : x18 = 1 ? 1 : 0

let xi_0 x13 x14 = x14 = x13 ? 1 : 0

let xi_1 x19 = x19

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x < a ? xi_1 (g l) : xi_2 x a (g l) (g r)

