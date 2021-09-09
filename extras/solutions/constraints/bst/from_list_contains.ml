
let xi_0 x12 x13 = x13 = x12

let xi_2 x14 x15 x16 x17 = ((x15 = x14) || x17) || x16

let xi_1 x18 = x18

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x < a ? xi_1 (g l) : xi_2 x a (g l) (g r)

