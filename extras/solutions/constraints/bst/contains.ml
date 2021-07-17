
let xi_0 x5 x6 = x6 = x5 ? 1 : 0

let xi_2 x7 x8 x9 x10 = x8 = x7 ? 1 : x9 = 1 ? 1 : x10 = 1 ? 1 : 0

let xi_1 x11 = x11

let rec g  =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x < a ? xi_1 (g l) : xi_2 x a (g l) (g r)

