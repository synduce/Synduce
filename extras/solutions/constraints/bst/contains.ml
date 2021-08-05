
let xi_0 x27 x28 = x28 = x27 ? 1 : 0

let xi_2 x29 x30 x31 x32 = x30 = x29 ? 1 : x31 = 1 ? 1 : x32 = 1 ? 1 : 0

let xi_1 x33 = x33

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x < a ? xi_1 (g l) : xi_2 x a (g l) (g r)

