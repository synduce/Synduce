
let xi_0 a b = a = b ? 1 : 0

let xi_1 c = c

let xi_2 x = x

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x = a ? 1 : x < a ? xi_1 (g l) : xi_2 (g r)

