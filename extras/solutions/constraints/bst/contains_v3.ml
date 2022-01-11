
let xi_0 a b = a = b ? 1 : 0

let xi_1 c = c

let xi_2 x y z = x = y ? 1 : z

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x < a ? xi_1 (g l) : xi_2 x a (g r)

