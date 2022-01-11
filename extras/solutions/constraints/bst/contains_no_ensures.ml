
let xi_0 a b = a = b ? 1 : 0

let xi_1 c = 1 = c ? c : 0

let xi_2 x y z w = x = y ? 1 : 1 = z ? 1 : 1 = w ? 1 : 0

let rec g =
  function Leaf(a) -> xi_0 x a
  | Node(a, l, r) -> x < a ? xi_1 (g l) : xi_2 x a (g l) (g r)

