
let xi_0 a b c x = (a > x) && (c > b)

let xi_11 y z = y || z

let xi_12 w = w

let xi_2 u v a b1 c1 x1 = (c1 || x1) || ((u > a) && (b1 > v))

let rec g =
  function Leaf(a, b) -> xi_0 hi lo a b
  | Node(a, b, l, r) -> b > hi ? a > lo ? xi_11 (g r) (g l) : xi_12 (g r) :
                          xi_2 hi lo b a (g l) (g r)

