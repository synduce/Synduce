
let xi_0 a b c x = (a > x) && (c > b)

let xi_1 y = y

let xi_2 z w u v a b1 = (a || b1) || ((z > v) && (u > w))

let rec g =
  function Leaf(a, b) -> xi_0 hi lo a b
  | Node(a, b, l, r) -> (b > hi) && (a < lo) ? xi_1 (g r) :
                          xi_2 hi lo a b (g l) (g r)

