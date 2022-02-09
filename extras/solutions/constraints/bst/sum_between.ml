
let f_a_gt_hi a = a

let f_a_lt_lo b = b

let f_else c x y z w = (c > y) && (x > c) ? (c + z) + w : z + w

let xi_0 u v a = (u > a) && (a > v) ? a : 0

let rec g =
  function Leaf(a) -> xi_0 hi lo a
  | Node(a, l, r) -> a < lo ? f_a_lt_lo (g r) :
                       a > hi ? f_a_gt_hi (g l) : f_else a hi lo (g r) (g l)

