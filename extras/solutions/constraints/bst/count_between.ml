
let f_else x107 x108 x109 x110 x111 =
  (x108 > x107) && (x107 > x109) ? (1 + x111) + x110 : x111 + x110

let xi_0 x104 x105 x106 = (x104 > x106) && (x106 > x105) ? 1 : 0

let f_a_gt_hi x112 = x112

let f_a_lt_lo x113 = x113

let rec g =
  function Leaf(a) -> xi_0 hi lo a
  | Node(a, l, r) -> a < lo ? f_a_lt_lo (g r) :
                       a > hi ? f_a_gt_hi (g l) : f_else a hi lo (g r) (g l)

