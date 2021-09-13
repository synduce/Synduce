
let f_else x100 x101 x102 x103 x104 =
  (x101 > x100) && (x100 > x102) ? (1 + x104) + x103 : x104 + x103

let xi_0 x97 x98 x99 = (x97 > x99) && (x99 > x98) ? 1 : 0

let f_a_gt_hi x105 = x105

let f_a_lt_lo x106 = x106

let rec g =
  function Leaf(a) -> xi_0 hi lo a
  | Node(a, l, r) -> a < lo ? f_a_lt_lo (g r) :
                       a > hi ? f_a_gt_hi (g l) : f_else a hi lo (g r) (g l)

