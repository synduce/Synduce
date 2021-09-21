
let s0 (x00, x01) x1 = (x00 < x1) && (x1 < x01)

let le_case x2 x3 x4 x5 = ((x2 > x3) || x5) || x4

let gt_case x6 x7 = x7 || x6

let rec g =
  function Leaf(a) -> s0 (lo, hi) a
  | Node(a, l, r) -> a ≥ hi ? gt_case (g l) (g r) :
                       le_case a lo (g l) (g r)

