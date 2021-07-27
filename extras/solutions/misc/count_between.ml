
let s0 (x7, x8) x0 = (x7 < x0) && (x0 < x8)

let le_case x1 x2 x3 x4 = ((x1 > x2) || x4) || x3

let gt_case x5 x6 = x6 || x5

let rec g  =
  function Leaf(a) -> s0 (lo, hi) a
  | Node(a, l, r) -> a ≥ hi ? gt_case (g l) (g r) :
                       le_case a lo (g l) (g r)

