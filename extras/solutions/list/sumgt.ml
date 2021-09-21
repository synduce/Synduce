
let s0  = 0

let f0 x0 x1 = x1 > x0 ? x1 : 0

let odot x2 x3 x4 = x3 + x4

let rec h =
  function CNil -> s0 | Single(a) -> f0 c a
  | Concat(x, y) -> odot c (h x) (h y)

