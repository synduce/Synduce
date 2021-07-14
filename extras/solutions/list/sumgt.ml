
let s0  = 0

let f0 x x0 = x0 > x ? x0 : 0

let odot x1 x2 x3 = x2 + x3

let rec h  =
  function CNil -> s0 | Single(a) -> f0 c a
  | Concat(x, y) -> odot c (h x) (h y)

