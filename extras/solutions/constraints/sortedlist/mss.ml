
let f0 a = (a, max a 0, max a 0, max a 0)

let f1 b (c0, c1, c2, c3) =
  (b + c0, max c1 (b + c0), max (b + c2) 0, max (max c3 (b + c2)) 0)

let odot (x0, x1, x2, x3) (y0, y1, y2, y3) =
  (x0 + y0, max y1 (x1 + y0), max x2 (x0 + y2), max (max x2 y3) (x1 + y2))

let rec h =
  function Single(a) -> f0 a
  | Concat(a, y, z) -> a < 0 ? f1 (asum y) (h z) : odot (h y) (h z)
and asum = function Single(a) -> a | Concat(a, y, z) -> (asum y) + (asum z)

