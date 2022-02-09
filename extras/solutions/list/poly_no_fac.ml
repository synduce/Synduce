
let f0 a b = (b, a)

let odot (c0, c1) (x0, x1) = (c0 + (c1 × x0), c1 × x1)

let s0  = (0, 1)

let rec h =
  function CNil -> s0 | Single(a) -> f0 x a
  | Concat(y, z) -> odot (h y) (h z)

