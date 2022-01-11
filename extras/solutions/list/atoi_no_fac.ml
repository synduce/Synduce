
let f0 a = (a, 10)

let odot (b0, b1) (c0, c1) = (b0 + (b1 × c0), b1 × c1)

let s0  = (0, 1)

let rec target =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(y, z) -> odot (target y) (target z)

