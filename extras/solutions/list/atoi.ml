
let odot (x20, x21) (x30, x31) = (x20 + (x21 × x30), x21 × x31)

let s0  = (0, 1)

let f0 x0 = (x0, 10)

let rec target =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(y, z) -> odot (target y) (target z)

