
let odot (x1000, x1001) (x1010, x1011) =
  ((x1001 × x1010) + x1000, x1011 × x1001)

let s0  = (0, 1)

let f0 x99 = (x99, 10)

let rec target =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(y, z) -> odot (target y) (target z)

