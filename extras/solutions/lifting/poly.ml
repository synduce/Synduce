
let odot (x1280, x1281) (x1290, x1291) =
  ((x1290 × x1281) + x1280, x1281 × x1291)

let s0  = (0, 1)

let f0 x124 x125 = (x125, x124)

let rec h =
  function CNil -> s0 | Single(a) -> f0 x a
  | Concat(y, z) -> odot (h y) (h z)

