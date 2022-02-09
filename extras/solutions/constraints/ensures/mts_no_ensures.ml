
let f0 a = (max a 0, a)

let join (b0, b1) (c0, c1) = (max c0 (b0 + c1), b1 + c1)

let s0  = (0, 0)

let rec hom =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(x, y) -> join (hom x) (hom y)

