
let s0  = 0

let f0 x = x

let f2 x0 = x0

let f1 x1 = x1

let rec hsum  =
  function CNil -> s0 | Single(a) -> f0 a
  | Concat(x, y) -> ((f1 (hsum x)) + (f2 (hsum y))) + 0

