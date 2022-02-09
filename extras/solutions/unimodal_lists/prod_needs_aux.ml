
let f0 a = (0, a)

let join b c (x0, x1) (y0, y1) = (((b × c) × x1) × y0, b)

let s0  = (0, 1)

let rec g =
  function UNil -> s0 | UElt(a) -> f0 a
  | USplit(x, a, b, y) -> join a b (g x) (g y)

