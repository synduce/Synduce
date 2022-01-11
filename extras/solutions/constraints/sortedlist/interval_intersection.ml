
let f0 a b = (false, a, b)

let f1 c x (y0, y1, y2) = ((y2 ≥ c) && (y0 || (x ≥ y1)), c, x)

let rec target =
  function Elt(a, b) -> f0 a b | Cons(a, b, l) -> f1 a b (target l)

