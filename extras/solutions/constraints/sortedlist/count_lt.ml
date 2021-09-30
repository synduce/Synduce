
let xi_1 x2 = 1 + x2

let xi_0 x0 x1 = x1 < x0 ? 1 : 0

let c0  = 0

let rec g =
  function Elt(x) -> xi_0 a x | Cons(hd, tl) -> hd < a ? xi_1 (g tl) : c0

