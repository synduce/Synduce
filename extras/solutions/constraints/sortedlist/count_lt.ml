
let xi_1 x8 = 1 + x8

let xi_0 x6 x7 = x7 < x6 ? 1 : 0

let c0  = 0

let rec g =
  function Elt(x) -> xi_0 a x | Cons(hd, tl) -> hd < a ? xi_1 (g tl) : c0

