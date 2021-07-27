
let xi_0 x5 x6 = x6 < x5 ? 1 : 0

let xi_1 x7 = 1 + x7

let c0  = 0

let rec g  =
  function Elt(x) -> xi_0 a x | Cons(hd, tl) -> hd < a ? xi_1 (g tl) : c0

