
let xi_0 x15 x16 = x16 < x15 ? 1 : 0

let xi_1 x17 = 1 + x17

let c0  = 0

let rec g =
  function Elt(x) -> xi_0 a x | Cons(hd, tl) -> hd < a ? xi_1 (g tl) : c0

