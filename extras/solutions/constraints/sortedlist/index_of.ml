
let xi_0 x4 x5 = x5 = x4 ? 1 : 0

let xi_2 x6 x7 x8 = x7 = x6 ? 1 : x8 = 0 ? 0 : 1 + x8

let xi_1 x9 x10 = x9 = x10 ? 1 : 0

let rec g  =
  function Elt(a) -> xi_0 x a
  | Cons(hd, tl) -> hd ≥ x ? xi_1 x hd : xi_2 x hd (g tl)

