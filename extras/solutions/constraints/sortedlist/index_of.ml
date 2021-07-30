
let xi_0 x56 x57 = x57 = x56 ? 1 : 0

let xi_2 x58 x59 x60 = x59 = x58 ? 1 : x60 = 0 ? 0 : 1 + x60

let xi_1 x61 x62 = x61 = x62 ? 1 : 0

let rec g  =
  function Elt(a) -> xi_0 x a
  | Cons(hd, tl) -> hd ≥ x ? xi_1 x hd : xi_2 x hd (g tl)

