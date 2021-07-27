
let xi_0 x16 x17 = x17 = x16 ? 1 : 0

let xi_2 x18 x19 x20 = x19 = x18 ? 1 : x20 = 0 ? 0 : 1 + x20

let xi_1 x21 x22 = x21 = x22 ? 1 : 0

let rec g  =
  function Elt(a) -> xi_0 x a
  | Cons(hd, tl) -> hd ≥ x ? xi_1 x hd : xi_2 x hd (g tl)

