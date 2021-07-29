
let xi_0 x19 x20 = x20 = x19 ? 1 : 0

let xi_2 x21 x22 x23 = x22 = x21 ? 1 : x23 = 0 ? 0 : 1 + x23

let xi_1 x24 x25 = x24 = x25 ? 1 : 0

let rec g  =
  function Elt(a) -> xi_0 x a
  | Cons(hd, tl) -> hd ≥ x ? xi_1 x hd : xi_2 x hd (g tl)

