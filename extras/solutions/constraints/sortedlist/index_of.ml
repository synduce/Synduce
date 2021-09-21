
let xi_2 x24 x25 x26 = x25 = x24 ? 1 : x26 = 0 ? 0 : 1 + x26

let xi_0 x22 x23 = x23 = x22 ? 1 : 0

let xi_1 x27 x28 = x27 = x28 ? 1 : 0

let rec g =
  function Elt(a) -> xi_0 x a
  | Cons(hd, tl) -> hd ≥ x ? xi_1 x hd : xi_2 x hd (g tl)

