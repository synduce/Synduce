
let xi_0 x22 x23 = x23 = x22 ? 1 : 0

let xi_1 x24 x25 = x25 = x24 ? 1 : 0

let rec g = function Elt(a) -> xi_0 x a | Cons(hd, tl) -> xi_1 x hd

