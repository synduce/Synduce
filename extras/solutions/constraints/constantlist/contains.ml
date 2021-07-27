
let xi_0 x7 x8 = x8 = x7 ? 1 : 0

let xi_1 x9 x10 = x10 = x9 ? 1 : 0

let rec g  = function Elt(a) -> xi_0 x a | Cons(hd, tl) -> xi_1 x hd

