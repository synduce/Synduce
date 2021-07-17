
let xi_0 x1 x2 = x2 = x1 ? 1 : 0

let xi_1 x3 x4 = x4 = x3 ? 1 : 0

let rec g  = function Elt(a) -> xi_0 x a | Cons(hd, tl) -> xi_1 x hd

