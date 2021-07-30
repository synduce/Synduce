
let xi_0 x92 x93 = x93 = x92 ? 1 : 0

let xi_1 x94 x95 = x95 = x94 ? 1 : 0

let rec g  = function Elt(a) -> xi_0 x a | Cons(hd, tl) -> xi_1 x hd

