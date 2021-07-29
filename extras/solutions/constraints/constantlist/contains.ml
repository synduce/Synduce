
let xi_0 x19 x20 = x20 = x19 ? 1 : 0

let xi_1 x21 x22 = x22 = x21 ? 1 : 0

let rec g  = function Elt(a) -> xi_0 x a | Cons(hd, tl) -> xi_1 x hd

