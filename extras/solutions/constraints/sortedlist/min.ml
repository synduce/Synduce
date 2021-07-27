
let xi_0 x3 = x3

let xi_1 x4 = x4

let rec target  = function Elt(x) -> xi_0 x | Cons(hd, tl) -> xi_1 hd

