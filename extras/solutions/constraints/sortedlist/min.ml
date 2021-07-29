
let xi_0 x4 = x4

let xi_1 x5 = x5

let rec target  = function Elt(x) -> xi_0 x | Cons(hd, tl) -> xi_1 hd

