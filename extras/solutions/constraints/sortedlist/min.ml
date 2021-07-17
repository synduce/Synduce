
let xi_0 x0 = x0

let xi_1 x1 = x1

let rec target  = function Elt(x) -> xi_0 x | Cons(hd, tl) -> xi_1 hd

