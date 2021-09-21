
let xi_0 x5 = x5

let xi_1 x6 = x6

let rec target = function Elt(x) -> xi_0 x | Cons(hd, tl) -> xi_1 hd

