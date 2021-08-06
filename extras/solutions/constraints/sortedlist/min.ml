
let xi_0 x33 = x33

let xi_1 x34 = x34

let rec target = function Elt(x) -> xi_0 x | Cons(hd, tl) -> xi_1 hd

