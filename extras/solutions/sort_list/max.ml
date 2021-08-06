
let join x x0 = x < x0 ? x0 : x

let rec target = function Elt(x) -> x | Cons(hd, tl) -> join hd (target tl)

