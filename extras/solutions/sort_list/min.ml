
let join x x0 = x < x0 ? x : x0

let rec target  = function Elt(x) -> x | Cons(hd, tl) -> join hd (target tl)

