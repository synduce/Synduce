
let join x0 x1 = x0 < x1 ? x0 : x1

let rec target = function Elt(x) -> x | Cons(hd, tl) -> join hd (target tl)

