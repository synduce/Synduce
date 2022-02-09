
let join a b = b + -1

let rec target = function Elt(x) -> 0 | Cons(hd, tl) -> join hd (target tl)

