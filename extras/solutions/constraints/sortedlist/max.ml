
let join x1 = x1

let rec amax  = function Elt(x) -> x | Cons(hd, tl) -> join hd

