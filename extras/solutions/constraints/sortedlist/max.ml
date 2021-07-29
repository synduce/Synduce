
let join x8 = x8

let rec amax  = function Elt(x) -> x | Cons(hd, tl) -> join hd

