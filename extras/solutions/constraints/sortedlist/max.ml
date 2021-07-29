
let join x10 = x10

let rec amax  = function Elt(x) -> x | Cons(hd, tl) -> join hd

