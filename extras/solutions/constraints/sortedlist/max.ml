
let join x2 = x2

let rec amax = function Elt(x) -> x | Cons(hd, tl) -> join hd

