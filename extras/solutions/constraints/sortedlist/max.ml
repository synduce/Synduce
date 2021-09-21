
let join x4 = x4

let rec amax = function Elt(x) -> x | Cons(hd, tl) -> join hd

