
let join x = x

let rec amax  = function Elt(x) -> x | Cons(hd, tl) -> join hd

