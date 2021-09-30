
let join x8 (x90, x91) = (max x90 (x8 + (- x91)), x8)

let g0 x6 = (0, x6)

let rec amax = function Elt(x) -> g0 x | Cons(hd, tl) -> join hd (amax tl)

