
let g0 a = (0, a, a)

let join b c = (max b 0, min b c, max c ((- (min b c)) + (max b 0)))

let rec ldiff2 = function Elt(x) -> g0 x | Cons(hd, tl) -> join hd (last tl)
and last = function Elt(x) -> x | Cons(hd, tl) -> last tl

