
let join a = 1 = a ? a + a : 3 = a ? 4 : 2 = a ? 3 : 5

let rec target = function Elt(x) -> 1 | Cons(hd, tl) -> join (target tl)

