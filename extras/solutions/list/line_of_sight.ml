
let f_0 a = (a, a, true)

let odot (b0, b1, b2) (c0, c1, c2) = (b0, max b1 c1, b1 > (b2 ? c1 : b1))

let rec h = function Elt(a) -> f_0 a | Cat(x, y) -> odot (h x) (h y)

