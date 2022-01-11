
let f0 a = max a a

let f1 b = b

let rec amax =
  function Elt(x) -> x | Cons(hd, tl) -> (hd % 2) = 0 ? f0 hd : f1 (lpen tl)

