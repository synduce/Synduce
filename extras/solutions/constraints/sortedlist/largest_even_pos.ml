
let base_case a = 0 = (a % 2) ? a : 0

let f0 b = max b b

let f1 c = c

let rec amax =
  function Elt(x) -> base_case x
  | Cons(hd, tl) -> ((hd % 2) = 0) && (hd > 0) ? f0 hd : f1 (lpen tl)

