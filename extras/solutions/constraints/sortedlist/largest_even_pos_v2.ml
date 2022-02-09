
let base_case a = 0 = (a % 2) ? a : 0

let f0 b = b > b ? b : 0

let f1 c x = (c > 0) && (0 = (c % 2)) ? max c x : x

let rec amax =
  function Elt(x) -> base_case x
  | Cons(hd, tl) -> hd ≤ 0 ? f0 hd : f1 hd (lpen tl)

