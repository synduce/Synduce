
let base_case  = 0

let init a = max a 0

let join b c = max b (b + c)

let rec h =
  function CNil -> base_case | Single(x) -> init x
  | Concat(x, y) -> join (h x) (h y)

