
let base_case a = max a 0

let oplus b c x = max x (b + c)

let rec target =
  function IElt(x) -> base_case x
  | ICons(hd, idx, tl) -> oplus hd idx (target tl)

