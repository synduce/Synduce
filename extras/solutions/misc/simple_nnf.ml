
let elit x = x

let eneglit x0 = ¬ x0

let eand x1 x2 = x1 && x2

let eor x3 x4 = x3 || x4

let rec eval2 =
  function NFLit(b) -> elit b | NFNegLit(b) -> eneglit b
  | NFAnd(x, y) -> eand (eval2 x) (eval2 y)
  | NFOr(x, y) -> eor (eval2 x) (eval2 y)

