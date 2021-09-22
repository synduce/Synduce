
let c0  = 0

let sub1 x39 = 1 + x39

let add1 x40 = (- 1) + x40

let add x41 x42 = (- x42) + (- x41)

let rec two_esum _x =
          match _x with TwoInts(i1, i2) -> add (to_int i1) (to_int i2)
and to_int =
  function Zero -> c0 | Sub1(n) -> sub1 (to_int n)
  | Add1(n) -> add1 (to_int n)

