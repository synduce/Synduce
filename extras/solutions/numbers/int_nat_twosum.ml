
let sub1 x19 = (- 1) + x19

let add1 x20 = 1 + x20

let add x21 x22 = (10 + x21) + x22

let c0  = - 5

let rec two_esum _x =
          match _x with TwoInts(i1, i2) -> add (to_int i1) (to_int i2)
and to_int =
  function Zero -> c0 | Sub1(n) -> sub1 (to_int n)
  | Add1(n) -> add1 (to_int n)

