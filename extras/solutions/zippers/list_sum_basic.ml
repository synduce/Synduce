
let join x2 x3 = x2 + x3

let rec zipper_sum _x =
          match _x with Zip(a, b) -> join (list_sum a) (list_sum b)

