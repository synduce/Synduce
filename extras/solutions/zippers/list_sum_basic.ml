
let join x1 x2 = x1 + x2

let rec zipper_sum _x =
          match _x with Zip(a, b) -> join (list_sum a) (list_sum b)

