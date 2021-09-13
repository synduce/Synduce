
let s0 x13 x14 x15 = x14 > x13 ? x15 : 0

let join2 x16 x17 x18 = x18

let join1 x19 x20 x21 x22 = x22 + x21

let rec g =
  function KeyValue(k, v) -> s0 key k v
  | Node(hd_key, l, r) -> hd_key > key ? join1 key hd_key (g l) (g r) :
                            join2 key hd_key (g r)

