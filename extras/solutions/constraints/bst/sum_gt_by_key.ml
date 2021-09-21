
let s0 x16 x17 x18 = x17 > x16 ? x18 : 0

let join2 x19 x20 x21 = x21

let join1 x22 x23 x24 x25 = x25 + x24

let rec g =
  function KeyValue(k, v) -> s0 key k v
  | Node(hd_key, l, r) -> hd_key > key ? join1 key hd_key (g l) (g r) :
                            join2 key hd_key (g r)

