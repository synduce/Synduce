type 'a list = Elt of 'a | Cons of 'a * 'a list

let rec repr = function Elt x -> Elt x | Cons (hd, tl) -> insert hd (repr tl)

and insert y = function
  | Elt x -> if y > x then Cons (y, Elt x) else Cons (x, Elt y)
  | Cons (hd, tl) -> if y > hd then Cons (y, Cons (hd, tl)) else Cons (hd, insert y tl)

let rec spec = function
  | Elt x -> if x >= 0 then x else 0
  | Cons (hd, tl) -> if hd >= 0 then hd + spec tl else 0

let rec target = function
  | Elt x -> [%synt base_case] x
  | Cons (hd, tl) -> [%synt oplus] hd (target tl)
