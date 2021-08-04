type 'a list = Elt of 'a | Cons of 'a * 'a list

let rec repr = function Elt x -> Elt x | Cons (hd, tl) -> insert hd (repr tl)

and insert y = function
  | Elt x -> if y < x then Cons (y, Elt x) else Cons (x, Elt y)
  | Cons (hd, tl) -> if y < hd then Cons (y, Cons (hd, tl)) else Cons (hd, insert y tl)

let rec spec = function Elt x -> x | Cons (hd, tl) -> hd

let rec target = function Elt x -> x | Cons (hd, tl) -> [%synt join] hd (target tl)
