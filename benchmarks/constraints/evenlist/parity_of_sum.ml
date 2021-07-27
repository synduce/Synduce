type 'a list = Elt of 'a | Cons of 'a * 'a list

let rec even_list = function Elt x -> x mod 2 = 0 | Cons (hd, tl) -> hd mod 2 = 0 && even_list tl

let rec spec = function Elt x -> x mod 2 | Cons (hd, tl) -> (hd + sum tl) mod 2

and sum = function Elt x -> x | Cons (hd, tl) -> hd + sum tl

let rec target = function Elt x -> [%synt s0] | Cons (a, l) -> [%synt f0] [@@requires even_list]
