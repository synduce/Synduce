type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let repr x = x 

let spec x t = 
  let rec f = function 
  | Nil -> 0 
  | Node(a, l, r) -> if a < x then (1 + f l + f r) else (f l + f r)
in f t
[@@ensures fun x -> x >= 0]


let target x t = 
  let rec g = function 
  | Nil -> [%synt xi_0]
  | Node(a, l, r) -> if a < x then [%synt xi_1] (g l) (g r) else [%synt xi_2] (g l)
  in g t 
