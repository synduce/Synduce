type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

type 'a tree' = Nil' of int | Node' of 'a * int * 'a tree' * 'a tree'

let rec repr = function Nil' _i -> Nil | Node' (a, _i, l, r) -> Node (a, repr l, repr r)

let rec pred = function
  | Nil' n -> n = 0
  | Node' (_a, n, l, r) -> n = 1 + aux l + aux r && pred l && pred r

and aux = function Nil' _n -> 0 | Node' (_a, n, _l, _r) -> n

let rec spec = function
  | Nil -> 0
  | Node (_a, l, r) -> 1 + spec l + spec r
  [@@ensures fun x -> x >= 0]

let rec target = function
  | Nil' _i -> [%synt s0]
  | Node' (_a, i, _l, _r) -> [%synt f] i
  [@@requires pred]
