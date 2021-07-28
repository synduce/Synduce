type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

type 'a tree_memo = MLeaf of int * 'a | MNode of int * 'a * 'a tree_memo * 'a tree_memo

let rec is_memo_count_lt_10 = function
  | MLeaf (n, a) -> n >= 0 && if a < 10 then n = 1 else n = 0
  | MNode (n, a, l, r) ->
      n >= 0
      && (if a < 10 then n = 1 + memo l + memo r else n = memo l + memo r)
      && is_memo_count_lt_10 l && is_memo_count_lt_10 r

and memo = function MLeaf (n, a) -> n | MNode (n, a, l, r) -> n

let rec repr = function MLeaf (n, a) -> Leaf a | MNode (n, a, tl, tr) -> Node (a, repr tl, repr tr)

let rec spec = function
  | Leaf a -> if a < 10 then 1 else 0
  | Node (a, l, r) -> if a < 10 then 1 + spec l + spec r else spec l + spec r
  [@@ensures fun x -> x >= 0]

let rec target = function
  | MLeaf (n, a) -> if a < 10 then [%synt c0] else [%synt c1]
  | MNode (n, a, l, r) -> if a < 10 then [%synt f0] n else [%synt f1] n
  [@@requires is_memo_count_lt_10]
