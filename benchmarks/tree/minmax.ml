type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf of 'a

let rec spec t = f t

and f = function
  | Leaf x -> (x, x)
  | Node (a, l, r) ->
      let amin, amax = g (a, a) l in
      g (amin, amax) r

and g s = function
  | Leaf x ->
      let amin, amax = s in
      (min amin x, max amax x)
  | Node (a, l, r) ->
      let amin, amax = s in
      g (g (min amin a, max amax a) l) r

let rec target t1 = h t1

and h = function Leaf x -> [%synt f0] x | Node (a, l, r) -> [%synt join] a (h l) (h r)

let repr x = x
