type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree

let rec spec t = f (0, 0) t

and f s = function
  | Nil -> s
  | Node (a, l, r) ->
    let sum1, m1 = f s l in
    f (sum1 + a, max (m1 + a) 0) r
  [@@ensures fun (x, y) -> y >= x && y >= 0]
;;

let rec mits = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> [%synt join1] a (mits l) (mits r)
;;

let repr x = x;;

assert (mits = repr @@ spec)
