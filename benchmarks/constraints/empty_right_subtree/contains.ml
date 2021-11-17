type 'a tree =
  | Nil of 'a
  | Leaf of 'a
  | Node of 'a * 'a tree * 'a tree

let rec empty_right = function
  | Nil x -> true
  | Leaf x -> true
  | Node (a, l, r) -> 0 = size r

and size = function
  | Nil x -> 0
  | Leaf x -> 1
  | Node (a, l, r) -> 1 + size l + size r
  [@@ensures fun x -> x >= 0]
;;

let repr x = x

let spec x t =
  let rec f = function
    | Nil a -> 0
    | Leaf a -> if a = x then 1 else 0
    | Node (a, l, r) ->
      if a = x then 1 else if f l = 1 then 1 else if f r = 1 then 1 else 0
  in
  f t
  [@@ensures fun x -> x >= 0 && x <= 1]
;;

let target y t =
  let rec g = function
    | Nil a -> [%synt xi_0]
    | Leaf a -> [%synt xi_1] y a
    | Node (a, l, r) -> [%synt xi_2] y a (g l)
  in
  g t
  [@@requires empty_right]
;;
