(** @synduce --no-gropt *)

type 'a tree =
  | Nil
  | Node of 'a * 'a tree * 'a tree
  | Single of 'a

let rec spec t = f (0, 0) t

and f s = function
  | Nil -> s
  | Single a ->
    let sum1, m1 = s in
    sum1 + a, max m1 (sum1 + a)
  | Node (a, l, r) ->
    let sum1, m1 = s in
    f (f (sum1 + a, max m1 (sum1 + a)) l) r
  [@@ensures fun (x, y) -> y >= 0 && y >= x]
;;

let rec mpps = function
  | Nil -> [%synt s0]
  | Single a -> [%synt f0] a
  | Node (a, l, r) -> [%synt join1] a (mpps l) (mpps r)
;;

assert (mpps = spec)
