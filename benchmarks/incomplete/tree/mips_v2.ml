type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

let rec repr x = x

let rec mips t = f (0, 0) t [@@ensures fun (x, y) -> y >= 0]

and f s = function
  | Leaf a ->
      let sum, m1 = s in
      (sum + a, max (sum + a) m1)
  | Node (a, l, r) ->
      let sum, m1 = f s l in
      f (sum + a, max (sum + a) m1) r

let rec hsum = function
  | Leaf a -> [%synt s0] a
  | Node (a, l, r) -> [%synt join] a (hsum l) (hsum r)

(* Declare the synthesis target *)
;;

assert (hsum = repr @@ mips)
