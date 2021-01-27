type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec repr  = function x -> x

let rec lpeak = function t -> f (0, 0, true, 0) t
and f s =
  function
  | Nil -> s
  | Node(a, l, r) ->
      let sum, m1, cl, aux = f s l in
      f ((if a > 0 then sum + a else 0),
          (if a > 0 then max (sum + a) m1 else m1),
          ((a > 0) && cl),
          (aux + (if cl && a > 0 then a else 0))) r
  [@inv (fun (sum, m1, cl, aux) -> sum >= 0 &&  m1 >= sum)]

let rec hsum =
  function
  | Nil -> [%synt s0]
  | Node(a, l, r) -> [%synt join] a (hsum l) (hsum r)
  [@equiv lpeak repr]