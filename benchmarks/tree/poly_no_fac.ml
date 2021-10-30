type tree =
  | Nil
  | Node of int * tree * tree

let spec x t =
  let rec main t = f 0 t
  and f s = function
    | Nil -> s
    | Node (a, l, r) ->
      let y = f s l in
      f (a + (x * y)) r
  in
  main t
;;

let target x t =
  let rec h = function
    | Nil -> [%synt s0]
    | Node (a, l, r) -> [%synt join] x a (h l) (h r)
  in
  h t
;;

assert (target = spec)
