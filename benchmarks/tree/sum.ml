type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec repr x = x

let rec spec t = f 0 t

and f s = function
  | Nil -> s
  | Node (a, l, r) ->
      let sum = f s l in
      f (sum + a) r

let rec target = function
  | Nil -> [%synt s0]
  | Node (a, l, r) -> [%synt join] a (target l) (target r)

(** If no assertion is used, the tool search for function names repr, spec and target. *)
