type tree =
  | Inner of tree * tree
  | Leaf of int

let rec g = function
  | Leaf a -> [%synt u0]
  | Inner (l, r) -> [%synt u1] (g l) (g r)

let rec size = function
  | Leaf _ -> 1
  | Inner (l, r) -> (size l) + (size r) + 1

let is_odd a = a mod 2 = 1

;;

assert (for_all (fun t -> is_odd (g t) && (g t) > (size t)))
