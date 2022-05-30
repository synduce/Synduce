type nat =
  | Zero
  | Succ of nat

let rec f = function
  | Zero -> g Zero
  | Succ n -> f n + g (Succ n)

and g = function
  | Zero -> 1
  | Succ n -> [%synt h] (g n)
;;

let rec t = function
  | Zero -> 1
  | Succ n -> (2 * t n) + 1
;;

let id a = a;;

assert (f = id @@ t)
