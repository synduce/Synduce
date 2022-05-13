type nat =
  | Z
  | S of nat

let rec odd = function
  | Z -> false
  | S x -> even x

and even = function
  | Z -> true
  | S x -> odd x
;;

let rec query x = odd x && odd (S (S x))
let rec f x = [%synt fi];;

assert (f = query)
