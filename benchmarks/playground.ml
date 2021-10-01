(** @synduce --no-lifting *)

type nat = Z | S of nat

type list = Nil | Cons of int * list

type compressed_list = CNil | CCons of nat * int * compressed_list

let rec no_adjacent_duplicates = function Nil -> true | Cons (hd, tl) -> nodup hd tl

and nodup x = function Nil -> true | Cons (hd, tl) -> (not (hd = x)) && nodup hd tl

let rec stutter = function CNil -> Nil | CCons (hdc, hdv, tl) -> repeat hdv (stutter tl) hdc

and repeat a cont = function Z -> cont | S n -> Cons (a, repeat a cont n)

let rec sum_list = function Nil -> 0 | Cons (hd, tl) -> hd + sum_list tl

let rec target = function
  | CNil -> [%synt s0]
  | CCons (hdc, hdv, tl) -> [%synt f] (value hdc) hdv (target tl)

and value = function Z -> 0 | S n -> 1 + value n
;;

assert (target = stutter @@ sum_list)
