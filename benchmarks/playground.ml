(** @synduce *)

type nat = Z | S of nat

type list = Nil | Cons of int * list

type compressed_list = CNil | CCons of (nat * int) * compressed_list

let rec no_adjacent_duplicates = function Nil -> true | Cons (hd, tl) -> nodup hd tl

and nodup x = function Nil -> true | Cons (hd, tl) -> (not (hd = x)) && nodup hd tl

let rec stutter =
  function
  | CNil()
  | CCons()
