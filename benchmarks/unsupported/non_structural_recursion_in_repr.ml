type list = Nil | Cons of int * list

type nat = Z | S of nat

type unique_list = ANil | ACons of int * int * unique_list

let rec is_alist = function
  | ANil -> true
  | ACons (hdk, hdv, tl) -> keys_differ hdk tl && is_alist tl

and keys_differ key = function
  | ANil -> true
  | ACons (hdk, hdv, tl) -> (not (hdk = key)) && keys_differ key tl

let rec repr = function
  | ANil -> Nil
  | ACons (hdk, hdv, tl) -> if hdv <= 0 then repr tl else Cons (hdk, repr (ACons (hdk, hdv - 1, tl)))

(* Count keys matching a. *)
let spec a l =
  let rec f = function Nil -> 0 | Cons (hd, tl) -> if hd = a then hd + f tl else f tl in
  f l

let target a l =
  let rec g = function
    | ANil -> [%synt s0] a
    | ACons (hd_key, hdv, tl) -> if hd_key = a then [%synt f0] hd_key else [%synt f1] hd_key (g tl)
  in
  g l
  [@@requires is_alist]
