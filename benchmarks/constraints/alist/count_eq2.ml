(** @synduce -NB --no-lifting *)

type list =
  | Nil
  | Cons of int * list

type nat =
  | Z
  | S of nat

type alist =
  | ANil
  | ACons of int * nat * alist

let rec is_alist = function
  | ANil -> true
  | ACons (hdk, hdv, tl) -> keys_differ hdk tl && is_alist tl

and keys_differ key = function
  | ANil -> true
  | ACons (hdk, hdv, tl) -> (not (hdk = key)) && keys_differ key tl
;;

let rec repr = function
  | ANil -> Nil
  | ACons (hdk, hdv, tl) -> dec hdk tl hdv

and dec k tl = function
  | S v -> Cons (k, dec k tl v)
  | Z -> repr tl
;;

(* Count keys matching a. *)
let spec a l =
  let rec f = function
    | Nil -> 0
    | Cons (hd, tl) -> if hd = a then 1 + f tl else f tl
  in
  f l
;;

let target a l =
  let rec g = function
    | ANil -> [%synt s0] a
    | ACons (hd_key, hdv, tl) ->
      if hd_key = a then [%synt f0] (int_of hdv) else [%synt f1] hd_key (g tl)
  and int_of = function
    | S n -> 1 + int_of n
    | Z -> 0
  in
  g l
  [@@requires is_alist]
;;
