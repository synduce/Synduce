(** @synduce --no-lifting *)

type list =
  | Nil
  | Cons of int * list

let rec is_unique_list = function
  | Nil -> true
  | Cons (hdk, tl) -> keys_differ hdk tl && is_unique_list tl

and keys_differ key = function
  | Nil -> true
  | Cons (hdk, tl) -> (not (hdk = key)) && keys_differ key tl
;;

let rec repr = function
  | Nil -> Nil
  | Cons (hdk, tl) -> Cons (hdk, repr tl)
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
    | Nil -> [%synt s0] a
    | Cons (hd_key, tl) ->
      if hd_key = a then [%synt f0] hd_key else [%synt f1] hd_key (g tl)
  in
  g l
  [@@requires is_unique_list]
;;
