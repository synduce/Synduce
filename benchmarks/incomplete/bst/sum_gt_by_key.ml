(** @synduce -s 2 --no-lifting -NB -n 30 *)

(* Non-empty lists type. *)
type list =
  | Elt of int * int
  | Cons of int * int * list

(* A binary search tree with integer keys and positive natural numbers as values. *)
type bst_map =
  | KeyValue of int * int
  | Node of int * bst_map * bst_map

let rec min_key = function
  | KeyValue (k, v) -> k
  | Node (a, l, r) -> min (min_key l) (min_key r)
;;

let rec max_key = function
  | KeyValue (k, v) -> k
  | Node (a, l, r) -> max (max_key l) (max_key r)
;;

let rec is_imap = function
  | KeyValue (k, v) -> true
  | Node (a, l, r) -> max_key l < a && a <= min_key r && is_imap l && is_imap r
;;

let rec repr = function
  | KeyValue (k, v) -> Elt (k, v)
  | Node (a, l, r) -> append (repr l) (repr r)

and append x = function
  | Elt (x0, y0) -> Cons (x0, y0, x)
  | Cons (hd0, hd1, tl) -> Cons (hd0, hd1, append x tl)
;;

(* Return most frequent element with its count. *)
let spec key l =
  let rec f = function
    | Elt (k, v) -> if k > key then v else 0
    | Cons (hdk, hdv, tl) -> if hdk > key then hdv + f tl else f tl
  in
  f l
;;

(* Synthesize a parallel version that is also linear time. *)
let target key t =
  let rec g = function
    | KeyValue (k, v) -> [%synt s0] key k v
    | Node (hd_key, l, r) ->
      if hd_key > key then [%synt join1] (g l) (g r) else [%synt join2] hd_key key (g r)
  in
  g t
  [@@requires is_imap]
;;
