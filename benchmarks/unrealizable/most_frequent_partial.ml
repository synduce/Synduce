(* Non-empty lists type. *)
type list =
  | Elt of int
  | Cons of int * list

(* Inductive type for positive integers. *)
type pos =
  | One
  | S of pos

(* A binary search tree with integer keys and positive natural numbers as values. *)
type bst_map =
  | KeyValue of int * pos
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
  | KeyValue (k, v) -> dup k v
  | Node (a, l, r) -> append (repr l) (repr r)

and append x = function
  | Elt y0 -> Cons (y0, x)
  | Cons (hd, tl) -> Cons (hd, append x tl)

and dup k = function
  | S v -> Cons (k, dup k v)
  | One -> Elt k
;;

(* Return most frequent element with its count. *)
let rec spec = function
  | Elt v -> 1, v
  | Cons (hd, tl) ->
    let cnt, v = spec tl in
    let cnt2 = count hd tl in
    if cnt2 + 1 > cnt then cnt2 + 1, hd else cnt, v
  [@@ensures fun (x, y) -> x > 0]

and count x = function
  | Elt v -> if v = x then 1 else 0
  | Cons (hd, tl) -> count x tl + if hd = x then 1 else 0
  [@@ensures fun x -> x > 0]
;;

(* Synthesize a parallel version that is also linear time. *)
let rec target = function
  | KeyValue (k, v) -> [%synt s0] k (int_of v)
  | Node (hd_key, l, r) -> [%synt join] (target l) (target r)
  [@@requires is_imap]

and int_of = function
  | S n -> [%synt int_succ] (int_of n)
  | One -> [%synt int_base]
;;
