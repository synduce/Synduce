type list =
  | Elt of int
  | Cons of int * list

type pos =
  | One
  | S of pos

type unique_list =
  | AElt of int * pos
  | ACons of int * pos * unique_list

let rec is_alist = function
  | AElt (k, v) -> true
  | ACons (hdk, hdv, tl) -> keys_differ hdk tl && is_alist tl

and keys_differ key = function
  | AElt (k, v) -> not (k = key)
  | ACons (hdk, hdv, tl) -> (not (hdk = key)) && keys_differ key tl
;;

let rec repr = function
  | AElt (k, v) -> dup k v
  | ACons (hdk, hdv, tl) -> dec hdk tl hdv

and dec k tl = function
  | S v -> Cons (k, dec k tl v)
  | One -> Cons (k, repr tl)

and dup k = function
  | S v -> Cons (k, dup k v)
  | One -> Elt k
;;

(* Return max frequency element. *)
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

let rec target = function
  | AElt (k, v) -> [%synt s0] k (int_of v)
  | ACons (hd_key, hdv, tl) -> [%synt join] hd_key (int_of hdv) (target tl)
  [@@requires is_alist]

and int_of = function
  | S n -> 1 + int_of n
  | One -> 1
;;
