(** @synduce -NB *)

type 'a tree =
  | TElt of 'a
  | TNode of 'a * int * 'a tree * 'a tree

type 'a list =
  | Elt of 'a
  | Cons of 'a * 'a list

type 'a ptree =
  | PElt of 'a
  | PNode of 'a * 'a ptree list

let rec repr = function
  | TElt x -> PElt x
  | TNode (a, s, l, r) -> PNode (a, Cons (repr l, repr_list r))

and repr_list = function
  | TElt x -> Elt (PElt x)
  | TNode (a, s, l, r) -> Cons (PElt a, Cons (repr l, repr_list r))
;;

let rec spec = function
  | PElt a -> a
  | PNode (a, l) -> a + sums_aux l

and sums_aux = function
  | Elt a_tree -> spec a_tree
  | Cons (hd, tl) -> spec hd + sums_aux tl
;;

let rec is_memo_rightsum = function
  | TElt a -> true
  | TNode (a, s, l, r) -> a = tsum r && is_memo_rightsum l

and tsum = function
  | TElt a -> a
  | TNode (a, s, l, r) -> a + tsum l + tsum r
;;

let rec target = function
  | TElt a -> [%synt f0] a
  | TNode (a, s, l, r) -> [%synt join1] a s (target l)
  [@@requires is_memo_rightsum]
;;
