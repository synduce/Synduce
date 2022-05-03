type 'a tree =
  | TNil
  | TNode of 'a * 'a tree * 'a tree

type 'a list =
  | LNil
  | Cons of 'a * 'a list

type 'a ptree =
  | PNil
  | PNode of 'a * 'a ptree list

let rec target = function
  | PNil -> 1
  | PNode (a, l) -> [%synt join] a (prod l)

and prod = function
  | LNil -> 1
  | Cons (hd, tl) -> [%synt j2] (target hd) (prod tl)
;;

let rec spec = function
  | TNil -> 1
  | TNode (a, l, r) -> a * spec l * spec r
;;

let rec repr = function
  | PNil -> TNil
  | PNode (a, l) -> TNode (a, TNil, f l)

and f = function
  | LNil -> TNil
  | Cons (hd, tl) -> TNode (1, repr hd, f tl)
;;
