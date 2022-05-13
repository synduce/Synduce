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
  | PNil -> 0
  | PNode (a, l) -> [%synt join] a (sum l)

and sum = function
  | LNil -> 0
  | Cons (hd, tl) -> [%synt j2] (target hd) (sum tl)
;;

let rec spec = function
  | TNil -> 0
  | TNode (a, l, r) -> a + spec l + spec r
;;

let rec repr = function
  | PNil -> TNil
  | PNode (a, l) -> TNode (a, TNil, f l)
;;

let rec f = function
  | LNil -> TNil
  | Cons (hd, tl) -> TNode (0, repr hd, f tl)
;;
