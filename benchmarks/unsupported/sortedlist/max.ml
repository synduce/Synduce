type 'a conslist = Nil | Cons of 'a * 'a conslist

(* Sort in increasing order. *)
let rec sort l = aux Nil l

and aux sorted = function Nil -> sorted | Cons (hd, tl) -> aux (insert hd sorted) tl

and insert x = function
  | Nil -> Cons (x, Nil)
  | Cons (hd, tl) -> if hd > x then Cons (x, Cons (hd, tl)) else Cons (hd, insert x tl)

(* Maximum element of a sorted list = head element *)
let maxs = function Nil -> Int.min_int | Cons (a, _) -> a

let rec amax l = match l with Nil -> Int.min_int | Cons (hd, tl) -> [%synt join] hd (amax tl)
