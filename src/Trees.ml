open Base
open Utils

type 'a tree =
  | Nil : 'a tree
  | Cont : 'a tree
  | Node : 'a * ('a tree) list -> 'a tree

let rec print_tree (p : Formatter.t -> 'a -> unit) (f : Formatter.t) (t : 'a tree) : unit =
  match t with
  | Nil -> ()
  | Cont -> Fmt.(pf f "_")
  | Node (x, l) ->
    if List.length l > 0 then
      Fmt.(pf f "%a(%a)" p x (box ~indent:0 (list ~sep:comma (print_tree p))) l)
    else
      Fmt.(pf f "%a" p x)

(**
   Perfectly balanced trees.
*)
module GTree =
struct
  type ('a, _) t =
    | GEmpty : ('a, z) t
    | GNode : ('a, 'n) t * 'a * ('a, 'n) t -> ('a, 'n s) t

  let rec depth : type a n.(a, n) t -> n =
    function
    | GEmpty -> Z
    | GNode (l, _, _) -> S (depth l)

  let top : type a n. (a, n s) t -> a = function
    | GNode (_, x, _) -> x
end
