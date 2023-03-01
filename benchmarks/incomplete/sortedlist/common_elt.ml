(** @synduce -NB -n 20 --no-lifting -s 2 *)

(* This is an example of a skeleton where every configuration is unrealizable. *)

type zipped_list =
  | Zip of int * int * zipped_list
  | Elt of int * int

let rec common_elt = function
  | Elt (a, b) -> a = b
  | Zip (a, b, y) -> a = b || common_elt y || find1 a y || find2 b y

and find1 e = function
  | Elt (a, b) -> b = e
  | Zip (a, b, x) -> b = e || find1 e x

and find2 e = function
  | Elt (a, b) -> a = e
  | Zip (a, b, x) -> a = e || find1 e x
;;

let rec sorted = function
  | Elt (a, b) -> true
  | Zip (a, b, x) ->
    let a2, b2 = head x in
    a < a2 && b < b2 && sorted x

and head = function
  | Elt (a, b) -> a, b
  | Zip (a, b, x) -> a, b
;;

(** @requires sorted *)
let rec target = function
  | Elt (a, b) -> [%synt f0] a b
  | Zip (a, b, x) -> [%synt f1] a b (target x)
  [@@requires sorted]
;;

assert (target = common_elt)
