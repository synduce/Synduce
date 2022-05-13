open Base
open Term

let get_id_const (op : Binop.t) : term option =
  match op with
  | Plus -> Some (mk_const (Constant.CInt 0))
  | Times -> Some (mk_const (Constant.CInt 1))
  | Min -> Some (mk_const (Constant.CInt Int.max_value))
  | Max -> Some (mk_const (Constant.CInt Int.min_value))
  | And -> Some (mk_const Constant.CTrue)
  | Or -> Some (mk_const Constant.CFalse)
  | _ -> None
;;

let is_id_int_const (op : Binop.t) (i : int) : bool =
  match op, i with
  | Plus, 0 | Div, 1 | Minus, 0 | Times, 1 -> true
  | _ -> false
;;

let is_null_int_const (op : Binop.t) (i : int) : bool =
  match op, i with
  | Times, 0 -> true
  | _ -> false
;;

let is_null_bool_const (op : Binop.t) (b : bool) : bool =
  match op, b with
  | And, false | Or, true -> true
  | _ -> false
;;

let is_and op =
  Binop.(
    match op with
    | And -> true
    | _ -> false)
;;

let is_or op =
  Binop.(
    match op with
    | Or -> true
    | _ -> false)
;;

let get_ty_const (typ : RType.t) : term =
  RType.(
    match typ with
    | TInt -> mk_const (Constant.of_int 0)
    | TBool -> mk_const (Constant.of_bool true)
    | _ -> mk_const (Constant.of_int 0))
;;

(**
  Left distributive operators: for each pair op1, op2 it means that:
  a op1 (b op2 c) = (a op1 b) op2 (a op1 c)
*)
let left_distrib =
  Map.of_alist_multi
    (module Operator)
    Operator.
      [ Binary Plus, Binary Max (* c + max(a,b) -> max(c+a, c+b) *)
      ; Binary Times, Binary Plus (* c * (a + b) -> c * a + c * b *)
      ; Binary Plus, Binary Min (* c + min(a,b) -> min(c + a, c + b) *)
      ; Binary And, Binary Or (* c && (a || b) -> c && a || c && b *)
      ]
;;

(** For example, `is_left_distrib Plus Max` is true. *)
let is_left_distrib op1 op2 =
  match Map.find left_distrib op1 with
  | Some binops -> Binop.(List.mem binops ~equal op2)
  | None -> false
;;

let get_distributing op =
  Operator.(
    match op with
    | Binary Plus -> Some (Binary Times)
    | Binary (Min | Max) -> Some (Binary Plus)
    | Binary Or -> Some (Binary And)
    | _ -> None)
;;

(**
  Right distributive operators: for each pair op1, op2 it means that:
  (b op2 c) op1 a = (b op1 a) op2 (c op1 a)
*)
let right_distrib =
  Map.of_alist_multi
    (module Operator)
    Binop.
      [ Binary Plus, Max
      ; Binary Times, Plus
      ; Binary Div, Plus
      ; Binary Plus, Min
      ; Binary And, Or
      ]
;;

(** For example, `is_right_distrib Plus Max` is true. *)
let is_right_distrib op1 op2 =
  match Map.find right_distrib (Binary op1) with
  | Some binops -> Binop.(List.mem binops ~equal op2)
  | None -> false
;;

(**
  Associative operators.
*)
let _assoc = OpSet.of_list [ Binary Plus; Binary Times; Binary Min; Binary Max ]

(**
  For example, `is_assoc Plus` is true.
*)
let is_assoc op = Set.mem _assoc (Binary op)

let mk_assoc_with_id (op : Binop.t) (tl : term list) : term option =
  let rec aux t rest =
    match rest with
    | hd :: tl -> aux (mk_bin ~pos:hd.tpos ~typ:(Some hd.ttyp) op t hd) tl
    | [] -> t
  in
  match tl with
  | [] -> get_id_const op
  | [ x ] -> Some x
  | hd :: tl -> Some (aux hd tl)
;;

(**
  Commutative operators.
*)
let _commut =
  OpSet.of_list
    [ Binary Plus
    ; Binary Times
    ; Binary Min
    ; Binary Max
    ; Binary Eq
    ; Binary And
    ; Binary Or
    ]
;;

(**
  For example, `is_commutative Plus` is true.
*)
let is_commutative op = Set.mem _commut op

let concrete_int_op (op : Operator.t) =
  Operator.(
    match op with
    | Binary Plus -> Some ( + )
    | Binary Times -> Some ( * )
    | Binary Minus -> Some ( - )
    | Binary Div -> Some ( / )
    | _ -> None)
;;

(** Set of ints = set of variables. Module is only meant to avoid (Set.... (module Int)) everywhere. *)
module IS = struct
  type t = Set.M(Int).t
  type elt = int

  let empty = Set.empty (module Int)
  let singleton i = Set.singleton (module Int) i
  let of_list l = Set.of_list (module Int) l
  let ( + ) = Set.union
  let ( - ) = Set.diff

  (** Set intersection. *)
  let ( ^ ) = Set.inter

  (** Checks is a set is empty.*)
  let ( ?. ) x = Set.is_empty x

  let ( ~$ ) x = singleton x

  let pp (f : Formatter.t) (s : t) : unit =
    Fmt.(braces (list ~sep:comma int)) f (Set.elements s)
  ;;
end

module RContext = struct
  type t =
    { vars : (int, variable) Hashtbl.t
    ; parent : Context.t
    }

  let empty () = { vars = Hashtbl.create (module Int); parent = Context.create () }
  let create ctx = { vars = Hashtbl.create (module Int); parent = ctx }
  let register_var (ctx : t) (v : variable) = Hashtbl.set ctx.vars ~key:v.vid ~data:v
  let get_var (ctx : t) (id : int) = Hashtbl.find ctx.vars id
end
