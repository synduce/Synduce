open Base
open Lexing
module O = Option

(**
   Variables have unique integer ids but two variables can have the same name.
   Additional information can be added via variable attributes.
   For example, a variable can be a Terminal or a NonTerminal in the context of a
   pattern matching recursion scheme.
*)
module Attributes = struct
  module Elt = struct
    module T = struct
      type t =
        | Anonymous
        | Builtin
        | Terminal
        | NonTerminal of int
      [@@deriving sexp, hash]

      let equal (a : t) (b : t) = Poly.equal a b
      let compare (a : t) (b : t) = Poly.compare a b
    end

    include T
    include Comparator.Make (T)

    let is_non_terminal a =
      match a with
      | NonTerminal _ -> true
      | _ -> false
    ;;
  end

  module AS = Set.M (Elt)
  include AS

  type elt = Elt.t

  let singleton = Set.singleton (module Elt)
  let empty = Set.empty (module Elt)
  let hash_fold_t : t Hash.folder = Set.hash_fold_m__t (module Elt)
end

type variable =
  { vname : string
  ; vid : int
  ; vattrs : Attributes.t
  }
[@@deriving hash]

let hash_fold_position (s : Hash.state) (p : position) =
  Hash.(fold_int (fold_int (fold_string s p.pos_fname) p.pos_lnum) p.pos_cnum)
;;

let dummy_loc : position * position = dummy_pos, dummy_pos
let pp_nice = ref true

(* ----------------------------------------------------- *)
(**
   Terms.
*)

module Binop = struct
  type t =
    | Lt
    | Gt
    | Ge
    | Le
    | Eq
    | Max
    | Min
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    | And
    | Or
    | Implies
    (* Set theory (CVC5) *)
    | SetUnion of RType.t
    | SetIntersection of RType.t
    | SetMinus of RType.t
    | SetMem of RType.t
    | SetSubset of RType.t
    | SetInsert of RType.t
  [@@deriving hash]

  let compare = Poly.compare
  let equal = Poly.equal

  let to_pp_string (op : t) =
    match op with
    | Lt -> "<"
    | Gt -> ">"
    | Ge -> "≥"
    | Le -> "≤"
    | Eq -> "="
    | Max -> "max"
    | Min -> "min"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "×"
    | Div -> "/"
    | Mod -> "%"
    | And -> "&&"
    | Or -> "||"
    | Implies -> "=>"
    (*  *)
    | SetUnion _ -> "union"
    | SetIntersection _ -> "inter"
    | SetMinus _ -> "minus"
    | SetMem _ -> "member"
    | SetSubset _ -> "subset"
    | SetInsert _ -> "insert"
  ;;

  let to_string (op : t) =
    match op with
    | Lt -> "<"
    | Gt -> ">"
    | Ge -> ">="
    | Le -> "<="
    | Eq -> "="
    | Max -> "max"
    | Min -> "min"
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "div"
    | Mod -> "mod"
    | And -> "and"
    | Or -> "or"
    | Implies -> "=>"
    (*  *)
    | SetUnion _ -> "union"
    | SetIntersection _ -> "inter"
    | SetMinus _ -> "minus"
    | SetMem _ -> "member"
    | SetSubset _ -> "subset"
    | SetInsert _ -> "insert"
  ;;

  let of_string ?(typ_param = None) (s : string) : t option =
    match s with
    | "<" -> Some Lt
    | ">" -> Some Gt
    | "<=" -> Some Le
    | ">=" -> Some Ge
    | "=" -> Some Eq
    | "max" -> Some Max
    | "min" -> Some Min
    | "+" -> Some Plus
    | "-" -> Some Minus
    | "*" -> Some Times
    | "div" | "/" -> Some Div
    | "mod" -> Some Mod
    | "and" | "&&" -> Some And
    | "or" | "||" -> Some Or (*  *)
    | "union" -> O.map ~f:(fun typ -> SetUnion typ) typ_param
    | "inter" -> O.map ~f:(fun typ -> SetIntersection typ) typ_param
    | "diff" -> O.map ~f:(fun typ -> SetMinus typ) typ_param
    | "mem" -> O.map ~f:(fun typ -> SetMem typ) typ_param
    | "subset" -> O.map ~f:(fun typ -> SetSubset typ) typ_param
    | "add" -> O.map ~f:(fun typ -> SetInsert typ) typ_param
    | _ -> None
  ;;

  let operand_types (op : t) : (RType.t * RType.t) list =
    RType.(
      match op with
      | Lt | Gt | Ge | Le -> [ TInt, TInt ]
      | Eq -> [ TInt, TInt; TBool, TBool ]
      | Max | Min | Plus | Minus | Times | Div | Mod -> [ TInt, TInt ]
      | Implies | And | Or -> [ TBool, TBool ] (*  *)
      | SetUnion typ -> [ TSet typ, TSet typ ]
      | SetIntersection typ -> [ TSet typ, TSet typ ]
      | SetMinus typ -> [ TSet typ, TSet typ ]
      | SetMem typ -> [ typ, TSet typ ]
      | SetSubset typ -> [ TSet typ, TSet typ ]
      | SetInsert typ -> [ typ, TSet typ ])
  ;;

  let result_type (op : t) =
    RType.(
      match op with
      | Lt | Gt | Ge | Le -> TBool
      | Eq -> TBool
      | Max | Min | Plus | Minus | Times | Div | Mod -> TInt
      | Implies | And | Or -> TBool
      | SetUnion typ -> TSet typ
      | SetIntersection typ -> TSet typ
      | SetMinus typ -> TSet typ
      | SetMem _ -> TBool
      | SetSubset _ -> TBool
      | SetInsert typ -> TSet typ)
  ;;

  let pp (frmt : Formatter.t) (op : t) = Fmt.string frmt (to_pp_string op)

  let is_ac (op : t) =
    match op with
    | Max | Min | Plus | Times | And | Or -> true
    | _ -> false
  ;;
end

module Unop = struct
  type t =
    | Neg
    | Not
    | Abs
    | SetCard of RType.t (** Set cardinality.  *)
    | SetComplement of RType.t (** Set complement. *)
    | SetSingleton of RType.t (** Set singleton *)
  [@@deriving hash]

  let compare = Poly.compare
  let equal = Poly.equal

  let operand_type (op : t) =
    RType.(
      match op with
      | Neg -> TInt
      | Not -> TBool
      | Abs -> TInt
      | SetCard t -> TSet t
      | SetComplement t -> TSet t
      | SetSingleton t -> t)
  ;;

  let result_type (op : t) =
    RType.(
      match op with
      | Neg -> TInt
      | Not -> TBool
      | Abs -> TInt
      | SetCard _ -> TInt
      | SetComplement t -> TSet t
      | SetSingleton t -> TSet t)
  ;;

  let to_pp_string (op : t) =
    match op with
    | Neg -> "-"
    | Not -> "¬"
    | Abs -> "abs"
    | SetCard _ -> "card"
    | SetComplement _ -> "complement"
    | SetSingleton _ -> "singleton"
  ;;

  let to_string (op : t) =
    match op with
    | Neg -> "-"
    | Not -> "not"
    | Abs -> "abs"
    | SetCard _ -> "card"
    | SetComplement _ -> "complement"
    | SetSingleton _ -> "singleton"
  ;;

  let of_string ?(typ_param = None) (s : string) : t option =
    match s with
    | "abs" -> Some Abs
    | "~-" | "-" -> Some Neg
    | "not" -> Some Not
    | "card" -> O.map ~f:(fun x -> SetCard x) typ_param
    | "complement" -> O.map ~f:(fun x -> SetComplement x) typ_param
    | "singleton" -> O.map ~f:(fun x -> SetSingleton x) typ_param
    | _ -> None
  ;;

  let pp frmt op = Fmt.string frmt (to_pp_string op)
end

module Operator = struct
  module T = struct
    type t =
      | Unary of Unop.t
      | Binary of Binop.t
    [@@deriving hash]

    let compare op1 op2 = Poly.compare op1 op2
    let equal op1 op2 = compare op1 op2 = 0

    let sexp_of_t op =
      match op with
      | Unary op -> Sexp.Atom (Unop.to_string op)
      | Binary op -> Sexp.Atom (Binop.to_string op)
    ;;

    let t_of_sexp s =
      match s with
      | Sexp.Atom s ->
        (match Unop.of_string s with
        | Some op -> Unary op
        | None ->
          (match Binop.of_string s with
          | Some op -> Binary op
          | None -> failwith "Not an operator."))
      | _ -> failwith "Not an operator."
    ;;
  end

  module C = Comparator.Make (T)
  include T
  include C

  let pp fmt = function
    | Unary op -> Unop.pp fmt op
    | Binary op -> Binop.pp fmt op
  ;;

  let to_string = function
    | Unary op -> Unop.to_string op
    | Binary op -> Binop.to_string op
  ;;

  let to_pp_string = function
    | Unary op -> Unop.to_pp_string op
    | Binary op -> Binop.to_pp_string op
  ;;

  let is_lia = function
    | Unary (Abs | Neg | Not) -> true
    | Binary (Plus | Minus | Max | Min | And | Or | Eq | Lt | Gt | Le | Ge) -> true
    | _ -> false
  ;;

  let is_bool = function
    | Unary Not -> true
    | Binary (And | Or) -> true
    | _ -> false
  ;;
end

module OpSet = struct
  include Set.M (Operator)

  let empty = Set.empty (module Operator)
  let singleton x = Set.singleton (module Operator) x
  let of_list l = Set.of_list (module Operator) l
  let pp frmt s = Fmt.(pf frmt "@[<hov 2>{%a}@]" (list Operator.pp) (Set.elements s))

  let comparison_operators : t =
    of_list Binop.[ Binary Gt; Binary Ge; Binary Le; Binary Lt; Binary Eq ]
  ;;
end

module Constant = struct
  type t =
    | CInt of int
    | CChar of char
    | CTrue
    | CFalse
    | CEmptySet of RType.t
  [@@deriving hash]

  let compare c1 c2 =
    match c1, c2 with
    | CInt i1, CInt i2 -> Int.compare i1 i2
    | CChar c1, CChar c2 -> Char.compare c1 c2
    | CTrue, CTrue -> 0
    | CTrue, CFalse -> 1
    | CFalse, CTrue -> -1
    | CFalse, CFalse -> 0
    | CEmptySet t1, CEmptySet t2 -> Poly.compare t1 t2
    | CEmptySet _, _ -> 1
    | _, CEmptySet _ -> -1
    | CInt _, _ -> 1
    | _, CInt _ -> -1
    | CChar _, _ -> 1
    | _, CChar _ -> -1
  ;;

  let equal c1 c2 = compare c1 c2 = 0
  let of_int i = CInt i
  let of_char c = CChar c
  let of_bool b = if b then CTrue else CFalse

  let _if c t f =
    match c with
    | CTrue -> t
    | _ -> f
  ;;

  let type_of (c : t) =
    match c with
    | CInt _ -> RType.TInt
    | CChar _ -> RType.TChar
    | CTrue | CFalse -> RType.TBool
    | CEmptySet t0 -> RType.TSet t0
  ;;

  let pp (frmt : Formatter.t) (c : t) =
    match c with
    | CInt i -> Fmt.int frmt i
    | CChar c -> Fmt.char frmt c
    | CTrue -> Fmt.bool frmt true
    | CFalse -> Fmt.bool frmt false
    | CEmptySet _ -> Fmt.string frmt "empty"
  ;;
end

(** Simple patterns for function arguments: a fpattern is either a variable or a tuple of patterns.  *)
type fpattern =
  | FPatAny
  | FPatVar of variable
  | FPatTup of fpattern list
[@@deriving hash]

(** More complex patterns are used in match-cases.
  In the current implementation, this is only used as a way to translate a PMRS back to
  a set of mutually recursive functions.
*)
type pattern =
  | PatAny
  | PatVar of variable
  | PatConstant of Constant.t
  | PatTuple of pattern list
  | PatConstr of string * pattern list
[@@deriving hash]

type termkind =
  | TApp of term * term list (** A function application. *)
  | TBin of Binop.t * term * term (** A binary operation. *)
  | TBox of term (** A boxed term, used in transformation to "protect" its contents.*)
  | TConst of Constant.t (** A constant. *)
  | TData of string * term list (** A datatype constructor. *)
  | TFun of fpattern list * term
      (** A function, where each argument capture by a f-pattern. *)
  | TIte of term * term * term (** A conditional. *)
  | TMatch of term * match_case list (** A pattern matching construct. *)
  | TSel of term * int (** A tuple projection. TSet(TTup tl, i) is List.nth tl i. *)
  | TTup of term list (** A tuple. *)
  | TUn of Unop.t * term (** A unary operation. *)
  | TVar of variable (** A variable. *)

and match_case = pattern * term

and term =
  { tpos : position * position
  ; tkind : termkind
  ; ttyp : RType.t
  }
[@@deriving hash]

type function_descr =
  { f_var : variable
  ; f_args : pattern list
  ; f_body : term
  }

type spec =
  { ensures : term option
  ; requires : term option
  }

type specs = (int, spec) Hashtbl.t

module Context = struct
  type t =
    { names : Alpha.t
    ; types : RType.env
    ; globals : (string, variable * fpattern list * term option * term) Hashtbl.t
    ; vartypes : (int, RType.t) Hashtbl.t
    ; varnames : (int, string) Hashtbl.t
    ; specs : specs
    }

  let create () =
    { names = Alpha.create ()
    ; types = RType.create ()
    ; globals = Hashtbl.create (module String)
    ; vartypes = Hashtbl.create (module Int)
    ; varnames = Hashtbl.create (module Int)
    ; specs = Hashtbl.create (module Int)
    }
  ;;

  let copy (ctx : t) =
    { names = Alpha.copy ctx.names
    ; types = RType.copy ctx.types
    ; globals = Hashtbl.copy ctx.globals
    ; vartypes = Hashtbl.copy ctx.vartypes
    ; varnames = Hashtbl.copy ctx.varnames
    ; specs = Hashtbl.copy ctx.specs
    }
  ;;

  let add_global
      (ctx : t)
      ~(key : string)
      ~(data : variable * fpattern list * term option * term)
    =
    Hashtbl.add ctx.globals ~key ~data
  ;;

  let find_global_var (ctx : t) (s : string) : variable option =
    match Hashtbl.find ctx.globals s with
    | Some (v, _, _, _) -> Some v
    | None -> None
  ;;

  let find_global (ctx : t) (s : string) = Hashtbl.find ctx.globals s
end
