open Base
open Lexing
open Utils
module O = Option

let dummy_loc : position * position = dummy_pos, dummy_pos

(* ----------------------------------------------------- *)

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
      [@@deriving sexp]

      let equal (a : t) (b : t) = Poly.equal a b
      let compare (a : t) (b : t) = Poly.compare a b
      let hash = Hashtbl.hash
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
end

type variable =
  { vname : string
  ; vid : int
  ; vattrs : Attributes.t
  }

(* Module of variables *)
module Variable = struct
  module T = struct
    type t = variable

    let sexp_of_t v = Sexp.(List [ Atom "var"; Atom v.vname ])

    (* Variables are compared by their id, not their name. *)
    let compare x y = compare x.vid y.vid

    (* Variables are compared by their id, not their name. *)
    let equal x y = x.vid = y.vid
    let ( = ) x y = equal x y
    let hash = Hashtbl.hash
  end

  include T
  include Comparator.Make (T)

  let types_tbl : (int, RType.t) Hashtbl.t = Hashtbl.create (module Int)
  let names_tbl : (int, string) Hashtbl.t = Hashtbl.create (module Int)

  let clear () =
    Hashtbl.clear types_tbl;
    Hashtbl.clear names_tbl
  ;;

  let _print_info = ref false
  let vtype_assign (v : t) (t : RType.t) = Hashtbl.set types_tbl ~key:v.vid ~data:t
  let vtype (v : t) = Hashtbl.find types_tbl v.vid
  let id (v : t) = v.vid
  let get_name (vid : int) = Hashtbl.find names_tbl vid

  let clear_type (v : t) =
    let new_t = RType.get_fresh_tvar () in
    vtype_assign v new_t
  ;;

  (* `vtype_or_new v` returns the type of variable v, or assigns a fresh type variable
      as its type if it doesn't have one.
      The type of a variable will automatically be assigned to satisfy constraints
      produced during type inference.
  *)
  let vtype_or_new (v : t) =
    match vtype v with
    | Some x -> x
    | None ->
      let new_t = RType.get_fresh_tvar () in
      vtype_assign v new_t;
      new_t
  ;;

  let update_var_types (tsubs : (RType.t * RType.t) list) =
    Hashtbl.map_inplace ~f:(fun t -> RType.sub_all tsubs t) types_tbl
  ;;

  let mk ?(attrs = Attributes.empty) ?(t = None) (name : string) =
    let v =
      Alpha.mk_with_id (-1) name (fun vid -> { vname = name; vid; vattrs = attrs })
    in
    Hashtbl.set names_tbl ~key:v.vid ~data:v.vname;
    (match t with
    | Some t -> vtype_assign v t
    | None -> vtype_assign v (RType.get_fresh_tvar ()));
    v
  ;;

  let is_anonymous (v : t) : bool = Set.mem v.vattrs Anonymous
  let make_anonymous (v : t) : t = { v with vattrs = Set.add v.vattrs Anonymous }
  let is_builtin (v : t) : bool = Set.mem v.vattrs Builtin
  let make_builtin (v : t) : t = { v with vattrs = Set.add v.vattrs Builtin }
  let has_attr (attr : Attributes.elt) (v : t) = Set.mem v.vattrs attr
  let is_nonterminal (v : t) = Set.exists ~f:Attributes.Elt.is_non_terminal v.vattrs
  let same_name (v : t) (v2 : t) : bool = String.equal v.vname v2.vname

  let pp (frmt : Formatter.t) (v : t) =
    if !_print_info
    then
      Fmt.(
        pf
          frmt
          "(%s%a: %a)"
          v.vname
          (styled `Faint (fun fmt i -> pf fmt "@%i" i))
          v.vid
          (styled `Italic RType.pp)
          (vtype_or_new v))
    else Fmt.(pf frmt "%s" v.vname)
  ;;

  let pp_id (frmt : Formatter.t) (v : t) = Fmt.(pf frmt "%s{%i}" v.vname v.vid)

  let pp_typed (frmt : Formatter.t) (v : t) =
    Fmt.(pf frmt "%s : %a" v.vname RType.pp (vtype_or_new v))
  ;;

  let free (var : t) =
    Alpha.forget var.vid var.vname;
    Hashtbl.remove types_tbl var.vid
  ;;

  let print_summary (frmt : Formatter.t) () =
    Utils.Log.(debug (wrap "Variables in tables:"));
    let le =
      Hashtbl.fold (Alpha.get_ids ()) ~init:0 ~f:(fun ~key:_ ~data l ->
          max l (String.length data))
    in
    Fmt.(pf frmt "\t  ID | %*s : TYPE@." le "NAME");
    Fmt.(pf frmt "\t---------------------------@.");
    Hashtbl.iteri (Alpha.get_ids ()) ~f:(fun ~key ~data ->
        match Hashtbl.find types_tbl key with
        | Some t -> Fmt.(pf frmt "\t%4i | %*s : %a@." key le data RType.pp t)
        | None -> Fmt.(pf frmt "\t%4i | %*s : ??@." key le data))
  ;;
end

module VarSet = struct
  module V = Set.M (Variable)
  include V

  type elt = variable

  let empty = Set.empty (module Variable)
  let singleton = Set.singleton (module Variable)
  let union_list = Set.union_list (module Variable)
  let elements vs = Set.elements vs
  let of_list = Set.of_list (module Variable)
  let map f vs : V.t = of_list (List.map ~f (elements vs))
  let max_elt = Set.max_elt
  let min_elt = Set.max_elt
  let find_by_id vs id : elt option = max_elt (Set.filter ~f:(fun elt -> elt.vid = id) vs)
  let has_name vs name : bool = Set.exists ~f:(fun elt -> String.equal elt.vname name) vs

  let find_by_name vs name : elt option =
    max_elt (Set.filter ~f:(fun elt -> String.equal elt.vname name) vs)
  ;;

  let vids_of_vs vs : int list = List.map ~f:(fun vi -> vi.vid) (elements vs)
  let has_vid vs id : bool = List.mem ~equal:( = ) (vids_of_vs vs) id
  let bindings vs = List.map ~f:(fun elt -> elt.vid, elt) (elements vs)
  let names vs = List.map ~f:(fun elt -> elt.vname) (elements vs)

  let record vs =
    List.map
      ~f:(fun elt -> elt.vname, Option.value_exn (Variable.vtype elt))
      (elements vs)
  ;;

  let to_env vs =
    Map.of_alist_reduce
      (module String)
      ~f:(fun b1 _ -> b1)
      (List.map ~f:(fun v -> v.vname, v) (elements vs))
  ;;

  (** Returns a list of pairs of variable, fresh copy of the variable. *)
  let prime (vs : t) : (variable * variable) list =
    Set.fold
      ~f:(fun subs v ->
        let primed_name = Alpha.fresh ~s:(v.vname ^ "_") () in
        let primed_var = Variable.mk ~t:(Variable.vtype v) primed_name in
        (v, primed_var) :: subs)
      ~init:[]
      vs
  ;;

  let add_prefix vs prefix =
    of_list (List.map ~f:(fun v -> { v with vname = prefix ^ v.vname }) (elements vs))
  ;;

  let iset vs ilist =
    of_list (List.filter ~f:(fun vi -> List.mem ilist vi.vid ~equal:( = )) (elements vs))
  ;;

  let pp_var_names formatter vs =
    Fmt.(list ~sep:comma Variable.pp formatter (elements vs))
  ;;

  let pp formatter vs =
    Fmt.(list ~sep:sp (parens Variable.pp_typed) formatter (elements vs))
  ;;

  let dump formatter vs = Fmt.Dump.(list Variable.pp_id formatter (elements vs))

  let of_sh sh =
    Hashtbl.fold
      sh
      ~f:(fun ~key:_ ~data:v vset -> Set.add vset v)
      ~init:(Set.empty (module Variable))
  ;;
end

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
  ;;

  let of_string (s : string) : t option =
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
    | "/" -> Some Div
    | "mod" -> Some Mod
    | "and" | "&&" -> Some And
    | "or" | "||" -> Some Or
    | _ -> None
  ;;

  let operand_types (op : t) : (RType.t * RType.t) list =
    RType.(
      match op with
      | Lt | Gt | Ge | Le -> [ TInt, TInt ]
      | Eq -> [ TInt, TInt; TBool, TBool ]
      | Max | Min | Plus | Minus | Times | Div | Mod -> [ TInt, TInt ]
      | And | Or -> [ TBool, TBool ])
  ;;

  let result_type (op : t) =
    RType.(
      match op with
      | Lt | Gt | Ge | Le -> TBool
      | Eq -> TBool
      | Max | Min | Plus | Minus | Times | Div | Mod -> TInt
      | And | Or -> TBool)
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

  let compare = Poly.compare
  let equal = Poly.equal

  let operand_type (op : t) =
    match op with
    | Neg -> RType.TInt
    | Not -> RType.TBool
    | Abs -> RType.TInt
  ;;

  let result_type (op : t) =
    match op with
    | Neg -> RType.TInt
    | Not -> RType.TBool
    | Abs -> RType.TInt
  ;;

  let to_pp_string (op : t) =
    match op with
    | Neg -> "-"
    | Not -> "¬"
    | Abs -> "abs"
  ;;

  let to_string (op : t) =
    match op with
    | Neg -> "-"
    | Not -> "not"
    | Abs -> "abs"
  ;;

  let of_string (s : string) : t option =
    match s with
    | "abs" -> Some Abs
    | "~-" | "-" -> Some Neg
    | "not" -> Some Not
    | _ -> None
  ;;

  let pp frmt op = Fmt.string frmt (to_pp_string op)
end

module Operator = struct
  module T = struct
    type t =
      | Unary of Unop.t
      | Binary of Binop.t

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
    | CTrue
    | CFalse

  let compare c1 c2 =
    match c1, c2 with
    | CInt i1, CInt i2 -> Int.compare i1 i2
    | CTrue, CTrue -> 0
    | CTrue, CFalse -> 1
    | CFalse, CTrue -> -1
    | CFalse, CFalse -> 0
    | CInt _, _ -> 1
    | _, CInt _ -> -1
  ;;

  let equal c1 c2 = compare c1 c2 = 0
  let of_int i = CInt i
  let of_bool b = if b then CTrue else CFalse

  let _if c t f =
    match c with
    | CTrue -> t
    | _ -> f
  ;;

  let type_of (c : t) =
    match c with
    | CInt _ -> RType.TInt
    | CTrue | CFalse -> RType.TBool
  ;;

  let pp (frmt : Formatter.t) (c : t) =
    match c with
    | CInt i -> Fmt.int frmt i
    | CTrue -> Fmt.bool frmt true
    | CFalse -> Fmt.bool frmt false
  ;;
end

(** Simple patterns for function arguments: a fpattern is either a variable or a tuple of patterns.  *)
type fpattern =
  | FPatAny
  | FPatVar of variable
  | FPatTup of fpattern list

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

type function_descr =
  { f_var : variable
  ; f_args : pattern list
  ; f_body : term
  }

let _globals : (string, Variable.t * fpattern list * term option * term) Hashtbl.t =
  Hashtbl.create (module String)
;;

let find_global (s : string) : variable option =
  match Hashtbl.find _globals s with
  | Some (v, _, _, _) -> Some v
  | None -> None
;;

(* F-Patterns helpers *)

let rec fpat_ty (fp : fpattern) : RType.t =
  match fp with
  | FPatVar v -> Variable.vtype_or_new v
  | FPatTup tl -> RType.(TTup (List.map ~f:fpat_ty tl))
  | FPatAny -> RType.get_fresh_tvar ()
;;

let rec fpat_vars (fp : fpattern) : VarSet.t =
  match fp with
  | FPatVar v -> VarSet.singleton v
  | FPatTup tl -> VarSet.union_list (List.map ~f:fpat_vars tl)
  | FPatAny -> VarSet.empty
;;

let pattern_of_term (t : term) =
  let rec aux t =
    match t.tkind with
    | TVar x -> PatVar x
    | TTup tl -> PatTuple (List.map ~f:aux tl)
    | TData (c, args) -> PatConstr (c, List.map ~f:aux args)
    | TConst c -> PatConstant c
    | _ -> PatAny
  in
  aux t
;;

(* ============================================================================================= *)
(*                        CONSTRUCTION FUNCTIONS                                                 *)
(* ============================================================================================= *)

let mk_pat_any = PatAny
let mk_pat_var v = PatVar v
let mk_pat_const c = PatConstant c
let mk_pat_tuple l = PatTuple l
let mk_pat_constr c l = PatConstr (c, l)

let mk_var ?(pos = dummy_loc) (v : variable) : term =
  { tpos = pos; tkind = TVar v; ttyp = Variable.vtype_or_new v }
;;

let var_or_none (t : term) : variable option =
  match t.tkind with
  | TVar x -> Some x
  | _ -> None
;;

let ext_var_or_none (t : term) : variable list option =
  match t.tkind with
  | TVar x -> Some [ x ]
  | TTup tl ->
    let tl' = List.map ~f:var_or_none tl in
    if List.for_all ~f:Option.is_some tl' then Some (List.filter_opt tl') else None
  | _ -> None
;;

let mk_const ?(pos = dummy_loc) (c : Constant.t) =
  let ctyp =
    match c with
    | Constant.CInt _ -> RType.TInt
    | Constant.CTrue | Constant.CFalse -> RType.TBool
  in
  { tpos = pos; tkind = TConst c; ttyp = ctyp }
;;

let mk_box ?(pos = dummy_loc) (t : term) : term =
  { tpos = pos; tkind = TBox t; ttyp = t.ttyp }
;;

let mk_app ?(pos = dummy_loc) ?(typ = None) (f : term) (x : term list) =
  let typ =
    match typ with
    | Some t -> t
    | None ->
      let args_t, ret_t = RType.fun_typ_unpack f.ttyp in
      (match List.drop args_t (List.length x) with
      | [] -> ret_t
      | _ as remaining_args -> RType.fun_typ_pack remaining_args ret_t)
  in
  { tpos = pos; tkind = TApp (f, x); ttyp = typ }
;;

let mk_app_v ?(pos = dummy_loc) ?(typ = None) (f : variable) (x : term list) =
  let typ =
    match typ with
    | Some t -> t
    | None ->
      let args_t, ret_t = RType.fun_typ_unpack (Variable.vtype_or_new f) in
      (match List.drop args_t (List.length x) with
      | [] -> ret_t
      | _ as remaining_args -> RType.fun_typ_pack remaining_args ret_t)
  in
  { tpos = pos; tkind = TApp (mk_var f, x); ttyp = typ }
;;

let mk_bin ?(pos = dummy_loc) ?(typ = None) (op : Binop.t) (t1 : term) (t2 : term) =
  let typ =
    match typ with
    | Some t -> t
    | None -> Binop.result_type op
  in
  { tpos = pos; tkind = TBin (op, t1, t2); ttyp = typ }
;;

(** Applies an associative operator to a list of terms recursively.
  Returns None if the list of arguments is empty.
*)
let mk_assoc (op : Binop.t) (tl : term list) : term option =
  let rec aux t rest =
    match rest with
    | hd :: tl -> aux (mk_bin ~pos:hd.tpos ~typ:(Some hd.ttyp) op t hd) tl
    | [] -> t
  in
  match tl with
  | [] -> None
  | [ x ] -> Some x
  | hd :: tl -> Some (aux hd tl)
;;

let mk_data ?(pos = dummy_loc) (c : string) (xs : term list) =
  let typ =
    match RType.type_of_variant c with
    | Some (t, _) -> t
    | _ -> failwith (Fmt.str "Trying to construct term with unknown constructor %s" c)
  in
  { tpos = pos; tkind = TData (c, xs); ttyp = typ }
;;

let mk_fun ?(pos = dummy_loc) (args : fpattern list) (body : term) =
  let targs = List.map ~f:(fun t -> fpat_ty t) args in
  { tpos = pos; tkind = TFun (args, body); ttyp = RType.fun_typ_pack targs body.ttyp }
;;

let mk_ite ?(pos = dummy_loc) ?(typ = None) (c : term) (th : term) (el : term) =
  let typ =
    match typ with
    | Some t -> t
    | None -> th.ttyp
  in
  { tpos = pos; tkind = TIte (c, th, el); ttyp = typ }
;;

let mk_tup ?(pos = dummy_loc) (l : term list) =
  { tpos = pos; tkind = TTup l; ttyp = RType.TTup (List.map ~f:(fun t -> t.ttyp) l) }
;;

let mk_sel ?(pos = dummy_loc) ?(typ = None) (t : term) (i : int) =
  let typ =
    match typ with
    | Some t -> t
    | None ->
      (match t.ttyp with
      | RType.TTup tl ->
        (match List.nth tl i with
        | Some x -> x
        | None -> RType.get_fresh_tvar ())
      | _ -> RType.get_fresh_tvar ())
  in
  { tpos = pos; tkind = TSel (t, i); ttyp = typ }
;;

let mk_un ?(pos = dummy_loc) ?(typ = None) (op : Unop.t) (t : term) =
  let typ =
    match typ with
    | Some t -> t
    | None -> RType.get_fresh_tvar ()
  in
  { tpos = pos; tkind = TUn (op, t); ttyp = typ }
;;

let mk_match ?(pos = dummy_loc) (x : term) (cases : match_case list) =
  let typ =
    match cases with
    | (_, t) :: _ -> t.ttyp
    | _ -> RType.get_fresh_tvar ()
  in
  { tpos = pos; tkind = TMatch (x, cases); ttyp = typ }
;;

let mk_let
    ?(pos = dummy_loc)
    ?(typ = None)
    (bindings : (variable * term) list)
    (body : term)
    : term
  =
  let var_args, term_args = List.unzip bindings in
  let t =
    mk_app ~pos (mk_fun ~pos (List.map ~f:(fun x -> FPatVar x) var_args) body) term_args
  in
  match typ with
  | Some typ -> { t with ttyp = typ }
  | None -> { t with ttyp = body.ttyp }
;;

let term_of_pattern (p : pattern) : term =
  let rec aux p =
    match p with
    | PatVar v -> mk_var v
    | PatTuple tl -> mk_tup (List.map ~f:aux tl)
    | PatConstant c -> mk_const c
    | PatConstr (c, args) -> mk_data c (List.map ~f:aux args)
    | PatAny -> mk_var (Variable.mk "_")
  in
  aux p
;;

let rec fpat_to_term fp =
  match fp with
  | FPatVar v -> mk_var v
  | FPatTup tl -> mk_tup (List.map ~f:fpat_to_term tl)
  | _ -> mk_tup []
;;

let fpat_sub (fp1 : fpattern) (fp2 : fpattern) =
  let rec aux (fp1, fp2) =
    match fp1, fp2 with
    | FPatVar v1, FPatVar v2 -> [ mk_var v1, mk_var v2 ]
    | FPatTup tl1, FPatTup tl2 ->
      (match List.zip tl1 tl2 with
      | Ok l -> List.concat (List.map ~f:aux l)
      | _ -> failwith "no sub")
    | FPatVar v1, _ -> [ mk_var v1, fpat_to_term fp2 ]
    | _, FPatVar v2 -> [ fpat_to_term fp1, mk_var v2 ]
    | _, _ -> []
  in
  try Some (aux (fp1, fp2)) with
  | _ -> None
;;

let fpat_sub_all fp1s fp2s =
  match List.zip fp1s fp2s with
  | Ok z ->
    (try
       Some
         (List.fold
            ~init:[]
            ~f:(fun l (a, b) ->
              match fpat_sub a b with
              | Some subs -> l @ subs
              | None -> failwith "done")
            z)
     with
    | _ -> None)
  | _ -> None
;;

let sexp_of_term (_ : term) = Sexp.Atom "TODO"

let rec mk_composite_base_type ?(prefix = "") (t : RType.t) : term =
  match t with
  | RType.TInt -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "i") ()))
  | RType.TBool -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "b") ()))
  | RType.TString -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "s") ()))
  | RType.TChar -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "c") ()))
  | RType.TTup tl -> mk_tup (List.map ~f:mk_composite_base_type tl)
  | RType.TNamed _ -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "l") ()))
  | _ -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh ~s:(prefix ^ "p") ()))
;;

(* | RType.TFun (_, _) | RType.TParam (_, _) | RType.TVar _ ->
    failwith Fmt.(str "mk_composite_base_type: %a is not a base type." RType.pp t) *)

(* ============================================================================================= *)
(*                             EQUALITY                                                          *)
(* ============================================================================================= *)

let rec term_compare (t1 : term) (t2 : term) : int =
  match t1.tkind, t2.tkind with
  | TConst c1, TConst c2 -> Constant.compare c1 c2
  | TVar v1, TVar v2 -> Variable.compare v1 v2
  | TBox t1', TBox t2' -> term_compare t1' t2'
  | TBox t1', _ -> term_compare t1' t2
  | _, TBox t2' -> term_compare t1 t2'
  | TData (c1, args1), TData (c2, args2) ->
    let c = String.compare c1 c2 in
    if c = 0 then List.compare term_compare args1 args2 else c
  | TApp (f1, args1), TApp (f2, args2) ->
    let c = term_compare f1 f2 in
    if c = 0 then List.compare term_compare args1 args2 else c
  | TBin (b1, t11, t12), TBin (b2, t21, t22) ->
    let c = Binop.compare b1 b2 in
    if c = 0
    then (
      let c' = term_compare t11 t21 in
      if c' = 0 then term_compare t12 t22 else c')
    else c
  | TUn (u1, t11), TUn (u2, t21) ->
    let c = Unop.compare u1 u2 in
    if c = 0 then term_compare t11 t21 else c
  | TFun (fargs1, body1), TFun (fargs2, body2) ->
    let c = compare (List.length fargs1) (List.length fargs2) in
    if c = 0
    then (
      match fpat_sub_all fargs1 fargs2 with
      | Some subs -> term_compare body1 (substitution subs body2)
      | None -> -1)
    else c
  | TTup tl1, TTup tl2 -> List.compare term_compare tl1 tl2
  | TSel (t1', i1), TSel (t2', i2) ->
    let c = compare i1 i2 in
    if c = 0 then term_compare t1' t2' else c
  | TMatch (t1', cases1), TMatch (t2', cases2) ->
    let c = term_compare t1' t2' in
    if c = 0
    then List.compare term_compare (snd (List.unzip cases1)) (snd (List.unzip cases2))
    else c
  | _, _ -> Poly.compare t1 t2

and substitution (substs : (term * term) list) (term : term) : term =
  let rec aux (_t : term) =
    match List.Assoc.find substs ~equal:term_equal _t with
    | Some t' -> t'
    | None ->
      let new_kind =
        match _t.tkind with
        | TBin (b1, t1, t2) -> TBin (b1, aux t1, aux t2)
        | TBox t -> TBox (aux t)
        | TUn (u, t1) -> TUn (u, aux t1)
        | TIte (c, tt, tf) -> TIte (aux c, aux tt, aux tf)
        | TTup tl -> TTup (List.map ~f:aux tl)
        | TSel (t, i) -> TSel (aux t, i)
        | TFun (args, body) -> TFun (args, aux body)
        | TApp (f, args) -> TApp (aux f, List.map ~f:aux args)
        | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
        | TMatch (tm, cases) -> TMatch (aux tm, List.map ~f:(fun (c, t) -> c, aux t) cases)
        | TVar _ | TConst _ -> _t.tkind
      in
      { _t with tkind = new_kind }
  in
  aux term

and term_equal t1 t2 = term_compare t1 t2 = 0

let mk_with_fresh_vars (vs : VarSet.t) (t : term) : VarSet.t * term =
  let substs =
    let f var =
      let fresh =
        let t = Some (Variable.vtype_or_new var) in
        Variable.mk ~t (Alpha.fresh ~s:var.vname ())
      in
      fresh, (mk_var var, mk_var fresh)
    in
    List.map ~f (Set.elements vs)
  in
  VarSet.of_list (List.map ~f:first substs), substitution (List.map ~f:second substs) t
;;

module VarMap = struct
  module M = Map.M (Variable)
  include M

  type 'value t = 'value M.t

  let empty = Map.empty (module Variable)
  let keyset (m : 'a t) : VarSet.t = VarSet.of_list (Map.keys m)

  let assigns_varname (m : 'a t) (s : string) =
    Map.existsi ~f:(fun ~key ~data:_ -> String.equal key.vname s) m
  ;;

  let singleton (v : variable) (elt : 'a) = Map.singleton (module Variable) v elt
  let of_alist (al : (variable * 'a) list) = Map.of_alist (module Variable) al
  let of_alist_exn (al : (variable * 'a) list) = Map.of_alist_exn (module Variable) al
  let to_subst (map : term t) = List.map ~f:(fun (v, t) -> mk_var v, t) (Map.to_alist map)
  let ( $@ ) (map : term t) (v : variable) = Map.find map v
end

(* ============================================================================================= *)
(*                              TRANFORMATION / REDUCTION  UTILS                                 *)
(* ============================================================================================= *)

(**
   `rewrite_with f t` rewrites the term t by applying the rule f bottom-up.
*)
let rewrite_with (f : term -> term) (t : term) =
  let rec aux t0 =
    let tk = t0.tkind in
    let tk' =
      match tk with
      | TApp (func, args) -> TApp (aux func, List.map ~f:aux args)
      | TBin (op, t1, t2) -> TBin (op, aux t1, aux t2)
      | TBox _ -> tk (* Do not rewrite Tbox *)
      | TConst _ -> (f t0).tkind
      | TUn (op, t1) -> TUn (op, aux t1)
      | TVar _ -> tk
      | TIte (c, t1, t2) -> TIte (aux c, aux t1, aux t2)
      | TTup tl -> TTup (List.map ~f:aux tl)
      | TSel (t, i) -> TSel (aux t, i)
      | TFun (fargs, body) -> TFun (fargs, aux body)
      | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
      | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases)
    in
    f { t0 with tkind = tk' }
  in
  aux t
;;

(**
   `rewrite_top_down f t` rewrites the term t by applying the rule f top-down.
*)
let rewrite_top_down (f : term -> term option) (t : term) =
  let rec aux t0 =
    match f t0 with
    | Some t0' -> t0'
    | None ->
      let tk' =
        match t0.tkind with
        | TBin (op, t1, t2) -> TBin (op, aux t1, aux t2)
        | TUn (op, t1) -> TUn (op, aux t1)
        | TConst _ -> t0.tkind
        | TVar _ -> t0.tkind
        | TBox _ -> t0.tkind
        | TIte (c, t1, t2) -> TIte (aux c, aux t1, aux t2)
        | TTup tl -> TTup (List.map ~f:aux tl)
        | TSel (t, i) -> TSel (aux t, i)
        | TFun (fargs, body) -> TFun (fargs, aux body)
        | TApp (func, args) -> TApp (aux func, List.map ~f:aux args)
        | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
        | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases)
      in
      { t0 with tkind = tk' }
  in
  aux t
;;

(**
   `rewrite_accum ~init ~f t` rewrites the term t by applying the rule f in a top-down manner,
   but as opposed to `rewrite_top_down` the function `f` can use an accumulator that accumulates
   information during the traversal.
*)
let rewrite_accum ~(init : 'a) ~(f : 'a -> term -> (term, 'a) Either.t) (t : term) =
  let rec aux a t0 =
    match f a t0 with
    | Either.First t0' -> t0'
    | Either.Second a' ->
      let tk' =
        match t0.tkind with
        | TBin (op, t1, t2) -> TBin (op, aux a' t1, aux a' t2)
        | TUn (op, t1) -> TUn (op, aux a' t1)
        | TConst _ -> t0.tkind
        | TVar _ -> t0.tkind
        | TBox _ -> t0.tkind
        | TIte (c, t1, t2) -> TIte (aux a' c, aux a' t1, aux a' t2)
        | TTup tl -> TTup (List.map ~f:(aux a') tl)
        | TSel (t, i) -> TSel (aux a' t, i)
        | TFun (fargs, body) -> TFun (fargs, aux a' body)
        | TApp (func, args) -> TApp (aux a' func, List.map ~f:(aux a') args)
        | TData (cstr, args) -> TData (cstr, List.map ~f:(aux a') args)
        | TMatch (tm, cases) -> TMatch (aux a' tm, list_map_snd ~f:(aux a') cases)
      in
      { t0 with tkind = tk' }
  in
  aux init t
;;

let rewrite_types t_subs =
  Variable.update_var_types t_subs;
  rewrite_with (fun _t -> { _t with ttyp = RType.sub_all t_subs _t.ttyp })
;;

(**
   `reduce ~init ~case ~join t` reduces the term by reducing each leaf to `init`, and at each node
    of the syntax tree, using `join` to merge the values. In a top-down traversal, if `case` returns
    `Some a` then the subterm is not recrusively reduced, but the value `a` is used instead.
*)
let reduce
    ~(init : 'a)
    ~(case : (term -> 'a) -> term -> 'a option)
    ~(join : 'a -> 'a -> 'a)
    (t : term)
    : 'a
  =
  let rec aux (t : term) : 'a =
    match case aux t with
    | Some x -> x
    | None ->
      (match t.tkind with
      | TBin (_, t1, t2) -> join (aux t1) (aux t2)
      | TUn (_, t1) -> aux t1
      | TConst _ -> init
      | TVar _ -> init
      | TBox t -> aux t
      | TIte (c, a, b) -> join (aux c) (join (aux a) (aux b))
      | TTup tl -> aux_l tl
      | TSel (t, _) -> aux t
      | TFun (_, body) -> aux body
      | TApp (func, args) -> join (aux func) (aux_l args)
      | TData (_, args) -> aux_l args
      | TMatch (tm, cases) -> join (aux tm) (aux_l (snd (List.unzip cases))))
  and aux_l l = List.fold ~init ~f:join (List.map ~f:aux l) in
  aux t
;;

let transform ~(case : (term -> term) -> term -> term option) (t : term) : term =
  let rec aux (t : term) : 'a =
    match case aux t with
    | Some x -> x
    | None ->
      { t with
        tkind =
          (match t.tkind with
          | TBin (bo, t1, t2) -> TBin (bo, aux t1, aux t2)
          | TUn (uo, t1) -> TUn (uo, aux t1)
          | TConst _ -> t.tkind
          | TVar _ -> t.tkind
          | TBox _ -> t.tkind
          | TIte (c, a, b) -> TIte (aux c, aux a, aux b)
          | TTup tl -> TTup (aux_l tl)
          | TSel (t, i) -> TSel (aux t, i)
          | TFun (args, body) -> TFun (args, aux body)
          | TApp (func, args) -> TApp (aux func, aux_l args)
          | TData (cstr, args) -> TData (cstr, aux_l args)
          | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases))
      }
  and aux_l l = List.map ~f:aux l in
  aux t
;;

let transform_at_depth
    (min_depth : int)
    ~(case : (term -> term) -> term -> term option)
    (t : term)
    : term
  =
  let rec aux (d : int) (t : term) : term =
    if d >= min_depth
    then (
      match case (aux d) t with
      | Some x -> x
      | None -> drec d t)
    else drec d t
  and drec d t =
    let aux = aux (d + 1) in
    let aux_l l = List.map ~f:aux l in
    { t with
      tkind =
        (match t.tkind with
        | TBin (bo, t1, t2) -> TBin (bo, aux t1, aux t2)
        | TUn (uo, t1) -> TUn (uo, aux t1)
        | TConst _ -> t.tkind
        | TVar _ -> t.tkind
        | TBox _ -> t.tkind
        | TIte (c, a, b) -> TIte (aux c, aux a, aux b)
        | TTup tl -> TTup (aux_l tl)
        | TSel (t, i) -> TSel (aux t, i)
        | TFun (args, body) -> TFun (args, aux body)
        | TApp (func, args) -> TApp (aux func, aux_l args)
        | TData (cstr, args) -> TData (cstr, aux_l args)
        | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases))
    }
  in
  aux 0 t
;;

let transform_info ~(f : term -> term) (t : term) : term =
  let rec aux (t : term) : 'a =
    { (f t) with
      tkind =
        (match t.tkind with
        | TBin (bo, t1, t2) -> TBin (bo, aux t1, aux t2)
        | TUn (uo, t1) -> TUn (uo, aux t1)
        | TConst _ -> t.tkind
        | TVar _ -> t.tkind
        | TBox _ -> t.tkind
        | TIte (c, a, b) -> TIte (aux c, aux a, aux b)
        | TTup tl -> TTup (aux_l tl)
        | TSel (t, i) -> TSel (aux t, i)
        | TFun (args, body) -> TFun (args, aux body)
        | TApp (func, args) -> TApp (aux func, aux_l args)
        | TData (cstr, args) -> TData (cstr, aux_l args)
        | TMatch (tm, cases) -> TMatch (aux tm, list_map_snd ~f:aux cases))
    }
  and aux_l l = List.map ~f:aux l in
  aux t
;;

let remove_boxes (t : term) =
  transform
    ~case:(fun f t ->
      match t.tkind with
      | TBox t -> Some (f t)
      | _ -> None)
    t
;;

let var_count (typ : RType.t) (t : term) =
  let case _ t =
    match t.tkind with
    | TVar v -> Some (if Poly.equal (Variable.vtype_or_new v) typ then 1 else 0)
    | _ -> None
  in
  reduce ~init:0 ~case ~join:(fun a b -> a + b) t
;;

let var_count_compare typ (t1 : term) (t2 : term) =
  compare (var_count typ t1) (var_count typ t2)
;;

let term_size (t : term) =
  let case _ t =
    match t.tkind with
    | TConst _ | TVar _ -> Some 1
    | _ -> None
  in
  reduce ~init:0 ~case ~join:(fun a b -> a + b + 1) t
;;

let term_size_compare (t1 : term) (t2 : term) = compare (term_size t1) (term_size t2)

let term_height (t : term) : int =
  let rec aux (t : term) : 'a =
    match t.tkind with
    | TBin (_, t1, t2) -> 1 + max (aux t1) (aux t2)
    | TUn (_, t1) -> 1 + aux t1
    | TConst _ -> 1
    | TVar _ -> 1
    | TBox t -> aux t
    | TIte (c, a, b) -> 1 + max (aux c) (max (aux a) (aux b))
    | TTup tl -> aux_l tl + 1
    | TSel (t, _) -> aux t
    | TFun (_, body) -> aux body
    | TApp (func, args) -> 1 + max (aux func) (aux_l args)
    | TData (_, args) -> 1 + aux_l args
    | TMatch (tm, cases) -> 1 + max (aux tm) (aux_l (snd (List.unzip cases)))
  and aux_l l = List.fold ~init:0 ~f:(fun a t -> max a (aux t)) l in
  aux t
;;

let term_height_compare (t1 : term) (t2 : term) =
  compare (term_height t1) (term_height t2)
;;

(** [proj_var v] returns the tuple term (v.0, v.1, ..., v.n) if [v] is a variable of
    tuple type with n compoenents, otherwise returns v as a term.
  *)
let proj_var (v : variable) : term =
  let rec p t =
    match t with
    | RType.TTup tl -> mk_tup (List.mapi ~f:(fun i ti -> mk_sel (p ti) i) tl)
    | _ -> mk_var v
  in
  p (Variable.vtype_or_new v)
;;

(** [tuplify t] transforms every subterm of [t] that has a tuple type into a tuple of
  projections. For example if t is a term of tuple type with n components, it returns
  (t.1, t.2, .., t.n).
*)
let tuplify (t : term) =
  let case _ t =
    match t.tkind with
    | TVar v ->
      (match Variable.vtype_or_new v with
      | TTup _ -> Some (proj_var v)
      | _ -> None)
    | _ ->
      (match t.ttyp with
      | RType.TTup tl -> Some (mk_tup (List.mapi ~f:(fun i _ -> mk_sel t i) tl))
      | _ -> None)
  in
  transform ~case t
;;

(* ============================================================================================= *)
(*                                    PRETTY PRINTERS                                            *)
(* ============================================================================================= *)
open Fmt

let rec pp_fpattern (frmt : Formatter.t) (fp : fpattern) =
  match fp with
  | FPatVar x -> Variable.pp frmt x
  | FPatTup tl -> pf frmt "%a" (box (parens (list ~sep:comma pp_fpattern))) tl
  | FPatAny -> pf frmt "_"
;;

let rec pp_pattern (frmt : Formatter.t) (p : pattern) =
  match p with
  | PatAny -> pf frmt "_"
  | PatVar v -> Variable.pp frmt v
  | PatConstant c -> Constant.pp frmt c
  | PatConstr (c, args) ->
    (match args with
    | [] -> string frmt c
    | _ -> pf frmt "%a(%a)" (styled `Italic string) c (list ~sep:comma pp_pattern) args)
  | PatTuple tl -> (parens (list ~sep:comma pp_pattern)) frmt tl
;;

let pp_term (frmt : Formatter.t) (x : term) =
  let rec aux (paren : bool) (frmt : Formatter.t) (t : term) =
    match t.tkind with
    | TConst c -> pf frmt "%a" Constant.pp c
    | TVar v -> pf frmt "%a" Variable.pp v
    | TBox t -> (aux paren) frmt t
    | TBin (op, t1, t2) ->
      (match op with
      | Binop.Max | Binop.Min ->
        if paren
        then pf frmt "@[<hov 2>(%a@;%a@;%a)@]" Binop.pp op (aux true) t1 (aux true) t2
        else pf frmt "@[<hov 2>%a@;%a@;%a@]" Binop.pp op (aux true) t1 (aux true) t2
      | _ ->
        if paren
        then pf frmt "@[<hov 2>(%a@;%a@;%a)@]" (aux true) t1 Binop.pp op (aux true) t2
        else pf frmt "@[<hov 2>%a@;%a@;%a@]" (aux true) t1 Binop.pp op (aux true) t2)
    | TUn (op, t1) ->
      if paren
      then pf frmt "@[<hov 2>(%a@;%a)@]" Unop.pp op (aux true) t1
      else pf frmt "@[<hov 2>%a@;%a@]" Unop.pp op (aux true) t1
    | TIte (c, t1, t2) ->
      if paren
      then
        pf
          frmt
          "@[<hov 2>(%a@;?@;%a@;:@;%a)@]"
          (aux false)
          c
          (aux false)
          t1
          (aux false)
          t2
      else
        pf frmt "@[<hov 2>%a@;?@;%a@;:@;%a@]" (aux false) c (aux false) t1 (aux false) t2
    | TTup tl -> pf frmt "@[<hov 2>(%a)@]" (list ~sep:comma (box (aux false))) tl
    | TSel (t, i) -> pf frmt "@[<hov 2>%a.%i@]" (aux true) t i
    (* Some application terms can be printed like let .. = .. in .. for readability. *)
    | TApp ({ tkind = TFun ([ arg_fpat ], body); _ }, [ app_arg ]) ->
      pf
        frmt
        "@[<v>@[let %a@[<hov 2> =@;%a@]@;in@]@;@[<hov 2>%a@]@]"
        pp_fpattern
        arg_fpat
        (aux false)
        app_arg
        (aux false)
        body
    | TFun (args, body) ->
      if paren
      then
        pf
          frmt
          "@[<hov 2>(fun %a -> @;%a)@]"
          (list ~sep:sp pp_fpattern)
          args
          (aux false)
          body
      else
        pf frmt "@[<hov 2>fun %a -> %a@]" (list ~sep:sp pp_fpattern) args (aux false) body
    | TApp (func, args) ->
      if paren
      then pf frmt "@[<hov 2>(%a@ %a)@]" (aux true) func (list ~sep:sp (aux true)) args
      else pf frmt "@[<hov 2>%a@ %a@]" (aux true) func (list ~sep:sp (aux true)) args
    | TData (cstr, args) ->
      if List.length args = 0
      then pf frmt "%a" (styled (`Fg `Green) string) cstr
      else
        pf
          frmt
          "%a(%a)"
          (styled (`Fg `Green) string)
          cstr
          (list ~sep:comma (aux false))
          args
    | TMatch (tm, cases) ->
      pf
        frmt
        "@[<hov 2>@[match %a with@]@;@[<v>%a@]@]"
        (aux false)
        tm
        (list ~sep:sp (fun fmt (l, r) ->
             pf fmt "@[<hov 2>| %a ->@;%a@]" (box pp_pattern) l (box (aux false)) r))
        cases
  in
  aux false frmt x
;;

let pp_subs (f : Formatter.t) (subs : (term * term) list) : unit =
  Fmt.(
    pf
      f
      "@[<hov 2>%a@]"
      (fun f l ->
        List.iter ~f:(fun (t1, t2) -> pf f "@[[%a -> %a]@]" pp_term t1 pp_term t2) l)
      subs)
;;

let pp_function_descr (fmt : Formatter.t) (fd : function_descr) : unit =
  let _, t_out = RType.fun_typ_unpack (Variable.vtype_or_new fd.f_var) in
  pf
    fmt
    "@[<hov 2>@[let rec %s %a : %a@] =@;@[%a@]@]"
    fd.f_var.vname
    (list ~sep:sp pp_pattern)
    fd.f_args
    RType.pp
    t_out
    pp_term
    fd.f_body
;;

(* ============================================================================================= *)
(*                                  TYPE INFERENCE                                               *)
(* ============================================================================================= *)

let infer_type (t : term) : term * RType.substitution =
  let rec aux t0 =
    let eloc = t0.tpos in
    let merge_subs = RType.merge_subs eloc in
    match t0.tkind with
    | TBox t -> aux t
    | TBin (op, t1, t2) ->
      let t_t1, c_t1 = aux t1
      and t_t2, c_t2 = aux t2 in
      (* Collect possible operand types. *)
      let possible_opty = Binop.operand_types op in
      (* Find a pair of operaand types that work. *)
      let maybe_t =
        List.find_map possible_opty ~f:(fun (ta, tb) ->
            match RType.unify [ t_t1.ttyp, ta; t_t2.ttyp, tb ] with
            | Ok subs ->
              Some
                ( mk_bin ~pos:t0.tpos ~typ:(Some (Binop.result_type op)) op t_t1 t_t2
                , merge_subs subs (merge_subs c_t1 c_t2) )
            | Error _ -> None)
      in
      (match maybe_t with
      | Some x -> x
      | None ->
        Log.error_msg Fmt.(str "Cannot infer type of binary expression %a." pp_term t0);
        Log.error_msg
          Fmt.(
            str
              "%a has type %a, and %a has type %a, expected pair to be one  of %a."
              pp_term
              t1
              RType.pp
              t_t1.ttyp
              pp_term
              t2
              RType.pp
              t_t2.ttyp
              (list ~sep:sp (parens (pair ~sep:comma RType.pp RType.pp)))
              possible_opty);
        failwith "Type inference failure.")
    | TUn (op, t1) ->
      let t_t1, c_t1 = aux t1 in
      (match RType.unify [ t_t1.ttyp, Unop.operand_type op ] with
      | Ok subs ->
        mk_un ~pos:t0.tpos ~typ:(Some (Unop.result_type op)) op t_t1, merge_subs subs c_t1
      | Error e ->
        Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
        Log.error_msg Fmt.(str "Cannot infer type of unary expression %a." pp_term t0);
        Log.error_msg
          Fmt.(
            str
              "%a has type %a, expected type %a."
              pp_term
              t1
              RType.pp
              t_t1.ttyp
              RType.pp
              (Unop.operand_type op));
        Log.loc_fatal_errmsg eloc "Type inference failure.")
    | TConst c -> { t0 with ttyp = Constant.type_of c }, []
    | TVar v ->
      let tv = Variable.vtype_or_new v in
      (match RType.unify [ tv, t0.ttyp ] with
      | Ok res -> { t0 with ttyp = tv }, res
      | Error e ->
        Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
        failwith "Type inference failure")
    | TIte (c, t1, t2) ->
      let t_c, c_c = aux c
      and t_t1, c_t1 = aux t1
      and t_t2, c_t2 = aux t2 in
      (match RType.unify [ t_c.ttyp, RType.TBool; t_t1.ttyp, t_t2.ttyp ] with
      | Ok subs ->
        ( mk_ite ~pos:t0.tpos ~typ:(Some t_t1.ttyp) t_c t_t1 t_t2
        , merge_subs subs (merge_subs c_c (merge_subs c_t1 c_t2)) )
      | Error e ->
        Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
        Log.error_msg
          Fmt.(str "ite(%a, %a, %a)." RType.pp c.ttyp RType.pp t1.ttyp RType.pp t2.ttyp);
        failwith "Type inference failure.")
    | TTup tl ->
      let term_l, c_l = List.unzip (List.map ~f:aux tl) in
      mk_tup ~pos:t0.tpos term_l, merge_subs (List.concat c_l) []
    | TSel (t, i) ->
      let t_c, c_c = aux t in
      (match t_c.ttyp with
      | RType.TTup tl ->
        (match List.nth tl i with
        | Some tout -> mk_sel ~pos:t0.tpos ~typ:(Some tout) t_c i, c_c
        | None ->
          Log.error_msg Fmt.(str "In tuple acessor %a, index out of bounds." pp_term t0);
          failwith "Type inference: tuple acessor, accesed tuple of wrong type.")
      | _ ->
        Log.error_msg
          Fmt.(str "Tuple accessor argument %a of type %a." pp_term t_c RType.pp t_c.ttyp);
        failwith "Type inference: tuple accessor on type acessor.")
    | TFun (args, body) ->
      let t_body, c_body = aux body in
      mk_fun ~pos:t0.tpos args t_body, c_body
    | TApp (func, fargs) ->
      let t_func, c_func = aux func
      and t_args, c_args = List.unzip (List.map ~f:aux fargs) in
      let argst = List.map ~f:(fun t -> t.ttyp) t_args in
      let csub = RType.(mkv c_func @ mkv (List.concat c_args)) in
      (match t_func.ttyp with
      | RType.TFun (_, _) ->
        let func_targs, func_tout = RType.fun_typ_unpack t_func.ttyp in
        (match List.zip func_targs argst with
        | Ok typ_pairs ->
          (match RType.(unify (csub @ typ_pairs)) with
          | Ok subs -> mk_app ~pos:t0.tpos ~typ:(Some func_tout) t_func t_args, subs
          | Error e ->
            Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
            Log.loc_fatal_errmsg
              eloc
              (Fmt.str
                 "Type inference failure: could not unify types in application %a(%a)"
                 pp_term
                 func
                 (box (list ~sep:sp pp_term))
                 fargs))
        | Unequal_lengths ->
          Log.loc_fatal_errmsg
            eloc
            (Fmt.str
               "Type inference failure: %a expects %i argument, given %i: %a."
               pp_term
               func
               (List.length func_targs)
               (List.length fargs)
               (box (list ~sep:sp pp_term))
               fargs))
      | RType.TVar f_tvar ->
        (* |- f_tvar : (_ -> _ -> _ .. -> _) -> 'b *)
        let t_out = RType.get_fresh_tvar () in
        let tf = RType.fun_typ_pack argst t_out in
        (match RType.(unify (csub @ [ tf, RType.TVar f_tvar ])) with
        | Ok subs -> mk_app ~pos:t0.tpos ~typ:(Some t_out) t_func t_args, subs
        | Error e ->
          Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
          failwith "Type inference failure.")
      | _ as tf ->
        if List.length fargs > 0
        then
          Log.loc_fatal_errmsg
            eloc
            (Fmt.str
               "Type inference failure: in %a, could not type %a as function."
               pp_term
               t
               RType.pp
               tf)
        else t_func, c_func)
    | TData (cstr, args) ->
      (match RType.type_of_variant cstr with
      | Some (tout, targs) ->
        let t_args, c_args = List.unzip (List.map ~f:aux args) in
        (match List.zip targs (List.map ~f:(fun term -> term.ttyp) t_args) with
        | Ok pairs ->
          (match RType.unify (pairs @ RType.mkv (List.concat c_args)) with
          | Ok subs -> { t0 with ttyp = tout; tkind = TData (cstr, t_args) }, subs
          | Error e ->
            Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
            Log.loc_fatal_errmsg
              eloc
              (Fmt.str
                 "Type inference failure: could not unify %s arguments %a."
                 cstr
                 (list ~sep:comma pp_term)
                 t_args))
        | Unequal_lengths ->
          Log.loc_fatal_errmsg
            eloc
            (Fmt.str
               "Type inference failure: could not match %s arguments: %a and %a."
               cstr
               (list ~sep:comma pp_term)
               t_args
               (list ~sep:comma RType.pp)
               targs))
      | None ->
        Log.loc_fatal_errmsg
          eloc
          Fmt.(str "Type inference failure: could not find type of %s." cstr))
    | TMatch (t, cases) ->
      let t_t, c_t = aux t in
      let per_case (pat, rhs) =
        let pat_term, pat_c = aux (term_of_pattern pat) in
        match RType.(unify [ pat_term.ttyp, t_t.ttyp ]) with
        | Ok subs ->
          let t_branch, c_branch = aux rhs in
          (pat, t_branch), merge_subs (merge_subs c_t pat_c) (merge_subs c_branch subs)
        | Error _ ->
          Log.loc_fatal_errmsg
            eloc
            Fmt.(str "Type inference failure: could unify match case")
      in
      let f (cases, subs, t) ((pat, rhs), sub) =
        match RType.unify ([ t, rhs.ttyp ] @ RType.mkv subs) with
        | Ok s -> cases @ [ pat, rhs ], merge_subs s sub, rhs.ttyp
        | Error _ ->
          Log.loc_fatal_errmsg
            eloc
            Fmt.(str "Type inference failure: could unify match case")
      in
      (match List.map ~f:per_case cases with
      | [] -> failwith "Empty match case?"
      | ((hd_pat, hd_rhs), sub1) :: tl ->
        let cases, subs, typ =
          List.fold ~f ~init:([ hd_pat, hd_rhs ], sub1, hd_rhs.ttyp) tl
        in
        { t0 with ttyp = typ; tkind = TMatch (t_t, cases) }, subs)
  in
  let t', subs = aux t in
  match RType.unify (RType.mkv subs) with
  | Ok merge_subs ->
    let tsubs = RType.mkv merge_subs in
    rewrite_types tsubs t', merge_subs
  | Error e ->
    Log.error_msg Fmt.(str "Error: %a" Sexp.pp_hum e);
    Log.loc_fatal_errmsg t'.tpos "Could not infer type."
;;

let erase_term_type (t : term) =
  let f t = { t with ttyp = RType.get_fresh_tvar () } in
  transform_info ~f t
;;

let type_of (t : term) = t.ttyp

(* ============================================================================================= *)
(*                                  SETS OF TERMS                                                *)
(* ============================================================================================= *)

module Terms = struct
  module E = struct
    type t = term

    let compare t1 t2 =
      let c = compare (term_size t1) (term_size t2) in
      if c = 0 then term_compare t1 t2 else c
    ;;

    let equal = term_equal
    let sexp_of_t = sexp_of_term
  end

  include E
  module C = Comparator.Make (E)
  include C

  let substs_of_alist (alist : (variable * term) list) : (term * term) list =
    List.map ~f:(fun (a, b) -> mk_var a, b) alist
  ;;

  (* Term building shortcuts. *)

  (** Create a term equal to the addition two terms. *)
  let ( + ) : t -> t -> t = mk_bin Binop.Plus

  (** Create a term equal to the substraction of two terms. *)
  let ( - ) : t -> t -> t = mk_bin Binop.Minus

  (** Create a term equal to the multiplication two terms. *)
  let ( * ) : t -> t -> t = mk_bin Binop.Times

  (** Create a term equal to the division two terms. *)
  let ( / ) : t -> t -> t = mk_bin Binop.Div

  (** Create a term equal to the disjunction of two terms. *)
  let ( || ) : t -> t -> t = mk_bin Binop.Or

  (** Create a term equal to the conjuction of two terms. *)
  let ( && ) : t -> t -> t = mk_bin Binop.And

  (** Create a term equal to the > comparison of two terms. *)
  let ( > ) : t -> t -> t = mk_bin Binop.Gt

  (** Create a term equal to the >= comparison of two terms. *)
  let ( >= ) : t -> t -> t = mk_bin Binop.Ge

  (** Create a term equal to the <= comparison of two terms. *)
  let ( <= ) : t -> t -> t = mk_bin Binop.Le

  (** Create a term equal to the < comparison of two terms. *)
  let ( < ) : t -> t -> t = mk_bin Binop.Lt

  (** Create a term equal to the equality of two terms. *)
  let ( == ) : t -> t -> t = mk_bin Binop.Eq

  (** Create a term equal to the max of two terms. *)
  let max : t -> t -> t = mk_bin Binop.Max

  (** Create a term equal to the min of two terms. *)
  let min : t -> t -> t = mk_bin Binop.Min

  (** Create an integer constant term. *)
  let int (i : int) : t = mk_const (Constant.of_int i)

  (** Create a boolean constant term. *)
  let bool (b : bool) : t = mk_const (Constant.of_bool b)

  (** Create a negation of a term. *)
  let ( ~! ) (t : term) : t = mk_un Unop.Not t

  (** Create an if-then-else term. *)
  let ite : t -> t -> t -> t = mk_ite

  (** Create a term from a variable.  *)
  let ( ~^ ) : variable -> t = mk_var

  (**
    Infers the type of the term, and returns the term with the correct types
    assigned.
  *)
  let typed (te : t) : t = fst (infer_type te)
end

module TermSet = struct
  module S = Set.M (Terms)
  include S

  let empty = Set.empty (module Terms)
  let map (s : S.t) = Set.map (module Terms) s
  let singleton = Set.singleton (module Terms)
  let of_list = Set.of_list (module Terms)
  let union_list = Set.union_list (module Terms)

  let pp (f : Formatter.t) (s : t) =
    Fmt.(pf f "@[{%a}@]" (list ~sep:comma pp_term) (Set.elements s))
  ;;
end

let pp_term_set (f : Formatter.t) (s : TermSet.t) =
  (braces (list ~sep:comma (box pp_term))) f (Set.elements s)
;;
