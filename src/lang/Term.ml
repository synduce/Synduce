open Base
open Lexing
open Utils
module O = Option

let dummy_loc : position * position = (dummy_pos, dummy_pos)

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
      type t = Anonymous | Builtin | Terminal | NonTerminal of int [@@deriving sexp]

      let equal (a : t) (b : t) = Poly.equal a b

      let compare (a : t) (b : t) = Poly.compare a b

      let hash = Hashtbl.hash
    end

    include T
    include Comparator.Make (T)

    let is_non_terminal a = match a with NonTerminal _ -> true | _ -> false
  end

  module AS = Set.M (Elt)
  include AS

  type elt = Elt.t

  let singleton = Set.singleton (module Elt)

  let empty = Set.empty (module Elt)
end

type variable = { vname : string; vid : int; vattrs : Attributes.t }

let sexp_of_variable v = Sexp.(List [ Atom "var"; Atom v.vname ])

(*  Atom (Int.to_string v.vid); sexp_of_typ v.vtype]) *)

let pp_variable f v = Fmt.((styled (`Fg `Cyan) string) f v.vname)

let pp_id_var f v = Fmt.(pf f "(%i : %s)" v.vid v.vname)

let dump_variable f v = Fmt.(string f v.vname)

module Variable = struct
  module T = struct
    type t = variable

    let compare x y = compare x.vid y.vid

    let equal x y = x.vid = y.vid

    let ( = ) x y = equal x y

    let sexp_of_t = sexp_of_variable

    let hash = Hashtbl.hash
  end

  include T
  include Comparator.Make (T)

  let _types : (int, RType.t) Hashtbl.t = Hashtbl.create (module Int)

  let vtype_assign (v : variable) (t : RType.t) = Hashtbl.set _types ~key:v.vid ~data:t

  let vtype (v : variable) = Hashtbl.find _types v.vid

  let vtype_or_new (v : variable) =
    match vtype v with
    | Some x -> x
    | None ->
        let new_t = RType.get_fresh_tvar () in
        vtype_assign v new_t;
        new_t

  let update_var_types (tsubs : (RType.t * RType.t) list) =
    Hashtbl.map_inplace _types ~f:(fun t -> RType.sub_all tsubs t)

  let mk ?(attrs = Attributes.empty) ?(t = None) (name : string) =
    let v = Alpha.mk_with_id (-1) name (fun vid -> { vname = name; vid; vattrs = attrs }) in
    (match t with Some t -> vtype_assign v t | None -> vtype_assign v (RType.get_fresh_tvar ()));
    v

  let is_anonymous (v : t) : bool = Set.mem v.vattrs Anonymous

  let make_anonymous (v : t) : t = { v with vattrs = Set.add v.vattrs Anonymous }

  let is_builtin (v : t) : bool = Set.mem v.vattrs Builtin

  let make_builtin (v : t) : t = { v with vattrs = Set.add v.vattrs Builtin }

  let has_attr (attr : Attributes.elt) (v : t) = Set.mem v.vattrs attr

  let is_nonterminal (v : t) = Set.exists ~f:Attributes.Elt.is_non_terminal v.vattrs

  let same_name (v : t) (v2 : t) : bool = String.equal v.vname v2.vname

  let pp (frmt : Formatter.t) (v : t) = Fmt.(pf frmt "%s" v.vname)

  let pp_typed (frmt : Formatter.t) (v : t) =
    Fmt.(pf frmt "%s : %a" v.vname RType.pp (vtype_or_new v))

  let print_summary (frmt : Formatter.t) () =
    Utils.Log.(debug (wrap "Variables in tables:"));
    let le =
      Hashtbl.fold (Alpha.get_ids ()) ~init:0 ~f:(fun ~key:_ ~data l -> max l (String.length data))
    in
    Fmt.(pf frmt "\t  ID | %*s : TYPE@." le "NAME");
    Fmt.(pf frmt "\t---------------------------@.");
    Hashtbl.iteri (Alpha.get_ids ()) ~f:(fun ~key ~data ->
        match Hashtbl.find _types key with
        | Some t -> Fmt.(pf frmt "\t%4i | %*s : %a@." key le data RType.pp t)
        | None -> Fmt.(pf frmt "\t%4i | %*s : ??@." key le data))
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

  let map f vs : t = of_list (List.map ~f (elements vs))

  let max_elt = Set.max_elt

  let min_elt = Set.max_elt

  let find_by_id vs id : elt option = max_elt (Set.filter ~f:(fun elt -> elt.vid = id) vs)

  let has_name vs name : bool = Set.exists ~f:(fun elt -> String.equal elt.vname name) vs

  let find_by_name vs name : elt option =
    max_elt (Set.filter ~f:(fun elt -> String.equal elt.vname name) vs)

  let vids_of_vs vs : int list = List.map ~f:(fun vi -> vi.vid) (elements vs)

  let has_vid vs id : bool = List.mem ~equal:( = ) (vids_of_vs vs) id

  let bindings vs = List.map ~f:(fun elt -> (elt.vid, elt)) (elements vs)

  let names vs = List.map ~f:(fun elt -> elt.vname) (elements vs)

  let record vs =
    List.map ~f:(fun elt -> (elt.vname, Option.value_exn (Variable.vtype elt))) (elements vs)

  let to_env vs =
    Map.of_alist_reduce
      (module String)
      ~f:(fun b1 _ -> b1)
      (List.map ~f:(fun v -> (v.vname, v)) (elements vs))

  let add_prefix vs prefix =
    of_list (List.map ~f:(fun v -> { v with vname = prefix ^ v.vname }) (elements vs))

  let iset vs ilist =
    of_list (List.filter ~f:(fun vi -> List.mem ilist vi.vid ~equal:( = )) (elements vs))

  let pp_var_names formatter vs = Fmt.(list ~sep:comma pp_variable formatter (elements vs))

  let pp formatter vs = Fmt.(list ~sep:sp (parens Variable.pp_typed) formatter (elements vs))

  let dump formatter vs = Fmt.Dump.(list pp_id_var formatter (elements vs))

  let of_sh sh =
    Hashtbl.fold sh
      ~f:(fun ~key:_ ~data:v vset -> Set.add vset v)
      ~init:(Set.empty (module Variable))
end

(* ----------------------------------------------------- *)
(**
   Terms.
*)

module Binop = struct
  type t = Lt | Gt | Ge | Le | Eq | Max | Min | Plus | Minus | Times | Div | Mod | And | Or

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

  let operand_types (op : t) =
    RType.(
      match op with
      | Lt | Gt | Ge | Le -> (TInt, TInt)
      | Eq -> (TInt, TInt)
      | Max | Min | Plus | Minus | Times | Div | Mod -> (TInt, TInt)
      | And | Or -> (TBool, TBool))

  let result_type (op : t) =
    RType.(
      match op with
      | Lt | Gt | Ge | Le -> TBool
      | Eq -> TBool
      | Max | Min | Plus | Minus | Times | Div | Mod -> TInt
      | And | Or -> TBool)

  let pp (frmt : Formatter.t) (op : t) = Fmt.string frmt (to_pp_string op)

  let is_ac (op : t) = match op with Max | Min | Plus | Times | And | Or -> true | _ -> false
end

module Unop = struct
  type t = Neg | Not | Abs

  let compare = Poly.compare

  let equal = Poly.equal

  let operand_type (op : t) =
    match op with Neg -> RType.TInt | Not -> RType.TBool | Abs -> RType.TInt

  let result_type (op : t) =
    match op with Neg -> RType.TInt | Not -> RType.TBool | Abs -> RType.TInt

  let to_pp_string (op : t) = match op with Neg -> "-" | Not -> "¬" | Abs -> "abs"

  let to_string (op : t) = match op with Neg -> "-" | Not -> "not" | Abs -> "abs"

  let of_string (s : string) : t option =
    match s with "-" -> Some Neg | "not" -> Some Not | "abs " -> Some Abs | _ -> None

  let pp frmt op = Fmt.string frmt (to_pp_string op)
end

module Operator = struct
  module T = struct
    type t = Unary of Unop.t | Binary of Binop.t

    let compare op1 op2 = Poly.compare op1 op2

    let equal op1 op2 = compare op1 op2 = 0

    let sexp_of_t op =
      match op with
      | Unary op -> Sexp.Atom (Unop.to_string op)
      | Binary op -> Sexp.Atom (Binop.to_string op)

    let t_of_sexp s =
      match s with
      | Sexp.Atom s -> (
          match Unop.of_string s with
          | Some op -> Unary op
          | None -> (
              match Binop.of_string s with
              | Some op -> Binary op
              | None -> failwith "Not an operator."))
      | _ -> failwith "Not an operator."
  end

  module C = Comparator.Make (T)
  include T
  include C

  let pp fmt = function Unary op -> Unop.pp fmt op | Binary op -> Binop.pp fmt op

  let to_string = function Unary op -> Unop.to_string op | Binary op -> Binop.to_string op

  let to_pp_string = function
    | Unary op -> Unop.to_pp_string op
    | Binary op -> Binop.to_pp_string op

  let is_lia = function
    | Unary (Abs | Neg | Not) -> true
    | Binary (Plus | Minus | Max | Min | And | Or) -> true
    | _ -> false

  let is_bool = function Unary Not -> true | Binary (And | Or) -> true | _ -> false
end

module OpSet = struct
  include Set.M (Operator)

  let empty = Set.empty (module Operator)

  let singleton x = Set.singleton (module Operator) x

  let of_list l = Set.of_list (module Operator) l

  let pp frmt s = Fmt.(pf frmt "@[<hov 2>{%a}@]" (list Operator.pp) (Set.elements s))

  let comparison_operators : t =
    of_list Binop.[ Binary Gt; Binary Ge; Binary Le; Binary Lt; Binary Eq ]
end

module Constant = struct
  type t = CInt of int | CTrue | CFalse

  let compare c1 c2 =
    match (c1, c2) with
    | CInt i1, CInt i2 -> Int.compare i1 i2
    | CTrue, CTrue -> 0
    | CTrue, CFalse -> 1
    | CFalse, CTrue -> -1
    | CFalse, CFalse -> 0
    | CInt _, _ -> 1
    | _, CInt _ -> -1

  let equal c1 c2 = compare c1 c2 = 0

  let of_int i = CInt i

  let of_bool b = if b then CTrue else CFalse

  let _if c t f = match c with CTrue -> t | _ -> f

  let type_of (c : t) = match c with CInt _ -> RType.TInt | CTrue | CFalse -> RType.TBool

  let pp (frmt : Formatter.t) (c : t) =
    match c with
    | CInt i -> Fmt.int frmt i
    | CTrue -> Fmt.bool frmt true
    | CFalse -> Fmt.bool frmt false
end

type fpattern = PatVar of variable | PatTup of fpattern list

type termkind =
  | TBin of Binop.t * term * term
  | TUn of Unop.t * term
  | TConst of Constant.t
  | TVar of variable
  | TIte of term * term * term
  | TTup of term list
  | TSel of term * int
  | TFun of fpattern list * term
  | TApp of term * term list
  | TData of string * term list

and term = { tpos : position * position; tkind : termkind; ttyp : RType.t }

let _globals : (int, Variable.t * fpattern list * term option * term) Hashtbl.t =
  Hashtbl.create (module Int)

(* F-Patterns helpers *)

let rec fpat_ty fp =
  match fp with
  | PatVar v -> Variable.vtype_or_new v
  | PatTup tl -> RType.(TTup (List.map ~f:fpat_ty tl))

let rec fpat_vars fp =
  match fp with
  | PatVar v -> VarSet.singleton v
  | PatTup tl -> VarSet.union_list (List.map ~f:fpat_vars tl)

(* ============================================================================================= *)
(*                        CONSTRUCTION FUNCTIONS                                                 *)
(* ============================================================================================= *)

let mk_var ?(pos = dummy_loc) (v : variable) : term =
  { tpos = pos; tkind = TVar v; ttyp = Variable.vtype_or_new v }

let var_or_none (t : term) : variable option = match t.tkind with TVar x -> Some x | _ -> None

let ext_var_or_none (t : term) : variable list option =
  match t.tkind with
  | TVar x -> Some [ x ]
  | TTup tl ->
      let tl' = List.map ~f:var_or_none tl in
      if List.for_all ~f:Option.is_some tl' then Some (List.filter_opt tl') else None
  | _ -> None

let mk_const ?(pos = dummy_loc) (c : Constant.t) =
  let ctyp =
    match c with Constant.CInt _ -> RType.TInt | Constant.CTrue | Constant.CFalse -> RType.TBool
  in
  { tpos = pos; tkind = TConst c; ttyp = ctyp }

let mk_app ?(pos = dummy_loc) ?(typ = None) (f : term) (x : term list) =
  let typ = match typ with Some t -> t | None -> RType.get_fresh_tvar () in
  { tpos = pos; tkind = TApp (f, x); ttyp = typ }

let mk_app_v ?(pos = dummy_loc) ?(typ = None) (f : variable) (x : term list) =
  let typ = match typ with Some t -> t | None -> RType.get_fresh_tvar () in
  { tpos = pos; tkind = TApp (mk_var f, x); ttyp = typ }

let mk_bin ?(pos = dummy_loc) ?(typ = None) (op : Binop.t) (t1 : term) (t2 : term) =
  let typ = match typ with Some t -> t | None -> RType.get_fresh_tvar () in
  { tpos = pos; tkind = TBin (op, t1, t2); ttyp = typ }

let mk_assoc (op : Binop.t) (tl : term list) : term option =
  let rec aux t rest =
    match rest with hd :: tl -> aux (mk_bin ~pos:hd.tpos ~typ:(Some hd.ttyp) op t hd) tl | [] -> t
  in
  match tl with [] -> None | [ x ] -> Some x | hd :: tl -> Some (aux hd tl)

let mk_data ?(pos = dummy_loc) (c : string) (xs : term list) =
  let typ = match RType.type_of_variant c with Some (t, _) -> t | _ -> RType.get_fresh_tvar () in
  { tpos = pos; tkind = TData (c, xs); ttyp = typ }

let mk_fun ?(pos = dummy_loc) (args : fpattern list) (body : term) =
  let targs = List.map ~f:(fun t -> fpat_ty t) args in
  { tpos = pos; tkind = TFun (args, body); ttyp = RType.fun_typ_pack targs body.ttyp }

let mk_ite ?(pos = dummy_loc) ?(typ = None) (c : term) (th : term) (el : term) =
  let typ = match typ with Some t -> t | None -> RType.get_fresh_tvar () in
  { tpos = pos; tkind = TIte (c, th, el); ttyp = typ }

let mk_tup ?(pos = dummy_loc) (l : term list) =
  { tpos = pos; tkind = TTup l; ttyp = RType.TTup (List.map ~f:(fun t -> t.ttyp) l) }

let mk_sel ?(pos = dummy_loc) ?(typ = None) (t : term) (i : int) =
  let typ =
    match typ with
    | Some t -> t
    | None -> (
        match t.ttyp with
        | RType.TTup tl -> (
            match List.nth tl i with Some x -> x | None -> RType.get_fresh_tvar ())
        | _ -> RType.get_fresh_tvar ())
  in
  { tpos = pos; tkind = TSel (t, i); ttyp = typ }

let mk_un ?(pos = dummy_loc) ?(typ = None) (op : Unop.t) (t : term) =
  let typ = match typ with Some t -> t | None -> RType.get_fresh_tvar () in
  { tpos = pos; tkind = TUn (op, t); ttyp = typ }

let rec fpat_to_term fp =
  match fp with PatVar v -> mk_var v | PatTup tl -> mk_tup (List.map ~f:fpat_to_term tl)

let fpat_sub fp1 fp2 =
  let rec aux (fp1, fp2) =
    match (fp1, fp2) with
    | PatVar v1, PatVar v2 -> [ (mk_var v1, mk_var v2) ]
    | PatTup tl1, PatTup tl2 -> (
        match List.zip tl1 tl2 with
        | Ok l -> List.concat (List.map ~f:aux l)
        | _ -> failwith "no sub")
    | PatVar v1, _ -> [ (mk_var v1, fpat_to_term fp2) ]
    | _, PatVar v2 -> [ (fpat_to_term fp1, mk_var v2) ]
  in
  try Some (aux (fp1, fp2)) with _ -> None

let fpat_sub_all fp1s fp2s =
  match List.zip fp1s fp2s with
  | Ok z -> (
      try
        Some
          (List.fold ~init:[]
             ~f:(fun l (a, b) ->
               match fpat_sub a b with Some subs -> l @ subs | None -> failwith "done")
             z)
      with _ -> None)
  | _ -> None

let sexp_of_term (_ : term) = Sexp.Atom "TODO"

let rec mk_composite_base_type (t : RType.t) : term =
  match t with
  | RType.TInt -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh "i_"))
  | RType.TBool -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh "b_"))
  | RType.TString -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh "s_"))
  | RType.TChar -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh "c_"))
  | RType.TTup tl -> mk_tup (List.map ~f:mk_composite_base_type tl)
  | RType.TNamed _ -> mk_var (Variable.mk ~t:(Some t) (Alpha.fresh "l_"))
  | RType.TFun (_, _) | RType.TParam (_, _) | RType.TVar _ ->
      failwith Fmt.(str "mk_composite_base_type: %a is not a base type." RType.pp t)

(* ============================================================================================= *)
(*                             EQUALITY                                                          *)
(* ============================================================================================= *)

let rec term_compare (t1 : term) (t2 : term) : int =
  match (t1.tkind, t2.tkind) with
  | TConst c1, TConst c2 -> Constant.compare c1 c2
  | TVar v1, TVar v2 -> Variable.compare v1 v2
  | TData (c1, args1), TData (c2, args2) ->
      let c = String.compare c1 c2 in
      if c = 0 then List.compare term_compare args1 args2 else c
  | TApp (f1, args1), TApp (f2, args2) ->
      let c = term_compare f1 f2 in
      if c = 0 then List.compare term_compare args1 args2 else c
  | TBin (b1, t11, t12), TBin (b2, t21, t22) ->
      let c = Binop.compare b1 b2 in
      if c = 0 then
        let c' = term_compare t11 t21 in
        if c' = 0 then term_compare t12 t22 else c'
      else c
  | TUn (u1, t11), TUn (u2, t21) ->
      let c = Unop.compare u1 u2 in
      if c = 0 then term_compare t11 t21 else c
  | TFun (fargs1, body1), TFun (fargs2, body2) ->
      let c = compare (List.length fargs1) (List.length fargs2) in
      if c = 0 then
        match fpat_sub_all fargs1 fargs2 with
        | Some subs -> term_compare body1 (substitution subs body2)
        | None -> -1
      else c
  | TTup tl1, TTup tl2 -> List.compare term_compare tl1 tl2
  | _, _ -> Poly.compare t1 t2

and substitution (substs : (term * term) list) (term : term) : term =
  let rec aux (_t : term) =
    match List.Assoc.find substs ~equal:term_equal _t with
    | Some t' -> t'
    | None ->
        let new_kind =
          match _t.tkind with
          | TBin (b1, t1, t2) -> TBin (b1, aux t1, aux t2)
          | TUn (u, t1) -> TUn (u, aux t1)
          | TIte (c, tt, tf) -> TIte (aux c, aux tt, aux tf)
          | TTup tl -> TTup (List.map ~f:aux tl)
          | TSel (t, i) -> TSel (aux t, i)
          | TFun (args, body) -> TFun (args, aux body)
          | TApp (f, args) -> TApp (aux f, List.map ~f:aux args)
          | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
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
        Variable.mk ~t (Alpha.fresh var.vname)
      in
      (fresh, (mk_var var, mk_var fresh))
    in
    List.map ~f (Set.elements vs)
  in
  (VarSet.of_list (List.map ~f:first substs), substitution (List.map ~f:second substs) t)

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
      | TBin (op, t1, t2) -> TBin (op, aux t1, aux t2)
      | TUn (op, t1) -> TUn (op, aux t1)
      | TConst _ -> tk
      | TVar _ -> tk
      | TIte (c, t1, t2) -> TIte (aux c, aux t1, aux t2)
      | TTup tl -> TTup (List.map ~f:aux tl)
      | TSel (t, i) -> TSel (aux t, i)
      | TFun (fargs, body) -> TFun (fargs, aux body)
      | TApp (func, args) -> TApp (aux func, List.map ~f:aux args)
      | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
    in
    f { t0 with tkind = tk' }
  in
  aux t

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
          | TIte (c, t1, t2) -> TIte (aux c, aux t1, aux t2)
          | TTup tl -> TTup (List.map ~f:aux tl)
          | TSel (t, i) -> TSel (aux t, i)
          | TFun (fargs, body) -> TFun (fargs, aux body)
          | TApp (func, args) -> TApp (aux func, List.map ~f:aux args)
          | TData (cstr, args) -> TData (cstr, List.map ~f:aux args)
        in
        { t0 with tkind = tk' }
  in
  aux t

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
          | TIte (c, t1, t2) -> TIte (aux a' c, aux a' t1, aux a' t2)
          | TTup tl -> TTup (List.map ~f:(aux a') tl)
          | TSel (t, i) -> TSel (aux a' t, i)
          | TFun (fargs, body) -> TFun (fargs, aux a' body)
          | TApp (func, args) -> TApp (aux a' func, List.map ~f:(aux a') args)
          | TData (cstr, args) -> TData (cstr, List.map ~f:(aux a') args)
        in
        { t0 with tkind = tk' }
  in
  aux init t

let rewrite_types t_subs =
  Variable.update_var_types t_subs;
  rewrite_with (fun _t -> { _t with ttyp = RType.sub_all t_subs _t.ttyp })

(**
   `reduce ~init ~case ~join t` reduces the term by reducing each leaf to `init`, and at each node
    of the syntax tree, using `join` to merge the values. In a top-down traversal, if `case` returns
    `Some a` then the subterm is not recrusively reduced, but the value `a` is used instead.
*)
let reduce ~(init : 'a) ~(case : (term -> 'a) -> term -> 'a option) ~(join : 'a -> 'a -> 'a)
    (t : term) : 'a =
  let rec aux (t : term) : 'a =
    match case aux t with
    | Some x -> x
    | None -> (
        match t.tkind with
        | TBin (_, t1, t2) -> join (aux t1) (aux t2)
        | TUn (_, t1) -> aux t1
        | TConst _ -> init
        | TVar _ -> init
        | TIte (c, a, b) -> join (aux c) (join (aux a) (aux b))
        | TTup tl -> aux_l tl
        | TSel (t, _) -> aux t
        | TFun (_, body) -> aux body
        | TApp (func, args) -> join (aux func) (aux_l args)
        | TData (_, args) -> aux_l args)
  and aux_l l = List.fold ~init ~f:join (List.map ~f:aux l) in
  aux t

let transform ~(case : (term -> term) -> term -> term option) (t : term) : term =
  let rec aux (t : term) : 'a =
    match case aux t with
    | Some x -> x
    | None ->
        {
          t with
          tkind =
            (match t.tkind with
            | TBin (bo, t1, t2) -> TBin (bo, aux t1, aux t2)
            | TUn (uo, t1) -> TUn (uo, aux t1)
            | TConst _ -> t.tkind
            | TVar _ -> t.tkind
            | TIte (c, a, b) -> TIte (aux c, aux a, aux b)
            | TTup tl -> TTup (aux_l tl)
            | TSel (t, i) -> TSel (aux t, i)
            | TFun (args, body) -> TFun (args, aux body)
            | TApp (func, args) -> TApp (aux func, aux_l args)
            | TData (cstr, args) -> TData (cstr, aux_l args));
        }
  and aux_l l = List.map ~f:aux l in
  aux t

let var_count (typ : RType.t) (t : term) =
  let case _ t =
    match t.tkind with
    | TVar v -> Some (if Poly.equal (Variable.vtype_or_new v) typ then 1 else 0)
    | _ -> None
  in
  reduce ~init:0 ~case ~join:(fun a b -> a + b) t

let var_count_compare typ (t1 : term) (t2 : term) = compare (var_count typ t1) (var_count typ t2)

let term_size (t : term) =
  let case _ t = match t.tkind with TConst _ | TVar _ -> Some 1 | _ -> None in
  reduce ~init:0 ~case ~join:(fun a b -> a + b + 1) t

let term_size_compare (t1 : term) (t2 : term) = compare (term_size t1) (term_size t2)

let term_height =
  let case _ _ = None in
  reduce ~init:0 ~case ~join:(fun a b -> 1 + max a b)

let term_height_compare (t1 : term) (t2 : term) = compare (term_height t1) (term_height t2)

let is_norec =
  let case _ t =
    match t.tkind with
    | TVar x -> Some (not (RType.is_recursive (Variable.vtype_or_new x)))
    | _ -> None
  in
  reduce ~init:true ~join:( && ) ~case

let is_novariant =
  let case _ t =
    match t.tkind with
    | TVar x -> Some (List.is_empty (RType.get_variants (Variable.vtype_or_new x)))
    | _ -> None
  in
  reduce ~init:true ~join:( && ) ~case

(* ============================================================================================= *)
(*                                    PRETTY PRINTERS                                            *)
(* ============================================================================================= *)
open Fmt

let rec pp_fpattern (frmt : Formatter.t) (fp : fpattern) =
  match fp with
  | PatVar x -> Variable.pp frmt x
  | PatTup tl -> pf frmt "%a" (box (parens (list ~sep:comma pp_fpattern))) tl

let pp_term (frmt : Formatter.t) (x : term) =
  let rec aux (paren : bool) (frmt : Formatter.t) (t : term) =
    match t.tkind with
    | TConst c -> pf frmt "%a" Constant.pp c
    | TVar v -> pf frmt "%a" Variable.pp v
    | TBin (op, t1, t2) -> (
        match op with
        | Binop.Max | Binop.Min ->
            if paren then pf frmt "@[<hov 2>(%a@;%a@;%a)@]" Binop.pp op (aux true) t1 (aux true) t2
            else pf frmt "@[<hov 2>%a@;%a@;%a@]" Binop.pp op (aux true) t1 (aux true) t2
        | _ ->
            if paren then pf frmt "@[<hov 2>(%a@;%a@;%a)@]" (aux true) t1 Binop.pp op (aux true) t2
            else pf frmt "@[<hov 2>%a@;%a@;%a@]" (aux true) t1 Binop.pp op (aux true) t2)
    | TUn (op, t1) ->
        if paren then pf frmt "@[<hov 2>(%a@;%a)@]" Unop.pp op (aux true) t1
        else pf frmt "@[<hov 2>%a@;%a@]" Unop.pp op (aux true) t1
    | TIte (c, t1, t2) ->
        if paren then
          pf frmt "@[<hov 2>(%a@;?@;%a@;:@;%a)@]" (aux false) c (aux false) t1 (aux false) t2
        else pf frmt "@[<hov 2>%a@;?@;%a@;:@;%a@]" (aux false) c (aux false) t1 (aux false) t2
    | TTup tl -> pf frmt "@[<hov 2>(%a)@]" (list ~sep:comma (aux false)) tl
    | TSel (t, i) -> pf frmt "@[<hov 2>%a.%i@]" (aux true) t i
    | TFun (args, body) ->
        if paren then
          pf frmt "@[<hov 2>(fun %a -> @;%a)@]" (list ~sep:sp pp_fpattern) args (aux false) body
        else pf frmt "@[<hov 2>fun %a -> %a@]" (list ~sep:sp pp_fpattern) args (aux false) body
    | TApp (func, args) ->
        if paren then pf frmt "@[<hov 2>(%a@;%a)@]" (aux true) func (list ~sep:sp (aux true)) args
        else pf frmt "@[<hov 2>%a@;%a@]" (aux true) func (list ~sep:sp (aux true)) args
    | TData (cstr, args) ->
        if List.length args = 0 then pf frmt "%s" cstr
        else pf frmt "%s(%a)" cstr (list ~sep:comma (aux false)) args
  in
  aux false frmt x

let pp_subs (f : Formatter.t) (subs : (term * term) list) : unit =
  Fmt.(
    pf f "@[<hov 2>%a@]"
      (fun f l -> List.iter ~f:(fun (t1, t2) -> pf f "@[[%a -> %a]@]" pp_term t1 pp_term t2) l)
      subs)

(* ============================================================================================= *)
(*                                  TYPE INFERENCE                                               *)
(* ============================================================================================= *)

let infer_type (t : term) : term * RType.substitution =
  let rec aux t0 =
    let eloc = t0.tpos in
    let merge_subs = RType.merge_subs eloc in
    match t0.tkind with
    | TBin (op, t1, t2) -> (
        let t_t1, c_t1 = aux t1 and t_t2, c_t2 = aux t2 in
        let ta, tb = Binop.operand_types op in
        match RType.unify [ (t_t1.ttyp, ta); (t_t2.ttyp, tb) ] with
        | Some subs ->
            ( mk_bin ~pos:t0.tpos ~typ:(Some (Binop.result_type op)) op t_t1 t_t2,
              merge_subs subs (merge_subs c_t1 c_t2) )
        | None ->
            Log.error_msg Fmt.(str "Cannot infer type of binary expression %a." pp_term t0);
            Log.error_msg
              Fmt.(
                str "%a has type %a, expected type %a. %a has type %a, expected %a." pp_term t1
                  RType.pp t_t1.ttyp RType.pp ta pp_term t2 RType.pp t_t2.ttyp RType.pp tb);
            failwith "Type inference failure.")
    | TUn (op, t1) -> (
        let t_t1, c_t1 = aux t1 in
        match RType.unify [ (t_t1.ttyp, Unop.operand_type op) ] with
        | Some subs ->
            (mk_un ~pos:t0.tpos ~typ:(Some (Unop.result_type op)) op t_t1, merge_subs subs c_t1)
        | None ->
            Log.error_msg Fmt.(str "Cannot infer type of unary expression %a." pp_term t0);
            Log.error_msg
              Fmt.(
                str "%a has type %a, expected type %a." pp_term t1 RType.pp t_t1.ttyp RType.pp
                  (Unop.operand_type op));
            Log.loc_fatal_errmsg eloc "Type inference failure.")
    | TConst c -> ({ t0 with ttyp = Constant.type_of c }, [])
    | TVar v -> (
        let tv = Variable.vtype_or_new v in
        match RType.unify [ (tv, t0.ttyp) ] with
        | Some res -> ({ t0 with ttyp = tv }, res)
        | None -> failwith "Type inference failure")
    | TIte (c, t1, t2) -> (
        let t_c, c_c = aux c and t_t1, c_t1 = aux t1 and t_t2, c_t2 = aux t2 in
        match RType.unify [ (t_c.ttyp, RType.TBool); (t_t1.ttyp, t_t2.ttyp) ] with
        | Some subs ->
            ( mk_ite ~pos:t0.tpos ~typ:(Some t_t1.ttyp) t_c t_t1 t_t2,
              merge_subs subs (merge_subs c_c (merge_subs c_t1 c_t2)) )
        | None ->
            Log.error_msg
              Fmt.(str "ite(%a, %a, %a)." RType.pp c.ttyp RType.pp t1.ttyp RType.pp t2.ttyp);
            failwith "Type inference failure.")
    | TTup tl ->
        let term_l, c_l = List.unzip (List.map ~f:aux tl) in
        (mk_tup ~pos:t0.tpos term_l, merge_subs (List.concat c_l) [])
    | TSel (t, i) -> (
        let t_c, c_c = aux t in
        match t_c.ttyp with
        | RType.TTup tl -> (
            match List.nth tl i with
            | Some tout -> (mk_sel ~pos:t0.tpos ~typ:(Some tout) t_c i, c_c)
            | None ->
                Log.error_msg Fmt.(str "In tuple acessor %a, index out of bounds." pp_term t0);
                failwith "Type inference: tuple acessor, accesed tuple of wrong type.")
        | _ ->
            Log.error_msg
              Fmt.(str "Tuple accessor argument %a of type %a." pp_term t_c RType.pp t_c.ttyp);
            failwith "Type inference: tuple accessor on type acessor.")
    | TFun (args, body) ->
        let t_body, c_body = aux body in
        (mk_fun ~pos:t0.tpos args t_body, c_body)
    | TApp (func, fargs) -> (
        let t_func, c_func = aux func and t_args, c_args = List.unzip (List.map ~f:aux fargs) in
        let argst = List.map ~f:(fun t -> t.ttyp) t_args in
        let _c = RType.(mkv c_func @ mkv (List.concat c_args)) in
        match t_func.ttyp with
        | RType.TFun (_, _) -> (
            let func_targs, func_tout = RType.fun_typ_unpack t_func.ttyp in
            match List.zip func_targs argst with
            | Ok typ_pairs -> (
                match RType.(unify (_c @ typ_pairs)) with
                | Some subs -> (mk_app ~pos:t0.tpos ~typ:(Some func_tout) t_func t_args, subs)
                | None ->
                    Log.loc_fatal_errmsg eloc
                      (Fmt.str "Type inference failure: could not unify types in application %a(%a)"
                         pp_term func
                         (box (list ~sep:sp pp_term))
                         fargs))
            | Unequal_lengths ->
                Log.loc_fatal_errmsg eloc
                  (Fmt.str "Type inference failure: %a expects %i argument, given %i: %a." pp_term
                     func (List.length func_targs) (List.length fargs)
                     (box (list ~sep:sp pp_term))
                     fargs))
        | RType.TVar f_tvar -> (
            (* |- f_tvar : (_ -> _ -> _ .. -> _) -> 'b *)
            let t_out = RType.get_fresh_tvar () in
            let tf = RType.fun_typ_pack argst t_out in
            match RType.(unify (_c @ [ (tf, RType.TVar f_tvar) ])) with
            | Some subs -> (mk_app ~pos:t0.tpos ~typ:(Some t_out) t_func t_args, subs)
            | None -> failwith "Type inference failure.")
        | _ as tf ->
            Log.loc_fatal_errmsg eloc
              (Fmt.str "Type inference failure: could not type %a as function." RType.pp tf))
    | TData (cstr, args) -> (
        match RType.type_of_variant cstr with
        | Some (tout, targs) -> (
            let t_args, c_args = List.unzip (List.map ~f:aux args) in
            match List.zip targs (List.map ~f:(fun term -> term.ttyp) t_args) with
            | Ok pairs -> (
                match RType.unify (pairs @ RType.mkv (List.concat c_args)) with
                | Some subs -> ({ t0 with ttyp = tout; tkind = TData (cstr, t_args) }, subs)
                | None ->
                    Log.loc_fatal_errmsg eloc
                      (Fmt.str "Type inference failure: could not unify %s arguments %a." cstr
                         (list ~sep:comma pp_term) t_args))
            | Unequal_lengths ->
                Log.loc_fatal_errmsg eloc
                  (Fmt.str "Type inference failure: could not match %s arguments: %a and %a." cstr
                     (list ~sep:comma pp_term) t_args (list ~sep:comma RType.pp) targs))
        | None ->
            Log.loc_fatal_errmsg eloc
              Fmt.(str "Type inference failure: could not find type of %s." cstr))
  in
  let t', subs = aux t in
  match RType.unify (RType.mkv subs) with
  | Some merge_subs ->
      let tsubs = RType.mkv merge_subs in
      (rewrite_types tsubs t', merge_subs)
  | None -> Log.loc_fatal_errmsg t'.tpos "Could not infer type."

(* ============================================================================================= *)
(*                                  SETS OF TERMS                                                *)
(* ============================================================================================= *)

module Terms = struct
  module E = struct
    type t = term

    let compare t1 t2 =
      let c = compare (term_size t1) (term_size t2) in
      if c = 0 then term_compare t1 t2 else c

    let equal = term_equal

    let sexp_of_t = sexp_of_term
  end

  include E
  module C = Comparator.Make (E)
  include C

  let substs_of_alist (alist : (variable * term) list) : (term * term) list =
    List.map ~f:(fun (a, b) -> (mk_var a, b)) alist
end

module TermSet = struct
  module S = Set.M (Terms)
  include S

  let empty = Set.empty (module Terms)

  let singleton = Set.singleton (module Terms)

  let of_list = Set.of_list (module Terms)

  let union_list = Set.union_list (module Terms)
end

let pp_term_set (f : Formatter.t) (s : TermSet.t) =
  (braces (list ~sep:comma (box pp_term))) f (Set.elements s)
