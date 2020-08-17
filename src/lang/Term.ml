open Base
open Lexing

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
        | NonTerminal of int [@@deriving sexp]
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


type variable = { vname : string; vid : int; vtype : RType.t; vattrs : Attributes.t }

let sexp_of_variable v =
  Sexp.(List [Atom "var"; Atom v.vname])(*  Atom (Int.to_string v.vid); sexp_of_typ v.vtype]) *)


let pp_variable f v = Fmt.((styled (`Fg `Cyan) string) f v.vname)


let pp_id_var f v = Fmt.(pf f "(%i : %s)" v.vid v.vname)


let dump_variable f v = Fmt.(string f v.vname)


module Variable = struct
  module T = struct
    type t = variable
    let compare x y = compare x.vid y.vid
    let equal x y = x.vid = y.vid
    let (=) x y = equal x y
    let sexp_of_t = sexp_of_variable
    let hash = Hashtbl.hash
  end
  include T
  include Comparator.Make (T)

  let mk ?(attrs = Attributes.empty) ?(t = RType.TAnon) (name : string) =
    Alpha.mk_with_id (-1) name (fun vid -> { vname = name; vid = vid; vtype = t; vattrs = attrs })

  let is_anonymous (v : t) : bool = Set.mem v.vattrs Anonymous
  let make_anonymous (v : t) : t =
    {v with vattrs = Set.add v.vattrs Anonymous }

  let is_builtin (v : t) : bool = Set.mem v.vattrs Builtin

  let make_builtin (v : t) : t =
    {v with vattrs = Set.add v.vattrs Builtin }

  let has_attr (attr : Attributes.elt) (v : t) = Set.mem v.vattrs attr

  let is_nonterminal (v : t) = Set.exists ~f:Attributes.Elt.is_non_terminal v.vattrs
end


module VarSet =
struct
  module V = Set.M (Variable)
  include V

  type elt = variable

  let empty = Set.empty (module Variable)

  let singleton = Set.singleton (module Variable)

  let elements vs =
    Set.elements vs

  let of_list = Set.of_list (module Variable)

  let map f vs : t =
    of_list (List.map ~f:f (elements vs))

  let max_elt = Set.max_elt
  let min_elt = Set.max_elt

  let find_by_id vs id : elt option =
    max_elt (Set.filter ~f:(fun elt -> elt.vid = id) vs)

  let has_name vs name : bool =
    Set.exists ~f:(fun elt -> String.equal elt.vname name) vs

  let find_by_name vs name : elt option =
    max_elt (Set.filter ~f:(fun elt -> String.equal elt.vname name) vs)

  let vids_of_vs vs : int list =
    List.map ~f:(fun vi -> vi.vid) (elements vs)

  let has_vid vs id : bool =
    List.mem ~equal:(=) (vids_of_vs vs) id

  let bindings vs =
    List.map ~f:(fun elt -> (elt.vid, elt)) (elements vs)

  let names vs =
    List.map ~f:(fun elt -> elt.vname) (elements vs)

  let types vs =
    List.map ~f:(fun elt -> elt.vtype) (elements vs)

  let record vs =
    List.map ~f:(fun elt -> elt.vname, elt.vtype) (elements vs)

  let add_prefix vs prefix =
    of_list (List.map ~f:(fun v -> {v with vname = prefix^v.vname}) (elements vs))

  let iset vs ilist =
    of_list
      (List.filter ~f:(fun vi -> List.mem ilist vi.vid ~equal:(=)) (elements vs))

  let pp_var_names formatter vs =
    Fmt.(list ~sep:comma pp_variable formatter (elements vs))

  let pp formatter vs =
    Fmt.(list ~sep:sp pp_id_var formatter (elements vs))

  let dump formatter vs =
    Fmt.Dump.(list pp_id_var formatter (elements vs))

  let of_sh sh = Hashtbl.fold ~f:(fun ~key:_ ~data:v vset -> Set.add vset v) sh ~init:(Set.empty (module Variable))
end

(* ----------------------------------------------------- *)
(**
   Terms.
*)

module Binop = struct
  type t =
    | Lt | Gt | Ge | Le | Eq | Neq
    | Max | Min
    | Plus | Minus
    | Times | Div | Mod
    | And | Or
end

module Unop = struct
  type t =
    | Neg | Not
    | Abs
end

module Constant = struct
  type t =
    | CInt of int
    | CTrue
    | CFalse

  let of_int i = CInt i
  let of_bool b = if b then CTrue else CFalse
  let _if c t f =
    match c with CTrue -> t | _ -> f

end


type termkind =
  | TBin of Binop.t * term * term
  | TUn of Unop.t * term
  | TConst of Constant.t
  | TVar of variable
  | TIte of term * term * term
  | TTup of term list
  | TFun of variable list * term
  | TApp of term * term
  | TData of string * term list

and term = { tpos: position * position; tkind : termkind; ttyp : RType.t }

let mk_var ?(pos = dummy_loc) ?(t = RType.TAnon) (v : variable) : term =
  { tpos = pos; tkind = TVar v; ttyp = t }