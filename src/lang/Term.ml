open Base
open Lexing

module O = Option

type vattribute =
  | Anonymous
  | Builtin

type variable = { vname : string; vid : int; vtype : RType.t; vattrs : vattribute list }

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

  let is_anonymous (v : t) : bool =
    List.mem v.vattrs Anonymous ~equal:Poly.equal
  let make_anonymous (v : t) : t =
    if is_anonymous v then v else {v with vattrs = Anonymous :: v.vattrs }

  let is_builtin (v : t) : bool =
    List.mem v.vattrs Builtin ~equal:Poly.equal
  let make_builtin (v : t) : t =
    if is_anonymous v then v else {v with vattrs = Builtin :: v.vattrs }

  let has_attr (attr : vattribute) (v : t) = List.mem ~equal:Poly.equal v.vattrs attr
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

and term = { pos: position * position; kind : termkind }