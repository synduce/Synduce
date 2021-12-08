open Base

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

  let find_by_id vs id : elt option =
    Set.max_elt (Set.filter ~f:(fun elt -> elt.vid = id) vs)
  ;;

  let filter_map (vs : t) ~f = Set.filter_map ~f (module Variable) vs
  let has_name vs name : bool = Set.exists ~f:(fun elt -> String.equal elt.vname name) vs

  let find_by_name vs name : elt option =
    Set.max_elt (Set.filter ~f:(fun elt -> String.equal elt.vname name) vs)
  ;;

  let vids_of_vs vs : int list = List.map ~f:(fun vi -> vi.vid) (elements vs)
  let has_vid vs id : bool = List.mem ~equal:( = ) (vids_of_vs vs) id
  let bindings vs = List.map ~f:(fun elt -> elt.vid, elt) (elements vs)
  let names vs = List.map ~f:(fun elt -> elt.vname) (elements vs)

  let filter_by_type vs t =
    Set.filter vs ~f:(fun v -> RType.t_equals (Variable.vtype_or_new v) t)
  ;;

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

  (* Returns a list of pairs of variable, fresh copy of the variable. *)
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
