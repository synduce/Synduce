open Base
open TermTypes

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

  let _print_info = ref false

  let vtype_assign (ctx : Context.t) (v : t) (t : RType.t) =
    Hashtbl.set ctx.vartypes ~key:v.vid ~data:t
  ;;

  let vtype (ctx : Context.t) (v : t) = Hashtbl.find ctx.vartypes v.vid
  let id (v : t) = v.vid
  let get_name (ctx : Context.t) (vid : int) = Hashtbl.find ctx.varnames vid

  let clear_type (ctx : Context.t) (v : t) =
    let new_t = RType.get_fresh_tvar ctx.types in
    vtype_assign ctx v new_t
  ;;

  (* `vtype_or_new v` returns the type of variable v, or assigns a fresh type variable
      as its type if it doesn't have one.
      The type of a variable will automatically be assigned to satisfy constraints
      produced during type inference.
  *)
  let vtype_or_new (ctx : Context.t) (v : t) =
    match vtype ctx v with
    | Some x -> x
    | None ->
      let new_t = RType.get_fresh_tvar ctx.types in
      vtype_assign ctx v new_t;
      new_t
  ;;

  let update_var_types (ctx : Context.t) (tsubs : (RType.t * RType.t) list) =
    Hashtbl.map_inplace ~f:(fun t -> RType.sub_all tsubs t) ctx.vartypes
  ;;

  let mk ?(attrs = Attributes.empty) ?(t = None) (ctx : Context.t) (name : string) =
    let v =
      Alpha.mk_with_id ctx.names (-1) name (fun vid ->
          { vname = name; vid; vattrs = attrs })
    in
    Hashtbl.set ctx.varnames ~key:v.vid ~data:v.vname;
    (match t with
    | Some t -> vtype_assign ctx v t
    | None -> vtype_assign ctx v (RType.get_fresh_tvar ctx.types));
    v
  ;;

  let is_anonymous (v : t) : bool = Set.mem v.vattrs Anonymous
  let make_anonymous (v : t) : t = { v with vattrs = Set.add v.vattrs Anonymous }
  let is_builtin (v : t) : bool = Set.mem v.vattrs Builtin
  let make_builtin (v : t) : t = { v with vattrs = Set.add v.vattrs Builtin }
  let has_attr (attr : Attributes.elt) (v : t) = Set.mem v.vattrs attr
  let is_nonterminal (v : t) = Set.exists ~f:Attributes.Elt.is_non_terminal v.vattrs
  let same_name (v : t) (v2 : t) : bool = String.equal v.vname v2.vname

  let pp (ctx : Context.t) (frmt : Formatter.t) (v : t) =
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
          (vtype_or_new ctx v))
    else Fmt.(pf frmt "%s" v.vname)
  ;;

  let pp_id (frmt : Formatter.t) (v : t) = Fmt.(pf frmt "%s{%i}" v.vname v.vid)

  let pp_typed (ctx : Context.t) (frmt : Formatter.t) (v : t) =
    Fmt.(pf frmt "%s : %a" v.vname RType.pp (vtype_or_new ctx v))
  ;;

  let free (ctx : Context.t) (var : t) =
    Alpha.forget ctx.names var.vid var.vname;
    Hashtbl.remove ctx.vartypes var.vid
  ;;

  let print_summary (frmt : Formatter.t) (ctx : Context.t) =
    Utils.Log.(debug (wrap "Variables in tables:"));
    let le =
      Hashtbl.fold ctx.names.ids ~init:0 ~f:(fun ~key:_ ~data l ->
          max l (String.length data))
    in
    Fmt.(pf frmt "\t  ID | %*s : TYPE@." le "NAME");
    Fmt.(pf frmt "\t---------------------------@.");
    Hashtbl.iteri ctx.names.ids ~f:(fun ~key ~data ->
        match Hashtbl.find ctx.vartypes key with
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

  let filter_by_type ctx vs t =
    Set.filter vs ~f:(fun v -> RType.t_equals (Variable.vtype_or_new ctx v) t)
  ;;

  let record ctx vs =
    List.map
      ~f:(fun elt -> elt.vname, Option.value_exn (Variable.vtype ctx elt))
      (elements vs)
  ;;

  let to_env vs =
    Map.of_alist_reduce
      (module String)
      ~f:(fun b1 _ -> b1)
      (List.map ~f:(fun v -> v.vname, v) (elements vs))
  ;;

  (* Returns a list of pairs of variable, fresh copy of the variable. *)
  let prime (ctx : Context.t) (vs : t) : (variable * variable) list =
    Set.fold
      ~f:(fun subs v ->
        let primed_name = Alpha.fresh ~s:(v.vname ^ "_") ctx.names in
        let primed_var = Variable.mk ctx ~t:(Variable.vtype ctx v) primed_name in
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

  let pp_var_names ctx formatter vs =
    Fmt.(list ~sep:comma (Variable.pp ctx) formatter (elements vs))
  ;;

  let pp ctx formatter vs =
    Fmt.(list ~sep:sp (parens (Variable.pp_typed ctx)) formatter (elements vs))
  ;;

  let dump formatter vs = Fmt.Dump.(list Variable.pp_id formatter (elements vs))

  let of_sh sh =
    Hashtbl.fold
      sh
      ~f:(fun ~key:_ ~data:v vset -> Set.add vset v)
      ~init:(Set.empty (module Variable))
  ;;
end
