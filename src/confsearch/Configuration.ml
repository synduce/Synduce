(** A configuration is a set of argument for unknowns in a recursion skeleton.  *)
open Base

open Common
open Env
open Lang
open Term
open Utils

(**
  A configuration is a map from variables (the unknown functions) to a list of arguments.
*)
type conf = term list VarMap.t

module Subconf = struct
  (**
  A subconfiguration (relative to some configuration) is a map from an unknown's variable
  id to a list of integer representing the index of the argument in the super-configuration
  argument list of that unknown.
*)
  type t = int list IntMap.t

  let sexp_of_t (m : t) : Sexp.t =
    List.sexp_of_t
      (fun (a, b) -> Sexp.(List [ Int.sexp_of_t a; List.sexp_of_t Int.sexp_of_t b ]))
      (Map.to_alist m)
  ;;

  let equal : t -> t -> bool = Map.equal (List.equal Int.equal)
  let compare (a : t) (b : t) = Map.compare_direct (List.compare Int.compare) a b

  let hash : t -> int =
    Hash.of_fold (Map.hash_fold_direct Int.hash_fold_t (List.hash_fold_t Int.hash_fold_t))
  ;;

  (** Return the configuration as a subconf. *)
  let of_conf (conf : conf) : t =
    Map.fold
      conf
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data m ->
        Map.set m ~key:key.vid ~data:(List.mapi ~f:(fun i _ -> i) data))
  ;;

  (** Return the subconf as a configuration, given the configuration it refines. *)
  let to_conf (conf : conf) (sub : t) : conf =
    Map.mapi conf ~f:(fun ~key:v ~data:args ->
        match Map.find sub v.vid with
        | Some indexes -> sub_list args indexes
        | None -> failwith "supconf should only be used on a proper subconfiguration.")
  ;;

  (**
  Create as many subconfigurations as possible from dropping an argument for an unknown.
 *)
  let drop_arg (conf : t) : t list =
    let drop_one (unknown, args) =
      match args with
      | [] -> []
      | [ _ ] -> [ Map.set ~key:unknown ~data:[] conf ]
      | _ ->
        (* Configurations with all possible ways to drop one argument *)
        List.mapi args ~f:(fun i _ ->
            (* new args is current args without ith element *)
            let data = List.filteri ~f:(fun j _ -> not (j = i)) args in
            Map.set ~key:unknown ~data conf)
    in
    List.concat_map (Map.to_alist conf) ~f:drop_one
  ;;
end

let ppm (ctx : env) (fmt : Formatter.t) (conf : conf) =
  Fmt.(
    brackets
      (list
         ~sep:semi
         (parens
            (pair
               ~sep:rightarrow
               (ctx @>- Variable.pp)
               (list ~sep:vbar (ctx @>- pp_term))))))
    fmt
    (Map.to_alist conf)
;;

(** Create an empty configuration of a set of variables.  *)
let of_varset = VarMap.init_from_varset ~init:(fun _ -> [])

(** Check that the unknowns of a PMRS occur at most once. *)
let check_pmrs (p : PMRS.t) =
  let counts = Hashtbl.create (module Int) in
  Set.iter p.psyntobjs ~f:(fun v -> Hashtbl.set counts ~key:v.vid ~data:0);
  Map.iter ~f:(fun (_, _, _, rhs) -> Analysis.count_occurrences counts rhs) p.prules;
  Hashtbl.for_all ~f:(fun o -> o < 2) counts
;;

(** Generate the base type arguments given a set of variables and
  a PMRS that defines a set of recursive functions (nonterminals).
*)
let base_type_args (ctx : env) (p : PMRS.t) (vs : VarSet.t) =
  let base_type_vars =
    let on_var v =
      if RType.is_recursive ctx.ctx.types ((ctx @>- Variable.vtype_or_new) v)
      then []
      else [ (ctx @>- mk_var) v ]
    in
    List.concat_map (Set.elements vs) ~f:on_var
  in
  let rec_function_applications =
    let on_nonterminal f_var =
      let argtypes, _ = RType.fun_typ_unpack ((ctx @>- Variable.vtype_or_new) f_var) in
      let arg_combinations =
        Utils.cartesian_nary_product
          (List.map argtypes ~f:(fun t ->
               Set.elements ((ctx @>- VarSet.filter_by_type) vs t)))
      in
      List.map arg_combinations ~f:(fun args ->
          (ctx @>- mk_app_v) f_var (List.map ~f:(ctx @>- mk_var) args))
    in
    List.concat_map (VarSet.elements p.pnon_terminals) ~f:on_nonterminal
  in
  TermSet.of_list (base_type_vars @ rec_function_applications)
;;

(** Build the argument map of the PMRS.
  The argument map is effectively the largest configuration: it is a map from each unknown
  to all the arguments available to the unknown.
*)
let max_configuration (ctx : env) (p : PMRS.t) : conf =
  let empty_conf = of_varset p.psyntobjs in
  let set_args_in_rule ~key:_ruleid ~data:(_, lhs_args, lhs_pat, rhs) vmap =
    let lhs_argset =
      Set.union
        (VarSet.of_list lhs_args)
        (Option.value_map lhs_pat ~default:VarSet.empty ~f:(fun p ->
             ctx >- Analysis.free_variables (Term.term_of_pattern ctx.ctx p)))
    in
    (* TODO: collect let-bound variables? Technically cannot provide more info,
      but has subexpression elim. simplification advantage.
    *)
    let args = base_type_args ctx p (Set.union lhs_argset (VarSet.of_list p.pargs)) in
    Set.fold
      (* The local unknowns *)
      (Set.inter (ctx >- Analysis.free_variables rhs) p.psyntobjs)
      ~init:vmap
        (* Unknown should appear for the first time if PMRS well formed for configuration. *)
      ~f:(fun m v -> Map.set m ~key:v ~data:(Set.elements args))
  in
  Map.fold p.prules ~init:empty_conf ~f:set_args_in_rule
;;

(** Count the number of subconfigurations of a given configuration. *)
let subconf_count (c : conf) =
  Map.fold ~init:1 ~f:(fun ~key:_ ~data:l c -> c * (2 ** List.length l)) c
;;

(** Return the configuration of a PMRS, assuming it has been checked. *)
let configuration_of (p : PMRS.t) : conf =
  let init = of_varset p.psyntobjs in
  let join m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_ m ->
        match m with
        | `Both (v1, _) -> Some v1
        | `Left v1 -> Some v1
        | `Right v1 -> Some v1)
  in
  let from_rhs =
    let case _ t =
      match t.tkind with
      | TApp ({ tkind = TVar f_var; _ }, args) ->
        if Set.mem p.psyntobjs f_var then Some (VarMap.singleton f_var args) else None
      | _ -> None
    in
    reduce ~init:VarMap.empty ~join ~case
  in
  Map.fold p.prules ~init ~f:(fun ~key:_ ~data:(_, _, _, rhs) accum ->
      join accum (from_rhs rhs))
;;

(** Apply a configuration to a given PMRS. The environment is copied before the type
  information for the unknowns is changed. The copied environment is then returned.
*)
let apply_configuration (ctx : env) (config : conf) (p : PMRS.t) : PMRS.t * env =
  let ctx = env_copy ctx in
  Set.iter p.psyntobjs ~f:(fun u -> (ctx @>- Variable.clear_type) u);
  let repl_in_rhs =
    let case _ t =
      match t.tkind with
      | TVar v when Set.mem p.psyntobjs v ->
        Option.map (Map.find config v) ~f:(fun args -> (ctx @>- mk_app_v) v args)
      | TApp ({ tkind = TVar v; _ }, _) when Set.mem p.psyntobjs v ->
        Option.map (Map.find config v) ~f:(fun args -> (ctx @>- mk_app_v) v args)
      | _ -> None
    in
    transform ~case
  in
  ( PMRS.(
      ctx
      >- infer_pmrs_types
           { p with
             prules =
               Map.map p.prules ~f:(fun (nt, args, pat, rhs) ->
                   nt, args, pat, repl_in_rhs rhs)
           })
  , ctx )
;;

(** Building graph of configurations. *)

module ConfGraph = struct
  module G = Graph.Imperative.Digraph.Concrete (Subconf)
  include G
  module Bfs = Graph.Traverse.Bfs (G)

  type marks = int Hashtbl.M(Subconf).t

  (** A type to represent the state of the configuration graph exploration.
    We need to remember the graph and mark configurations as solved or not.
  *)
  type state =
    { graph : t (** The graph of configurations. *)
    ; marks : marks
          (**
          A negative mark means unrealizable.
          A positive mark means a solution has been found.
      Otherwise, a mark of 0 means it has not been solved.
    *)
    ; root : Subconf.t (**
    The maximum configuration of the graph.
        *)
    ; super : conf
    ; ctx : env
    }

  let mark_unrealizable (s : state) (conf : Subconf.t) =
    Hashtbl.set s.marks ~key:conf ~data:(-1)
  ;;

  let mark_realizable (s : state) (conf : Subconf.t) =
    Hashtbl.set s.marks ~key:conf ~data:1
  ;;

  let is_unmarked (s : state) (conf : Subconf.t) =
    match Hashtbl.find s.marks conf with
    | Some 0 | None -> true
    | _ -> false
  ;;

  (** `expand g conf` adds the edges from `conf` to all its refinements in `g`. *)
  let expand (s : state) (conf : Subconf.t) : unit =
    match Hashtbl.find s.marks conf with
    (* The configuration is unrealizable. No subconfiguration can be realizable. *)
    | Some x when x < 0 -> ()
    | _ ->
      List.iter (Subconf.drop_arg conf) ~f:(fun c ->
          match Hashtbl.find s.marks c with
          (* Already solved: no need to add new edge. *)
          | Some x when not (x = 0) -> ()
          | Some _ -> add_edge s.graph conf c
          | None ->
            Hashtbl.set s.marks ~key:c ~data:0;
            add_edge s.graph conf c)
  ;;

  (**
    Find the next 0-marked configuration in the graph.
    Return None if there is no such configuration.
  *)
  let next (s : state) : Subconf.t option =
    let q = Queue.create () in
    Queue.enqueue q s.root;
    let rec loop () =
      Option.bind (Queue.dequeue q) ~f:(fun curr ->
          match Hashtbl.find s.marks curr with
          | Some 0 -> Some curr
          | None ->
            Hashtbl.set s.marks ~key:curr ~data:0;
            Some curr
          | Some x when x < 0 -> loop ()
          | Some _ ->
            Queue.enqueue_all q (List.filter ~f:(is_unmarked s) (succ s.graph curr));
            loop ())
    in
    loop ()
  ;;

  (** Generate the inital graph of configurations of a PMRS with unknowns. *)
  let generate_configurations (ctx : env) (p : PMRS.t) : state =
    let super = max_configuration ctx p in
    let root = Subconf.of_conf super in
    let size = subconf_count super in
    let graph = create ~size () in
    add_vertex graph root;
    let marks = Hashtbl.create (module Subconf) ~size in
    Hashtbl.set marks ~key:root ~data:0;
    { graph; marks; root; super; ctx }
  ;;
end
