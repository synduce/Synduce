(** A configuration graph represents the partial order between configurations.
  An algorithm can build the configuration graph using the `next` function and
  maintain its state using a struct of type `state`.
*)

open Base
open Common
open Configuration
open Env
open ProblemDefs
open Lang
open Utils
module G = Graph.Imperative.Digraph.Concrete (Subconf)
module O = Config.Optims
include G

module XG = Graph.Graphviz.Dot (struct
  include G
  (* use the graph module from above *)

  let edge_attributes (e1, _) =
    let s1 = Subconf.to_string e1 in
    [ `Label s1; `Color 4711 ]
  ;;

  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [ `Shape `Box ]
  let vertex_name v = Subconf.to_string v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

type mark =
  | Realizable
  | Unrealizable
  | Failed
  | Unsolved

type edge_mark =
  | EAddArg of int * int
  | ERemArg of int * int

(** A type to represent the state of the configuration graph exploration.
  We need to remember the graph and mark configurations as solved or not.
*)
type state =
  { st_graph : t (** The graph of configurations. *)
  ; st_marks : mark Hashtbl.M(Subconf).t
        (**
        A negative mark means unrealizable.
        A positive mark means a solution has been found.
    Otherwise, a mark of 0 means it has not been solved.
  *)
  ; st_emarks : edge_mark Hashtbl.M(SubconfEdge).t
  ; st_root : Subconf.t (**
  The maximum configuration of the graph.
      *)
  ; st_super : conf
        (** A configuration with more information that must
      be larger than any configuration in the graph. *)
  ; st_super_subc : Subconf.t
  ; st_ctx : env (** The orginal environment of the super configuration. *)
  ; st_cache : ECache.t
  ; st_strategy : Config.Optims.exploration_strategy
  }

let out_graph (s : state) (filename : string) =
  let oc = Stdio.Out_channel.create filename in
  XG.output_graph oc s.st_graph;
  Stdio.Out_channel.close oc
;;

let mark_unrealizable (s : state) (conf : Subconf.t) =
  Hashtbl.set s.st_marks ~key:conf ~data:Unrealizable
;;

let mark_realizable (s : state) (conf : Subconf.t) =
  Hashtbl.set s.st_marks ~key:conf ~data:Realizable
;;

let mark_failed (s : state) (conf : Subconf.t) =
  Hashtbl.set s.st_marks ~key:conf ~data:Failed
;;

let is_unmarked (s : state) (conf : Subconf.t) =
  match Hashtbl.find s.st_marks conf with
  | Some Unsolved | None -> true
  | _ -> false
;;

let mark_add_arg
    (s : state)
    (conf_orig : Subconf.t)
    (conf_dest : Subconf.t)
    ((a, b) : int * int)
  =
  Hashtbl.set s.st_emarks ~key:(conf_orig, conf_dest) ~data:(EAddArg (a, b))
;;

let cache (s : state) (u : unrealizability_witness list) =
  let add_as_eset uc =
    let conv witness = ECache.norm witness.witness_eqn in
    match conv uc.ci, conv uc.cj with
    | Some ei, Some ej ->
      let eset = ExpressionSet.of_list [ ei; ej ] in
      Utils.Log.verbose
        Fmt.(
          fun fmt () ->
            pf
              fmt
              "CACHE: unrealizable sample {%a}"
              (list ~sep:semi Expression.pp)
              (Set.elements eset));
      ECache.add s.st_cache eset
    | _ ->
      Utils.Log.verbose_msg "CACHE: failed.";
      (* If failure to convert, do nothing; we cannot cache just one expression. *)
      ()
  in
  List.iter ~f:add_as_eset u
;;

let check_unrealizable_from_cache (ctx : env) (p : PsiDef.t) (s : state) =
  (* Fmt.(pf stdout "@.== > CACHE:@;%a@." ECache.dump s.cache); *)
  let rstar_t, _ = get_rstar ctx p !Utils.Config.Optims.rstar_limit in
  let eset =
    let eqns, _ =
      Se2gis.Equations.make
        ~silent:true
        ~count_reused_predicates:false
        ~ctx
        ~p
        ~lifting:Se2gis.Lifting.empty_lifting
        rstar_t
    in
    ExpressionSet.of_list (List.filter_opt (List.map ~f:ECache.norm eqns))
  in
  (* Fmt.(pf stdout "@.==> EXPRS:@;%a@." ExpressionSet.pp eset); *)
  ECache.check_subset eset s.st_cache
;;

(** `expand_down g conf` adds the edges from `conf` to all its refinements in `g`.
  If `~use_po` is set to false, then the expand algorithm does not check whether
  the configuration to expand is realizable or not (the partial order between
  configurations is not used to prune unrealizable cases).
  `use_po` is `true` by default.
*)
let expand_down ?(mark = Unsolved) (s : state) (conf : Subconf.t) : unit =
  List.iter (Subconf.drop_arg conf) ~f:(fun ((unknown, added_arg), c) ->
      match Hashtbl.find s.st_marks c with
      (* Already solved: no need to add new edge. *)
      | Some Realizable | Some Unrealizable | Some Failed -> ()
      | Some Unsolved ->
        Hashtbl.set s.st_emarks ~key:(conf, c) ~data:(ERemArg (unknown, added_arg));
        add_edge s.st_graph conf c
      | None ->
        (match mark with
        | Unsolved | Unrealizable -> Hashtbl.set s.st_marks ~key:c ~data:mark
        | _ -> ());
        Hashtbl.set s.st_emarks ~key:(conf, c) ~data:(ERemArg (unknown, added_arg));
        add_edge s.st_graph conf c)
;;

(** `expand_down g conf` adds the edges from `conf` to all its expansions in `g`.
  If `~use_po` is set to false, then the expand algorithm does not check whether
  the configuration to expand is realizable or not (the partial order between
  configurations is not used to prune unrealizable cases).
  `use_po` is `true` by default.
*)
let expand_up ?(mark = Unsolved) (s : state) (conf : Subconf.t) : unit =
  List.iter
    (Subconf.add_arg ~sup:(Subconf.of_conf s.st_super) conf)
    ~f:(fun ((unknown, added_arg), c) ->
      match Hashtbl.find s.st_marks c with
      (* Already solved: no need to add new edge. *)
      | Some Realizable | Some Unrealizable | Some Failed -> ()
      | Some Unsolved ->
        Hashtbl.set s.st_emarks ~key:(conf, c) ~data:(EAddArg (unknown, added_arg));
        add_edge s.st_graph conf c
      | None ->
        (match mark with
        | Unsolved | Realizable -> Hashtbl.set s.st_marks ~key:c ~data:mark
        | _ -> ());
        Hashtbl.set s.st_emarks ~key:(conf, c) ~data:(EAddArg (unknown, added_arg));
        add_edge s.st_graph conf c)
;;

let to_explore (s : state) (orig : Subconf.t) (dest : Subconf.t) =
  let orig_mark = Hashtbl.find s.st_marks orig in
  let dest_mark = Hashtbl.find s.st_marks dest in
  match Hashtbl.find s.st_emarks (orig, dest) with
  | Some (ERemArg _) ->
    (match orig_mark, dest_mark with
    | Some Unrealizable, _ -> false
    | _, Some Unrealizable -> false
    | _, Some Failed -> not !Utils.Config.node_failure_behavior
    | _ -> true)
  | Some (EAddArg _) ->
    (match orig_mark, dest_mark with
    | Some Realizable, _ -> false
    | _, Some Realizable -> false
    | _, Some Failed -> not !Utils.Config.node_failure_behavior
    | _ -> true)
  | None ->
    Log.error_msg "Edge not found";
    false
;;

(**
  Find the next 0-marked configuration in the graph.
  Return None if there is no such configuration.
*)
let next ?(shuffle = false) (s : state) : Subconf.t option =
  let q = Queue.create () in
  let perm_opt x = if shuffle then List.permute x else x in
  Queue.enqueue q s.st_root;
  let rec loop () =
    let explore_next curr =
      let children = List.filter ~f:(to_explore s curr) (succ s.st_graph curr) in
      Queue.enqueue_all q (perm_opt children);
      loop ()
    in
    Option.bind (Queue.dequeue q) ~f:(fun curr ->
        match Hashtbl.find s.st_marks curr with
        | None ->
          Hashtbl.set s.st_marks ~key:curr ~data:Unsolved;
          Some curr
        | Some Unsolved -> Some curr
        | Some Failed ->
          if !Utils.Config.node_failure_behavior then loop () else explore_next curr
        | Some _ -> explore_next curr)
  in
  loop ()
;;

(**
  Find the next 0-marked configuration in the graph.
  Return None if there is no such configuration.
*)
let next_dfs ?(shuffle = false) (s : state) : Subconf.t option =
  let q = Stack.create () in
  Stack.push q s.st_root;
  let rec loop () =
    let explore_next curr =
      let children = List.filter ~f:(to_explore s curr) (succ s.st_graph curr) in
      List.iter (if shuffle then List.permute children else children) ~f:(Stack.push q);
      loop ()
    in
    Option.bind (Stack.pop q) ~f:(fun curr ->
        match Hashtbl.find s.st_marks curr with
        | Some Unsolved -> Some curr
        | Some Failed ->
          if !Utils.Config.node_failure_behavior then loop () else explore_next curr
        | Some _ -> explore_next curr
        | None ->
          Hashtbl.set s.st_marks ~key:curr ~data:Unsolved;
          Some curr)
  in
  loop ()
;;

(** Generate the inital graph of configurations of a PMRS with unknowns. *)
let generate_configurations ?(strategy = O.ESTopDown) (ctx : env) (p : PMRS.t) : state =
  let super = max_configuration ctx p in
  let root =
    match strategy with
    | ESTopDown -> Subconf.of_conf super
    | ESBottomUp -> Subconf.zero_of_conf super
  in
  let size = subconf_count super in
  let st_graph = create ~size () in
  add_vertex st_graph root;
  (* Vertex marks *)
  let st_marks = Hashtbl.create (module Subconf) ~size in
  Hashtbl.set st_marks ~key:root ~data:Unsolved;
  (* Edge marks*)
  let st_emarks = Hashtbl.create (module SubconfEdge) ~size in
  let st_cache = ctx >- ECache.create () in
  { st_graph
  ; st_marks
  ; st_emarks
  ; st_root = root
  ; st_super = super
  ; st_super_subc = Subconf.of_conf super
  ; st_ctx = ctx
  ; st_cache
  ; st_strategy = strategy
  }
;;
