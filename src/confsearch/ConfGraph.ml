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
  | EAddArg of bool * int * int
  | ERemArg of bool * int * int

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
        (** The edges of the configurations
      are marked as either adding arguments or removing arguments. *)
  ; st_root : Subconf.t (** The maximum configuration of the graph. *)
  ; mutable st_next_candidate : Subconf.t option
        (** The state can contain a candidate for
            the next configuration to return.*)
  ; st_recs : Subconf.t
        (** A subconfiguration mapping to the
      recursive call arguments. *)
  ; st_super : conf
        (** A configuration with more information that must
      be larger than any configuration in the graph. *)
  ; st_super_subc : Subconf.t
  ; st_ctx : env (** The orginal environment of the super configuration. *)
  ; st_cache : ECache.t
  ; st_strategy : Config.Optims.exploration_strategy
        (** The exploration strategy used in n
      the algorithm *)
  ; st_coverage_up : Subconf.t Queue.t (** The join of the configuration covered. *)
  ; st_coverage_down : Subconf.t Queue.t (** The meet of the configurations covered. *)
  ; mutable st_covered : int (** The number of configurations covered. *)
  ; mutable st_total_confs : int (** The number of total configurations. *)
  }

(** Return true if the pair of integer identifies an argument with
     a recursive call. *)
let is_rec_arg (s : state) ((u_id, a_id) : int * int) =
  match Map.find s.st_recs u_id with
  | Some l -> List.mem ~equal l a_id
  | None -> false
;;

let out_graph (s : state) (filename : string) =
  let oc = Stdio.Out_channel.create filename in
  XG.output_graph oc s.st_graph;
  Stdio.Out_channel.close oc
;;

let get_coverage_percentage (s : state) : float =
  100.0 *. (Int.to_float s.st_covered /. Int.to_float s.st_total_confs)
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

let mark_unsolved (s : state) (conf : Subconf.t) =
  Hashtbl.set s.st_marks ~key:conf ~data:Unsolved
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
    ((kind, a, b) : bool * int * int)
  =
  Hashtbl.set s.st_emarks ~key:(conf_orig, conf_dest) ~data:(EAddArg (kind, a, b))
;;

let mark_rem_arg
    (s : state)
    (conf_orig : Subconf.t)
    (conf_dest : Subconf.t)
    ((kind, a, b) : bool * int * int)
  =
  Hashtbl.set s.st_emarks ~key:(conf_orig, conf_dest) ~data:(ERemArg (kind, a, b))
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
  if ECache.is_empty s.st_cache
  then false
  else (
    (* Fmt.(pf stdout "@.== > CACHE:@;%a@." ECache.dump s.cache); *)
    let rstar_t, _ =
      get_rstar ~fuel:!Config.Optims.rstar_fuel ctx p !Utils.Config.Optims.rstar_limit
    in
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
    ECache.check_subset eset s.st_cache)
;;

(** `expand_down g conf` adds the edges from `conf` to all its refinements in `g`.
  If `~use_po` is set to false, then the expand algorithm does not check whether
  the configuration to expand is realizable or not (the partial order between
  configurations is not used to prune unrealizable cases).
  `use_po` is `true` by default.
*)
let expand_down ?(mark = Unsolved) (s : state) (conf : Subconf.t) : unit =
  let drop_arg_choices =
    if !Config.Optims.search_constant_variations
    then Subconf.drop_arg conf
    else Subconf.drop_arg ~filter:(Some s.st_recs) conf
  in
  List.iter drop_arg_choices ~f:(fun ((unknown, added_arg), c) ->
      match Hashtbl.find s.st_marks c with
      (* Already solved: no need to add new edge. *)
      | Some Realizable | Some Unrealizable | Some Failed -> ()
      | Some Unsolved ->
        let b = is_rec_arg s (unknown, added_arg) in
        Hashtbl.set s.st_emarks ~key:(conf, c) ~data:(ERemArg (b, unknown, added_arg));
        add_edge s.st_graph conf c
      | None ->
        (match mark with
        | Unsolved | Unrealizable -> Hashtbl.set s.st_marks ~key:c ~data:mark
        | _ -> ());
        let b = is_rec_arg s (unknown, added_arg) in
        Hashtbl.set s.st_emarks ~key:(conf, c) ~data:(ERemArg (b, unknown, added_arg));
        add_edge s.st_graph conf c)
;;

let update_coverage_down (s : state) (conf : Subconf.t) (m : mark) =
  let cnt_subs = Subconf.Lattice.count_subs conf in
  let cover_unr () =
    let add_cov =
      Queue.fold
        ~init:cnt_subs
        ~f:(fun cnt conf' ->
          let meet = Subconf.Lattice.meet conf' conf in
          let meet_cov = Subconf.Lattice.count_subs meet in
          cnt - meet_cov)
        s.st_coverage_down
    in
    s.st_covered <- s.st_covered + add_cov;
    Queue.enqueue s.st_coverage_down conf
  in
  match m with
  | Realizable -> s.st_covered <- s.st_covered + 1
  | Unrealizable -> cover_unr ()
  | Failed -> if !Utils.Config.node_failure_behavior then cover_unr () else ()
  | _ -> ()
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
        let b = is_rec_arg s (unknown, added_arg) in
        Hashtbl.set s.st_emarks ~key:(conf, c) ~data:(EAddArg (b, unknown, added_arg));
        add_edge s.st_graph conf c
      | None ->
        (match mark with
        | Unsolved | Realizable -> Hashtbl.set s.st_marks ~key:c ~data:mark
        | _ -> ());
        let b = is_rec_arg s (unknown, added_arg) in
        Hashtbl.set s.st_emarks ~key:(conf, c) ~data:(EAddArg (b, unknown, added_arg));
        add_edge s.st_graph conf c)
;;

let update_coverage_up (s : state) (conf : Subconf.t) (m : mark) =
  let cnt_sups = Subconf.Lattice.count_sups ~sup:s.st_super_subc conf in
  let cover_real () =
    let add_cov =
      Queue.fold
        ~init:cnt_sups
        ~f:(fun cnt conf' ->
          let meet = Subconf.Lattice.join conf' conf in
          let meet_cov = Subconf.Lattice.count_sups ~sup:s.st_super_subc meet in
          cnt - meet_cov)
        s.st_coverage_up
    in
    s.st_covered <- s.st_covered + add_cov;
    Queue.enqueue s.st_coverage_up conf
  in
  match m with
  | Realizable -> cover_real ()
  | Unrealizable -> s.st_covered <- s.st_covered + 1
  | _ -> ()
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

(** During the graph exploration, root causing can suggest a configuration to solve next.
    [add_next_candidate ~origin s c] adds the configuration [c] to the graph in the state [s],
    with a sequence of edges starting from [origin].
    Does nothing is there is no path that consists in edges only adding arguments to [origin] to get
    to [c].
*)
let add_next_candidate ~(origin : Subconf.t) (s : state) (c : Subconf.t) : unit =
  if not (is_unmarked s c)
  then ()
  else (
    let config_path = Subconf.diff origin c in
    let final_conf =
      List.fold config_path ~init:origin ~f:(fun curr (must_add, loc_id, arg_id) ->
          let c' = Subconf.apply_diff (must_add, loc_id, arg_id) curr in
          G.add_edge s.st_graph curr c';
          if is_unmarked s c' then mark_unsolved s c';
          if must_add
          then mark_add_arg s curr c' (must_add, loc_id, arg_id)
          else mark_rem_arg s curr c' (must_add, loc_id, arg_id);
          c')
    in
    s.st_next_candidate <- Some final_conf)
;;

(**
  Find the next 0-marked configuration in the graph.
  Return None if there is no such configuration.
*)
let next ?(shuffle = false) (s : state) : Subconf.t option =
  match s.st_next_candidate with
  | Some next ->
    s.st_next_candidate <- None;
    Some next
  | None ->
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
    | ESBottomUp ->
      if !Config.Optims.search_constant_variations
      then (* Start from configuration with zero args. *)
        Subconf.zero_of_conf super
      else
        (* Start from configuration with no recursive calls, but otherwise
          all constant-time arguments.
         *)
        Subconf.largest_ctime_conf super
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
  let st_super_subc = Subconf.of_conf super in
  { st_graph
  ; st_marks
  ; st_emarks
  ; st_root = root
  ; st_next_candidate = None
  ; st_super = super
  ; st_recs = Subconf.rec_calls_conf super
  ; st_super_subc
  ; st_ctx = ctx
  ; st_cache
  ; st_strategy = strategy
  ; st_coverage_up = Queue.create ()
  ; st_coverage_down = Queue.create ()
  ; st_covered = 0
  ; st_total_confs = Subconf.(Lattice.count_subs st_super_subc)
  }
;;
