(** A configuration graph represents the partial order between configurations.
  An algorithm can build the configuration graph using the `next` function and
  maintain its state using a struct of type `state`.
*)

open Base
open Configuration
open Common
open Env
open ProblemDefs
open Lang
module G = Graph.Imperative.Digraph.Concrete (Subconf)
include G

type mark =
  | Realizable
  | Unrealizable
  | Failed
  | Unsolved

(** A type to represent the state of the configuration graph exploration.
  We need to remember the graph and mark configurations as solved or not.
*)
type state =
  { graph : t (** The graph of configurations. *)
  ; marks : mark Hashtbl.M(Subconf).t
        (**
        A negative mark means unrealizable.
        A positive mark means a solution has been found.
    Otherwise, a mark of 0 means it has not been solved.
  *)
  ; root : Subconf.t (**
  The maximum configuration of the graph.
      *)
  ; super : conf
        (** A configuration with more information that must
      be larger than any configuration in the graph. *)
  ; ctx : env (** The orginal environment of the super configuration. *)
  ; cache : ECache.t
  }

let mark_unrealizable (s : state) (conf : Subconf.t) =
  Hashtbl.set s.marks ~key:conf ~data:Unrealizable
;;

let mark_realizable (s : state) (conf : Subconf.t) =
  Hashtbl.set s.marks ~key:conf ~data:Realizable
;;

let mark_failed (s : state) (conf : Subconf.t) =
  Hashtbl.set s.marks ~key:conf ~data:Failed
;;

let is_unmarked (s : state) (conf : Subconf.t) =
  match Hashtbl.find s.marks conf with
  | Some Unsolved | None -> true
  | _ -> false
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
      ECache.add s.cache eset
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
      Se2gis.Equations.make ~ctx ~p ~lifting:Se2gis.Lifting.empty_lifting rstar_t
    in
    ExpressionSet.of_list (List.filter_opt (List.map ~f:ECache.norm eqns))
  in
  (* Fmt.(pf stdout "@.== > EXPRS:@;%a@." ExpressionSet.pp eset); *)
  ECache.check_subset eset s.cache
;;

(** `expand g conf` adds the edges from `conf` to all its refinements in `g`.
  If `~use_po` is set to false, then the expand algorithm does not check whether
  the configuration to expand is realizable or not (the partial order between
  configurations is not used to prune unrealizable cases).
  `use_po` is `true` by default.
*)
let expand ?(mark = Unsolved) (s : state) (conf : Subconf.t) : unit =
  List.iter (Subconf.drop_arg conf) ~f:(fun c ->
      match Hashtbl.find s.marks c with
      (* Already solved: no need to add new edge. *)
      | Some Realizable | Some Unrealizable | Some Failed -> ()
      | Some Unsolved -> add_edge s.graph conf c
      | None ->
        Hashtbl.set s.marks ~key:c ~data:mark;
        add_edge s.graph conf c)
;;

let to_explore (s : state) (conf : Subconf.t) =
  match Hashtbl.find s.marks conf with
  | Some Unrealizable -> false
  | Some Failed -> not !Utils.Config.node_failure_behavior
  | _ -> true
;;

(**
  Find the next 0-marked configuration in the graph.
  Return None if there is no such configuration.
*)
let next ?(shuffle = false) (s : state) : Subconf.t option =
  let q = Queue.create () in
  Queue.enqueue q s.root;
  let rec loop () =
    Option.bind (Queue.dequeue q) ~f:(fun curr ->
        match Hashtbl.find s.marks curr with
        | None ->
          Hashtbl.set s.marks ~key:curr ~data:Unsolved;
          Some curr
        | Some Unsolved -> Some curr
        | Some Unrealizable -> loop ()
        | Some Failed ->
          if !Utils.Config.node_failure_behavior
          then loop ()
          else (
            let children = List.filter ~f:(to_explore s) (succ s.graph curr) in
            Queue.enqueue_all q (if shuffle then List.permute children else children);
            loop ())
        | Some _ ->
          let children = List.filter ~f:(to_explore s) (succ s.graph curr) in
          Queue.enqueue_all q (if shuffle then List.permute children else children);
          loop ())
  in
  loop ()
;;

(**
  Find the next 0-marked configuration in the graph.
  Return None if there is no such configuration.
*)
let next_dfs ?(shuffle = false) (s : state) : Subconf.t option =
  let q = Stack.create () in
  Stack.push q s.root;
  let rec loop () =
    Option.bind (Stack.pop q) ~f:(fun curr ->
        match Hashtbl.find s.marks curr with
        | Some Unsolved -> Some curr
        | Some Unrealizable -> loop ()
        | Some _ ->
          let children = List.filter ~f:(to_explore s) (succ s.graph curr) in
          List.iter
            (if shuffle then List.permute children else children)
            ~f:(Stack.push q);
          loop ()
        | None ->
          Hashtbl.set s.marks ~key:curr ~data:Unsolved;
          Some curr)
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
  Hashtbl.set marks ~key:root ~data:Unsolved;
  let cache = ctx >- ECache.create () in
  { graph; marks; root; super; ctx; cache }
;;
