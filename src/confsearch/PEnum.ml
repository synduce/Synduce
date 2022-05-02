open Common.ProblemDefs
open Base
open Lang
open Term
open PMRS
open Utils

(** A type for refinement location. *)
type rloc =
  { rid : int (**  The rule index. *)
  ; rxi : variable (** Each location is associated to a variable. *)
  ; rtokens : VarSet.t (** Each location has some tokens. *)
  }

let default_rloc =
  { rid = -1; rxi = Variable.mk (Context.create ()) "null"; rtokens = VarSet.empty }
;;

let _pp_rloc ~(ctx : Context.t) (frmt : Formatter.t) (r : rloc) =
  Fmt.(pf frmt "@[[%i:%a]<%a>@]" r.rid (Variable.pp ctx) r.rxi (VarSet.pp ctx) r.rtokens)
;;

(* ============================================================================================= *)
(*                    ANALYSIS FUNCTIONS                                                         *)
(* ============================================================================================= *)
let analyze_rec_args_at_loc ~(ctx : Context.t) (p : PsiDef.t) (r : rloc) =
  match Map.find p.PsiDef.target.prules r.rid with
  | Some (_, lhs_args, _, _) ->
    Set.partition_tf r.rtokens ~f:(fun v ->
        (not (RType.is_base (Variable.vtype_or_new ctx v)))
        (* Only the last element can be deconstructed in the PMRS model. *)
        && List.is_prefix lhs_args ~equal:Variable.equal ~prefix:[ v ])
  | None -> VarSet.empty, VarSet.empty
;;

let safe_remove_unknown ~(ctx : Context.t) (r : rloc) p =
  let unknown_in_other_rules =
    Map.existsi p.PsiDef.target.prules ~f:(fun ~key ~data ->
        let _, _, _, rhs = data in
        (not (key = r.rid)) && Set.mem (Analysis.free_variables ~ctx rhs) r.rxi)
  in
  if unknown_in_other_rules
  then p.PsiDef.target.psyntobjs
  else Set.remove p.PsiDef.target.psyntobjs r.rxi
;;

let applicable_nonterminals ~(ctx : Context.t) (p : PsiDef.t) (args : TermSet.t) =
  let f nont =
    let in_typs, _ = RType.fun_typ_unpack (Variable.vtype_or_new ctx nont) in
    let arg_choices =
      List.fold_left
        in_typs
        ~init:[ [], args ]
        ~f:(fun args_so_far in_ty ->
          List.concat_map args_so_far ~f:(fun (ts, rem_args) ->
              List.map
                ~f:(fun v -> ts @ [ v ], Set.remove rem_args v)
                (Set.elements (TermSet.filter_by_type rem_args in_ty))))
    in
    List.map ~f:(fun (argvs, _) -> nont, argvs) arg_choices
  in
  List.concat_map ~f (Set.elements p.PsiDef.target.pnon_terminals)
;;

(* ============================================================================================= *)
(*                    RECURSION SKELETON EXPANSION FUNCTIONS                                     *)
(* ============================================================================================= *)

let extend_function
    ~(ctx : Context.t)
    ~from:(f : variable)
    ~to_:(g : variable)
    (extra_args : term list)
  =
  let case _ t =
    match t.tkind with
    | TApp ({ tkind = TVar f'; _ }, args) when Variable.equal f f' ->
      Some (mk_app (mk_var ctx g) (args @ extra_args))
    | TVar f' when Variable.equal f f' -> Some (mk_app (mk_var ctx g) extra_args)
    | _ -> None
  in
  transform ~case
;;

let mk_new_rule ~(ctx : Context.t) xi (nt, args, decons_arg, rhs) (constrname, constrargs)
  =
  let pat_vars =
    List.map
      ~f:(fun typ ->
        Variable.mk ctx ~t:(Some typ) (Alpha.fresh ~s:decons_arg.vname ctx.names))
      constrargs
  in
  let new_scalars =
    List.filter ~f:(fun v -> RType.is_base (Variable.vtype_or_new ctx v)) pat_vars
  in
  let pat = PatConstr (constrname, List.map ~f:mk_pat_var pat_vars) in
  let new_args = List.map ~f:(mk_var ctx) new_scalars in
  let new_unknown =
    (* Input type is extended by adding arguments *)
    let in_t, out_t = RType.fun_typ_unpack (Variable.vtype_or_new ctx xi) in
    let extra_t = List.map ~f:type_of new_args in
    let t = Some (RType.fun_typ_pack (in_t @ extra_t) out_t) in
    Variable.mk ctx ~t (Alpha.fresh ~s:xi.vname ctx.names)
  in
  let new_rhs = extend_function ~ctx ~from:xi ~to_:new_unknown new_args rhs in
  let new_rewrite_rule = nt, args, Some pat, new_rhs in
  new_unknown, new_rewrite_rule
;;

let with_args ~(ctx : Context.t) ~(p : PsiDef.t) (r : rloc) (cargs : term list) =
  let new_xi =
    let in_t, re_t = RType.fun_typ_unpack (Variable.vtype_or_new ctx r.rxi) in
    (* Input type is extended by adding arguments *)
    let t = Some (RType.fun_typ_pack (in_t @ List.map ~f:type_of cargs) re_t) in
    Variable.mk ctx ~t (Alpha.fresh ~s:r.rxi.vname ctx.names)
  in
  let add_arg_and_replace_unknown _t =
    let nt, lhs, pat, rhs = Map.find_exn p.PsiDef.target.prules r.rid in
    nt, lhs, pat, extend_function ~ctx ~from:r.rxi ~to_:new_xi cargs rhs
  in
  let new_psi_target =
    { p.PsiDef.target with
      psyntobjs = Set.add (safe_remove_unknown ~ctx r p) new_xi
    ; prules = Map.update p.PsiDef.target.prules r.rid ~f:add_arg_and_replace_unknown
    }
  in
  PsiDef.{ p with id = new_psi_id (); target = new_psi_target }, { r with rxi = new_xi }
;;

let with_deconstruction ~(ctx : Context.t) ~(p : PsiDef.t) (r : rloc) (dec : variable)
    : (PsiDef.t * VarSet.t) option
  =
  let deconstruct_one (nt, lhs_args, _, rhs) v =
    match RType.get_variants ctx.types (Variable.vtype_or_new ctx v) with
    | _ :: _ as cases ->
      let last_arg = List.last_exn lhs_args in
      let other_args = List.drop_last_exn lhs_args in
      let new_cases =
        List.map ~f:(mk_new_rule ~ctx r.rxi (nt, other_args, last_arg, rhs)) cases
      in
      let new_rules =
        match new_cases with
        (* No constructor. *)
        | [] -> None
        (* Datatype has a single constructor. *)
        | [ (a, b) ] ->
          Some (VarSet.singleton a, Map.set p.PsiDef.target.prules ~key:r.rid ~data:b)
        (* Multiple rules need to be addded to the PMRS. *)
        | (new_unknown, new_rewrite_rule) :: tl ->
          let map0 = Map.set p.PsiDef.target.prules ~key:r.rid ~data:new_rewrite_rule in
          (* Map cannot be empty here. *)
          let max_rule_id, _ = Map.max_elt_exn map0 in
          let new_unknowns, new_rules, _ =
            List.fold_left
              tl
              ~init:(VarSet.singleton new_unknown, map0, max_rule_id + 1)
              ~f:(fun (xis, rules, mrid) (new_unknown, new_rule) ->
                ( Set.add xis new_unknown
                , Map.add_exn rules ~key:mrid ~data:new_rule
                , mrid + 1 ))
          in
          Some (new_unknowns, new_rules)
      in
      (match new_rules with
      | Some (n_xis, nr) ->
        let psyntobjs = safe_remove_unknown ~ctx r p in
        Some
          ( PsiDef.
              { p with
                id = new_psi_id ()
              ; target =
                  { p.PsiDef.target with
                    psyntobjs = Set.union n_xis psyntobjs
                  ; prules = nr
                  }
              }
          , n_xis )
      | None -> None)
    | _ -> None
    (* No variants, nothing to be done here. *)
  in
  Option.(
    bind
      ~f:(fun rrule -> deconstruct_one rrule dec)
      (Map.find p.PsiDef.target.prules r.rid))
;;

(** An edge links to synthesis problems, with a label explaining how the synthesis
    problem was modified.
*)
module ExtensionPoint = struct
  (** Program transformation kind. *)
  type kind =
    | RecDeconstr of variable
    | CstExt of term list
    | RecExt of term list
    | NoExt

  type t = rloc * kind

  let compare k1 k2 = Poly.compare k1 k2
  let default : t = default_rloc, NoExt
  let _loc_of (ept : t) = fst ept
  let mk_rec_deconst r v = r, RecDeconstr v
  let mk_cst_ext r args = r, CstExt args
  let mk_rec_ext r args = r, RecExt args

  let apply ~(ctx : Context.t) ((r, k) : t) (p : PsiDef.t) : (PsiDef.t * VarSet.t) option =
    match k with
    | RecDeconstr d -> with_deconstruction ~ctx ~p r d
    | RecExt args ->
      let p', r' = with_args ~ctx ~p r args in
      Some (p', VarSet.singleton r'.rxi)
    | CstExt args ->
      let p', r' = with_args ~ctx ~p r args in
      Some (p', VarSet.singleton r'.rxi)
    | NoExt -> None
  ;;
end

let ext_with_rec_args ~(ctx : Context.t) ~(p : PsiDef.t) (r : rloc)
    : ExtensionPoint.t list
  =
  if Set.is_empty r.rtokens
  then []
  else (
    (* Analayse whether the arguments are usable as-is or need to be unpacked. *)
    let args_require_deconstruction, args_usable = analyze_rec_args_at_loc ~ctx p r in
    let arg_choices =
      List.map
        ~f:(fun (f, args) -> mk_app (mk_var ctx f) args)
        (applicable_nonterminals ~ctx p (TermSet.of_varset args_usable))
    in
    List.map ~f:(ExtensionPoint.mk_rec_ext r) (subsets arg_choices)
    @ List.map
        ~f:(ExtensionPoint.mk_rec_deconst r)
        (Set.elements args_require_deconstruction))
;;

let ext_with_constant_args ~(ctx : Context.t) (r : rloc) : ExtensionPoint.t option =
  let cargs =
    let of_base_type, _ =
      Set.partition_tf r.rtokens ~f:(fun v -> RType.is_base (Variable.vtype_or_new ctx v))
    in
    Set.elements of_base_type
  in
  match cargs with
  | _ :: _ -> Some (ExtensionPoint.mk_cst_ext r (List.map ~f:(mk_var ctx) cargs))
  | _ -> None
;;

let find_extension_locs
    ?(mask = VarSet.empty)
    ?(only = None)
    ~(ctx : Context.t)
    (p : PsiDef.t)
  =
  let g = p.PsiDef.target in
  let synt_objs =
    match only with
    | Some s -> s
    | None -> Set.diff g.psyntobjs mask
  in
  let paramset = VarSet.of_list g.pargs in
  (* Check for each rule.  *)
  let f ((rule_id, rrule) : int * rewrite_rule) =
    let _, lhs_args, lhs_pat, rhs = rrule in
    let rhs_argset = Analysis.free_variables ~ctx ~include_functions:false rhs in
    let rhs_funcs = Analysis.free_variables ~ctx ~include_functions:true rhs in
    let lhs_argset =
      Set.union
        (VarSet.of_list lhs_args)
        (Option.value_map lhs_pat ~default:VarSet.empty ~f:(fun p ->
             Analysis.free_variables ~ctx (Term.term_of_pattern ctx p)))
    in
    if not (Set.are_disjoint synt_objs rhs_funcs)
    then (
      let more_args = Set.diff (Set.union paramset lhs_argset) rhs_argset in
      if not (Set.is_empty more_args)
      then (
        let unknowns = Set.inter synt_objs rhs_funcs in
        List.concat_map
          ~f:(fun rxi ->
            let _, _, _, rhs_rrule = rrule in
            let more_args = Set.diff more_args (Analysis.argset_of rxi ~ctx rhs_rrule) in
            if Set.is_empty more_args
            then []
            else [ { rid = rule_id; rxi; rtokens = more_args } ])
          (Set.elements unknowns))
      else [])
    else []
  in
  List.concat_map ~f (Map.to_alist g.prules)
;;

(* ============================================================================================= *)
(*                    MAIN ENTRY POINT                                                           *)
(* ============================================================================================= *)

open Graph
module PGraph = Imperative.Digraph.ConcreteLabeled (PsiDef) (ExtensionPoint)

module EdgeWeights = struct
  type edge = PGraph.E.t
  type t = int

  let weight _ = 1
  let compare = compare
  let add = ( + )
  let zero = 0
end

module PPath = Path.Dijkstra (PGraph) (EdgeWeights)
module PBfs = Traverse.Bfs (PGraph)

let root_program : PsiDef.t option = None
let enumeration_graph = PGraph.create ~size:20 ()
let problem_versions : (int, PsiDef.t) Hashtbl.t = Hashtbl.create ~size:20 (module Int)

let list_problems () : PsiDef.t list =
  List.rev
    (PGraph.fold_vertex (fun new_psi accum -> new_psi :: accum) enumeration_graph [])
;;

let clear () =
  PGraph.clear enumeration_graph;
  Hashtbl.clear problem_versions
;;

let _root_path (p : PsiDef.t) =
  match root_program with
  | Some rp ->
    (try Some (fst (PPath.shortest_path enumeration_graph rp p)) with
    | Caml.Not_found -> None)
  | None -> None
;;

let rec expand_vertex ?(mask = VarSet.empty) ?(n = 10) ~(ctx : Context.t) (p : PsiDef.t) =
  if n < 0
  then ()
  else (
    PGraph.add_vertex enumeration_graph p;
    List.iter (find_extension_locs ~ctx ~mask p) ~f:(fun rloc ->
        (* First try adding constant-time computable args. *)
        let p', rloc' =
          match ext_with_constant_args ~ctx rloc with
          | Some ept ->
            (match ExtensionPoint.apply ~ctx ept p with
            | Some (pd, new_xis) ->
              let new_edge = PGraph.E.create p ept pd in
              PGraph.add_edge_e enumeration_graph new_edge;
              pd, { rloc with rxi = Set.max_elt_exn new_xis }
            | None -> p, rloc)
          | None -> p, rloc
        in
        List.iter (ext_with_rec_args ~ctx ~p:p' rloc') ~f:(fun ept ->
            match ExtensionPoint.apply ~ctx ept p' with
            | Some (new_p, new_xis) ->
              PGraph.add_edge_e enumeration_graph (PGraph.E.create p' ept new_p);
              let new_mask =
                match snd ept with
                | RecDeconstr _ -> mask
                | _ -> Set.union mask new_xis
              in
              expand_vertex ~ctx ~mask:new_mask ~n:(n - 1) new_p
            | _ -> ())))
;;

let enumerate_p ~(ctx : Context.t) (p : PsiDef.t) =
  expand_vertex ~ctx p;
  list_problems ()
;;

(* ============================================================================================= *)
(*                    PROGRAM SEARCH                                                             *)
(* ============================================================================================= *)

let _program_search (p : PsiDef.t) = expand_vertex p
