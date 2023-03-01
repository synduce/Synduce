(** A configuration is a set of argument for unknowns in a recursion skeleton.
    This module contains operations on configurations, notable a submodule  `Subconf`
    for representing configuration refinements.
*)

open Base
open Env
open Lang
open Term
open Utils

(**
  A configuration is a map from variables (the unknown functions) to a list of arguments.
*)
type conf = term list VarMap.t

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

module Subconf = struct
  (**
  A subconfiguration (relative to some configuration) is a map from an unknown's variable
  id to a list of integer representing the index of the argument in the super-configuration
  argument list of that unknown.
*)
  type t = int list IntMap.t

  let to_string (s : t) =
    String.concat
      ~sep:"-"
      (List.map
         ~f:(fun (i, l) ->
           Int.to_string i ^ ":" ^ String.concat ~sep:"." (List.map ~f:Int.to_string l))
         (Map.to_alist s))
  ;;

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

  (* ========= Constructing configurations =============== *)

  (** Return the configuration as a subconf. *)
  let of_conf (conf : conf) : t =
    Map.fold
      conf
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data m ->
        Map.set m ~key:key.vid ~data:(List.mapi ~f:(fun i _ -> i) data))
  ;;

  (** Create a subconfiguration without arguments but where each unknown is associated
      with a list of arguments.  *)
  let zero_of_conf (conf : conf) : t =
    Map.fold
      conf
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data:_ m -> Map.set m ~key:key.vid ~data:[])
  ;;

  (** Create the largest subconfiguration that has no recursive call in it.  *)
  let largest_ctime_conf (conf : conf) : t =
    Map.fold
      conf
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data m ->
        let const_args =
          let f i t =
            match t.tkind with
            | TVar _ -> Some i
            | _ -> None
          in
          List.filter_mapi ~f data
        in
        Map.set m ~key:key.vid ~data:const_args)
  ;;

  (** Create the subconfiguration mapping to the ids of function calls.  *)
  let rec_calls_conf (conf : conf) : t =
    Map.fold
      conf
      ~init:(Map.empty (module Int))
      ~f:(fun ~key ~data m ->
        let const_args =
          let f i t =
            match t.tkind with
            | TVar _ -> None
            | _ -> Some i
          in
          List.filter_mapi ~f data
        in
        Map.set m ~key:key.vid ~data:const_args)
  ;;

  (** Return the subconf as a configuration, given the configuration it refines. *)
  let to_conf ~(sup : conf) (sub : t) : conf =
    Map.mapi sup ~f:(fun ~key:v ~data:args ->
        match Map.find sub v.vid with
        | Some indexes -> sub_list args indexes
        | None -> failwith "(supconf) should only be used on a proper subconfiguration.")
  ;;

  (**
  Create as many subconfigurations as possible from dropping an argument for an unknown.
  Optional filter is a map from unknown id to a list of ids that are to be dropped.
 *)
  let drop_arg ?(filter = None) (conf : t) : ((int * int) * t) list =
    let drop_one (unknown, args) =
      let in_filter i =
        match Option.bind ~f:(fun flt -> Map.find flt unknown) filter with
        | Some l -> List.mem l ~equal:Int.equal i
        | None -> true
      in
      match args with
      | [] -> []
      | [ x ] -> [ (unknown, x), Map.set ~key:unknown ~data:[] conf ]
      | _ ->
        (* Configurations with all possible ways to drop one argument *)
        List.filter_mapi args ~f:(fun i k ->
            if in_filter k
            then (
              (* new args is current args without ith element *)
              let data = List.filteri ~f:(fun j _ -> not (j = i)) args in
              Some ((unknown, i), Map.set ~key:unknown ~data conf))
            else None)
    in
    List.concat_map (Map.to_alist conf) ~f:drop_one
  ;;

  (**
  Create as many subconfigurations as possible from adding an argument for an unknown.
 *)
  let add_arg ~(sup : t) (conf : t) : ((int * int) * t) list =
    let pick_in_sup unknown filter =
      match Map.find sup unknown with
      | Some l -> List.find ~f:(fun x -> not (filter x)) l
      | None -> None
    in
    let add_one (unknown, args) =
      match args with
      | [] ->
        (match pick_in_sup unknown (fun _ -> false) with
        | None -> []
        | Some a -> [ (unknown, a), Map.set ~key:unknown ~data:[ a ] conf ])
      | [ x ] ->
        (match pick_in_sup unknown (fun y -> y = x) with
        | None -> []
        | Some a -> [ (unknown, a), Map.set ~key:unknown ~data:[ x; a ] conf ])
      | _ ->
        let others =
          match Map.find sup unknown with
          | Some all ->
            List.filter all ~f:(fun x -> not (List.mem ~equal:Int.equal args x))
          | None -> []
        in
        (match others with
        | [] -> []
        | _ :: _ ->
          (* Configurations with all possible ways to add one argument *)
          List.map others ~f:(fun new_arg ->
              (unknown, new_arg), Map.set ~key:unknown ~data:(new_arg :: args) conf))
    in
    List.concat_map (Map.to_alist conf) ~f:add_one
  ;;

  let apply_diff ((must_add, loc_id, arg_id) : bool * int * int) (c : t) =
    let f ~key ~data =
      if key = loc_id
      then
        if must_add (* Add the argument. *)
        then data @ [ arg_id ] (* Remove the argument. *)
        else List.filter ~f:(fun x -> not (x = arg_id)) data
      else data
    in
    Map.mapi c ~f
  ;;

  let diff (c1 : t) (c2 : t) : (bool * int * int) list =
    let f ~key ~data accum =
      match Map.find c2 key with
      | Some data' ->
        accum
        (* All the elements in data not in data' are args to remove from c1 to get to c2 *)
        @ List.filter_map data ~f:(fun x ->
              if List.mem ~equal:Int.equal data' x then None else Some (false, key, x))
        @ (* All the elements in data' not in data are args to add to c1 to get to c2 *)
        List.filter_map data' ~f:(fun x ->
            if List.mem ~equal:Int.equal data x then None else Some (true, key, x))
      | None ->
        (* All the argument in c1 not in c2 must be removed. *)
        accum @ List.map data ~f:(fun x -> false, key, x)
    in
    Map.fold c1 ~init:[] ~f
  ;;

  module Lattice = struct
    let count_subs (a : t) =
      Map.fold ~init:1 ~f:(fun ~key:_ ~data accum -> accum * (2 ** List.length data)) a
    ;;

    let count_sups ~(sup : t) (a : t) =
      Map.fold
        ~init:1
        ~f:(fun ~key ~data accum ->
          let rem =
            match Map.find sup key with
            | Some r -> List.filter r ~f:(fun x -> not (List.mem data ~equal:Int.equal x))
            | None -> []
          in
          accum * (2 ** List.length rem))
        a
    ;;

    let join (a : t) (b : t) : t =
      Map.mapi a ~f:(fun ~key ~data ->
          let both_a_b =
            match Map.find b key with
            | Some bl -> bl @ data
            | None -> data
          in
          List.dedup_and_sort ~compare:Int.compare both_a_b)
    ;;

    let meet (a : t) (b : t) =
      Map.mapi a ~f:(fun ~key ~data ->
          let both_a_b =
            match Map.find b key with
            | Some bl -> List.filter data ~f:(fun x -> List.mem bl ~equal:Int.equal x)
            | None -> []
          in
          List.dedup_and_sort ~compare:Int.compare both_a_b)
    ;;
  end
end

module SubconfEdge = struct
  type t = Subconf.t * Subconf.t

  let compare ((a, a') : t) ((b, b') : t) : int =
    let c = Subconf.compare a b in
    if c = 0 then Subconf.compare a' b' else c
  ;;

  let equal ((a, a') : t) ((b, b') : t) : bool = Subconf.equal a b && Subconf.equal a' b'
  let hash ((a, a') : t) = Subconf.hash a + Subconf.hash a'
  let sexp_of_t ((a, a') : t) = Sexp.List [ Subconf.sexp_of_t a; Subconf.sexp_of_t a' ]
end

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
let base_type_args (ctx : env) ~(rule : PMRS.rewrite_rule) (p : PMRS.t) (vs : VarSet.t) =
  let base_type_vars =
    let on_var v =
      if RType.is_datatype ctx.ctx.types ((ctx @>- Variable.vtype_or_new) v)
      then []
      else [ (ctx @>- mk_var) v ]
    in
    List.concat_map (Set.elements vs) ~f:on_var
  in
  let is_allowed_app x =
    let nont, lhs_args, p, _ = rule in
    match p with
    | None ->
      not (Terms.equal (mk_app_v ctx.ctx nont (List.map ~f:(mk_var ctx.ctx) lhs_args)) x)
    | Some _ -> true
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
  let allowed_rec_func_apps = List.filter ~f:is_allowed_app rec_function_applications in
  TermSet.of_list (base_type_vars @ allowed_rec_func_apps)
;;

(** Build the argument map of the PMRS.
  The argument map is effectively the largest configuration: it is a map from each unknown
  to all the arguments available to the unknown.
*)
let max_configuration (ctx : env) (p : PMRS.t) : conf =
  let empty_conf = of_varset p.psyntobjs in
  let set_args_in_rule ~key:_ruleid ~data:(f, lhs_args, lhs_pat, rhs) vmap =
    (* Compute the set of local variables in the rule. *)
    let lhs_argset =
      Set.union
        (VarSet.of_list lhs_args)
        (Option.value_map lhs_pat ~default:VarSet.empty ~f:(fun p ->
             ctx >- Analysis.free_variables (Term.term_of_pattern ctx.ctx p)))
    in
    (* TODO: collect let-bound variables? Technically cannot provide more info,
      but has subexpression elim. simplification advantage.
    *)
    let args =
      base_type_args
        ctx
        p
        ~rule:(f, lhs_args, lhs_pat, rhs)
        (Set.union lhs_argset (VarSet.of_list p.pargs))
    in
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
let conf_of (p : PMRS.t) : conf =
  let init = of_varset p.psyntobjs in
  let join m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_ m ->
        match m with
        | `Both (v1, v2) -> Some (v1 @ v2)
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

(** [same_conf p1 p2] is true if [p1] and [p2] are in the same configuration. *)
let same_conf (p1 : PMRS.t) (p2 : PMRS.t) : bool =
  let c1 = conf_of p1
  and c2 = conf_of p2 in
  let s1 = Subconf.of_conf c1
  and s2 = Subconf.of_conf c2 in
  Subconf.(equal s1 s2)
;;

(** Apply a configuration to a given PMRS. The environment is copied before the type
  information for the unknowns is changed. The copied environment is then returned.
*)
let apply_configuration ~(ctx : env) (config : conf) (p : PMRS.t) : PMRS.t * env =
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
  let new_p =
    { p with
      prules =
        Map.map p.prules ~f:(fun (nt, args, pat, rhs) -> nt, args, pat, repl_in_rhs rhs)
    }
  in
  PMRS.(ctx >- infer_pmrs_types new_p), ctx
;;

let num_rec_calls ~(ctx : env) (c : conf) : int =
  let count_rec l = List.count l ~f:(fun x -> not (ctx >- Analysis.is_norec x)) in
  Map.fold c ~init:0 ~f:(fun ~key:_ ~data:args accum -> accum + count_rec args)
;;

let get_rstar ~(fuel : float) (ctx : env) (p : ProblemDefs.PsiDef.t) (k : int)
    : TermSet.t * TermSet.t
  =
  let start_t = Unix.gettimeofday () in
  let fuel_left () = Float.((100.0 * (Unix.gettimeofday () - start_t)) - fuel) in
  let x0 =
    mk_var
      ctx.ctx
      (Variable.mk ctx.ctx ~t:(Some (get_theta ctx)) (Alpha.fresh ctx.ctx.names))
  in
  let s = TermSet.of_list (ctx >- Analysis.expand_once x0) in
  let set_t0, set_u0 = Set.partition_tf ~f:(Expand.is_mr_all ~ctx p) s in
  let rec aux k (t, u) =
    if k <= 0 || Float.(fuel_left () < 0.)
    then t, u
    else (
      match Expand.expand_all ~fuel:(fuel_left ()) ~ctx p (t, u) with
      | Ok x -> aux (k - 1) x
      | Error _ -> t, u)
  in
  aux k (set_t0, set_u0)
;;
