open Base
open Common
open Env
open ProblemDefs
open Lang
open Utils
module G = ConfGraph

let total_configurations = ref 0

let single_configuration_solver
    ?(lemmas = Se2gis.Lemmas.empty_lemmas ())
    ~(ctx : env)
    (p : PsiDef.t)
    : Syguslib.Sygus.solver_response segis_response
  =
  let resp =
    if !Config.Optims.use_segis (* Symbolic CEGIS. *)
    then Se2gis.Baselines.algo_segis ~ctx p
    else if !Config.Optims.use_cegis (* Concrete CEGIS. *)
    then
      Se2gis.Baselines.algo_cegis ~ctx p
      (* Default algorithm: best combination of techniques (TODO) *)
    else Se2gis.Main.solve_problem ~lemmas ~ctx p
  in
  (* Print intermediate result if we are looking for more than one solution *)
  if !Config.Optims.max_solutions > 0
  then (
    let elapsed = Stats.get_glob_elapsed ()
    and verif = !Stats.verif_time in
    match resp with
    | Realizable s ->
      AlgoLog.show_stat_intermediate_solution
        ~ctx
        p
        (Some (Either.first s))
        elapsed
        verif
        !total_configurations
    | Unrealizable u ->
      AlgoLog.show_stat_intermediate_solution
        ~ctx
        p
        (Some (Either.second u))
        elapsed
        verif
        !total_configurations
    | Failed _ ->
      AlgoLog.show_stat_intermediate_solution
        ~ctx
        p
        None
        elapsed
        verif
        !total_configurations);
  (* Save stats an restart counters. *)
  LogJson.save_stats_and_restart p.id;
  resp
;;

let find_and_solve_problem
    ~(ctx : env)
    (psi_comps : (string * string * string) option)
    (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t)
    : int * (env * PsiDef.t * Syguslib.Sygus.solver_response segis_response) list
  =
  (*  Find problem components *)
  let target_fname, spec_fname, repr_fname =
    match psi_comps with
    | Some names -> names
    | None ->
      Utils.Log.debug_msg "Looking for the default names.";
      "target", "spec", "repr"
  in
  let top_userdef_problem =
    ProblemFinder.find_problem_components ~ctx (target_fname, spec_fname, repr_fname) pmrs
  in
  let find_multiple_solutions ctx top_userdef_problem mc =
    let num_attempts = ref 0 in
    let open Configuration in
    let rstate = G.generate_configurations ctx top_userdef_problem.PsiDef.target in
    let rec find_sols a =
      match G.next ~shuffle:true rstate with
      | Some sub_conf ->
        Int.incr num_attempts;
        let conf = Subconf.to_conf mc sub_conf in
        let new_target, new_ctx =
          apply_configuration ctx conf top_userdef_problem.target
        in
        Utils.Log.sep ~i:(Some !num_attempts) ();
        let new_pdef =
          { top_userdef_problem with target = new_target; id = !num_attempts }
        in
        (* Update stat: whether we have hit the original configuration. *)
        Stats.orig_solution_hit := same_conf new_pdef.target top_userdef_problem.target;
        (* Check unrealizability via cache first. *)
        if G.check_unrealizable_from_cache new_ctx new_pdef rstate
        then (
          Log.info
            Fmt.(fun fmt () -> pf fmt "Configuration is unrealizable according to cache.");
          G.mark_unrealizable rstate sub_conf;
          (* Update stats: number of cache hits. *)
          Int.incr Stats.num_unr_cache_hits;
          find_sols (a @ [ new_ctx, new_pdef, Unrealizable [] ]))
        else (
          (* Call the single configuration solver. *)
          match single_configuration_solver ~ctx:new_ctx new_pdef with
          | Realizable s ->
            G.mark_realizable rstate sub_conf;
            G.expand rstate sub_conf;
            find_sols ((new_ctx, new_pdef, Realizable s) :: a)
          | Unrealizable u ->
            G.mark_unrealizable rstate sub_conf;
            G.cache rstate u;
            find_sols ((new_ctx, new_pdef, Unrealizable u) :: a)
          | Failed f ->
            G.mark_failed rstate sub_conf;
            if !Config.node_failure_behavior then () else G.expand rstate sub_conf;
            find_sols ((new_ctx, new_pdef, Failed f) :: a))
      | None -> a
    in
    find_sols []
  in
  (* Check that the user want more than one solution, and that the problem defined
        is well-formed. Otherwise, just try to solve the user-defined configuration.
     *)
  if !Config.Optims.max_solutions >= 0
     && Configuration.check_pmrs top_userdef_problem.target
  then (
    (* Find multiple solutions to the problem. *)
    let max_configuration =
      Configuration.max_configuration ctx top_userdef_problem.target
    in
    Utils.Log.verbose (fun fmt () ->
        Fmt.pf fmt "Max configuration:@;%a" (Configuration.ppm ctx) max_configuration);
    let subconf_count =
      Map.fold
        ~init:1
        ~f:(fun ~key:_ ~data:l c -> c * (2 ** List.length l))
        max_configuration
    in
    total_configurations := subconf_count;
    Utils.Log.info (fun fmt () -> Fmt.pf fmt "%i configurations possible." subconf_count);
    subconf_count, find_multiple_solutions ctx top_userdef_problem max_configuration)
  else
    (* Only solve the top-level skeleton, i.e. the problem specified by the user. *)
    1, [ ctx, top_userdef_problem, single_configuration_solver ~ctx top_userdef_problem ]
;;
