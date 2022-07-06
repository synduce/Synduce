open Base
open Common
open Env
open ProblemDefs
open Lang
open RootCauseAnalysis
open Utils
module Sub = Configuration.Subconf
module G = ConfGraph
module CO = Config.Optims

type multi_soln_result =
  { r_best : (env * PsiDef.t * Syguslib.Sygus.solver_response segis_response) option
  ; r_all : (env * PsiDef.t * Syguslib.Sygus.solver_response segis_response) list
  ; r_subconf_count : int
  ; r_final_state : G.state
  }

let total_configurations = ref 0

let is_definite = function
  | Realizable _ -> true
  | Unrealizable _ -> true
  | Failed _ -> false
;;

let pw ctx = Lwt.map (fun x -> ctx, x)

let log_step ctx p resp (elapsed, verif) s =
  AlgoLog.log_solution ~ctx ~p resp;
  AlgoLog.show_stat_intermediate_solution
    ~ctx
    p
    resp
    elapsed
    verif
    !total_configurations
    (G.get_coverage_percentage s)
;;

(* ============================================================================================= *)
(*                            SINGLE NODE (CONFIGURATION) SOLVERS                                *)
(* ============================================================================================= *)

let portfolio_solver ~(ctx : env) (p : PsiDef.t) =
  let counter = ref 2 in
  Lwt_main.run
    (Lwt.pick
       [ Concurrency.pwait is_definite counter ctx (Se2gis.Main.solve_problem ~ctx p)
       ; (let ctx' = env_copy ctx in
          Concurrency.pwait
            is_definite
            counter
            ctx'
            (Se2gis.Baselines.algo_segis ~ctx:ctx' p))
       ])
;;

let single_configuration_solver ~(ctx : env) (p : PsiDef.t)
    : env * float * float * Syguslib.Sygus.solver_response segis_response
  =
  let ctx, resp =
    if !CO.use_segis (* Symbolic CEGIS. *)
    then Lwt_main.run (pw ctx (Se2gis.Baselines.algo_segis ~ctx p))
    else if !CO.use_cegis (* Concrete CEGIS. *)
    then Lwt_main.run (pw ctx (Se2gis.Baselines.algo_cegis ~ctx p))
    else if !CO.use_se2gis
    then
      Lwt_main.run (pw ctx (Se2gis.Main.solve_problem ~ctx p))
      (* Default algorithm: best combination of techniques (TODO) *)
    else portfolio_solver ~ctx p
  in
  let elapsed = Stats.get_glob_elapsed ()
  and verif = !Stats.verif_time in
  (* Save stats an restart counters. *)
  LogJson.save_stats_and_restart p.id;
  ctx, elapsed, verif, resp
;;

(* ============================================================================================= *)
(*                            MAIN CONFIGURATION GRAPH SOLVING ALGORITHM                         *)
(* ============================================================================================= *)

let find_multiple_solutions
    ~(ctx : env)
    ~score:(score_func : Env.env -> PsiDef.t -> soln -> Configuration.conf -> int)
    (top_userdef_problem : PsiDef.t)
    (sup : Configuration.conf)
    : multi_soln_result
  =
  let num_attempts = ref 0 in
  let best_score = ref 1000 in
  let best_solution = ref None in
  let open Configuration in
  let rstate =
    G.generate_configurations
      ~strategy:!CO.exploration_strategy
      ctx
      top_userdef_problem.PsiDef.target
  in
  let expand_func =
    match !CO.exploration_strategy with
    | ESTopDown -> G.expand_down
    | ESBottomUp -> G.expand_up
  in
  let update_coverage_func =
    match !CO.exploration_strategy with
    | ESTopDown -> G.update_coverage_down
    | ESBottomUp -> G.update_coverage_up
  in
  let orig_target = top_userdef_problem.target in
  let rec find_sols a =
    match
      (if !Config.next_algo_bfs then G.next else G.next_dfs)
        ~shuffle:!CO.shuffle_configurations
        rstate
    with
    | Some curr_conf ->
      Int.incr num_attempts;
      (* Apply sub-configuration to configuration and problem components. *)
      let conf = Subconf.to_conf ~sup curr_conf in
      let new_target, new_ctx =
        apply_configuration ~ctx conf top_userdef_problem.target
      in
      Log.sep ~i:(Some !num_attempts) ();
      let new_pdef =
        { top_userdef_problem with target = new_target; id = !num_attempts }
      in
      Log.info
        Fmt.(
          fun fmt () ->
            pf fmt "New target:@;@[%a@]" (ctx >- PMRS.pp_ocaml ~short:false) new_target);
      (* Check unrealizability via cache first. *)
      if !CO.use_rstar_caching && G.check_unrealizable_from_cache new_ctx new_pdef rstate
      then (
        Log.info
          Fmt.(fun fmt () -> pf fmt "Configuration is unrealizable according to cache.");
        G.mark_unrealizable rstate curr_conf;
        expand_func ~mark:G.Unrealizable rstate curr_conf;
        (* Update stats: number of cache hits. *)
        Int.incr Stats.num_unr_cache_hits;
        (* Log an unrealizable solution (logged by single_configuration_solver in other branches) *)
        AlgoLog.log_solution ~ctx ~p:new_pdef (Unrealizable (NoRepair, []));
        (* Update the coverage. *)
        update_coverage_func rstate curr_conf G.Unrealizable;
        find_sols (a @ [ new_ctx, new_pdef, Unrealizable (NoRepair, []) ]))
      else (
        (* Call the single configuration solver. *)
        match single_configuration_solver ~ctx:new_ctx new_pdef with
        | new_ctx', elapsed, verif, Realizable s ->
          G.mark_realizable rstate curr_conf;
          expand_func ~mark:G.Realizable rstate curr_conf;
          (* Update the coverage *)
          update_coverage_func rstate curr_conf G.Realizable;
          (* Update stat: whether we have hit the original configuration. *)
          if same_conf new_pdef.target orig_target then Stats.orig_solution_hit := true;
          (* Looking for the best solution according to some metric. *)
          let score = score_func ctx top_userdef_problem s conf in
          if score < !best_score
          then (
            best_score := score;
            best_solution := Some (new_ctx, new_pdef, Realizable s));
          log_step new_ctx' new_pdef (Realizable s) (elapsed, verif) rstate;
          find_sols ((new_ctx', new_pdef, Realizable s) :: a)
        | new_ctx', elapsed, verif, Unrealizable (r, u) ->
          G.mark_unrealizable rstate curr_conf;
          expand_func ~mark:G.Unrealizable rstate curr_conf;
          (* Cache the unrealizable configuration for R*. *)
          G.cache rstate u;
          (* Update the coverage. *)
          update_coverage_func rstate curr_conf G.Unrealizable;
          (* Analyze the witnesses and root cause to suggest next configuration
             to solve. *)
          if !Config.Optims.use_root_causing
          then analyze_witnesses ~ctx:new_ctx' ~g:new_target rstate curr_conf r u;
          (* Continue *)
          log_step new_ctx' new_pdef (Unrealizable (r, u)) (elapsed, verif) rstate;
          find_sols ((new_ctx', new_pdef, Unrealizable (r, u)) :: a)
        | new_ctx', elapsed, verif, Failed (s, f) ->
          G.mark_failed rstate curr_conf;
          expand_func rstate curr_conf;
          (* Update the coverage. *)
          update_coverage_func rstate curr_conf G.Failed;
          log_step new_ctx' new_pdef (Failed (s, f)) (elapsed, verif) rstate;
          find_sols ((new_ctx', new_pdef, Failed (s, f)) :: a))
    | None -> a
  in
  let all_solns = find_sols [] in
  { r_best = !best_solution
  ; r_all = all_solns
  ; r_subconf_count = -1
  ; r_final_state = rstate
  }
;;

let find_and_solve_problem
    ~(ctx : env)
    ~(filename : string)
    (psi_comps : (string * string * string) option)
    (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t)
    : multi_soln_result
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
    ProblemFinder.find_problem_components
      ~filename
      ~ctx
      (target_fname, spec_fname, repr_fname)
      pmrs
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
      Configuration.Subconf.(Lattice.count_subs (of_conf max_configuration))
    in
    total_configurations := subconf_count;
    Log.info (fun fmt () -> Fmt.pf fmt "%i configurations possible." subconf_count);
    AlgoLog.log_confsearch_problem ~ctx ~p:top_userdef_problem subconf_count;
    let multi_sols =
      (* TODO: multiple scoring functions. *)
      let score_func ctx _p _soln conf = Configuration.num_rec_calls ~ctx conf in
      find_multiple_solutions ~ctx ~score:score_func top_userdef_problem max_configuration
    in
    { multi_sols with r_subconf_count = subconf_count })
  else (
    (* Only solve the top-level skeleton, i.e. the problem specified by the user. *)
    let placeholder_state =
      G.generate_configurations
        ~strategy:!CO.exploration_strategy
        ctx
        top_userdef_problem.target
    in
    let ctx', _, _, top_soln = single_configuration_solver ~ctx top_userdef_problem in
    { r_best = Some (ctx', top_userdef_problem, top_soln)
    ; r_all = [ ctx', top_userdef_problem, top_soln ]
    ; r_subconf_count = 1
    ; r_final_state = placeholder_state
    })
;;
