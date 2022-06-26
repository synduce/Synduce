open Base
open Common
open Env
open ProblemDefs
open Lang
open Utils
module G = ConfGraph
module CO = Config.Optims

type multi_soln_result =
  { r_best : (env * PsiDef.t * Syguslib.Sygus.solver_response segis_response) option
  ; r_all : (env * PsiDef.t * Syguslib.Sygus.solver_response segis_response) list
  ; r_subconf_count : int
  }

let total_configurations = ref 0

let is_definite = function
  | Realizable _ -> true
  | Unrealizable _ -> true
  | Failed _ -> false
;;

let pw ctx = Lwt.map (fun x -> ctx, x)

let portfolio_solver ~(ctx : env) (p : PsiDef.t) =
  let counter = ref 2 in
  Lwt.pick
    [ Concurrency.pwait is_definite counter ctx (Se2gis.Main.solve_problem ~ctx p)
    ; (let ctx' = env_copy ctx in
       Concurrency.pwait
         is_definite
         counter
         ctx'
         (Se2gis.Baselines.algo_segis ~ctx:ctx' p))
    ]
;;

let single_configuration_solver ~(ctx : env) (p : PsiDef.t)
    : (env * Syguslib.Sygus.solver_response segis_response) Lwt.t
  =
  let%lwt ctx, resp =
    if !CO.use_segis (* Symbolic CEGIS. *)
    then pw ctx (Se2gis.Baselines.algo_segis ~ctx p)
    else if !CO.use_cegis (* Concrete CEGIS. *)
    then pw ctx (Se2gis.Baselines.algo_cegis ~ctx p)
    else if !CO.use_se2gis
    then
      pw ctx (Se2gis.Main.solve_problem ~ctx p)
      (* Default algorithm: best combination of techniques (TODO) *)
    else portfolio_solver ~ctx p
  in
  (* Print intermediate result if we are looking for more than one solution *)
  if !CO.max_solutions > 0
  then (
    let elapsed = Stats.get_glob_elapsed ()
    and verif = !Stats.verif_time in
    AlgoLog.log_solution ~ctx ~p resp;
    AlgoLog.show_stat_intermediate_solution
      ~ctx
      p
      resp
      elapsed
      verif
      !total_configurations);
  (* Save stats an restart counters. *)
  LogJson.save_stats_and_restart p.id;
  Lwt.return (ctx, resp)
;;

let find_multiple_solutions
    ~(ctx : env)
    ~score:(score_func : Env.env -> PsiDef.t -> soln -> Configuration.conf -> int)
    (top_userdef_problem : PsiDef.t)
    (sup : Configuration.conf)
    : multi_soln_result Lwt.t
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
  let rec find_sols a =
    match
      (if !Config.next_algo_bfs then G.next else G.next_dfs)
        ~shuffle:!CO.shuffle_configurations
        rstate
    with
    | Some sub_conf ->
      Int.incr num_attempts;
      (* Apply sub-configuration to configuration and problem components. *)
      let conf = Subconf.to_conf ~sup sub_conf in
      let new_target, new_ctx =
        apply_configuration ~ctx conf top_userdef_problem.target
      in
      Log.sep ~i:(Some !num_attempts) ();
      let new_pdef =
        { top_userdef_problem with target = new_target; id = !num_attempts }
      in
      Log.verbose
        Fmt.(
          fun fmt () ->
            pf fmt "New target:@[%a@]" (ctx >- PMRS.pp ~short:false) new_target);
      (* Update stat: whether we have hit the original configuration. *)
      Stats.orig_solution_hit
        := !Stats.orig_solution_hit
           || same_conf new_pdef.target top_userdef_problem.target;
      (* Check unrealizability via cache first. *)
      if !CO.use_rstar_caching && G.check_unrealizable_from_cache new_ctx new_pdef rstate
      then (
        Log.info
          Fmt.(fun fmt () -> pf fmt "Configuration is unrealizable according to cache.");
        G.mark_unrealizable rstate sub_conf;
        expand_func ~mark:G.Unrealizable rstate sub_conf;
        (* Update stats: number of cache hits. *)
        Int.incr Stats.num_unr_cache_hits;
        find_sols (a @ [ new_ctx, new_pdef, Unrealizable [] ]))
      else (
        (* Call the single configuration solver. *)
        match%lwt single_configuration_solver ~ctx:new_ctx new_pdef with
        | new_ctx', Realizable s ->
          G.mark_realizable rstate sub_conf;
          expand_func ~mark:G.Realizable rstate sub_conf;
          (* Looking for the best solution according to some metric. *)
          let score = score_func ctx top_userdef_problem s conf in
          if score < !best_score
          then (
            best_score := score;
            best_solution := Some (new_ctx, new_pdef, Realizable s));
          find_sols ((new_ctx', new_pdef, Realizable s) :: a)
        | new_ctx', Unrealizable u ->
          G.mark_unrealizable rstate sub_conf;
          expand_func ~mark:G.Unrealizable rstate sub_conf;
          G.cache rstate u;
          find_sols ((new_ctx', new_pdef, Unrealizable u) :: a)
        | new_ctx', Failed (s, f) ->
          G.mark_failed rstate sub_conf;
          expand_func rstate sub_conf;
          find_sols ((new_ctx', new_pdef, Failed (s, f)) :: a))
    | None -> Lwt.return a
  in
  let%lwt all_solns = find_sols [] in
  Lwt.return { r_best = !best_solution; r_all = all_solns; r_subconf_count = -1 }
;;

let find_and_solve_problem
    ~(ctx : env)
    (psi_comps : (string * string * string) option)
    (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t)
    : multi_soln_result Lwt.t
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
    Log.info (fun fmt () -> Fmt.pf fmt "%i configurations possible." subconf_count);
    AlgoLog.log_confsearch_problem ~ctx ~p:top_userdef_problem subconf_count;
    let%lwt multi_sols =
      (* TODO: multiple scoring functions. *)
      let score_func ctx _p _soln conf = Configuration.num_rec_calls ~ctx conf in
      find_multiple_solutions ~ctx ~score:score_func top_userdef_problem max_configuration
    in
    Lwt.return { multi_sols with r_subconf_count = subconf_count })
  else (
    (* Only solve the top-level skeleton, i.e. the problem specified by the user. *)
    let%lwt ctx', top_soln = single_configuration_solver ~ctx top_userdef_problem in
    Lwt.return
      { r_best = Some (ctx', top_userdef_problem, top_soln)
      ; r_all = [ ctx', top_userdef_problem, top_soln ]
      ; r_subconf_count = 1
      })
;;
