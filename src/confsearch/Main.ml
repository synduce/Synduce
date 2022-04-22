open Base
open Common
open Env
open ProblemDefs
open Lang
open Utils

let find_and_solve_problem
    ~(ctx : env)
    (psi_comps : (string * string * string) option)
    (pmrs : (string, PMRS.t, Base.String.comparator_witness) Map.t)
    : (env * PsiDef.t * Syguslib.Sygus.solver_response segis_response) list
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
  let single_configuration_solver =
    if !Config.Optims.use_segis (* Symbolic CEGIS. *)
    then Se2gis.Baselines.algo_segis
    else if !Config.Optims.use_cegis (* Concrete CEGIS. *)
    then
      Se2gis.Baselines.algo_cegis
      (* Default algorithm: best combination of techniques (TODO) *)
    else Se2gis.Main.solve_problem
  in
  let find_multiple_solutions ctx top_userdef_problem mc =
    let open Configuration in
    let rstate =
      ConfGraph.generate_configurations ctx top_userdef_problem.PsiDef.target
    in
    let rec find_sols a =
      match ConfGraph.next rstate with
      | Some sub_conf ->
        let conf = Subconf.to_conf mc sub_conf in
        let new_target, new_ctx =
          apply_configuration ctx conf top_userdef_problem.target
        in
        let new_pdef = { top_userdef_problem with target = new_target } in
        (match single_configuration_solver ~ctx:new_ctx new_pdef with
        | Realizable s ->
          ConfGraph.mark_realizable rstate sub_conf;
          ConfGraph.expand rstate sub_conf;
          find_sols ((new_ctx, new_pdef, Realizable s) :: a)
        | Unrealizable u ->
          ConfGraph.mark_unrealizable rstate sub_conf;
          find_sols (a @ [ new_ctx, new_pdef, Unrealizable u ])
        | Failed _ ->
          ConfGraph.mark_unrealizable rstate sub_conf;
          find_sols a)
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
    Utils.Log.info (fun fmt () -> Fmt.pf fmt "%i configurations possible." subconf_count);
    find_multiple_solutions ctx top_userdef_problem max_configuration)
  else [ ctx, top_userdef_problem, single_configuration_solver ~ctx top_userdef_problem ]
;;
