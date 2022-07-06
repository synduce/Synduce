(* Expose part of the functionality of the executable in the Lib.  *)
module Parsers = Frontend.Parsers
module Utils = Utils
module Single = Se2gis.Main
module Many = Confsearch.Main
module Lang = Lang
open Base
open Common
open ProblemDefs
open Lang
open Codegen.Commons
open Env

let solve_file ?(print_info = false) (filename : string)
    : (env * problem_descr * (soln option, unrealizability_witness list) Either.t) list
    Lwt.t
  =
  Utils.Config.problem_name
    := Caml.Filename.basename (Caml.Filename.chop_extension filename);
  Utils.Config.info := print_info;
  Utils.Config.timings := false;
  let is_ocaml_syntax = Caml.Filename.check_suffix filename ".ml" in
  let prog, psi_comps =
    if is_ocaml_syntax then Parsers.parse_ocaml filename else Parsers.parse_pmrs filename
  in
  let pb_env = Env.group (Term.Context.create ()) (PMRS.Functions.create ()) in
  pb_env >- Parsers.seek_types prog;
  let all_pmrs = pb_env >>- Parsers.translate prog in
  let outputs = Single.find_and_solve_problem ~filename ~ctx:pb_env psi_comps all_pmrs in
  let final_step l =
    List.map l ~f:(fun (problem, result) ->
        let pd = pb_env >- problem_descr_of_psi_def problem in
        match result with
        | Realizable soln -> pb_env, pd, Either.First (Some soln)
        | Unrealizable (_, witnesses) -> pb_env, pd, Either.Second witnesses
        | Failed _ -> pb_env, pd, Either.First None)
  in
  Lwt.map final_step outputs
;;

let load_ctx_of_file (filename : string) : env =
  Utils.Config.problem_name
    := Caml.Filename.basename (Caml.Filename.chop_extension filename);
  Utils.Config.timings := false;
  let is_ocaml_syntax = Caml.Filename.check_suffix filename ".ml" in
  let prog, _ =
    if is_ocaml_syntax then Parsers.parse_ocaml filename else Parsers.parse_pmrs filename
  in
  let pb_env = Env.group (Term.Context.create ()) (PMRS.Functions.create ()) in
  pb_env >- Parsers.seek_types prog;
  let _ = pb_env >>- Parsers.translate prog in
  pb_env
;;

(**
  Call [get_lemma_hints ()] after [solve_file] to get a list of potential useful lemmas for
  the proof of correctness.
*)
let get_lemma_hints () =
  let eqns =
    match !Common.ProblemDefs.solved_eqn_system with
    | Some eqns -> eqns
    | None -> []
  in
  eqns
;;
