(* Expose part of the functionality of the executable in the Lib.  *)
module Parsers = Frontend.Parsers
module Utils = Utils
module Algos = Algo.Refinement
module Lang = Lang
open Algo
open AState
open Base
open Lang
open Codegen.Commons

(** Use [reinit] to reinitialize all the global variables used in Synduce when solving
  multiple problems.
*)
let reinit () =
  AState.reinit ();
  Term.Variable.clear ();
  Alpha.reinit ();
  RType.reinit ();
  PMRS.reinit ();
  Specifications.reinit ()
;;

let solve_file ?(print_info = false) (filename : string)
    : (problem_descr * (soln option, unrealizability_ctex list) Either.t) list
  =
  Utils.Config.problem_name
    := Caml.Filename.basename (Caml.Filename.chop_extension filename);
  Utils.Config.info := print_info;
  Utils.Config.timings := false;
  let is_ocaml_syntax = Caml.Filename.check_suffix filename ".ml" in
  let prog, psi_comps =
    if is_ocaml_syntax then Parsers.parse_ocaml filename else Parsers.parse_pmrs filename
  in
  Parsers.seek_types prog;
  let all_pmrs = Parsers.translate prog in
  let outputs = Refinement.solve_problem psi_comps all_pmrs in
  List.map outputs ~f:(fun (problem, result) ->
      let pd = problem_descr_of_psi_def problem in
      match result with
      | Realizable soln -> pd, Either.First (Some soln)
      | Unrealizable soln -> pd, Either.Second soln
      | Failed _ -> pd, Either.First None)
;;

(**
  Call [get_lemma_hints ()] after [solve_file] to get a list of potential useful lemmas for
  the proof of correctness.
*)
let get_lemma_hints () =
  let eqns =
    match !AState.solved_eqn_system with
    | Some eqns -> eqns
    | None -> []
  in
  eqns
;;
