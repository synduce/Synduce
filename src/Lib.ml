module Parsers = Parsers
module Utils = Utils
module Algos = Algo.PmrsAlgos

(* Expose part of the functionality of the executable in the Lib.  *)
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

let solve_file ?(print_info = false) (filename : string) : problem_descr * soln option =
  Utils.Config.problem_name := Caml.Filename.basename (Caml.Filename.chop_extension filename);
  Utils.Config.info := print_info;
  Utils.Config.timings := false;
  let is_ocaml_syntax = Caml.Filename.check_suffix filename ".ml" in
  let prog, psi_comps =
    if is_ocaml_syntax then Parsers.parse_ocaml filename else Parsers.parse_pmrs filename
  in
  Parsers.seek_types prog;
  let all_pmrs = Parsers.translate prog in
  let problem, maybe_soln =
    match PmrsAlgos.solve_problem psi_comps all_pmrs with
    | problem, Ok soln -> (problem, Some soln)
    | problem, Error _ -> (problem, None)
  in
  ( {
      pd_target = PMRS.func_of_pmrs problem.psi_target;
      pd_reference = PMRS.func_of_pmrs problem.psi_reference;
      pd_repr = PMRS.func_of_pmrs problem.psi_repr;
    },
    maybe_soln )

(**
  Call [get_lemma_hints ()] after [solve_file] to get a list of potential useful lemmas for
  the proof of correctness.
*)
let get_lemma_hints () =
  let eqns = match !AState.solved_eqn_system with Some eqns -> eqns | None -> [] in
  eqns
