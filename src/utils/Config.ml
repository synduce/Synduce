open Base
open Getopt

let problem_name = ref "unknown"

(** Toggle debugging. *)
let debug = ref false

(** Print messages in compact form, non human readable.*)
let compact = ref false

(** Limit the size of debugging messages. *)
let debug_msg_max_chars = ref 400

(** Toggle info and error messages. Set to true by default. *)
let info = ref true

(** Json output toggle.  *)
let json_out = ref false

(**
  When Json output is on, set true to get partial updates in Json format.
  When the tool searches for a single solution, print intermediate refinement steps.
  When the tool searches for multiples ones, print intermediate results (unrealizable,
  failures and solutions).
*)
let json_progressive = ref false

let print_unrealizable_configs = ref false

(** Toggle printing timing info when info is off. Set to true by default. *)
let timings = ref true

(** Maximum steps of rewrites to apply during PMRS reduction (symbolic evaluation). *)
let reduction_limit = ref 100

(** After 50 refinement rounds, synthesized is probably using too many eager optimizations and
  the solution does not follow occam's razor.
 *)
let refinement_rounds_warning_limit = ref 20

(* Toggle to show a summary of variables used and their types. *)
let show_vars = ref false

(** Toggle verbose messages. *)
let verbose = ref false

let math_display = ref false

(** Optional output folder for solution.
    If None, the solution is only printed on the standard output.
    If Some path, the solution is written in path/inputfilename
*)
let output_folder : string option ref = ref None

let set_output_folder s = output_folder := Some s

let get_output_file s =
  Option.map !output_folder ~f:(fun o_f ->
      let base = Caml.Filename.basename s in
      Caml.Filename.concat o_f base)
;;

(** Optional output file for logging.
*)
let output_log : string option ref = ref None

let set_output_log s = output_log := Some s

(** Turn of all output except json. *)
let set_json_out () =
  verbose := false;
  info := false;
  debug := false;
  timings := false;
  json_out := true
;;

(** When printing a system of equations, put a limit on how many equations are printed. *)
let pp_eqn_count = ref 20

(* ============================================================================================= *)
(*                                TEMPORARY OPTIONS                                              *)
(* ============================================================================================= *)

(**
  Prompt user to input a precondition (lemma) for each equation, while the equations are being
  generated from a set of terms in Equations.make.
*)
let interactive_lemmas = ref false

let interactive_lemmas_loop = ref false

(** Prompt for lifting expressions. *)
let interactive_lifting = ref false

let classify_witness = ref false
let interactive_check_lemma = ref false

(**
  Check whether a system of equations defines a "functionally realizable" synthesis problem.
  ON by default.
*)
let check_unrealizable = ref true

(** Check whether a synthesis problem is unrealizable by checking whether the problem with
second-order quantification is unrealizable, using a SMT solver (Z3 by default).
*)
let check_unrealizable_smt_unsatisfiable = ref false

(**
  Accept no-sat in bounded checking as unsat.
*)
let no_bounded_sat_as_unsat = ref false

let bounded_lemma_check = ref false
let only_bounded_check = ref false

let set_bounded_only () =
  no_bounded_sat_as_unsat := true;
  bounded_lemma_check := true;
  only_bounded_check := true
;;

(**
  Run with the optimizations used to synthesize solutions for systems of equations.
  Works best when there are more than 4 cores.
*)
let sysfe_opt = ref true

(**
    Sometimes non-linearity occurs even when the reference only has linear operators.
*)
let force_nonlinear = ref false

(**
  When `node_failure_behaviour` is set to `true` (the default value), the failure to solve a
  given configuration is considered as an unrealizability result. That is, the configuration will
  not be refined further. If `node_failure_behaviour` is set to `false`, the failure will only
  interpreted as realizable: subconfigurations will be explored.
*)
let node_failure_behavior = ref false

(** Use DFS or BFS when searching for next configuration (true for BFS, false for DFS)*)
let next_algo_bfs = ref true

(* ============================================================================================= *)
(*                                STORAGE AND BINARY PATHS                                       *)
(* ============================================================================================= *)

let tmp_folder = Caml.Filename.get_temp_dir_name ()
let root_folder = Caml.Filename.current_dir_name
let base s = Caml.Filename.concat root_folder s

(* Set to true to force using cvc4 even if cvc5 is available. *)
(* There are still bugs with CVC5, leave true for now. *)
let use_cvc4 = ref true

(** You can use mkTuple and (Tuple ..) types with CVC4, but it doesn't
 seem to work with CVC5. *)
let using_cvc4_tuples = ref false

(** Don't generate a special grammar for constants if true. *)
let no_grammar_for_constants = ref true

let cvc4_binary_path =
  try Some (FileUtil.which "cvc4") with
  | _ -> None
;;

let cvc5_binary_path =
  try Some (FileUtil.which "cvc5") with
  | _ -> None
;;

let using_cvc5 () =
  (Option.is_some cvc5_binary_path && not !use_cvc4) || Option.is_none cvc4_binary_path
;;

let cvc_binary_path () =
  if !use_cvc4
  then (
    match cvc4_binary_path with
    | Some p -> p
    | None ->
      (match cvc5_binary_path with
      | Some p -> p
      | None -> failwith "CVC4 and CVC5 not found."))
  else (
    match cvc5_binary_path with
    | Some p -> p
    | None ->
      (match cvc4_binary_path with
      | Some p -> p
      | None -> failwith "CVC5 and CVC4 not found."))
;;

let z3_binary_path =
  try FileUtil.which "z3" with
  | _ -> failwith "Z3 not found (using 'which z3')."
;;

let yices_binary_path =
  try Some (FileUtil.which "yices-smt2") with
  | _ -> None
;;

(* TODO fix this. Not functional. *)
let dryadsynth_binary_path =
  try FileUtil.which "DryadSynth" with
  | _ -> ""
;;

let use_eusolver = ref false

let eusolver_binary_path =
  try FileUtil.which "eusolver" with
  | _ -> ""
;;

let verification_solver = ref "z3"

let set_verification_solver (s : string) =
  match s with
  | "z3" -> verification_solver := "z3"
  | "cvc4" -> verification_solver := "cvc4"
  | "cvc5" -> verification_solver := "cvc5"
  | "yices" -> verification_solver := "yices"
  | _ -> ()
;;

(* Smt solver logging. *)

let smt_solver_log_file = ref "/tmp/solver.smt2"
let smt_log_queries = ref true
let smt_solve_verbose = ref true

(* Generating realizable and unrealizable SyGuS benchmarks. *)
let generate_benchmarks = ref false
let benchmark_generation_dir = ref tmp_folder
let benchmark_lang_version = ref "1.1"

let set_benchmark_generation_dir (s : string) =
  generate_benchmarks := true;
  benchmark_generation_dir := s
;;

let new_benchmark_file ?(hint = "") suffix =
  Caml.Filename.temp_file
    ~temp_dir:!benchmark_generation_dir
    ("bench_" ^ hint ^ !problem_name)
    suffix
;;

let generate_proof = ref false
let proof_generation_file = ref ""

let set_proof_output_file (s : string) =
  generate_proof := true;
  proof_generation_file := s
;;

let included_files : string list ref = ref []

let set_included_files (s : string) =
  let files = String.split ~on:',' s in
  included_files := List.map ~f:String.strip files
;;

(** A set of options to search for multiple solutions. *)
let set_default_config_confsearch () =
  Optims.max_solutions := 2;
  Optims.attempt_lifting := false;
  set_bounded_only () (* Optims.num_expansions_check := 20 *)
;;

(* ============================================================================================= *)
(*                  SYSTEM OF EQUATIONS OPTIMIZATION FLAGS                                       *)
(* ============================================================================================= *)

module Optims = Optims
open Optims

(* ============================================================================================= *)
(*                                CLI OPTIONS                                                    *)
(* ============================================================================================= *)

let options print_usage parse_only =
  [ 'A', "all-solutions-config", Some set_default_config_confsearch, None
  ; 'b', "bmc", None, Some set_check_depth
  ; 'B', "bounded-lemma-check", set bounded_lemma_check true, None
  ; 'c', "simple-init", set simple_init true, None
  ; 'C', "check-smt-unrealizable", set check_unrealizable_smt_unsatisfiable true, None
  ; 'u', "no-check-unrealizable", set check_unrealizable false, None
  ; 'd', "debug", set debug true, None
  ; 'e', "expand-on-failure", set node_failure_behavior false, None
  ; 'f', "force-nonlinear", set force_nonlinear true, None
  ; 'h', "help", Some print_usage, None
  ; 'i', "info-off", set info false, None
  ; 'I', "include", None, Some set_included_files
  ; 'j', "json", Some set_json_out, None
  ; 'J', "interactive-lifting", set interactive_lifting true, None
  ; 'k', "post-bounding", set bound_after_verif true, None
  ; 'l', "lemma-sketch", set make_partial_lemma_sketches true, None
  ; 'L', "interactive-loop", set interactive_lemmas_loop true, None
  ; 'm', "style-math", set math_display true, None
  ; 'n', "verification", None, Some set_num_expansions_check
  ; 'N', "no-sat-as-unsat", set no_bounded_sat_as_unsat true, None
  ; 'o', "output", None, Some set_output_folder
  ; 'O', "log-output", None, Some set_output_log
  ; 'p', "num-threads", None, Some set_num_threads
  ; 'P', "reuse-predicates-off", set reuse_predicates false, None
  ; 's', "multi-max-solutions", None, Some set_max_solutions
  ; 't', "solve-timeout", None, Some set_wait_parallel_tlimit
  ; 'v', "verbose", set verbose true, None
  ; 'W', "verif-with", None, Some set_verification_solver
  ; 'X', "classify-witness", set classify_witness true, None
  ; 'Z', "multi-shuffle-configs", set shuffle_configurations true, None
  ; '\000', "segis", set use_segis true, None
  ; '\000', "se2gis", set use_se2gis true, None
  ; '\000', "cegis", set use_cegis true, None
  ; '\000', "cvc4", set use_cvc4 true, None
  ; '\000', "cvc5", set use_cvc4 false, None
  ; '\000', "compact", set compact true, None
  ; '\000', "const-grammars", set no_grammar_for_constants false, None
  ; '\000', "eusolver", set use_eusolver true, None
  ; '\000', "fuzzing", None, Some set_fuzzing_count
  ; '\000', "gropt-level", None, Some set_grammar_optimization_level
  ; '\000', "generate-benchmarks", None, Some set_benchmark_generation_dir
  ; '\000', "generate-proof", None, Some set_proof_output_file
  ; '\000', "interactive", set interactive_lemmas true, None
  ; '\000', "interactive-check-lemma", set interactive_check_lemma true, None
  ; '\000', "json-progress", set json_progressive true, None
  ; '\000', "max-lifting", None, Some set_max_lifting_attempts
  ; '\000', "multi-use-dfs", set next_algo_bfs false, None
  ; '\000', "multi-rstar-limit", None, Some set_rstar_limit
  ; '\000', "multi-no-rstar", set use_rstar_caching false, None
  ; '\000', "multi-no-rc", set use_root_causing false, None
  ; '\000', "multi-strategy", None, Some set_exploration_strategy
  ; '\000', "multi-reconly", set search_constant_variations false, None
  ; '\000', "no-assumptions", set make_partial_correctness_assumption false, None
  ; '\000', "no-detupling", set detupling_on false, None
  ; '\000', "no-gropt", set optimize_grammars 0, None
  ; '\000', "no-splitting", set split_solve_on false, None
  ; '\000', "no-rew", set use_rewrite_solver false, None
  ; '\000', "no-lifting", set attempt_lifting false, None
  ; '\000', "no-simplify", set simplify_eqns false, None
  ; '\000', "no-syndef", set use_syntactic_definitions false, None
  ; '\000', "parse-only", set parse_only true, None
  ; '\000', "show-vars", set show_vars true, None
  ; '\000', "sysfe-opt-off", set sysfe_opt false, None
  ; '\000', "use-bmc", set use_bmc true, None
  ; (* Background solver parameters *)
    '\000', "ind-tlimit", None, Some set_induction_proof_tlimit
  ]
;;
