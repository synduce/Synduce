open Base
open Getopt

let problem_name = ref "unknown"

(** Toggle debugging. *)
let debug = ref false

(** Limit the size of debugging messages. *)
let debug_msg_max_chars = ref 400

(** Toggle info and error messages. Set to true by default. *)
let info = ref true

(** Toggle printing timing info when info is off. Set to true by default. *)
let timings = ref true

(** Maximum steps of rewrites to apply during PMRS reduction (symbolic evaluation). *)
let reduction_limit = ref 100

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

(* ============================================================================================= *)
(*                                TEMPORARY OPTIONS                                              *)
(* ============================================================================================= *)

(**
  Prompt user to input a precondition (lemma) for each equation, while the equations are being generated from a set of terms in Equations.make.
*)
let interactive_lemmas = ref false

let interactive_lemmas_loop = ref false

(** Prompt for lifting expressions. *)
let interactive_lifting = ref false

let classify_ctex = ref false
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

(**
  Run with the optimizations used to synthesize solutions for systems of equations.
  Works best when there are more than 4 cores.
*)
let sysfe_opt = ref true

(**
    Sometimes non-linearity occurs even when the reference only has linear operators.
*)
let force_nonlinear = ref false

(* ============================================================================================= *)
(*                                STORAGE AND BINARY PATHS                                       *)
(* ============================================================================================= *)

let tmp_folder = Caml.Filename.get_temp_dir_name ()
let root_folder = Caml.Filename.current_dir_name
let base s = Caml.Filename.concat root_folder s

(* Set to true to force using cvc4 even if cvc5 is available. *)
(* There are still bugs with CVC5, leave true for now. *)
let use_cvc4 = ref true

let cvc4_binary_path =
  try Some (FileUtil.which "cvc4") with
  | _ -> None
;;

let cvc5_binary_path =
  try Some (FileUtil.which "cvc5") with
  | _ -> None
;;

let using_cvc5 () = Option.is_some cvc5_binary_path && not !use_cvc4

let cvc_binary_path () =
  if !use_cvc4
  then (
    match cvc4_binary_path with
    | Some p -> p
    | None -> failwith "CVC4 not found using 'which cvc4').")
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

(* ============================================================================================= *)
(*                  SYSTEM OF EQUATIONS OPTIMIZATION FLAGS                                       *)
(* ============================================================================================= *)

(** Unkmowns of type i -> a * b * c .. are split into i -> a, i -> b  and
    equations between tuples are separated into different equations.
    Turn off using option -t or --detupling-off
*)
let detupling_on = ref true

(**
  Syntactic definitions: detect syntactic definitions in the equation systems and
  use them.
  Turn off with --no-syndef
 *)
let use_syntactic_definitions = ref true

(** Separate systems of equations into subsystems, and solve independently each subsystem.
  Use option -s or --split-solving-off to turn off.
*)
let split_solve_on = ref true

(** Initialize T using a single variable of type theta, instead of searching for a set of
    terms covering all unknowns.
    Use option --simple-init or -c to turn on.
*)
let simple_init = ref false

(**
  Not an optimization. Set to true to use the base abstract cegis algorithm.
  Option --use-acegis sets this flag to true.
  *)
let use_acegis = ref false

(**
  Not an optimization. Set to true to use the base concrete cegis algorithm.
  Option --use-ccegis sets this flag to true.
  *)
let use_ccegis = ref false

(**
  Simplify equations before feeding them to equations solver.
*)
let simplify_eqns = ref true

(**
  Use the equations as a indicator to optimize grammars, without compromising soundness.
  OFF for CAV
*)
let optimize_grammars = ref true

(** When printing a system of equations, put a limit on how many equations are printed. *)
let pp_eqn_count = ref 20

(* ============================================================================================= *)
(*                  BOUNDED EXPANSIONS / VERIFICATION / REWIRTING PARAMETERS                     *)
(* ============================================================================================= *)

(** Maximum depth of pointwise expansions to perform. Careful setting this variable to high,
  it will lead to explosion in the number of terms expanded. A lazy expansion should be
  implemented. *)
let expand_depth = ref 2

(* Maximum of expansion depth performed during bounded checking. *)
let num_expansions_check = ref 124

(* Cut expansion after `expand_cut` terms generated. *)
let expand_cut = ref 124

(** num_expansions_check is set by the -n or --verification option of the CLI. *)
let set_num_expansions_check (s : string) =
  let i = Int.of_string s in
  if i > 0 && i < 1024 then num_expansions_check := i
;;

(** Use bounded model checking. From CLI, use --use-bmc to set to true. *)
let use_bmc = ref false

(** Depth of bounded model checking. *)
let check_depth = ref 5

(** Bounded model checking depth is set by the -b or --bmc option of the CLI. *)
let set_check_depth (s : string) =
  let i = Int.of_string s in
  if i > 0 && i < 1024 then check_depth := i
;;

(** A time limit for induction proofs.
  Infinity if set to negative.
*)
let induction_proof_tlimit = ref (-1)

let set_induction_proof_tlimit (s : string) =
  let i = Int.of_string s in
  induction_proof_tlimit := i
;;

(** A time limit parallel calls when waiting on first result..
  Infinity if set to negative.
  Default is 10 mins.
*)
let wait_parallel_tlimit = ref 600.

let set_wait_parallel_tlimit (s : string) =
  let i = Float.of_string s in
  wait_parallel_tlimit := i
;;

(** A limit for the number of rewriting steps applied during deduction.
*)
let rewrite_limit = ref 100

let set_rewrite_limit (s : string) =
  let i = Int.of_string s in
  rewrite_limit := i
;;

(** When a model has been found, attempt fuzzing to find models that satisfy the same constraints.
  Used in Counterexamples.ml.
*)
let fuzzing_count = ref 0

let set_fuzzing_count (s : string) =
  try
    let i = Int.of_string s in
    if i >= 0 && i < 1024 then fuzzing_count := i
  with
  | _ -> ()
;;

(**
  Attempt to lift the function if there is no solution.
*)
let attempt_lifting = ref true

(** The number of times Synduce should attempt to add a lifting variable. *)
let max_lifting_attempts = ref 2

let set_max_lifting_attempts (s : string) =
  try
    let i = Int.of_string s in
    if i >= 0 && i < 64 then max_lifting_attempts := i
  with
  | _ -> ()
;;

(* ============================================================================================= *)
(*                                CLI OPTIONS                                                    *)
(* ============================================================================================= *)

let options print_usage parse_only =
  [ 'b', "bmc", None, Some set_check_depth
  ; 'c', "simple-init", set simple_init true, None
  ; 'u', "no-check-unrealizable", set check_unrealizable false, None
  ; 'd', "debug", set debug true, None
  ; 'f', "force-nonlinear", set force_nonlinear true, None
  ; 'h', "help", Some print_usage, None
  ; 'i', "info-off", set info false, None
  ; 'I', "interactive", set interactive_lemmas true, None
  ; 'J', "interactive-lifting", set interactive_lifting true, None
  ; 'L', "interactive-loop", set interactive_lemmas_loop true, None
  ; 'm', "style-math", set math_display true, None
  ; 'n', "verification", None, Some set_num_expansions_check
  ; 'N', "no-sat-as-unsat", set no_bounded_sat_as_unsat true, None
  ; 'B', "bounded-lemma-check", set bounded_lemma_check true, None
  ; 'o', "output", None, Some set_output_folder
  ; 's', "no-splitting", set split_solve_on false, None
  ; 't', "no-detupling", set detupling_on false, None
  ; 'v', "verbose", set verbose true, None
  ; 'X', "classify-ctex", set classify_ctex true, None
  ; 'C', "interactive-check-lemma", set interactive_check_lemma true, None
  ; '\000', "acegis", set use_acegis true, None
  ; '\000', "ccegis", set use_ccegis true, None
  ; '\000', "cvc4", set use_cvc4 true, None
  ; '\000', "cvc5", set use_cvc4 false, None
  ; '\000', "check-smt-unrealizable", set check_unrealizable_smt_unsatisfiable true, None
  ; '\000', "fuzzing", None, Some set_fuzzing_count
  ; '\000', "generate-benchmarks", None, Some set_benchmark_generation_dir
  ; '\000', "generate-proof", None, Some set_proof_output_file
  ; '\000', "parse-only", set parse_only true, None
  ; '\000', "max-lifting", None, Some set_max_lifting_attempts
  ; '\000', "no-gropt", set optimize_grammars false, None
  ; '\000', "no-lifting", set attempt_lifting false, None
  ; '\000', "no-simplify", set simplify_eqns false, None
  ; '\000', "no-syndef", set use_syntactic_definitions false, None
  ; '\000', "show-vars", set show_vars true, None
  ; '\000', "sysfe-opt-off", set sysfe_opt false, None
  ; '\000', "use-bmc", set use_bmc true, None
  ; '\000', "verif-with", None, Some set_verification_solver
  ; (* Background solver parameters *)
    '\000', "ind-tlimit", None, Some set_induction_proof_tlimit
  ]
;;
