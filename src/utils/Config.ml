open Base

(** Toggle debugging. *)
let debug = ref false

(* Limit the size of debugging messages. *)
let debug_msg_max_chars = ref 400

(* Toggle info messages. Set to true by default. *)
let info = ref true

(* Toggle printing timing info when info is off. Set to true by default. *)
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
  OFF by default.
*)
let check_unrealizable = ref true

(**
  Attempt to lift the function if there is no solution.
*)
let attempt_lifting = ref true

(**
  Accept no-sat in bounded checking as unsat.
*)
let no_bounded_sat_as_unsat = ref false

(* ============================================================================================= *)
(*                                GLOBAL TIMING INFO                                             *)
(* ============================================================================================= *)

(* Start time. *)
let glob_start = ref 0.0

(** Total elapsed time spent in verification of solutions.  *)
let verif_time = ref 0.0

(* ============================================================================================= *)
(*                                STORAGE AND BINARY PATHS                                       *)
(* ============================================================================================= *)

let tmp_folder = Caml.Filename.get_temp_dir_name ()

let root_folder = Caml.Filename.current_dir_name

let base s = Caml.Filename.concat root_folder s

let cvc4_binary_path = try FileUtil.which "cvc4" with _ -> failwith "CVC4 not found."

let z3_binary_path = try FileUtil.which "z3" with _ -> failwith "Z3 not found."

(* TODO fix this. Not functional. *)
let dryadsynth_binary_path = try FileUtil.which "DryadSynth" with _ -> ""

let eusolver_binary_path = try FileUtil.which "eusolver" with _ -> ""

(* Smt solver logging. *)

let smt_solver_log_file = ref "/tmp/solver.smt2"

let smt_log_queries = ref true

let smt_solve_verbose = ref true

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
let optimize_grammars = ref false

(** When printing a system of equations, put a limit on how many equations are printed. *)
let pp_eqn_count = ref 10

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

(** Use bounded model checking. From CLI, use --use-bmc to set to true. *)
let use_bmc = ref false

(** Depth of bounded model checking. *)
let check_depth = ref 5

(** Bounded model checking depth is set by the -b or --bmc option of the CLI. *)
let set_check_depth (s : string) =
  let i = Int.of_string s in
  if i > 0 && i < 1024 then check_depth := i

(** A time limit for induction proofs.
  Infinity if set to negative.
*)
let induction_proof_tlimit = ref (-1)

let set_induction_proof_tlimit (s : string) =
  let i = Int.of_string s in
  induction_proof_tlimit := i

(** A limit for the number of rewriting steps applied during deduction.
*)
let rewrite_limit = ref 100

let set_rewrite_limit (s : string) =
  let i = Int.of_string s in
  rewrite_limit := i
