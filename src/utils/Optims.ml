open Base

let turn_off x = x := false
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

(**
  Rewrite sovler: find solutions by rewriting expressions and isolate possible
  solutions for the functions.
  Turn off with --no-rew
 *)
let use_rewrite_solver = ref true

(**
  Partial correctenss asumptions: don't throw away partial solutions
  Turn off with --no-assumptions
 *)
let make_partial_correctness_assumption = ref true

(**
  Turn on with --lemma-sketches.
 *)
let make_partial_lemma_sketches = ref false

(** Separate systems of equations into subsystems, and solve independently each subsystem.
  Use option -s or --split-solving-off to turn off.
*)
let split_solve_on = ref true

(** Initialize T using a single variable of type theta, instead of searching for a set of
    terms covering all unknowns.
    Use option --simple-init or -c to turn on.
*)
let simple_init = ref false

(** In some cases, synthesizing lemmas nca be too hard; the algorithm can revert to bounded
  mode after a failed round of solution synthesis.
*)
let bound_after_verif = ref false

(**
  Not an optimization. Set to true to use the base abstract cegis algorithm.
  Option --segis sets this flag to true.
  *)
let use_segis = ref false

(**
  Not an optimization. Set to true to use the base concrete cegis algorithm.
  Option --cegis sets this flag to true.
  *)
let use_cegis = ref false

(**
  Not an optimization. Set to true to use the SE2GIS algorithm *only*.
  Option --se2gis sets this flag to true.
  *)
let use_se2gis = ref false

(**
  Simplify equations before feeding them to equations solver.
  Turn off with --no-simplify
*)
let simplify_eqns = ref true

(**
  Use the equations as a indicator to optimize grammars, without compromising soundness.
*)
let optimize_grammars = ref 2

let set_grammar_optimization_level (s : string) : unit =
  try
    let i = Int.of_string s in
    if i >= 0 then optimize_grammars := max i 2
  with
  | _ -> ()
;;

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
let expand_cut = ref 240

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

(* ============================================================================================= *)
(*                        PARAMETERS FOR FINDING MULTIPLE SOLUTIONS                              *)
(* ============================================================================================= *)

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

(**
  The maximum number of different solutions Synduce attempts to find.
  If set to (-1), Synduce only attempt to solve the given skeleton.
*)
let max_solutions = ref (-1)

(** Set the maximum number of solutions. The string argument must represent
  a number between 1 and 32.
*)
let set_max_solutions (s : string) =
  try
    let i = Int.of_string s in
    if i > 0 && i <= 32 then max_solutions := i
  with
  | _ -> ()
;;

(** The number of expansions an algorithm goes through to identify an algorithm run.
*)
let rstar_limit = ref 3

(** Set the number of expansions the algorithm uses to identify an algorithm run.
*)
let set_rstar_limit (s : string) =
  try
    let i = Int.of_string s in
    if i > 0 && i <= 32 then rstar_limit := i
  with
  | _ -> ()
;;

let rstar_fuel = ref 300.

(** Use the deterministic algorithm to precompute sets of equations that might have unrealizable
    sets.
*)
let use_rstar_caching = ref true

(** Reuse predicates across different configurations. *)
let reuse_predicates = ref true

(** Shuffle configurations when choosing the next configuration to solve. *)
let shuffle_configurations = ref false

type exploration_strategy =
  | ESTopDown
  | ESBottomUp

let exploration_strategy = ref ESTopDown

let set_exploration_strategy (s : string) =
  match s with
  | "td" | "top-down" -> exploration_strategy := ESTopDown
  | "bu" | "bottom-up" -> exploration_strategy := ESBottomUp
  | _ ->
    failwith
      "Did not recognize exploration strategy, should be top-down (td) or bottom-up (bu)"
;;

(** Set to false if solution with only variying constants are not wanted. *)
let search_constant_variations = ref true

(** Set to [true] to use the root-causing algorithm during the search. *)
let use_root_causing = ref true

(* ============================================================================================= *)
(*                        OTHER                                                                  *)
(* ============================================================================================= *)

let some_eager_optim_on () =
  !simplify_eqns || !use_syntactic_definitions || !make_partial_correctness_assumption
;;

let turn_off_eager_optims () =
  turn_off simplify_eqns;
  turn_off use_syntactic_definitions;
  turn_off make_partial_correctness_assumption
;;

(* ============================================================================================= *)
(*                                                    MULTITHREADING                             *)
(* ============================================================================================= *)

let num_threads = ref 4

let set_num_threads (s : string) =
  try
    let i = Int.of_string s in
    if i > 0 && i <= 32 then num_threads := i
  with
  | _ -> ()
;;
