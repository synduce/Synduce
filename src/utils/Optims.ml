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
  Option --use-segis sets this flag to true.
  *)
let use_segis = ref false

(**
  Not an optimization. Set to true to use the base concrete cegis algorithm.
  Option --use-cegis sets this flag to true.
  *)
let use_cegis = ref false

(**
  Simplify equations before feeding them to equations solver.
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
