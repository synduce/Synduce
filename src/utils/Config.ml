open Base

(** Toggle verbose messages. *)
let verbose = ref false

(** Toggle debugging. *)
let debug = ref false

(* Limit the size of debugging messages. *)
let debug_msg_max_chars = ref 400

(* Toggle info messages. Set to true by default. *)
let info = ref true

(* Start time. *)
let glob_start = ref 0.0

(* Toggle to show a summary of variables used and their types. *)
let show_vars = ref false

(** Maximum steps of rewrites to apply during PMRS reduction. *)
let reduction_limit = ref 100


(* ============================================================================================= *)
(*                                STORAGE AND BINARY PATHS                                       *)
(* ============================================================================================= *)

let tmp_folder = Caml.Filename.get_temp_dir_name ()

let root_folder = Caml.Filename.current_dir_name

let base s = Caml.Filename.concat root_folder s

let cvc4_binary_path = FileUtil.which "cvc4"

let z3_binary_path = FileUtil.which "z3"


(* TODO fix this. *)
let dryadsynth_binary_path = "/home/victorn/tools/DryadSynth/exec.sh"
let eusolver_binary_path = "/home/victorn/tools/DryadSynth/exec.sh"


(* ============================================================================================= *)
(*                  SYSTEM OF EQUATIONS OPTIMIZATION FLAGS                                       *)
(* ============================================================================================= *)

(** Unkmowns of type i -> a * b * c .. are split into i -> a, i -> b  and
    equations between tuples are separated into different equations.
    Turn off using option -t or --detupling-off
*)
let detupling_on = ref true

(** Stratified solving: first solve for constant definitions, and syntactic definitions,
  and then solve for the rest of the equations.
  Turn off using option --stratifying-off
 *)
let stratify_on = ref true

(** Separate systems of equations into subsystems, and solve independently each subsystem.
  Use option -s or --split-solving-off to turn off.
*)
let split_solve_on = ref true

(**
  When reducing terms, nested calls to the recursion skeleton are replaced by
  calls to the reference function composed with the representation function.
*)
let replace_recursion = ref true

(**
  Not an optimization. Set to true to use the naive base algorithm.
  Option --use-naive sets this flag to true.
  *)
let use_naive = ref false

(* ============================================================================================= *)
(*                        BOUNDED EXPANSIONS / VERIFICATION PARAMETERS                           *)
(* ============================================================================================= *)

(** Maximum depth of pointwise expansions to perform. Careful setting this variable to high,
  it will lead to explosion in the number of terms expanded. A lazy expansion should be
  implemented. *)
let expand_depth = ref 2

(* Maximum of expansion depth performed during bounded checking. *)
let num_expansions_check = ref 15

(* Cut expansion after `expand_cut` terms generated. *)
let expand_cut = ref 124

(** num_expansions_check is set by the -n or --verification option of the CLI. *)
let set_num_expansions_check (s : string) =
  let i = Int.of_string s in
  if i > 0 && i < 1024 then num_expansions_check := i


(** Use bounded model checking. From CLI, use --use-bmc to set to true. *)
let use_bmc = ref false

(** Depth of bounded model checking. *)
let check_depth = ref 3

(** Bounded model checking depth is set by the -b or --bmc option of the CLI. *)
let set_check_depth (s : string) =
  let i = Int.of_string s in
  if i > 0 && i < 1024 then check_depth := i


