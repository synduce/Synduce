open Base
let verbose = ref false

let debug = ref false

let debug_msg_max_chars = ref 400

let info = ref true


let glob_start = ref 0.0

let show_vars = ref false

let reduction_limit = ref 100

let expand_depth = ref 2

let check_depth = ref 3

let set_check_depth (s : string) =
  let i = Int.of_string s in
  if i > 0 && i < 1024 then check_depth := i


let num_expansions_check = ref 15

let set_num_expansions_check (s : string) =
  let i = Int.of_string s in
  if i > 0 && i < 1024 then num_expansions_check := i


let expand_cut = ref 124

let tmp_folder = Caml.Filename.get_temp_dir_name ()

(* TODO automate this *)
let root_folder = Caml.Filename.current_dir_name

let base s = Caml.Filename.concat root_folder s

let cvc4_binary_path = "/usr/bin/cvc4"

let z3_binary_path = "/usr/bin/z3"


(* TODO fix this. *)
let dryadsynth_binary_path = "/home/victorn/tools/DryadSynth/exec.sh"
let eusolver_binary_path = "/home/victorn/tools/DryadSynth/exec.sh"


(* Optimization flags *)
let detupling_on = ref true

let stratify_on = ref true

let split_solve_on = ref true

let replace_recursion = ref true

let use_naive = ref false