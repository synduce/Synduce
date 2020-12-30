open Base
let verbose = ref false

let debug = ref false

let info = ref true

let show_vars = ref false

let reduction_limit = ref 100

let num_expansions_check = ref 10

let set_num_expansions_check (s : string) =
  let i = Int.of_string s in
  if i > 5 && i < 1024 then num_expansions_check := i

let tmp_folder = Caml.Filename.get_temp_dir_name ()

(* TODO automate this *)
let root_folder = Caml.Filename.current_dir_name

let base s = Caml.Filename.concat root_folder s

let cvc4_binary_path = "/usr/bin/cvc4"

let z3_binary_path = "/usr/bin/z3"


(* Optimization flags *)
let detupling_on = ref true

let stratify_on = ref true

let replace_recursion = ref true