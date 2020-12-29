let verbose = ref false

let debug = ref false

let info = ref true

let show_vars = ref false

let reduction_limit = ref 100

let num_expansions_check = 4

let tmp_folder = Caml.Filename.get_temp_dir_name ()

(* TODO automate this *)
let root_folder = Caml.Filename.current_dir_name

let base s = Filename.concat root_folder s

let cvc4_binary_path = "/usr/bin/cvc4"

let z3_binary_path = "/usr/bin/z3"


(* Optimization flags *)
let detupling_on = ref true

let stratify_on = ref true