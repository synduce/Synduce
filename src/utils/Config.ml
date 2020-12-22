let verbose = ref false

let debug = ref false
let reduction_limit = ref 100

let num_expansions_check = 4

let tmp_folder = ref "/tmp/"

(* TODO automate this *)
let root_folder = "/home/victorn/repos/refuns/"
let base s = Filename.concat root_folder s

let cvc4_binary_path = "/usr/bin/cvc4"

let z3_binary_path = "/usr/bin/z3"