let verbose = ref false

let debug = ref false
let reduction_limit = ref 100

let tmp_folder = ref "/tmp/"

(* TODO automate this *)
let root_folder = "/home/victorn/repos/refuns/"
let base s = Filename.concat root_folder s

let cvc4_binary_path = "/home/victorn/tools/cvc4-1.8-x86_64-linux-opt"