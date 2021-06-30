open Fmt

let sum, _ = Lib.solve_file "benchmarks/tree/sum.ml"

(*
  Don't execute both for now, solve_file should be only caleld once per executable.
  We'll solve this limitation later.
*)
(* let search, _ = Lib.solve_file "benchmarks/list/search.ml" *)

;;
Lib.pp_problem_descr stdout sum
