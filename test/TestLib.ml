open Fmt

let sum, soln = Lib.solve_file "benchmarks/tree/sum.ml"

let eqns = Lib.get_lemma_hints ()
(*
  Don't execute both for now, solve_file should be only called once per executable.
  We'll solve this limitation later.
*)
(* let search, _ = Lib.solve_file "benchmarks/list/search.ml" *)

;;
Lib.pp_problem_descr stdout sum;
match soln with Some s -> Algo.AState.pp_soln ~use_ocaml_syntax:true stdout s | None -> ()

;;
List.iter (fun eqn -> pf stdout "%a@." Algo.AState.pp_equation eqn) eqns
