open Fmt
open Lang
open Term
open Base

(*
  Don't execute both for now, solve_file should be only called once per executable.
  We'll solve this limitation later.
*)
(* let search, _ = Lib.solve_file "benchmarks/list/search.ml" *)

let soln_descr filename =
  let sum, soln = Lib.solve_file filename in
  let eqns = Lib.get_lemma_hints () in
  Codegen.Commons.pp_problem_descr Fmt.stdout sum;
  (match soln with
  | Some s -> Algo.Common.ProblemDefs.pp_soln ~use_ocaml_syntax:true Fmt.stdout s
  | None -> ());
  List.iter
    ~f:(fun eqn -> pf Fmt.stdout "%a@." Algo.Common.ProblemDefs.pp_equation eqn)
    eqns;
  Fmt.(pf stdout "=== EXPAND ===@.");
  let v =
    mk_var (Variable.mk ~t:(Some !Algo.Common.ProblemDefs._theta) (Alpha.fresh ()))
  in
  Fmt.(pf stdout "%a@." pp_term v);
  let expansions = Analysis.expand_once v in
  List.iter ~f:(fun t -> Fmt.(pf stdout "@[%a -> %a@]@." pp_term v pp_term t)) expansions;
  let spec = mk_var (List.nth_exn sum.pd_reference 0).f_var in
  let reductions =
    List.map
      ~f:(fun t -> mk_app spec [ t ], Reduce.calc_term (mk_app spec [ t ]))
      expansions
  in
  List.iter
    ~f:(fun (t, out) ->
      Fmt.(pf stdout "@[%a = %a@]@." pp_term t (list ~sep:comma (braces pp_term)) out))
    reductions;
  let l1 = mk_var (Variable.mk "l1") in
  Fmt.(pf stdout "=== MATCH CASES ===@.");
  let match_case =
    (* Reduction function: Reduce.reduce_term (mk_app f [t]) *)
    mk_match
      v
      (List.map
         ~f:(fun t ->
           ( pattern_of_term t
           , let free_v = VarSet.elements (Analysis.free_variables t) in
             mk_app
               l1
               (List.filter
                  ~f:(fun t -> not (Analysis.is_norec t))
                  (List.map ~f:mk_var free_v)) ))
         expansions)
  in
  Fmt.(pf stdout "%a@." pp_term match_case);
  Lib.reinit ()
;;

List.iter ~f:soln_descr [ "benchmarks/tree/sum.ml"; "benchmarks/list/sum.ml" ]
