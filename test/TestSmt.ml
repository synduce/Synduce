open Smtlib
open SmtLib
open Sexplib
open Lang.SmtInterface
open Domainslib
module S = SyncSmt

let x = mk_var "x"
let y = mk_var "y"
let z = mk_var "z"

let dirty_smt (s : string) =
  match smtTerm_of_sexp (Sexp.of_string s) with
  | Some t -> t
  | None -> mk_true
;;

let test_parallel_solvers () =
  let f i z3 () =
    S.load_min_max_defs z3;
    match S.exec_command z3 mk_exit with
    | Success ->
      Fmt.(pf stdout "PARALLEL TEST: Solver %i exited succesfully.@." i);
      true
    | _ -> failwith (Fmt.str "PARALLEL TEST: Solver %i did not exit successfully" i)
  in
  let pool = Task.setup_pool ~num_additional_domains:4 () in
  Task.run pool (fun () ->
      let d1 = Task.async pool (f 1 (S.make_z3_solver ())) in
      let d2 = Task.async pool (f 2 (S.make_cvc_solver ())) in
      Task.await pool d1 && Task.await pool d2)
;;

let test_dirty_cancel () =
  let f i z3 () =
    S.load_min_max_defs z3;
    let commands =
      [ mk_set_logic Logics.ALL
      ; mk_const_decl "x" mk_int_sort
      ; mk_const_decl "y" mk_int_sort
      ; mk_const_decl "z" mk_int_sort
      ; mk_assert (dirty_smt "(= (+ x y) (- 2))")
      ; mk_assert (dirty_smt "(= (+ x z) 0)")
      ; mk_assert (dirty_smt "(= (* x z) (- 1))")
      ; mk_assert (dirty_smt "(= (* x y) 1)")
      ]
    in
    S.exec_all z3 commands;
    let j = ref 0 in
    while !j < 100 do
      Unix.sleepf 0.02;
      incr j;
      S.smt_assert z3 (dirty_smt "(= (+ x y z) (- z))")
    done;
    Fmt.(pf stdout "Done emitting commands.@.");
    (* Check sat. *)
    (match S.exec_command z3 mk_check_sat with
    | Sat -> Fmt.(pf stdout "%i says sat@." i)
    | Unsat -> Fmt.(pf stdout "%i says unsat@." i)
    | resp -> Fmt.(pf stdout "%i says %a@." i pp_solver_response resp));
    (* Exit with (exit) *)
    match S.exec_command z3 mk_exit with
    | Success ->
      Fmt.(pf stdout "KILL TEST: Solver %i exited succesfully.@." i);
      true
    | _ ->
      Fmt.(pf stdout "KILL TEST: Solver %i did not exit successfully@." i);
      false
  in
  let timeout_killer pid () =
    Unix.sleepf 0.2;
    try
      Fmt.(pf stdout "Kill solver %i@." pid);
      Unix.kill pid Sys.sigkill
    with
    | _ -> failwith "Fail"
  in
  let pool = Task.setup_pool ~num_additional_domains:4 () in
  Task.run pool (fun () ->
      let z1 = S.make_z3_solver () in
      let z2 = S.make_z3_solver () in
      let _ = Task.async pool (f 1 z1) in
      let d2 = Task.async pool (f 2 z2) in
      let _ = Task.async pool (timeout_killer z1.s_pid) in
      if Task.await pool d2 then failwith "Thread 2 exited well." else ();
      Task.teardown_pool pool)
;;

ignore (test_parallel_solvers ());
ignore (test_dirty_cancel ())
