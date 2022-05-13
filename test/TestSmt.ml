open Base
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

let test_parallel_solvers n =
  let f _ () =
    let z3 = S.make_z3_solver () in
    S.load_min_max_defs z3;
    match S.exec_command z3 mk_exit with
    | Success -> true
    | _ -> false
  in
  let pool = Task.setup_pool ~num_additional_domains:4 () in
  let res =
    Task.run pool (fun () ->
        let ds = List.init n ~f:(fun i -> Task.async pool (f i)) in
        List.fold ~init:true ~f:(fun acc d -> acc && Task.await pool d) ds)
  in
  Task.teardown_pool pool;
  res
;;

let test_dirty_cancel () =
  let f i zid () =
    let z3 = S.make_z3_solver () in
    zid := z3.S.s_pid;
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
      Int.incr j;
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
  let timeout_killer flag pid () =
    Unix.sleepf 0.2;
    try
      Fmt.(pf stdout "Kill solver %i@." pid);
      Unix.kill pid Caml.Sys.sigkill;
      flag := true
    with
    | _ -> flag := false
  in
  let pool = Task.setup_pool ~num_additional_domains:4 () in
  let res =
    Task.run pool (fun () ->
        let flag = ref false in
        let zid1 = ref 0 in
        let zid2 = ref 0 in
        let zid3 = ref 0 in
        let _ = Task.async pool (f 1 zid1) in
        let d2 = Task.async pool (f 2 zid2) in
        let d3 = Task.async pool (f 3 zid3) in
        let _ = Task.async pool (timeout_killer flag !zid1) in
        Task.await pool d2 && Task.await pool d3 && !flag)
  in
  Task.teardown_pool pool;
  res
;;

let test_task_net () =
  let f c i solver () =
    S.load_min_max_defs solver;
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
    S.exec_all solver commands;
    let j = ref 0 in
    while !j < 10 do
      Unix.sleepf 0.02;
      Int.incr j;
      S.smt_assert solver (dirty_smt "(= (+ x y z) (- z))")
    done;
    (* Check sat. *)
    let resp = S.exec_command solver mk_check_sat in
    (* Exit with (exit) *)
    match S.exec_command solver mk_exit with
    | Success -> Chan.send c (resp, i)
    | _ -> Chan.send c (Error "n", i)
  in
  let rec g channels =
    let check_success c =
      match Chan.recv_poll c with
      | Some (resp, _) ->
        (match resp with
        | Sat -> None
        | _ -> failwith "Terminated with error.")
      | None -> Some c
    in
    if List.length channels > 0
    then (
      Unix.sleepf 0.01;
      g (List.filter_map ~f:check_success channels))
    else true
  in
  let pool = Task.setup_pool ~num_additional_domains:4 () in
  Task.run pool (fun () ->
      let c1 = Chan.make_unbounded () in
      let c2 = Chan.make_unbounded () in
      let c3 = Chan.make_unbounded () in
      let c4 = Chan.make_unbounded () in
      let _ = Task.async pool (f c1 1 (S.make_z3_solver ())) in
      let _ = Task.async pool (f c2 2 (S.make_z3_solver ())) in
      let _ = Task.async pool (f c3 3 (S.make_z3_solver ())) in
      let _ = Task.async pool (f c4 4 (S.make_z3_solver ())) in
      if g [ c1; c2; c3; c4 ] then Fmt.(pf stdout "PARALLEL TASKS: Terminated.@.") else ();
      Task.teardown_pool pool)
;;

let test_task_net_take_first_sol n =
  let f in_c i solver () =
    S.load_min_max_defs solver;
    let commands =
      [ mk_set_logic Logics.ALL
      ; mk_const_decl "x" mk_int_sort
      ; mk_const_decl "y" mk_int_sort
      ; mk_const_decl "z" mk_int_sort
      ; mk_const_decl "a" mk_int_sort
      ; mk_assert (dirty_smt "(= (+ x y) (- 2))")
      ; mk_assert (dirty_smt "(= (+ x z) 0)")
      ; mk_assert (dirty_smt "(= (* x z) (- 1))")
      ; mk_assert (dirty_smt "(= (* x y) 1)")
      ; mk_assert (dirty_smt "(= (* x y) a)")
      ]
    in
    S.exec_all solver commands;
    let j = ref 0 in
    while !j < 50 do
      Unix.sleepf 0.01;
      Int.incr j;
      S.smt_assert solver (dirty_smt "(= (+ x y z) (- z))")
    done;
    (* Check sat. *)
    let resp = S.exec_command solver mk_check_sat in
    (* Exit with (exit) *)
    match S.exec_command solver mk_exit with
    | Success -> Chan.send in_c (resp, i)
    | _ -> Chan.send in_c (Unknown, i)
  in
  let rec check_channels channels =
    let check_success (in_c, idx, pid) =
      match Chan.recv_poll in_c with
      | Some (resp, _) ->
        (match resp with
        | Sat -> `Fst idx
        | _ -> `Snd idx)
      | None -> `Trd (in_c, idx, pid)
    in
    let terminated_ok, _, running = List.partition3_map ~f:check_success channels in
    if List.length terminated_ok > 0
    then (
      List.iter ~f:(fun (_, _, pid) -> Unix.kill pid Caml.Sys.sigkill) running;
      terminated_ok)
    else if List.length running > 0
    then (
      (* Sleep 10ms and check again. *)
      Fmt.(pf stderr "Checking...@.");
      Unix.sleepf 0.01;
      check_channels running)
    else []
  in
  let pool = Task.setup_pool ~num_additional_domains:5 () in
  let res =
    Task.run pool (fun () ->
        let channels =
          List.init n ~f:(fun i ->
              let in_c = Chan.make_unbounded () in
              let solver = S.make_z3_solver () in
              let _ = Task.async pool (f in_c i (S.make_z3_solver ())) in
              in_c, i, solver.s_pid)
        in
        match check_channels channels with
        | _ :: _ -> "sat"
        | _ -> "no-answer")
  in
  Task.teardown_pool pool;
  res
;;

(* ignore (test_parallel_solvers ());
for _ = 1 to 5 do
  ignore (test_task_net ());
  ignore (test_task_net_take_first_sol ())
done *)

let () =
  let open Alcotest in
  run
    "Solvers"
    [ (* ( "dirty_cancel"
      , [ test_case "3-threads" `Quick (fun () ->
              (check bool) "3-threads" (test_dirty_cancel ()) true)
        ] ) *)
      (* ; *)
      ( "parallel_solvers"
      , [ test_case "2-threads" `Quick (fun () ->
              (check bool) "2-threads" (test_parallel_solvers 2) true)
        ; test_case "3-threads" `Slow (fun () ->
              (check bool) "3-threads" (test_parallel_solvers 3) true)
        ; test_case "4-threads" `Slow (fun () ->
              (check bool) "4-threads" (test_parallel_solvers 4) true)
        ; test_case "5-threads" `Slow (fun () ->
              (check bool) "5-threads" (test_parallel_solvers 5) true)
        ] )
      (* ; ( "task_net_take_first_sol"
      , [ test_case "2-threads" `Quick (fun () ->
              (check string) "2-threads" (test_task_net_take_first_sol 2) "sat")
        ; test_case "3-threads" `Slow (fun () ->
              (check string) "3-threads" (test_task_net_take_first_sol 3) "sat")
        ; test_case "4-threads" `Slow (fun () ->
              (check string) "4-threads" (test_task_net_take_first_sol 4) "sat")
        ] ) *)
    ]
;;
