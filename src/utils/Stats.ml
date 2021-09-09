open Base

(* ============================================================================================= *)
(*                                GLOBAL TIMING INFO                                             *)
(* ============================================================================================= *)

type proc_time_info = { pti_start : float; pti_status : int; pti_elapsed : float; pti_last : float }

let _TIME_TABLE_ : (int, proc_time_info) Hashtbl.t = Hashtbl.create ~size:100 (module Int)

let solver_kind_pids : (string, int list) Hashtbl.t = Hashtbl.create ~size:3 (module String)

let _PID_ = ref (-2)

let pid_placeholder () : int =
  let id = !_PID_ in
  Int.decr _PID_;
  id

let log_proc_start (pid : int) =
  let t = Unix.gettimeofday () in
  let data = { pti_start = t; pti_status = -1; pti_elapsed = 0.0; pti_last = t } in
  match Hashtbl.add ~key:pid ~data _TIME_TABLE_ with
  | `Duplicate ->
      Log.error_msg Fmt.(str "Process with pid %i already logged in, ignoring new start time." pid)
  | `Ok -> ()

(** Log when a process is restarted. *)
let log_proc_restart (pid : int) =
  let t = Unix.gettimeofday () in
  match Hashtbl.find _TIME_TABLE_ pid with
  | Some pti ->
      let data = { pti with pti_last = t } in
      Hashtbl.set _TIME_TABLE_ ~key:pid ~data
  | None -> log_proc_start pid

(** Update the elapsed time. When pausing a subprocess, first update the
    elapsed time and update the restart time when restarting the process.
  *)
let log_alive (pid : int) =
  let t = Unix.gettimeofday () in
  match Hashtbl.find _TIME_TABLE_ pid with
  | Some pti ->
      let elapsed = pti.pti_elapsed +. (t -. pti.pti_last) in
      let data = { pti with pti_elapsed = elapsed; pti_last = t } in
      Hashtbl.set _TIME_TABLE_ ~key:pid ~data
  | None -> log_proc_start pid

(** Log when a process quits without error (status = 0 by default).
*)
let log_proc_quit ?(status = 0) (pid : int) =
  log_alive pid;
  match Hashtbl.find _TIME_TABLE_ pid with
  | Some pti ->
      let data = { pti with pti_status = status } in
      Hashtbl.set _TIME_TABLE_ ~key:pid ~data
  | None -> Log.error_msg (Fmt.str "Quitting %i without registering it." pid)

let get_elapsed (pid : int) =
  match Hashtbl.find _TIME_TABLE_ pid with Some pti -> pti.pti_elapsed | None -> 0.0

(* === Global timing === *)
let glob_start () =
  Hashtbl.set _TIME_TABLE_ ~key:(-1)
    ~data:{ pti_start = Unix.gettimeofday (); pti_status = -1; pti_elapsed = 0.0; pti_last = 0.0 }

let get_glob_elapsed () =
  let t = Unix.gettimeofday () in
  match Hashtbl.find _TIME_TABLE_ (-1) with
  | Some pti ->
      let elp = t -. pti.pti_start in
      Hashtbl.set ~key:(-1) ~data:{ pti with pti_elapsed = elp } _TIME_TABLE_;
      elp
  | None ->
      let data = { pti_start = t; pti_status = -1; pti_elapsed = 0.0; pti_last = 0.0 } in
      Hashtbl.set ~key:(-1) ~data _TIME_TABLE_;
      0.0

(** Total elapsed time spent in verification of solutions.  *)
let verif_time = ref 0.0

let add_verif_time (t : float) = verif_time := !verif_time +. t

(* === Solvers timing === *)
let log_solver_start (pid : int) (name : string) =
  log_proc_start pid;
  Hashtbl.add_multi solver_kind_pids ~key:name ~data:pid

let print_solvers_summary (frmt : Formatter.t) () : unit =
  let open Fmt in
  let total_solvers_time = ref 0.0 in
  let total_instances = ref 0 in
  let f (key, data) =
    let total_time = List.sum (module Float) ~f:get_elapsed data in
    let instances = List.length data in
    total_solvers_time := !total_solvers_time +. total_time;
    total_instances := !total_instances + instances;
    (key, total_time, instances)
  in
  let pp frmt (key, time, instances) =
    Fmt.(pf frmt "@[<v 0>%-10s [%4i instances] %.3fs @]" key instances time)
  in
  let l = List.map ~f (Hashtbl.to_alist solver_kind_pids) in
  pf frmt "@[<v 0>Total time spent in solvers:@;@[%a@]@;> %-8s [%4i instances]: %.3f@]@;"
    (list ~sep:(fun fmt () -> pf fmt "@;<80 0>") pp)
    l "TOTAL" !total_instances !total_solvers_time
