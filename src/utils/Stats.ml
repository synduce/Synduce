open Base

let timed (f : unit -> 'a) : float * 'a =
  let t0 = Unix.gettimeofday () in
  let res = f () in
  Unix.gettimeofday () -. t0, res
;;

(* ============================================================================================= *)
(*                                SUBPROCESS TIMING INFO                                         *)
(* ============================================================================================= *)

type proc_time_info =
  { pti_start : float
  ; pti_status : int
  ; pti_elapsed : float
  ; pti_last : float
  ; pti_exited : bool
  }

let _TIME_TABLE_ : (int, proc_time_info) Hashtbl.t = Hashtbl.create ~size:100 (module Int)
let _PID_TABLE_ : (int, string) Hashtbl.t = Hashtbl.create ~size:100 (module Int)

let solver_kind_pids : (string, int list) Hashtbl.t =
  Hashtbl.create ~size:3 (module String)
;;

let get_solver_pids () = Hashtbl.to_alist solver_kind_pids
let _PID_ = ref (-2)

let log_proc_start (pid : int) =
  let t = Unix.gettimeofday () in
  let data =
    { pti_start = t
    ; pti_status = -1
    ; pti_elapsed = 0.0
    ; pti_last = t
    ; pti_exited = false
    }
  in
  match Hashtbl.add ~key:pid ~data _TIME_TABLE_ with
  | `Duplicate ->
    Fmt.(pf stderr "Process with pid %i already logged in, ignoring new start time." pid)
  | `Ok -> ()
;;

(** Log when a process is restarted. *)
let log_proc_restart (pid : int) =
  let t = Unix.gettimeofday () in
  match Hashtbl.find _TIME_TABLE_ pid with
  | Some pti ->
    let data = { pti with pti_last = t } in
    Hashtbl.set _TIME_TABLE_ ~key:pid ~data
  | None -> log_proc_start pid
;;

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
;;

(** Get the list of all registered subprocesses still running.  *)
let get_alive () =
  let pids =
    List.map
      ~f:(fun (s_id, _) ->
        match Hashtbl.find _PID_TABLE_ s_id with
        | Some solver_name -> solver_name, s_id
        | None -> if s_id < 0 then "self", s_id else "unknown", s_id)
      (Hashtbl.to_alist (Hashtbl.filter _TIME_TABLE_ ~f:(fun p -> not p.pti_exited)))
  in
  pids
;;

(** Log when a process quits without error (status = 0 by default).
*)
let log_proc_quit ?(status = 0) (pid : int) =
  log_alive pid;
  match Hashtbl.find _TIME_TABLE_ pid with
  | Some pti ->
    let data = { pti with pti_status = status; pti_exited = true } in
    Hashtbl.set _TIME_TABLE_ ~key:pid ~data
  | None -> Fmt.(pf stderr "Quitting %i without registering it." pid)
;;

let get_elapsed (pid : int) =
  match Hashtbl.find _TIME_TABLE_ pid with
  | Some pti -> pti.pti_elapsed
  | None -> 0.0
;;

(* === Global timing === *)
let glob_start () =
  Hashtbl.set
    _TIME_TABLE_
    ~key:(-1)
    ~data:
      { pti_start = Unix.gettimeofday ()
      ; pti_status = -1
      ; pti_elapsed = 0.0
      ; pti_last = 0.0
      ; pti_exited = false
      }
;;

let get_glob_elapsed () =
  let t = Unix.gettimeofday () in
  match Hashtbl.find _TIME_TABLE_ (-1) with
  | Some pti ->
    let elp = t -. pti.pti_start in
    Hashtbl.set ~key:(-1) ~data:{ pti with pti_elapsed = elp } _TIME_TABLE_;
    elp
  | None ->
    let data =
      { pti_start = t
      ; pti_status = -1
      ; pti_elapsed = 0.0
      ; pti_last = 0.0
      ; pti_exited = false
      }
    in
    Hashtbl.set ~key:(-1) ~data _TIME_TABLE_;
    0.0
;;

(** Total elapsed time spent in verification of solutions.  *)
let verif_time = ref 0.0

let add_verif_time (t : float) = verif_time := !verif_time +. t

(* === Solvers timing === *)
let log_solver_start (pid : int) (name : string) =
  log_proc_start pid;
  Hashtbl.set _PID_TABLE_ ~key:pid ~data:name;
  Hashtbl.add_multi solver_kind_pids ~key:name ~data:pid
;;

(* ============================================================================================= *)
(*                               SYNDUCE ALGORITHM TIMING                                        *)
(* ============================================================================================= *)

let refinement_log : (string * Yojson.t) list Stack.t = Stack.create ()
let major_step_counter = ref 0

let log_new_major_step ~tsize ~usize () =
  Int.incr major_step_counter;
  let starttime = get_glob_elapsed () in
  Stack.push
    refinement_log
    [ "step_no", `Int !major_step_counter
    ; ( "start_info"
      , `Assoc
          [ "start_time", `Float starttime
          ; "count_in_t", `Int tsize
          ; "count_in_u", `Int usize
          ] )
    ]
;;

let log_major_step_end
    ~(synth_time : float)
    ~(verif_time : float)
    ~(t : int)
    ~(u : int)
    (verified : bool)
    : unit
  =
  let elapsed = get_glob_elapsed () in
  let json : Yojson.t =
    `Assoc
      [ "elapsed", `Float elapsed
      ; "syth_time", `Float synth_time
      ; "verif_time", `Float verif_time
      ; "count_in_t", `Int t
      ; "count_in_u", `Int u
      ; "verified", `Bool verified
      ]
  in
  match Stack.pop refinement_log with
  | Some x -> Stack.push refinement_log (x @ [ "synt_success", json ])
  | None -> Stack.push refinement_log [ "synt_success", json ]
;;

let log_minor_step ~(synth_time : float) ~(auxtime : float) (lifted : bool) : unit =
  let elapsed = get_glob_elapsed () in
  let json : Yojson.t =
    `Assoc
      [ "elapsed", `Float elapsed
      ; "synth_time", `Float synth_time
      ; "aux_time", `Float auxtime
      ; "lifted", `Bool lifted
      ]
  in
  match Stack.pop refinement_log with
  | Some x -> Stack.push refinement_log (x @ [ "synt_failure", json ])
  | None -> Stack.push refinement_log [ "synt_failure", json ]
;;
