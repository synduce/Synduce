open Base

let timed (f : unit -> 'a) : float * 'a =
  let t0 = Unix.gettimeofday () in
  let res = f () in
  Unix.gettimeofday () -. t0, res
;;

let lwt_timed (f : unit Lwt.t -> 'a Lwt.t) : (float * 'a) Lwt.t =
  let t0 = Unix.gettimeofday () in
  Lwt.map (fun res -> Unix.gettimeofday () -. t0, res) (f (Lwt.return ()))
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

let _SOLVER_KIND_PIDS_ : (string, int list) Hashtbl.t =
  Hashtbl.create ~size:3 (module String)
;;

let get_solver_pids () = Hashtbl.to_alist _SOLVER_KIND_PIDS_
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
  Hashtbl.add_multi _SOLVER_KIND_PIDS_ ~key:name ~data:pid
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
    ?(failure_step = false)
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
      ; "synth_time", `Float synth_time
      ; "verif_time", `Float verif_time
      ; "count_in_t", `Int t
      ; "count_in_u", `Int u
      ; "verified", `Bool verified
      ]
  in
  let kw = if failure_step then "failed" else "success" in
  match Stack.pop refinement_log with
  | Some x ->
    (match x with
    (* We should always have a step_no and a start_info entr *)
    | step_no :: start_info :: failures ->
      Stack.push
        refinement_log
        ([ step_no; start_info; "failures", `Assoc failures ] @ [ kw, json ])
    | _ -> Stack.push refinement_log (x @ [ kw, json ]))
  | None -> Stack.push refinement_log [ kw, json ]
;;

type verif_method =
  | BoundedChecking
  | Induction

let verif_method_to_str = function
  | BoundedChecking -> "bounded checking"
  | Induction -> "induction"
;;

let counterexample_classification_method : verif_method option ref = ref None

let update_counterexample_classification_method (v : verif_method) =
  match !counterexample_classification_method with
  | None -> counterexample_classification_method := Some v
  (* If the method was induction, it either stays induction or switched to bmc. *)
  | Some Induction -> counterexample_classification_method := Some v
  (* Method cannot change back to induction if we used boundedchecking at some point. *)
  | Some BoundedChecking -> ()
;;

let last_lemma_synthesized : (string * string) option ref = ref None

(**  If a lemma has been proved, the [Some true] means it has been proved by induction
  and [Some false] means it has been proved by bounded checking.
*)
let last_lemma_proved : verif_method option ref = ref None

let set_lemma_synthesized (kind : string) (expression : string) =
  last_lemma_synthesized := Some (kind, expression)
;;

let set_last_lemma_proof_method (v : verif_method) = last_lemma_proved := Some v

let log_minor_step ~(synth_time : float) ~(auxtime : float) (lifted : bool) : unit =
  let elapsed = get_glob_elapsed () in
  let json : Yojson.t =
    `Assoc
      ([ "elapsed", `Float elapsed
       ; "synth_time", `Float synth_time
       ; "aux_time", `Float auxtime
       ; "lifted", `Bool lifted
       ]
      @ (match !counterexample_classification_method with
        | Some vmethod ->
          [ "cex_classification_with", `String (verif_method_to_str vmethod) ]
        | None -> [])
      @
      match !last_lemma_synthesized with
      | Some (kind_id, lemma_term) ->
        [ kind_id, `String lemma_term ]
        @
        (match !last_lemma_proved with
        | Some proof_method -> [ "proved_by", `String (verif_method_to_str proof_method) ]
        | None -> [])
      | None -> [])
  in
  (* Resset all the fields. *)
  last_lemma_synthesized := None;
  last_lemma_proved := None;
  counterexample_classification_method := None;
  match Stack.pop refinement_log with
  | Some x ->
    let entry_name = Fmt.(str "failure_%i" (List.length x - 2)) in
    Stack.push refinement_log (x @ [ entry_name, json ])
  | None -> Stack.push refinement_log [ "failure_0", json ]
;;

let restart () =
  (* Clear process logging fields. *)
  _PID_ := -2;
  Hashtbl.clear _PID_TABLE_;
  Hashtbl.clear _TIME_TABLE_;
  Hashtbl.clear _SOLVER_KIND_PIDS_;
  verif_time := 0.0;
  (* Clear refinement step logging fields. *)
  Stack.clear refinement_log;
  major_step_counter := 0;
  last_lemma_proved := None;
  last_lemma_synthesized := None;
  counterexample_classification_method := None;
  (* Restart global time. *)
  glob_start ()
;;

(* ============================================================================================= *)
(*                               OTHER STATS                                                     *)
(* ============================================================================================= *)

let num_unr_cache_hits = ref 0
let orig_solution_hit = ref false
let num_foreign_lemma_uses = ref 0
