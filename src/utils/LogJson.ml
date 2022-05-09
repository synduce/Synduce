open Base

let saved_solver_stats : (int, Yojson.t) Hashtbl.t = Hashtbl.create ~size:10 (module Int)
let saved_summaries : (int, Yojson.t) Hashtbl.t = Hashtbl.create ~size:10 (module Int)
let saved_timings : (int, float * float) Hashtbl.t = Hashtbl.create ~size:10 (module Int)

(** Outputs a JSON summarizing the time spent in each solver as well as the number
  of solver instances used during the execution of the algorithm.  *)
let solvers_summary () : Yojson.t =
  let total_solvers_time = ref 0.0 in
  let total_instances = ref 0 in
  let f (key, data) =
    let total_time = List.sum (module Float) ~f:Stats.get_elapsed data in
    let instances = List.length data in
    total_solvers_time := !total_solvers_time +. total_time;
    total_instances := !total_instances + instances;
    key, `Assoc [ "total-time", `Float total_time; "num-instances", `Int instances ]
  in
  let l : (string * Yojson.t) list = List.map ~f (Stats.get_solver_pids ()) in
  `Assoc l
;;

(** Outputs a summary of the refinement steps taken by the algorithm in
    the form of a JSON.
  *)
let refinement_steps_summary () : Yojson.t =
  let steps = List.rev (Stack.to_list Stats.refinement_log) in
  `Assoc (List.mapi steps ~f:(fun i s -> Fmt.str "step_%i" i, `Assoc s))
;;

(** Save the stats in a json format, and reset the counters. *)
let save_stats_and_restart (id : int) : unit =
  Hashtbl.set saved_solver_stats ~key:id ~data:(solvers_summary ());
  Hashtbl.set saved_summaries ~key:id ~data:(refinement_steps_summary ());
  Hashtbl.set saved_timings ~key:id ~data:(Stats.get_glob_elapsed (), !Stats.verif_time);
  Stats.restart ()
;;

let get_solver_stats (id : int) = Hashtbl.find saved_solver_stats id
let get_summary (id : int) = Hashtbl.find saved_summaries id
let get_simple_stats (id : int) = Hashtbl.find saved_timings id
