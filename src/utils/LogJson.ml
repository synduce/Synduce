open Base

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
    key, `Assoc [ "total_time", `Float total_time; "num_instances", `Int instances ]
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
