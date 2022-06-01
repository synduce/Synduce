let run_with_timeouts l = l

let rec wait_and_check counter t =
  Lwt.bind (Lwt_unix.sleep 1.0) (fun _ ->
      if !counter > 1 || t < 0. then wait_and_check counter (t -. 1.0) else Lwt.return ())
;;

let pwait stop_pred counter ctx e =
  let open Base in
  Lwt.bind e (fun x ->
      if stop_pred x
      then
        Lwt.map
          (fun _ -> ctx, x)
          (if !counter > 1
          then (
            Int.decr counter;
            wait_and_check counter (10. *. !Config.Optims.wait_parallel_tlimit))
          else Lwt.return ())
      else Lwt.return (ctx, x))
;;
