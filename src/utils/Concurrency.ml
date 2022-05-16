let run_with_timeouts l = l

let rec wait_and_check counter t =
  Lwt.bind (Lwt_unix.sleep 1.0) (fun _ ->
      if !counter > 1 || t < 0. then wait_and_check counter (t -. 1.0) else Lwt.return ())
;;
