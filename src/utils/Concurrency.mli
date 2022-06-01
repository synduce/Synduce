val run_with_timeouts : 'a -> 'a
val wait_and_check : int ref -> float -> unit Lwt.t
val pwait : ('a -> bool) -> int ref -> 'b -> 'a Lwt.t -> ('b * 'a) Lwt.t
