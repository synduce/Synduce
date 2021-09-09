open Base

val get_ids : unit -> (int, string) Hashtbl.t
(** Get a reference to the table of ids mapping integers ids to names.  *)

val reinit : unit -> unit
(** Reinitialize all state values in the name. *)

val new_id : unit -> int
(** Returns a fresh, unused id. *)

val forget : int -> string -> unit
(** `forget id name` Forget the assignments of id and name. *)

val fresh : ?s:string -> unit -> string
(** Creates a fresh name with base ~s (default "x"). *)

val mk_with_id : int -> string -> (int -> 'a) -> 'a
(** [mk_with_id i s f] applies [f] to [i] is [i < 0], otherwise creates a fresh id,
    reigsters the name [s] to that fresh id, and returns the result of applying [f]
    to that fresh id.
*)
