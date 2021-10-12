open Base

(** Get a reference to the table of ids mapping integers ids to names.  *)
val get_ids : unit -> (int, string) Hashtbl.t

(** Reinitialize all state values in the name. *)
val reinit : unit -> unit

(** Returns a fresh, unused id. *)
val new_id : unit -> int

(** `forget id name` Forget the assignments of id and name. *)
val forget : int -> string -> unit

(** Creates a fresh name with base ~s (default "x"). *)
val fresh : ?s:string -> unit -> string

(** [mk_with_id i s f] applies [f] to [i] is [i < 0], otherwise creates a fresh id,
    reigsters the name [s] to that fresh id, and returns the result of applying [f]
    to that fresh id.
*)
val mk_with_id : int -> string -> (int -> 'a) -> 'a

(** Get the name that corresponds to a given id. *)
val get_exn : int -> string
