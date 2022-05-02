open Base

(** An environment for names associated to ids.  *)
type t =
  { ids : (int, string) Hashtbl.t
  ; names : (string, int list) Hashtbl.t
  ; max_id : int ref
  }

val create : unit -> t
val copy : t -> t

(** Reinitialize all state values in the name. *)
val reinit : t -> unit

(** Returns a fresh, unused id. *)
val new_id : t -> int

(** `forget id name` Forget the assignments of id and name. *)
val forget : t -> int -> string -> unit

(** Creates a fresh name with base ~s (default "x"). *)
val fresh : ?s:string -> t -> string

(** [mk_with_id i s f] applies [f] to [i] is [i < 0], otherwise creates a fresh id,
    reigsters the name [s] to that fresh id, and returns the result of applying [f]
    to that fresh id.
*)
val mk_with_id : t -> int -> string -> (int -> 'a) -> 'a

(** Get the name that corresponds to a given id. *)
val get_exn : t -> int -> string

(** Check that a variable name from the source file obeys some basic restriction with synduce. *)
val check_source_variable_name : Lexing.position * Lexing.position -> string -> unit

module Nice : sig
  val next : unit -> string
end
