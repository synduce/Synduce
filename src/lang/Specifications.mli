open Term

(** Reinitialize the state of the specifiications. *)
val reinit : ctx:Context.t -> unit -> unit

(** An empty spec with no ensures clause and no requires clause. *)
val empty_spec : spec

val get_ensures : ctx:Context.t -> variable -> term option
val get_requires : ctx:Context.t -> variable -> term option
val get_spec : ctx:Context.t -> variable -> spec option
val set_spec : ctx:Context.t -> variable -> spec -> unit
val set_ensures : ctx:Context.t -> variable -> term -> unit
val set_requires : ctx:Context.t -> variable -> term -> unit
val is_not_empty : spec -> bool
val pp_spec : ctx:Context.t -> Format.formatter -> spec -> unit
val dump_all : ctx:Context.t -> Format.formatter -> unit -> unit
