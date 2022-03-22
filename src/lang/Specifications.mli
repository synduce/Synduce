open Term

type spec =
  { ensures : term option
  ; requires : term option
  }

(** Reinitialize the state of the specifiications. *)
val reinit : unit -> unit

(** An empty spec with no ensures clause and no requires clause. *)
val empty_spec : spec

val get_ensures : variable -> term option
val get_requires : variable -> term option
val get_spec : variable -> spec option
val set_spec : variable -> spec -> unit
val set_ensures : variable -> term -> unit
val set_requires : variable -> term -> unit
val is_not_empty : spec -> bool
val pp_spec : ctx:Context.t -> Format.formatter -> spec -> unit
val dump_all : ctx:Context.t -> Format.formatter -> unit -> unit
