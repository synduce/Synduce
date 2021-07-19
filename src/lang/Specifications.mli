type spec = { ensures : Term.term option; requires : Term.term option }

val empty_spec : spec

val get_ensures : Term.variable -> Term.term option

val get_requires : Term.variable -> Term.term option

val get_spec : Term.variable -> spec option

val set_spec : Term.variable -> spec -> unit

val set_ensures : Term.variable -> Term.term -> unit

val set_requires : Term.variable -> Term.term -> unit

val is_not_empty : spec -> bool

val pp_spec : Format.formatter -> spec -> unit

val dump_all : Format.formatter -> unit -> unit
