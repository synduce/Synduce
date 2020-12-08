open Base

type typ = string * int
val get_ids : unit -> (int, string) Hashtbl.t
val new_id : unit -> int
val fresh : string -> string

val mk_with_id : int -> string ->  (int -> 'a) -> 'a
