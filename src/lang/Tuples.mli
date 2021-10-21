val type_name_of_types : RType.t list -> string
val constr_name_of_types : RType.t list -> string
val is_constr_name : string -> bool
val proj_name_of_types : RType.t list -> int -> string
val types_of_tuple_name : string -> RType.t list option
val proj_of_proj_name : string -> (RType.t list * int) option
