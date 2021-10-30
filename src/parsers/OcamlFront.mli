(** Parse a string as an Ocaml program. Extract a list of declarations (Front.decl)
    and return a synthesis objective, if one is defined in the file.
  *)
val parse_ocaml : string -> Front.definition list * (string * string * string) option
