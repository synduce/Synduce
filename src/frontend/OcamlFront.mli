(** Parse a string as an Ocaml program. Extract a list of definitions (Front.definition)
    and return a synthesis objective, if one is defined in the file.
  *)
val parse_ocaml : string -> Front.definition list * Front.psi_def option
