(** {2 not encoded} *)

val findlib_predicates_set_by_dune : string -> bool

val already_linked_libraries : string list

val builtin_library : (string * Meta_parser.t) list
