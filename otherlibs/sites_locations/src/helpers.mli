(** Provide locations information *)

module Location : sig
  type t = string
end

module Section : sig
  type t =
    | Lib
    | Lib_root
    | Libexec
    | Libexec_root
    | Bin
    | Sbin
    | Toplevel
    | Share
    | Share_root
    | Etc
    | Doc
    | Stublibs
    | Man
    | Misc
end

val site :
     package:string
  -> section:Section.t
  -> suffix:string
  -> encoded:string
  -> Location.t list

val relocatable : bool Lazy.t

val ocamlpath : string list Lazy.t

val sourceroot : string -> string option

val stdlib : string Lazy.t

val path_sep : string

module HardcodedOcamlPath : sig
  type t =
    | None
    | Relocatable
    | Hardcoded of string list
    | FindlibConfig of string

  val t : t Lazy.t
end
