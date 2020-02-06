module type S = sig
  val paths : string list

  val list : unit -> string list

  val load_all : unit -> unit

  val load : string -> unit
end

module Make (X : sig
  val paths : string list
end) : S

val load : string -> unit
