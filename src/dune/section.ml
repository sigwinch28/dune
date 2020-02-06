open! Stdune

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

let compare : t -> t -> Ordering.t = Poly.compare

let to_dyn x =
  let open Dyn.Encoder in
  match x with
  | Lib -> constr "Lib" []
  | Lib_root -> constr "Lib_root" []
  | Libexec -> constr "Libexec" []
  | Libexec_root -> constr "Libexec_root" []
  | Bin -> constr "Bin" []
  | Sbin -> constr "Sbin" []
  | Toplevel -> constr "Toplevel" []
  | Share -> constr "Share" []
  | Share_root -> constr "Share_root" []
  | Etc -> constr "Etc" []
  | Doc -> constr "Doc" []
  | Stublibs -> constr "Stublibs" []
  | Man -> constr "Man" []
  | Misc -> constr "Misc" []

module Key = struct
  type nonrec t = t

  let compare = compare

  let to_dyn = to_dyn
end

module O = Comparable.Make (Key)
module Map = O.Map
module Set = O.Set

let to_string = function
  | Lib -> "lib"
  | Lib_root -> "lib_root"
  | Libexec -> "libexec"
  | Libexec_root -> "libexec_root"
  | Bin -> "bin"
  | Sbin -> "sbin"
  | Toplevel -> "toplevel"
  | Share -> "share"
  | Share_root -> "share_root"
  | Etc -> "etc"
  | Doc -> "doc"
  | Stublibs -> "stublibs"
  | Man -> "man"
  | Misc -> "misc"

let of_string = function
  | "lib" -> Some Lib
  | "lib_root" -> Some Lib_root
  | "libexec" -> Some Libexec
  | "libexec_root" -> Some Libexec_root
  | "bin" -> Some Bin
  | "sbin" -> Some Sbin
  | "toplevel" -> Some Toplevel
  | "share" -> Some Share
  | "share_root" -> Some Share_root
  | "etc" -> Some Etc
  | "doc" -> Some Doc
  | "stublibs" -> Some Stublibs
  | "man" -> Some Man
  | "misc" -> Some Misc
  | _ -> None

let parse_string s =
  match of_string s with
  | Some s -> Ok s
  | None -> Error (sprintf "invalid section: %s" s)

let decode =
  let open Dune_lang.Decoder in
  enum
    [ ("lib", Lib)
    ; ("lib_root", Lib_root)
    ; ("libexec", Libexec)
    ; ("libexec_root", Libexec_root)
    ; ("bin", Bin)
    ; ("sbin", Sbin)
    ; ("toplevel", Toplevel)
    ; ("share", Share)
    ; ("share_root", Share_root)
    ; ("etc", Etc)
    ; ("doc", Doc)
    ; ("stublibs", Stublibs)
    ; ("man", Man)
    ; ("misc", Misc)
    ]

let encode v =
  let open Dune_lang.Encoder in
  string (to_string v)

let all =
  Set.of_list
    [ Lib
    ; Lib_root
    ; Libexec
    ; Libexec_root
    ; Bin
    ; Sbin
    ; Toplevel
    ; Share
    ; Share_root
    ; Etc
    ; Doc
    ; Stublibs
    ; Man
    ; Misc
    ]

let should_set_executable_bit = function
  | Lib
  | Lib_root
  | Toplevel
  | Share
  | Share_root
  | Etc
  | Doc
  | Man
  | Misc ->
    false
  | Libexec
  | Libexec_root
  | Bin
  | Sbin
  | Stublibs ->
    true

let valid_format_doc =
  Pp.text
    "Module names must be non-empty and composed only of the following \
     characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'."

module Modulelike (S : sig
  type t

  val module_ : string

  val description : string

  val to_string : t -> string

  val make : string -> t
end) =
Stringlike.Make (struct
  include S

  let valid_char = function
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '\''
    | '_' ->
      true
    | _ -> false

  let description_of_valid_string = Some valid_format_doc

  let hint_valid =
    Some
      (fun name ->
        String.filter_map name ~f:(fun c ->
            if valid_char c then
              Some c
            else
              match c with
              | '.'
              | '-' ->
                Some '_'
              | _ -> None))

  let is_valid_module_name name =
    match name with
    | "" -> false
    | s -> (
      try
        ( match s.[0] with
        | 'A' .. 'Z'
        | 'a' .. 'z' ->
          ()
        | _ -> raise_notrace Exit );
        String.iter s ~f:(fun c ->
            if not (valid_char c) then raise_notrace Exit);
        true
      with Exit -> false )

  let of_string_opt s =
    if is_valid_module_name s then
      Some (S.make s)
    else
      None
end)

module Site = struct
  module T =
    Interned.Make
      (struct
        let initial_size = 16

        let resize_policy = Interned.Conservative

        let order = Interned.Natural
      end)
      ()

  include T

  include (
    Modulelike (struct
      let module_ = "Section.Site"

      let description = "site name"

      include T
    end) :
      Stringlike_intf.S with type t := t )

  module Infix = Comparator.Operators (T)
end

let sites_locations_syntax =
  Dune_lang.Syntax.create ~name:"sites_locations"
    ~desc:"the sites locations extension (experimental)"
    [ ((0, 1), `Since (2, 7)) ]
