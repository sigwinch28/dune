module Location = struct
  type t = string
end

module Section = struct
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

  let of_string = function
    | "lib" -> Lib
    | "lib_root" -> Lib_root
    | "libexec" -> Libexec
    | "libexec_root" -> Libexec_root
    | "bin" -> Bin
    | "sbin" -> Sbin
    | "toplevel" -> Toplevel
    | "share" -> Share
    | "share_root" -> Share_root
    | "etc" -> Etc
    | "doc" -> Doc
    | "stublibs" -> Stublibs
    | "man" -> Man
    | "misc" -> Misc
    | _ -> assert false

  (* since produced by Section.to_string *)
end

let dirs : (string * Section.t, string) Hashtbl.t = Hashtbl.create 10

(* multi-bindings first is the one with least priority *)

let path_sep =
  if Sys.win32 then
    ';'
  else
    ':'

let () =
  match Sys.getenv_opt "DUNE_DIR_LOCATIONS" with
  | None -> ()
  | Some s ->
    let rec aux = function
      | [] -> ()
      | package :: section :: dir :: l ->
        let section = Section.of_string section in
        Hashtbl.add dirs (package, section) dir;
        aux l
      | _ -> invalid_arg "Error parsing DUNE_DIR_LOCATIONS"
    in
    let l = String.split_on_char path_sep s in
    aux l

(* Parse the replacement format described in [artifact_substitution.ml]. *)
let eval s =
  let len = String.length s in
  if s.[0] = '=' then
    let colon_pos = String.index_from s 1 ':' in
    let vlen = int_of_string (String.sub s 1 (colon_pos - 1)) in
    (* This [min] is because the value might have been truncated if it was too
       large *)
    let vlen = min vlen (len - colon_pos - 1) in
    Some (String.sub s (colon_pos + 1) vlen)
  else
    None
  [@@inline never]

let get_dir ~package ~section = Hashtbl.find_all dirs (package, section)

module HardcodedOcamlPath = struct
  type t =
    | None
    | Relocatable
    | Hardcoded of string list
    | FindlibConfig of string

  let t =
    lazy
      ( match eval Sites_locations_data.hardcoded_ocamlpath with
      | None -> None
      | Some "relocatable" -> Relocatable
      | Some s -> (
        let l = String.split_on_char '\000' s in
        match l with
        | "hardcoded" :: l -> Hardcoded l
        | [ "findlibconfig"; p ] -> FindlibConfig p
        | _ -> invalid_arg "dune error: hardcoded_ocamlpath parsing error" ) )
end

let relocatable =
  lazy
    ( match Lazy.force HardcodedOcamlPath.t with
    | Relocatable -> true
    | _ -> false )

let prefix =
  lazy
    (let path = Sys.executable_name in
     let bin = Filename.dirname path in
     let prefix = Filename.dirname bin in
     prefix)

let relocate_if_needed path =
  if Lazy.force relocatable then
    Filename.concat (Lazy.force prefix) path
  else
    path

let site ~package ~section ~suffix ~encoded =
  let dirs = get_dir ~package ~section in
  let dirs =
    match eval encoded with
    | None -> dirs
    | Some d -> relocate_if_needed d :: dirs
  in
  List.rev_map (fun dir -> Filename.concat dir suffix) dirs
  [@@inline never]

let sourceroot local =
  match eval local with
  | Some "" -> None
  | Some _ as x -> x
  | None ->
    (* None if the binary is executed from _build but not by dune, which should
       not happend *)
    Sys.getenv_opt "DUNE_SOURCEROOT"

let path_sep =
  if Sys.win32 then
    ";"
  else
    ":"

let ocamlpath =
  lazy
    (let env =
       match Sys.getenv_opt "OCAMLPATH" with
       | None -> []
       | Some x -> String.split_on_char path_sep.[0] x
     in
     let static =
       match Lazy.force HardcodedOcamlPath.t with
       | HardcodedOcamlPath.None ->
         String.split_on_char path_sep.[0] (Sys.getenv "DUNE_OCAML_HARDCODED")
       | HardcodedOcamlPath.Relocatable ->
         [ Filename.concat (Lazy.force prefix) "lib" ]
       | HardcodedOcamlPath.Hardcoded l -> l
       | HardcodedOcamlPath.FindlibConfig _ -> assert false
     in
     env @ static)

let stdlib =
  lazy
    ( match eval Sites_locations_data.stdlib_dir with
    | None -> Sys.getenv "DUNE_OCAML_STDLIB"
    | Some s -> s )
