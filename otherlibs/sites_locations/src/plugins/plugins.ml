open Sites_locations.Private_
module Data = Sites_locations_plugins_data

let readdir dirs =
  List.concat
    (List.map
       (fun dir -> Array.to_list (Sys.readdir dir))
       (List.filter Sys.file_exists dirs))

(* let file_exists dirs plugin =
 *   List.exists (fun d -> Sys.file_exists (Filename.concat d plugin)) dirs *)

module type S = sig
  val paths : string list

  val list : unit -> string list

  val load_all : unit -> unit

  val load : string -> unit
end

let rec check_predicates predicates =
  match (Sys.backend_type, predicates) with
  | _, [] -> true
  | Sys.Native, Meta_parser.Pos "byte" :: _ -> false
  | Sys.Bytecode, Meta_parser.Pos "native" :: _ -> false
  | Sys.Native, Meta_parser.Pos "native" :: predicates ->
    check_predicates predicates
  | Sys.Bytecode, Meta_parser.Pos "byte" :: predicates ->
    check_predicates predicates
  | Sys.Native, Meta_parser.Neg "native" :: _ -> false
  | Sys.Bytecode, Meta_parser.Neg "byte" :: _ -> false
  | Sys.Native, Meta_parser.Neg "byte" :: predicates ->
    check_predicates predicates
  | Sys.Bytecode, Meta_parser.Neg "native" :: predicates ->
    check_predicates predicates
  | _, Meta_parser.Pos pred :: predicates ->
    Data.findlib_predicates_set_by_dune pred && check_predicates predicates
  | _, Meta_parser.Neg pred :: predicates ->
    (not (Data.findlib_predicates_set_by_dune pred))
    && check_predicates predicates

let rec get_plugin directory plugins requires entries =
  match entries with
  | [] -> (directory, List.rev plugins, List.rev requires)
  | Meta_parser.Comment _ :: entries ->
    get_plugin directory plugins requires entries
  | Package _ :: entries -> get_plugin directory plugins requires entries
  | Rule { var = "directory"; predicates = []; action = Set; value } :: entries
    ->
    get_plugin (Some value) plugins requires entries
  | Rule { var = "plugin"; predicates; action = Set; value } :: entries
    when check_predicates predicates ->
    get_plugin directory [ value ] requires entries
  | Rule { var = "plugin"; predicates; action = Add; value } :: entries
    when check_predicates predicates ->
    get_plugin directory (value :: plugins) requires entries
  | Rule { var = "requires"; predicates; action = Set; value } :: entries
    when check_predicates predicates ->
    get_plugin directory plugins [ value ] entries
  | Rule { var = "requires"; predicates; action = Add; value } :: entries
    when check_predicates predicates ->
    get_plugin directory plugins (value :: requires) entries
  | Rule _ :: entries -> get_plugin directory plugins requires entries

exception Lib_not_found of string

let rec find_library ~rest meta =
  match rest with
  | [] -> meta
  | pkg :: rest ->
    let rec aux pkg = function
      | [] -> raise (Lib_not_found pkg)
      | Meta_parser.Package { name = Some name; entries } :: _
        when String.equal name pkg ->
        find_library ~rest entries
      | _ :: entries -> aux pkg entries
    in
    aux pkg meta

let extract_words s ~is_word_char =
  let rec skip_blanks i =
    if i = String.length s then
      []
    else if is_word_char s.[i] then
      parse_word i (i + 1)
    else
      skip_blanks (i + 1)
  and parse_word i j =
    if j = String.length s then
      [ StringLabels.sub s ~pos:i ~len:(j - i) ]
    else if is_word_char s.[j] then
      parse_word i (j + 1)
    else
      StringLabels.sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
  in
  skip_blanks 0

let extract_comma_space_separated_words s =
  extract_words s ~is_word_char:(function
    | ','
    | ' '
    | '\t'
    | '\n' ->
      false
    | _ -> true)

let split_all l = List.concat (List.map extract_comma_space_separated_words l)

let find_plugin ~dir ~rest meta =
  let directory, plugins, requires =
    get_plugin None [] [] (find_library ~rest meta.Meta_parser.entries)
  in
  let directory =
    match directory with
    | None -> dir
    | Some pkg_dir ->
      if pkg_dir.[0] = '+' || pkg_dir.[0] = '^' then
        Filename.concat
          (Lazy.force Helpers.stdlib)
          (String.sub pkg_dir 1 (String.length pkg_dir - 1))
      else if Filename.is_relative pkg_dir then
        Filename.concat dir pkg_dir
      else
        pkg_dir
  in
  let plugins = split_all plugins in
  let requires = split_all requires in
  (directory, plugins, requires)

let load file ~pkg =
  let entries =
    let ic = open_in file in
    try
      let lb = Lexing.from_channel ic in
      lb.lex_curr_p <-
        { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      let r = Meta_parser.Parse.entries lb 0 [] in
      close_in ic;
      r
    with exn ->
      close_in ic;
      raise exn
  in
  { Meta_parser.name = Some pkg; entries }

exception LibraryNotFound of string

let meta_fn = "META"

let lookup_and_load_one_dir ~dir ~pkg =
  let meta_file = Filename.concat dir meta_fn in
  if Sys.file_exists meta_file then
    Some (load meta_file ~pkg)
  else
    (* Alternative layout *)
    let dir = Filename.dirname dir in
    let meta_file = Filename.concat dir (meta_fn ^ "." ^ pkg) in
    if Sys.file_exists meta_file then
      Some (load meta_file ~pkg)
    else
      None

let split name =
  match String.split_on_char '.' name with
  | [] -> raise (LibraryNotFound name)
  | pkg :: rest -> (pkg, rest)

let lookup_and_summarize dirs name =
  let pkg, rest = split name in
  let rec loop dirs =
    match dirs with
    | [] -> (
      List.assoc_opt pkg Data.builtin_library |> function
      | None -> raise (LibraryNotFound name)
      | Some meta -> find_plugin ~dir:(Lazy.force Helpers.stdlib) ~rest meta )
    | dir :: dirs -> (
      let dir = Filename.concat dir pkg in
      match lookup_and_load_one_dir ~dir ~pkg with
      | None -> loop dirs
      | Some p -> find_plugin ~dir ~rest p )
  in
  loop dirs

let loaded_libraries =
  lazy
    (let h = Hashtbl.create 10 in
     List.iter (fun s -> Hashtbl.add h s ()) Data.already_linked_libraries;
     h)

let load_gen ~load_requires dirs name =
  let loaded_libraries = Lazy.force loaded_libraries in
  if not (Hashtbl.mem loaded_libraries name) then (
    Hashtbl.add loaded_libraries name ();
    let directory, plugins, requires = lookup_and_summarize dirs name in
    List.iter load_requires requires;
    List.iter
      (fun p ->
        let file = Filename.concat directory p in
        Dynlink.loadfile file)
      plugins
  )

let rec load_requires name =
  load_gen ~load_requires (Lazy.force Helpers.ocamlpath) name

let load_plugin plugin_paths name = load_gen ~load_requires plugin_paths name

module Make (X : sig
  val paths : string list
end) : S = struct
  include X

  let list () = readdir paths

  let load name = load_plugin paths name

  let load_all () = List.iter load (list ())
end

let load = load_requires
