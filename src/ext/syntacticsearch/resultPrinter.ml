open GoblintCil
open CodeQuery
open Feature

let rec contains list elem =
  match list with
  | x :: xs -> if elem = x then true else contains xs elem
  | [] -> false

let rec get_max_lengths result_list name_l line_l file_l byte_l typ_l id_l =
  match result_list with
  | (name, loc, typ, id) :: xs ->
      get_max_lengths xs
        (max name_l (String.length name))
        (max line_l (String.length (string_of_int loc.line)))
        (max file_l (String.length loc.file))
        (max byte_l (String.length (string_of_int loc.byte)))
        (max typ_l (String.length typ))
        (max id_l (String.length (string_of_int id)))
  | [] -> (name_l, line_l, file_l, byte_l, typ_l, id_l)

let print_result result query =
  let print_name = contains query.sel Name_sel in
  let print_loc = contains query.sel Location_sel in
  let print_typ = contains query.sel Type_sel in
  let print_id = contains query.sel ID_sel in
  let max_lengths = get_max_lengths result 0 0 0 0 0 0 in
  let name_l = match max_lengths with x, _, _, _, _, _ -> x in
  let line_l = match max_lengths with _, x, _, _, _, _ -> x in
  let file_l = match max_lengths with _, _, x, _, _, _ -> x in
  let byte_l = match max_lengths with _, _, _, x, _, _ -> x in
  let typ_l = match max_lengths with _, _, _, _, x, _ -> x in
  let id_l = match max_lengths with _, _, _, _, _, x -> x in
  let rec add_whitespace num =
    if num <= 0 then "" else " " ^ add_whitespace (num - 1)
  in
  let create_name name =
    if print_name then
      "name: " ^ name ^ add_whitespace (name_l - String.length name) ^ ", "
    else ""
  in
  let create_location loc =
    if print_loc then
      "line: " ^ string_of_int loc.line
      ^ add_whitespace (line_l - String.length (string_of_int loc.line))
      ^ ", file: " ^ loc.file
      ^ add_whitespace (file_l - String.length loc.file)
      ^ ", byte: " ^ string_of_int loc.byte
      ^ add_whitespace (byte_l - String.length (string_of_int loc.byte))
      ^ ", "
    else ""
  in
  let create_type typ =
    if print_typ then
      "type: " ^ typ ^ add_whitespace (typ_l - String.length typ) ^ ", "
    else ""
  in
  let create_id id =
    if print_id then
      "id: " ^ string_of_int id
      ^ add_whitespace (id_l - String.length (string_of_int id))
      ^ ""
    else ""
  in
  let create_entry (name, loc, typ, id) =
    create_name name ^ create_location loc ^ create_type typ ^ create_id id
    ^ "\n"
  in
  let rec create_printout list =
    match list with x :: xs -> create_entry x ^ create_printout xs | [] -> ""
  in
  if print_name || print_loc || print_typ || print_id then
    create_printout result
  else ""

let query_file_name = ref ""

let feature = {
  fd_name = "syntacticsearch";
  fd_enabled = false;
  fd_description = "Syntactic Search in CIL programs";
  fd_extraopt = [
    ("--syntacticsearch_query_file",
     Arg.Set_string query_file_name,
     "<fname> Name of the file containing the syntactic search query")
  ];
  fd_doit = (fun f ->
    Printexc.record_backtrace true;
    let q = CodeQuery.parse_json_file !query_file_name in
    let results = QueryMapping.map_query q f in
    print_endline (print_result results q));
  fd_post_check = false
}

let () = Feature.register feature
