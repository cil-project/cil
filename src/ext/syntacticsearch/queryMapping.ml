open Cil
open CodeQuery

(* Default output if the input-query is not supported *)
let loc_default = { line = -1; file = ""; byte = -1; column = -1 }

let rec delete_elem (name1, loc1, typ1, id1) list =
  match list with
  | (name2, loc2, typ2, id2) :: xs ->
      if
        String.compare name1 name2 = 0
        && loc1.line = loc2.line && loc1.byte = loc2.byte && loc1.column = loc2.column
        && String.compare loc1.file loc2.file = 0
        && String.compare typ1 typ2 = 0
        && id1 = id2
      then delete_elem (name1, loc1, typ1, id1) xs
      else (name2, loc2, typ2, id2) :: delete_elem (name1, loc1, typ1, id1) xs
  | [] -> []

let rec delete_duplicates list tbl =
  match list with
  | x :: xs -> (
      let _ = try Hashtbl.find tbl x with Not_found -> Hashtbl.add tbl x 1; 1 in
      x :: delete_duplicates xs tbl
      )
  | [] -> []

let rec and_one_elem (name1, loc1, typ1, id1) list =
  match list with
  | (name2, loc2, typ2, id2) :: xs ->
      if loc1.line = loc2.line then
        [ (name1, loc1, typ1, id1); (name2, loc2, typ2, id2) ]
        @ and_one_elem (name1, loc1, typ1, id1) xs
      else and_one_elem (name1, loc1, typ1, id1) xs
  | [] -> []

let rec and_two_lists list1 list2 =
  match list1 with
  | x :: xs -> and_one_elem x list2 @ and_two_lists xs list2
  | [] -> []

let rec and_several_lists list_of_lists =
  match list_of_lists with
  | x :: y :: xs -> and_several_lists (and_two_lists x y :: xs)
  | [ x ] -> x
  | [] -> []

(* Naming of functions: resolve_query_[kind]_[find]_[structure] *)

(* Resolution of datatype-oriented queries *)
let resolve_query_datatype_uses_fun query cilfile funname =
  match query.tar with
  | Name_t name -> FuncDatatype.find_uses_in_fun name funname cilfile
  | And_t list ->
      and_several_lists
        (List.map
           (fun x -> FuncDatatype.find_uses_in_fun x funname cilfile)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncDatatype.find_uses_in_fun x funname cilfile)
           list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_datatype_uses_none query cilfile =
  match query.tar with
  | Name_t name -> FuncDatatype.find_uses name cilfile
  | And_t list ->
      and_several_lists
        (List.map (fun x -> FuncDatatype.find_uses x cilfile) list)
  | Or_t list ->
      List.flatten (List.map (fun x -> FuncDatatype.find_uses x cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_datatype_uses_cond query cilfile =
  match query.tar with
  | Name_t name -> FuncDatatype.find_uses_in_cond name cilfile
  | And_t list ->
      and_several_lists
        (List.map (fun x -> FuncDatatype.find_uses_in_cond x cilfile) list)
  | Or_t list ->
      List.flatten
        (List.map (fun x -> FuncDatatype.find_uses_in_cond x cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_datatype_uses_noncond query cilfile =
  match query.tar with
  | Name_t name -> FuncDatatype.find_uses_in_noncond name cilfile
  | And_t list ->
      and_several_lists
        (List.map (fun x -> FuncDatatype.find_uses_in_noncond x cilfile) list)
  | Or_t list ->
      List.flatten
        (List.map (fun x -> FuncDatatype.find_uses_in_cond x cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_datatype_uses query cilfile =
  match query.str with
  | Fun_s funname -> resolve_query_datatype_uses_fun query cilfile funname
  | Cond_s -> resolve_query_datatype_uses_cond query cilfile
  | NonCond_s -> resolve_query_datatype_uses_noncond query cilfile
  | None_s -> resolve_query_datatype_uses_none query cilfile

let resolve_query_datatype_defs_none query cilfile =
  match query.tar with
  | Name_t name -> FuncDatatype.find_def name cilfile
  | Or_t list ->
      List.flatten (List.map (fun x -> FuncDatatype.find_def x cilfile) list)
  | All_t -> FuncDatatype.find_def_all cilfile
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_datatype_defs query cilfile =
  match query.str with
  | None_s -> resolve_query_datatype_defs_none query cilfile
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_datatype query cilfile =
  match query.f with
  | Uses_f -> resolve_query_datatype_uses query cilfile
  | Defs_f -> resolve_query_datatype_defs query cilfile
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

(* Resolution of variable-oriented queries *)
let resolve_query_var_uses_fun query cilfile funname =
  match query.tar with
  | Name_t name -> FuncVar.find_uses_in_fun name (-1) funname cilfile false
  | ID_t id -> FuncVar.find_uses_in_fun "" id funname cilfile false
  | AllGlobVar_t -> FuncVar.find_uses_in_fun_all_glob funname cilfile false
  | All_t -> FuncVar.find_uses_in_fun_all funname cilfile false
  | And_t list ->
      and_several_lists
        (List.map
           (fun x -> FuncVar.find_uses_in_fun x (-1) funname cilfile false)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncVar.find_uses_in_fun x (-1) funname cilfile false)
           list)

let resolve_query_var_uses_none query cilfile =
  match query.tar with
  | Name_t name -> FuncVar.find_uses name (-1) cilfile false
  | ID_t id -> FuncVar.find_uses "" id cilfile false
  | AllGlobVar_t -> FuncVar.find_uses_all_glob cilfile false
  | All_t -> FuncVar.find_uses_all cilfile false
  | And_t list ->
      and_several_lists
        (List.map (fun x -> FuncVar.find_uses x (-1) cilfile false) list)
  | Or_t list ->
      List.flatten
        (List.map (fun x -> FuncVar.find_uses x (-1) cilfile false) list)

let resolve_query_var_uses_cond query cilfile =
  match query.tar with
  | Name_t name -> FuncVar.find_uses_in_cond name (-1) cilfile false
  | ID_t id -> FuncVar.find_uses_in_cond "" id cilfile false
  | AllGlobVar_t -> FuncVar.find_uses_in_cond_all_glob cilfile false
  | All_t -> FuncVar.find_uses_in_cond_all cilfile false
  | And_t list ->
      and_several_lists
        (List.map
           (fun x -> FuncVar.find_uses_in_cond x (-1) cilfile false)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncVar.find_uses_in_cond x (-1) cilfile false)
           list)

let resolve_query_var_uses_noncond query cilfile =
  match query.tar with
  | Name_t name -> FuncVar.find_uses_in_noncond name (-1) cilfile false
  | ID_t id -> FuncVar.find_uses_in_noncond "" id cilfile false
  | AllGlobVar_t -> FuncVar.find_uses_in_noncond_all_glob cilfile false
  | All_t -> FuncVar.find_uses_in_noncond_all cilfile false
  | And_t list ->
      and_several_lists
        (List.map
           (fun x -> FuncVar.find_uses_in_noncond x (-1) cilfile false)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncVar.find_uses_in_noncond x (-1) cilfile false)
           list)

let resolve_query_var_uses query cilfile =
  match query.str with
  | Fun_s funname -> resolve_query_var_uses_fun query cilfile funname
  | Cond_s -> resolve_query_var_uses_cond query cilfile
  | NonCond_s -> resolve_query_var_uses_noncond query cilfile
  | None_s -> resolve_query_var_uses_none query cilfile

let resolve_query_var_decl_fun query cilfile funname =
  match query.tar with
  | Name_t name -> FuncVar.find_decl_in_fun name (-1) funname cilfile
  | ID_t id -> FuncVar.find_decl_in_fun "" id funname cilfile
  | All_t -> FuncVar.find_decl_in_fun_all funname cilfile
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncVar.find_decl_in_fun x (-1) funname cilfile)
           list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_var_decl_none query cilfile =
  match query.tar with
  | AllGlobVar_t -> FuncVar.find_decl_all_glob cilfile
  | Name_t name -> FuncVar.find_decl name (-1) cilfile
  | ID_t id -> FuncVar.find_decl "" id cilfile
  | All_t -> FuncVar.find_decl_all cilfile
  | Or_t list ->
      List.flatten (List.map (fun x -> FuncVar.find_decl x (-1) cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_var_decl query cilfile =
  match query.str with
  | Fun_s funname -> resolve_query_var_decl_fun query cilfile funname
  | None_s -> resolve_query_var_decl_none query cilfile
  | NonCond_s -> resolve_query_var_decl_none query cilfile
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_var_defs_fun query cilfile funname =
  match query.tar with
  | Name_t name -> FuncVar.find_defs_in_fun name (-1) funname cilfile
  | ID_t id -> FuncVar.find_defs_in_fun "" id funname cilfile
  | AllGlobVar_t -> FuncVar.find_defs_in_fun_all_glob funname cilfile
  | All_t -> FuncVar.find_defs_in_fun_all funname cilfile
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncVar.find_defs_in_fun x (-1) funname cilfile)
           list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_var_defs_none query cilfile =
  match query.tar with
  | Name_t name -> FuncVar.find_defs name (-1) cilfile
  | ID_t id -> FuncVar.find_defs "" id cilfile
  | AllGlobVar_t -> FuncVar.find_defs_all_glob cilfile
  | All_t -> FuncVar.find_defs_all cilfile
  | Or_t list ->
      List.flatten (List.map (fun x -> FuncVar.find_defs x (-1) cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_var_defs query cilfile =
  match query.str with
  | Fun_s funname -> resolve_query_var_defs_fun query cilfile funname
  | None_s -> resolve_query_var_defs_none query cilfile
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_var query cilfile =
  match query.f with
  | Uses_f -> resolve_query_var_uses query cilfile
  | Decl_f -> resolve_query_var_decl query cilfile
  | Defs_f -> resolve_query_var_defs query cilfile
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_return_none query cilfile =
  match query.tar with
  | Name_t name -> FuncFunction.find_returns name (-1) cilfile
  | ID_t id -> FuncFunction.find_returns "" id cilfile
  | All_t -> FuncFunction.find_returns_all cilfile
  | Or_t list ->
      List.flatten
        (List.map (fun x -> FuncFunction.find_returns x (-1) cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_return query cilfile =
  match query.str with
  | None_s -> resolve_query_fun_return_none query cilfile
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_defs_none query cilfile =
  match query.tar with
  | Name_t name -> FuncFunction.find_def name (-1) cilfile
  | ID_t id -> FuncFunction.find_def "" id cilfile
  | All_t -> FuncFunction.find_def_all cilfile
  | Or_t list ->
      List.flatten
        (List.map (fun x -> FuncFunction.find_def x (-1) cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_defs query cilfile =
  match query.str with
  | None_s -> resolve_query_fun_defs_none query cilfile
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_uses_none query cilfile =
  match query.tar with
  | Name_t name -> FuncFunction.find_uses name (-1) cilfile
  | ID_t id -> FuncFunction.find_uses "" id cilfile
  | All_t -> FuncFunction.find_uses_all cilfile
  | And_t list ->
      and_several_lists
        (List.map (fun x -> FuncFunction.find_uses x (-1) cilfile) list)
  | Or_t list ->
      List.flatten
        (List.map (fun x -> FuncFunction.find_uses x (-1) cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_uses_fun query cilfile funname =
  match query.tar with
  | Name_t name -> FuncFunction.find_uses_in_fun name (-1) funname cilfile
  | ID_t id -> FuncFunction.find_uses_in_fun "" id funname cilfile
  | All_t -> FuncFunction.find_uses_in_fun_all funname cilfile
  | And_t list ->
      and_several_lists
        (List.map
           (fun x -> FuncFunction.find_uses_in_fun x (-1) funname cilfile)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncFunction.find_uses_in_fun x (-1) funname cilfile)
           list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_uses_cond query cilfile =
  match query.tar with
  | Name_t name -> FuncFunction.find_uses_cond name (-1) cilfile
  | ID_t id -> FuncFunction.find_uses_cond "" id cilfile
  | All_t -> FuncFunction.find_uses_all cilfile
  | And_t list ->
      and_several_lists
        (List.map (fun x -> FuncFunction.find_uses_cond x (-1) cilfile) list)
  | Or_t list ->
      List.flatten
        (List.map (fun x -> FuncFunction.find_uses_cond x (-1) cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_uses_noncond query cilfile =
  match query.tar with
  | Name_t name -> FuncFunction.find_uses_noncond name (-1) cilfile
  | ID_t id -> FuncFunction.find_uses_noncond "" id cilfile
  | All_t -> FuncFunction.find_uses_noncond_all cilfile
  | And_t list ->
      and_several_lists
        (List.map (fun x -> FuncFunction.find_uses_noncond x (-1) cilfile) list)
  | Or_t list ->
      List.flatten
        (List.map (fun x -> FuncFunction.find_uses_noncond x (-1) cilfile) list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_uses query cilfile =
  match query.str with
  | None_s -> resolve_query_fun_uses_none query cilfile
  | Fun_s funname -> resolve_query_fun_uses_fun query cilfile funname
  | Cond_s -> resolve_query_fun_uses_cond query cilfile
  | NonCond_s -> resolve_query_fun_uses_noncond query cilfile

let resolve_query_fun_usesvar_fun query cilfile varname strucfunname =
  match query.tar with
  | Name_t funname ->
      FuncFunction.find_usesvar_in_fun funname (-1) strucfunname varname cilfile
  | ID_t id ->
      FuncFunction.find_usesvar_in_fun "" id strucfunname varname cilfile
  | All_t -> FuncFunction.find_usesvar_in_fun_all strucfunname varname cilfile
  | And_t list ->
      and_several_lists
        (List.map
           (fun x ->
             FuncFunction.find_usesvar_in_fun x (-1) strucfunname varname
               cilfile)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x ->
             FuncFunction.find_usesvar_in_fun x (-1) strucfunname varname
               cilfile)
           list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_usesvar_none query cilfile varname =
  match query.tar with
  | Name_t funname -> FuncFunction.find_usesvar funname (-1) varname cilfile
  | ID_t id -> FuncFunction.find_usesvar "" id varname cilfile
  | All_t -> FuncFunction.find_usesvar_all varname cilfile
  | And_t list ->
      and_several_lists
        (List.map
           (fun x -> FuncFunction.find_usesvar x (-1) varname cilfile)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncFunction.find_usesvar x (-1) varname cilfile)
           list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_usesvar_cond query cilfile varname =
  match query.tar with
  | Name_t funname ->
      FuncFunction.find_usesvar_cond funname (-1) varname cilfile
  | ID_t id -> FuncFunction.find_usesvar_cond "" id varname cilfile
  | All_t -> FuncFunction.find_usesvar_cond_all varname cilfile
  | And_t list ->
      and_several_lists
        (List.map
           (fun x -> FuncFunction.find_usesvar_cond x (-1) varname cilfile)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncFunction.find_usesvar_cond x (-1) varname cilfile)
           list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_usesvar_noncond query cilfile varname =
  match query.tar with
  | Name_t funname ->
      FuncFunction.find_usesvar_noncond funname (-1) varname cilfile
  | ID_t id -> FuncFunction.find_usesvar_noncond "" id varname cilfile
  | All_t -> FuncFunction.find_usesvar_noncond_all varname cilfile
  | And_t list ->
      and_several_lists
        (List.map
           (fun x -> FuncFunction.find_usesvar_noncond x (-1) varname cilfile)
           list)
  | Or_t list ->
      List.flatten
        (List.map
           (fun x -> FuncFunction.find_usesvar_noncond x (-1) varname cilfile)
           list)
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

let resolve_query_fun_usesvar query cilfile varname =
  match query.str with
  | Fun_s strucfunname ->
      resolve_query_fun_usesvar_fun query cilfile varname strucfunname
  | None_s -> resolve_query_fun_usesvar_none query cilfile varname
  | Cond_s -> resolve_query_fun_usesvar_cond query cilfile varname
  | NonCond_s -> resolve_query_fun_usesvar_noncond query cilfile varname

let resolve_query_fun query cilfile =
  match query.f with
  | Returns_f -> resolve_query_fun_return query cilfile
  | Defs_f -> resolve_query_fun_defs query cilfile
  | Uses_f -> resolve_query_fun_uses query cilfile
  | UsesWithVar_f varname -> resolve_query_fun_usesvar query cilfile varname
  | _ ->
      Printf.printf "Not supported.\n";
      [ ("", loc_default, "", -1) ]

(* Main mapping function *)
let map_query query cilfile =
  let tmp =
    if query.lim != None_c then
      Printf.printf
        "Constraint is not supported yet. This parameter will be ignored.\n"
    else ();
    match query.k with
    | Datatype_k -> resolve_query_datatype query cilfile
    | Var_k -> resolve_query_var query cilfile
    | Fun_k -> resolve_query_fun query cilfile
  in
  let hashtbl = Hashtbl.create (List.length tmp) in
  delete_duplicates tmp hashtbl
