open Cil
open Cabs2cil

(* Helper functions *)
let is_equal_varname_varid varinfo name id =
  if String.compare varinfo.vname name = 0 || varinfo.vid = id then true
  else false

let rec delete_elem list s =
  match list with
  | x :: xs ->
      if String.compare x s = 0 then delete_elem xs s else x :: delete_elem xs s
  | [] -> []

let rec delete_duplicates list tbl =
  match list with
  | x :: xs -> (
      match Hashtbl.find_opt tbl x with
      | None ->
          Hashtbl.add tbl x 1;
          x :: delete_duplicates xs tbl
      | Some _ -> delete_duplicates xs tbl )
  | [] -> []

let map_gfun f = function GFun (dec, loc) -> f dec loc | _ -> None

let map_gvar f = function
  | GVar (varinfo, initinfo, loc) -> f varinfo initinfo loc
  | _ -> None

let is_temporary id = Inthash.mem allTempVars id

let generate_func_loc_table cilfile =
  List.filter_map
    (map_gfun (fun dec loc -> Some (dec.svar.vname, loc.line)))
    cilfile.globals

let generate_globalvar_list cilfile =
  List.filter_map
    (map_gvar (fun varinfo _ _ -> Some varinfo.vname))
    cilfile.globals

let get_all_alphaconverted_in_fun varname funname cilfile =
  let fun_loc_table = generate_func_loc_table cilfile in
  let loc_start =
    snd
    @@ List.find (function x, _ -> String.compare x funname = 0) fun_loc_table
  in
  let rec iter_fun_loc list =
    match list with
    | (fname, _) :: xs ->
        if fname = funname then
          match xs with (_, line) :: _ -> line | [] -> max_int
        else iter_fun_loc xs
    | [] -> 0
  in
  let loc_end = iter_fun_loc fun_loc_table in
  let tmp =
    List.filter_map
      (function
        | EnvVar varinfo, loc when loc.line >= loc_start && loc.line < loc_end
          ->
            Some varinfo.vname
        | _ -> None)
      (Hashtbl.find_all environment varname)
  in
  delete_duplicates
    ( tmp
    @
    if
      List.exists
        (function x -> String.compare x varname = 0)
        (generate_globalvar_list cilfile)
    then [ varname ]
    else [] )
    (Hashtbl.create 30)

class var_search_in_expr varname varid loc result includeCallTmp : nopCilVisitor
  =
  object
    inherit nopCilVisitor

    method! vvrbl info =
      if
        is_equal_varname_varid info varname varid
        && (includeCallTmp || not (is_temporary info.vid))
      then
        result :=
          !result
          @ [
              ( info.vname,
                loc,
                String.trim (Pretty.sprint ~width:1 (d_type () info.vtype)),
                info.vid );
            ]
      else ();
      SkipChildren
  end

(* Finds a variable in an expression *)
let search_expression exp name loc varid includeCallTmp =
  let result = ref [] in
  let visitor = new var_search_in_expr name varid loc result includeCallTmp in
  ignore (visitCilExpr visitor exp);
  !result

(* Finds a variable in a lhost *)
let search_lhost host name loc varid includeCallTmp =
  match host with
  | Var info ->
      if
        is_equal_varname_varid info name varid
        && (includeCallTmp || not (is_temporary info.vid))
      then
        [
          ( info.vname,
            loc,
            String.trim (Pretty.sprint ~width:1 (d_type () info.vtype)),
            info.vid );
        ]
      else []
  | Mem exp -> search_expression exp name loc varid includeCallTmp

let rec search_offset os name loc varid includeCallTmp =
  match os with
  | NoOffset -> []
  | Field (_, offset) -> search_offset offset name loc varid includeCallTmp
  | Index (exp, offset) ->
      search_expression exp name loc varid includeCallTmp
      @ search_offset offset name loc varid includeCallTmp

(* Finds a variable in a list of expressions *)
let rec search_expression_list list name loc varid includeCallTmp =
  match list with
  | x :: xs ->
      search_expression x name loc varid includeCallTmp
      @ search_expression_list xs name loc varid includeCallTmp
  | [] -> []

(* Finds a variable in a list of instructions *)
let rec search_instr_list_for_var list name varid includeCallTmp =
  match list with
  | Set ((lhost, offset), exp, loc) :: xs ->
      search_lhost lhost name loc varid includeCallTmp
      @ search_offset offset name loc varid includeCallTmp
      @ search_expression exp name loc varid includeCallTmp
      @ search_instr_list_for_var xs name varid includeCallTmp
  | VarDecl (info, loc) :: xs ->
      ( match info.vtype with
      | TArray (_, Some exp, _) ->
          search_expression exp name loc varid includeCallTmp
      | _ -> [] )
      @
      if
        is_equal_varname_varid info name varid
        && (includeCallTmp || not (is_temporary info.vid))
      then
        ( info.vname,
          loc,
          String.trim (Pretty.sprint ~width:1 (d_type () info.vtype)),
          info.vid )
        :: search_instr_list_for_var xs name varid includeCallTmp
      else search_instr_list_for_var xs name varid includeCallTmp
  | Call (Some (lhost, offset), exp, exp_list, loc) :: xs ->
      search_lhost lhost name loc varid includeCallTmp
      @ search_offset offset name loc varid includeCallTmp
      @ search_expression exp name loc varid includeCallTmp
      @ search_expression_list exp_list name loc varid includeCallTmp
      @ search_instr_list_for_var xs name varid includeCallTmp
  | Call (None, exp, exp_list, loc) :: xs ->
      search_expression exp name loc varid includeCallTmp
      @ search_expression_list exp_list name loc varid includeCallTmp
      @ search_instr_list_for_var xs name varid includeCallTmp
  (* Should I consider Asm too? *)
  | _ :: xs -> search_instr_list_for_var xs name varid includeCallTmp
  | [] -> []

(* Finds a variable in a list of statements *)
let rec search_stmt_list_for_var list name varid includeCallTmp =
  match list with
  | x :: xs ->
      ( match x.skind with
      | Instr ins_list ->
          search_instr_list_for_var ins_list name varid includeCallTmp
      | Return (Some exp, loc) ->
          search_expression exp name loc varid includeCallTmp
      | Goto (s_ref, _) ->
          search_stmt_list_for_var [ !s_ref ] name varid includeCallTmp
      | ComputedGoto (exp, loc) ->
          search_expression exp name loc varid includeCallTmp
      | If (exp, b1, b2, loc) ->
          search_expression exp name loc varid includeCallTmp
          @ search_stmt_list_for_var b1.bstmts name varid includeCallTmp
          @ search_stmt_list_for_var b2.bstmts name varid includeCallTmp
      | Switch (exp, _, stmt_list, loc) ->
          search_expression exp name loc varid includeCallTmp
          @ search_stmt_list_for_var stmt_list name varid includeCallTmp
      | Loop (block, _, None, None) ->
          search_stmt_list_for_var block.bstmts name varid includeCallTmp
      | Loop (block, _, None, Some s2) ->
          search_stmt_list_for_var block.bstmts name varid includeCallTmp
          @ search_stmt_list_for_var [ s2 ] name varid includeCallTmp
      | Loop (block, _, Some s1, None) ->
          search_stmt_list_for_var block.bstmts name varid includeCallTmp
          @ search_stmt_list_for_var [ s1 ] name varid includeCallTmp
      | Loop (block, _, Some s1, Some s2) ->
          search_stmt_list_for_var block.bstmts name varid includeCallTmp
          @ search_stmt_list_for_var [ s1 ] name varid includeCallTmp
          @ search_stmt_list_for_var [ s2 ] name varid includeCallTmp
      | Block block ->
          search_stmt_list_for_var block.bstmts name varid includeCallTmp
      | TryFinally (b1, b2, _) ->
          search_stmt_list_for_var b1.bstmts name varid includeCallTmp
          @ search_stmt_list_for_var b2.bstmts name varid includeCallTmp
      | TryExcept (b1, (instr_list, exp), b2, loc) ->
          search_stmt_list_for_var b1.bstmts name varid includeCallTmp
          @ search_instr_list_for_var instr_list name varid includeCallTmp
          @ search_expression exp name loc varid includeCallTmp
          @ search_stmt_list_for_var b2.bstmts name varid includeCallTmp
      | _ -> [] )
      @ search_stmt_list_for_var xs name varid includeCallTmp
  | [] -> []

(* Finds all uses of a variable in a function-body *)
let find_uses_in_fun_var dec name varid includeCallTmp cilfile =
  if varid != -1 then
    search_stmt_list_for_var dec.sbody.bstmts name varid includeCallTmp
  else
    let list = get_all_alphaconverted_in_fun name dec.svar.vname cilfile in
    List.flatten
    @@ List.map
         (fun x ->
           search_stmt_list_for_var dec.sbody.bstmts x (-1) includeCallTmp)
         list

(* Finds the function in which a variable shall be found *)
let find_uses_in_fun_find_fun list name varname varid includeCallTmp cilfile =
  let r =
    List.find_opt
      (function
        | GFun (dec, _) -> String.compare dec.svar.vname name = 0 | _ -> false)
      list
  in
  match r with
  | Some (GFun (dec, _)) ->
      find_uses_in_fun_var dec varname varid includeCallTmp cilfile
  | _ -> []

(* Finds all uses of a variable in a function *)
let find_uses_in_fun varname varid funname file includeCallTmp =
  find_uses_in_fun_find_fun file.globals funname varname varid includeCallTmp
    file

let find_all_glob_vars list =
  List.filter_map (map_gvar (fun info _ _ -> Some info.vid)) list

(* Finds all uses of all global variables in a function *)
let find_uses_in_fun_all_glob funname file includeCallTmp =
  let id_list = find_all_glob_vars file.globals in
  List.flatten
  @@ List.map
       (fun x -> find_uses_in_fun "" x funname file includeCallTmp)
       id_list

let find_fundec globals funname =
  let r =
    List.find_opt
      (function
        | GFun (dec, _) -> String.compare dec.svar.vname funname = 0
        | _ -> false)
      globals
  in
  match r with Some (GFun (dec, _)) -> Some dec | _ -> None

(* Finds all uses of all variables in a function *)
let find_uses_in_fun_all funname file includeCallTmp =
  let flat l =
    List.flatten
    @@ List.map (fun x -> find_uses_in_fun "" x funname file includeCallTmp) l
  in
  match find_fundec file.globals funname with
  | None -> []
  | Some fundec ->
      find_uses_in_fun_all_glob funname file includeCallTmp
      @ flat (List.map (fun x -> x.vid) fundec.sformals)
      @ flat (List.map (fun x -> x.vid) fundec.slocals)

let find_var_in_globals varname varid list =
  let r =
    List.find_opt
      (function
        | GVar (info, _, _) -> is_equal_varname_varid info varname varid
        | _ -> false)
      list
  in
  match r with
  | Some (GVar (info, _, loc)) ->
      [
        ( info.vname,
          loc,
          String.trim (Pretty.sprint ~width:1 (d_type () info.vtype)),
          info.vid );
      ]
  | _ -> []

(* Find all uses of a variable in all functions *)
let find_uses varname varid file includeCallTmp =
  let uses_in_all_fun =
    List.flatten
    @@ List.filter_map
         (map_gfun (fun dec _ ->
              Some
                (find_uses_in_fun varname varid dec.svar.vname file
                   includeCallTmp)))
         file.globals
  in
  find_var_in_globals varname varid file.globals @ uses_in_all_fun

(* Finds all uses of global variables in all functions *)
let find_uses_all_glob file includeCallTmp =
  let res =
    List.flatten
    @@ List.filter_map
         (map_gfun (fun dec _ ->
              Some
                (find_uses_in_fun_all_glob dec.svar.vname file includeCallTmp)))
         file.globals
  in
  List.flatten
    (List.map
       (fun x -> find_var_in_globals "" x file.globals)
       (find_all_glob_vars file.globals))
  @ res

(* Finds uses of all variables in all functions *)
let find_uses_all file includeCallTmp =
  let res =
    List.flatten
    @@ List.filter_map
         (map_gfun (fun dec _ ->
              Some (find_uses_in_fun_all dec.svar.vname file includeCallTmp)))
         file.globals
  in
  List.flatten
    (List.map
       (fun x -> find_var_in_globals "" x file.globals)
       (find_all_glob_vars file.globals))
  @ res

let rec cond_search_uses_stmt_list list varname varid includeCallTmp =
  match list with
  | x :: xs ->
      ( match x.skind with
      | If (exp, b1, b2, loc) ->
          search_expression exp varname loc varid includeCallTmp
          @ cond_search_uses_stmt_list (b1.bstmts @ b2.bstmts) varname varid
              includeCallTmp
      | Switch (exp, block, _, loc) ->
          search_expression exp varname loc varid includeCallTmp
          @ cond_search_uses_stmt_list block.bstmts varname varid includeCallTmp
      | Loop (block, _, None, None) ->
          cond_search_uses_stmt_list block.bstmts varname varid includeCallTmp
      | Loop (block, _, None, Some s1) ->
          cond_search_uses_stmt_list (s1 :: block.bstmts) varname varid
            includeCallTmp
      | Loop (block, _, Some s2, None) ->
          cond_search_uses_stmt_list (s2 :: block.bstmts) varname varid
            includeCallTmp
      | Loop (block, _, Some s2, Some s1) ->
          cond_search_uses_stmt_list (s2 :: s1 :: block.bstmts) varname varid
            includeCallTmp
      | Block block ->
          cond_search_uses_stmt_list block.bstmts varname varid includeCallTmp
      | TryFinally (b1, b2, _) ->
          cond_search_uses_stmt_list (b1.bstmts @ b2.bstmts) varname varid
            includeCallTmp
      | TryExcept (b1, _, b2, _) ->
          cond_search_uses_stmt_list (b1.bstmts @ b2.bstmts) varname varid
            includeCallTmp
      | _ -> [] )
      @ cond_search_uses_stmt_list xs varname varid includeCallTmp
  | [] -> []

(* Finds all uses of a variable in conditions of a function *)
let find_uses_in_cond_in_fun varname varid funname file includeCallTmp =
  match find_fundec file.globals funname with
  | None -> []
  | Some fundec ->
      if varid != -1 then
        cond_search_uses_stmt_list fundec.sbody.bstmts varname varid
          includeCallTmp
      else
        List.flatten
        @@ List.map
             (fun x ->
               cond_search_uses_stmt_list fundec.sbody.bstmts x (-1)
                 includeCallTmp)
             (get_all_alphaconverted_in_fun varname funname file)

(* Finds all uses of a variable in conditions in all functions *)
let find_uses_in_cond varname varid file includeCallTmp =
  let rec iter_functions list =
    match list with
    | GFun (dec, _) :: xs ->
        find_uses_in_cond_in_fun varname varid dec.svar.vname file
          includeCallTmp
        @ iter_functions xs
    | _ :: xs -> iter_functions xs
    | [] -> []
  in
  iter_functions file.globals

(* Finds all uses of global variables in conditions in all functions *)
let find_uses_in_cond_all_glob file includeCallTmp =
  let id_list = find_all_glob_vars file.globals in
  List.flatten
  @@ List.map (fun x -> find_uses_in_cond "" x file includeCallTmp) id_list

(* Finds all uses of global variables in conditions in a function *)
let find_uses_in_cond_in_fun_all_glob funname file includeCallTmp =
  let id_list = find_all_glob_vars file.globals in
  List.flatten
  @@ List.map
       (fun x -> find_uses_in_cond_in_fun "" x funname file includeCallTmp)
       id_list

(* Finds all uses of variables in conditions in a function *)
let find_uses_in_cond_in_fun_all funname file includeCallTmp =
  let get_formals_locals dec = dec.sformals @ dec.slocals in
  let fundec_opt = find_fundec file.globals funname in
  match fundec_opt with
  | None -> []
  | Some fundec ->
      find_uses_in_cond_in_fun_all_glob funname file includeCallTmp
      @ List.flatten
      @@ List.map
           (fun x ->
             find_uses_in_cond_in_fun x.vname (-1) funname file includeCallTmp)
           (get_formals_locals fundec)

(* Finds all uses of variables in conditions in all functions *)
let find_uses_in_cond_all file includeCallTmp =
  List.flatten
  @@ List.filter_map
       (map_gfun (fun dec _ ->
            Some
              (find_uses_in_cond_in_fun_all dec.svar.vname file includeCallTmp)))
       file.globals

let rec remove_result list res =
  match list with
  | x :: xs ->
      if x = res then remove_result xs res else x :: remove_result xs res
  | [] -> []

(* Finds all uses of a variable in non-conditions *)
let find_uses_in_noncond varname varid file includeCallTmp =
  let no_struc_result = find_uses varname varid file includeCallTmp in
  let cond_result = find_uses_in_cond varname varid file includeCallTmp in
  List.filter (fun x -> not (List.mem x cond_result)) no_struc_result

(* Finds all uses of global variables in non-conditions *)
let find_uses_in_noncond_all_glob file includeCallTmp =
  let id_list = find_all_glob_vars file.globals in
  List.flatten
  @@ List.map (fun x -> find_uses_in_noncond "" x file includeCallTmp) id_list

(* Finds all uses of variables in non-conditions *)
let find_uses_in_noncond_all file includeCallTmp =
  let no_struc_result = find_uses_all file includeCallTmp in
  let cond_result = find_uses_in_cond_all file includeCallTmp in
  List.filter (fun x -> not (List.mem x cond_result)) no_struc_result

(* Finds the declaration of a variable in a function *)
let find_decl_in_fun varname varid funname file =
  let get_formals_locals dec = dec.sformals @ dec.slocals in
  let iter_list_name list name =
    List.filter_map
      (fun x ->
        if String.compare x.vname name = 0 && not (is_temporary x.vid) then
          Some
            ( x.vname,
              x.vdecl,
              String.trim (Pretty.sprint ~width:1 (d_type () x.vtype)),
              x.vid )
        else None)
      list
  in
  let iter_namelist name_list varinfo_list =
    List.flatten @@ List.map (fun x -> iter_list_name varinfo_list x) name_list
  in
  match find_fundec file.globals funname with
  | None -> []
  | Some fundec ->
      if varid != -1 then
        List.filter_map
          (fun x ->
            if x.vid = varid then
              Some
                ( x.vname,
                  x.vdecl,
                  String.trim (Pretty.sprint ~width:1 (d_type () x.vtype)),
                  x.vid )
            else None)
          (get_formals_locals fundec)
      else
        iter_namelist
          (get_all_alphaconverted_in_fun varname funname file)
          (get_formals_locals fundec)

(* Finds all declarations in a function *)
let find_decl_in_fun_all funname file =
  match find_fundec file.globals funname with
  | None -> []
  | Some fundec ->
      List.map
        (fun x ->
          ( x.vname,
            x.vdecl,
            String.trim (Pretty.sprint ~width:1 (d_type () x.vtype)),
            x.vid ))
        (fundec.sformals @ fundec.slocals)

(* Finds all global variable declarations *)
let find_decl_all_glob file =
  List.filter_map
    (map_gvar (fun info _ loc ->
         Some
           ( info.vname,
             loc,
             String.trim (Pretty.sprint ~width:1 (d_type () info.vtype)),
             info.vid )))
    file.globals

(* Finds the declarations of a variable globally and in all functions  *)
let find_decl varname varid file =
  let rec iter_global_decls result =
    match result with
    | (name, loc, typ, id) :: xs ->
        if String.compare varname name = 0 || id = varid then
          [ (name, loc, typ, id) ]
        else iter_global_decls xs
    | [] -> []
  in
  let rec iter_functions globals =
    match globals with
    | GFun (dec, _) :: xs ->
        find_decl_in_fun varname varid dec.svar.vname file @ iter_functions xs
    | _ :: xs -> iter_functions xs
    | [] -> []
  in
  iter_global_decls (find_decl_all_glob file) @ iter_functions file.globals

(* Finds all declaration globally and in all functions *)
let find_decl_all file =
  let list =
    List.flatten
    @@ List.filter_map
         (map_gfun (fun dec _ ->
              Some (find_decl_in_fun_all dec.svar.vname file)))
         file.globals
  in
  find_decl_all_glob file @ list

class var_find_def_in_fun varname varid funname result : nopCilVisitor =
  object
    inherit nopCilVisitor

    method! vfunc fundec =
      if String.compare fundec.svar.vname funname = 0 then DoChildren
      else SkipChildren

    method! vinst instr =
      match instr with
      | Set ((Var info, _), _, loc) ->
          if is_equal_varname_varid info varname varid then (
            result :=
              !result
              @ [
                  ( info.vname,
                    loc,
                    String.trim (Pretty.sprint ~width:1 (d_type () info.vtype)),
                    info.vid );
                ];
            SkipChildren )
          else SkipChildren
      | _ -> SkipChildren
  end

(* Finds definitions of a variable in a function *)
let find_defs_in_fun varname varid funname file =
  let result = ref [] in
  let visitor = new var_find_def_in_fun varname varid funname result in
  if varid != -1 then (
    visitCilFileSameGlobals visitor file;
    !result )
  else
    let list = get_all_alphaconverted_in_fun varname funname file in
    List.iter
      (fun x ->
        visitCilFileSameGlobals
          (new var_find_def_in_fun x (-1) funname result)
          file)
      list;
    !result

(* Finds definitions of all global variables in a function *)
let find_defs_in_fun_all_glob funname file =
  List.flatten
  @@ List.filter_map
       (map_gvar (fun info _ _ ->
            Some (find_defs_in_fun "" info.vid funname file)))
       file.globals

(* Finds definitions of all variables in a functions *)
let find_defs_in_fun_all funname file =
  let fundec_opt = find_fundec file.globals funname in
  let get_formals_locals dec = dec.sformals @ dec.slocals in
  match fundec_opt with
  | None -> []
  | Some fundec ->
      find_defs_in_fun_all_glob funname file
      @ List.flatten
      @@ List.map
           (fun x -> find_defs_in_fun "" x.vid funname file)
           (get_formals_locals fundec)

(* Finds definitions of a variable in all functions *)
let find_defs varname varid file =
  let r =
    List.flatten
    @@ List.filter_map
         (map_gfun (fun dec _ ->
              Some (find_defs_in_fun varname varid dec.svar.vname file)))
         file.globals
  in
  find_var_in_globals varname varid file.globals @ r

(* Finds definitions of all global variables in all functions *)
let find_defs_all_glob file =
  List.flatten
    (List.map
       (fun x -> find_var_in_globals "" x file.globals)
       (find_all_glob_vars file.globals))
  @ List.flatten
  @@ List.filter_map
       (map_gfun (fun dec _ ->
            Some (find_defs_in_fun_all_glob dec.svar.vname file)))
       file.globals

(* Finds definitions of all variables in all functions *)
let find_defs_all file =
  List.flatten
    (List.map
       (fun x -> find_var_in_globals "" x file.globals)
       (find_all_glob_vars file.globals))
  @ List.flatten
  @@ List.filter_map
       (map_gfun (fun dec _ -> Some (find_defs_in_fun_all dec.svar.vname file)))
       file.globals
