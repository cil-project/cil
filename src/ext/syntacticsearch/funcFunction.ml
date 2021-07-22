open Cil
open Cabs2cil

let is_equal_funname_funid varinfo name id =
  if String.compare varinfo.vname name = 0 || varinfo.vid = id then true
  else false

let rec delete_elem list s =
  match list with
  | x :: xs ->
      if String.compare x s = 0 then delete_elem xs s else x :: delete_elem xs s
  | [] -> []

let rec delete_duplicates list acc =
  match list with
  | x :: xs -> delete_duplicates (delete_elem xs x) (x :: acc)
  | [] -> acc

let map_gfun f = function GFun (dec, loc) -> f dec loc | _ -> None

let find_all_with_origname n =
  let ns = Hashtbl.find_all environment n in
  BatList.filter_map (function | (EnvVar v,_) -> Some v.vname | _ -> None) ns

class fun_find_returns funname funid result : nopCilVisitor =
  object
    inherit nopCilVisitor

    method! vfunc fundec =
      if is_equal_funname_funid fundec.svar funname funid then DoChildren
      else SkipChildren

    method! vstmt stmt =
      match stmt.skind with
      | Return (Some exp, loc) ->
          result :=
            !result
            @ [
                ( "",
                  loc,
                  String.trim (Pretty.sprint ~width:1 (d_type () (typeOf exp))),
                  -1 );
              ];
          DoChildren
      | Return (None, loc) ->
          result := !result @ [ ("", loc, "void", -1) ];
          DoChildren
      | _ -> DoChildren
  end

(* Finds all returns of a function *)
let find_returns funname funid file =
  let result = ref [] in
  let visitor = new fun_find_returns funname funid result in
  ignore (visitCilFileSameGlobals visitor file);
  !result

(* Finds all returns in all functions *)
let find_returns_all file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ -> Some (find_returns "" fundec.svar.vid file)))
       file.globals

class fun_find_sig funname funid result : nopCilVisitor =
  object
    inherit nopCilVisitor

    method! vfunc fundec =
      if is_equal_funname_funid fundec.svar funname funid then DoChildren
      else SkipChildren

    method! vstmt stmt =
      match stmt.skind with
      | Return (Some exp, loc) ->
          result :=
            !result
            @ [
                ( "",
                  loc,
                  String.trim (Pretty.sprint ~width:1 (d_type () (typeOf exp))),
                  -1 );
              ];
          SkipChildren
      | Return (None, loc) ->
          result := !result @ [ ("", loc, "void", -1) ];
          SkipChildren
      | _ -> DoChildren
  end

let create_sig fundec file =
  let result = ref [] in
  let return_type =
    match
      visitCilFileSameGlobals
        (new fun_find_sig fundec.svar.vname fundec.svar.vid result)
        file;
      !result
    with
    | (_, _, typ, _) :: _ -> typ
    | [] ->
        Printf.printf "This should never happen\n";
        ""
  in
  let rec input_type list =
    match list with
    | [ x ] ->
        String.trim (Pretty.sprint ~width:1 (d_type () x.vtype)) ^ " " ^ x.vname
    | x :: xs ->
        String.trim (Pretty.sprint ~width:1 (d_type () x.vtype))
        ^ " " ^ x.vname ^ ", " ^ input_type xs
    | [] -> ""
  in
  return_type ^ " " ^ fundec.svar.vname ^ " (" ^ input_type fundec.sformals
  ^ ")"

(* Finds all definitions of a function *)
let find_def funname funid file =
  let fn fundec loc =
    if is_equal_funname_funid fundec.svar funname funid then
      Some (fundec.svar.vname, loc, create_sig fundec file, fundec.svar.vid)
    else None
  in
  BatList.filter_map (map_gfun fn) file.globals

(* Finds all definitions of all functions *)
let find_def_all file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ -> Some (find_def "" fundec.svar.vid file)))
       file.globals

let find_fundec funname funid list =
  let gfun =
    BatList.find_opt
      (fun x ->
        match x with
        | GFun (dec, _) -> is_equal_funname_funid dec.svar funname funid
        | _ -> false)
      list
  in
  match gfun with Some (GFun (dec, _)) -> Some dec | _ -> None

class fun_find_uses funname funid file result : nopCilVisitor =
  object
    inherit nopCilVisitor

    method! vinst instr =
      match instr with
      | Call (_, Lval (Var varinfo, NoOffset), _, loc) ->
          if is_equal_funname_funid varinfo funname funid then (
            match find_fundec funname funid file.globals with
            | None -> SkipChildren
            | Some dec ->
                result :=
                  !result
                  @ [ (varinfo.vname, loc, create_sig dec file, varinfo.vid) ];
                SkipChildren )
          else SkipChildren
      | _ -> SkipChildren
  end

(* Finds all calls of a function in all functions *)
let find_uses funname funid file =
  let result = ref [] in
  let visitor = new fun_find_uses funname funid file result in
  ignore (visitCilFileSameGlobals visitor file);
  !result

(* Find all calls of all functions in all functions *)
let find_uses_all file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ -> Some (find_uses "" fundec.svar.vid file)))
       file.globals

class fun_find_uses_in_fun funname funid funstrucname file result :
  nopCilVisitor =
  object
    inherit nopCilVisitor

    method! vfunc fundec =
      if is_equal_funname_funid fundec.svar funstrucname (-1) then DoChildren
      else SkipChildren

    method! vinst instr =
      match instr with
      | Call (_, Lval (Var varinfo, NoOffset), _, loc) ->
          if is_equal_funname_funid varinfo funname funid then (
            match find_fundec funname funid file.globals with
            | None -> SkipChildren
            | Some dec ->
                result :=
                  !result
                  @ [ (varinfo.vname, loc, create_sig dec file, varinfo.vid) ];
                SkipChildren )
          else SkipChildren
      | _ -> SkipChildren
  end

(* Finds calls of a function in a function *)
let find_uses_in_fun funname funid funstrucname file =
  let result = ref [] in
  let visitor =
    new fun_find_uses_in_fun funname funid funstrucname file result
  in
  ignore (visitCilFileSameGlobals visitor file);
  !result

(* Finds all calls of all functions in a function *)
let find_uses_in_fun_all funstrucname file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ ->
            Some (find_uses_in_fun "" fundec.svar.vid funstrucname file)))
       file.globals

let loc_default = { line = -1; file = ""; byte = -1; column = -1 }

class fun_find_usesvar_in_fun fundec funstrucname varname varid file result :
  nopCilVisitor =
  object
    inherit nopCilVisitor

    method! vfunc dec =
      if is_equal_funname_funid dec.svar funstrucname (-1) then DoChildren
      else SkipChildren

    method! vinst instr =
      match instr with
      | Call (_, exp, list, loc) -> (
          match exp with
          | Lval (Var varinfo, _) ->
              if
                is_equal_funname_funid varinfo fundec.svar.vname fundec.svar.vid
              then
                if
                  List.length
                    (FuncVar.search_expression_list list varname loc_default
                       varid true)
                  > 0
                then (
                  result :=
                    !result
                    @ [
                        (varinfo.vname, loc, create_sig fundec file, varinfo.vid);
                      ];
                  SkipChildren )
                else SkipChildren
              else SkipChildren
          | _ -> SkipChildren )
      | _ -> SkipChildren
  end

(* Finds calls of a function with a var in argument in a function *)
let find_usesvar_in_fun funname funid funstrucname varname file =
  match find_fundec funname funid file.globals with
  | None -> []
  | Some fundec ->
      let result = ref [] in
      let dedup =
        delete_duplicates (find_all_with_origname varname) []
      in
      List.iter
        (fun x ->
          visitCilFileSameGlobals
            (new fun_find_usesvar_in_fun fundec funstrucname x (-1) file result)
            file)
        dedup;
      !result

(* Finds calls of all function with a var in argument in a function *)
let find_usesvar_in_fun_all funstrucname varname file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ ->
            Some
              (find_usesvar_in_fun "" fundec.svar.vid funstrucname varname file)))
       file.globals

(* Finds all calls of a function with a var in argument in all functions *)
let find_usesvar funname funid varname file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ ->
            Some
              (find_usesvar_in_fun funname funid fundec.svar.vname varname file)))
       file.globals

(* Finds all calls of all functions with a var in argument in all functions *)
let find_usesvar_all varname file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ ->
            Some (find_usesvar "" fundec.svar.vid varname file)))
       file.globals

let is_temporary id = Inthash.mem allTempVars id

class find_calls_with_tmp result funname funid : nopCilVisitor =
  object
    inherit nopCilVisitor

    method! vinst instr =
      match instr with
      | Call (lval_opt, Lval (Var varinfo, _), _, _) ->
          if is_equal_funname_funid varinfo funname funid then
            match lval_opt with
            | Some (Var tmpinfo, _) ->
                if is_temporary tmpinfo.vid then
                  result := !result @ [ (tmpinfo.vid, varinfo.vid) ];
                SkipChildren
            | _ -> SkipChildren
          else SkipChildren
      | _ -> SkipChildren
  end

let find_lval_of_calls funname funid file =
  let result = ref [] in
  let visitor = new find_calls_with_tmp result funname funid in
  visitCilFileSameGlobals visitor file;
  !result

let create_fun_res name id file loc =
  let fundec_opt = find_fundec name id file.globals in
  match fundec_opt with
  | None -> ("", loc_default, "", -1)
  | Some fundec ->
      (fundec.svar.vname, loc, create_sig fundec file, fundec.svar.vid)

(* Finds all calls of a function in a condition in all functions *)
let find_uses_cond funname funid file =
  let id_list = find_lval_of_calls funname funid file in
  BatList.filter_map
    (fun (tmp, func) ->
      match FuncVar.find_uses_in_cond "" tmp file true with
      | (_, loc, _, _) :: _ -> Some (create_fun_res "" func file loc)
      | _ -> None)
    id_list

(* Finds all calls of all functions in a condition in all functions *)
let find_uses_cond_all file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ -> Some (find_uses_cond "" fundec.svar.vid file)))
       file.globals

(* Finds calls of a function in non-condition in all functions *)
let find_uses_noncond funname funid file =
  let uses_cond = find_uses_cond funname funid file in
  let all_uses = find_uses funname funid file in
  List.filter (fun x -> not (List.mem x uses_cond)) all_uses

(* Finds calls of all functions in non-condition in all functions *)
let find_uses_noncond_all file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ ->
            Some (find_uses_noncond "" fundec.svar.vid file)))
       file.globals

class find_calls_usesvar_with_tmp result funname funid varname : nopCilVisitor =
  object
    inherit nopCilVisitor

    method! vinst instr =
      match instr with
      | Call (lval_opt, Lval (Var varinfo, _), arg_list, loc) ->
          if
            is_equal_funname_funid varinfo funname funid
            && List.length
                 (List.flatten
                    (List.map
                       (fun x ->
                         FuncVar.search_expression_list arg_list x loc (-1) true)
                       (find_all_with_origname varname)))
               > 0
          then
            match lval_opt with
            | Some (Var tmpinfo, _) ->
                if
                  String.length tmpinfo.vname > 2
                  && String.compare "tmp" (String.sub tmpinfo.vname 0 3) = 0
                then result := !result @ [ (tmpinfo.vid, varinfo.vid) ];
                SkipChildren
            | _ -> SkipChildren
          else SkipChildren
      | _ -> SkipChildren
  end

let find_lval_of_calls_usesvar funname funid varname file =
  let result = ref [] in
  let visitor = new find_calls_usesvar_with_tmp result funname funid varname in
  visitCilFileSameGlobals visitor file;
  !result

(* Finds calls of a function with a variable as argument in conditions *)
let find_usesvar_cond funname funid varname file =
  let id_list = find_lval_of_calls_usesvar funname funid varname file in
  BatList.filter_map
    (fun (tmp, func) ->
      match FuncVar.find_uses_in_cond "" tmp file true with
      | (_, loc, _, _) :: _ -> Some (create_fun_res "" func file loc)
      | _ -> None)
    id_list

(* Finds calls of all functions with a variable as argument in conditions *)
let find_usesvar_cond_all varname file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ ->
            Some (find_usesvar_cond "" fundec.svar.vid varname file)))
       file.globals

(* Finds calls of a function with a variable as argument in non-conditions *)
let find_usesvar_noncond funname funid varname file =
  let uses_cond = find_usesvar_cond funname funid varname file in
  let all_uses = find_usesvar funname funid varname file in
  List.filter (fun x -> not (List.mem x uses_cond)) all_uses

(* Finds calls of all functions with a variable as argument in non-conditions *)
let find_usesvar_noncond_all varname file =
  List.flatten
  @@ BatList.filter_map
       (map_gfun (fun fundec _ ->
            Some (find_usesvar_noncond "" fundec.svar.vid varname file)))
       file.globals
