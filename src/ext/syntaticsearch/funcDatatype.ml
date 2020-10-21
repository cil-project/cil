open Cil

(* Finds definition of a user-defined type *)
let find_def name file =
  List.filter_map
    (function
      | GType (info, loc) ->
          if String.compare name info.tname = 0 then Some ("", loc, name, -1)
          else None
      | GCompTag (info, loc) ->
          if String.compare name info.cname = 0 then Some ("", loc, name, -1)
          else None
      | GEnumTag (info, loc) ->
          if String.compare name info.ename = 0 then Some ("", loc, name, -1)
          else None
      | _ -> None)
    file.globals

(* Finds all definition of user-defined types *)
let find_def_all file =
  List.filter_map
    (function
      | GType (info, loc) -> Some ("", loc, info.tname, -1)
      | GCompTag (info, loc) -> Some ("", loc, info.cname, -1)
      | GEnumTag (info, loc) -> Some ("", loc, info.ename, -1)
      | _ -> None)
    file.globals

let find_in_globals list name =
  List.filter_map
    (function
      | GVar (info, _, _) ->
          if
            String.compare name
              (String.trim (Pretty.sprint ~width:1 (d_type () info.vtype)))
            = 0
          then Some info.vid
          else None
      | _ -> None)
    list

let find_in_varinfos list name =
  List.filter_map
    (fun info ->
      if
        String.compare name
          (String.trim (Pretty.sprint ~width:1 (d_type () info.vtype)))
        = 0
      then Some info.vid
      else None)
    list

let find_fundec globals name =
  let gfun =
    List.find_opt
      (function
        | GFun (fundec, _) -> String.compare fundec.svar.vname name = 0
        | _ -> false)
      globals
  in
  match gfun with Some (GFun (fundec, _)) -> Some fundec | _ -> None

let find_typevar_uses_in_fun list funname file =
  List.flatten
  @@ List.map (fun x -> FuncVar.find_uses_in_fun "" x funname file false) list

(* Finds uses of a datatype in a function *)
let find_uses_in_fun typename funname file =
  match find_fundec file.globals funname with
  | None -> []
  | Some f ->
      find_typevar_uses_in_fun
        ( find_in_globals file.globals typename
        @ find_in_varinfos f.slocals typename
        @ find_in_varinfos f.sformals typename )
        f.svar.vname file

(* Finds all uses of a datatype in all functions *)
let find_uses typename file =
  let list = FuncVar.find_uses_all file false in
  List.filter (fun (_, _, typ, _) -> String.compare typ typename = 0) list

(* Finds all uses of a datatype in conditions *)
let find_uses_in_cond typename file =
  let list = FuncVar.find_uses_in_cond_all file false in
  List.filter (fun (_, _, typ, _) -> String.compare typ typename = 0) list

(* Finds all uses of a datatype in non-conditions *)
let find_uses_in_noncond typename file =
  let list = FuncVar.find_uses_in_noncond_all file false in
  List.filter (fun (_, _, typ, _) -> String.compare typ typename = 0) list
