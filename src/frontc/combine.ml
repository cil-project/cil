(* combine --- combine C source files into one.
**
** Project: FrontC
** File:  combine.ml
** Version: 1.0 (CABS To CABS)
** Date:  4.10.01
** Author:  Raymond To
**
**  1.0   4.10.01 Raymond To First version.
**  2.0   7.1.01 George Necula. Reimplemented with proper handling of scope 
**        and alpha-conversion.

*)

open Cabs
open Trace
open Pretty
module E = Errormsg
module H = Hashtbl


(* We get from the parser a list of files. We concatenate them together being 
 * carefully to rename file-scope symbols (static, typedef, 
 * structure/union/enum tags appearing at file scope) and then we make one 
 * pass over the entiere AST to apply the new names. We also eliminate 
 * certain duplicate file-scope definitions.  *)

(* 1. Non-static declarations are never renamed. But we try to remove 
 * duplicate ones. We check duplication by using Ocaml structural equality by 
 * we ignore the location of the declaration. *)

(* 2. Static declarations and function defintions are renamed (but only once 
 * per file) without effort to try to eliminate duplicate definitions  *)

(* 3. Struct/Union/Enum tags are considered to have file scope also, but 
 * before they are renamed we check to see if this is a duplicate definition 
 * or maybe a conflicting definition. We keep track of all the definitions 
 * for a given tag so when we check for duplicates we check against all 
 * previous definitions for a tag.  *)

(* 4. Typedefs are also considered to have file scope but we try to reuse 
 * them. We use a similar mechanism as for the struct/union/enum tags *)






(* Global names are split into a prefix, a ___ (3 underscores) separator and 
 * an integer suffix. For each prefix we keep the maximum integer suffix with 
 * which the prefix was encountered. This suffix is -1 when only the version 
 * without the suffix was encountered.  *)
let alphaTable: (string, int ref) H.t = H.create 511


(* To keep different name scopes different, we add prefixes to names 
 * specifying the kind of name: the kind can be one of "" for variables or 
 * enum tags, "struct" for structures, "enum" for enumerations, "union" for 
 * union types, or "type" for types *)
let kindPlusName (kind: string)
                 (origname: string) : string =
  if kind = "" then origname else
  kind ^ " " ^ origname
                

let stripKind (kind: string) (kindplusname: string) : string = 
  let l = 1 + String.length kind in
  if l > 1 then 
    String.sub kindplusname l (String.length kindplusname - l)
  else
    kindplusname

(* Keep track of the current location *)
let currentLoc: cabsloc ref = ref { lineno = -1; filename = "<no file>" }

(* Keep an environment that maps a name to the current alpha-converted name 
 * in the current scope. This gets flushed at the end of each file. If an 
 * string is not present in the environment then it is an external 
 * and we leave it alone. *)
let env: (string, string) H.t = H.create 511


(* A list of things to do when you exit a scope *)
type undoElement = 
    UndoScope (* To mark the begining of a scope *)
  | UndoThis of (unit -> unit)

let undo: undoElement list ref = ref []

let enterScope () = 
  undo := UndoScope :: !undo

let exitScope () = 
  let rec loop = function
      [] -> E.s (E.bug "Empty scope")
    | UndoScope :: rest -> undo := rest
    | UndoThis f :: rest -> f(); loop rest
  in
  loop !undo


(* Add a local to the environment and push an undo entry *)
let addLocalToEnv (orig: string) (newname: string) = 
  H.add env orig newname;
  undo := UndoThis (fun _ -> H.remove env orig) :: !undo


let lookup (kind: string) (origname: string) : string = 
  let lookupname = kindPlusName kind origname in
  try
    let alphaname = H.find env lookupname in
    alphaname
  with Not_found -> origname




(* The main alpha conversion routine *)
let rec newAlphaName (kind: string) 
                     (origname: string) : string = 
  let lookupname = kindPlusName kind origname in
  (* Copied from CIL *)
  let prefix, _, suffix = splitNameForAlpha lookupname in
  (* ignore (E.log "newAlphaName(%s). P=%s, S=%d\n" lookupname prefix suffix);
     *)
  let newname = 
    try
      let rc = H.find alphaTable prefix in
      let newsuffix = if suffix > !rc then suffix else !rc + 1 in
      rc := newsuffix;
      prefix ^ "___" ^ (string_of_int newsuffix)
    with Not_found -> begin (* First variable with this prefix *)
      H.add alphaTable prefix (ref suffix);
      lookupname  (* Return the original name *)
    end
  in
  let n' = stripKind kind newname in
  n'




(* Strip the suffix. Return the prefix, the separator (empty or ___) and a 
 * numeric suffix (-1 if the separator is empty or if ____ is the last thing 
 * in the name)  *)
and splitNameForAlpha (lookupname: string) : (string * string * int) = 
  (* Split the lookup name into a prefix, a separator (empty or ___) and a 
   * suffix. The suffix is numberic and is separated by ___ from the prefix  *)
  try
    let under_idx = String.rindex lookupname '_' in
    let l = String.length lookupname in
    (* Check that we have two more underscores preceeding it *)
    if under_idx < 3 then raise Not_found;
    if String.get lookupname (under_idx - 1) != '_' ||
       String.get lookupname (under_idx - 2) != '_' then 
      raise Not_found;
    (* Check that we have only digits following the underscore *)
    if under_idx = l - 1 then raise Not_found;
    (* If we have a 0 right after the _ and more characters after that then 
     * we consider that we do not have a suffix *)
    if String.get lookupname (under_idx + 1) = '0' &&
       under_idx < l - 2 then raise Not_found;
    let rec collectSuffix (acc: int) (i: int) = 
      if i = l then 
        (String.sub lookupname 0 (under_idx - 2), "___", acc)
      else
        let c = Char.code (String.get lookupname i) - Char.code '0' in
        if c >= 0 && c < 9 then 
          collectSuffix (10 * acc + c) (i + 1)
        else
          raise Not_found
    in
    collectSuffix 0 (under_idx + 1)
  with Not_found -> (* No suffix in the name *)
    (lookupname, "", -1)

(* Sometimes we have names that we do not want to change. We need to register 
 * them however for two reasons. First, they might be in conflict with some 
 * local names already registered. In that case we must add them to the 
 * conflicts list to be fixed in a second pass over the files. Another reason 
 * is that we want to register them into the alphaTable so that new locals 
 * that we create don't inadvertedly reuse the name *)
let conflicts: string list ref = ref []
let registeredGlobals: (string, bool) H.t = H.create 511
let registerGlobalName (n: string) = 
  if not (H.mem registeredGlobals n) then begin
    H.add registeredGlobals n true;
    (* Put it through the alpha converter to register its prefix and to see 
     * if there is already somebody else with this name *)
    let n' = newAlphaName "" n in
    if n <> n' then 
      conflicts := n :: !conflicts;
  end

let processDeclName 
    (isglobal: bool)
    (isstatic: bool)
    (orig: string) : string = 
  if isglobal then 
    if isstatic then begin (* Can change name. But do it only once in a 
                            * scope. *)
      try
        H.find env orig 
      with Not_found -> begin
        let n' = newAlphaName "" orig in
        (* Add it to the env *)
        addLocalToEnv orig n';
        n'
      end
    end else (* Not static. Register the name *) begin
      registerGlobalName orig;
      orig
    end
  else begin (* local name. Add it to the env *)
    addLocalToEnv orig orig;
    orig
  end

(* --------------------- equality functions -------------------- *)

(* Generic comparison of lists *)
let rec equal_lists (eqone: 'a -> 'a -> bool) 
                (l1: 'a list)
                (l2: 'a list) = 
    match l1, l2 with
      [], [] -> true
    | h1 :: t1, h2 :: t2 -> eqone h1 h2 && equal_lists eqone t1 t2
    | _ -> false


(* sm: fairly detailed (though still not complete) comparison of
 * declarations for the purpose of detecting inconsistency between
 * different source files; I'm sure this kind of comparison must
 * have already been written by someone somewhere, but I can't
 * find it *)
let rec equal_specs (tagequal: bool) (* Trust tag equality *)
                    (s1 : spec_elem) (s2 : spec_elem) : bool =
begin
  match (s1,s2) with
  | SpecTypedef, SpecTypedef -> true
  | SpecInline, SpecInline -> true
  | SpecAttr _, SpecAttr _ -> true               (* good enough for now *)
  | SpecStorage _, SpecStorage _ -> true         (* don't care right now *)
  | SpecType(t1), SpecType(t2) -> 
      (equal_typespecs tagequal t1 t2) (* the point of this exercise  *)
  | _, _ -> false                                (* mismatching kinds *)
end

and equal_typespecs 
         (tagequal: bool) (* If true then trust tag equality *)
         (t1 : typeSpecifier) (t2 : typeSpecifier) : bool =
begin
  match (t1,t2) with
  | Tvoid, Tvoid -> true
  | Tchar, Tchar -> true
  | Tshort, Tshort -> true
  | Tint, Tint -> true
  | Tlong, Tlong -> true
  | Tint64, Tint64 -> true
  | Tfloat, Tfloat -> true
  | Tdouble, Tdouble -> true
  | Tsigned, Tsigned -> true
  | Tunsigned, Tunsigned -> true
  | Tnamed(n1), Tnamed(n2) -> (n1 = n2)
  | Tstruct(n1, None), Tstruct(n2, None) -> (n1 = n2)
  | Tstruct(n1, Some fields1), Tstruct(n2, Some fields2) ->
      n1 = n2 &&
      (tagequal || (equal_field_group_lists fields1 fields2))
  | Tstruct (n1, _), Tstruct(n2, _) -> tagequal && n1 = n2

  | Tunion(n1, None), Tunion(n2, None) -> (n1 = n2)
  | Tunion(n1, Some fields1), Tunion(n2, Some fields2) ->
      n1 = n2 &&
      (tagequal || (equal_field_group_lists fields1 fields2))
  | Tunion (n1, _), Tunion(n2, _) -> tagequal && n1 = n2

  | Tenum(n1, None), Tenum(n2, None) -> (n1 = n2)
  | Tenum(n1, Some fields1), Tenum(n2, Some fields2) ->
      n1 = n2 &&
      (tagequal || (equal_enum_item_lists fields1 fields2))
  | Tenum (n1, _), Tenum(n2, _) -> tagequal && n1 = n2


  (* yikes because we need to compare a bunch more details, and
   * yikes because one type might be written explicitly and
   * another as the type itself, so resolving names is needed.. *)
  | TtypeofE(e1), TtypeofE(e2) -> e1 = e2
  | TtypeofT(s1,t1), TtypeofT(s2,t2) -> 
      equal_spec_lists true s1 s2 && equal_decltypes t1 t2

  | _,_ -> false
end


and equal_enum_item_lists (fields1 : enum_item list)
                          (fields2 : enum_item list) : bool =
  equal_lists 
    (fun (tag1, e1) (tag2, e2) -> tag1 = tag2 && e1 = e2)
    fields1
    fields2
(*
begin
  match fields1, fields2 with
  | [], [] -> true
  | (tag1, e1) :: rest1, (tag2, e2) :: rest2 ->
       &&
      (equal_enum_item_lists rest1 rest2)

  | _, _ -> false
end
*)

and equal_field_group_lists (fields1 : field_group list)
                            (fields2 : field_group list) : bool =
  equal_lists
    (fun (specs1, flds1) (specs2, flds2) ->
      equal_spec_lists true specs1 specs2 &&
      equal_field_lists flds1 flds2)
    fields1
    fields2
(*
begin
  match (fields1, fields2) with
  | [], [] -> true
  | (specs1, names1) :: rest1, (specs2, names2) :: rest2 -> (
      (equal_spec_lists true specs1 specs2) &&
      (equal_field_lists names1 names2) &&
      (equal_field_group_lists rest1 rest2)
    )          
  | _, _ -> false
end
*)
and equal_spec_lists (tagequal: bool)  (* Trust tag equality *) 
                     (specs1 : spec_elem list)
                     (specs2 : spec_elem list) : bool =
  equal_lists
    (fun s1 s2 -> equal_specs tagequal s1 s2)
    specs1
    specs2

(*
begin
  match (specs1, specs2) with
  | [], [] -> true
  | s1 :: rest1, s2 :: rest2 -> (
      (equal_specs tagequal s1 s2) &&
      (equal_spec_lists tagequal rest1 rest2)
    )
  | _, _ -> false
end
*)

and equal_name_lists (names1 : name list) 
                     (names2 : name list) : bool =
  equal_lists
    (fun n1 n2 -> equal_names n1 n2) 
    names1
    names2
(*
  match (names1, names2) with
  | [], [] -> true
  | n1 :: rest1, n2 :: rest2 -> (
      (equal_names n1 n2) &&
      (equal_name_lists rest1 rest2)
    )
  | _, _ -> false
*)

and equal_field_lists (flds1: (name * expression option) list)
                      (flds2: (name * expression option) list) =
  equal_lists
    (fun (n1, w1) (n2, w2) -> equal_names n1 n2 && w1 = w2)
    flds1
    flds2
                      
and equal_names (n1 : name) (n2 : name) : bool =
begin
  let (id1, dtype1, _) = n1 in
  let (id2, dtype2, _) = n2 in

  id1 = id2 &&      (* names equal *)
  (equal_decltypes dtype1 dtype2)

  (* don't check attributes... *)
end

and equal_decltypes (t1 : decl_type) (t2 : decl_type) : bool =
begin
  match t1,t2 with
  | JUSTBASE, JUSTBASE -> true
  | PARENTYPE(_, p1, _), PARENTYPE(_, p2, _) ->
      (* continuing to ignore attributes.. *)
      (equal_decltypes p1 p2)
  | ARRAY(elt1, sz1), ARRAY(elt2, sz2) ->
      sz1 = sz2 &&
      (equal_decltypes elt1 elt2)
  | PTR(_, base1), PTR(_, base2) ->
      (equal_decltypes base1 base2)
  | PROTO(rt1,args1,va1), PROTO(rt2,args2,va2) ->
      equal_decltypes rt1 rt2 &&
      va1 = va2 &&
      List.length args1 = List.length args2 &&
      (* Check the arguments but ignore the names *)
      List.fold_left2 
        (fun acc (s1, (_, da1, _)) (s2, (_, da2, _)) -> 
          acc && equal_spec_lists true s1 s2 && equal_decltypes da1 da2) 
        true args1 args2

  | _, _ -> false
end

and equal_single_names (specs1, n1) (specs2, n2) = 
  equal_spec_lists true specs1 specs2 && equal_names n1 n2

(* ----- end of equality stuff ------ *)

(* We want to eliminate duplicate definitions of struct/union/enum tags, 
 * function, typedef and decdefs. We keep a few hash tables. *)

(* Indexed by "struct foo", etc. We keep multiple mappings for each original 
 * name tag. The mapping contains the new name, the type specifier of the old 
 * one and the location *)
let definedTags: (string, string * typeSpecifier * cabsloc) H.t = H.create 113

type conflict = 
    FirstDef of string (* The first definition. But the name must be 
                        * alpha-converted still because of some other 
                        * conflicts  *)
  | Duplicate of string (* Not the first definition but another one just like 
                         * that is present. Give the name of that one *)
  | Conflict of string  (* There is a conflict with another declaration. 
                         * Return a name that can be used for the new one *)
let checkTagDefinition
    (kind: string) 
    (t: string) 
    (ts: typeSpecifier) 
    (l: cabsloc) : conflict = 
  let lookupname = kindPlusName kind t in
  let tforalpha = if t = "" then "anon" ^ kind else t in
  let olds = H.find_all definedTags lookupname in
  if olds = [] then begin
    (* Put it through the alpha converter *)
    let t' = newAlphaName kind tforalpha in
(*    if t <> t' then 
      E.s (E.bug "First tag definition, yet the alpha converter changed the name. Orig=%s %s. New=%s" kind t 
 * t'); *)
    H.add definedTags lookupname (t', ts, l);
    addLocalToEnv lookupname t';
    FirstDef t'
  end else 
    (* Go through the list and find one that matches *)
    let rec loop = function
        [] -> (* None matches. We have a conflict. Except if we are dealing 
               * with an anonymoust tag *)
          if t = "" then begin (* A new definition *)
            let t' = newAlphaName kind tforalpha in
            H.add definedTags lookupname (t', ts, l);
            addLocalToEnv lookupname t';
            FirstDef t'
          end else begin
            (Printf.printf 
               "WARNING: conflicting redefinitions of tag %s %s;\n" 
               kind t);
            List.iter 
              (fun (_, oldts, oldloc) -> 
                (Printf.printf "previous at %s:%d: \n\t" 
                   oldloc.filename oldloc.lineno);
                (Cprint.print_specifiers [SpecType oldts]);
                (Cprint.new_line ())) olds;
            (Printf.printf "new specifier (at %s:%d): \n\t" 
               l.filename l.lineno);
            (Cprint.print_specifiers [SpecType ts]);
            (Cprint.new_line ());
            (* Pick a new name *)
            let t' = newAlphaName kind tforalpha in
            (* Remember the new mapping *)
            H.add definedTags lookupname (t', ts, l);
            (* Add it to the environment, but only for this file *)
            addLocalToEnv lookupname t';
            Conflict t'
          end
            
      | (t', oldts, oldl) :: rest  when equal_typespecs false ts oldts -> 
          (* We found a matching one *)
          (* Add it to the environment *)
          addLocalToEnv lookupname t';
          Duplicate t'
            
      | _ :: rest -> loop rest
    in
    loop olds

  

(* Keep a map of already declared typenames. Keep, the alpha-converted name, 
 * the definition (a single_name with the original name as the actual name) 
 * and the location  *)
let definedTypes: (string, (string * single_name * cabsloc)) H.t 
    = H.create 111


let checkTypeDefinition
    (n: string) 
    (sn: single_name) 
    (l: cabsloc) : conflict = 
  let olds = H.find_all definedTypes n in
  if olds = [] then begin
    (* Put it through the alpha converter *)
    let n' = newAlphaName "" n in
(*
    if n <> n' then 
      E.s(E.bug "First type definition, yet the alpha converter changed the name. Orig=%s, New=%s\n" n 
 * n'); *)
    H.add definedTypes n (n', sn, l);
    addLocalToEnv n n';
    FirstDef n'
  end else 
    (* Go through the list and find one that matches *)
    let rec loop = function
        [] -> (* None matches. We have a conflict. *)
          (Printf.printf "WARNING: conflicting type redefinitions for %s\n"
             n);
          List.iter 
            (fun (_, oldsn, oldloc) -> 
              (Printf.printf "previous at %s:%d: \n\t" 
                 oldloc.filename oldloc.lineno);
              (Cprint.print_single_name oldsn);
              (Cprint.new_line ())) olds;
            (Printf.printf "new definition (at %s:%d): \n\t" 
               l.filename l.lineno);
          (Cprint.print_single_name sn);
          (Cprint.new_line ());
          (* Pick a new name *)
          let n' = newAlphaName "" n in
          (* Remember the new mapping *)
          H.add definedTypes n (n', sn, l);
          (* Add it to the environment, but only for this file *)
          addLocalToEnv n n';
          Conflict n'
            
      | (n', oldsn, oldl) :: rest  when equal_single_names sn oldsn -> 
          (* We found a matching one *)
          (* Add it to the environment *)
          addLocalToEnv n n';
          Duplicate n'
            
      | _ :: rest -> loop rest
    in
    loop olds


(* Remember the declarations that we have seen so that we can drop identical 
 * ones *)
let declarations: (init_name_group, bool) H.t = H.create 1111

(* Remeber the inline functions that we have seen so that we could eliminate 
 * duplicates *)
let functions: (definition , bool) H.t = H.create 1111

(*********************************
 *********************************
 *********************************
 *)

(* ALPHA-CONVERSION *)

(* Scan various AST elements and apply the new names *)


let rec alpha_attr (s, el) = 
  (s, List.map alpha_expression el)

and alpha_attrs al = 
  List.map alpha_attr al


(* Process the specifiers. Be careful about the definitions of the 
 * struct/union/enum tags and about the enum items *)
and doSpecs (isglobal: bool) (* Whether we are at global scope *)
            (se: spec_elem list) : spec_elem list =
  let doFields (ngl: field_group list) = 
    let doOneField (((id, typ, al): name), widtho) =
      let doWidth = function
          None -> None
        | Some w -> Some (alpha_expression w)
      in
      ((id, alpha_decl_type typ, alpha_attrs al), doWidth widtho)
    in
    let doNameGroup (specs, ng) = 
      (doSpecs false specs, List.map doOneField ng)
    in
    List.map doNameGroup ngl
  in
  match se with
  | [] -> []
  | SpecTypedef :: rest -> SpecTypedef :: doSpecs isglobal rest
  | SpecInline :: rest -> SpecInline :: doSpecs isglobal rest
  | SpecStorage s :: rest -> SpecStorage s :: doSpecs isglobal rest
  | SpecAttr a :: rest -> SpecAttr (alpha_attr a) :: doSpecs isglobal rest
  | SpecType t :: rest -> begin
      let t' = 
        match t with 
          Tnamed n -> Tnamed (lookup "" n)
        | Tstruct (s, None) -> (* Just a reference *)
            Tstruct(lookup "struct" s, None)
        | Tstruct (s, Some flds) -> begin (* A def *)
            let flds' = doFields flds in
            if not isglobal then begin (* Just add to the env *)
              addLocalToEnv "struct" s; (* Do not change the name *)
              Tstruct (s, Some flds')
            end else begin
              (* Prepare the result *)
              match checkTagDefinition "struct" s 
                    (Tstruct ("", Some flds')) !currentLoc with 
                FirstDef s2 -> Tstruct (s2, Some flds')
              | Duplicate s2 -> Tstruct (s2, None) (* Turn it into a 
                                                    * reference to some 
                                                    * already defined struct 
                                                    *) 
              | Conflict s2 -> Tstruct (s2, Some flds') 
            end
        end
        | Tunion (s, None) -> (* Just a reference *)
            Tunion(lookup "union" s, None)
        | Tunion (s, Some flds) -> begin (* A def *)
            let flds' = doFields flds in
            if not isglobal then begin (* Just add to the env *)
              addLocalToEnv "union" s; (* Do not change the name *)
              Tunion (s, Some flds')
            end else begin
              (* Prepare the result *)
              match checkTagDefinition "union" s 
                    (Tunion ("", Some flds')) !currentLoc with 
                FirstDef s2 -> Tunion (s2, Some flds')
              | Duplicate s2 -> Tunion (s2, None) (* Turn it into a 
                                                    * reference to some 
                                                    * already defined union 
                                                    *) 
              | Conflict s2 -> Tunion (s2, Some flds') 
            end
        end
        | Tenum (s, None) -> (* Just a reference *)
            Tenum (lookup "enum" s, None)
        | Tenum (s, Some flds) -> begin (* A def *)
            if not isglobal then begin
              addLocalToEnv "enum" s; (* Do not change the name *)
              let flds' = 
                List.map 
                  (fun (n, e) -> (processDeclName isglobal true n, 
                                  alpha_expression e)) flds in
              Tenum (s, Some flds')
            end else begin
              (* First process the fields as if for a local environment *)
              enterScope ();
              let flds' = 
                List.map (fun (n, e) -> (processDeclName false false n, 
                                         alpha_expression e)) flds in
              exitScope (); (* Undo the things we added to the environment 
                             * for the processing of the fields *)
              (* We use this set of fields to check for previous definitions 
               * of this enum *)
              match checkTagDefinition "enum" s
                  (Tenum ("", Some flds')) !currentLoc with
                FirstDef s2 -> Tenum (s2, 
                                   (* Now process the items for real *)
                                   Some 
                                     (List.map 
                                        (fun (n, e) -> 
                                          (processDeclName isglobal true n,
                                           alpha_expression e))
                                        flds))
              | Duplicate s2 -> Tenum (s2, None) (* Just a reference *)
              | Conflict s2 -> 
                  Tenum (s2, 
                         (* Now process the items for real *)
                         Some 
                           (List.map 
                              (fun (n, e) -> 
                                (processDeclName isglobal true n,
                                 alpha_expression e))
                              flds))
            end
        end                           

        | TtypeofE e -> TtypeofE (alpha_expression e)

        | TtypeofT (specs, decl) -> (* Does never define tags *)
            TtypeofT(doSpecs false specs, alpha_decl_type decl)
        | t -> t
      in
      SpecType t' :: doSpecs isglobal rest
  end


and alpha_decl_type = function
  | JUSTBASE -> JUSTBASE
  | PROTO (typ, pars, ell) -> 
      PROTO(alpha_decl_type typ, alpha_params pars, ell)
  | PTR (attrs, typ) -> PTR(alpha_attrs attrs, alpha_decl_type typ)
  | ARRAY (typ, dim) -> ARRAY(alpha_decl_type typ, alpha_expression dim)
  | PARENTYPE (al1, typ, al2) -> 
      PARENTYPE (alpha_attrs al1, alpha_decl_type typ,
                 alpha_attrs al2)

and alpha_only_type (specs, dt) =
  (doSpecs true specs, alpha_decl_type dt)

(* ATTRIBUTES ARE ADDED BACK *)    
and alpha_name (kind: string) ((id, typ, al) : name) = begin
 if id = "___missing_field_name" then
   (id, alpha_decl_type typ, alpha_attrs al)
 else
   (lookup kind id, alpha_decl_type typ, alpha_attrs al)
end
        
and alpha_name_group (kind: string) (specs, names) =
  (doSpecs false specs, List.map (alpha_name kind) names)
    
and alpha_single_name (kind: string) (specs, name) =
  (doSpecs false specs, alpha_name kind name) 

(* Raymond added declare_id lookup_id *)
and alpha_params (pars : single_name list) = begin
  List.map (fun single_name -> alpha_single_name "" single_name) pars
end

        
and alpha_exps exps =
  List.map alpha_expression exps

and alpha_init_expression (iexp: init_expression) : init_expression = 
  match iexp with
    NO_INIT -> NO_INIT
  | SINGLE_INIT e -> SINGLE_INIT (alpha_expression e)
  | COMPOUND_INIT initexps ->
      let doinitexp = function
          NEXT_INIT, e -> (NEXT_INIT, alpha_init_expression e)
        | i, e -> 
            let rec doinit = function
                NEXT_INIT -> NEXT_INIT
              | INFIELD_INIT (fn, i) -> INFIELD_INIT(fn, doinit i)
              | ATINDEX_INIT (e, i) -> 
                  ATINDEX_INIT(alpha_expression e, doinit i)
              | ATINDEXRANGE_INIT (s, e) ->
                  ATINDEXRANGE_INIT(alpha_expression s, 
                                    alpha_expression e)
            in
            (doinit i, alpha_init_expression e)
      in
      COMPOUND_INIT (List.map doinitexp initexps)
        
(* No need to rename fields *)    
and alpha_expression (exp : expression) : expression =
  match exp with
    NOTHING | LABELADDR _ -> exp

  | UNARY (op, exp') ->
      UNARY(op, alpha_expression exp')  
  | BINARY (op, exp1, exp2) ->
      BINARY(op, alpha_expression exp1, alpha_expression exp2)      
  | QUESTION (exp1, exp2, exp3) ->
      QUESTION(alpha_expression exp1, 
               alpha_expression exp2, alpha_expression exp3)    
  | CAST (typ, iexp) ->
      CAST(alpha_only_type typ, alpha_init_expression iexp)     
  | CALL (exp, args) ->
      CALL(alpha_expression exp, alpha_exps args)   
  | COMMA exps ->
      COMMA(alpha_exps exps)
  | CONSTANT cst ->
      CONSTANT(
        (match cst with
        CONST_INT i -> CONST_INT i
      | CONST_FLOAT r -> CONST_FLOAT r
      | CONST_CHAR c -> CONST_CHAR c
      | CONST_STRING s -> CONST_STRING s))

  | VARIABLE name -> 
      VARIABLE(lookup "" name)
  | EXPR_SIZEOF exp ->
      EXPR_SIZEOF (alpha_expression exp)
  | TYPE_SIZEOF (specs, dt) ->
      TYPE_SIZEOF(doSpecs false specs, alpha_decl_type dt)
  | EXPR_ALIGNOF exp ->
      EXPR_ALIGNOF (alpha_expression exp)
  | TYPE_ALIGNOF (specs, dt) ->
      TYPE_ALIGNOF(doSpecs false specs, alpha_decl_type dt)
  | INDEX (exp, idx) ->
      INDEX(alpha_expression exp, alpha_expression idx)
  | MEMBEROF (exp, fld) ->
      MEMBEROF(alpha_expression exp, fld)
  | MEMBEROFPTR (exp, fld) ->
      MEMBEROFPTR(alpha_expression exp, fld)
  | GNU_BODY (blk) ->
      GNU_BODY (alpha_block blk)

and alpha_block (labs, defs, stmts) = 
  enterScope ();
  let res = 
    (* make sure we do the definitions first *)
    let defs' = doDefinitions false defs in
    (labs, defs',
     List.map alpha_statement stmts) in
  exitScope ();
  res

(*
** Statement combining
*)
and alpha_statement stat =
  match stat with
    NOP loc ->
      currentLoc := loc;
      NOP loc

  | COMPUTATION (exp, loc) ->
      currentLoc := loc;
      COMPUTATION(alpha_expression exp, loc)
  | BLOCK (blk, loc) ->
      currentLoc := loc;
      BLOCK(alpha_block blk, loc)
  | SEQUENCE (s1, s2, loc) ->
      currentLoc := loc;
      SEQUENCE(alpha_statement s1, alpha_statement s2, loc)
  | IF (exp, s1, s2, loc) ->
      currentLoc := loc;
      IF(alpha_expression exp, alpha_substatement s1, 
         alpha_substatement s2, loc)
  | WHILE (exp, stat, loc) ->
      currentLoc := loc;
      WHILE(alpha_expression exp, alpha_substatement stat, loc)
  | DOWHILE (exp, stat, loc) ->
      currentLoc := loc;
      DOWHILE(alpha_expression exp, alpha_substatement stat, loc)
  | FOR (exp1, exp2, exp3, stat, loc) ->
      currentLoc := loc;
      FOR(alpha_expression exp1, 
          alpha_expression exp2, alpha_expression exp3, 
          alpha_substatement stat, loc)
  | BREAK(loc) ->
      currentLoc := loc;
      BREAK(loc) 
  | CONTINUE (loc)->
      currentLoc := loc;
      CONTINUE (loc)
  | RETURN (exp, loc) ->
      currentLoc := loc;
      RETURN (alpha_expression exp, loc)
  | SWITCH (exp, stat, loc) ->
      currentLoc := loc;
      SWITCH(alpha_expression exp, alpha_substatement stat, loc)
  | CASE (exp, stat, loc) ->
      currentLoc := loc;
      CASE(alpha_expression exp, alpha_substatement stat, loc)
  | CASERANGE (expl, exph, stat, loc) ->
      currentLoc := loc;
      CASERANGE(alpha_expression expl, alpha_expression exph, 
                alpha_substatement stat, loc)
  | DEFAULT (stat, loc) ->
      currentLoc := loc;
      DEFAULT(alpha_substatement stat, loc)
  | LABEL (name, stat, loc) ->
      currentLoc := loc;
      LABEL(name, alpha_substatement stat, loc)
  | GOTO (name, loc) ->
      currentLoc := loc;
      GOTO(name, loc)
  | COMPGOTO (exp, loc) -> 
      currentLoc := loc;
      COMPGOTO (alpha_expression exp, loc)
  | ASM (tlist, isvol, outs, ins, clobs, loc) ->
      currentLoc := loc;
      ASM(tlist, isvol, outs, ins, clobs, loc)   
           
and alpha_substatement stat =
  alpha_statement stat

and doDefinitions (isglobal: bool) (* Whether at global scope *)
                  (defs: definition list) =
  let res = List.fold_left (doDefinition isglobal) [] defs in
  List.rev res

  
and doDefinition (isglobal: bool) (* Whether at global scope *)
                 (acc: definition list) (* Accumulate the definitions in 
                                         * reverse order *) = function
    FUNDEF ((specs, (n, decl, attrs)), body, loc) -> begin
      currentLoc := loc;
(*      ignore (E.log "Doing body of function %s\n" n); *)
      (* On MSVC force inline functions to be static. Otherwise the compiler
       * might complain that the function is declared with multiple bodies *)
      let specs1 = 
        if !Cprint.msvcMode && isInline specs && not (isStatic specs) then
          SpecStorage STATIC :: specs else specs
      in
      let specs2 = doSpecs isglobal specs1 in
      let n' = processDeclName isglobal (isStatic specs2) n in
      let decl' = alpha_decl_type decl in
      let attrs' = alpha_attrs attrs in
      let body' = alpha_block body in
      let res = 
        FUNDEF ((specs2, 
                 (n', decl', attrs')), body', loc) 
      in
      if isInline specs2 then 
        (* For inline functions we might see the same definition multiple 
         * times. Keep only one. Index everything, including the location. 
         * There is no point in throwing the location away unless we rewrite 
         * the body to throw all the locations it containts *)
        if H.mem functions res then 
          acc (* Duplicate *)
        else begin
          H.add functions res true;
          res :: acc
        end
      else
        res :: acc
    end
               
  | DECDEF ((specs, inl), loc) -> begin
      currentLoc := loc;
      let specs1 = doSpecs isglobal specs in
      let static = isStatic specs1 in
      let ng = (specs1,
                List.map 
                  (fun ((n, decl, attrs), ie) ->
                    (* Do the declaration first *)
                    let n' = processDeclName isglobal static n in
                    ((n', 
                      alpha_decl_type decl, 
                      alpha_attrs attrs),
                     alpha_init_expression ie)) inl)
      in
      (* Keep a hash of DEFDEF's and drop identical ones. Use OCAML's 
       * structural equality to test for identical declarations. If we don't 
       * do this then cabs2cil slows to a crawl. *)
      if isglobal && H.mem declarations ng then 
        acc
      else begin
        if isglobal then H.add declarations ng true;
        DECDEF (ng, loc) :: acc
      end
    end
      

  | TYPEDEF ((specs, ng), loc) -> begin
      currentLoc := loc;
      let specs1 = doSpecs isglobal specs in
      let static = true in (* Treat TYPEDEF as a static one *)
      (* Go through the names and find if it is a duplicate definition *)
      let ng' = 
        List.fold_left 
          (fun acc (n, decl, attrs) -> 
            (* Process the decl and the attrs *)
            let decl' = alpha_decl_type decl in
            let attrs' = alpha_attrs attrs in
            match checkTypeDefinition n (specs1, (n, decl', attrs')) loc with
              FirstDef n' -> (n', decl', attrs') :: acc
            | Duplicate n' -> (* Drop it *) acc
            | Conflict n' -> (n', decl', attrs') :: acc)
          []
          ng
      in
      let ng' = List.rev ng' in
      if ng' = [] then 
        (* See if there is anything interesting in the specs1. If not then 
        * drop this completely *)
        if List.exists 
            (function
                SpecType (Tstruct(_, Some _)) -> true
              | SpecType (Tunion(_, Some _)) -> true
              | SpecType (Tenum(_, Some _)) -> true
              | _ -> false) specs1 then
          ONLYTYPEDEF (specs1, loc) :: acc
        else
          acc
      else 
        TYPEDEF ((specs1, ng'), loc) :: acc
  end
      
  | ONLYTYPEDEF (specs, loc) -> begin
      currentLoc := loc;
      let specs1 = doSpecs isglobal specs in
      let res = ONLYTYPEDEF (specs1, loc) in
      res :: acc
  end
      
  | GLOBASM (s, l) -> GLOBASM (s, l) :: acc

  | PRAGMA (e, l) -> PRAGMA (alpha_expression e, l) :: acc
      
    
let initialize () = 
      (* Clean up the alpha table *)
  H.clear alphaTable;
  H.clear env;
  H.clear registeredGlobals;
  H.clear definedTags;
  H.clear definedTypes;
  H.clear declarations;
  H.clear functions

  
(* The MAIN COMBINER *)
let combine (files : Cabs.file list) : Cabs.file = 
  initialize ();
  let doOneFile (defs: definition list) = 
    enterScope ();
    let defs' = doDefinitions true defs in
    exitScope ();
    defs'
  in
  (* Do a first pass *)
  let files1 = List.map doOneFile files in
  (* See if some globals are shadowed by some previously defined locals *)
  let files2 = 
    if !conflicts == [] then files1 else begin
      initialize ();
      ignore (E.warn "Combiner does a second pass because file-scope identifiers %a conflicted with locals\n"
                (docList (chr ',' ++ break) text) !conflicts);
      (* Add the conflicting names to the alphaTable to reserve those names *)
      List.iter (fun n -> ignore (processDeclName true false n)) !conflicts;
      List.map doOneFile files
    end
  in
  initialize (); (* To make the GC happy *)
  List.flatten files2
  

