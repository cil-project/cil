(* Implementation of whole-program merging *)

open Cabs
open Cabsvisit
open Pretty
module E = Errormsg
module H = Hashtbl



(* Keep track of the current location *)
let currentLoc: cabsloc ref = ref { lineno = -1; filename = "<no file>" }


(****************** ENVIRONMENTS ***********************)
(* For each file we keep an environment. This one grows as we descend in 
 * local scopes. At the end of the file will contain the renaming of toplevel 
 * file-scope constructs *)
(* We have different kinds of names being declared *)
type envKind = 
    EType                               (* A typedef *)
  | EStruct                             (* A structure tag *)
  | EUnion
  | EEnum                               (* An enumeration tags *)
  | EVar                                (* A variable or enumeration item *)

let env: (envKind * string, string) H.t = H.create 111
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
let addLocalToEnv (kind: envKind) (orig: string) (newname: string) = 
  let key = kind, orig in
  H.add env key newname;
  undo := UndoThis (fun _ -> H.remove env key) :: !undo

(* Lookup. If not in the env then it is a global and we leave it alone *)
let lookup (kind: envKind) (origname: string) : string = 
  try
    let alphaname = H.find env (kind, origname) in
    alphaname
  with Not_found -> origname


(*** Apply renaming to a definition ****)
let scopeDepth = ref 0 (* 0 means that we are at top level scope *)
      (* Apply renaming to a name, if at top level. If not at top level then 
       * just add it to the environment *)
let changeName 
    (k: envKind) (n: string) (mkitem: string -> 'a) 
    : 'a visitAction = 
  if !scopeDepth = 0 then 
    let n' = lookup k n in
    if n <> n' then 
      ChangeDoChildrenPost (mkitem n', fun x -> x)
    else
      DoChildren
  else begin
    addLocalToEnv k n n;
    DoChildren
  end

class renameClass : cabsVisitor = object (self)
  inherit nopCabsVisitor

      (* Use of a variable. Always try to rename *)
  method vvar s = lookup EVar s


        (* Keep track of scopes *)
  method vEnterScope () = incr scopeDepth; enterScope ()
  method vExitScope () = decr scopeDepth; exitScope ()


      (* Definition of a name *)
  method vname (k: nameKind) (s: specifier) (n, dt, a) = 
    match k with 
      NType -> changeName EType n (fun n' -> (n', dt, a))
    | NField -> DoChildren
    | NVar -> changeName EVar n (fun n' -> (n', dt, a))
              

        (* And declared structure, union and enumeration tags *)
  method vtypespec ts = 
    try
      match ts with 
        Tstruct (n, defs) -> changeName EStruct n (fun n' -> Tstruct(n', defs))
      | Tunion (n, defs) -> changeName EUnion n (fun n' -> Tunion(n', defs))
      | Tenum (n, defs) -> changeName EEnum n (fun n' -> Tenum(n', defs))
      | Tnamed n -> changeName EType n (fun n' -> Tnamed n')
      | _ -> DoChildren
    with Not_found -> DoChildren
end
let renameVisitor : cabsVisitor = new renameClass


(************** ALPHA CONVERSION ********************)
(* Global names are split into a prefix, a ___ (3 underscores) separator and 
 * an integer suffix. For each prefix we keep the maximum integer suffix with 
 * which the prefix was encountered. This suffix is -1 when only the version 
 * without the suffix was encountered.  *)
let alphaTableType: (string, int ref) H.t = H.create 511
let alphaTableVar: (string, int ref) H.t = H.create 511
let alphaTableStruct: (string, int ref) H.t = H.create 511
let alphaTableUnion: (string, int ref) H.t = H.create 511
let alphaTableEnum: (string, int ref) H.t = H.create 511

let resetAlpha () = 
  H.clear alphaTableType;
  H.clear alphaTableVar;
  H.clear alphaTableStruct;
  H.clear alphaTableUnion;
  H.clear alphaTableEnum

let selectAlphaTable (kind: envKind) = 
  match kind with 
  | EVar -> alphaTableVar
  | EType -> alphaTableType
  | EStruct -> alphaTableStruct
  | EUnion -> alphaTableUnion
  | EEnum -> alphaTableEnum

(* The main alpha conversion routine *)
let rec newAlphaName (kind: envKind) 
                     (origname: string) : string = 
  let alphaTable = selectAlphaTable kind in
  (* Copied from CIL *)
  let prefix, _, suffix = splitNameForAlpha origname in
  (* ignore (E.log "newAlphaName(%s). P=%s, S=%d\n" origname prefix suffix);
     *)
  let newname = 
    try
      let rc = H.find alphaTable prefix in
      let newsuffix = if suffix > !rc then suffix else !rc + 1 in
      rc := newsuffix;
      prefix ^ "___" ^ (string_of_int newsuffix)
    with Not_found -> begin (* First variable with this prefix *)
      H.add alphaTable prefix (ref suffix);
      origname  (* Return the original name *)
    end
  in
  newname




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






(* Collect the type and tag definitions in a file. For each definition, keep 
 * its kind, its original name, a specifier and a name. If the kind is EType 
 * then the specifier and name are as given by the TYPEDEF. If the kinds is a 
 * tag kind then the name ie empty and the specifier has one element which is 
 * the definition of the tag *)
type typeTagDef = envKind * string * specifier * name
let fileTypeTags : typeTagDef list ref = ref []
  
class collectTypeTagDefsClass  : cabsVisitor = object (self)
  inherit nopCabsVisitor

  val scopeDepth = ref 0  (* 0 means top-level *)

  method vEnterScope () = incr scopeDepth
  method vExitScope () = decr scopeDepth

  method vdef d = 
    if !scopeDepth > 0 then SkipChildren else
    match d with 
    | TYPEDEF ((s, nl), l) -> 
        List.iter (fun ((n, _, _) as nm) -> 
          fileTypeTags := (EType, n, s, nm) :: !fileTypeTags) nl;
        DoChildren

    | _ -> DoChildren

  (* Don't go into blocks *)      
  method vblock _ = SkipChildren
      
        (* And defined structure, union and enumeration tags *)
  method vtypespec ts = 
    let emptyName = ("", JUSTBASE, []) in
    (match ts with 
      Tstruct (n, Some flds) -> 
        fileTypeTags := 
           (EStruct, n, [SpecType ts], emptyName) :: !fileTypeTags
    | Tunion (n, Some flds) -> 
        fileTypeTags := 
           (EUnion, n, [SpecType ts], emptyName) :: !fileTypeTags
    | Tenum (n, Some eil) ->  
        fileTypeTags := 
           (EEnum, n, [SpecType ts], emptyName) :: !fileTypeTags
    | _ -> ());
    DoChildren

end
let collectTypeTagDefs = new collectTypeTagDefsClass


      

(* Store some global definitions for typeTags. For each name we store the 
 * list of all defitions for that name that were encountered in the whole 
 * progra. Each such definition uses the _original_ name for the defined 
 * entity. And each such definition is accompanied by the name with which the 
 * definition actually appears *)
let globalTypeTags : (envKind * string, typeTagDef * string) H.t = H.create 111

(* Extend the environment with mappings for the type names and tags defined 
 * in this file *)
let findTypeTagNames (f: Cabs.file) = 
  fileTypeTags := [];
  ignore (visitCabsFile collectTypeTagDefs f);
  let tt_list = List.rev !fileTypeTags in
  (ignore (E.log "typeTags= ");
   List.iter (fun (k, n, _, _) -> ignore (E.log "%s," n)) tt_list;
   ignore (E.log "\n"));
  (* We keep track of those names for which we assigned fresh names *)
  let typeTagAssignedFresh : (envKind * string, string) H.t = H.create 111 in

  (* Rename a type tag but leave the defined name alone *)
  let renameTT (defk, defn, defspec, defname) = 
    let defnk = (defk, defn) in
    H.add env defnk defn; (* Leave the defined name alone *)
    let tt' = 
      let nk = match defk with EType -> NType | _ -> NVar in
      let defspec' = visitCabsSpecifier renameVisitor defspec in
      let defname' = visitCabsName renameVisitor nk defspec' defname in
      (defk, defn, defspec', defname')
    in
    H.remove env defnk; (* Undo the last change *)
    tt'
  in
  (* Keep track if we have changed the rename table *)
  let renameTableChanged = ref true in
  (* Iterate until fixed point *)
  while !renameTableChanged do
    renameTableChanged := false;
    List.iter 
      (fun ((defk, defn, defspec, defname) as tt) -> 
        let defnk = (defk, defn) in
        (* Leave this one alone if we have already decided to assign a fresh 
         * name to it *)
        if not (H.mem typeTagAssignedFresh defnk) then begin
          (* See if it is renamed or not *)
          let renamed_o = try Some (H.find env defnk) with Not_found -> None
          in 
          (* Now find if we can reuse some name or if the previous reuse is 
           * fine *)
          let tt' = renameTT tt in
          let rec loop = function
              [] -> (* No definition matches. Make a new name for this one *)
                ignore (E.log "No matches for %s\n" defn);
                let n' = newAlphaName defk defn in
                H.add typeTagAssignedFresh defnk n';
                H.add env defnk n';
                renameTableChanged := true

            | (ttold, oldname) :: rest when ttold = tt' -> 
                (* We found a match *)
                (match renamed_o with
                  None -> (* Not already renamed *)
                    ignore (E.log "Found a match for %s\n" defn);
                    H.add env defnk oldname;
                    renameTableChanged := true

                | Some renamed -> 
                    (* If this is already renamed, accept only matches with 
                     * the same name *)
                    if renamed = oldname then 
                      (* Put it back. No change to the rename table *)
                      H.add env defnk renamed
                    else begin
                      loop rest
                    end)
            | _ :: rest -> loop rest
          in
          loop (H.find_all globalTypeTags defnk)
        end)
      tt_list
  done;
  (* Now add to the globalTypeTags what we found *)
  List.iter 
    (fun ((defk, defn, defspec, defname) as tt) -> 
      let defnk = (defk, defn) in
      try
        let n' = H.find typeTagAssignedFresh defnk in
        let tt' = renameTT tt in 
        H.add globalTypeTags defnk (tt', n')
      with Not_found -> ())
    tt_list

(*********** COLLECT GLOBALS **********************)
let globals : (string, bool) H.t = H.create 1111

let registerGlobal (n: string) = 
  if not (H.mem globals n) then begin
    H.add globals n true; 
    (* And add it to the alpha table *)
    let n' = newAlphaName EVar n in 
    (* No way to conflict at this point *)
    assert (n == n');
  end

let collectGlobals (fl: file list) = 
  let collectOneGlobal = function
      FUNDEF ((specs, (n, decl, attrs)), _, _) -> 
        let islocal = isStatic specs || isInline specs in
        if not islocal then registerGlobal n
        
    | DECDEF ((specs, inl), _) -> 
        let islocal = isStatic specs in
        if not islocal then 
          List.iter (fun ((n, _, _), _) -> registerGlobal n) inl

    | _ -> ()
  in
  H.clear globals;
  List.iter (fun f -> List.iter collectOneGlobal f) fl


let theProgram : Cabs.definition list ref = ref []

(* The MAIN MERGER *)
let merge (files : Cabs.file list) : Cabs.file = 
  resetAlpha (); (* Clear the alpha tables *)
  collectGlobals files; (* Collect all the globals *)
  theProgram := [];
  let doOneFile (f: Cabs.file) = 
    H.clear env; (* Start with a brand new environment *)
    (* Find the new names for type and tags *)
    findTypeTagNames f;
    (* Now apply the new names and while doing that remove some global 
     * declarations *)
    let f' = visitCabsFile renameVisitor f in
    List.iter (fun d -> theProgram := d :: !theProgram) f'
  in
  List.iter doOneFile files;
  let res = List.rev !theProgram in
  (* Make the GC happy *)
  resetAlpha ();
  theProgram := [];
  H.clear globals;
  H.clear env;
  res

