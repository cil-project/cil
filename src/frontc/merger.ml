(* Implementation of whole-program merging *)

open Cabs
open Cabsvisit
open Pretty
module E = Errormsg
module H = Hashtbl

let debugTypes = false

(* Keep track of the current location *)
let currentLoc: cabsloc ref = ref { lineno = -1; filename = "<no file>" }


(****************** ENVIRONMENTS ***********************)
(* For each file we keep an environment. This one grows and shrinks as we 
 * descend in local scopes. It is initialized at the begining of each file 
 * and at the end of each the file will contain the renaming of toplevel 
 * file-scope constructs *)
(* We have different kinds of names being declared *)
type envKind = 
    EType                               (* A typedef *)
  | EStruct                             (* A structure tag *)
  | EUnion
  | EEnum                               (* An enumeration tags *)
  | EVar                                (* A variable or enumeration item *)

let ek2text = function
    EType -> "type"
  | EStruct -> "struct"
  | EUnion -> "union"
  | EEnum -> "enum"
  | EVar -> "variable"

let env: (envKind * string, string) H.t = H.create 111

(* The reused names that we created for file-scope names. This will be 
 * initialized before each file *)
let reused: (envKind * string, string) H.t = H.create 111

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
    (isdef: bool) (* Is it a definition or just a reference of a tag. For 
                   * variables always true *)
    (k: envKind) 
    (n: string) 
    (mkitem: string -> 'a)
    : 'a visitAction = 
  if isdef && !scopeDepth > 0 then begin
    (* This is a definition on inner scope, so ignore the previous 
       declarations and keep this definition *) 
    addLocalToEnv k n n;
    DoChildren
  end else begin
    (* This is either at top level or not a definition of a name *)
    let n' = lookup k n in
    ChangeDoChildrenPost (mkitem n', fun x -> x)
  end

let changeDefsIntoRefs = ref false
let reuseDefs (k: envKind) 
              (n: string) 
              (defs: 'a option) : 'a option = 
  if !scopeDepth = 0 && !changeDefsIntoRefs && H.mem reused (k, n) then begin
    None
  end else
    defs

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
      NType -> changeName true EType n (fun n' -> (n', dt, a))
    | NField -> DoChildren
    | NVar -> changeName true EVar n (fun n' -> (n', dt, a))
              

        (* And declared structure, union and enumeration tags *)
  method vtypespec ts = 
    try
      match ts with 
        Tstruct (n, defs) -> 
          let isdef = defs <> None in
          let defs' = reuseDefs EStruct n defs in
          changeName isdef EStruct n (fun n' -> Tstruct(n', defs'))
      | Tunion (n, defs) -> 
          let isdef = defs <> None in
          let defs' = reuseDefs EUnion n defs in
          changeName isdef EUnion n (fun n' -> Tunion(n', defs'))
      | Tenum (n, defs) -> 
          let isdef = defs <> None in
          let defs' = reuseDefs EEnum n defs in
          changeName isdef EEnum n (fun n' -> Tenum(n', defs'))
      | Tnamed n -> 
          changeName false EType n (fun n' -> Tnamed n')
      | _ -> DoChildren
    with Not_found -> DoChildren
end
let renameVisitor = new renameClass
let renameDefinition (d: Cabs.definition) : Cabs.definition = 
  let d' = visitCabsDefinition renameVisitor d in
  match d' with 
    [x] -> x
  | _ -> E.s (E.bug "renameVisitor returns multiple definitions")
  

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


(** Handling of types and tag definitions *)


(* Collect the type and tag definitions in a file. For each definition, keep 
 * its kind, its original name, a specifier and a name. If the kind is EType 
 * then the specifier and name are as given by the TYPEDEF. If the kinds is a 
 * tag kind then the name ie empty and the specifier has one element which is 
 * the definition of the tag *)
type typeTagDef = envKind * string * specifier * name * cabsloc
let fileTypeTags: (envKind * string, specifier * name * cabsloc) H.t 
    = H.create 53
  
class collectTypeTagDefsClass  : cabsVisitor = object (self)
  inherit nopCabsVisitor

  val scopeDepth = ref 0  (* 0 means top-level *)

  method vEnterScope () = incr scopeDepth
  method vExitScope () = decr scopeDepth

  method vdef d = 
    if !scopeDepth > 0 then SkipChildren else
    begin
      visitorLocation := get_definitionloc d;
      match d with 
      | TYPEDEF ((s, nl), l) -> 
          List.iter (fun ((n, _, _) as nm) -> 
            H.add fileTypeTags (EType, n) (s, nm, !visitorLocation)) nl;
          DoChildren
            
      | _ -> DoChildren
    end
      (* Don't go into blocks *)      
  method vblock _ = SkipChildren
      
        (* And defined structure, union and enumeration tags *)
  method vtypespec ts = 
    let emptyName = ("", JUSTBASE, []) in
    (match ts with 
      Tstruct (n, Some flds) -> 
        if n <> "" then 
          H.add fileTypeTags (EStruct, n) 
            ([SpecType ts], emptyName, !visitorLocation)
    | Tunion (n, Some flds) -> 
        if n <> "" then
          H.add fileTypeTags (EUnion, n) 
            ([SpecType ts], emptyName, !visitorLocation)
    | Tenum (n, Some eil) ->
        if n <> "" then   
          H.add fileTypeTags (EEnum, n) 
            ([SpecType ts], emptyName, !visitorLocation)
    | _ -> ());
    DoChildren

end
let collectTypeTagDefs = new collectTypeTagDefsClass


(* Equality constraints *)
type eqConstraint = envKind * string * string
let rec compareSpecs 
     (s1: specifier) 
     (s2: specifier) (acc: eqConstraint list) : eqConstraint list = 
  (* filter out the type specifiers *)
  let ts1, rest1 = 
    List.partition (function SpecType _ -> true | _ -> false) s1 in
  let ts2, rest2 = 
    List.partition (function SpecType _ -> true | _ -> false) s2 in
  if rest1 <> rest2 then raise Not_found;
  match ts1, ts2 with 
    [SpecType ts1], [SpecType ts2] -> begin
      (* We don't go into the definition of a struct here, except if it is 
       * anonymous *)
      match ts1, ts2 with 
        Tstruct (s1, fg1), Tstruct (s2, fg2) -> 
          if s1 = "" && s2 = "" then 
            compareFieldGroupLists acc fg1 fg2
          else
            if s1 = "" || s2 = "" then raise Not_found else
            (EStruct, s1, s2) :: acc
      | Tunion (s1, fg1), Tunion (s2, fg2) -> 
          if s1 = "" && s2 = "" then 
            compareFieldGroupLists acc fg1 fg2
          else
            if s1 = "" || s2 = "" then raise Not_found else
            (EUnion, s1, s2) :: acc
      | Tenum (s1, el1), Tenum (s2, el2) -> 
          if s1 = "" && s2 = "" then 
            if el1 = el2 then acc else raise Not_found
          else
            if s1 = "" || s2 = "" then raise Not_found else
            (EEnum, s1, s2) :: acc
      | Tnamed t1, Tnamed t2 -> 
          (EType, t1, t2) :: acc
      | TtypeofT (s1, dt1), TtypeofT (s2, dt2) -> 
          if dt1 <> dt2 then raise Not_found;
          compareSpecs s1 s2 acc
          
      | _ -> if ts1 = ts2 then acc else raise Not_found
    end
  | _ -> if ts1 = ts2 then acc else raise Not_found
  
and compareFieldGroups acc (s1, fg1) (s2, fg2) = 
  if fg1 <> fg2 then raise Not_found;
  compareSpecs s1 s2 acc

and compareFieldGroupLists acc fgl1 fgl2 =
  match fgl1, fgl2 with 
    Some fgl1, Some fgl2 -> List.fold_left2 compareFieldGroups [] fgl1 fgl2
  | None, None -> acc
  | _, _ -> raise Not_found
      

(* Store some global definitions for typeTags. For each orignal name and kind 
 * we store the list of all defintions for that name that were encountered in 
 * the whole program. *)
let globalTypeTags : (envKind * string, typeTagDef) H.t = H.create 111

let dumpGlobalTypeTags (msg: string) = 
  let old = !Cprint.out in
  Cprint.out := stderr;
  Cprint.print msg;
  H.iter (fun (k, n) (_, n', specs, nm, _) -> 
    Cprint.print ("Node(" ^ ek2text k ^ "," ^ n ^ ") -> " ^ n' ^ ",");
    Cprint.print_specifiers specs;
    Cprint.print ",";
    Cprint.print_name nm;
    Cprint.print ")\n")
    globalTypeTags;
  flush !Cprint.out;
  Cprint.out := old
  

(* We keep a graph of equality constraints *)
type eqNode = { 
            id: int;            (* For debugging only *)
    mutable eq: bool;           (* Whether they can be equal *)
    mutable succs: eqNode list; (* All the nodes that cannot be equal if this 
                                 * node is not equal *)
  } 
  
let constraintGraph: (envKind * string * string, eqNode) H.t = H.create 111
let nodeId = ref 0
let getNode (key: eqConstraint) = 
  try
    H.find constraintGraph key
  with Not_found -> begin
    (* New nodes are made not eq *)
    let node = { id = !nodeId; eq = false; succs = [] } in
    incr nodeId;
    H.add constraintGraph key node;
    node
  end
let dumpGraph (msg: string) = 
  ignore (E.log " constraint graph(%s):\n" msg);
  H.iter (fun (k, on, nn) n -> 
    ignore (E.log " %d (old=%s, new=%s, eq=%b) -> %a\n"
              n.id on nn n.eq
              (docList (chr ',') (fun succ -> num succ.id)) n.succs))
    constraintGraph
    
let constructConstraintGraph () = 
  H.clear constraintGraph; nodeId := 0;
  let doOne (defk, defn) (defspec, ((_, def_dt, def_a) as defname), defloc) = 
    (* All previous typeTagDefs for the same original name *)
    let old = H.find_all globalTypeTags (defk, defn)  in
    List.iter
      (fun (_, oldn, oldspec, ((_, old_dt, old_a) as oldname), oldloc) -> 
        (* Make a node *)
        let node = getNode (defk, oldn, defn) in
        (* This node must be made eq *)
        node.eq <- true;
        (* Find the equality constraints that must all hold for defn to be 
        * compatible with oldn. Watch out for Not_found *)
        try
          (* Compare the name as well, except the string part *)
          let constr = 
            match defk with 
              EType -> 
                if old_dt <> def_dt || old_a <> def_a then raise Not_found;
                compareSpecs oldspec defspec []
            | EStruct -> begin
                match oldspec, defspec with 
                  [SpecType (Tstruct (_, Some ofg))], 
                  [SpecType (Tstruct (_, Some nfg))] -> 
                    compareFieldGroupLists [] (Some ofg) (Some nfg)
                | _ -> E.s (E.bug "compareTypeTags(struct)")
            end
            | EUnion -> begin
                match oldspec, defspec with 
                  [SpecType (Tunion (_, Some ofg))], 
                  [SpecType (Tunion (_, Some nfg))] -> 
                    compareFieldGroupLists [] (Some ofg) (Some nfg)
                | _ -> E.s (E.bug "compareTypeTags(union)")
            end
            | EEnum -> begin
                match oldspec, defspec with 
                  [SpecType (Tenum (_, Some el1))], 
                  [SpecType (Tenum (_, Some el2))] -> 
                    if el1 = el2 then [] else raise Not_found
                | _ -> E.s (E.bug "compareTypeTags(enum)")
            end
            | _ -> E.s (E.bug "compareTypeTags(other)")
          in
          (* Add an edge in the graph for each constraint *)
          List.iter (fun ckey ->
            let node1 = getNode ckey in (* Make the node not eq if it does 
                                         * not exist already *)
            node1.succs <- node :: node1.succs) constr
        with _ -> (* This node cannot possibly match *)
          node.eq <- false)
      old
  in
  H.iter doOne fileTypeTags;
  (* Now we can have constraint pairs for undefined "struct foo". If they both 
   * have the same name then mark them as equal *)
  H.iter (fun (k,on,nn) nd -> 
    if on = nn && not nd.eq && not (H.mem fileTypeTags (k, nn)) 
               && not (H.mem globalTypeTags (k, on)) then
      nd.eq <- true) constraintGraph;
 
  if debugTypes then dumpGraph " before pushing";
  (* Push eq = false to successors *)
  let rec push (n: eqNode) = 
    if n.eq then begin n.eq <- false; List.iter push n.succs end in
  H.iter (fun key node -> 
    if not node.eq then List.iter push node.succs) constraintGraph;
  if debugTypes then dumpGraph " after pushing"
          

(* Extend the environment with mappings for the type names and tags defined 
 * in this file *)
let findTypeTagNames (f: Cabs.file) = 
  H.clear fileTypeTags;
  (* Collect the type and tags defined in this file *)
  ignore (visitCabsFile collectTypeTagDefs f);
  (* Now collect and solve the constraint graph *)
  constructConstraintGraph ();
  (* Now assign names to types and tags *)
  H.iter 
    (fun (defk, defn) (defspec, defname, defloc) -> 
      begin
        let defkn = (defk, defn) in
        (* Find the first old name that matches *)
        let oldnames = H.find_all globalTypeTags defkn in
        let matches = 
          List.filter 
            (fun (_, on, _, _, _) -> let n = getNode (defk, on, defn) in n.eq)
            oldnames in
        match matches with
          [] -> (* No match. Use a new name *)
            let defn' = newAlphaName defk defn in
            H.add env defkn defn';
            if oldnames <> [] then 
              ignore (E.warn "%s:%d: Conflicting redefinition of %s %s"
                        defloc.filename defloc.lineno
                        (ek2text defk) defn);
            if debugTypes then 
              ignore (E.log "fresh name for %s -> %s\n" defn defn')
              

        | (_, on, _, _, _) :: rest -> 
            if debugTypes then
              ignore (E.log "reusing name for %s -> %s\n" defn on);
            if rest <> [] then 
              ignore (E.warn " more than one old name found!\n");
            H.add reused defkn on
      end)
    fileTypeTags;
  (* Clean the graph *)
  H.clear constraintGraph;
  (* Add new new names to globalTypeTags. Must do it this late because we 
   * must rename the typeTags and only now we have the whole renaming *)
  H.iter 
    (fun (defk, defn) (defspec, defname, defloc) -> 
      let defkn = (defk, defn) in
      if not (H.mem reused defkn) then begin
        let defn' = lookup defk defn in
        let defspec' = visitCabsSpecifier renameVisitor defspec in
        let nk = match defk with EType -> NType | _ -> NVar in
        let defname' = visitCabsName renameVisitor nk defspec' defname in
        H.add globalTypeTags defkn (defk, defn', defspec', defname', defloc)
      end)
    fileTypeTags;
  if debugTypes then dumpGlobalTypeTags "globalTypeTags"
  


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


(* Remember some definitions so that we can drop duplicates *)
let globalDefinitions: (Cabs.definition, bool) H.t = H.create 113


let theProgram: Cabs.definition list ref = ref []

(* The MAIN MERGER *)
let merge (files : Cabs.file list) : Cabs.file = 
  resetAlpha (); (* Clear the alpha tables *)
  H.clear globalDefinitions;
  H.clear globalTypeTags;
  collectGlobals files; (* Collect all the globals *)
  theProgram := [];
  let doOneFile (f: Cabs.file) = 
    H.clear env; (* Start with a brand new environment *)
    H.clear reused;
    (* Find the new names for type and tags *)
    if debugTypes then 
      ignore (E.log "Combining a file\n");
    changeDefsIntoRefs := false;
    findTypeTagNames f;
    changeDefsIntoRefs := true;
    (* Now apply the new names and while doing that remove some global 
     * declarations *)
    let doOneDefinition = function
        DECDEF ((s, inl), l) as d -> 
          currentLoc := l;
          if isStatic s then begin
            (* We rename all static declarations. No attempt to share them *)
            List.iter (fun ((n, _,_), _) -> 
              if not (H.mem env (EVar, n)) then begin
                let n' = newAlphaName EVar n in
                H.add env (EVar, n) n'
              end) inl;
            theProgram := renameDefinition d :: !theProgram
          end else begin
            (* We apply the renaming to the declaration *)
            let d' = renameDefinition d in
            (* We try to reuse function prototypes for non-static decls *)
            if List.for_all 
                (fun ((_, dt, _), _) -> 
                  (* See if this is a function type *)
                  let rec isFunctionType = function
                      PROTO (JUSTBASE, _, _) -> true
                    | PROTO (dt, _, _) -> isFunctionType dt
                    | PARENTYPE (_, dt, _) -> isFunctionType dt
                    | ARRAY (dt, _) -> isFunctionType dt
                    | PTR (_, dt) -> isFunctionType dt
                    | JUSTBASE -> false
                  in
                  isFunctionType dt) inl then
              if H.mem globalDefinitions d' then 
                () (* Drop it *)
              else begin
                H.add globalDefinitions d' true;
                theProgram := d' :: !theProgram
              end
                
            else
              theProgram := d' :: !theProgram
          end
            

      | FUNDEF ((s, (n, dt, a)), b, l) as d -> 
          currentLoc := l;
         (* Force inline functions to be static. Otherwise the compiler might 
          * complain that the function is declared with multiple bodies  *)
          let s' = 
            if isInline s && not (isStatic s) then
              SpecStorage STATIC :: s else s
          in
          if isStatic s' then begin
            (* We must change the name *)
            if not (H.mem env (EVar, n)) then
              let n' = newAlphaName EVar n in
              H.add env (EVar, n) n'
          end;
          let d' = renameDefinition d in
          (* Try to drop duplicate inline functions *)
          if isInline s' then begin
            assert (isStatic s');
            (* But beware that we have changed the name. Make a defintion 
             * with the original name for purpose of searching in existing 
             * definitions. If the function was recursive then we are 
             * guaranteed not to find it.  *)
            let d'' = match d' with 
              FUNDEF ((s', (_, dt', a')), b', l') -> 
                FUNDEF ((s', (n, dt', a')), b', l')
            | _ -> E.s (E.bug "rename FUNDEF")
            in
            if H.mem globalDefinitions d'' then begin
              H.add env (EVar, n) n (* Set the name back to original *)
            end else begin
              (* Add the definition only if the name is the original (the 
               * first definition) *)
              if lookup EVar n = n then 
                H.add globalDefinitions d' true;
              theProgram := d' :: !theProgram
            end
          end else
            theProgram := d' :: !theProgram


      | TYPEDEF ((s, nl), l) ->
          let s' = visitCabsSpecifier renameVisitor s in
          let rec loop = function
              [] -> []
            | ((n, dt, a) as nm) :: rest -> 
                if H.mem reused (EType, n) then 
                  loop rest
                else
                  visitCabsName renameVisitor NType s' nm :: loop rest
          in
          let nl' = loop nl in
          theProgram := 
             (if nl == [] then ONLYTYPEDEF (s',l) else TYPEDEF ((s', nl'), l))
             :: !theProgram
                  
      | d -> theProgram := (visitCabsDefinition renameVisitor d) @ !theProgram
    in
    List.iter doOneDefinition f
  in
  List.iter doOneFile files;
  let res = List.rev !theProgram in
  (* Make the GC happy *)
  resetAlpha ();
  theProgram := [];
  H.clear globals;
  H.clear env;
  H.clear reused;
  H.clear globalDefinitions;
  H.clear globalTypeTags;
  res

