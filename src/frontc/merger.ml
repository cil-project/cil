








                    THIS FILE IS OBSOLETE!

               Its replacement is src/mergecil.ml










(*
 *
 * Copyright (c) 2001 by
 *  George C. Necula	necula@cs.berkeley.edu
 *  Scott McPeak        smcpeak@cs.berkeley.edu
 *  Wes Weimer          weimer@cs.berkeley.edu
 *   
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software for research purposes only is hereby granted, 
 * provided that the following conditions are met: 
 * 1. Redistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 * 3. The name of the authors may not be used to endorse or promote products 
 * derived from  this software without specific prior written permission. 
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(* Implementation of whole-program merging *)

open Cabs
open Cabsvisit
open Pretty
module E = Errormsg
module H = Hashtbl

(** Be aggressive when merging types *)
let aggressive = ref false

let renameAll = false

let debugTypes  = false
let debugFundef = false

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
    | NVar | NFun -> changeName true EVar n (fun n' -> (n', dt, a))


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
 * numeric suffix (-1 if the separator is empty or if ___ is the last thing 
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
 * tag kind then the name is empty and the specifier has one element which is 
 * the definition of the tag *)
type typeTagDef = envKind * string * specifier * name * cabsloc
let fileTypeTags: (envKind * string, specifier * name * cabsloc) H.t 
    = H.create 53

(* Put some effort into replacing type names with types. But since we can 
 * easily get confused in the presence of attributes, do it only when it is 
 * easy *)
let debugApplyNames = false
let rec applyNamesToSpecName 
          (s: specifier) 
          (((ns, dt, al) as nm) : name) : specifier * name = 
    (* Go through the specifiers and see if you find a defined type. Raise 
     * Not_found if cannot make the change. Whether or not this is possible 
     * does not depend on nm *)
    let rec loopSpec = function
        [] -> [], nm
      | (SpecType (Tnamed s)) :: rests -> begin
          (* Find how is this type defined *)
          let sdef, (_, tdt, ta), _ = 
            H.find fileTypeTags (EType, s) in  (* raises Not_found *)
          (* We don't like attributes in sdef *)
          if ta <> [] || List.exists (function SpecAttr _ -> true 
            | _ -> false) sdef then 
            begin
              if debugApplyNames then
                ignore (E.log "applyNames: not replacing %s because it has attributes"
                          s);
              raise Not_found;
            end;
          (* Plug the dt instead of JUSTBASE in ndef, but only there are no 
            * attributes modifying JUSTBASE *)
          let rec plug (hasattr: bool) = function
              JUSTBASE -> (if hasattr then raise Not_found); dt
            | PARENTYPE (prea, tdt, posta) -> 
                PARENTYPE (prea, plug (prea <> [] || posta <> []) tdt, posta)
            | ARRAY(tdt, e) -> ARRAY(plug false tdt, e)
            | PTR (al, tdt) -> PTR (al, plug (al <> []) tdt)
            | PROTO (dt, args, isva) -> PROTO (plug false dt, args, isva)
          in
          if debugApplyNames then
            ignore (E.log "Applied name %s\n" s);
          (List.filter (function SpecTypedef -> false | _ -> true) sdef) 
          @ rests, (ns, plug false tdt, al)
      end
          
      | s :: rests -> 
          let rests', n' = loopSpec rests in
          s :: rests', n'
    in
    try
      loopSpec s
    with Not_found -> 
      s, nm
        
let applyNamesToFieldGroup ((s, nl) as fgl) = 
  match nl with 
  | [] -> fgl
        (* Do the first name and get the changed s' *)
  | (nm, e) :: restnl ->
      let s', nm' = applyNamesToSpecName s nm in
      let restnl' = 
        List.map 
          (fun (nm, e) -> 
            let s'', nm'' = applyNamesToSpecName s nm in
            assert (s'' = s');
            (nm'', e)) 
          restnl
      in
      (s', (nm', e) :: restnl')

  
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
            let s', nm' = applyNamesToSpecName s nm in
(*            ignore (E.log "Adding typedef of %s to fileTypeTags\n" n); *)
            H.add fileTypeTags (EType, n) (s', nm', !visitorLocation)) nl;
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
          let flds' = List.map applyNamesToFieldGroup flds in
          H.add fileTypeTags (EStruct, n) 
            ([SpecType (Tstruct(n, Some flds'))], emptyName, !visitorLocation)
    | Tunion (n, Some flds) -> 
        if n <> "" then
          let flds' = List.map applyNamesToFieldGroup flds in
          H.add fileTypeTags (EUnion, n) 
            ([SpecType (Tunion(n, Some flds'))], emptyName, !visitorLocation)
    | Tenum (n, Some eil) ->
        if n <> "" then   
          H.add fileTypeTags (EEnum, n) 
            ([SpecType ts], emptyName, !visitorLocation)
    | _ -> ());
    DoChildren

end
let collectTypeTagDefs = new collectTypeTagDefsClass


(* Define a visitor that strips location information from a definition. We do 
 * this when we want to locate duplicates *)
let noCabsLoc = { lineno = 0; filename = "" }
class stripLocationClass  : cabsVisitor = object (self)
  inherit nopCabsVisitor

  method vdef d = 
    let stripdef = function
        FUNDEF (sn, b, _) -> FUNDEF (sn, b, noCabsLoc)
      | DECDEF (ing, _) -> DECDEF (ing, noCabsLoc)
      | TYPEDEF (ng, _) -> TYPEDEF (ng, noCabsLoc)
      | ONLYTYPEDEF (s, _) -> ONLYTYPEDEF (s, noCabsLoc)
      | d -> d
    in
    let stripdeflist = function
        [d] -> [stripdef d]
      | _ -> E.s (E.bug "stripLocation.vdef")
    in
    ChangeDoChildrenPost ([d], stripdeflist)

  method vstmt s = 
    let stripstmt = function
        NOP _ -> NOP noCabsLoc
      | COMPUTATION (e, _) -> COMPUTATION (e, noCabsLoc)
      | BLOCK (b, _) -> BLOCK (b, noCabsLoc)
      | SEQUENCE (s1, s2, _) -> SEQUENCE (s1, s2, noCabsLoc)
      | IF(e, s1, s2, _) -> IF (e, s1, s2, noCabsLoc)
      | WHILE(e, s, _) -> WHILE(e, s, noCabsLoc)
      | DOWHILE(e, s, _) -> WHILE(e, s, noCabsLoc)
      | FOR(e1,e2,e3,s,_) -> FOR(e1,e2,e3,s,noCabsLoc) 
      | BREAK _ -> BREAK noCabsLoc
      | CONTINUE _ ->CONTINUE noCabsLoc  
      | RETURN (e, _) -> RETURN (e, noCabsLoc)
      | SWITCH (e, s, _) -> SWITCH (e, s, noCabsLoc)
      | CASE (e, s, _) -> CASE (e, s, noCabsLoc) 
      | CASERANGE (e1, e2, s, _) -> CASERANGE (e1, e2, s, noCabsLoc)
      | DEFAULT (s, _) -> DEFAULT (s, noCabsLoc)
      | LABEL (l, s, _) -> LABEL (l, s, noCabsLoc) 
      | GOTO (l, _) -> GOTO (l, noCabsLoc)
      | COMPGOTO (e, _) -> COMPGOTO (e, noCabsLoc) 
      | ASM (templ, vol, outs, ins, mods, _) ->
          ASM (templ, vol, outs, ins, mods, noCabsLoc)
    in
    let stripstmtlist = function
        [s] -> [stripstmt s]
      | _ -> E.s (E.bug "stripLocation.vstmt")
    in
    ChangeDoChildrenPost ([s], stripstmtlist)
end

let stripLocationVisitor = new stripLocationClass

let stripLocation (g: definition) : definition = 
  match visitCabsDefinition stripLocationVisitor g with
    [d] -> d
  | _ -> E.s (E.bug "stripLocationVisitor returns too many definitions")
  



(* Equality constraints. The first string is always from the program already 
 * merged and the new one if from the file being merged. *)
type eqConstraint = envKind * string * string

let d_eqc () ((ek, oldn, newn) : eqConstraint) : doc = 
  dprintf "%s(%s, %s)" (ek2text ek) oldn newn
let d_eqc_list () (el : eqConstraint list) : doc = 
  dprintf "@[%a@]" (docList (chr ',' ++ break) (d_eqc ())) el

let alreadyKnownToBeEqual (eqc : eqConstraint) (acc: eqConstraint list) = 
  List.exists (fun x -> x = eqc) acc


(* Compare two specifiers and collect a number of constraints that must all 
 * be satisfied for the specifiers to be isomorphic *)
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
       * anonymous or we are being aggressive *)
      match ts1, ts2 with 
        Tstruct (s1, fg1), Tstruct (s2, fg2) -> 
          if s1 = "" && s2 = "" then (* We always go into Anonymous structs. 
                                      * No danger of recursion here *)
            compareFieldGroupLists acc fg1 fg2
          else
            (* If only one is anonymous we cannot do anything. Too bad. *)
            let eqc = (EStruct, s1, s2) in
            if s1 = "" || s2 = "" then raise Not_found 
            else if !aggressive then 
              (* Check that we have not seen these already *)
              if alreadyKnownToBeEqual eqc acc then 
                acc
              else 
                compareFieldGroupLists (eqc :: acc) fg1 fg2
            else (* Not aggressive. Do not go into the struct *)
              eqc :: acc
                                     
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
  (* Names of fields, attributes and decl_types must be the same *)
  if fg1 <> fg2 then raise Not_found;
  compareSpecs s1 s2 acc

and compareFieldGroupLists acc fgl1 fgl2 =
  match fgl1, fgl2 with 
    Some fgl1, Some fgl2 -> List.fold_left2 compareFieldGroups acc fgl1 fgl2
  | None, None -> acc
  | _, _ -> raise Not_found
      

(* Store some global definitions for typeTags. For each orignal name and kind 
 * we store the list of all defintions for that name that were encountered in 
 * the whole program. If we are in aggressive merging mode then all the names 
 * in the hash key are the empty string. *)
let globalTypeTags : (envKind * string, typeTagDef) H.t = H.create 111

(** Just a set of defined global type tags *)
let globalDefinedTypeTags : (envKind * string, bool) H.t = H.create 111

let dumpGlobalTypeTags (msg: string) = 
  Cprint.force_new_line ();
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
  Cprint.force_new_line ();
  flush !Cprint.out;
  Cprint.out := old
  
let dumpFileTypeTags (msg: string) = 
  Cprint.force_new_line ();
  let old = !Cprint.out in
  Cprint.out := stderr;
  Cprint.print msg;
  H.iter (fun (k, n) (specs, nm, _) -> 
    Cprint.print ("Node(" ^ ek2text k ^ "," ^ n ^ ") -> ");
    Cprint.print_specifiers specs;
    Cprint.print ",";
    Cprint.print_name nm;
    Cprint.print ")\n")
    fileTypeTags;
  Cprint.force_new_line ();
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
    (* All previous typeTagDefs for the same original name. If in aggressive 
     * mode then look for all definitions of the same kind. *)
    let defn' = if !aggressive then "" else defn in
    let old = H.find_all globalTypeTags (defk, defn')  in
    List.iter
      (fun (_, oldn, oldspec, ((_, old_dt, old_a) as oldname), oldloc) -> 
        (* Make a node *)
        let node = getNode (defk, oldn, defn) in
        (* This node must be made eq to start with *)
        node.eq <- true;
        (* Find the equality constraints that must all hold for defn to be 
        * compatible with oldn. Watch out for Not_found *)
        try
          (* Compare the name as well, except the string part *)
          let constr = 
            match defk with 
              EType -> 
                if old_dt <> def_dt || old_a <> def_a then raise Not_found;
                if debugTypes then 
                  ignore (E.log "compareSpecs %s %s and %s\n"
                            (ek2text defk) oldn defn);
                let res = compareSpecs oldspec defspec [] in
                if debugTypes then 
                  ignore (E.log "\tsuccess with %a\n" d_eqc_list res);
                res
                
            | EStruct -> begin
                match oldspec, defspec with 
                  [SpecType (Tstruct (_, Some ofg))], 
                  [SpecType (Tstruct (_, Some nfg))] -> 
                    if debugTypes then 
                      ignore (E.log "compareFieldGroupList %s %s and %s\n"
                                (ek2text defk) oldn defn);
                    let res = 
                      compareFieldGroupLists 
                        [(defk, oldn, defn)] (Some ofg) (Some nfg) in
                    if debugTypes then 
                      ignore (E.log "\tsuccess with %a\n" d_eqc_list res);
                    res
                | _ -> E.s (E.bug "compareTypeTags(struct)")
            end
            | EUnion -> begin
                match oldspec, defspec with 
                  [SpecType (Tunion (_, Some ofg))], 
                  [SpecType (Tunion (_, Some nfg))] -> 
                    if debugTypes then 
                      ignore (E.log "compareFieldGroupList %s %s and %s\n"
                                (ek2text defk) oldn defn);
                    let res = compareFieldGroupLists [] (Some ofg) (Some nfg)
                    in
                    if debugTypes then 
                      ignore (E.log "\tsuccess with %a\n" d_eqc_list res);
                    res
                    
                | _ -> E.s (E.bug "compareTypeTags(union)")
            end
            | EEnum -> begin
                match oldspec, defspec with 
                  [SpecType (Tenum (_, Some el1))], 
                  [SpecType (Tenum (_, Some el2))] -> 
                    if debugTypes then 
                      ignore (E.log "compareEnum %s %s and %s\n"
                                (ek2text defk) oldn defn);
                    let res = if el1 = el2 then [] else raise Not_found in
                    if debugTypes then 
                      ignore (E.log "\tsuccess with %a\n" d_eqc_list res);
                    res

                | _ -> E.s (E.bug "compareTypeTags(enum)")
            end
            | _ -> E.s (E.bug "compareTypeTags(other)")
          in
          (* Add an edge in the graph for each constraint *)
          List.iter (fun ckey ->
            let node1 = getNode ckey in (* Make the node not eq if it does 
                                         * not exist already *)
            node1.succs <- node :: node1.succs) constr
        with _ -> begin (* This node cannot possibly match *)
          if debugTypes then 
            ignore (E.log "\tfailed !!!\n");
          node.eq <- false;
        end)
      old
  in
  H.iter doOne fileTypeTags;
  (* Now we can have constraint pairs when at least one is an undefined 
   * struct and the other is a defined or undefined struct with the same 
   * name. For the local tags we can look in fileTypeTags. For the global 
   * ones we have a separate hashtable. *)
  H.iter (fun (k,on,nn) nd -> 
    if (k = EStruct || k = EUnion) &&on = nn then
      if not (H.mem fileTypeTags (k, nn)) ||
         not (H.mem globalDefinedTypeTags (k, on)) then
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
  if debugTypes then dumpFileTypeTags "fileTypeTags before collect:\n";
  (* Now collect and solve the constraint graph *)
  constructConstraintGraph ();
  (* Now assign names to types and tags *)
  H.iter 
    (fun (defk, defn) (defspec, defname, defloc) -> 
      begin
        let defkn = (defk, defn) in
        (* Find the first old name that matches *)
        let defkn' = if !aggressive then (defk, "") else defkn in
        let oldnames = H.find_all globalTypeTags defkn' in
        (* Sometimes we want to rename everything *)
        let oldnames = if renameAll then [] else oldnames in
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
            if rest <> [] && not !aggressive then 
              (* We have too many warnings if we are in the aggressive mode *)
              ignore (E.warn " more than one old name found for %s!\n" defn);
            (* Add to the environment the new name *)
            H.add env defkn on;
            H.add reused defkn on
      end)
    fileTypeTags;
  (* Clean the graph *)
  H.clear constraintGraph;
  (* Add new names to globalTypeTags. Must do it this late because we 
   * must rename the typeTags and only now we have the whole renaming *)
  H.iter 
    (fun (defk, defn) (defspec, defname, defloc) -> 
      let defkn = (defk, defn) in
      if not (H.mem reused defkn) then begin
        let defn' = lookup defk defn in
        let defspec' = visitCabsSpecifier renameVisitor defspec in
        let nk = match defk with EType -> NType | _ -> NVar in
        let defname' = visitCabsName renameVisitor nk defspec' defname in
        let defkn' = if !aggressive then (defk, "") else defkn in
        H.add globalTypeTags defkn' (defk, defn', defspec', defname', defloc);
        (* And mark it as defined *)
        H.add globalDefinedTypeTags defkn true;
      end)
    fileTypeTags;
  if debugTypes then dumpGlobalTypeTags "globalTypeTags:\n"
  


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
        let islocal = 
          isStatic specs || (isInline specs && not (isExtern specs)) in
        if not islocal then registerGlobal n
        
    | DECDEF ((specs, inl), _) -> 
        let islocal = isStatic specs in
        if not islocal then 
          List.iter (fun ((n, _, _), _) -> registerGlobal n) inl

    | _ -> ()
  in
  H.clear globals;
  List.iter (fun (fname, f) -> List.iter collectOneGlobal f) fl


(* Remember some definitions so that we can drop duplicates *)
let globalDefinitions: (Cabs.definition, bool) H.t = H.create 113


let theProgram: Cabs.definition list ref = ref []

(* The MAIN MERGER *)
let merge (files : Cabs.file list) : Cabs.definition list = 
  resetAlpha (); (* Clear the alpha tables *)
  H.clear globalDefinitions;
  H.clear globalTypeTags;
  H.clear globalDefinedTypeTags;
  collectGlobals files; (* Collect all the globals *)
  theProgram := [];
  let doOneFile ((fname, f): Cabs.file) = 
    if !E.verboseFlag then 
      ignore (E.log "Merging %s\n" fname);
    H.clear env; (* Start with a brand new environment *)
    H.clear reused;
    (* Find the new names for type and tags *)
    if debugTypes then 
      ignore (E.log "Combining file %s\n" fname);
    changeDefsIntoRefs := false;
    findTypeTagNames (fname, f);
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
            let dcheck = stripLocation d' in
            (* We try to reuse function prototypes for non-static decls and 
             * extern declarations *)
            if isExtern s || 
               (List.for_all 
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
                    isFunctionType dt) inl) then 
              begin
                if H.mem globalDefinitions dcheck then 
                  () (* Drop it *)
                else begin
                  H.add globalDefinitions dcheck true;
                  theProgram := d' :: !theProgram
                end
              end
            else
              theProgram := d' :: !theProgram
          end
            

      | FUNDEF ((s, (n, dt, a)), b, l) as d -> 
          currentLoc := l;
         (* Force inline functions to be static. Otherwise the compiler might 
          * complain that the function is declared with multiple bodies  *)
          if debugFundef then 
            ignore (E.log "Doing fundef %s\n" n);
          let s' = 
            if isInline s && not (isStatic s) && not (isExtern s) then
              SpecStorage STATIC :: s else s
          in
          if isStatic s' then begin
            (* We must change the name *)
            if not (H.mem env (EVar, n)) then
              let n' = newAlphaName EVar n in
              H.add env (EVar, n) n'
          end;
          let d' = renameDefinition d in
          let n' = lookup EVar n in
          if debugFundef then 
            ignore (E.log "  new name is %s\n" n');
          (* Try to drop duplicate inline functions *)
          if isInline s' then begin
            if debugFundef then
              ignore (E.log "  is inline\n");
            (* But beware that we have changed the name. Make a definition 
             * with the original name for purpose of searching in existing 
             * definitions. If the function was recursive then we are 
             * guaranteed not to find it.  *)
            let d'' = match d' with 
              FUNDEF ((s', (_, dt', a')), b', l') -> 
                FUNDEF ((s', (n, dt', a')), b', l')
            | _ -> E.s (E.bug "rename FUNDEF")
            in
            let dcheck = stripLocation d'' in
            if H.mem globalDefinitions dcheck then begin
              if debugFundef then
                ignore (E.log "  already present in the file\n");
              H.add env (EVar, n) n (* Set the name back to original *)
            end else begin
              (* Add the definition only if the name is the original (the 
               * first definition) *)
              if debugFundef then
                ignore (E.log "  not already present in the file\n");
              if lookup EVar n = n then 
                H.add globalDefinitions dcheck true;
              theProgram := d' :: !theProgram
            end
          end else
            theProgram := d' :: !theProgram


      | TYPEDEF ((s, nl), l) ->
          let s' = visitCabsSpecifier renameVisitor s in
          let rec loop = function
              [] -> []
            | ((n, dt, a) as nm) :: rest -> begin
                try
                  let n' = H.find reused (EType, n) in
                  loop rest
                with Not_found ->
                  visitCabsName renameVisitor NType s' nm :: loop rest
            end
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
  H.clear globalDefinedTypeTags;
  res

