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
 * 1. XSRedistributions of source code must retain the above copyright notice, 
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

open Pretty
open Trace      (* sm: 'trace' function *)
open Cil
module E = Errormsg
module H = Hashtbl

let debugMerge = true

let lu = locUnknown

(* A name is identified by the index of the file in which it occurs (starting 
 * at 0 with the first file) and by the actual name. We'll keep name spaces 
 * separate *)

(* We define a data structure for the equivalence classes *)
type 'a node = 
    { nname: string;   (* The actual name *)
      nfidx: int;      (* The file index *)
      ndata: 'a;       (* Data associated with the node *)
      mutable nloc: location;  (* location where defined *)
      mutable nrep: 'a node;  (* A pointer to another node in its class (one 
                               * closer to the representative). The nrep node 
                               * is always in an earlier file, except for the 
                               * case where a name is undefined in one file 
                               * and defined in a later file. If this pointer 
                               * points to the node itself then this is the 
                               * representative.  *)
    } 

(* Make a node with a self loop. This is quite tricky. *)
let mkSelfNode (eq: (int * string, 'a node) H.t)
               (fidx: int) (name: string) (data: 'a) (l: location) = 
  let res = { nname = name; nfidx = fidx; ndata = data; nloc = l;
              nrep  = Obj.magic 1} in
  res.nrep <- res; (* Make the self cycle *)
  H.add eq (fidx, name) res; (* Add it to the proper table *)
  res

(* Find the representative with or without path compression *)
let rec find (pathcomp: bool) (nd: 'a node) = 
  if nd.nrep == nd then nd 
  else begin
    let res = find pathcomp nd.nrep in
    if pathcomp then nd.nrep <- res; (* Compress the paths *)
    res
  end


(* Union two nodes. For each node we also have a boolean that says "try not 
 * to make this representative". We return a function for undoing the union. 
 * Make sure that between the union and the undo you do not do path 
 * compression  *)
let union (nd1: 'a node) (try_not1: bool) 
          (nd2: 'a node) (try_not2: bool) : unit -> unit = 
   if nd1.nfidx < nd2.nfidx && (not try_not1 || try_not2) then begin
     (* Make 1 the reprsentative *)
     let old2rep = nd2.nrep in
     nd2.nrep <- nd1; 
     fun () -> nd2.nrep <- old2rep
   end else begin
     (* Make 2 the reprsentative *)
     let old1rep = nd1.nrep in
     nd1.nrep <- nd2; 
     fun () -> nd1.nrep <- old1rep
   end
     
(* Find the representative for a node and compress the paths in the process*)
let findRepresentative
    (pathcomp: bool)
    (eq: (int * string, 'a node) H.t)
    (fidx: int)
    (name: string) : 'a option =
  try
    let nd = H.find eq (fidx, name) in
    if nd == nd.nrep then None else Some (find pathcomp nd).ndata
  with Not_found -> 
    None

(* Dump a graph *)
let dumpGraph (what: string) (eq: (int * string, 'a node) H.t) : unit = 
  ignore (E.log "Equivalence graph for %s is:\n" what);
  H.iter (fun (fidx, name) nd -> 
    ignore (E.log "  %s(%d) -> " name fidx);
    if nd == nd.nrep then 
      ignore (E.log "*\n")
    else
      ignore (E.log " %s(%d)\n" nd.nrep.nname nd.nrep.nfidx ))
    eq

(* Make a node if one does not already exists *)
let getNode    (eq: (int * string, 'a node) H.t)
               (fidx: int) (name: string) (data: 'a) (l: location) = 
  try
    let res = H.find eq (fidx, name) in
    (* Maybe we have a better location now *)
    if res.nloc == lu && l != lu then res.nloc <- l;
    find false res (* No path compression *)
  with Not_found -> 
    mkSelfNode eq fidx name data l




(* For each name space we define a set of equivalence classes *)
let vEq: (int * string, varinfo node) H.t = H.create 111 (* Vars *)
let sEq: (int * string, compinfo node) H.t = H.create 111 (* Structs *)
let uEq: (int * string, compinfo node) H.t = H.create 111 (* Unions *)
let eEq: (int * string, enuminfo node) H.t = H.create 111 (* Enums *)
let tEq: (int * string, (string * typ) node) H.t = H.create 111 (* Type names*)
        

(** A global environment for variables. Put in here only the non-static 
  * variables, indexed by their name.  *)
let vEnv : (string, varinfo node) H.t = H.create 111

(* A file-local environment for type names *)
let tEnv : (string, string * typ)  H.t = H.create 111


(** A number of alpha conversion tables *)
let vAlpha : (string, int ref) H.t = H.create 57 (* Variables *)
let sAlpha : (string, int ref) H.t = H.create 57 (* Structures *)
let uAlpha : (string, int ref) H.t = H.create 57 (* Unions *)
let eAlpha : (string, int ref) H.t = H.create 57 (* Enumerations *)
let tAlpha : (string, int ref) H.t = H.create 57 (* Type names *)

  (* Accumulate here the globals in the merged file *)
let theFileTypes = ref [] 
let theFile      = ref []

(* The idnex of the current file being scanned *)
let currentFidx = ref 0 

let pushGlobal (g: global) : unit = theFile := g :: !theFile
let pushGlobals gl = List.iter pushGlobal gl
    
(* Initialize the module *)
let init () = 
  H.clear tAlpha;
  H.clear sAlpha;
  H.clear uAlpha;
  H.clear eAlpha;
  H.clear vAlpha;

  H.clear vEnv;
  H.clear tEnv;

  H.clear vEq;
  H.clear sEq;
  H.clear uEq;
  H.clear eEq;
  H.clear tEq;

  theFile := [];
  theFileTypes := [];

  currentFidx := 0


      


    (* Combine the types. Raises the Failure exception with an error message. 
     * isdef says whether the new type is for a definition *)
type combineWhat = 
    CombineFundef (* The new definition is for a function definition. The old 
                   * is for a prototype *)
  | CombineFunarg (* Comparing a function argument type with an old prototype 
                   * arg *)
  | CombineFunret (* Comparing the return of a function with that from an old 
                   * prototype *)
  | CombineOther


let rec combineTypes (what: combineWhat) 
                     (oldfidx: int)  (oldt: typ) 
                     (fidx: int) (t: typ)  : typ = 
  match oldt, t with
  | TVoid olda, TVoid a -> TVoid (addAttributes olda a)
  | TInt (oldik, olda), TInt (ik, a) -> 
      let combineIK oldk k = 
        if oldk == k then oldk else
        (* GCC allows a function definition to have a more precise integer 
         * type than a prototype that says "int" *)
        if not !msvcMode && oldk = IInt && bitsSizeOf t <= 32 
           && (what = CombineFunarg || what = CombineFunret) 
        then 
          k
        else
          raise (Failure "(different integer types)")
      in
      TInt (combineIK oldik ik, addAttributes olda a)

  | TFloat (oldfk, olda), TFloat (fk, a) -> 
      let combineFK oldk k = 
        if oldk == k then oldk else
        (* GCC allows a function definition to have a more precise integer 
         * type than a prototype that says "double" *)
        if not !msvcMode && oldk = FDouble && k = FFloat  
           && (what = CombineFunarg || what = CombineFunret) 
        then 
          k
        else
          raise (Failure "(different floating point types)")
      in
      TFloat (combineFK oldfk fk, addAttributes olda a)

  | TEnum (oldei, olda), TEnum (ei, a) -> begin
      let origoldei = oldei in (* Save the original for the end *)
      (* Find the node for this enum, no path compression. *)
      let oldeinode = getNode eEq oldfidx oldei.ename oldei locUnknown in
      let einode    = getNode eEq fidx ei.ename ei locUnknown in
      if oldeinode == einode then (* We already know they are the same *)
        TEnum (oldei, addAttributes olda a) 
      else begin
        (* Replace with the representative data *)
        let oldei = oldeinode.ndata in
        let oldfidx = oldeinode.nfidx in
        let ei = einode.ndata in
        let fidx = einode.nfidx in
        (* We do not have a mapping. They better be defined in the same way *)
        if List.length oldei.eitems <> List.length ei.eitems then 
          raise (Failure "(different number of enumeration elements)");
        (* We check that they are defined in the same way. This is a fairly 
         * conservative check. *)
        List.iter2 
          (fun (old_iname, old_iv) (iname, iv) -> 
            if old_iname <> iname then 
              raise (Failure "(different names for enumeration items)");
            if old_iv <> iv then 
              raise (Failure "(different values for enumeration items)"))
          oldei.eitems ei.eitems;
        (* We get here if the enumerations match *)
        oldei.eattr <- addAttributes oldei.eattr ei.eattr;
        (* Set the representative *)
        let _ = union oldeinode false einode false in
(*        if debugMerge then 
          ignore (E.log "  Renaming enum %s to %s\n" ei.ename  oldei.ename);*)
        TEnum (origoldei, addAttributes olda a)
      end
  end
        (* Strange one. But seems to be handled by GCC *)
  | TEnum (oldei, olda) , TInt(IInt, a) -> TEnum(oldei, 
                                                 addAttributes olda a)
        (* Strange one. But seems to be handled by GCC *)
  | TInt(IInt, olda), TEnum (ei, a) -> TEnum(ei, addAttributes olda a)
        
        
  | TComp (oldci, olda) , TComp (ci, a) -> begin
      let origoldci = oldci in
      if oldci.cstruct <> ci.cstruct then 
        raise (Failure "(different struct/union types)");
      (* See if we have a mapping already *)
      let eqH = if ci.cstruct then sEq else uEq in
      (* Make the nodes if not already made. Actually return the 
       * representatives *)
      let oldcinode = getNode eqH oldfidx oldci.cname oldci lu in
      let    cinode = getNode eqH    fidx    ci.cname    ci lu in 
      if oldcinode == cinode then (* We already know they are the same *)
        TComp (origoldci, addAttributes olda a) 
      else begin
        (* Replace with the representative data *)
        let oldci = oldcinode.ndata in
        let oldfidx = oldcinode.nfidx in
        let ci = cinode.ndata in
        let fidx = cinode.nfidx in

        let old_len = List.length oldci.cfields in
        let len = List.length ci.cfields in
        (* It is easy to catch here the case when the new structure is 
         * undefined and the old one was defined. We just reuse the old *)
        (* More complicated is the case when the old one is not defined but 
         * the new one is. We still reuse the old one and we'll take care of 
         * defining it later with the new fields. *)
        if len <> 0 && old_len <> 0 && old_len <> len then 
          raise (Failure "(different number of structure fields)");
        ignore (E.log "Comparing %s(%d) with %s(%d)\n"
                  (compFullName oldci) oldfidx
                  (compFullName ci) fidx);
        (* We check that they are defined in the same way. While doing this 
         * there might be recursion and we have to watch for going into an 
         * infinite loop. So we add the assumption that they are equal *)
        let undo = union oldcinode (old_len = 0) cinode (len = 0) in
        (* We check the fields but watch for Failure. We only do the check 
         * when the lengths are the same. Due to the code above this the 
         * other possibility is that one of the length is 0, in which case we 
         * reuse the old compinfo. *)
        if old_len = len then
          (try
            List.iter2 (fun oldf f -> 
              if oldf.fbitfield <> f.fbitfield then 
                raise (Failure "(different bitfield info)");
              if oldf.fattr <> f.fattr then 
                raise (Failure "(different field attributes)");
              (* Make sure the types are compatible *)
              oldf.ftype <- 
                 combineTypes CombineOther oldfidx oldf.ftype fidx f.ftype;
            ) oldci.cfields ci.cfields
          with Failure reason -> begin 
            (* Our assumption was wrong. Forget the isomorphism *)
            undo ();
            let msg = 
              sprint ~width:80
                (dprintf
                   "\n\tFailed assumption that %s and %s are isomorphic %s"
                   (compFullName oldci) (compFullName ci) reason) in
            raise (Failure msg)
          end);
        (* We get here when we succeeded checking that they are equal *)
        oldci.cattr <- addAttributes oldci.cattr ci.cattr;
(*        if debugMerge then 
          ignore (E.log " Renaming %s to %s\n" (compFullName ci) oldci.cname);
*)
        TComp (origoldci, addAttributes olda a) 
      end
  end

  | TArray (oldbt, oldsz, olda), TArray (bt, sz, a) -> 
      let newbt = combineTypes CombineOther 0 oldbt 0 bt in
      let newsz = 
        if oldsz = sz then sz else
        match oldsz, sz with
          None, Some _ -> sz
        | Some _, None -> oldsz
        | _ -> raise (Failure "(different array lengths)")
      in
      TArray (newbt, newsz, addAttributes olda a)
        
  | TPtr (oldbt, olda), TPtr (bt, a) -> 
      TPtr (combineTypes CombineOther oldfidx oldbt fidx bt, 
            addAttributes olda a)
        
  | TFun (_, _, _, [Attr("missingproto",_)]), TFun _ -> t
  | TFun _, TFun (_, _, _, [Attr("missingproto",_)]) -> oldt
        
  | TFun (oldrt, oldargs, oldva, olda), TFun (rt, args, va, a) ->
      let newrt = combineTypes 
          (if what = CombineFundef then CombineFunret else CombineOther) 
          oldfidx oldrt fidx rt 
      in
      if oldva != va then 
        raise (Failure "(diferent vararg specifiers)");
      (* If one does not have arguments, believe the one with the 
      * arguments *)
      let newargs = 
        if oldargs = None then args else
        if args = None then oldargs else
        let oldargslist = argsToList oldargs in
        let argslist = argsToList args in
        if List.length oldargslist <> List.length argslist then 
          raise (Failure "(different number of arguments)")
        else begin
          (* Go over the arguments and update the old ones with the 
          * adjusted types *)
          List.iter2 
            (fun oldarg arg -> 
              (* Update the names. Always prefer the new name. This is very 
               * important if the prototype uses different names than the 
               * function definition. *)
              if arg.vname <> "" then oldarg.vname <- arg.vname;
              oldarg.vattr <- addAttributes oldarg.vattr arg.vattr;
              oldarg.vtype <- 
                 combineTypes 
                   (if what = CombineFundef then 
                     CombineFunarg else CombineOther) 
                   oldfidx oldarg.vtype fidx arg.vtype)
            oldargslist argslist;
          oldargs
        end
      in
      TFun (newrt, newargs, oldva, addAttributes olda a)
        
  | TNamed (oldn, oldt, olda), TNamed (n, t, a) -> begin
      (* Find the node for this enum, no path compression. *)
      let oldtnode = getNode tEq oldfidx oldn (oldn, oldt) locUnknown in
      let    tnode = getNode tEq    fidx    n (   n,    t) locUnknown in
      if oldtnode == tnode then (* We already know they are the same *)
        TNamed(oldn, oldt, addAttributes olda a) 
      else begin
        (* Replace with the representative data *)
        let (oldn, oldt) = oldtnode.ndata in
        let oldfidx = oldtnode.nfidx in
        let (n, t) = tnode.ndata in
        let fidx = tnode.nfidx in
        (* Check that they are the same *)
        (try
          ignore (combineTypes CombineOther oldfidx oldt fidx t);
        with Failure reason -> begin
          let msg = 
            sprint ~width:80
              (dprintf
                 "\n\tFailed assumption that %s and %s are isomorphic %s"
                 oldn n reason) in
          raise (Failure msg)
        end);
        let _ = union oldtnode false tnode false in
(*        if debugMerge then 
          ignore (E.log " Renaming type name %s to %s\n" n oldn); *)
        TNamed (oldn, oldt, addAttributes olda a)
      end
  end
        
        (* Unroll first the new type *)
  | _, TNamed (n, t, a) -> 
      let res = combineTypes what oldfidx oldt fidx t in
      typeAddAttributes a res
        
        (* And unroll the old type as well if necessary *)
  | TNamed (oldn, oldt, a), _ -> 
      let res = combineTypes what oldfidx oldt fidx t in
      typeAddAttributes a res
        
  | _ -> raise (Failure "(different type constructors)")


(** A visitor the renames uses of variables and types *)      
class renameVisitorClass = object (self)
  inherit nopCilVisitor 
      
      (* This is either a global variable which we took care of, or a local 
       * variable. Must do its type and attributes. *)
  method vvdec (vi: varinfo) = DoChildren

      (* This is a variable use. See if we must change it *)
  method vvrbl (vi: varinfo) : varinfo visitAction = 
    if not vi.vglob then DoChildren else
    match findRepresentative true vEq !currentFidx vi.vname with
      None -> DoChildren
    | Some vi' -> ChangeTo vi'

          
        (* The use of a type *)
  method vtype (t: typ) = 
    match t with 
      TComp (ci, a) -> begin
        let eqH = if ci.cstruct then sEq else uEq in
        match findRepresentative true eqH !currentFidx ci.cname with
          None -> DoChildren
        | Some ci' -> 
            ChangeTo (TComp (ci', visitCilAttributes (self :> cilVisitor) a))
      end
    | TEnum (ei, a) -> begin
        match findRepresentative true eEq !currentFidx ei.ename with
          None -> DoChildren
        | Some ei' -> 
            ChangeTo (TEnum (ei', visitCilAttributes (self :> cilVisitor) a))
      end

    | TNamed (tn, typ, a) -> begin
        try
          let tn', typ' = H.find tEnv tn in
          let a' = visitCilAttributes (self :> cilVisitor) a in
          if tn = tn' && typ == typ' && a = a' then SkipChildren else
          (* No need to visit the types because they are actually done 
             already *)
          ChangeTo (TNamed(tn', typ', a'))
        with Not_found -> 
          (* This must be a type name that was already changed to point to an 
           * old type. Otherwise we would see it in the map *)
          SkipChildren
    end 
        
    | _ -> DoChildren
end

let renameVisitor = new renameVisitorClass

(* Scan all files and do two things *)
(* 1. Initialize the alpha renaming tables with the names of the globals so 
 * that when we come in the second pass to generate new names, we do not run 
 * into conflicts.  *)
(* 2. For all declarations of globals unify their types. In the process 
 * construct a set of equivalence classes on type names, structure and 
 * enumeration tags  *)

let rec oneFilePass1 (f:file) : unit = 
  if debugMerge || !E.verboseFlag then 
    ignore (E.log "Pre-merging (%d) %s\n" !currentFidx f.fileName);

  if f.globinitcalled || f.globinit <> None then
    E.s (E.warn "Merging file %s has global initializer" f.fileName);

  (* We scan each file and we look at all global varinfo. We see if globals 
   * with the same name have been encountered before and we merge those types 
   * *)
  let matchVarinfo (vi: varinfo) (l: location) = 
    ignore (E.log "matchVarinfo %s\n" vi.vname);
    ignore (registerAlphaName vAlpha vi.vname);
    (* Make a node for it and put it in vEq *)
    let vinode = mkSelfNode vEq !currentFidx vi.vname vi l in
    try
      let oldvinode = H.find vEnv vi.vname in 
      let oldvi     = oldvinode.ndata in
      (* There is an old definition. We must combine the types. Do this first 
      * because it might fail  *)
      (try
        oldvi.vtype <- 
           combineTypes CombineOther 
             oldvinode.nfidx oldvi.vtype  
             !currentFidx vi.vtype;
      with (Failure reason) -> begin
        ignore (warn "Incompatible declaration for %s. Previous was at %a %s" 
                  vi.vname d_loc oldvinode.nloc reason);
        raise Not_found
      end);
      (* Combine the attributes as well *)
      oldvi.vattr <- addAttributes oldvi.vattr vi.vattr;
      (* clean up the storage.  *)
      let newstorage = 
        if vi.vstorage = oldvi.vstorage || vi.vstorage = Extern then 
          oldvi.vstorage 
        else if oldvi.vstorage = Extern then vi.vstorage 
        else begin
          ignore (warn "Inconsistent storage specification for %s. Previous was at: %a" 
                    vi.vname d_loc oldvinode.nloc);
          vi.vstorage
        end
      in
      oldvi.vstorage <- newstorage;
      let _ = union oldvinode false vinode false in ()
    with Not_found -> (* Not present in the previous files. Remember it for 
                       * later  *)
      H.add vEnv vi.vname vinode

  in
  List.iter
    (function 
      | GDecl (vi, l) | GVar (vi, _, l) -> 
          currentLoc := l;
          if vi.vstorage <> Static then 
            matchVarinfo vi l
      | GFun (fdec, l) -> 
          currentLoc := l;
          (* Force inline functions to be static *) 
          if fdec.sinline && fdec.svar.vstorage = NoStorage then 
            fdec.svar.vstorage <- Static;
          if fdec.svar.vstorage <> Static then 
            matchVarinfo fdec.svar l
      | _ -> ())
    f.globals


  
  (* Now we go once more through the file and we rename the globals that we 
   * keep. We also scan the entire body and we replace references to the 
   * representative types or variables *)
let oneFilePass2 (f: file) = 
  if debugMerge || !E.verboseFlag then 
    ignore (E.log "Final merging phase (%d): %s\n" 
              !currentFidx f.fileName);
  let processOneGlobal (g: global) : unit = 
      (* Process a varinfo. Reuse an old one, or rename it if necessary *)
    let processVarinfo (vi: varinfo) (vloc: location) : varinfo =  
      (* Maybe it is static. Rename it then *)
      if vi.vstorage = Static then begin
        vi.vname <- newAlphaName vAlpha vi.vname;
        vi.vid <- H.hash vi.vname;
        vi
      end else begin
        (* Find the representative *)
        match findRepresentative true vEq !currentFidx vi.vname with
          None -> vi
        | Some vi' -> vi'
      end
    in
    try
      match g with 
      | GDecl (vi, l) as g -> 
          currentLoc := l;
          let vi' = processVarinfo vi l in
          if vi != vi' then (* Drop this declaration *) () else
          pushGlobals (visitCilGlobal renameVisitor g)
            
      | GVar (vi, init, l) as g -> 
          currentLoc := l;
          let vi' = processVarinfo vi l in
          (* We must keep this definition even if we reuse this varinfo, 
          * because maybe the previous one was a declaration *)
          pushGlobals (visitCilGlobal renameVisitor (GVar(vi', init, l)))
            
      | GFun (fdec, l) as g -> 
          currentLoc := l;
          let vi' = processVarinfo fdec.svar l in
          (* We keep it anyway. Like a definition. *)
          pushGlobals (visitCilGlobal renameVisitor g)
            
      | GCompTag (ci, l) as g -> begin
          currentLoc := l;
          let eqH, alphaH = 
            if ci.cstruct then sEq, sAlpha else uEq, uAlpha 
          in
          match findRepresentative true sEq !currentFidx ci.cname with
            None -> 
              (* A new one, we must rename it and keep the definition *)
              ci.cname <- newAlphaName alphaH ci.cname;
              ci.ckey <- H.hash (compFullName ci);
              pushGlobals (visitCilGlobal renameVisitor g)
          | Some oldci -> begin
              (* This is not the representative *)
              (* See if the old one is empty and the new one is not *)
              if oldci.cfields = [] then begin
                (* Define the old compinfo here (because it was not defined) 
                * but put our fields in it first ! *)
                if debugMerge then 
                  ignore (E.log " Adding a definition for the empty %s\n"
                            (compFullName oldci));
                oldci.cfields <- 
                   List.map (fun fi -> { fi with fcomp = oldci }) ci.cfields;
                pushGlobals (visitCilGlobal renameVisitor (GCompTag(oldci, l)))
              end else (* It is an old structure that is not empty. Drop this 
                * declaration because we'll not be using it. *)
                ()
          end
      end  
      | GEnumTag (ei, l) as g -> begin
          currentLoc := l;
          match findRepresentative true eEq !currentFidx ei.ename with 
            None -> (* We must rename it *)
              ei.ename <- newAlphaName eAlpha ei.ename;
              pushGlobals (visitCilGlobal renameVisitor g);
          | Some _ -> () (* Drop this since we are reusing it from before *)
      end
      | GType (n, t, l) as g -> begin
          currentLoc := l;
          if n = "" then begin (* This is here just to introduce an undefined 
                                * structure  *)
            let t'  = visitCilType renameVisitor t in
            pushGlobal (GType(n, t', l))
          end else begin
            let nt' = 
              match findRepresentative true tEq !currentFidx n with
                None -> (* We must rename it *)
                  let n' = newAlphaName tAlpha n in
                  let t'  = visitCilType renameVisitor t in
                  pushGlobal (GType(n', t', l));
                  n', t'
              | Some (n', t') -> n', t' (* And we drop it *)
            in
            (* Add it to the local environment so we know how to rename types*)
            H.add tEnv n nt';
          end
      end
      | g -> pushGlobals (visitCilGlobal renameVisitor g)
  with e -> begin
    ignore (E.log "error when merging global: %s\n" (Printexc.to_string e));
    pushGlobal (GText (sprint 80 (dprintf "/* error at %t:" d_thisloc)));
    pushGlobal g;
    pushGlobal (GText ("*************** end of error*/"));
        
  end
  in
  List.iter processOneGlobal f.globals


let merge (files: file list) (newname: string) : file = 
  init ();

  (* Make the first pass over the files *)
  currentFidx := 0;
  List.iter (fun f -> oneFilePass1 f; incr currentFidx) files;

  (* Now maybe dump the graph *)
  if debugMerge then begin
    dumpGraph "type" tEq;
    dumpGraph "struct" sEq;
    dumpGraph "union" uEq;
    dumpGraph "enum" eEq;
  end;
  (* Make the second pass over the files. This is when we start rewriting the 
   * file *)
  currentFidx := 0;
  List.iter (fun f -> oneFilePass2 f; incr currentFidx) files;

  (* Now reverse the result and return the resulting file *)
  let rec revonto acc = function
      [] -> acc
    | x :: t -> revonto (x :: acc) t
  in
  let res = 
    { fileName = newname;
      globals  = revonto (revonto [] !theFile) !theFileTypes;
      globinit = None;
      globinitcalled = false } in
  init (); (* Make the GC happy *)
  res




