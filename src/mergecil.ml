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


(** For each name in the new file we map it to a corresponding name in the 
  * previously combined files, if there is such an isomorphic name *)
let vReuse: (string, varinfo) H.t = H.create 111
let sReuse: (string, compinfo) H.t = H.create 111
let uReuse: (string, compinfo) H.t = H.create 111
let eReuse: (string, enuminfo) H.t = H.create 111
let tReuse: (string, string * typ) H.t = H.create 111
        

(** A global environment for variables. Put in here only the non-static 
  * variables *)
let vEnv : (string, varinfo * location) H.t = H.create 111

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

  theFile := [];
  theFileTypes := []

(* Initialize before processing one file *)
let initOneFile () =
  H.clear vReuse;
  H.clear sReuse;
  H.clear uReuse;
  H.clear eReuse;
  H.clear tReuse;

  H.clear tEnv;
      


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


let rec combineTypes (what: combineWhat) (oldt: typ) (t: typ) : typ = 
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
          raise (Failure "different integer types")
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
          raise (Failure "different floating point types")
      in
      TFloat (combineFK oldfk fk, addAttributes olda a)

  | TEnum (oldei, olda), TEnum (ei, a) -> begin
      (* See if we have a mapping already *)
      try
        let ei' = H.find eReuse ei.ename in 
        if oldei.ename = ei'.ename then 
          TEnum (oldei, addAttributes olda a) 
        else
          raise (Failure "different enumeration tags")
      with Not_found -> begin
        (* We do not have a mapping. They better be defined in the same way *)
        if List.length oldei.eitems <> List.length ei.eitems then 
          raise (Failure "different number of enumeration elements");
        (* We check that they are defined in the same way. This is a fairly 
         * conservative check. *)
        List.iter2 
          (fun (old_iname, old_iv) (iname, iv) -> 
            if old_iname <> iname then 
              raise (Failure "different names for enumeration items");
            if old_iv <> iv then 
              raise (Failure "different values for enumeration items"))
          oldei.eitems ei.eitems;
        (* We get here if the enumerations match *)
        oldei.eattr <- addAttributes oldei.eattr ei.eattr;
        H.add eReuse ei.ename oldei;
        if debugMerge then 
          ignore (E.log "  Renaming enum %s to %s\n" ei.ename  oldei.ename);
        TEnum (oldei, addAttributes olda a)
      end
  end
        (* Strange one. But seems to be handled by GCC *)
  | TEnum (oldei, olda) , TInt(IInt, a) -> TEnum(oldei, 
                                                 addAttributes olda a)
        (* Strange one. But seems to be handled by GCC *)
  | TInt(IInt, olda), TEnum (ei, a) -> TEnum(ei, addAttributes olda a)
        
        
  | TComp (oldci, olda) , TComp (ci, a) -> begin
      if oldci.cstruct <> ci.cstruct then 
        raise (Failure "different struct/union types");
      (* See if we have a mapping already *)
      let reuseH = if ci.cstruct then sReuse else uReuse in
      try
        let ci' = H.find reuseH ci.cname in 
        if oldci.cname = ci'.cname then 
          TComp (oldci, addAttributes olda a) 
        else
          raise (Failure "different structure tags")
      with Not_found -> begin
        (* We do not have a mapping. They better be defined in the same way *)
        let old_len = List.length oldci.cfields in
        let len = List.length ci.cfields in
        if old_len <> len then 
          raise (Failure "different number of structure fields");
        (* We check that they are defined in the same way. While doing this 
         * there might be recursion and we have to watch for going into an 
         * infinite loop. So we add the assumption that they are equal *)
        H.add reuseH ci.cname oldci;
        (* We check the fields but watch for Failure *)
        (try
          List.iter2 (fun oldf f -> 
            if oldf.fbitfield <> f.fbitfield then 
              raise (Failure "different structs(bitfield info)");
            if oldf.fattr <> f.fattr then 
              raise (Failure "different structs(field attributes)");
            (* Make sure the types are compatible *)
            ignore (combineTypes CombineOther oldf.ftype f.ftype);
            ) oldci.cfields ci.cfields
        with Failure reason -> begin 
          (* Our assumption was wrong. Forget the isomorphism *)
          H.remove reuseH ci.cname;
          let msg = 
            sprint ~width:80
              (dprintf
                 "\n\tFailed in our assumption that %s and %s are isomorphic%s"
                 (compFullName oldci) (compFullName ci) reason) in
          raise (Failure msg)
        end);
        (* We get here when we succeeded checking that they are equal *)
        oldci.cattr <- addAttributes oldci.cattr ci.cattr;
        if debugMerge then 
          ignore (E.log " Renaming %s to %s\n" (compFullName ci) oldci.cname);
        TComp (oldci, addAttributes olda a) 
      end
  end

  | TArray (oldbt, oldsz, olda), TArray (bt, sz, a) -> 
      let newbt = combineTypes CombineOther oldbt bt in
      let newsz = 
        if oldsz = sz then sz else
        match oldsz, sz with
          None, Some _ -> sz
        | Some _, None -> oldsz
        | _ -> raise (Failure "different array lengths")
      in
      TArray (newbt, newsz, addAttributes olda a)
        
  | TPtr (oldbt, olda), TPtr (bt, a) -> 
      TPtr (combineTypes CombineOther oldbt bt, addAttributes olda a)
        
  | TFun (_, _, _, [Attr("missingproto",_)]), TFun _ -> t
        
  | TFun (oldrt, oldargs, oldva, olda), TFun (rt, args, va, a) ->
      let newrt = combineTypes 
          (if what = CombineFundef then CombineFunret else CombineOther) 
          oldrt rt 
      in
      if oldva != va then 
        raise (Failure "diferent vararg specifiers");
      (* If one does not have arguments, believe the one with the 
      * arguments *)
      let newargs = 
        if oldargs = None then args else
        if args = None then oldargs else
        let oldargslist = argsToList oldargs in
        let argslist = argsToList args in
        if List.length oldargslist <> List.length argslist then 
          raise (Failure "different number of arguments")
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
                   oldarg.vtype arg.vtype)
            oldargslist argslist;
          oldargs
        end
      in
      TFun (newrt, newargs, oldva, addAttributes olda a)
        
  | TNamed (oldn, oldt, olda), TNamed (n, t, a) -> begin
      (* See if there is already a known match *)
      try
        let tn', _ = H.find tReuse n in
        if tn' <> oldn then 
          raise (Failure "different type names");
        TNamed (oldn, oldt, addAttributes olda a)
      with Not_found -> begin
        (* Check that they are the same *)
        (try
          ignore (combineTypes CombineOther oldt t);
        with Failure reason -> begin
          let msg = 
            sprint ~width:80
              (dprintf
                 "\n\tFailed in our assumption that %s and %s are isomorphic%s"
                 oldn n reason) in
          raise (Failure msg)
        end);
        H.add tReuse n (oldn, oldt);
        if debugMerge then 
          ignore (E.log " Renaming type name %s to %s\n" n oldn);
        TNamed (oldn, oldt, addAttributes olda a)
      end
  end
        
        (* Unroll first the new type *)
  | _, TNamed (n, t, a) -> 
      let res = combineTypes what oldt t in
      typeAddAttributes a res
        
        (* And unroll the old type as well if necessary *)
  | TNamed (oldn, oldt, a), _ -> 
      let res = combineTypes what oldt t in
      typeAddAttributes a res
        
  | _ -> raise (Failure "different type constructors")


(** A visitor the renames uses of variables and types *)      
class renameVisitorClass = object (self)
  inherit nopCilVisitor 
      
      (* This is either a global variable which we took care of, or a local 
       * variable. Must do its type and attributes. *)
  method vvdec (vi: varinfo) = DoChildren

      (* This is a variable use. See if we must change it *)
  method vvrbl (vi: varinfo) : varinfo visitAction = 
    if not vi.vglob then DoChildren else
    try
      let vi' = H.find vReuse vi.vname in
      ChangeTo vi'
    with Not_found -> DoChildren


        (* The use of a type *)
  method vtype (t: typ) = 
    match t with 
      TComp (ci, a) -> begin
        let reuseH = if ci.cstruct then sReuse else uReuse in
        try 
          let ci' = H.find reuseH ci.cname in
          ChangeTo (TComp (ci', visitCilAttributes (self :> cilVisitor) a))
        with Not_found -> DoChildren
      end
    | TEnum (ei, a) -> begin
        try 
          let ei' = H.find eReuse ei.ename in
          ChangeTo (TEnum (ei', visitCilAttributes (self :> cilVisitor) a))
        with Not_found -> DoChildren
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

(* Here is the processing of one new file *)
let rec doOneFile (f:file) : unit = 
  if debugMerge || !E.verboseFlag then 
    ignore (E.log "Merging %s\n" f.fileName);

  (* Initialize the reuse maps *)
  initOneFile ();

  (* Scan the file and find all global varinfo's. If they are for globals that 
   * we have seen already then try to merge the type into the old one and 
   * discard this definition *)

  (* We scan each file and we look at all global varinfo. We see if globals 
   * with the same name have been encountered before and we merge those types 
   * *)
  let matchVarinfo (vi: varinfo) (l: location) = 
    (* Do nothing here for static variables  *)
    if vi.vstorage = Static then () else
    try
      let oldvi, oldloc = H.find vEnv vi.vname in 
      (* There is an old definition. We must combine the types. Do this first 
      * because it might fail  *)
      (try
        oldvi.vtype <- 
           combineTypes CombineOther oldvi.vtype vi.vtype;
      with (Failure reason) -> begin
        ignore (warn "Incompatible declaration for %s: %s.\n Previous was at %a." 
                  vi.vname reason d_loc oldloc);
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
          ignore (warn "Inconsistent storage specification for %s. Previous declaration: %a" 
                    vi.vname d_loc oldloc);
          vi.vstorage
        end
      in
      oldvi.vstorage <- newstorage;
      H.add vReuse vi.vname oldvi;
    with Not_found -> () (* Not present in the previous files *)
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
    f.globals;
  
  (* Now we go once more through the file and we rename the globals that we 
   * keep. We also scan the entire body and we replace references to the 
   * representative types or variables *)

  let processOneGlobal (g: global) : unit = 
      (* Process a varinfo. Reuse an old one, or rename it if necessary and 
       * return an indication if we used an old one  *)
    let processVarinfo (vi: varinfo) (vloc: location) : varinfo * bool= 
      (* Maybe it is static. Rename it then *)
      if vi.vstorage = Static then begin
        vi.vname <- newAlphaName vAlpha vi.vname;
        vi.vid <- H.hash vi.vname;
        vi, false
      end else begin
        try
          let oldvi = H.find vReuse vi.vname in
          oldvi, true
        with Not_found -> begin
          (* Not replacing. Add it to the environment to have it for future 
          * files  *)
          H.add vEnv vi.vname (vi,vloc);
          vi, false
        end
      end
    in
    try
      match g with 
      | GDecl (vi, l) as g -> 
          currentLoc := l;
          let vi', isold = processVarinfo vi l in
          if isold then (* Drop this declaration *) () else
          pushGlobals (visitCilGlobal renameVisitor g)
            
      | GVar (vi, init, l) as g -> 
          currentLoc := l;
          let vi', _= processVarinfo vi l in
          (* We must keep this definition even if we reuse this varinfo, 
          * because maybe the previous one was a declaration *)
          pushGlobals (visitCilGlobal renameVisitor (GVar(vi', init, l)))
            
      | GFun (fdec, l) as g -> 
          currentLoc := l;
          let vi', isold = processVarinfo fdec.svar l in
          pushGlobals (visitCilGlobal renameVisitor g)
            
      | GCompTag (ci, l) as g -> 
          currentLoc := l;
          let reuseH, alphaH = 
            if ci.cstruct then sReuse, sAlpha else uReuse, uAlpha 
          in
          if H.mem reuseH ci.cname then
            () (* Drop this since we are reusing it from before *)
          else begin
            (* We must rename it *)
            ci.cname <- newAlphaName alphaH ci.cname;
            ci.ckey <- H.hash (compFullName ci);
            pushGlobals (visitCilGlobal renameVisitor g);
          end
              
      | GEnumTag (ei, l) as g -> 
          currentLoc := l;
          if H.mem eReuse ei.ename then
            () (* Drop this since we are reusing it from before *)
          else begin
            (* We must rename it *)
            ei.ename <- newAlphaName eAlpha ei.ename;
            pushGlobals (visitCilGlobal renameVisitor g);
          end
              
      | GType (n, t, l) as g -> begin
          currentLoc := l;
          let n', t' = 
            try 
              H.find tReuse n
            with Not_found ->  begin
              (* We must rename it *)
              let n' = newAlphaName tAlpha n in
              let t'  = visitCilType renameVisitor t in
              pushGlobal (GType(n', t', l));
              n', t'
            end
          in
          H.add tEnv n (n', t')
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
  (* Scan all files and initialize the alpha tables with all the global names 
   * so that when we rename later we do not generate new names *)
  List.iter 
    (fun fl -> 
      if fl.globinitcalled || fl.globinit <> None then
        E.s (E.warn "Merging file %s has global initializer" fl.fileName);
      List.iter 
        (function 
          | GDecl (vi, _) | GVar (vi, _, _) -> 
              ignore (registerAlphaName vAlpha vi.vname)
          | GFun (fdec, _) -> ignore (newAlphaName vAlpha fdec.svar.vname)
          | _ -> ())
        fl.globals)
    files;


  List.iter doOneFile files;

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
  init (); initOneFile (); (* Make the GC happy *)
  res




