(*
 *
 * Copyright (c) 2001-2002 by
 *  George C. Necula	necula@cs.berkeley.edu
 *  Scott McPeak        smcpeak@cs.berkeley.edu
 *  Wes Weimer          weimer@cs.berkeley.edu
 *  Ben Liblit          liblit@cs.berkeley.edu
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

(* rmtmps.ml *)
(* implementation for rmtmps.mli *)

open Pretty
open Trace
open Cil
module H = Hashtbl
module E = Errormsg
module U = Util



(***********************************************************************
 *
 *  Clearing of "referenced" bits
 *
 *)


let clearReferencedBits file =
  let considerGlobal = function
    | GType (info, _) ->
	trace "usedType" (dprintf "clearing mark: typedef %s\n" info.tname);
	info.treferenced <- false

    | GEnumTag (info, _)
    | GEnumTagDecl (info, _) ->
	trace "usedType" (dprintf "clearing mark: enum %s\n" info.ename);
	info.ereferenced <- false

    | GCompTag (info, _)
    | GCompTagDecl (info, _) ->
	trace "usedType" (dprintf "clearing mark: compound %s\n" info.cname);
	info.creferenced <- false

    | GVar ({vname = name} as info, _, _)
    | GVarDecl ({vname = name} as info, _) ->
	trace "usedGlobal" (dprintf "clearing mark: global %s\n" name);
	info.vreferenced <- false

    | GFun ({svar = info} as func, _) ->
	trace "usedGlobal" (dprintf "clearing mark: global fun %s\n" info.vname);
	info.vreferenced <- false;
	let clearMark local =
	  trace "usedLocal" (dprintf "clearing mark: local %s\n" local.vname);
	  local.vreferenced <- false
	in
	List.iter clearMark func.slocals

    | _ ->
	()
  in
  iterGlobals file considerGlobal


(***********************************************************************
 *
 *  Scanning and categorization of pragmas
 *
 *)


(* collections of names of things to keep *)
type collection = (string, unit) H.t
type keepers = {
    typedefs : collection;
    enums : collection;
    structs : collection;
    unions : collection;
    defines : collection;
  }


(* rapid transfer of control when we find a malformed pragma *)
exception Bad_pragma


(* CIL and CCured define several pragmas which prevent removal of
 * various global symbols.  Here we scan for those pragmas and build
 * up collections of the corresponding symbols' names.
 *)

let categorizePragmas file =

  (* names of things which should be retained *)
  let keepers = {
    typedefs = H.create 0;
    enums = H.create 0;
    structs = H.create 0;
    unions = H.create 0;
    defines = H.create 1
  } in
  
  (* populate these name collections in light of each pragma *)
  let considerPragma =

    let badPragma location pragma =
      ignore (warnLoc location "Invalid argument to pragma %s" pragma)
    in
    
    function
      | GPragma (Attr ("cilnoremove" as directive, args), location) ->
	  (* a very flexible pragma: can retain typedefs, enums,
	   * structs, unions, or globals (functions or variables) *)
	  begin
	    let processArg arg =
	      try
		match arg with
		| AStr specifier ->
		    (* isolate and categorize one symbol name *)
		    let collection, name =
		      (* Two words denotes a typedef, enum, struct, or
		       * union, as in "type foo" or "enum bar".  A
		       * single word denotes a global function or
		       * variable. *)
		      let whitespace = Str.regexp "[ \t]+" in
		      let words = Str.split whitespace specifier in
		      match words with
		      | ["type"; name] ->
			  keepers.typedefs, name
		      | ["enum"; name] ->
			  keepers.enums, name
		      | ["struct"; name] ->
			  keepers.structs, name
		      | ["union"; name] ->
			  keepers.unions, name
		      | [name] ->
			  keepers.defines, name
		      | _ ->
			  raise Bad_pragma
		    in
		    H.add collection name ()
		| _ ->
		    raise Bad_pragma
	      with Bad_pragma ->
		badPragma location directive
	    in
	    List.iter processArg args
	  end

      | GPragma (Attr("boxmodelof" as directive, attribute :: _), location)
      | GPragma (Attr("ccuredwrapperof" as directive, attribute :: _), location) -> 
	  (* these pragmas indirectly require that we keep the named function *)
	  begin
	    match attribute with
	    | AStr name ->
		H.add keepers.defines name ()
	    | _ ->
		badPragma location directive
	  end

      |	_ ->
	  ()
  in
  iterGlobals file considerPragma;
  keepers



(***********************************************************************
 *
 *  Function body elimination from pragmas
 *
 *)


(* When performing global slicing, any functions not explicitly marked
 * as pragma roots are reduced to mere declarations.  This leaves one
 * with a reduced source file that still compiles to object code, but
 * which contains the bodies of only explicitly retained functions.
 *)

let amputateFunctionBodies keptGlobals file =
  let considerGlobal = function
    | GFun ({svar = {vname = name} as info}, location)
      when not (H.mem keptGlobals name) ->
	trace "usedGlobal" (dprintf "slicing: reducing to prototype: function %s\n" name);
	GVarDecl (info, location)
    | other ->
	other
  in
  mapGlobals file considerGlobal



(***********************************************************************
 *
 *  Root collection from pragmas
 *
 *)


let markPragmaRoots keepers file =

  (* check each global against the appropriate "keep" list *)
  let considerGlobal global =
    match global with
    | GType ({tname = name} as info, _) ->
	if H.mem keepers.typedefs name then
	  begin
	    trace "usedType" (dprintf "marking root (pragma): typedef %s\n" name);
	    info.treferenced <- true
	  end
    | GEnumTag ({ename = name} as info, _) ->
	if H.mem keepers.enums name then
	  begin
	    trace "usedType" (dprintf "marking root (pragma): enum %s\n" name);
	    info.ereferenced <- true
	  end
    | GCompTag ({cname = name; cstruct = structure} as info, _) ->
	let collection = if structure then keepers.structs else keepers.unions in
	if H.mem collection name then
	  begin
	    trace "usedType" (dprintf "marking root (pragma): compound %s\n" name);
	    info.creferenced <- true
	  end
    | GVar ({vname = name} as info, _, _)
    | GFun ({svar = {vname = name} as info}, _) ->
	if H.mem keepers.defines name then
	  begin
	    trace "usedGlobal" (dprintf "marking root (pragma): global %s\n" name);
	    info.vreferenced <- true
	  end
    | _ ->
	()
  in
  iterGlobals file considerGlobal



(***********************************************************************
 *
 *  Root collection from external linkage
 *
 *)


(* Exported roots are those global symbols which are visible to the
 * linker and dynamic loader.  For variables, this consists of
 * anything that is not "static".  For functions, this consists of:
 *
 * - functions declared extern inline
 * - functions declared neither inline nor static
 * - functions bearing a "constructor" or "destructor" attribute
 *)

let markExportedRoots file =
  let considerGlobal =
    let mark info =
      trace "usedGlobal" (dprintf "marking root (external linkage): global %s\n" info.vname);
      info.vreferenced <- true
    in
    function
      | GVar ({vstorage = storage} as info, _, _)
	when storage != Static ->
	  mark info
      | GFun ({svar = {vinline = true; vstorage = Extern} as info}, _)
      | GFun ({svar = {vinline = false; vstorage = NoStorage} as info}, _) ->
	  mark info
      |	GFun ({svar = {vattr = attributes} as info}, _)
	when
	  let rec isExportingAttribute = function
	    | Attr ("constructor", []) -> true
	    | Attr ("destructor", []) -> true
	    | _ -> false
	  in
	  List.exists isExportingAttribute attributes
	  ->
	    mark info
      | _ ->
	  ()
  in
  iterGlobals file considerGlobal



(***********************************************************************
 *
 *  Transitive reachability closure from roots
 *
 *)


(* This visitor recursively marks all reachable types and variables as used. *)
class markReachableVisitor globalMap = object (self)
  inherit nopCilVisitor

  method vvrbl v =
    if not v.vreferenced then
      begin
	v.vreferenced <- true;
	
	let name = v.vname in
	if v.vglob then
	  trace "usedGlobal" (dprintf "marking transitive use: global %s\n" name)
	else
	  trace "usedLocal" (dprintf "marking transitive use: local %s\n" name);
	
        (* If this is a global, we need to keep everything used in its
	 * definition and declarations. *)
	if v.vglob then
	  begin
	    trace "usedGlobal" (dprintf "descending: global %s\n" name);
	    let descend global =
	      ignore (visitCilGlobal (self :> cilVisitor) global)
	    in
	    let globals = Hashtbl.find_all globalMap name in
	    List.iter descend globals
	  end;
	
	v.vreferenced <- true
      end;
    SkipChildren

  method vtype typ =
    let old : bool =
      match typ with
      | TEnum(e, _) ->
	  let old = e.ereferenced in
	  if not old then
	    begin
	      trace "usedType" (dprintf "marking transitive use: enum %s\n" e.ename);
	      e.ereferenced <- true
	    end;
	  old

      | TComp(c, _) ->
	  let old = c.creferenced in
          if not old then
            begin
	      trace "usedType" (dprintf "marking transitive use: compound %s\n" c.cname);
	      c.creferenced <- true;

              (* to recurse, we must ask explicitly *)
	      let recurse f = ignore (visitCilType (self :> cilVisitor) f.ftype) in
	      List.iter recurse c.cfields
	    end;
	  old

      | TNamed(ti, _) ->
	  let old = ti.treferenced in
          if not old then
	    begin
	      trace "usedType" (dprintf "marking transitive use: typedef %s\n" ti.tname);
	      ti.treferenced <- true;
	      
	      (* recurse deeper into the type referred-to by the typedef *)
	      (* to recurse, we must ask explicitly *)
	      ignore (visitCilType (self :> cilVisitor) ti.ttype);
	    end;
	  old

      | _ ->
          (* for anything else, just look inside it *)
	  false
    in
    if old then
      SkipChildren
    else
      DoChildren
end


let markReachable file =
  (* build a mapping from global names back to their definitions & declarations *)
  let globalMap = Hashtbl.create 1 in
  let considerGlobal global =
    match global with
    | GFun ({svar = info}, _)
    | GVar (info, _, _)
    | GVarDecl (info, _) ->
	Hashtbl.add globalMap info.vname global
    | _ ->
	()
  in
  iterGlobals file considerGlobal;

  (* mark everything reachable from the global roots *)
  let visitor = new markReachableVisitor globalMap in
  let considerGlobal global =
    match global with
    | GType ({treferenced = true}, _)
    | GEnumTag ({ereferenced = true}, _)
    | GCompTag ({creferenced = true}, _)
    | GVar ({vreferenced = true}, _, _)
    | GFun ({svar = {vreferenced = true}}, _)
    | GPragma _ ->
	ignore (visitCilGlobal visitor global)
    | _ ->
	()
  in
  iterGlobals file considerGlobal



(***********************************************************************
 *
 *  Removal of unused symbols
 *
 *)


(* regular expression matching names of uninteresting locals *)
let uninteresting =
  let names = [
    (* Cil.makeTempVar *)
    "tmp";
    
    (* sm: I don't know where it comes from but these show up all over. *)
    (* this doesn't seem to do what I wanted.. *)
    "iter";

    (* various macros in glibc's <bits/string2.h> *)		   
    "__result";
    "__s"; "__s1"; "__s2";
    "__s1_len"; "__s2_len";
    "__retval"; "__len";

    (* various macros in glibc's <ctype.h> *)
    "__c"; "__res";
  ] in

  (* optional alpha renaming *)
  let alpha = "\\(___[0-9]+\\)?" in
  
  let pattern = "\\(" ^ (String.concat "\\|" names) ^ "\\)" ^ alpha ^ "$" in
  Str.regexp pattern


let removeUnmarked file =
  let removedLocals = ref [] in
  
  let filterGlobal = function
    (* unused global types are simply removed *)
    | GType ({treferenced = false; tname = name}, _)
    | GCompTag ({creferenced = false; cname = name}, _)
    | GCompTagDecl ({creferenced = false; cname = name}, _)
    | GEnumTag ({ereferenced = false; ename = name}, _)
    | GEnumTagDecl ({ereferenced = false; ename = name}, _) ->
	trace "usedType" (dprintf "removing type: %s\n" name);
	false

    (* unused global variables and functions are simply removed *)
    | GVar ({vreferenced = false; vname = name}, _, _)
    | GVarDecl ({vreferenced = false; vname = name}, _)
    | GFun ({svar = {vreferenced = false; vname = name}}, _) ->
	trace "usedGlobal" (dprintf "removing global: %s\n" name);
	false

    (* retained functions may wish to discard some unused locals *)
    | GFun (func, _) ->
	let rec filterLocal local =
	  if not local.vreferenced then
	    begin
	      (* along the way, record the interesting locals that were removed *)
	      let name = local.vname in
	      trace "usedLocal" (dprintf "removing local: %s\n" name);
	      if not (Str.string_match uninteresting name 0) then
		removedLocals := (func.svar.vname ^ "::" ^ name) :: !removedLocals;
	    end;
	  local.vreferenced
	in
	func.slocals <- List.filter filterLocal func.slocals;
	true

    (* all other globals are retained *)
    | _ ->
	true
  in
  file.globals <- List.filter filterGlobal file.globals;
  !removedLocals


(***********************************************************************
 *
 *  Exported interface
 *
 *)


type rootsMarker = Cil.file -> unit

let defaultRootsMarker = markExportedRoots


let keepUnused = ref false

let rec removeUnusedTemps ?(markRoots : rootsMarker = defaultRootsMarker) file =
  if !keepUnused || traceActive "disableTmpRemoval" then
    trace "disableTmpRemoval" (dprintf "temp removal disabled\n")
  else
    begin
      if traceActive "printCilTree" then
	dumpFile defaultCilPrinter stdout file;

      (* begin by clearing all the 'referenced' bits *)
      clearReferencedBits file;

      (* digest any pragmas that would create additional roots *)
      let keepers = categorizePragmas file in

      (* if slicing, remove the bodies of non-kept functions *)
      if !Util.sliceGlobal then
	amputateFunctionBodies keepers.defines file;

      (* build up the root set *)
      markPragmaRoots keepers file;
      markRoots file;

      (* mark everything reachable from the global roots *)
      markReachable file;

      (* take out the trash *)
      let removedLocals = removeUnmarked file in

      (* print which original source variables were removed *)
      if removedLocals != [] then
	let count = List.length removedLocals in
	if count > 2000 then 
	  ignore (E.warn "%d unused local variables removed" count)
	else
	  ignore (E.warn "%d unused local variables removed:@!%a"
		    count (docList (chr ',' ++ break) text) removedLocals)
    end
