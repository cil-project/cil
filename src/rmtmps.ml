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

(* rmtmps.ml *)
(* implementation for rmtmps.mli *)

open Pretty
open Trace
open Cil
module H = Hashtbl
module E = Errormsg


(* Keep a list of names that we don't want removed. These are either names of 
 * variables or functions, or names of types (preceeded by "type ") or 
 * struct/union (preceeded by "struct " or "union ") *)
let forceToKeep : (string, bool) H.t = H.create 111

(* simple visitor to clear the 'referenced' bits *)
class clearRefBitsVis = object
  inherit nopCilVisitor (* in CIL *)

  method vvdec (v: varinfo) = begin
    (* declared variables: clear the 'referenced' bits *)
    (* assume declaration preceed all uses *)
    v.vreferenced <- false;
    DoChildren
  end

  method vglob = function
      GPragma (Attr("cilnoremove", args), _) -> 
        List.iter 
          (function AStr s -> H.add forceToKeep s true
            | _ -> ignore (warn "Invalid argument to pragma cilnoremove"))
          args;
        SkipChildren

    | GPragma (Attr("boxmodelof", AStr s :: _), _) -> 
        H.add forceToKeep s true;
        SkipChildren

    | _ -> DoChildren

end



let isInlineFunc (f: fundec) : bool = f.sinline

(* weimer Mon Dec 10 16:28:11  2001
 * This huge list of removed temporaries is annoying to look at. Let's
 * spruce it up a bit.
 *)
let removed_temps = ref [] 


(* This visitor recursively marks all reachable types and variables as used. *)
(* You construct it with a hash table, which is already partially *)
(* marked; this visitor destructively updates the hash table. *)
(* The hash table is used to track which typedef names are used. *)
class removeTempsVis (usedTypedefs : (string,bool) H.t) = object (self)
  inherit nopCilVisitor

  method vvrbl (v : varinfo) = begin
    if (not (v.vglob)) then (
      (trace "usedLocal" (dprintf "local var ref: %s\n" v.vname))
    )
    else (
      (trace "usedVar" (dprintf "global var ref: %s\n" v.vname))
    );
    v.vreferenced <- true;
    DoChildren
  end

  method vtype (t : typ) = begin
    match t with
    | TEnum(e, _) -> (
        (* mark this enum as used, and recurse, only if it *)
        (* hasn't already been marked *)
        if (not e.ereferenced) then (
          e.ereferenced <- true;
          DoChildren    (* recurse (though actually recursing into an enum does nothing) *)
        )
        else (
          SkipChildren   (* don't recurse *)
        )
      )

    | TComp(c, _) -> (
        (* same logic as with TEnum *)
        if (not c.creferenced) then (
          c.creferenced <- true;
          
          (* to recurse, we must ask explicitly *)
          List.iter 
            (fun f -> 
              ignore (visitCilType (self :> cilVisitor) f.ftype)) c.cfields;

          DoChildren   (* this actually does nothing *)
        )
        else (
          SkipChildren
        )
      )

    | TNamed(s, t, _) -> (
        (* again same logic as above, though this time the 'then' *)
        (* and 'else' branches reverse roles.. :) *)
        (* see if this typedef name has already been marked *)
        if (H.mem usedTypedefs s) then (
          (* already marked, don't recurse further *)
          SkipChildren
        )
        else (
          (trace "usedType" (dprintf "marking used typedef: %s\n" s));

          (* not already marked; first mark the typedef name *)
          (H.add usedTypedefs s true);

          (* recurse deeper into the type referred-to by the typedef *)
          DoChildren
        )
      )

    | _ -> (
        (* for anything else, just look inside it *)
        DoChildren
      )
  end

  method vfunc (f : fundec) = 
    (* Do everything after the function is visited *)
    let doafter (f: fundec) = 
      (* check the 'referenced' bits on the locals *)
      f.slocals <- 
         (List.filter
            (fun (v : varinfo) ->
              if (not v.vreferenced) then begin
                (trace "usedLocal" 
                   (dprintf "removing unused: var decl: %s\n" v.vname));
                if ((String.length v.vname) < 3 ||
                (String.sub v.vname 0 3) <> "tmp") then
                  (* sm: if I'd had this to begin with, it would have been
                  * a little easier to track down the bug where I didn't
                  * check the function return-value destination *)
                  removed_temps := (f.svar.vname ^ "::" ^ v.vname) :: 
                    !removed_temps ;
                  (* (ignore (E.warn "Removing unused source variable %s"
                             v.vname)); *)
                false   (* remove it *)
              end
              else
                true    (* keep it *)
                  )
            f.slocals);
      f
    in
    ChangeDoChildrenPost (f, doafter)

end


let keepUnused = ref false

(* this traces which variables are used, from the set    *)
(* of root declarations, which are:                      *)
(*   - non-inline function definitions                   *)
(*   - non-extern global variable declarations           *)
(*   - inline funcs & extern globals that are referenced *)
(* it only works if we visit toplevel decls in reverse order *)
let removeUnusedTemps (file : file) =
  if !keepUnused then () else
begin
  if (traceActive "disableTmpRemoval") then
    (trace "disableTmpRemoval" (dprintf "temp removal disabled\n"))
  else

  if (traceActive "printCilTree") then (
    (printFile stdout file)
  );

  (* find every global function prototype *)
  let globalDecls : (string, bool) H.t = H.create 17 in
  iterGlobals file
    (function
        GDecl (v, _) ->
          (H.add globalDecls v.vname true)
      | _ -> ()
    );

  (* for every typedef name, this records whether it is needed *)
  let usedTypedefs : (string, bool) H.t = H.create 17 in

  (* begin by clearing all the 'referenced' bits, and noting all *)
  (* those declarations that are marked 'cilnoremove' *)
  H.clear forceToKeep;
  ignore (visitCilFile (new clearRefBitsVis) file);

  (* create the visitor object *)
  let vis = (new removeTempsVis usedTypedefs) in

  (* iterate over the list of globals in reverse, marking things *)
  (* as reachable if they are reachable from the roots, and *)
  (* removing ultimately unreachable toplevel constructs *)
  let rec revLoop (lst : global list) : global list =
    match lst with
    | hd::tl -> (
        (* process the tail first; going backwards is the key *)
        (* to keeping this dependency analysis simple, since it *)
        (* means we will always process all of the uses of a *)
        (* variable or type, before we finally see the declaration *)
        let processedTail = (revLoop tl) in

        (* the 'trace' calls below are actually quite inexpensive *)
        (* (when the corresponding flag is off), because they are only *)
        (* encountered at once per toplevel construct; so, I leave *)
        (* them uncommented *)

        (* now examine the head *)
        let retainHead = match hd with
          | GFun(f,_) -> (
              let keepIt = ref false in
              let reason = ref "not needed" in

              if (f.svar.vreferenced) then (
                keepIt := true;
                reason := "vreferenced flag is set"
              )
              else if (H.mem globalDecls f.svar.vname) then (
                (* sm: this isn't entirely ideal; we keep all inlines that *)
                (* have prototypes.  under normal situations it is very *)
                (* unusual to forward-declare an inline, but our combiner *)
                (* emits prototypes for some (most? all?) inlines... *)
                keepIt := true;
                reason := "saw a prototype"
              )
              else if (not (isInlineFunc f)) then (
                keepIt := true;
                reason := "not an inline function"
              )
              else if (H.mem forceToKeep f.svar.vname) then (
                keepIt := true;
                reason := "of noremove pragma"
              );
              
              if (!keepIt) then (
                (trace "usedVar" (dprintf "keeping func: %s because %s\n" 
                                          f.svar.vname !reason));
                ignore (visitCilFunction vis f);    (* root: trace it *)
                true
              )
              else (
                (* this will be deleted; don't trace *)
                (trace "usedVar" (dprintf "removing func: %s\n" f.svar.vname));
                false
              )
            )

          | GEnumTag(e, _) -> (
              if e.ereferenced ||
                 H.mem forceToKeep ("enum " ^ e.ename) then (
                (trace "usedType" (dprintf "keeping enum %s\n" e.ename));
                true
              )
              else (
                (trace "usedType" (dprintf "removing enum %s\n" e.ename));
                false
              )
            )

          | GCompTag(c, _) -> (
              let kind = if (c.cstruct) then "struct" else "union" in
              if c.creferenced || H.mem forceToKeep (kind ^ " " ^ c.cname)
              then (
                (trace "usedType" (dprintf "keeping %s %s\n" kind c.cname));
                true
              )
              else (
                (trace "usedType" (dprintf "removing %s %s\n" kind c.cname));
                false
              )
            )

          | GType(s, t, _) -> (
              if (s = "") then (
                (* it should be forward structure declaration *)
                match t with
                | TComp(c, _) -> (
                    if (c.creferenced) then (
                      (trace "usedType" (dprintf "keeping fwd decl of %s\n"
                                                 c.cname));

                      (* should not have to trace from here *)
                      (*ignore (visitCilType vis t);*)

                      (* retain this type definition *)
                      true
                    )                        
                    else (
                      (trace "usedType" (dprintf "removing fwd decl of %s\n"
                                                 c.cname));
                      false
                    )
                  )
                | _ -> (
                  (* don't know what this is.. *)
                  (trace "usedType"
                    (dprintf "removing/ignoring bad GType %a\n" d_type t));
                  false
                )
              )
              else (
                (* this is a typedef *)
                if H.mem usedTypedefs s ||
                   H.mem forceToKeep ("type " ^ s) 
                then (
                  (trace "usedType" (dprintf "keeping typedef %s\n" s));
                  (* I think we don't need to trace again during sweep, because *)
                  (* all tracing of types should have finished during mark phase *)
                  (*(visitCilType vis t);*)           (* root; trace it *)
                  true                            (* used; keep it *)
                )
                else (
                  (* not used, remove it *)
                  (trace "usedType" (dprintf "removing typedef %s\n" s));
                  false
                )
              )
            )

          | GDecl(v, _) -> (
              if v.vreferenced || H.mem forceToKeep v.vname then begin
                trace "usedVar" (dprintf "keeping global: %s\n" v.vname);

                (* since it's referenced, use it as a root for the type dependency *)
                ignore (visitCilVarDecl vis v);

                (* it's referenced: keep it *)
                true
              end else (
                (trace "usedVar" (dprintf "removing global: %s\n" v.vname));
                false
              )
            )

          (* something else: keep it *)
          | _ -> (
              ignore (visitCilGlobal vis hd);
              true
            )
        in

        if (retainHead) then
          hd :: processedTail
        else
          processedTail
      )

    | [] -> []
  in

  (* print which original source variables were removed *)
  file.globals <- (revLoop file.globals) ;
  if !removed_temps <> [] then begin
    let len = List.length !removed_temps in
    if len > 20 then 
      (ignore (E.warn "%d unused source variables removed" len))
    else 
      (ignore (E.warn "Removed unused source variables:@!%a"
        (docList (chr ',' ++ break) text) !removed_temps)) ;
    removed_temps := [] 
  end
end


(*
let hack_Cil_d_global () (g : Cil.global) =
  Cil.d_global g
*)
