
(*
 * "Copyright (c) 2003 The Regents of the University  of California.  
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice, the following
 * two paragraphs and the author appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Authors: Matt Harren (matth@cs.berkeley.edu)
*)


(*
 * Emits assembly annotations for CQual attributes. 
 * This is only useful to me.   -- Matt
*)

open Cil  
module E = Errormsg

let sensitive_attributes = ["EQ_tainted" ; "LE_tainted" ; 
			    "GE_untainted" ; "EQ_untainted"]  
let const_attribute      = "const"
let tainted_attribute    = "EQ_tainted"

(* Checks whether the given type has a the "tainted" attribute.
 *)
let rec containsSmallocAttribute (t:typ): bool =
  (hasAttribute tainted_attribute (typeAttrs t))
  ||
  (match unrollType t with
    | TArray(t, _, _) -> containsSmallocAttribute t
    | TComp(ci, _) -> begin
        (* recurse on the fields of the struct *)
        try
          ignore (List.find (fun f -> containsSmallocAttribute f.ftype) 
                    ci.cfields);
          true (* iter stops when it finds a match, ie finds an annoted field*)
        with Not_found -> false  (* if no annotated field exists, throws *)
      end
    | _ -> false)

(* Given a type T*, is T tainted? *)
let baseTypeContainsSmallocAttribute (t:typ): bool =
  match unrollType t with
    | TPtr(bt, _) -> containsSmallocAttribute bt
    | _ ->E.s (error "Expecting a pointer type, got %a\n" d_type t)



(* Clears all "tainted" attributes from all types. Useful since gcc doesn't
 * understand the "tainted" attribute and throws warnings.
 *)        
class smallocClearAttributes (attrnames : string list ) = object
  inherit nopCilVisitor
  method vattr a =
    match a with Attr(attrname, _) ->
      if List.exists (fun a -> a = attrname) attrnames then 
	ChangeTo []
      else
	DoChildren
end
        


(***  Step 1. Find those functions that shouldn't be modified.      ***)  
class smallocMarkFunctions = object (self)
  inherit nopCilVisitor

  method vglob g = match g with 
(*     GFun(f,l) as g ->  *)
(*       if f.svar.vname = "main" then begin *)
(* 	if not (Hashtbl.mem dontFixFunctions f.svar) then begin *)
(* 	  ignore (E.warn "Not fixing parameters to entry point %s."  *)
(* 		    f.svar.vname); *)
(* 	  Hashtbl.add dontFixFunctions f.svar (); *)
(* 	end; *)
(*       end; *)
(*       let _, _, isva, _ = splitFunctionType f.svar.vtype in *)
(*       if isva then begin *)
(* 	if not (Hashtbl.mem dontFixFunctions f.svar) then begin *)
(* 	  ignore (E.warn "Not fixing parameters to vararg func %s."  *)
(* 		    f.svar.vname); *)
(* 	  Hashtbl.add dontFixFunctions f.svar (); *)
(* 	end; *)
(*       end; *)
(*       DoChildren *)
  | _ -> DoChildren

end




(* (\* Class to rewrite globals with allocations into the secure heap. It creates *)
(*  * a new struct with all globals and creates a constructor function to  *)
(*  * allocate this struct on the secure heap. It then places any initializers *)
(*  * for this global into the constructor function *)
(*  *  *)
(*  * Need to run the visitor and then the modify method. *)
(*  *\) *)
(* class smallocAnalyzeGlobalVisitor f make_malloc_call free norm secure = object *)
(*   inherit nopCilVisitor *)
(*   val mutable counter = ref 0  (\* pos of next global in the global struct *\) *)
(*   val mutable varlist = ref [] (\* list of ordered pairs (var, init, count)  *)
(*                                   of variables to be rewritten *\) *)
(*   val atrvisitor = new smallocClearAttributes [const_attribute]  *)
                     
(*   method vglob gl =  *)
(*     (\* Hunt for globals with the allocation tag. enque them and remove *\) *)
(*     match gl with *)
(*         GVar( vi, init, location) -> *)
(* 	  incr total_globals; *)
(*           if containsSmallocAttribute vi.vtype then begin *)
(* 	    incr sensitive_globals; *)
(*             vi.vtype <- visitCilType atrvisitor vi.vtype ; *)
(*             varlist := (vi,init,!counter) :: !varlist ; *)
(*             incr counter; *)
(*             ChangeTo ( [])  (\* remove decl from file -- will be added below *\) *)
(*           end else DoChildren *)
(*       | _ -> DoChildren *)

(* end *)


(* let smalloc_xform (f: file) make_malloc_call make_calloc_call  *)
(*     (realloc: lval)(free : lval) (norm: exp)  *)
(*     (secure: exp) (stack_action: int) (pthreads:bool) = *)
(*   (\* We run 4 visitors:  1. fix function parameters (actually 3 passes) *)
(*                          2. move stack allocated structures to somewhere secure *)
(*                          3. replaces malloc / free calls with smalloc calls *)
(*                          4. clears the annotations *)
(*    *\) *)
(*   if stack_action <> stack_NONE then begin *)
(*     ignore (E.log "Changing tainted function parameters.\n"); *)
(*     ignore (visitCilFileSameGlobals (new smallocMarkFunctions :>cilVisitor) f); *)
(*     ignore (visitCilFileSameGlobals (new smallocModifyFormals :>cilVisitor) f); *)
(*     ignore (visitCilFileSameGlobals (new smallocModifyActuals :>cilVisitor) f) *)
(*   end; *)
(*   if stack_action = stack_HEAP then begin *)
(*     ignore (visitCilFile (new smallocHeapifyVisitor f make_malloc_call  *)
(* 			    free norm secure) f) *)
(*   end *)
(*   else if stack_action = stack_SHADOW then begin *)
(*     let sp = if pthreads then begin *)
(*  	       ignore (E.log "Using PThreads\n"); *)
(* 	       let key = makeGlobalVar "__sp_tls_key" keyType in  *)
(* 	       f.globals <-  *)
(* 		    GText("#include <pthread.h>") *)
(* 		 :: GVar(key, {init = Some(SingleInit Cil.zero)}, locUnknown)  *)
(* 		 :: f.globals; *)
(* 	       PThreads(key) *)
(* 	     end else begin *)
(* 	       let global_sp = makeGlobalVar "stackPointer" charPtrType in *)
(* 	       f.globals <- GVar(global_sp, *)
(* 				 {init = Some(SingleInit Cil.zero)}, *)
(* 				 locUnknown) :: f.globals; *)
(*                Global(global_sp) *)
(* 	     end in *)
(*     ignore (visitCilFile (new smallocShadowStackVisitor f make_malloc_call  *)
(* 			    free norm secure sp) f); *)
(*     if shadowStackDebug then  *)
(*       f.globals <- GText("#include <assert.h>\n") :: f.globals;  *)
(*   end; *)

(*   let rlimit_func = findOrCreateFunc f "getrlimit"  *)
(*                       (TFun (intType,  *)
(*                              Some([("resource", intType, []); *)
(*                                    ("rlim", TPtr(stackLimitType, []), [])]), *)
(*                              false, [])) in *)
(*   let isStructRlimit g =  *)
(*     match g with *)
(*       GCompTag(ci, _) when ci.cname = "rlimit" -> true *)
(*     | _ -> false *)
(*   in *)
(*   if List.exists isStructRlimit f.globals then *)
(*     (\* struct rlimit; *\) *)
(*     f.globals <- [ GCompTagDecl(stackLimit_ci, locUnknown) ] @ f.globals *)
(*   else *)
(*     (\* struct rlimit {   ...  }; *\) *)
(*     f.globals <- [ GCompTag(stackLimit_ci, locUnknown) ] @ f.globals; *)

(*   ignore(visitCilFile (new smallocModifyMallocFree make_malloc_call  *)
(*                           make_calloc_call realloc  *)
(*                           free norm secure) f); *)
(*   (\* handle globals in 2 phases: 1. analyze 2. modify *\) *)
(*   let gv = new smallocAnalyzeGlobalVisitor f make_malloc_call free norm secure in *)
(*     ignore (visitCilFile (gv :> cilVisitor) f); *)
(*     gv#modify ; *)

(*   visitCilFile (new smallocClearAttributes sensitive_attributes ) f; *)
(*   () *)


(****  Entry point to the transformation ****)

let entry_point (f : file) =
  ()
(*   let normal_heap = integer 0 in  *)
(*   let secure_heap = integer 1 in  *)
(*   let secureBitType = intType in *)
(*   let (alloc_fun:varinfo), (free_name:string), (realloc_name:string),  *)
(*     make_calloc_call (\* has type (ret*args*issecure*location -> instr) *\),  *)
(*     make_malloc_call (\* has type (ret*args*issecure*location -> instr) *\)  =  *)
(*     if not !flag_use_vmalloc then *)
(*       let alloc_fun' = findOrCreateFunc f  "malloc" *)
(*                       (TFun ( voidPtrType,  *)
(*                               Some([("size", uintType, [])]),  *)
(*                               false, [])) in *)
(*       let make_malloc_call' (ret: lval option) (args: exp list)  *)
(* 	                    (issecure: exp) (loc: location) = *)
(* 	(\* Throw away the issecure flag *\) *)
(* 	Call(ret, Lval(var alloc_fun'), args, loc) *)
(*       in *)
(*       let calloc_fun' = findOrCreateFunc f "calloc" *)
(*                         (TFun (voidPtrType,  *)
(*                                Some([("nmemb", uintType, []) ;  *)
(*                                      ("size", uintType, []) ]),  *)
(*                                false, [])) in *)
(*       let make_calloc_call' (ret: lval option) (args: exp list) *)
(*                             (issecure: exp) (loc:location) = *)
(*          (\* Throw away the issecure flag *\) *)
(*          Call (ret, Lval(var calloc_fun'), args, loc) in  *)
(*       (\* bind these in the "let" above *\) *)
(*       alloc_fun', "free", "realloc", make_calloc_call', make_malloc_call' *)
(*     else *)
(*       let alloc_fun' = findOrCreateFunc f "smalloc" *)
(*                       (TFun ( voidPtrType,  *)
(*                               Some([("size", uintType, []) ;  *)
(* 				    ("issecure", secureBitType, [])]),  *)
(*                               false, [])) in *)
(*       let make_malloc_call' (ret: lval option) (args: exp list)  *)
(* 	                    (issecure: exp) (loc: location) = *)
(* 	Call(ret, Lval(var alloc_fun'), args @ [issecure], loc) *)
(*       in *)
(*       let calloc_fun' = findOrCreateFunc f "scalloc" *)
(*                         (TFun (voidPtrType,  *)
(*                                Some([("nmemb", uintType, []) ; *)
(*                                      ("size", uintType, []) ;  *)
(*                                      ("issecure", secureBitType, [])]),  *)
(*                                false, [])) in *)
(*       let make_calloc_call' (ret: lval option) (args: exp list) *)
(*                             (issecure: exp) (loc:location) = *)
(*            Call (ret, Lval(var calloc_fun'), args @ [issecure] , loc)  in *)
(*       (\* bind these in the "let" above *\) *)
(*       alloc_fun', "sfree", "srealloc", make_calloc_call', make_malloc_call' *)
(*   in *)
(*   let free_lval = var (findOrCreateFunc f  free_name  *)
(*                        (TFun ( voidType , *)
(* 			       Some([("ptr", voidPtrType, [])]),  *)
(* 			       false, []))) in *)
(*   let realloc_lval = var (findOrCreateFunc f realloc_name *)
(*                           (TFun (voidPtrType, *)
(* 				 Some([("ptr", voidPtrType, []) ; *)
(* 				       ("size", uintType, [])]),  *)
(* 				 false, []))) in *)
(*   (\* run the transformations *\) *)
(*   if !flag_setjmp then modifySetJmp f !flag_pthreads; *)
(*   smalloc_xform f make_malloc_call make_calloc_call  *)
(*     realloc_lval free_lval normal_heap secure_heap  *)
(*     !flag_stack_action !flag_pthreads; *)
(*   ignore(E.log "%d / %d of stack variables are tainted.\n"  *)
(* 	    !sensitive_stackvars !total_stackvars); *)
(*   ignore(E.log "%d / %d of heap variables (malloc calls) are tainted.\n" *)
(* 	    !sensitive_mallocs !total_mallocs); *)
(*   ignore(E.log "%d / %d of global variables are tainted.\n"  *)
(* 	    !sensitive_globals !total_globals); *)
(*   ignore(E.log "%d / %d of formal arguments are tainted.\n"  *)
(* 	    !sensitive_formals !total_formals); *)
(*   ignore(E.log "(not counting %d / %d formals that weren't fixed.)\n\n"  *)
(* 	    !sensitive_skipped_formals !total_skipped_formals) *)


(* This is on by default -- even if we don't do any memory transformations,
 * invoking the transformer will clean up EQ_tainted attributes that gcc might
 * otherwise complain about. *)
let enableAnn = ref true  

(***********************
 * The Cil.featureDesc that tells the CIL front-end how to call this module.
 * This is the only value that needs to be exported from smalloc.ml. **)

let feature : featureDescr = 
  { fd_name = "cqualann";
    fd_enabled = enableAnn;
    fd_description = "Adds assembly annotation for Cqual qualifiers." ;
    fd_extraopt = [];
    fd_doit = entry_point;
    fd_post_check = true
  } 

