(* rmtmps.ml *)
(* implementation for rmtmps.mli *)

open Pretty
open Trace
open Cil
module H = Hashtbl
module E = Errormsg

(* COPIED FROM cil.ml *)
(* the default visitor does nothing at each node, but does *)
(* not stop; hence they return true *)
class nopCilVisitor = object
  method vvrbl (v:varinfo) = true     (* variable *)
  method vvdec (v:varinfo) = true     (* variable declaration *)
  method vexpr (e:exp) = true         (* expression *)
  method vlval (l:lval) = true        (* lval (base is 1st field) *)
  method voffs (o:offset) = true      (* lval offset *)
  method vinst (i:instr) = true       (* imperative instruction *)
  method vstmt (s:stmt) = true        (* constrol-flow statement *)
  method vfunc (f:fundec) = true      (* function definition *)
  method vfuncPost (f:fundec) = true  (*   postorder version *)
  method vglob (g:global) = true      (* global (vars, types, etc.) *)
  method vinit (i:init) = true        (* global initializer *)
  method vtype (t:typ) = true         (* use of some type *)
  method vtdec (s:string) (t:typ) = true    (* typedef *)
  method venum (e:enuminfo) = true
  method vcomp (c:compinfo) = true
end


(* simple visitor to clear the 'referenced' bits *)
class clearRefBitsVis = object
  inherit nopCilVisitor

  method vvdec (v: varinfo) = begin
    (* declared variables: clear the 'referenced' bits *)
    (* assume declaration preceed all uses *)
    v.vreferenced <- false;
    true
  end
end


(* encapsulate this since rumor has it Raymond has changed *)
(* how we represent 'inline' *)
let isInlineFunc (f: fundec) : bool = (
  (hasAttribute "inline" f.svar.vattr)
)


(* This visitor recursively marks all reachable types and variables as used. *)
(* You construct it with a hash table, which is already partially *)
(* marked; this visitor destructively updates the hash table. *)
(* The hash table is used to track which typedef names are used. *)
class removeTempsVis (*(usedTypes : (typ,bool) H.t)*)
                     (usedTypedefs : (string,bool) H.t) = object (self)
  inherit nopCilVisitor

  method vvrbl (v : varinfo) = begin
    if (not (v.vglob)) then (
      (trace "usedLocal" (dprintf "local var ref: %s\n" v.vname))
    )
    else (
      (trace "usedVar" (dprintf "global var ref: %s\n" v.vname))
    );
    v.vreferenced <- true;
    true
  end

  method vtype (t : typ) = begin
    match t with
    | TEnum(e, _) -> (
        (* mark this enum as used, and recurse, only if it *)
        (* hasn't already been marked *)
        if (not e.ereferenced) then (
          e.ereferenced <- true;
          true    (* recurse (though actually recursing into an enum does nothing) *)
        )
        else (
          false   (* don't recurse *)
        )
      )

    | TComp(c, _) -> (
        (* same logic as with TEnum *)
        if (not c.creferenced) then (
          c.creferenced <- true;
          
          (* to recurse, we must ask explicitly *)
          (visitCompFields (self :> cilVisitor) c);

          true   (* this actually does nothing *)
        )
        else (
          false 
        )
      )

    | TNamed(s, t, _) -> (
        (* again same logic as above, though this time the 'then' *)
        (* and 'else' branches reverse roles.. :) *)
        (* see if this typedef name has already been marked *)
        if (H.mem usedTypedefs s) then (
          (* already marked, don't recurse further *)
          false
        )
        else (
          (trace "usedType" (dprintf "marking used typedef: %s\n" s));

          (* not already marked; first mark the typedef name *)
          (H.add usedTypedefs s true);

          (* recurse deeper into the type referred-to by the typedef *)
          true
        )
      )

    | _ -> (
        (* for anything else, just look inside it *)
        true
      )
  end

  method vfuncPost (f : fundec) = begin
    (* check the 'referenced' bits on the locals *)
    f.slocals <- (List.filter
      (fun (v : varinfo) ->
        if (not v.vreferenced) then begin
          (trace "usedLocal" (dprintf "removing unused: var decl: %s\n" v.vname));
          if ((String.length v.vname) < 3 ||
              (String.sub v.vname 0 3) <> "tmp") then
            (* sm: if I'd had this to begin with, it would have been
             * a little easier to track down the bug where I didn't
             * check the function return-value destination *)
            (ignore (E.warn "Removing unused source variable %s\n"
                       v.vname));
          false   (* remove it *)
        end
        else
          true    (* keep it *)
      )
      f.slocals);

    true
  end
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
    (trace "disableTmpRemoval" (dprintf "trace removal disabled\n"))
  else

  (* associate with every 'typ' whether it is referred-to *)
  (* by a global variable or function *)
  (*let usedTypes : (typ, bool) H.t = H.create 17 in*)

  (* and similarly for every typedef name *)
  let usedTypedefs : (string, bool) H.t = H.create 17 in

  if (traceActive "printCilTree") then (
    (printFile stdout file)
  );

  (* begin by clearing all the 'referenced' bits *)
  (visitCilFile (new clearRefBitsVis) file);

  (* create the visitor object *)
  let vis = (new removeTempsVis (*usedTypes*) usedTypedefs) in

  (* this was here before, and it might still be useful later *)
  (*
    iterGlobals file
      (function
          GDecl (v, _) -> ignore (E.log " %s referenced=%b\n"
                                    v.vname v.vreferenced)
        | _ -> ());
  *)

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
              if (f.svar.vreferenced ||
                  (not (isInlineFunc f))) then (
                (trace "usedVar" (dprintf "keeping func: %s\n" f.svar.vname));
                (visitCilFunction vis f);    (* root: trace it *)
                true
              )
              else (
                (* this will be deleted; don't trace *)
                (trace "usedVar" (dprintf "removing func: %s\n" f.svar.vname));
                false
              )
            )

          | GEnumTag(e, _) -> (
              if (e.ereferenced) then (
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
              if (c.creferenced) then (
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
                (* comments in cil.mli say this can't happen.. *)
                (* can I ignore it if it does? *)

                (* it's a normal type declaration *)
                if (false (*H.mem usedTypes t*)) then (
                  (trace "usedType" (dprintf "keeping type %a\n"
                                             d_type t));

                  (* also trace from here *)
                  (visitCilType vis t);

                  (* and retain this type definition *)
                  true
                )
                else (
                  (* not used, remove it *)
                  (trace "usedType" 
                    (dprintf "removing/ignoring bad GType %a\n" d_type t));
                  false
                )
              )
              else (
                (* this is a typedef *)
                if (H.mem usedTypedefs s) then (
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
              if (not v.vreferenced) then (
                (trace "usedVar" (dprintf "removing global: %s\n" v.vname));
                false
              )
              else (
                (trace "usedVar" (dprintf "keeping global: %s\n" v.vname));

                (* since it's referenced, use it as a root for the type dependency *)
                (visitCilVarDecl vis v);

                (* it's referenced: keep it *)
                true
              )
            )

          (* something else: keep it *)
          | _ -> (
              (visitCilGlobal vis hd);
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

  file.globals <- (revLoop file.globals)
end


(*
let hack_Cil_d_global () (g : Cil.global) =
  Cil.d_global g
*)
