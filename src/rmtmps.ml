(* rmtmps.ml *)
(* implementation for rmtmps.mli *)

open Pretty
open Trace
open Cil
module H = Hashtbl


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
class removeTempsVis (usedTypes : (typ,bool) H.t)
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
    | TNamed(s, t, _) -> (
        (* see if this typedef name has already been marked *)
        if (H.mem usedTypedefs s) then (
          (* already marked, don't recurse further *)
          true
        )
        else (
          (trace "usedType" (dprintf "marking used typedef: %s\n" s));

          (* not already marked; first mark the typedef name *)
          (H.add usedTypedefs s true);

          (* also recursively mark the type it refers to; *)
          (* even if we return true, the visitor does not *)
          (* automatically recurse since named types are the *)
          (* cut-points for circularity *)
          (* WRONG: TForward are the cut-points *)
          (*(visitCilType (self :> cilVisitor) t);*)

          (* like I say, this has no effect *)
          true
        )
      )

    | _ -> (
        (* for anything but a typedef, look only at its structure *)
        if (H.mem usedTypes t) then (
          (* this type has already been marked reachable, so *)
          (* stop recursing into it *)
          true
        )
        else (
          (trace "usedType" (dprintf "marking used type: %a\n"
                                     d_type t));

          (* it is reachable; mark it as such *)
          (H.add usedTypes t true);
          
          (* we have to handle TForward specially -- if a TForward *)
          (* is used, we need the associated TComp to be marked used also *)
          (match t with
          | TComp (true, cinfo, a) -> (
              (trace "usedType" (dprintf "also recursing into associated TComp: %a\n" d_type t));

              (* not only do I need to mark it, I need to recurse *)
              (* into its interior; apparently, this TComp will be *)
              (* considered equal (by the hash table) to the TComp *)
              (* presumably already in the Cil tree *)
              (visitCilType (self :> cilVisitor) (TComp(false, cinfo, a)))
            )
          | _ -> ());

          (* then recurse into types reachable from here *)
          true
        )
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
            (ignore (printf "WARNING: removing unused source variable %s\n"
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
  let usedTypes : (typ, bool) H.t = H.create 17 in

  (* and similarly for every typedef name *)
  let usedTypedefs : (string, bool) H.t = H.create 17 in

  if (traceActive "printCilTree") then (
    (printFile stdout file)
  );

  (* begin by clearing all the 'referenced' bits *)
  (visitCilFile (new clearRefBitsVis) file);

  (* create the visitor object *)
  let vis = (new removeTempsVis usedTypes usedTypedefs) in

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

          | GType(s, t, _) -> (
              if (s = "") then (
                (* it's a normal type declaration *)
                if (H.mem usedTypes t) then (
                  (trace "usedType" (dprintf "keeping type %a\n"
                                             d_type t));

                  (* also trace from here *)
                  (visitCilType vis t);

                  (* and retain this type definition *)
                  true
                )
                else (
                  (* not used, remove it *)
                  (trace "usedType" (dprintf "removing type %a\n"
                                             d_type t));
                  false
                )
              )
              else (
                (* this is a typedef *)
                if (H.mem usedTypedefs s) then (
                  (trace "usedType" (dprintf "keeping typedef %s\n" s));
                  (visitCilType vis t);           (* root; trace it *)
                  true                            (* used; keep it *)
                )
                else if (H.mem usedTypes t) then (
                  (trace "usedType" (dprintf "keeping typedef %s because un-typedef'd type is used\n" s));
                  (visitCilType vis t);           (* root; trace it *)
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
