
(*
 * "Copyright (c) 2005 The Regents of the University  of California.  
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
open Pretty
module E = Errormsg
module H = Hashtbl

let sensitive_attributes = ["EQ_tainted" ; "LE_tainted" ; 
			    "GE_untainted" ; "EQ_untainted";
                            "Poly_tainted" ; "EQ_const"]  
let const_attribute      = "const"
let tainted_attribute    = "EQ_tainted"
let poly_taint_attribute = "Poly_tainted"

let builtinLongLong = "builtinLongLong"

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
      if List.mem attrname attrnames then 
	ChangeTo []
      else
	DoChildren
end
        


let findOrCreateFunc f name t = 
  let rec search glist = 
    match glist with
	GVarDecl(vi,_) :: rest when isFunctionType vi.vtype 
	  && vi.vname = name -> vi
      | _ :: rest -> search rest (* tail recursive *)
      | [] -> (*not found, so create one *)
	  let new_decl = makeGlobalVar name t in
	  f.globals <- GVarDecl(new_decl, locUnknown) :: f.globals;
	  new_decl
  in
    search f.globals

let stringOf (i:int): string = Int32.to_string (Int32.of_int i)

let arrayLen eo : int = 
  try
    lenOfArray eo
  with LenOfArray -> E.s (unimp "array without a size")

(* flatten nested arrays *)
let rec getSize t: int * typ =
  match unrollType t with 
      TArray(bt, e, _) ->
        let mylen = arrayLen e in
        let len', bt' = getSize bt in
        (mylen*len'), bt'
    | _ -> 1, t
              

(* exception Unimp *)
let uniqueUnimplLabel = ref 0
let unimplementedT t =  
  ignore (warn "Can't annotate unimplemented type: %a  (Attrs: %a)\n" 
            d_type t d_attrlist (typeAttrs t));
(*   raise Unimp *)
  incr uniqueUnimplLabel;
  "unimplemented"^(stringOf !uniqueUnimplLabel)

let rec encodeType (t:typ):string = 
  let unimplemented () = unimplementedT t in
  let taint =
    let a = typeAttrs t in
    if hasAttribute tainted_attribute a then
      "T" 
    else begin
      match filterAttributes poly_taint_attribute a with
          [] -> "U"
        | [Attr(s, [AStr varname])] -> "P("^varname^")"
        | _ -> E.s (error "bad attributesin %a." d_plaintype t)
    end
  in
  match unrollType t with
      TInt _ as t' when bitsSizeOf t' = 32 -> (*int, uint, long, ulong*)
         taint^"int"
    | TInt _ as t' when bitsSizeOf t' = 8 -> taint^"char"
    | TInt _ as t' when bitsSizeOf t' = 16 -> taint^"short"
    | TInt _ as t' when bitsSizeOf t' = 64 ->  (* long long *)
        "_"^taint^builtinLongLong
    | TPtr(bt, _) -> begin
        let bt' = encodeType bt in
        taint^"*" ^ bt'
      end
    | TComp(ci, _) when ci.cstruct ->
        "_" ^ ci.cname
    | TVoid _ -> taint^"void"
    | _ -> 
        taint^(unimplemented ())


(* For arrays inside structs, unroll them into "len" different fields *)
(* FIXME: this doesn't work well for variable access *)
let encodeArrayType (fieldName:string) (t:typ) =
  if not (isArrayType t) then 
    E.s (bug " non-array passed to encodeArrayType");
  let len, bt = getSize t in
  let acc: doc list ref = ref [] in
  let typestr = encodeType bt in
  for i = len - 1 downto 0 do
    let d = dprintf ", \"%s%d\", \"%s\"" fieldName i typestr in
    acc := d::!acc
  done;
  Pretty.sprint 10000 (docList ~sep:nil (fun x -> x) () !acc)


(*******  Annotation macros  *****************************************)

let quoted s: string =
  "\"" ^ s ^ "\""

(* Like quoted, but prepends _ to identifiers if Cil.underscore_name is true.*)
let quotedLabel s: string = 
  if !Cil.underscore_name then
    "\"_" ^ s ^ "\""
  else 
    "\"" ^ s ^ "\""
    
let globalAnn label args:  global =
  let annstr = "#ANN(" ^ label ^", " ^ args ^")" in
  GAsm(annstr, !currentLoc)
  
let localAnn label args: instr =
  let annstr = "#ANN(" ^ label ^", " ^ args ^ ") " in
  Asm([], [annstr], [], [], [], !currentLoc)

let localVarAnn label func v typ: instr =
  (*combine the function name and the var name *)
  let vname = quotedLabel (func.svar.vname ^ ":" ^ v.vname) in
  (* FIXME: are the input/outputs right? *)
  let annstr = "#ANN(" ^ label ^", " ^ vname ^ ", \"%0\", " ^ typ ^ ") " in
  let lv = if isArrayType v.vtype then
    (Var v, Index(Cil.zero, NoOffset))
  else
    (Var v, NoOffset)
  in
  Asm([], [annstr], [None, "=m", lv], 
      (* ["0", Lval(lv)] *)
      [], [], !currentLoc)




let structANN = "ANN_STRUCT"
let funcANN = "ANN_FUNC"    (* A func that is declared or defined *)
let rootANN = "ANN_ROOT"    (* A func that is defined *)
let globalANN = "ANN_GLOBAL"
let globalarrayANN = "ANN_GLOBALARRAY"

let allocANN = "ANN_ALLOC"
let localANN = "ANN_LOCAL"
let localarrayANN = "ANN_LOCALARRAY"
  

(*******   Strings  *******)

let newGlobals = ref []

let stringId = ref 0 
let newStringName () = 
  incr stringId;
  "__string" ^ (string_of_int !stringId)

let taintedChar = typeAddAttributes [Attr(tainted_attribute, [])] charType

let global4String (s : string) (charIsTainted: bool): exp = 
  let l = 1 + (String.length s) in
  let stringInit =  
    let initl' = ref [] in
    let idx = ref 0 in
    String.iter (fun c ->
                   let i = (Index(integer !idx, NoOffset), 
                            SingleInit(Const(CChr c))) in
                   incr idx;
                   initl' := i::!initl') s;
    initl' := (Index(integer l, NoOffset),
               SingleInit(integer 0)) :: !initl';
    List.rev !initl'
  in
  let bt = if charIsTainted then taintedChar else charType in
  let newt = TArray(bt, Some (integer l), []) in
  let gvar = makeGlobalVar (newStringName ()) newt in
  gvar.vstorage <- Static;
  let start = AddrOf (Var gvar, Index(zero, NoOffset)) in
  let init =  CompoundInit(newt, stringInit) in
  newGlobals := (GVar (gvar, {init=Some init}, !currentLoc))::!newGlobals;
  start

class stringVisitor 
= object(self)
  inherit nopCilVisitor
    
  method vexpr e = begin
    match e with 
        Const(CStr s) -> 
(*           ignore (E.log "String without cast: %a\n" d_plainexp e); *)
          ChangeTo(global4String s false)
      | CastE(t, Const(CStr s)) ->
          let taint =  baseTypeContainsSmallocAttribute t in
(*           ignore (E.log "%stainted String: %a\n"  *)
(*                     (if taint then "" else "Un") d_plainexp e); *)
          ChangeTo(CastE(t, global4String s taint))
      | _ -> DoChildren
  end
end
(*******   Visitor   *******)


let startsWith s prefix =
  let n = String.length prefix in
  (String.length s >= n) && ((Str.first_chars s n) = prefix)

let annotatedFunctions: (varinfo, unit) H.t = H.create 19
let annotateFundec fv = 
  if H.mem annotatedFunctions fv then
    None
  else begin
    H.add annotatedFunctions fv ();
    let fname = fv.vname in
    let rt, args, _, _ = splitFunctionType fv.vtype in
    let rec doParams = function
      | (_, t, _)::rest ->
          let t' = encodeType t in
          ", " ^ quoted t' ^ (doParams rest)
      | [] -> ""
    in
    let typestr = quotedLabel fname ^ ", "
                  ^ quoted (encodeType rt)
                  ^ doParams (argsToList args) in
    let ann = globalAnn funcANN typestr in
    Some ann
  end

class annotationVisitor 
= object(self)
  inherit nopCilVisitor
    
  val mutable currentFunction: fundec = Cil.dummyFunDec

  method vvdec v = begin
(* FIXME:    if maybeStack v.vattr then begin *)
(*       assert (not v.vglob); *)
(*       (\* For a local, this flag would only be set if we take the address of v,  *)
(*          right? *\) *)
(*       (\* ignore (E.log "  We take the address of %s.\n" v.vname); *\) *)
(*       let t = encodeType v.vtype in *)
(*       self#queueInstr  *)
(*         [localVarAnn ccuredlocal currentFunction v (quoted t)]; *)
(*       () *)
(*     end *)
(*     else *)
      if not v.vglob then begin
      match v.vtype with
          TArray (bt, Some size, a) ->
            let size' = isInteger (constFold true size) in
            if size' = None then E.s (error "Non-constant array size");
            let size'' = Int64.to_int (Util.valOf size') in
            let t = encodeType bt in
            self#queueInstr 
              [localVarAnn localarrayANN currentFunction v 
                 ((quoted t) ^ ", " ^ (stringOf size''))];
            ()
        | TArray _ -> E.s (unimp "array without a size")
        | _ -> ()
    end;
    DoChildren
  end

  method vglob g = begin
    try
      match g with 
          GFun (fdec, l) ->
            currentFunction <- fdec;
            (* Step 1: declare the function signature *)

            let anno = annotateFundec fdec.svar in
            let rootAnn = globalAnn rootANN
                            (quotedLabel fdec.svar.vname) in
            let newG = match anno with
                Some ann -> [ann; rootAnn; g]
              | None -> [rootAnn; g]
            in
            ChangeDoChildrenPost(
              newG,
              (fun g -> currentFunction <- Cil.dummyFunDec; g)
            )
        | GVarDecl (vi, l) 
            when isFunctionType vi.vtype (* && vi.vname <> "__ccuredInit" *) ->
            begin
              let anno = annotateFundec vi in
              match anno with
                  Some ann -> ChangeDoChildrenPost( [ann; g],(fun g -> g))
                | None -> DoChildren
            end
        | GCompTag (ci, l) ->
            if ci.cname = "printf_arguments" then begin
              ignore (warn "skipping \"%s\"" ci.cname );
              DoChildren
            end
            else if ci.cstruct then begin
              (* ignore (E.log "printing struct \"%s\"\n" ci.cname ); *)
              let annstr = ref (quoted ci.cname) in
              let isMetaStruct = Util.hasPrefix "meta_" ci.cname in
              List.iter
                (fun fi ->
                   if fi.fname = Cil.missingFieldName then
                     E.s (unimp "not a real field? in %a" d_global g);
                   if isArrayType fi.ftype then 
                     annstr := !annstr ^ encodeArrayType fi.fname fi.ftype
                   else begin
                     let typestr = encodeType fi.ftype in
                     annstr := !annstr ^ ", " ^ quoted fi.fname 
                                       ^ ", " ^ quoted typestr
                   end)
                ci.cfields;
              let ann = globalAnn structANN !annstr in
              ChangeDoChildrenPost(
                [ann; g],
                (fun g -> g)
              )
            end
            else begin
              ignore (unimplementedT (TComp(ci,[])));
              SkipChildren
            end
        | GVar (vi, _, l) ->
            (* ignore (E.log "annotating %s: %a\n" vi.vname d_type vi.vtype); *)
            (match vi.vtype with
                 TArray(bt, leno, a) when (bitsSizeOf bt) < 32 ->
                   (* FIXME: hack for chars.  Expand this array so its 
                      length is a multiple of 4. *)
                   let len = arrayLen leno in
                   let len' = ((len + 3) / 4) * 4 in
                   assert (len'>=len && len'<len+4);
                   vi.vtype <- TArray(bt, Some (integer len'), a);
               | _ -> ());
            let ann = 
              match vi.vtype with
                  TArray _ ->
                    let size, bt = getSize vi.vtype in
                    globalAnn globalarrayANN
                      (quotedLabel vi.vname
                       ^ ", " 
                       ^ quoted (encodeType bt)
                       ^ ", " 
                       ^ (stringOf size))
                | TFun _ -> E.s (bug "Use GVarDecl for function prototypes.")
                | _ -> globalAnn globalANN (quotedLabel vi.vname
                                            ^ ", " 
                                            ^ quoted (encodeType vi.vtype))
            in
            ChangeDoChildrenPost( 
              [ann; g],
              (fun g -> g)
            )
      | _ -> 
          DoChildren
    with e -> 
      (* DoChildren *)
      raise e
  end
        
end


(****  Entry point to the transformation ****)

let entry_point (f : file) =
  ignore (E.log "Annotating function parameters.\n");
  let longlongU =
    globalAnn structANN 
      ("\"UbuiltinLongLong\", \"q1\", \"Uint\", \"q2\", \"Uint\"") in
  let longlongT =
    globalAnn structANN 
      ("\"TbuiltinLongLong\", \"q1\", \"Tint\", \"q2\", \"Tint\"") in
  newGlobals := [longlongU; longlongT];
  visitCilFileSameGlobals (new stringVisitor :>cilVisitor) f;
  f.globals <- Util.list_append !newGlobals f.globals;
  visitCilFile (new annotationVisitor :>cilVisitor) f;
  visitCilFileSameGlobals (new smallocClearAttributes sensitive_attributes ) f;
  ()



let enableAnn = ref false 

(***********************
 * The Cil.featureDesc that tells the CIL front-end how to call this module.
 * This is the only value that needs to be exported from smalloc.ml. **)

let feature : featureDescr = 
  { fd_name = "CqualAnn";
    fd_enabled = enableAnn;
    fd_description = "adding assembly annotations for Cqual qualifiers." ;
    fd_extraopt = [];
    fd_doit = entry_point;
    fd_post_check = true
  } 

