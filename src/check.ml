(* A consistency checker for CIL *)
open Cil
open Pretty


  (* Attributes must be sorted *)
let checkAttributes (attrs: attribute list) : unit = 
  let aName = function (* Attribute name *)
      AId s -> s | ACons (s, _) -> s
    | _ -> E.s (E.unimp "Unexpected attribute")
  in 
  let rec loop lastname = function
      [] -> ()
    | a :: resta -> 
        let an = aName a in
        ignore (an >= lastname || (E.s (E.bug "Attributes not sorted")));
        loop an resta
  in
  loop "" attrs


  (* Keep track of defined types *)
let typeDefs : (string, typ) H.t = H.create 117

  (* Keep track of defined struct/union/enum tags *)
let structTags : (string, typ) H.t = H.create 37
let unionTags  : (string, typ) H.t = H.create 37
let enumTags   : (string, typ) H.t = H.create 37


  (* Keep track of all variables names, enum tags and type names *)
let varNamesEnv : (string, unit) H.t = H.create 117

  (* We also keep a map of variables indexed by id, to ensure that only one 
   * varinfo has a given id *)
let varIdsEnv : (int, varinfo) H.t = H.create 117
 (* Also keep a list of environments. We place an empty string in the list to 
  * mark the start of a local environment (i.e. a function) *)
let varNamesList : (string * int) list ref = ref []
let defineName s = 
  if s = "" then
    E.s (E.bug "Empty name\n"); 
  if H.mem varNamesEnv s then
    E.s (E.bug "Multiple definitions for %s\n" s);
  H.add varNamesEnv s ()

let defineVariable vi = 
  defineName vi.vname;
  varNamesList := (vi.vname, vi.vid) :: !varNamesList;
  (* Check the id *)
  if vi.vglob then 
    if vi.vid <> H.hash vi.vname then
      E.s (E.bug "Id of global %s is not valid\n" vi.vname);
  if H.mem varIdsEnv vi.vid then
    E.s (E.bug "Id %d is already defined (%s)\n" vi.vid vi.vname);
  H.add varIdsEnv vi.vid vi

(* Check that a varinfo has already been registered *)
let checkVariable vi = 
  try
    if vi != H.find varIdsEnv vi.vid then
      E.s (E.bug "varinfos for %s not shared\n" vi.vname);
  with Not_found -> 
    E.s (E.bug "Unknown id (%d) for %s\n" vi.vid vi.vname)


let startEnv () = 
  varNamesList := ("", -1) :: !varNamesList

let endEnv () = 
  let rec loop = function
      [] -> E.s (E.bug "Cannot find start of env")
    | ("", _) :: rest -> varNamesList := rest
    | (s, id) :: rest -> begin
        H.remove varNamesEnv s;
        H.remove varIdsEnv id;
        loop rest
    end
  in
  loop !varNamesList
    

(* The current function being checked *)
let currentReturnType : typ ref = ref voidType

(* A map of labels in the current function *)
let labels : (string, unit) H.t = H.create 17
let gotos  : (string, unit) H.t = H.create 17

(*** TYPES ***)
(* Cetain types can only occur in some contexts, so keep a list of context *)
type ctxType = 
    CTStruct                            (* In a composite type *)
  | CTUnion
  | CTFArg                              (* In a function argument type *)
  | CTFRes                              (* In a function result type *)
  | CTArray                             (* In an array type *)
  | CTPtr                               (* In a pointer type *)
  | CTOther                             (* Somewhere else *)

  (* Check a type *)
let rec checkType (t: typ) (ctx: ctxType) = 
  (* Check that it appears in the right context *)
  let rec checkContext = function
      TVoid _ -> ctx = CTPtr || ctx = CTFRes
    | TBitfield _ ->  ctx = CTStruct  (* bitfields only in structures *)
    | TNamed (_, t, a) -> checkContext t
    | TArray _ -> ctx <> CTPtr && ctx <> CTFArg && ctx <> CTFRes
    | _ -> true
  in
  ignore (checkContext t || (E.s (E.bug "Type used in wrong context")));
  match t with
    TVoid a -> checkAttributes a
  | TInt (ik, a) -> checkAttributes a
  | TFloat (_, a) -> checkAttributes a
  | TBitfield (ik, w, a) -> 
      checkAttributes a;
      ignore ((w >= 0 && w <= bitsSizeOf (TInt(ik, a))) || 
               E.s (E.bug "Wrong width (%d) in bitfield" w))

  | TPtr (t, a) -> checkAttributes a;  checkType t CTPtr

  | TNamed (n, t, a) -> 
        (* The name must be already defined. The t must be identical to the 
         * one used in the definition. We assume that the type is checked *)
      ignore ((try t == H.find typeDefs n with Not_found -> false) ||
              E.s (E.bug "Named type %s invalid" n));
      checkAttributes a

  | TForward (iss, n, tr, a) -> 
      checkAttributes a;
      (match !tr with 
        TComp (iss', n', _, _, tr') as t' ->
          if not  
              (tr == tr' && (* They must share the self cell *)
               iss' = iss &&
               n = n') then
            E.s (E.bug "Invalid TForward(%s)" n);
          checkType t' ctx

      | _ -> E.s (E.bug "TForward does not point to TComp"))

  | TComp(iss, n, fields, a, tr) -> begin
      (* Make sure the self pointer is set properly *)
      ignore (!tr == t || E.s (E.bug "Self pointer not set in %s" n));
      (* Name cannot be empty *)
      ignore (n <> "" || E.s (E.bug "TComp with empty name"));
      try
        let t' = H.find (if iss then structTags else unionTags) n in
        (* If we have seen one with the same name then it must be the same 
         * exact one. This check also avoids looping.  *)
        if t != t' then 
          E.s (E.bug "Redefinition of struct/union tag %s" n)
      with Not_found -> begin
        (* Add it to the map, before we check the fields *)
        H.add (if iss then structTags else unionTags) n t;
        let fctx = if iss then CTStruct else CTUnion in
        let rec checkField f =
          if not 
              (f.fcomp == tr &&  (* Each field must share the self cell of 
                                  * the host *)
               f.fname <> "") then
            E.s (E.bug "Self pointer not set in field %s of %s" f.fname n);
          checkType f.ftype fctx;
          checkAttributes f.fattr
        in
        List.iter checkField fields
      end
  end

  | TEnum (n, tags, a) -> begin
      checkAttributes a;
      ignore (n <> "" || E.s (E.bug "Enum with empty tag"));
      try
        let t' = H.find enumTags n in
        (* If we have seen one with the same name then it must be the same 
         * one  *)
        if t != t' then 
          E.s (E.bug "Redefinition of enum %s" n)
      with Not_found -> 
        (* Add it to the enumTags *)
        H.add enumTags n t;
        List.iter (fun (tn, _) -> defineName tn) tags
  end

  | TArray(bt, len, a) -> 
      checkAttributes a;
      checkType bt CTArray;
      (match len with
        None -> ()
      | Some l -> checkExpType true l intType)

  | TFun (rt, targs, isva, a) -> 
      checkAttributes a;
      checkType rt CTFRes;
      List.iter 
        (fun ta -> 
          checkType ta.vtype CTFArg;
          checkAttributes ta.vattr;
          if not (not ta.vglob &&
                  ta.vstorage <> Extern &&
                  ta.vstorage <> Static &&
                  not ta.vaddrof) then
            E.s (E.bug "Invalid argument varinfo")) targs

and checkLval (lv: lval) : typ = typeOfLval lv
        
and checkExpType (isconstt: bool) (e: exp) (t: typ) = ()


and checkStmt (s: stmt) = 
  match s with
    Skip | Break | Continue | Default | Case _ -> ()
  | Sequence ss -> List.iter checkStmt ss
  | Loop s -> checkStmt s
  | Label l -> begin
      if H.mem labels l then
        E.s (E.bug "Multiply defined label %s" l);
      H.add labels l ()
  end
  | Goto l -> H.add gotos l ()
  | IfThenElse (e, st, sf) -> 
      checkExpType false e intType;
      checkStmt st;
      checkStmt sf
  | Return re -> begin
      match re, !currentReturnType with
        None, TVoid _  -> ()
      | _, TVoid _ -> E.s (E.bug "Invalid return value")
      | None, _ -> E.s (E.bug "Invalid return value")
      | Some re', rt' -> checkExpType false re' rt'
  end
  | Switch (e, s) -> 
      checkExpType false e intType;
      checkStmt s

  | Instr (Set (dest, e, _)) -> 
      let t = checkLval dest in
      (* Not all types can be assigned to *)
      (match unrollType t with
        TFun _ -> E.s (E.bug "Assignment to a function type")
      | TArray _ -> E.s (E.bug "Assignment to an array type")
      | TVoid _ -> E.s (E.bug "Assignment to a void type")
      | _ -> ());
      checkExpType false e t

  | Instr (Call(dest, what, args, _)) -> 
      let (rt, formals, isva) = 
        match unrollType (typeOf what) with
          TFun(rt, formals, isva, _) -> rt, formals, isva
        | _ -> E.s (E.bug "Call to a non-function")
      in
      (* Now check the return value*)
      (match dest, unrollType rt with
        None, TVoid _ -> ()
      | Some _, TVoid _ -> E.s (E.bug "Call of subroutine is assigned")
      | None, _ -> E.s (E.bug "Call of function is not assigned")
      | Some destvi, _ -> 
          checkVariable destvi;
          if typeSig destvi.vtype <> typeSig rt then
            E.s (E.bug "Mismatch at return type in call"));
      (* Now check the arguments *)
      let rec loopArgs formals args = 
        match formals, args with
          [], _ when (isva || args = []) -> ()
        | fo :: formals, a :: args -> 
            checkExpType false a fo.vtype;
            loopArgs formals args

        | _, _ -> E.s (E.bug "Not enough arguments")
      in
      loopArgs formals args
          
  | Instr (Asm _) -> ()  (* Not yet implemented *)
  
let rec checkGlobal = function
    GAsm _ -> ()
  | GPragma _ -> ()
  | GType (n, t) -> 
      E.withContext (fun _ -> dprintf "GType(%s)" n)
        (fun _ ->
          checkType t CTOther;
          if H.mem typeDefs n then
            E.s (E.bug "Type %s is multiply defined" n);
          defineName n;
          H.add typeDefs n t)
        ()

  | GDecl vi -> 
      (* We might have seen it already *)
      E.withContext (fun _ -> dprintf "GDecl(%s)" vi.vname)
        (fun _ -> 
          (* If we have seen this vid already then it must be for the exact 
           * same varinfo *)
          if H.mem varIdsEnv vi.vid then
            checkVariable vi
          else begin
            defineVariable vi; 
            checkAttributes vi.vattr;
            checkType vi.vtype CTOther;
            if not (vi.vglob &&
                    vi.vstorage <> Register) then
              E.s (E.bug "Invalid declaration of %s" vi.vname)
          end)
        ()
        
  | GVar (vi, init) -> 
      (* Maybe this is the first occurrence *)
      E.withContext (fun _ -> dprintf "GVar(%s)" vi.vname)
        (fun _ -> 
          checkGlobal (GDecl vi);
          (* Check the initializer *)
          begin match init with
            None -> ()
          | Some i -> ignore (checkExpType true i vi.vtype)
          end)
        ()
        

  | GFun fd -> begin
      (* Check if this is the first occurrence *)
      let vi = fd.svar in
      let fname = vi.vname in
      E.withContext (fun _ -> dprintf "GFun(%s)" fname)
        (fun _ -> 
          checkGlobal (GDecl vi);
          (* Check that the argument types in the type match the formals *)
          let rec loopArgs targs formals = 
            match targs, formals with
              [], [] -> ()
            | ta :: targs, fo :: formals -> 
                if typeSig ta.vtype <> typeSig fo.vtype then
                  E.s (E.bug "Inconsistent type for formal %s in %s" 
                         fo.vname fname);
                loopArgs targs formals

            | _ -> 
                E.s (E.bug "Type has different number of formals for %s" 
                       fname)
          in
          begin match vi.vtype with
            TFun (rt, args, isva, a) -> begin
              currentReturnType := rt;
              loopArgs args fd.sformals
            end
          | _ -> E.s (E.bug "Function %s does not have a function type" 
                        fname)
          end;
          ignore (fd.smaxid >= 0 || E.s (E.bug "smaxid < 0 for %s" fname));
          (* Now start a new environment, in a finally clause *)
          begin try
            startEnv ();
            (* Do the locals *)
            let doLocal v = 
              if not 
                  (v.vid >= 0 && v.vid <= fd.smaxid && not v.vglob &&
                   v.vstorage <> Extern) then
                E.s (E.bug "Invalid local %s in %s" v.vname fname);
              checkType v.vtype CTOther;
              checkAttributes v.vattr;
              defineVariable v
            in
            List.iter doLocal fd.sformals;
            List.iter doLocal fd.slocals;
            checkStmt fd.sbody;
            (* Now check that all gotos have a target *)
            H.iter 
              (fun k _ -> if not (H.mem labels k) then 
                E.s (E.bug "Label %s is not defined" k)) gotos;
            H.clear labels;
            H.clear gotos;
            (* Done *)
            endEnv ()
          with e -> 
            endEnv ();
            raise e
          end;
          ())
        () (* final argument of withContext *)
  end


let checkFile fl = 
  List.iter (fun g -> try checkGlobal g with _ -> ()) fl;
  (* Clean the hashes to let the GC do its job *)
  H.clear typeDefs;
  H.clear structTags;
  H.clear unionTags;
  H.clear enumTags;
  H.clear varNamesEnv;
  H.clear varIdsEnv;
  varNamesList := [];
  true
  
