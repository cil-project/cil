(* A consistency checker for CIL *)
open Cil
open Pretty


  (* Attributes must be sorted *)
let checkAttributes (attrs: attribute list) = 
  let aName = function (* Attribute name *)
      AId s -> s | ACons (s, _) -> s
    | _ -> E.s (E.unimp "Unexpected attribute")
  in 
  let rec loop lastname = function
      [] -> true
    | a :: resta -> 
        let an = aName a in
        an >= lastname && loop an resta
  in
  loop "" attrs || E.s (E.bug "Attributes not sorted")


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
  checkContext t &&
  match t with
    TVoid a -> checkAttributes a
  | TInt (ik, a) -> checkAttributes a
  | TFloat (_, a) -> checkAttributes a
  | TBitfield (ik, w, a) -> 
      checkAttributes a &&
      w >= 0 &&
      w <= bitsSizeOf (TInt(ik, a))

  | TPtr (t, a) -> 
      checkAttributes a && checkType t CTPtr

  | TNamed (n, t, a) -> 
        (* The name must be already defined. The t must be identical to the 
         * one used in the definition. We assume that the type is checked *)
      (try t == H.find typeDefs n with Not_found -> false) &&
      checkAttributes a

  | TForward (iss, n, tr, a) -> 
      checkAttributes a &&
      (match !tr with 
        TComp (iss', n', _, _, tr') as t' -> 
          tr == tr' && (* They must share the self cell *)
          iss' = iss &&
          n = n' &&
          checkType t' ctx
      | _ -> E.s (E.bug "TForward does not point to TComp"))

  | TComp(iss, n, fields, a, tr) -> begin
      !tr == t &&  (* Make sure the self pointer is set properly *)
      n <> "" && (* Name cannot be empty *)
      try
        let t' = H.find (if iss then structTags else unionTags) n in
        t == t' (* If we have seen one with the same name then it must be the 
                 * same exact one. This check also avoids looping. *)
      with Not_found -> 
        (* Add it to the map, before we check the fields *)
        H.add (if iss then structTags else unionTags) n t;
        let fctx = if iss then CTStruct else CTUnion in
        let rec checkFields = function
            [] -> true (* Ought to check that there are fields *)
          | f :: rest -> 
              (f.fcomp == tr &&  (* Each field must share the self cell of 
                                  * the host *)
               f.fname <> "" &&
               checkType f.ftype fctx &&
               checkAttributes f.fattr &&
               checkFields rest)
        in
        checkFields fields
  end

  | TEnum (n, tags, a) -> begin
      checkAttributes a &&
      n <> "" &&
      try
        let t' = H.find enumTags n in
        t == t' (* If we have seen one with the same name then it must be the 
                 * same one *)
      with Not_found -> 
        (* Add it to the enumTags *)
        H.add enumTags n t;
        let rec checkTags = function
            [] -> true
          | (tn, _) :: rest -> defineName tn; checkTags rest
        in
        checkTags tags
  end

  | TArray(bt, len, a) -> 
      checkAttributes a &&
      checkType bt CTArray &&
      (match len with
        None -> true
      | Some l -> checkExp true l intType)

  | TFun (rt, targs, isva, a) -> 
      checkAttributes a &&
      checkType rt CTFRes &&
      List.for_all (fun ta -> checkType ta.vtype CTFArg &&
                              checkAttributes ta.vattr &&
                              not ta.vglob &&
                              ta.vstorage <> Extern &&
                              ta.vstorage <> Static &&
                              not ta.vaddrof) targs
        
        
and checkExp (isconstt: bool) (e: exp) (t: typ) = true
  
let rec checkGlobal = function
    GAsm _ -> ()
  | GPragma _ -> ()
  | GType (n, t) -> 
      E.withContext (fun _ -> dprintf "GType(%s)" n)
        (fun _ ->
          ignore (checkType t CTOther && not (H.mem typeDefs n));
          defineName n;
          H.add typeDefs n t)
        ()

  | GDecl vi -> 
      E.withContext (fun _ -> dprintf "GDecl(%s)" vi.vname)
        (fun _ -> 
          defineVariable vi; 
          ignore (vi.vglob &&
                  vi.vstorage <> Register &&
                  checkAttributes vi.vattr &&
                  checkType vi.vtype CTOther))
        ()
        
  | GFun fd -> begin
      (* Check if this is the first occurrence *)
      let vi = fd.svar in
      E.withContext (fun _ -> dprintf "GFun(%s)" vi.vname)
        (fun _ -> 
          checkGlobal (GDecl vi);
          ())
        ()
  end

  | GVar (vi, init) -> 
      (* Maybe this is the first occurrence *)
      E.withContext (fun _ -> dprintf "GVar(%s)" vi.vname)
        (fun _ -> 
          checkGlobal (GDecl vi);
          (* Check the initializer *)
          begin match init with
            None -> ()
          | Some i -> ignore (checkExp true i vi.vtype)
          end)
        ()
        

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
  
