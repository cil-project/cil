(* A consistency checker for CIL *)
open Cil
open Pretty


  (* Attributes must be sorted *)
type ctxAttr = 
    CALocal                             (* Attribute of a local variable *)
  | CAGlobal                            (* Attribute of a global variable *)
  | CAType                              (* Attribute of a type *)

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
  | CTExp                               (* In an expression, as the type of 
                                         * the result of binary operators, or 
                                         * in a cast *)
  | CTSizeof                            (* In a sizeof *)
  | CTDecl                              (* In a typedef, or a declaration *)


let compInfoNameEnv : (string, unit) H.t = H.create 17
let compInfoIdEnv : (int, compinfo) H.t = H.create 117

(* Keep track of all TForward that we have see *)
let compForwards : (int, compinfo) H.t = H.create 117
(* Keep track of all definitions that we have seen *)
let compDefined : (int, unit) H.t = H.create 117

 
    

  (* Check a type *)
let rec checkType (t: typ) (ctx: ctxType) = 
  (* Check that it appears in the right context *)
  let rec checkContext = function
      TVoid _ -> ctx = CTPtr || ctx = CTFRes
    | TBitfield _ ->  ctx = CTStruct  (* bitfields only in structures *)
    | TNamed (_, t, a) -> checkContext t
    | TArray _ -> 
        (ctx = CTStruct || ctx = CTUnion || ctx = CTSizeof || ctx = CTDecl)
    | TComp _ -> ctx <> CTExp 
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

  | TForward (comp, a) -> 
      checkAttributes a;
      checkCompInfo comp;
      (* Mark it as a forward *)
      H.add compForwards comp.ckey comp

  | TComp comp -> 
      checkCompInfo comp;
      (* Mark it as a definition *)
      H.add compDefined comp.ckey ()

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

(* Check that a type is a promoted integral type *)
and checkIntegralType (t: typ) = 
  checkType t CTExp;
  match unrollType t with
    TInt _ -> ()
  | _ -> E.s (E.bug "Non-integral type")

(* Check that a type is a promoted arithmetic type *)
and checkArithmeticType (t: typ) = 
  checkType t CTExp;
  match unrollType t with
    TInt _ | TFloat _ -> ()
  | _ -> E.s (E.bug "Non-arithmetic type")

(* Check that a type is a promoted boolean type *)
and checkBooleanType (t: typ) = 
  checkType t CTExp;
  match unrollType t with
    TInt _ | TFloat _ | TPtr _ -> ()
  | _ -> E.s (E.bug "Non-boolean type")


(* Check that a type is a pointer type *)
and checkPointerType (t: typ) = 
  checkType t CTExp;
  match unrollType t with
    TPtr _ -> ()
  | _ -> E.s (E.bug "Non-pointer type")


and typeMatch (t1: typ) (t2: typ) = 
  if typeSig t1 <> typeSig t2 then
    E.s (E.bug "Type mismatch:@!    %a@!and %a@!" d_type t1 d_type t2)

and checkCompInfo comp = 
  (* Check if we have seen it already *)
  let fullname = compFullName comp in
  try
    let oldci = H.find compInfoIdEnv comp.ckey in
    if oldci != comp then
      E.s (E.bug "Compinfo for %s is not shared" fullname)
  with Not_found -> begin
    (* Check that the name is not empty *)
    if comp.cname = "" then 
      E.s (E.bug "Compinfo with empty name");
    (* Check that the name is unique *)
    if H.mem compInfoNameEnv fullname then
      E.s (E.bug "Duplicate name %s" fullname);
    (* Check that the ckey is correct *)
    if comp.ckey <> H.hash fullname then
      E.s (E.bug "Invalid ckey for compinfo %s" fullname);
    (* Add it to the map before we go on *)
    H.add compInfoNameEnv fullname ();
    H.add compInfoIdEnv comp.ckey comp;
    let fctx = if comp.cstruct then CTStruct else CTUnion in
    let rec checkField f =
      if not 
          (f.fcomp == comp &&  (* Each field must share the self cell of 
                                * the host *)
           f.fname <> "") then
        E.s (E.bug "Self pointer not set in field %s of %s" f.fname fullname);
      checkType f.ftype fctx;
      checkAttributes f.fattr
    in
    List.iter checkField comp.cfields
  end
    

and checkLval (isconst: bool) (lv: lval) : typ = 
  match lv with
    Var vi, off -> 
      checkVariable vi; 
      checkOffset vi.vtype off

  | Mem addr, off -> begin
      if isconst then
        E.s (E.bug "Memory operation in constant");
      let ta = checkExp false addr in
      match unrollType ta with
        TPtr (t, _) -> checkOffset t off
      | _ -> E.s (E.bug "Mem on a non-pointer")
  end

and checkOffset basetyp = function
    NoOffset -> basetyp
  | Index (ei, o) -> 
      checkExpType false ei intType; checkOffset basetyp o
  | Field (fi, o) -> 
      (* Make sure we have seen the type of the host *)
      if not (H.mem compInfoIdEnv fi.fcomp.ckey) then
        E.s (E.bug "The host of field %s is not defined" fi.fname);
      (* Now check that the host is shared propertly *)
      checkCompInfo fi.fcomp;
      (* Check that this exact field is part of the host *)
      if not (List.exists (fun f -> f == fi) fi.fcomp.cfields) then
        E.s (E.bug "Field %s not part of %s" fi.fname (compFullName fi.fcomp));
      checkOffset fi.ftype o

  | First o -> begin
      match unrollType basetyp with
        TArray (t, _, _) -> checkOffset t o
      | t -> E.s (E.bug "typeOffset: First on a non-array: %a" d_plaintype t)
  end
        
and checkExpType (isconst: bool) (e: exp) (t: typ) =
  let t' = checkExp isconst e in (* compute the type *)
  typeMatch t' t

and checkExp (isconst: bool) (e: exp) : typ = 
  E.withContext 
    (fun _ -> dprintf "check%s: %a" 
        (if isconst then "Const" else "Exp") d_exp e)
    (fun _ ->
      match e with
        Const(CInt (_, ik, _), _) -> TInt(ik, [])
      | Const(CChr _, _) -> charType
      | Const(CStr _, _) -> charPtrType 
      | Const(CReal (_, fk, _), _) -> TFloat(fk, [])
      | Lval(lv) -> 
          if isconst then
            E.s (E.bug "Lval in constant");
          checkLval isconst lv

      | SizeOf(t, _) -> begin
          (* Sizeof cannot be applied to certain types *)
          checkType t CTSizeof;
          match unrollType t with
            (TFun _ | TVoid _ | TBitfield _) -> 
              E.s (E.bug "Invalid operand for sizeof")
          | _ -> uintType
      end
      | UnOp (Neg, e, tres, _) -> 
          checkArithmeticType tres; checkExpType isconst e tres; tres

      | UnOp (BNot, e, tres, _) -> 
          checkIntegralType tres; checkExpType isconst e tres; tres

      | UnOp (LNot, e, tres, _) -> 
          let te = checkExp isconst e in
          checkBooleanType te;
          checkIntegralType tres; (* Must check that t is well-formed *)
          typeMatch tres intType;
          tres

      | BinOp (bop, e1, e2, tres, _) -> begin
          let t1 = checkExp isconst e1 in
          let t2 = checkExp isconst e2 in
          match bop with
            (Mult | Div | Eq |Ne|Lt|Le|Ge|Gt) -> 
              typeMatch t1 t2; checkArithmeticType tres; 
              typeMatch t1 tres; tres
          | Mod|BAnd|BOr|BXor -> 
              typeMatch t1 t2; checkIntegralType tres;
              typeMatch t1 tres; tres
          | Shiftlt | Shiftrt -> 
              typeMatch t1 tres; checkIntegralType t1; 
              checkIntegralType t2; tres
          | (PlusA | MinusA) -> 
                typeMatch t1 t2; typeMatch t1 tres;
                checkArithmeticType tres; tres
          | (PlusPI | MinusPI) -> 
              checkPointerType tres;
              typeMatch t1 tres;
              checkIntegralType t2;
              tres
          | (MinusPP | EqP | NeP | LtP | LeP | GeP | GtP)  -> 
              checkPointerType t1; checkPointerType t2;
              typeMatch tres intType; 
              tres
      end
      | Question (eb, et, ef, _) -> 
          let tb = checkExp isconst eb in
          checkBooleanType tb;
          let tt = checkExp isconst et in
          let tf = checkExp isconst ef in
          typeMatch tt tt;
          tt

      | AddrOf (lv, _) -> begin
          let tlv = checkLval isconst lv in
          (* Only certain types can be in AddrOf *)
          match unrollType tlv with
          | TArray _ -> E.s (E.bug "AddrOf on an array")
          | TBitfield _ -> E.s (E.bug "AddrOf on a bitfield")
          | TVoid _ -> E.s (E.bug "AddrOf on void")
          | (TInt _ | TFloat _ | TPtr _ | TComp _ | TFun _ ) -> 
              TPtr(tlv, [])
          | TEnum _ -> intPtrType
          | _ -> E.s (E.bug "AddrOf on unknown type")
      end

      | StartOf lv -> begin
          let tlv = checkLval isconst lv in
          match unrollType tlv with
            TArray (t,_, _) -> TPtr(t, [])
          | TFun _ as t -> TPtr(t, [])
          | _ -> E.s (E.bug "typeOf: StartOf on a non-array or non-function")
      end
            
      | Compound (t, initl) -> 
          checkType t CTSizeof;
          let checkOneExp (oo: offset option) (ei: exp) (et: typ) _ : unit = 
            (match oo with
              None -> ()
            | Some o -> 
                let ot = checkOffset t o in
                typeMatch et ot);
            checkExpType isconst ei et
          in
          (* foldLeftCompound will check that t is a TComp or a TArray *)
          foldLeftCompound checkOneExp t initl ();
          t

      | CastE (tres, e, _) -> begin
          let et = checkExp isconst e in
          checkType tres CTExp;
          (* Not all types can be cast *)
          match unrollType et with
            TArray _ -> E.s (E.bug "Cast of an array type")
          | TFun _ -> E.s (E.bug "Cast of a function type")
          | TComp _ -> E.s (E.bug "Cast of a composite type")
          | TVoid _ -> E.s (E.bug "Cast of a void type")
          | _ -> tres
      end)
    () (* The argument of withContext *)

and checkStmt (s: stmt) = 
  E.withContext 
    (fun _ -> 
      (* Print context only for certain small statements *)
      match s with 
        Sequence _ | Loop _ | IfThenElse _ | Switch _  -> nil
      | _ -> dprintf "checkStmt: %a" d_stmt s)
    (fun _ -> 
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
          let te = checkExp false e in
          checkBooleanType te;
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
          let t = checkLval false dest in
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
          | None, _ -> () (* "Call of function is not assigned" *)
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
            
      | Instr (Asm _) -> ())  (* Not yet implemented *)
    () (* The argument to withContext *)
  
let rec checkGlobal = function
    GAsm _ -> ()
  | GPragma _ -> ()
  | GType (n, t) -> 
      E.withContext (fun _ -> dprintf "GType(%s)" n)
        (fun _ ->
          checkType t CTDecl;
          if n <> "" then begin
            if H.mem typeDefs n then
              E.s (E.bug "Type %s is multiply defined" n);
            defineName n;
            H.add typeDefs n t
          end else begin
            match unrollType t with
              TComp _ -> ()
            | _ -> E.s (E.bug "Empty type name for type %a" d_type t)
          end)
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
            checkType vi.vtype CTDecl;
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
            let doLocal tctx v = 
              if not 
                  (v.vid >= 0 && v.vid <= fd.smaxid && not v.vglob &&
                   v.vstorage <> Extern) then
                E.s (E.bug "Invalid local %s in %s" v.vname fname);
              checkType v.vtype tctx;
              checkAttributes v.vattr;
              defineVariable v
            in
            List.iter (doLocal CTFArg) fd.sformals;
            List.iter (doLocal CTDecl) fd.slocals;
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
  (* Check that for all TForward there is a definition *)
  H.iter 
    (fun k comp -> if not (H.mem compDefined k) then 
      E.s (E.bug "Compinfo %s is not defined" (compFullName comp))) 
    compForwards;
  (* Clean the hashes to let the GC do its job *)
  H.clear typeDefs;
  H.clear structTags;
  H.clear unionTags;
  H.clear enumTags;
  H.clear varNamesEnv;
  H.clear varIdsEnv;
  H.clear compInfoNameEnv;
  H.clear compInfoIdEnv;
  H.clear compForwards;
  H.clear compDefined;
  varNamesList := [];
  true
  
