(* A consistency checker for CIL *)
open Cil
module E = Errormsg
module H = Hashtbl
open Pretty


(* A few parameters to customize the checking *)
type checkFlags = 
    NoCheckGlobalIds   (* Do not check that the global ids have the proper 
                        * hash value *)

let checkGlobalIds = ref true

  (* Attributes must be sorted *)
type ctxAttr = 
    CALocal                             (* Attribute of a local variable *)
  | CAGlobal                            (* Attribute of a global variable *)
  | CAType                              (* Attribute of a type *)

let checkAttributes (attrs: attribute list) : unit = 
  let rec loop lastname = function
      [] -> ()
    | (Attr(an, _) as a) :: resta -> 
        if an < lastname then
          ignore (warn "Attributes not sorted");
        loop an resta
  in
  loop "" attrs


  (* Keep track of defined types *)
let typeDefs : (string, typ) H.t = H.create 117


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
    E.s (bug "Empty name\n"); 
  if H.mem varNamesEnv s then
    ignore (warn "Multiple definitions for %s\n" s);
  H.add varNamesEnv s ()

let defineVariable vi = 
  defineName vi.vname;
  varNamesList := (vi.vname, vi.vid) :: !varNamesList;
  (* Check the id *)
  if vi.vglob then 
    if !checkGlobalIds && vi.vid <> H.hash vi.vname then
      ignore (warn "Id of global %s is not valid\n" vi.vname);
  if H.mem varIdsEnv vi.vid then
    ignore (warn "Id %d is already defined (%s)\n" vi.vid vi.vname);
  H.add varIdsEnv vi.vid vi

(* Check that a varinfo has already been registered *)
let checkVariable vi = 
  try
    if vi != H.find varIdsEnv vi.vid then
      ignore (warn "varinfos for %s not shared\n" vi.vname);
  with Not_found -> 
    ignore (warn "Unknown id (%d) for %s\n" vi.vid vi.vname)


let startEnv () = 
  varNamesList := ("", -1) :: !varNamesList

let endEnv () = 
  let rec loop = function
      [] -> E.s (bug "Cannot find start of env")
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

let d_context () = function
    CTStruct -> text "CTStruct"
  | CTUnion -> text "CTUnion"
  | CTFArg -> text "CTFArg"
  | CTFRes -> text "CTFRes"
  | CTArray -> text "CTArray"
  | CTPtr -> text "CTPtr"
  | CTExp -> text "CTExp"
  | CTSizeof -> text "CTSizeof"
  | CTDecl -> text "CTDecl"

let compInfoNameEnv : (string, unit) H.t = H.create 17
let compInfoIdEnv : (int, compinfo) H.t = H.create 117

(* Keep track of all tags that we use. *)
let compForwards : (int, compinfo) H.t = H.create 117
let enumForwards : (string, enuminfo) H.t = H.create 117
(* Keep track of all tags that we define *)
let compDefined : (int, compinfo) H.t = H.create 117
let enumDefined : (string, enuminfo) H.t = H.create 117

 
    

  (* Check a type *)
let rec checkType (t: typ) (ctx: ctxType) = 
  (* Check that it appears in the right context *)
  let rec checkContext = function
      TVoid _ -> ctx = CTPtr || ctx = CTFRes
    | TNamed (_, t, a) -> checkContext t
    | TArray _ -> 
        (ctx = CTStruct || ctx = CTUnion 
         || ctx = CTSizeof || ctx = CTDecl || ctx = CTArray || ctx = CTPtr)
    | TComp _ -> ctx <> CTExp 
    | _ -> true
  in
  if not (checkContext t) then 
    ignore (warn "Type (%a) used in wrong context. Expected context: %a"
              d_plaintype t d_context ctx);
  match t with
    TVoid a -> checkAttributes a
  | TInt (ik, a) -> checkAttributes a
  | TFloat (_, a) -> checkAttributes a
  | TPtr (t, a) -> checkAttributes a;  checkType t CTPtr

  | TNamed (n, t, a) -> 
        (* The name must be already defined. The t must be identical to the 
         * one used in the definition. We assume that the type is checked *)
      (try
        let oldt = H.find typeDefs n in
        if oldt != t then
          if typeSig oldt <> typeSig (unrollType t) then
            ignore (warn "Named type %s is inconsistent.@!In typedef: %a@!Now: %a" n d_plaintype oldt d_plaintype t)
      with Not_found -> 
        ignore (warn "Named type %s is undefined\n" n));
      checkAttributes a

  | TComp (comp, a) -> (* A forward reference *)
      checkAttributes a;
      (* Mark it as a forward. We'll check it later. If we try to check it 
       * now we might encounter undefined types *)
      H.add compForwards comp.ckey comp


  | TEnum (enum, a) -> begin
      checkAttributes a;
      H.add enumForwards enum.ename enum
  end

  | TArray(bt, len, a) -> 
      checkAttributes a;
      checkType bt CTArray;
      (match len with
        None -> ()
      | Some l -> begin
          let t = checkExp true l in
          match t with 
            TInt((IInt|IUInt), _) -> ()
          | _ -> E.s (bug "Type of array length is not integer")
      end)

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
            ignore (warn "Invalid argument varinfo")) targs

(* Check that a type is a promoted integral type *)
and checkIntegralType (t: typ) = 
  checkType t CTExp;
  match unrollType t with
    TInt _ -> ()
  | _ -> ignore (warn "Non-integral type")

(* Check that a type is a promoted arithmetic type *)
and checkArithmeticType (t: typ) = 
  checkType t CTExp;
  match unrollType t with
    TInt _ | TFloat _ -> ()
  | _ -> ignore (warn "Non-arithmetic type")

(* Check that a type is a promoted boolean type *)
and checkBooleanType (t: typ) = 
  checkType t CTExp;
  match unrollType t with
    TInt _ | TFloat _ | TPtr _ -> ()
  | _ -> ignore (warn "Non-boolean type")


(* Check that a type is a pointer type *)
and checkPointerType (t: typ) = 
  checkType t CTExp;
  match unrollType t with
    TPtr _ -> ()
  | _ -> ignore (warn "Non-pointer type")


and typeMatch (t1: typ) (t2: typ) = 
  if typeSig t1 <> typeSig t2 then
    match unrollType t1, unrollType t2 with
    (* Allow free interchange of TInt and TEnum *)
      TInt (IInt, _), TEnum _ -> ()
    | TEnum _, TInt (IInt, _) -> ()

    | _, _ -> ignore (warn "Type mismatch:@!    %a@!and %a@!" 
                        d_type t1 d_type t2)

and checkCompInfo comp = 
  (* Check if we have seen it already *)
  let fullname = compFullName comp in
  try
    let oldci = H.find compInfoIdEnv comp.ckey in
    if oldci != comp then
      ignore (warn "Compinfo for %s is not shared" fullname)
  with Not_found -> begin
    (* Check that the name is not empty *)
    if comp.cname = "" then 
      E.s (bug "Compinfo with empty name");
    (* Check that the name is unique *)
    if H.mem compInfoNameEnv fullname then
      ignore (warn "Duplicate name %s" fullname);
    (* Check that the ckey is correct *)
    if comp.ckey <> H.hash fullname then
      ignore (warn "Invalid ckey for compinfo %s" fullname);
    (* Add it to the map before we go on *)
    H.add compInfoNameEnv fullname ();
    H.add compInfoIdEnv comp.ckey comp;
    let fctx = if comp.cstruct then CTStruct else CTUnion in
    let rec checkField f =
      if not 
          (f.fcomp == comp &&  (* Each field must share the self cell of 
                                * the host *)
           f.fname <> "") then
        ignore (warn "Self pointer not set in field %s of %s" 
                  f.fname fullname);
      checkType f.ftype fctx;
      (* Check the bitfields *)
      (match unrollType f.ftype, f.fbitfield with
      | TInt (ik, a), Some w -> 
          checkAttributes a;
          if w < 0 || w >= bitsSizeOf (TInt(ik, a)) then
            ignore (warn "Wrong width (%d) in bitfield" w)
      | _, Some w -> 
          ignore (E.error "Bitfield on a non integer type\n")
      | _ -> ());
      checkAttributes f.fattr
    in
    List.iter checkField comp.cfields
  end
    

(* Check an lvalue. If isconst then the lvalue appears in a context where 
 * only a compile-time constant can appear. Return the type of the lvalue. 
 * See the typing rule from cil.mli *)
and checkLval (isconst: bool) (lv: lval) : typ = 
  match lv with
    Var vi, off -> 
      checkVariable vi; 
      checkOffset vi.vtype off

  | Mem addr, off -> begin
      if isconst then
        ignore (warn "Memory operation in constant");
      let ta = checkExp false addr in
      match unrollType ta with
        TPtr (t, _) -> checkOffset t off
      | _ -> E.s (bug "Mem on a non-pointer")
  end

(* Check an offset. The basetype is the type of the object referenced by the 
 * base. Return the type of the lvalue constructed from a base value of right 
 * type and the offset. See the typing rules from cil.mli *)
and checkOffset basetyp : offset -> typ = function
    NoOffset -> basetyp
  | Index (ei, o) -> 
      checkExpType false ei intType; 
      begin
        match unrollType basetyp with
          TArray (t, _, _) -> checkOffset t o
        | t -> E.s (bug "typeOffset: Index on a non-array: %a" d_plaintype t)
      end

  | Field (fi, o) -> 
      (* Make sure we have seen the type of the host *)
      if not (H.mem compInfoIdEnv fi.fcomp.ckey) then
        ignore (warn "The host of field %s is not defined" fi.fname);
      (* Now check that the host is shared propertly *)
      checkCompInfo fi.fcomp;
      (* Check that this exact field is part of the host *)
      if not (List.exists (fun f -> f == fi) fi.fcomp.cfields) then
        ignore (warn "Field %s not part of %s" 
                  fi.fname (compFullName fi.fcomp));
      checkOffset fi.ftype o
        
and checkExpType (isconst: bool) (e: exp) (t: typ) =
  let t' = checkExp isconst e in (* compute the type *)
  if isconst then begin (* For initializers allow a string to initialize an 
                         * array of characters  *)
    if typeSig t' <> typeSig t then 
      match e, t with
      | _ -> typeMatch t' t
  end else
    typeMatch t' t

(* Check an expression. isconst specifies if the expression occurs in a 
 * context where only a compile-time constant can occur. Return the computed 
 * type of the expression *)
and checkExp (isconst: bool) (e: exp) : typ = 
  E.withContext 
    (fun _ -> dprintf "check%s: %a" 
        (if isconst then "Const" else "Exp") d_exp e)
    (fun _ ->
      match e with
      | Const(CInt64 (_, ik, _)) -> TInt(ik, [])
      | Const(CChr _) -> charType
      | Const(CStr _) -> charPtrType 
      | Const(CReal (_, fk, _)) -> TFloat(fk, [])
      | Lval(lv) -> 
          if isconst then
            ignore (warn "Lval in constant");
          checkLval isconst lv

      | SizeOf(t) -> begin
          (* Sizeof cannot be applied to certain types *)
          checkType t CTSizeof;
          (match unrollType t with
            (TFun _ | TVoid _) -> 
              ignore (warn "Invalid operand for sizeof")
          | _ ->());
          uintType
      end
      | SizeOfE(e) ->
          (* The expression in a sizeof can be anything *)
          let te = checkExp false e in
          checkExp isconst (SizeOf(te))

      | AlignOf(t) -> begin
          (* Sizeof cannot be applied to certain types *)
          checkType t CTSizeof;
          (match unrollType t with
            (TFun _ | TVoid _) -> 
              ignore (warn "Invalid operand for sizeof")
          | _ ->());
          uintType
      end
      | AlignOfE(e) ->
          (* The expression in an AlignOfE can be anything *)
          let te = checkExp false e in
          checkExp isconst (AlignOf(te))

      | UnOp (Neg, e, tres) -> 
          checkArithmeticType tres; checkExpType isconst e tres; tres

      | UnOp (BNot, e, tres) -> 
          checkIntegralType tres; checkExpType isconst e tres; tres

      | UnOp (LNot, e, tres) -> 
          let te = checkExp isconst e in
          checkBooleanType te;
          checkIntegralType tres; (* Must check that t is well-formed *)
          typeMatch tres intType;
          tres

      | BinOp (bop, e1, e2, tres) -> begin
          let t1 = checkExp isconst e1 in
          let t2 = checkExp isconst e2 in
          match bop with
            (Mult | Div) -> 
              typeMatch t1 t2; checkArithmeticType tres; 
              typeMatch t1 tres; tres
          | (Eq|Ne|Lt|Le|Ge|Gt) -> 
              typeMatch t1 t2; checkArithmeticType t1; 
              typeMatch tres intType; tres
          | Mod|BAnd|BOr|BXor -> 
              typeMatch t1 t2; checkIntegralType tres;
              typeMatch t1 tres; tres
          | Shiftlt | Shiftrt -> 
              typeMatch t1 tres; checkIntegralType t1; 
              checkIntegralType t2; tres
          | (PlusA | MinusA) -> 
                typeMatch t1 t2; typeMatch t1 tres;
                checkArithmeticType tres; tres
          | (PlusPI | MinusPI | IndexPI) -> 
              checkPointerType tres;
              typeMatch t1 tres;
              checkIntegralType t2;
              tres
          | (MinusPP | EqP | NeP | LtP | LeP | GeP | GtP)  -> 
              checkPointerType t1; checkPointerType t2;
              typeMatch t1 t2;
              typeMatch tres intType;
              tres
      end
      | Question (eb, et, ef) -> 
          if not isconst then
            ignore (warn "Question operator not in a constant\n");
          let tb = checkExp isconst eb in
          checkBooleanType tb;
          let tt = checkExp isconst et in
          let tf = checkExp isconst ef in
          typeMatch tt tf;
          tt

      | AddrOf (lv) -> begin
          let tlv = checkLval isconst lv in
          (* Only certain types can be in AddrOf *)
          match unrollType tlv with
          | TVoid _ -> 
              E.s (bug "AddrOf on improper type");
              
          | (TInt _ | TFloat _ | TPtr _ | TComp _ | TFun _ | TArray _ ) -> 
              TPtr(tlv, [])

          | TEnum _ -> intPtrType
          | _ -> E.s (bug "AddrOf on unknown type")
      end

      | StartOf lv -> begin
          let tlv = checkLval isconst lv in
          match unrollType tlv with
            TArray (t,_, _) -> TPtr(t, [])
          | _ -> E.s (bug "StartOf on a non-array")
      end
            
      | CastE (tres, e) -> begin
          let et = checkExp isconst e in
          checkType tres CTExp;
          (* Not all types can be cast *)
          match unrollType et with
            TArray _ -> E.s (bug "Cast of an array type")
          | TFun _ -> E.s (bug "Cast of a function type")
          | TComp _ -> E.s (bug "Cast of a composite type")
          | TVoid _ -> E.s (bug "Cast of a void type")
          | _ -> tres
      end)
    () (* The argument of withContext *)

and checkInit  (i: init) : typ = 
  E.withContext 
    (fun _ -> dprintf "checkInit: %a" d_init i)
    (fun _ ->
      match i with
        SingleInit e -> checkExp true e
      | CompoundInit (t, initl) -> 
          checkType t CTSizeof;
          let checkOneInit (oo: offset) (ei: init) (et: typ) _ : unit = 
            let ot = checkOffset t oo in
            checkInitType ei ot
          in
          (* foldLeftCompound will check that t is a TComp or a TArray *)
          foldLeftCompound checkOneInit t initl ();
          t)
    () (* The arguments of withContext *)


and checkInitType (i: init) (t: typ) : unit = 
  let it = checkInit i in
  typeMatch it t
  
and checkStmt (s: stmt) = 
  E.withContext 
    (fun _ -> 
      (* Print context only for certain small statements *)
      match s.skind with 
        Loop _ | If _ | Switch _  -> nil
      | _ -> dprintf "checkStmt: %a" d_stmt s)
    (fun _ -> 
      (* Check the labels *)
      let checkLabel = function
          Label (ln, l) -> 
            if H.mem labels ln then
              ignore (warn "Multiply defined label %s" ln);
            H.add labels ln ()
        | Case (e, _) -> checkExpType true e intType
        | _ -> () (* Not yet implemented *)
      in
      List.iter checkLabel s.labels;
      match s.skind with
        Break _ | Continue _ -> ()
      | Goto (gref, l) -> 
          currentLoc := l;
          if not (List.exists (function Label _ -> true | _ -> false) 
                    !gref.labels) then
            ignore (warn "Goto to block without a label\n")

      | Return (re,l) -> begin
          currentLoc := l;
          match re, !currentReturnType with
            None, TVoid _  -> ()
          | _, TVoid _ -> ignore (warn "Invalid return value")
          | None, _ -> ignore (warn "Invalid return value")
          | Some re', rt' -> checkExpType false re' rt'
        end
      | Loop (b, l) -> checkBlock b
      | Block b -> checkBlock b
      | If (e, bt, bf, l) -> 
          currentLoc := l;
          let te = checkExp false e in
          checkBooleanType te;
          checkBlock bt;
          checkBlock bf
      | Switch (e, b, cases, l) -> 
          currentLoc := l;
          (* Do not check cases for now *)
          checkExpType false e intType;
          checkBlock b
            
      | Instr il -> List.iter checkInstr il)
    () (* argument of withContext *)

and checkBlock (b: block) : unit = 
  List.iter checkStmt b.bstmts


and checkInstr (i: instr) = 
  match i with 
  | Set (dest, e, l) -> 
      currentLoc := l;
      let t = checkLval false dest in
      (* Not all types can be assigned to *)
      (match unrollType t with
        TFun _ -> ignore (warn "Assignment to a function type")
      | TArray _ -> ignore (warn "Assignment to an array type")
      | TVoid _ -> ignore (warn "Assignment to a void type")
      | _ -> ());
      checkExpType false e t
            
  | Call(dest, what, args, l) -> 
      currentLoc := l;
      let (rt, formals, isva) = 
        match checkExp false what with
          TFun(rt, formals, isva, _) -> rt, formals, isva
        | _ -> E.s (bug "Call to a non-function")
      in
          (* Now check the return value*)
      (match dest, unrollType rt with
        None, TVoid _ -> ()
      | Some _, TVoid _ -> ignore (warn "Call of subroutine is assigned")
      | None, _ -> () (* "Call of function is not assigned" *)
      | Some destlv, rt' -> 
          let desttyp = checkLval false destlv in
          if typeSig desttyp <> typeSig rt then
                  (* Not all types can be cast *)
              (match rt' with
                TArray _ -> ignore (warn "Cast of an array type")
              | TFun _ -> ignore (warn "Cast of a function type")
              | TComp _ -> ignore (warn "Cast of a composite type")
              | TVoid _ -> ignore (warn "Cast of a void type")

              | _ -> ()));
          (* Now check the arguments *)
      let rec loopArgs formals args = 
        match formals, args with
          [], _ when (isva || args = []) -> ()
        | fo :: formals, a :: args -> 
            checkExpType false a fo.vtype;
            loopArgs formals args
        | _, _ -> ignore (warn "Not enough arguments")
      in
      loopArgs formals args
        
  | Asm _ -> ()  (* Not yet implemented *)
  
let rec checkGlobal = function
    GAsm _ -> ()
  | GPragma _ -> ()
  | GText _ -> ()
  | GType (n, t, l) -> 
      currentLoc := l;
      E.withContext (fun _ -> dprintf "GType(%s)" n)
        (fun _ ->
          checkType t CTDecl;
          if n <> "" then begin
            if H.mem typeDefs n then
              E.s (bug "Type %s is multiply defined" n);
            defineName n;
            H.add typeDefs n t
          end else begin
            match unrollType t with
              TComp _ -> ()
            | _ -> E.s (bug "Empty type name for type %a" d_type t)
          end)
        ()

  | GCompTag (comp, l) -> 
      currentLoc := l;
      checkCompInfo comp;
      (* Mark it as a definition. We'll use this later to check the forwards *)
      if H.mem compDefined comp.ckey then 
        ignore (E.log "%s is multiply defined\n" (compFullName comp));
      H.add compDefined comp.ckey comp;

  | GEnumTag (enum, l) -> 
      currentLoc := l;
      if enum.ename = "" then
        E.s (bug "Enum with empty tag");
      if H.mem enumDefined enum.ename then 
        ignore (E.log "enum %s is multiply defined\n" enum.ename);
      (* Add it to the enumTags *)
      List.iter (fun (tn, _) -> defineName tn) enum.eitems

  | GDecl (vi, l) -> 
      currentLoc := l;
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
              E.s (bug "Invalid declaration of %s" vi.vname)
          end)
        ()
        
  | GVar (vi, init, l) -> 
      currentLoc := l;
      (* Maybe this is the first occurrence *)
      E.withContext (fun _ -> dprintf "GVar(%s)" vi.vname)
        (fun _ -> 
          checkGlobal (GDecl (vi, l));
          (* Check the initializer *)
          begin match init with
            None -> ()
          | Some i -> ignore (checkInitType i vi.vtype)
          end;
          (* Cannot be a function *)
          if isFunctionType vi.vtype then
            E.s (bug "GVar for a function (%s)\n" vi.vname);
          )
        ()
        

  | GFun (fd, l) -> begin
      currentLoc := l;
      (* Check if this is the first occurrence *)
      let vi = fd.svar in
      let fname = vi.vname in
      E.withContext (fun _ -> dprintf "GFun(%s)" fname)
        (fun _ -> 
          checkGlobal (GDecl (vi, l));
          (* Check that the argument types in the type are identical to the 
           * formals *)
          let rec loopArgs targs formals = 
            match targs, formals with
              [], [] -> ()
            | ta :: targs, fo :: formals -> 
                if ta != fo then 
                  ignore (warn "Formal %s not shared (type + locals) in %s" 
                         fo.vname fname);
                loopArgs targs formals

            | _ -> 
                E.s (bug "Type has different number of formals for %s" 
                       fname)
          in
          begin match vi.vtype with
            TFun (rt, args, isva, a) -> begin
              currentReturnType := rt;
              loopArgs args fd.sformals
            end
          | _ -> E.s (bug "Function %s does not have a function type" 
                        fname)
          end;
          ignore (fd.smaxid >= 0 || E.s (bug "smaxid < 0 for %s" fname));
          (* Now start a new environment, in a finally clause *)
          begin try
            startEnv ();
            (* Do the locals *)
            let doLocal tctx v = 
              if not 
                  (v.vid >= 0 && v.vid <= fd.smaxid && not v.vglob &&
                   v.vstorage <> Extern) then
                E.s (bug "Invalid local %s in %s" v.vname fname);
              checkType v.vtype tctx;
              checkAttributes v.vattr;
              defineVariable v
            in
            List.iter (doLocal CTFArg) fd.sformals;
            List.iter (doLocal CTDecl) fd.slocals;
            checkBlock fd.sbody;
            H.clear labels;
            (* Done *)
            endEnv ()
          with e -> 
            endEnv ();
            raise e
          end;
          ())
        () (* final argument of withContext *)
  end


let checkFile flags fl = 
  if !E.verboseFlag then ignore (E.log "Checking file %s\n" fl.fileName);
  List.iter 
    (function
        NoCheckGlobalIds -> checkGlobalIds := false)
    flags;
  iterGlobals fl (fun g -> try checkGlobal g with _ -> ());
  (* Check that for all struct/union tags there is a definition *)
  (try
    H.iter 
      (fun k comp -> 
        try
          let cdef = H.find compDefined k in
          if cdef != comp then 
            ignore (warn "Compinfo for %s not shared (forwards)"
                      (compFullName comp))
        with Not_found -> 
          ignore (warn "Compinfo %s is referenced but not defined" 
                    (compFullName comp))) 
      compForwards
  with _ -> ());
  (* Check that for all enum tags there is a definition *)
  (try
    H.iter 
      (fun k enum -> 
        try
          let edef = H.find enumDefined k in
          if edef != enum then 
            ignore (warn "Enuminfo for %s not shared (forwards)" k)
        with Not_found -> 
          ignore (warn "Enuminfo %s is referenced but not defined" k))
      enumForwards
  with _ -> ());
  (* Clean the hashes to let the GC do its job *)
  H.clear typeDefs;
  H.clear varNamesEnv;
  H.clear varIdsEnv;
  H.clear compInfoNameEnv;
  H.clear compInfoIdEnv;
  H.clear compForwards;
  H.clear compDefined;
  H.clear enumForwards;
  H.clear enumDefined;
  varNamesList := [];
  if !E.verboseFlag then 
    ignore (E.log "Finished checking file %s\n" fl.fileName);
  ()
  
