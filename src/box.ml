open Cil
open Pretty

module H = Hashtbl

let debugType = false
let debug = false

let checkReturn = true
           (* After processing an expression, we create its type, a list of 
            * instructions that should be executed before this exp is used, 
            * and a replacement exp *)
type expRes = 
    typ * stmt list * exp

          (* When we create fat expressions we want to postpone some 
           * operations because they might involve creation of temporaries 
           * and statements. In some situations (e.g. cast from constant to 
           * fat pointer and then back to lean pointer, or initialization of 
           * a fat constant) we want to have as few statements as possible *)
and fexp = 
    L of typ * exp                      (* A lean expression of a given type *)
  | F1 of typ * exp                     (* A fat expression that is already 
                                         * converted to an expression *)
  | F2 of typ * exp * exp               (* A fat expression of a given (fat) 
                                         * type made out of two lean 
                                         * expression (the pointer and the 
                                         * base)  *)
  | FC of typ * typ * exp               (* A fat expression that is a cast to 
                                         * a fat type from another fat type. 
                                         * The inner expression is an F1 
                                         * whose type and value are given *)



            (* Same for offsets *)
type offsetRes = 
    typ * stmt list * offset


(*** Helpers *)            

let prefix p s = 
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p



  (* We collect here the new file *)
let theFile : global list ref = ref []
(**** Make new types ****)

   (* Keep some global counters for anonymous types *)
let anonTypeId = ref 0
let makeNewTypeName base = 
  incr anonTypeId;
  base ^ (string_of_int !anonTypeId)

   (* Make a type name, for use in type defs *)
let rec typeName = function
    TForward n -> typeName (resolveForwardType n)
  | TNamed (n, _, _) -> n
  | TVoid(_) -> "void"
  | TInt(IInt,_) -> "int"
  | TInt(IUInt,_) -> "uint"
  | TInt(IShort,_) -> "short"
  | TInt(IUShort,_) -> "ushort"
  | TInt(IChar,_) -> "char"
  | TInt(IUChar,_) -> "uchar"
  | TInt(ISChar,_) -> "schar"
  | TInt(ILong,_) -> "long"
  | TInt(IULong,_) -> "ulong"
  | TInt(ILongLong,_) -> "llong"
  | TInt(IULongLong,_) -> "ullong"
  | TFloat(FFloat,_) -> "float"
  | TFloat(FDouble,_) -> "double"
  | TFloat(FLongDouble,_) -> "ldouble"
  | TEnum (n, _, _) -> 
      if String.sub n 0 1 = "@" then makeNewTypeName "enum"
      else "enum_" ^ n
  | TStruct (n, _, _) -> 
      if String.sub n 0 1 = "@" then makeNewTypeName "struct"
      else "struct_" ^ n
  | TUnion (n, _, _) -> 
      if String.sub n 0 1 = "@" then makeNewTypeName "union"
      else "union_" ^ n
  | TFun _ -> makeNewTypeName "fun"
  | _ -> makeNewTypeName "type"



(**** FIXUP TYPE ***)
let fixedTypes : (typsig, typ) H.t = H.create 17

    (* For each fat pointer name, keep track of various versions, usually due 
     * to varying attributes *)
let fatPointerNames : (string, int ref) H.t = H.create 17
let newFatPointerName t = 
  let n = "fatp_" ^ (typeName t) in
  try
    let r = H.find fatPointerNames n in
    incr r;
    n ^ (string_of_int !r)
  with Not_found -> 
    H.add fatPointerNames n (ref 0);
    n


(***** Convert all pointers in types for fat pointers ************)
let rec fixupType t = 
  match t with
    TForward n -> t
  | TNamed (n, t, a) -> TNamed(n, fixupType t, a) (* Keep the Named types *)
                                        (* We leave alone vararg functions 
                                         * mainly because it is hard to write 
                                         * a wrapper for them. We'll check 
                                         * that we only pass lean pointers to 
                                         * them *)
  | TFun (_, _, true, _) -> t
    (* Sometimes we find a function type without arguments (a prototype that 
     * we have done before). Do the regular fixit and then put the argument 
     * names back.  *)
  | TFun (_, args, _, _) -> begin
      match fixit t with
        TFun (rt, args', isva, a) -> 
          TFun(rt,
               List.map2 (fun a a' -> {a' with vname = a.vname}) args args',
               isva, a)
      | _ -> E.s (E.bug "1")
  end
  | _ -> fixit t

and fixit t = 
  let ts = typeSig t in
  try
    H.find fixedTypes ts 
  with Not_found -> begin
    let doit t =
      match t with 
        (TInt _|TEnum _|TFloat _|TVoid _|TBitfield _) -> t
(*
      | TPtr ((TFun _) as t', a) -> 
          TPtr (fixupType t', a)         Pointers to functions are lean *)

      | TPtr (t', a) -> begin
        (* Now do the base type *)
          let fixed' = fixupType t' in
          let tname  = newFatPointerName fixed' in (* The name *)
          let fixed = 
            TStruct(tname, [ { fstruct = tname;
                               fname   = "_p";
                               ftype   = TPtr(fixed', a);
                               fattr   = [];
                             };
                             { fstruct = tname;
                               fname   = "_b";
                               ftype   = TPtr(TVoid ([]), []);
                               fattr   = [];
                             }; ], []) 
          in
          let tres = TNamed(tname, fixed, []) in
          H.add fixedTypes (typeSig fixed) fixed; (* We add fixed ourselves. 
                                                   * The TNamed will be added 
                                                   * after doit  *)
          H.add fixedTypes (typeSig (TPtr(fixed',a))) fixed;
          theFile := GType(tname, fixed) :: !theFile;
          tres
      end
            
      | TForward _ ->  t              (* Don't follow TForward *)
      | TNamed (n, t', a) -> TNamed (n, fixupType t', a)
            
      | TStruct(n, flds, a) -> begin
          let r = 
            TStruct(n, 
                    List.map 
                      (fun fi -> 
                        {fi with ftype = fixupType fi.ftype}) flds,
                    a) in
          replaceForwardType ("struct " ^ n) r;
          r
      end
      | TUnion (n, flds, a) -> 
          let r = 
            TUnion(n, 
                   List.map (fun fi -> 
                     {fi with ftype = fixupType fi.ftype}) flds,
                   a) in
          replaceForwardType ("union " ^ n) r;
          r
            
      | TArray(t', l, a) -> TArray(fixupType t', l, a)
            
      | TFun(rt,args,isva,a) ->
          let args' = 
            List.map
              (fun argvi -> {argvi with vtype = fixupType argvi.vtype}) args in
          let res = TFun(fixupType rt, args', isva, a) in
          res
    in
    let fixed = doit t in
    H.add fixedTypes ts fixed;
    H.add fixedTypes (typeSig fixed) fixed;
    fixed
  end

(** Create some fat types *)
let _ = fixupType (voidPtrType) 
let _ = fixupType (charPtrType)
let _ = fixupType (TPtr(TInt(IChar, [AId("const")]), []))
let _ = theFile := []   (* Remove them from the file *)

(**** We know in what function we are ****)
let currentFunction : fundec ref  = ref dummyFunDec

    (* Test if a type is FAT *)
let isFatType t = 
  match unrollType t with
    TStruct(_, [p;b],_) when p.fname = "_p" && b.fname = "_b" -> true
  | _ -> false

let getPtrFieldOfFat t = 
  match unrollType t with
    TStruct(_, [p;b],_) when p.fname = "_p" && b.fname = "_b" -> p
  | _ -> E.s (E.bug "getPtrFieldOfFat %a\n" d_type t)

let getBaseFieldOfFat t = 
  match unrollType t with
    TStruct(_, [p;b],_) when p.fname = "_p" && b.fname = "_b" -> b
  | _ -> E.s (E.bug "getBaseFieldOfFat %a\n" d_type t)


let rec readPtrBaseField e et =     
  if isFatType et then
    let fptr  = getPtrFieldOfFat et in
    let fbase = getBaseFieldOfFat et in
    let rec compOffsets = function
        NoOffset -> Field(fptr, NoOffset), Field(fbase, NoOffset)
      | Field(fi, o) -> 
          let po, bo = compOffsets o in
          Field(fi, po), Field(fi, bo)
      | Index(e, o) ->
          let po, bo = compOffsets o in
          Index(e, po), Index(e, bo)
      | First o -> 
          let po, bo = compOffsets o in
          First po, First bo
    in
    let ptre, basee = 
      match e with
        Lval(Var vi, o) -> 
          let po, bo = compOffsets o in
          Lval(Var vi, po), Lval(Var vi, bo)
      | Lval(Mem e'', o) -> 
          let po, bo = compOffsets o in
          Lval(Mem e'', po), Lval(Mem e'', bo)
      | Question (e1, e2, e3, lq) ->
          let e2t, e2', e2'' = readPtrBaseField e2 et in
          let   _, e3', e3'' = readPtrBaseField e3 et in
          (Question(e1,e2',e3', lq), Question(e1,e2'',e3'', lq))
      | _ -> E.s (E.unimp "split _p field offset: %a" d_plainexp e)
    in
    (fptr.ftype, ptre, basee)
  else
    (et, e, e)

    (* Create a new temporary of a fat type and set its pointer and base 
     * fields *)
let setFatPointer (t: typ) (p: typ -> exp) (b: exp) : stmt list * lval = 
  let tmp = makeTempVar !currentFunction t in
  let fptr = getPtrFieldOfFat t in
  let fbase = getBaseFieldOfFat t in
  let p' = p fptr.ftype in
  ([ mkSet (Var tmp, Field(fptr,NoOffset)) p';
     mkSet (Var tmp, Field(fbase,NoOffset)) b ], 
     (Var tmp, NoOffset))
      
let readPtrField e t = 
  let (tptr, ptr, base) = readPtrBaseField e t in ptr
      
let readBaseField e t = 
  let (tptr, ptr, base) = readPtrBaseField e t in base

let fromPtrToBase e = 
  let rec replacePtrBase = function
      Field(fip, NoOffset) when fip.fname = "_p" -> begin
        (* Find the fat type that this belongs to *)
        try
          let fat = 
            H.find fixedTypes (typeSig (TForward ("struct " ^ fip.fstruct))) in
          let bfield = getBaseFieldOfFat fat in
          Field(bfield, NoOffset)
        with Not_found -> 
          E.s (E.unimp "Field %s is not a component of a fat type" fip.fname)
      end
    | Field(f', o) -> Field(f',replacePtrBase o)
    | _ -> E.s (E.unimp "Cannot find the _p field to replace in %a\n"
                  d_plainexp e)
  in
  match e with
    Lval (b, off) -> Lval(b, replacePtrBase off)
  | _ -> E.s (E.unimp "replacing _p with _b in a non-lval")
  
   (* Test if we must check the return value *)
let mustCheckReturn tret =
  checkReturn &&
  match unrollType tret with
    TPtr _ -> true
  | TArray _ -> true
  | _ -> isFatType tret


   (* Test if we have changed the type *)
let rec typeHasChanged t =
  match unrollType t with
    TStruct(_, [p;b],_) when p.fname = "_p" && b.fname = "_b" -> true
  | TStruct(_, flds, _) -> List.exists (fun f -> typeHasChanged f.ftype) flds
  | TUnion(_, flds, _) -> List.exists (fun f -> typeHasChanged f.ftype) flds
  | TArray(t, _, _) -> typeHasChanged t
  | TFun(rt, args, _, _) -> begin
      typeHasChanged rt ||
      List.exists (fun a -> typeHasChanged a.vtype) args
  end
  | TPtr (t, _) -> typeHasChanged t
  | _ -> false


(* Create tags for types along with the newly created fields and initializers 
 * for tags and for the length  *)
(* Check whether the type contains an embedded array *)
let mustBeTagged v = 
  let rec containsArray t = 
    match unrollType t with 
      TArray _ -> true
    | TStruct(_, flds, _) -> 
        List.exists (fun f -> containsArray f.ftype) flds
    | TPtr _ -> false
    | (TInt _ | TEnum _ | TFloat _ | TBitfield _ ) -> false
    | _ -> E.s (E.unimp "containsArray: %a" d_plaintype t)
  in
  if v.vglob then 
    match v.vtype with 
      TFun _ -> false 
    | TArray(_, None, _) -> false (* I don't know what to do with these *)
    | _ -> true
  else v.vaddrof || containsArray v.vtype
    
    
(* We need to avoid generating multiple copies of the same tagged type 
 * because we run into trouble if a variable is defined twice (once with 
 * extern). *)
let taggedTypes: (typsig, typ) H.t = H.create 123

let tagType (t: typ) : (typ * fieldinfo * fieldinfo * fieldinfo * 
                          (offset * exp) list * exp) = 
  let bytes = intSizeOf t in   (* Maybe we should not compute sizeof here *)
  let words = (bytes + 3) lsr 2 in
  let tagwords = (words + 15) lsr 4 in
  let newtype = 
    let tsig = typeSig t in
    try
      H.find taggedTypes tsig
    with Not_found -> 
      let newt = 
        mkCompType true ""
          [ ("_tags", TArray(intType, 
                             Some (integer tagwords), []));
            ("_len", intType);
            ("_data", t) ] [] in
      let tname = "_tagged_" ^ typeName t in
      let named = TNamed (tname, newt, []) in
      theFile := GType (tname, newt) :: !theFile;
      H.add taggedTypes tsig named;
      named
  in
  let tfld, lfld, dfld = 
    match unrollType newtype with
      TStruct (_, [tfld; lfld; dfld], _) -> tfld, lfld, dfld
    | _ -> E.s (E.bug "tagLocal")
  in
            (* Now create the initializer *)
  let rec loopTags idx = 
    if idx >= tagwords then [] else
    ((Field(tfld, First (Index (integer idx, NoOffset)))), zero) :: 
    loopTags (idx + 1)
  in
  newtype, tfld, lfld, dfld, (loopTags 0), (integer words)
 

(****** the CHECKERS ****)
let fatVoidPtr = fixupType voidPtrType
let checkSafeRetFat = 
  let fdec = emptyFunction "CHECK_SAFERETFAT" in
  let arg  = makeLocalVar fdec "x" fatVoidPtr in
  fdec.svar.vtype <- TFun(voidType, [ arg ], false, []);
  fdec
    
let checkSafeFatLeanCast = 
  let fdec = emptyFunction "CHECK_SAFEFATLEANCAST" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec

let doCheckFat which arg argt = 
  (* Take the argument and break it apart *)
  let (_, ptr, base) = readPtrBaseField arg argt in 
  call None (Lval(var which.svar)) [ CastE(voidPtrType, ptr, lu); 
                                     CastE(voidPtrType, base, lu) ]

let doCheckLean which arg  = 
  call None (Lval(var which.svar)) [ CastE(voidPtrType, arg, lu)]


(* Make a bunch of read safety checks *)
let checkSafeMemComp = 
  let mkCheck name words =
    let fdec = emptyFunction (name ^ (string_of_int words)) in
    let p  = makeLocalVar fdec "p" voidPtrType in
    let b  = makeLocalVar fdec "b" voidPtrType in
    let len  = makeLocalVar fdec "len" intType in
    let rec loop idx = 
      if idx >= words then [] 
      else (makeLocalVar fdec "t" uintType) ::
           (makeLocalVar fdec "t" uintType) :: loop (idx + 1) 
    in
    fdec.svar.vtype <- TFun(voidType, p :: b :: len :: loop 0, false, []);
    fdec
  in
  let readChecks = [| mkCheck "CHECK_SAFEREADCOMP" 1; 
                      mkCheck "CHECK_SAFEREADCOMP" 2; |] in
  let writeChecks = [| mkCheck "CHECK_SAFEWRITECOMP" 1; 
                       mkCheck "CHECK_SAFEWRITECOMP" 2; |] in
  fun iswrite words -> 
    if words > 2 || words < 1 then
      E.s (E.unimp "checkSafeReadComp not available for %d words\n" words)
    else
      if iswrite then
        writeChecks.(words - 1)
      else
        readChecks.(words - 1)
          
let checkSafeReadLean = 
  let fdec = emptyFunction "CHECK_SAFEREADLEAN" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec
  
let checkSafeReadFun = 
  let fdec = emptyFunction "CHECK_SAFEREADFUN" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec
  
let checkSafeWriteLean = 
  let fdec = emptyFunction "CHECK_SAFEWRITELEAN" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec
  
let checkWritePointer = 
  let fdec = emptyFunction "CHECK_SAFEWRITEPOINTER" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec
  

 (* Given a type compute a list of tags for its components *)

let bitsForType iswrite t = 
  (* Mask and value for the various kinds of data *)
  let bLean = (if iswrite then 3 else 0), 0 in
  let bPtr  = 3, 2 in
  let bBase = 3, 1 in (* Leave this with 1 in LSB so we can recognize it 
                         easily *)
  let rec pushBit ((maskb, valueb) as b)
                  (idx,currentmask,currentvalue,acc) = 
    let newmask = currentmask lor (maskb lsl idx) in
    let newvalue = currentvalue lor (valueb lsl idx) in
    if idx = 28 then
      (0, 0, 0, newvalue :: newmask :: acc)
    else
      (idx + 2, newmask, newvalue, acc)
  in
  let rec tagOfType acc = function
    | (TInt _ | TFloat _ | TEnum _) as t -> 
        let sz = intSizeOf t in
        if      sz <= 4 then pushBit bLean acc
        else if sz <= 8 then pushBit bLean (pushBit bLean acc)
        else E.s (E.unimp "TagOfType: large scalar")
    | TStruct (_, [p;b], _) when p.fname = "_p" && b.fname = "_b" -> 
        pushBit bBase (pushBit bPtr acc)
    | TStruct (_, flds, _) -> 
        List.fold_left (fun acc f -> tagOfType acc f.ftype) acc flds
    | TNamed (_, t, _) -> tagOfType acc t
    | t -> E.s (E.unimp "tagOfType: %a" d_plaintype t)
  in
  let finishAcc (idx, currentmask, currentvalue, acc) = 
(*    ignore (E.log "Finish for %a@! idx=%d, current=%d@!" 
              d_plaintype t idx current); *)
    ((15 * (List.length acc) + idx) lsr 1,
     List.rev (if idx = 0 then acc else currentvalue :: currentmask :: acc))
  in
  finishAcc (tagOfType (0,0,0,[]) t)

    (* Check a read *)
let checkMem (towrite: exp option) 
             (lv: lval) (base: exp) (t: typ) : stmt list = 
  let iswrite, towrite = 
    match towrite with
      None -> false, zero
    | Some e -> true, e
  in
  match unrollType t with 
    TFun _ when not iswrite -> 
        [call None (Lval(var checkSafeReadFun.svar))
            (CastE(voidPtrType, AddrOf(lv, lu), lu) :: base :: [])]
  | _ ->
      (* For a write also verify that we do not write stack pointers to the 
       * heap *)
      let checkwrite = 
        if iswrite then
          let theCheck = Lval(var checkWritePointer.svar) in
          let rec doCheckWrite (what: exp) (t: typ) acc = 
            match unrollType t with 
            | (TInt _ | TFloat _ | TEnum _ ) -> acc
            | TStruct(_, [p;b], _) when p.fname = "_p" && b.fname = "_b" -> 
                let (_, p, b) = readPtrBaseField what t in
                (call None theCheck [ p; b]) :: acc
            | TStruct (_, flds, _) -> 
                let lv = 
                  match what with 
                    Lval x -> x
                  | _ -> E.s (E.unimp "checkwrite on a non-lval")
                in
                let doField acc f = 
                  doCheckWrite (Lval(addOffset (Field(f, NoOffset)) lv))
                               f.ftype acc
                in
                List.fold_left doField acc flds
            | _ -> E.s (E.unimp "unexpected type in checkwrite")
          in
          doCheckWrite towrite t []
        else [] 
      in
      let (nrWords, tags) = bitsForType iswrite t in
      let tlen = (List.length tags) lsr 1 in
      if nrWords = 1 then (* A lean memory operation *)
        let theCheck = 
          if iswrite then checkSafeWriteLean else checkSafeReadLean in
        (call None (Lval(var theCheck.svar))
            (CastE(voidPtrType, AddrOf(lv, lu), lu) :: base :: [])) ::
        checkwrite
      else
        let theCheck = checkSafeMemComp iswrite tlen in
        (call None (Lval(var theCheck.svar)) 
            (CastE(voidPtrType, AddrOf(lv, lu), lu) :: base ::
             (integer nrWords) :: (List.map hexinteger tags))) ::
        checkwrite
          
    (* Check a write *)
let checkRead = checkMem None
let checkWrite e = checkMem (Some e)

(* A major hack for MSVC *)
let getIOBFunction = 
  let fdec = emptyFunction "__get_iob_fp" in
  let argn = makeLocalVar fdec "n" intType in
  fdec.svar.vtype <- TFun(fatVoidPtr, [ argn ], false, []);
  fdec


(* Check if an offset contains a non-zero index *)
let rec containsIndex = function
    NoOffset -> false
  | First o -> containsIndex o
  | Field (_, o) -> containsIndex o
  | Index (Const(CInt(0, _, _), _), o) -> containsIndex o
  | Index _ -> true

    (************* STATEMENTS **************)
let rec boxstmt (s : stmt) : stmt = 
  try
    match s with 
      Sequence sl -> mkSeq (List.map boxstmt sl)
          
    | (Label _ | Goto _ | Case _ | Default | Skip | 
      Return None | Break | Continue) -> s
          
    | Loop s -> Loop (boxstmt s)
          
    | IfThenElse (e, st, sf) -> 
        let (_, doe, e') = boxexp (CastE(intType, e, lu)) in
          (* We allow casts from pointers to integers here *)
        mkSeq (doe @ [IfThenElse (e', boxstmt st, boxstmt sf)])
          
    | Switch (e, s) -> 
        let (_, doe, e') = boxexp (CastE(intType, e, lu)) in
        mkSeq (doe @ [Switch (e', boxstmt s)])

    | Return (Some e) -> 
        let retType = (* Already fixed *)
          match !currentFunction.svar.vtype with 
            TFun(tRes, _, _, _) -> tRes
          | _ -> E.s (E.bug "Current function's type is not TFun")
        in 
        let (et, doe, e') = boxexp e in
        let doe' = (* Add the check *)
          if mustCheckReturn retType then
            if isFatType retType then begin
                (* Cast e' to fat_voidptr *)
              doe @ [doCheckFat checkSafeRetFat e' et]
            end else begin
              doe
            end
          else
            doe
        in
        mkSeq (doe' @ [Return (Some e')])
    | Instr i -> boxinstr i
  with e -> begin
    ignore (E.log "boxstmt (%s)\n" (Printexc.to_string e));
    dStmt (dprintf "booo_statement(%a)" d_stmt s)
  end

  

and boxinstr (ins: instr) : stmt = 
  if debug then
    ignore (E.log "Boxing %a\n" d_instr ins);
  try
    match ins with
    | Set (lv, e, l) -> 
        let (rest, dolv, lv', lvbase) = boxlval lv in
        let (_, doe, e') = boxexp e in
        let check = 
          match lv' with
            Mem _, _ -> checkWrite e' lv' lvbase rest
          | Var _, off when containsIndex off -> checkWrite e' lv' lvbase rest
          | _ -> []
        in
        mkSeq (dolv @ doe @ check @ [Instr(Set(lv', e', l))])

    | Call(vi, f, args, l) ->
        let (ft, dof, f') = boxexp f in
        let (ftret, ftargs, isva) =
          match ft with 
            TFun(fret, fargs, isva, _) -> (fret, fargs, isva) 
          | _ -> E.s (E.unimp "call of a non-function: %a @!: %a" 
                        d_plainexp f' d_plaintype ft) 
        in
        let (doargs, args') =
          if isva then (* We leave vararg functions alone. But we check all 
                        * arguments and, if needed we pack the result *)
            let rec doArgs = function
              | [] -> ([], [])
              | a :: resta -> 
                  let (at, doa, a') = boxexp a in
                  let (doresta, resta') = doArgs resta in
                  let (checka, a'') = 
                    if isFatType at then 
                      ([doCheckFat checkSafeFatLeanCast a' at],
                       readPtrField a' at)
                    else ([], a')
                  in
                  (doa @ checka @ doresta, a'' :: resta')
            in
            doArgs args
          else
            let rec doArgs restargs restargst = (* The types of functions 
                                                 * have 
                                                 * been fixed already  *) 
              match restargs, restargst with
                [], [] -> [], []
              | a :: resta, t :: restt -> 
                  let (doa, fa') = boxexpf a in
                  let (doa', fa'') = castTo fa' t.vtype doa in
                  let (_, doa'', a2) = fexp2exp fa'' doa' in
                  let (doresta, resta') = doArgs resta restt in
                (doa'' @ doresta,  a2 :: resta')
              | _ -> E.s (E.unimp "vararg in call to %a" d_exp f)
            in
            doArgs args ftargs  
        in
        mkSeq (dof @ doargs @ [call vi f' args'])

    | Asm(tmpls, isvol, outputs, inputs, clobs) ->
        let rec doOutputs = function
            [] -> [], []
          | (c, lv) :: rest -> 
              let (lvt, dolv, lv', lvbase) = boxlval lv in
              let check = 
                match lv' with
                  Mem _, _ -> checkWrite (integer 0) lv' lvbase lvt
                | _ -> []
              in
              if isFatType lvt then
                ignore (E.log "Warning: fat output in %a\n"
                          d_instr ins);
              let (doouts, outs) = doOutputs rest in
              (dolv @ check @ doouts, (c, lv') :: outs)
        in
        let (doouts, outputs') = doOutputs outputs in
        let rec doInputs = function
            [] -> [], []
          | (c, ei) :: rest -> 
              let (et, doe, e') = boxexp ei in
              if isFatType et then
                ignore (E.log "Warning: fat input %a in %a\n"
                          d_exp ei d_instr ins);
              let (doins, ins) = doInputs rest in
              (doe @ doins, (c, e') :: ins)
        in
        let (doins, inputs') = doInputs inputs in
        mkSeq (doouts @ doins @ 
               [Instr(Asm(tmpls, isvol, outputs', inputs', clobs))])
            
  with e -> begin
    ignore (E.log "boxinstr (%s)\n" (Printexc.to_string e));
    dStmt (dprintf "booo_instruction(%a)" d_instr ins)
  end


 (* Given an lvalue, generate the type of denoted element, a list of 
  * statements to prepare to replacement lval, the replacement lval, and an 
  * expression that denotes the base of the memory area containing the lval *)
and boxlval (b, off) : (typ * stmt list * lval * exp) = 
  let (b', dob, tbase, baseaddr, off') = 
    match b with
      Var vi -> 
        (* If the address of this variable is taken or if the variable 
         * contains an array, then we must have changed the type into a 
         * structure type, with tags. All globals are also tagged since we 
         * might take their address at any time.  *)
        let offbase, tbase, off' = 
          if mustBeTagged vi then
            let dataf = 
              match unrollType vi.vtype with
                TStruct(_, [_; _; df], _) -> df
              | _ -> E.s (E.bug "addrOf but no tagged type")
            in
            Field(dataf, NoOffset), dataf.ftype, Field(dataf, off)
          else
            NoOffset, vi.vtype, off
        in
        let baseaddr = 
          match vi.vtype with
            TArray _ -> StartOf(Var vi, offbase)
          | _ -> AddrOf((Var vi, offbase), lu)
        in
        (b, [], tbase, baseaddr, off')
    | Mem addr -> 
        let (addrt, doaddr, addr', addr'base) = boxexpSplit addr in
        let addrt' = 
          match addrt with
            TPtr(t, _) -> t
          | _ -> E.s (E.unimp "Reading from a non-pointer type: %a\n"
                        d_plaintype addrt)
        in
        (Mem addr', doaddr, addrt', addr'base, off)
  in
  let (t, dooff, off'') = boxoffset off' tbase in
  (t, dob @ dooff, (b', off''), baseaddr)


and boxoffset (off: offset) (basety: typ) : offsetRes = 
  match off with 
  | NoOffset ->
      (basety, [], NoOffset)

  | Index (e, resto) -> 
      let (_, doe, e') = boxexp e in
      let (rest', doresto, off') = boxoffset resto basety in
      (rest', doe @ doresto, Index(e', off'))

  | Field (fi, resto) ->
      let fi' = {fi with ftype = fixupType fi.ftype} in
      let (rest, doresto, off') = boxoffset resto fi'.ftype in
      (rest, doresto, Field(fi', off'))
  | First o -> 
      let etype = 
        match unrollType basety with
          TArray (x, _, _) -> x
        | _ -> E.s (E.bug "First on a non-array.@!T=%a\n" d_plaintype basety)
      in
      let (rest, doresto, off') = boxoffset o etype in
      (rest, doresto, First off')

    (* Box an expression and return the fexp version of the result. If you do 
     * not care about an fexp, you can call the wrapper boxexp *)
and boxexpf (e: exp) : stmt list * fexp = 
  match e with
  | Lval (lv) -> 
      let rest, dolv, lv', lvbase = boxlval lv in
      let check = (* Check a read if it is in memory of if the offset contains 
                     an Index *)
        match lv' with
          Mem _, _ -> checkRead lv' lvbase rest
        | Var _, off when containsIndex off -> checkRead lv' lvbase rest
        | _, _ -> []
      in
      if isFatType rest then
        (dolv @ check, F1(rest, Lval(lv')))
      else
        (dolv @ check, L(rest, Lval(lv')))

  | Const (CInt (_, ik, _), _) -> ([], L(TInt(ik, []), e))
  | Const ((CChr _), _) -> ([], L(charType, e))
  | Const (CReal (_, fk, _), _) -> ([], L(TFloat(fk, []), e))
  | CastE (t, e, l) -> begin
      let t' = fixupType t in
      let (doe, fe') = boxexpf e in
      (* Put e into a variable *)
      castTo fe' t' doe
  end
        
  | Const (CStr s, cloc) -> 
      (* Make a global variable that stores this one, so that we can attach a 
       * tag to it *)
      let l = 1 + String.length s in 
      let newt, tfld, _, dfld, initags, inilen = 
        tagType (TArray(charType, Some (integer l), [])) in
      let gvar = makeGlobalVar (makeNewTypeName "___string") newt in
      gvar.vstorage <- Static;
      (* Build an initializer *)
      let varinit = 
        Compound (newt, 
                  (* The tags. We could use designators but MSVC does not 
                   * like them *)
                  (None,
                   Compound(tfld.ftype, 
                            List.map (fun (_, itag) -> (None, itag)) initags))
                  ::
                  (* Now the length *)
                  (None, inilen) 
                  ::
                  (* Now the real initializer *)
                  (None, Const(CStr s, cloc)) :: []) in
      theFile := GVar (gvar, Some varinit) :: !theFile;
      let fatChrPtrType = fixupType charPtrType in
      let result = Lval(Var gvar, Field(dfld, NoOffset)) in
      ([], F2 (fatChrPtrType, result, CastE(voidPtrType, result, lu)))


  | UnOp (uop, e, restyp, l) -> 
      let restyp' = fixupType restyp in
      let (et, doe, e') = boxexp e in
      assert (not (isFatType restyp'));
          (* The result is never a pointer *)
      (doe, L(restyp', UnOp(uop, e', restyp', l)))

  | BinOp (bop, e1, e2, restyp, l) -> begin
      let restyp' = fixupType restyp in
      let (et1, doe1, e1') = boxexp e1 in
      let (et2, doe2, e2') = boxexp e2 in
      match bop, isFatType et1, isFatType et2 with
      | (Plus|Minus), true, false -> 
          let ptype = (getPtrFieldOfFat et1).ftype in
          (doe1 @ doe2, F2 (restyp', 
                            BinOp(bop, readPtrField e1' et1, e2', ptype, l),
                            readBaseField e1' et1))
      | (Plus), false, true -> 
          let ptype = (getPtrFieldOfFat et2).ftype in
          (doe1 @ doe2, F2 (restyp', 
                            BinOp(bop, e1', readPtrField e2' et2, ptype, l),
                            readBaseField e2' et2))
      | (Minus|Eq|Ne|Le|Lt|Ge|Gt), true, true -> 
          (doe1 @ doe2, 
           L(restyp', BinOp(bop, readPtrField e1' et1, 
                                 readPtrField e2' et2, restyp', l)))

      | _, false, false -> 
          (doe1 @ doe2, L(restyp', BinOp(bop,e1',e2',restyp', l)))

      | _, _, _ -> E.s (E.unimp "boxBinOp: %a@!et1=%a@!et2=%a@!" 
                          d_binop bop d_plaintype et1 d_plaintype et2)
  end

  | SizeOf (t, l) -> 
      let t' = fixupType t in
      ([], L(intType, SizeOf(t', l)))

   (* Intercept references of _iob. A major hack *)
  | AddrOf ((Var vi,
             First(Index(Const(CInt _, _) as n, NoOffset))) as lv, 
            _) when !msvcMode && vi.vname = "_iob_fp_" 
    -> 
      let tres = fixupType (typeOf e) in
      let tmp1 = makeTempVar !currentFunction fatVoidPtr in
      let tmp2 = makeTempVar !currentFunction tres in
      let seq  = 
        [ boxstmt (call (Some tmp1) (Lval(var getIOBFunction.svar)) [ n ]);
          boxstmt (assign tmp2 (CastE(tres, Lval(var tmp1), lu))) ] in
      (seq, F1(tres, Lval(var tmp2)))

  | AddrOf (lv, l) ->
      let (lvt, dolv, lv', baseaddr) = boxlval lv in
      (* Check that variables whose address is taken are flagged *)
      (match lv' with
        (Var vi, _) when not vi.vaddrof -> 
          E.s (E.bug "addrof not set for %s" vi.vname)
      | _ -> ());
      (* The result type. *)
      let ptrtype = 
        match lvt with
          TArray(t, _, _) -> fixupType (TPtr(t, [])) 
        | _ -> fixupType (TPtr(lvt, []))
      in
      (dolv, F2 (ptrtype, AddrOf(lv', l), baseaddr))

    (* StartOf is like an AddrOf except for typing issues. *)
  | StartOf lv -> 
      let (lvt, dolv, lv', baseaddr) = boxlval lv in
      (* Check that variables whose address is taken are flagged *)
      (match lv' with
        (Var vi, _) when not vi.vaddrof -> 
          E.s (E.bug "addrof not set for %s" vi.vname)
      | _ -> ());
      (* The result type. *)
      let ptrtype = 
        match unrollType lvt with
          TArray(t, _, _) -> fixupType (TPtr(t, [])) 
        | _ -> E.s (E.unimp "StartOf on a non-array")
      in
      (dolv, F2 (ptrtype, AddrOf(addOffset (Index(zero, NoOffset)) lv', lu),
                 baseaddr))

  | Question (e1, e2, e3, l) ->       
      let (_, doe1, e1') = boxexp (CastE(intType, e1, lu)) in
      let (et2, doe2, e2') = boxexp e2 in
      let (et3, doe3, e3') = boxexp e3 in
      let result = 
        if isFatType et2 then 
          F1 (et2, Question (e1', e2', e3', l))
        else
          L  (et2, Question (e1', e2', e3', l))
      in
      (doe1 @ doe2 @ doe3, result)

  | _ -> begin
      ignore (E.log "boxexp: %a\n" d_exp e);
      ([], L (charPtrType, dExp (dprintf "booo expression(%a)" d_exp e)))
  end

and fexp2exp (fe: fexp) (doe: stmt list) : expRes = 
  match fe with
    L (t, e') -> (t, doe, e')            (* Done *)
  | F1 (t, e') -> (t, doe, e')           (* Done *)
  | F2 (ft, ep, eb) -> 
      let (doset, lv) = setFatPointer ft (fun _ -> ep) eb in
      (ft, doe @ doset, Lval(lv))
  | FC (newt, oldt, e') -> 
      (* Put e1' in a variable if not an lval *)
      let caste, tmp = 
        match e' with
        | Lval tmp -> [], tmp
        | _ -> 
            let tmp = var (makeTempVar !currentFunction oldt) in
            ([mkSet tmp e'], tmp)
      in
      (newt, 
       doe @ caste, 
       Lval(Mem (CastE(TPtr(newt, []), 
                       AddrOf (tmp, lu), lu)),
            Index(zero, NoOffset)))


    (* Box an expression and resolve the fexp into statements *)
and boxexp (e : exp) : expRes = 
  let (doe, fe) = boxexpf e in
  fexp2exp fe doe

and boxexpSplit (e: exp) = 
  let (doe, fe') = boxexpf e in
  match fe' with
    L(lt, e') -> (lt, doe, e', e')
  | F2(ft,p,b) -> ((getPtrFieldOfFat ft).ftype, doe, p, b)
  | _  ->
      let (et, caste, e'') = fexp2exp fe' doe in
      let (tptr, ptr, base) = readPtrBaseField e'' et in
      (tptr, doe @ caste, ptr, base)


    (* Cast an fexp to another one. Accumulate necessary statements to doe *)
and castTo (fe: fexp) (newt: typ) (doe: stmt list) : stmt list * fexp = 
  match fe, isFatType newt with
  | L(lt, e) , false -> (* LEAN -> LEAN *)
      (doe, L(newt, doCast e lt newt))
  | L(lt, e), true -> (* LEAN -> FAT *)
      let ptype = (getPtrFieldOfFat newt).ftype in
      let newp = 
        if typeSig lt = typeSig ptype then e else CastE (ptype, e, lu) in
      (doe, F2 (newt, newp, CastE(voidPtrType, zero, lu)))
  
  (* FAT -> LEAN *)
  | F1(oldt, e), false ->
      (doe, L(newt, CastE(newt, readPtrField e oldt, lu)))
  | F2(oldt, ep, eb), false ->
      (doe, L(newt, CastE(newt, ep, lu)))
  | FC(oldt, prevt, e), false ->
      (doe, L(newt, CastE(newt, readPtrField e prevt, lu)))

  (* FAT -> FAT *)
  | F1(oldt, e), true -> (doe, FC (newt, oldt, e))
  | F2(oldt, ep, eb), true -> 
      let ptype = (getPtrFieldOfFat newt).ftype in
      (doe, F2 (newt, CastE(ptype, ep, lu), eb))
  | FC(oldt, prevt, e), true -> 
      (doe, FC(newt, prevt, e))
      

    (* Create a new temporary of a fat type and set its pointer and base 
     * fields *)
and setFatPointer (t: typ) (p: typ -> exp) (b: exp) : stmt list * lval = 
  let tmp = makeTempVar !currentFunction t in
  let fptr = getPtrFieldOfFat t in
  let fbase = getBaseFieldOfFat t in
  let p' = p fptr.ftype in
  ([ mkSet (Var tmp, Field(fptr,NoOffset)) p';
     mkSet (Var tmp, Field(fbase,NoOffset)) b ], 
     (Var tmp, NoOffset))
      
and readPtrField e t = 
  let (tptr, ptr, base) = readPtrBaseField e t in ptr
      
and readBaseField e t = 
  let (tptr, ptr, base) = readPtrBaseField e t in base

and fromPtrToBase e = 
  let rec replacePtrBase = function
      Field(fip, NoOffset) when fip.fname = "_p" -> begin
        (* Find the fat type that this belongs to *)
        try
          let fat = 
            H.find fixedTypes (typeSig (TForward ("struct " ^ fip.fstruct))) in
          let bfield = getBaseFieldOfFat fat in
          Field(bfield, NoOffset)
        with Not_found -> 
          E.s (E.unimp "Field %s is not a component of a fat type" fip.fname)
      end
    | Field(f', o) -> Field(f',replacePtrBase o)
    | _ -> E.s (E.unimp "Cannot find the _p field to replace in %a\n"
                  d_plainexp e)
  in
  match e with
    Lval (b, off) -> Lval(b, replacePtrBase off)
  | _ -> E.s (E.unimp "replacing _p with _b in a non-lval")

let boxFile globals =
  ignore (E.log "Boxing file\n");
  let doGlobal g = 
    match g with
                                        (* We ought to look at pragmas to see 
                                         * if they talk about alignment of 
                                         * structure fields *)
    | GPragma s -> theFile := g :: !theFile
    | GVar (vi, init) as g -> begin 
        if debug then
          ignore (E.log "Boxing GVar(%s)\n" vi.vname); 
        vi.vtype <- fixupType vi.vtype;
        let isFuncType = 
          match unrollType vi.vtype with
            TFun _ -> true
          | TPtr (TFun _, _) -> true
          | _ -> false
        in
          (* If the type has changed and this is a global variable then we
           * also change its name *)
        if vi.vglob && vi.vstorage <> Static && typeHasChanged vi.vtype then
          begin
            let oldname = vi.vname in
            vi.vname <- vi.vname ^ "_fp_"
          end;
        (* Tag all globals, except function prototypes *)
        if not (mustBeTagged vi) then
          theFile := GVar(vi, None) :: !theFile
        else begin
          (* Make the initializer *)
          (* tag the type *)
          let newtyp, tfld, _, _, initags, inilen = tagType vi.vtype in
          let init' = 
            try
              match init with
                None -> []
              | Some e -> 
                  let (et, doe, e', e'base) = 
                    boxexpSplit (CastE(vi.vtype, e, lu)) in
                  if doe <> [] then
                    E.s (E.unimp "Non-pure initializer %a\n"
                           d_exp e);
                  if isFatType vi.vtype then
                    [(None, Compound(vi.vtype, [ (None, e'); (None, e'base)]))]
                  else
                    [(None, e')]
            with e -> [(None, dExp (dprintf "booo_init(%s)"
                                           (Printexc.to_string e)))]
          in
          vi.vtype <- newtyp;
          (* Add it to the tag initializer *)
          let varinit = 
            if vi.vstorage = Extern then None else
            Some (Compound (newtyp, 
                      (* The tags. We could use designators but MSVC does not 
                       * like them  *)
                      (None,
                       Compound(tfld.ftype, 
                                List.map (fun (_, itag) -> (None, itag)) 
                                  initags))
                      ::
                      (* Now the length *)
                      (None, inilen) 
                      ::
                      (* Now the real initializer *)
                      init')) in
          theFile := GVar(vi, varinit) :: !theFile
        end
    end
    | GType (n, t) as g -> 
        if debug then
          ignore (E.log "Boxing GType(%s)\n" n);
        let tnew = fixupType t in (* Do this first *)
        theFile := GType (n, tnew) :: !theFile

    | GFun f -> 
        if debug then
          ignore (E.log "Boxing GFun(%s)\n" f.svar.vname);
        (* Fixup the return type as well *)
        f.svar.vtype <- fixupType f.svar.vtype;
          (* If the type has changed and this is a global function then we
           * also change its name *)
        if f.svar.vglob && f.svar.vstorage <> Static 
            && typeHasChanged f.svar.vtype then
          f.svar.vname <- f.svar.vname ^ "_fp_";
        (* Fixup the types of the remaining locals *)
        List.iter (fun l -> l.vtype <- fixupType l.vtype) f.slocals;
        currentFunction := f;           (* so that maxid and locals can be 
                                         * updated in place *)
        (* Now we must take all locals whose address is taken and turn their 
           types into structures with tags and length field *)
        let tagLocal stmacc l = 
          if not (mustBeTagged l) then stmacc
          else
            let newtype, _, lfld, _, initags, inilen = tagType l.vtype in
            l.vtype <- newtype;
            (* Create the initializers *)
            (List.map (fun (off, tv) ->
              mkSet (Var l, off) tv) initags) 
            @
            (mkSet (Var l, Field(lfld, NoOffset)) inilen) :: stmacc
        in
        let inilocals = List.fold_left tagLocal [] f.slocals in
        f.sbody <- mkSeq (inilocals @ [boxstmt f.sbody]);
        theFile := GFun f :: !theFile

    | (GAsm s) as g -> theFile := g :: !theFile
  in
  if debug then
    ignore (E.log "Boxing file\n");
  let doGlobal x = 
    try doGlobal x with e -> begin
      ignore (E.log "boxglobal (%s)\n" (Printexc.to_string e));
      theFile := GAsm ("booo_global") :: !theFile
    end 
  in
  H.clear taggedTypes;
  List.iter doGlobal globals;
  List.rev (!theFile)
      
