open Cil
open Pretty

module H = Hashtbl

let debugType = false
let debug = false

           (* After processing an expression, we create its type, a list of 
            * instructions that should be executed before this exp is used, 
            * and a replacement exp *)
type expRes = 
    typ * stmt list * exp

            (* Same for offsets *)
type offsetRes = 
    typ * stmt list * offset


(*** Helpers *)            


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
  | TNamed (n, _) -> n
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
      if n = "" then makeNewTypeName "enum"
      else "enum_" ^ n
  | TStruct (n, _, _) -> 
      if n = "" then makeNewTypeName "struct"
      else "struct_" ^ n
  | TUnion (n, _, _) -> 
      if n = "" then makeNewTypeName "union"
      else "union_" ^ n
  | TFun _ -> makeNewTypeName "fun"
  | _ -> makeNewTypeName "type"


 (* Define tag structures, one for each size of tags, along with initializers
  *)
let tagTypes : (int, typ) H.t = H.create 17
let tagTypeInit sz =                    (* sz is the sizeOf the data *)
  let words = (sz + 3) lsr 2 in
  let tagwords = (words + 15) lsr 4 in  (* 2 bits / word, rounded up *)
  let tagtype = 
    try
      H.find tagTypes tagwords            (* Maybe we already made one *)
    with Not_found -> begin
      let tname = "tag_" ^ (string_of_int tagwords) in (* The name *)
      let t = 
        TStruct(tname, [ { fstruct = tname;
                           fname   = "_t";
                           ftype   = TArray(intType, 
                                            Some (integer tagwords), []);
                           fattr   = [];
                         };
                         { fstruct = tname;
                           fname   = "_l";
                           ftype   = intType;
                           fattr   = [];
                         }; ], []) in
      let t' = TNamed(tname, t) in
      H.add tagTypes tagwords t';
      theFile := GType(tname, t) :: !theFile;
      t'
    end
  in
  let rec mkTags acc = function
      0 -> acc
    | n -> mkTags (zero :: acc) (n - 1)
  in
  (tagtype, 
   Compound(tagtype, 
            [ Compound(TArray(intType, Some (integer tagwords), []),
                       mkTags [] tagwords);
              integer words ]))


(**** FIXUP TYPE ***)
let fatPointerTypes : (typ, typ) H.t = H.create 17
let rec fixupType t = 
  if debugType then
    ignore (E.log "Fixing up type %a\n" d_type t);
  match t with 
    (TInt _|TEnum _|TFloat _|TVoid _|TBitfield _) -> t
  | TPtr (t', a) -> begin
        (* Now do the base type *)
      let fixed' = fixupType t' in
      let tname = "fatp_" ^ (typeName fixed') in (* The name *)
      try
        H.find fatPointerTypes t
      with Not_found -> begin
        let fixed = 
          TStruct(tname, [ { fstruct = tname;
                             fname   = "_p";
                             ftype   = TPtr(fixed', a);
                             fattr   = [];
                           };
                           { fstruct = tname;
                             fname   = "_b";
                             ftype   = voidPtrType;
                             fattr   = [];
                           }; ], []) 
        in
        let tres = TNamed(tname, fixed) in
        H.add fatPointerTypes t tres;
        theFile := GType(tname, fixed) :: !theFile;
        tres
      end
  end
        
  | TForward _ ->  t              (* Don't follow TForward *)
  | TNamed (n, t') -> TNamed (n, fixupType t')
        
  | TStruct(n, flds, a) -> begin
      (* Maybe this is already fixed *)
      match flds with 
        [p;b] when p.fname = "_p" && b.fname = "_b" -> t
      | _ -> 
          let r = 
            TStruct(n, 
                    List.map 
                      (fun fi -> {fi with ftype = fixupType fi.ftype}) flds,
                    a) in
          replaceForwardType ("struct " ^ n) r;
          r
  end
  | TUnion (n, flds, a) -> 
      let r = 
        TUnion(n, 
               List.map (fun fi -> {fi with ftype = fixupType fi.ftype}) flds,
               a) in
      replaceForwardType ("union " ^ n) r;
      r
        
  | TArray(t', l, a) -> TArray(fixupType t', l, a)
        
  | TFun(rt,args,isva,a) ->
      List.iter (fun argvi -> argvi.vtype <- fixupType argvi.vtype) args;
      (* no tag on functions *)
      TFun(fixupType rt, args, isva, a)

  

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

  
   (* Test if we must check the return value *)
let mustCheckReturn tret =
  match unrollType tret with
    TPtr _ -> true
  | TArray _ -> true
  | _ -> isFatType tret



(****** the CHECKERS ****)
let checkSafeRetFat = 
  let fatVoidPtr = fixupType voidPtrType in
  let fdec = emptyFunction "CHECK_SAFERETFAT" in
  let arg  = makeLocalVar fdec "x" fatVoidPtr in
  fdec.svar.vtype <- TFun(voidType, [ arg ], false, []);
  fdec
    
let checkSafeRetLean = 
  let fdec = emptyFunction "CHECK_SAFERETLEAN" in
  let arg  = makeLocalVar fdec "x" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ arg ], false, []);
  fdec
    
let checkSafeRdLean = 
  let fdec = emptyFunction "CHECK_SAFERDLEAN" in
  let arg  = makeLocalVar fdec "x" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ arg ], false, []);
  fdec
    
let checkSafeRdFat = 
  let fdec = emptyFunction "CHECK_SAFERDFAT" in
  let arg  = makeLocalVar fdec "x" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ arg ], false, []);
  fdec
    
let checkSafeWrLean = 
  let fdec = emptyFunction "CHECK_SAFEWRLEAN" in
  let arg  = makeLocalVar fdec "x" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ arg ], false, []);
  fdec
    
let checkSafeWrFat = 
  let fdec = emptyFunction "CHECK_SAFEWRFAT" in
  let arg  = makeLocalVar fdec "x" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ arg ], false, []);
  fdec
    


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
        let (_, doe, e') = boxexp e in
        let doe' = 
          match !currentFunction.svar.vtype with 
            TFun(tRes, _, _, _) when mustCheckReturn tRes -> 
              if isFatType tRes then begin
                (* Cast e' to fat_voidptr *)
                let fatptr = fixupType voidPtrType in
                let (caste, rese) = castTo e' tRes fatptr in
                doe @ caste @ 
                [call None (Lval(var checkSafeRetFat.svar)) [rese]]
              end else begin
                doe @ 
                [call None (Lval(var checkSafeRetLean.svar)) [ e' ]]
              end
          | _ -> doe
        in
        mkSeq (doe' @ [Return (Some e')])
          
    | Instruction i -> boxinstr i
  with e -> begin
    ignore (E.log "boxstmt (%s)\n" (Printexc.to_string e));
    dStmt (dprintf "booo_statement(%a)" d_stmt s)
  end

and boxinstr (ins: instr) = 
  if debug then
    ignore (E.log "Boxing %a\n" d_instr ins);
  try
    match ins with 
    | Set(Var(vi, off, l), e, l') ->
        let (rest, dooff, off') = boxoffset off vi.vtype in
        let (_, doe, e') = boxexp e in
        mkSeq (dooff @ doe @ [Instruction(Set(Var(vi, off', l), e', l'))])

    | Set(Mem(addr, off, l), e, l') ->
        let (addrt, doaddr, addr', addr'base) = boxexpSplit addr in
        let (rest, dooff, off') = boxoffset off addrt in
        let (et, doe, e') = boxexp e in
        let newlval = Mem(addr', off', l) in
        mkSeq (doaddr @ dooff @ doe @ 
               [Instruction(Set(newlval,e',l'))])
        
    | Call(vi, f, args, l) ->
        let (ft, dof, f') = boxexp f in
        let (ftret, ftargs, isva) =
          match ft with 
            TFun(fret, fargs, isva, _) -> (fret, fargs, isva) 
          | _ -> E.s (E.unimp "call of a non-function: %a : %a" 
                        d_exp f' d_type ft) 
        in
        let rec doArgs restargs restargst = 
          match restargs, restargst with
            [], [] -> [], []
          | a :: resta, _ :: restt -> 
              let (_, doa, a') = boxexp a in
              let (doresta, resta') = doArgs resta restt in
              (doa @ doresta,  a' :: resta')
          | a :: resta, [] when isva -> 
              let (at, doa, a') = boxexp a in
              let (doresta, resta') = doArgs resta [] in
              let a'' = 
                if isFatType at then readPtrField a' at else a'
              in
              (doa @ doresta, a'' :: resta')

          | _ -> E.s (E.unimp "vararg in call to %a" d_exp f)
        in
        let (doargs, args') = doArgs args ftargs in
        mkSeq (dof @ doargs @ [call vi f' args'])
          
    | i -> E.s (E.unimp "boxinstr: %a\n" d_instr ins)
  with e -> begin
    ignore (E.log "boxinstr (%s)\n" (Printexc.to_string e));
    dStmt (dprintf "booo_instruction(%a)" d_instr ins)
  end


and boxoffset (off: offset) (basety: typ) : offsetRes = 
  match off with 
  | NoOffset -> (* Leave it fat if already fat *)
      (basety, [], NoOffset)

  | Index (e, resto) when isFatType basety ->
      let fptr = getPtrFieldOfFat basety in
      let (rest, doo, off') = boxoffset off fptr.ftype in
      (rest, doo, Field(fptr, off'))

  | Index (e, resto) -> 
      let rest =                    (* The result type *)
        match basety with
          TPtr (t, _) -> t
        | TArray (t, _, _) -> t
        | _ -> E.s (E.bug "Index for type %a" d_type basety)
      in
      let (_, doe, e') = boxexp e in
      let (rest', doresto, off') = boxoffset resto rest in
      (rest', doe @ doresto, Index(e', off'))

  | Field (fi, resto) ->
      let fi' = {fi with ftype = fixupType fi.ftype} in
      let (rest, doresto, off') = boxoffset resto fi'.ftype in
      (rest, doresto, Field(fi', off'))
        
  | _ -> begin
      ignore (E.log "boxoffset\n");
      (charPtrType, [], NoOffset) 
  end 
        

and boxexp (e : exp) : expRes = 
  match e with
  | Lval (Var(vi, off, l)) ->       (* Reading a variable *)
      let (rest, dooff, off') = boxoffset off vi.vtype in
      (rest, dooff, Lval(Var(vi, off', l)))

                                        (* Reading a memory address *)
  | Lval (Mem(addr, off, l)) -> 
      let (addrt, doaddr, addr', addr'base) = boxexpSplit addr in
      let (rest, dooff, off') = boxoffset off addrt in
      let newlval = Mem(addr', off', l) in
      (rest, 
       doaddr @ dooff,
       Lval(newlval))

  | Const ((CInt _ | CChr _), _) -> (intType, [], e)
  | Const (CReal _, _) -> (doubleType, [], e)
  | CastE (t, e, l) -> begin
      let t' = fixupType t in
      let (et, doe, e') = boxexp e in
      (* Put e into a variable *)
      let doe', rese = castTo e' et t' in
      (t', doe @ doe', rese)
  end
        
  | Const (CStr s, l) -> 
      (* Make a global variable that stores this one *)
      let l = 1 + String.length s in 
      let tres = TArray(charType, Some (integer l), []) in
      let gvar = makeGlobalVar (makeNewTypeName "___string") tres in
      gvar.vstorage <- Static;
      let (tagtype, taginit) = tagTypeInit l in
      let tagvar = makeGlobalVar ("__tag_of_" ^ gvar.vname) tagtype in
      tagvar.vstorage <- Static;
      theFile := GVar (tagvar, Some (taginit)) :: !theFile;
      (* Now the initializer *)
      let rec loop i =
        if i >= l - 1 then
          [Const(CChr(Char.chr 0), lu)]
        else
          Const(CChr(String.get s i), lu) :: loop (i + 1)
      in
      theFile := GVar (gvar, Some (Compound(tres, loop 0))) :: !theFile;
      (* Now create a new temporary variable *)
      let fatChrPtrType = fixupType charPtrType in
      let result = Lval(var gvar) in
      let (doset, reslv) = setFatPointer fatChrPtrType 
                                         (fun _ -> result)
                                         (CastE(voidPtrType, result, lu)) in
      (fatChrPtrType, doset, Lval(reslv))


  | UnOp (uop, e, restyp, l) -> 
      let restyp' = fixupType restyp in
      let (et, doe, e') = boxexp e in
      assert (not (isFatType restyp'));
          (* The result is never a pointer *)
      (restyp', doe, UnOp(uop, e', restyp', l))

  | BinOp (bop, e1, e2, restyp, l) -> begin
      let restyp' = fixupType restyp in
      let (et1, doe1, e1') = boxexp e1 in
      let (et2, doe2, e2') = boxexp e2 in
      match bop, isFatType et1, isFatType et2 with
      | Plus, true, false -> 
          let doset, reslv =
            setFatPointer restyp' 
                          (fun t -> 
                            BinOp(Plus, readPtrField e1' et1, e2', t, l))
                          (CastE(voidPtrType, zero, lu)) in
          (restyp', doe1 @ doe2 @ doset, Lval(reslv))
      | Plus, false, true -> 
          let doset, reslv =
            setFatPointer restyp' 
                          (fun t -> 
                            BinOp(Plus, e1', readPtrField e2' et2, t, l))
                          (CastE(voidPtrType, zero, lu)) in
          (restyp', doe1 @ doe2 @ doset, Lval(reslv))
      | (Minus|Eq|Ne|Le|Lt|Ge|Gt), true, true -> 
          (restyp', doe1 @ doe2, 
           BinOp(bop, readPtrField e1' et1, readPtrField e2' et2, restyp', l))

      | _, false, false -> (restyp', doe1 @ doe2, BinOp(bop,e1',e2',restyp', l))

      | _, _, _ -> E.s (E.unimp "boxBinOp")
  end
        
  | AddrOf (Var(vi,off,l), l') -> 
      if not vi.vaddrof then 
        E.s (E.bug "addrof not set for %s" vi.vname);
      let (rest, dooff, off') = boxoffset off vi.vtype in
      let tres = fixupType (TPtr(rest, [])) in
      let (doset, reslv) = 
        setFatPointer tres 
          (fun _ -> AddrOf(Var(vi,off',l), l))
          (CastE(voidPtrType, 
                 AddrOf(Var(vi,NoOffset,l), l),
                 lu)) in
      (tres, dooff @ doset, Lval(reslv))
        
  | AddrOf (Mem(e, off, l), l') -> 
      let (et, doe, e', e'base) = boxexpSplit e in
      let (rest, dooff, off') = boxoffset off et in
      let tres = fixupType (TPtr(rest, [])) in
      let (doset, reslv) = 
        setFatPointer tres 
          (fun _ -> AddrOf(Mem(e',off',l), l))
          e'base in
      (tres, doe @ dooff @ doset, Lval(reslv))
        
  | _ -> begin
      ignore (E.log "boxexp: %a\n" d_exp e);
      (charPtrType, [], dExp (dprintf "booo expression(%a)" d_exp e))
  end

and boxexpSplit (e: exp) = 
  let (et, doe, e') = boxexp e in
  let (tptr, ptr, base) = readPtrBaseField e' et in
  (tptr, doe, ptr, base)

and readPtrBaseField e et =     
  if isFatType et then
    let fptr = getPtrFieldOfFat et in
    let fbase = getBaseFieldOfFat et in
    let rec compOffsets = function
        NoOffset -> Field(fptr, NoOffset), Field(fbase, NoOffset)
      | Field(fi, o) -> 
          let po, bo = compOffsets o in
          Field(fi, po), Field(fi, bo)
      | Index(e, o) -> (* e is already processed *)
          let po, bo = compOffsets o in
          Index(e, po), Index(e, bo)
      | CastO(t, o) ->
          let po, bo = compOffsets o in
          CastO(t, po), CastO(t, bo)
    in
    let ptre, basee = 
      match e with
        Lval(Var(vi,o,l)) -> 
          let po, bo = compOffsets o in
          Lval(Var(vi,po,l)), Lval(Var(vi,bo,l))
      | Lval(Mem(e'',o,l)) -> 
          let po, bo = compOffsets o in
          Lval(Mem(e'',po,l)), Lval(Mem(e'',bo,l))
      | _ -> E.s (E.unimp "split _p field offset")
    in
    (fptr.ftype, ptre, basee)
  else
    (et, e, e)

and castTo e et newt = 
  match isFatType et, isFatType newt with
    true, true ->                       (* Cast from struct to struct. Put 
                                         * the expression in a variable first*)
      let doe, tmp = 
        match e with
          Lval tmp -> [], tmp
        | _ -> 
            let tmp = var (makeTempVar !currentFunction et) in
            ([mkSet tmp e], tmp)
      in
      (doe, 
       Lval(Mem(CastE(TPtr(newt, []), 
                      AddrOf (tmp, lu), lu),NoOffset,lu)))

  | true, false -> ([], CastE(newt, readPtrField e et, lu))

  | false, false -> ([], CastE(newt, e, lu))

  | false, true -> 
      (*ignore (E.log "Casting %a from %a to %a\n" 
                d_exp e d_type et d_type newt);*)
      let docast, reslv = setFatPointer newt 
                                (fun t -> CastE(t, e, lu))
                                (CastE(voidPtrType, zero, lu)) in
      (docast, Lval(reslv))
          

    (* Create a new temporary of a fat type and set its pointer and base 
     * fields *)
and setFatPointer (t: typ) (p: typ -> exp) (b: exp) : stmt list * lval = 
  let tmp = makeTempVar !currentFunction t in
  let fptr = getPtrFieldOfFat t in
  let fbase = getBaseFieldOfFat t in
  let p' = p fptr.ftype in
  ([ mkSet (Var(tmp,Field(fptr,NoOffset),lu)) p';
     mkSet (Var(tmp,Field(fbase,NoOffset),lu)) b ], 
     Var(tmp, NoOffset, lu))
      
and readPtrField e t = 
  let (tptr, ptr, base) = readPtrBaseField e t in ptr
      
and readBaseField e t = 
  let (tptr, ptr, base) = readPtrBaseField e t in base


let boxFile globals =
  theFile := GVar (checkSafeRetFat.svar, None) :: !theFile;
  theFile := GVar (checkSafeRetLean.svar, None) :: !theFile;
  let doGlobal g = 
    match g with
    | GVar (vi, init) as g -> begin
        if debug then
          ignore (E.log "Boxing GVar(%s)\n" vi.vname);
        vi.vtype <- fixupType vi.vtype;
        if vi.vstorage <> Extern &&
          (match vi.vtype with TFun _ -> false | _ -> true) then begin
            let sz = intSizeOfNoExc vi.vtype in
            let (tagtype, taginit) = tagTypeInit sz in
            let tagvar = makeGlobalVar ("__tag_of_" ^ vi.vname) tagtype in
            tagvar.vstorage <- Static;
            theFile := GVar (tagvar, Some (taginit)) :: !theFile
          end;
        theFile := g :: !theFile
    end
    | GType (n, t) as g -> 
        if debug then
          ignore (E.log "Boxing GType(%s)\n" n);
        let tnew = fixupType t in (* Do this first *)
        theFile := GType (n, tnew) :: !theFile

    | GFun f -> 
        if debug then
          ignore (E.log "Boxing GFun(%s)\n" f.svar.vname);
        (* Fixup the types of locals *)
        List.iter (fun l -> l.vtype <- fixupType l.vtype) f.slocals;
        (* Fixup the return type as well *)
        f.svar.vtype <- fixupType f.svar.vtype;
        currentFunction := f;           (* so that maxid and locals can be 
                                         * updated in place *)
        f.sbody <- boxstmt f.sbody;
        theFile := GFun f :: !theFile

    | (GAsm s) as g -> theFile := g :: !theFile
  in
  if debug then
    ignore (E.log "Boxing file\n");
  List.iter doGlobal globals;
  List.rev (!theFile)
      
