open Cil
open Pretty

module H = Hashtbl

let debugType = false
let debug = false

let checkReturn = false
           (* After processing an expression, we create its type, a list of 
            * instructions that should be executed before this exp is used, 
            * and a replacement exp *)
type expRes = 
    typ * stmt list * exp

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

(* A list of functions to be left alone *)
let leaveAlone = 
  [ "printf"; "fprintf"; ]

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
  | TNamed (n, t) -> TNamed(n, fixupType t) (* Keep the Named types *)
    (* Sometimes we find a function type without arguments (a prototype that 
     * we have done before). Put the argument names back *)
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
      | TPtr ((TFun _) as t', a) -> 
          TPtr (fixupType t', a)        (* Pointers to functions are lean *)

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
                               ftype   = TPtr(TVoid ([]), a);
                               fattr   = [];
                             }; ], []) 
          in
          let tres = TNamed(tname, fixed) in
          H.add fixedTypes (typeSig fixed) fixed; (* We add fixed ourselves. 
                                                   * The TNamed will be added 
                                                   * after doit  *)
          H.add fixedTypes (typeSig (TPtr(fixed',a))) fixed;
          theFile := GType(tname, fixed) :: !theFile;
          tres
      end
            
      | TForward _ ->  t              (* Don't follow TForward *)
      | TNamed (n, t') -> TNamed (n, fixupType t')
            
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

  
   (* Test if we must check the return value *)
let mustCheckReturn tret =
  checkReturn &&
  match unrollType tret with
    TPtr _ -> true
  | TArray _ -> true
  | _ -> isFatType tret


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
        let retType = (* Already fixed *)
          match !currentFunction.svar.vtype with 
            TFun(tRes, _, _, _) -> tRes
          | _ -> E.s (E.bug "Current function's type is not TFun")
        in 
        let (_, doe, e') = boxexp e in
        let doe' = (* Add the check *)
          if mustCheckReturn retType then
            if isFatType retType then begin
                (* Cast e' to fat_voidptr *)
              let fatptr = fixupType voidPtrType in
              let (caste, rese) = castTo e' retType fatptr doe in
              caste @ 
              [call None (Lval(var checkSafeRetFat.svar)) [rese]]
            end else begin
              doe @ 
              [call None (Lval(var checkSafeRetLean.svar)) [ e' ]]
            end
          else
            doe
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
    | Set (lv, e, l) -> 
        let (rest, dolv, lv', _) = boxlval lv in
        let (_, doe, e') = boxexp e in
        mkSeq (dolv @ doe @ [Instruction(Set(lv', e', l))])
(*        
    | Set((Var vi, off), e, l') ->
        let tbase = 
          match vi.vtype with
            TArray(t, _, _) -> t
          | x -> x
        in
        let (rest, dooff, off') = boxoffset off tbase in

    | Set((Mem addr, off), e, l') ->
        let (addrt, doaddr, addr', addr'base) = boxexpSplit addr in
        let addrt' = 
          match addrt with
            TPtr(t, _) -> t
          | TArray(t, _, _) -> t
          | _ -> E.s (E.unimp "Reading from a non-pointer type: %a\n"
                        d_plaintype addrt)
        in
        let (rest, dooff, off') = boxoffset off addrt' in
        let (et, doe, e') = boxexp e in
        let newlval = Mem addr', off' in
        mkSeq (doaddr @ dooff @ doe @ 
               [Instruction(Set(newlval, e', l'))])
*)        
    | Call(vi, f, args, l) ->
        let (ft, dof, f') = boxexp f in
        let (ftret, ftargs, isva) =
          match ft with 
            TFun(fret, fargs, isva, _) -> (fret, fargs, isva) 
          | _ -> E.s (E.unimp "call of a non-function: %a @!: %a" 
                        d_plainexp f' d_plaintype ft) 
        in
        let rec doArgs restargs restargst = (* The types of functions have 
                                             * been fixed already  *) 
          match restargs, restargst with
            [], [] -> [], []
          | a :: resta, t :: restt -> 
              let (at, doa, a') = boxexp a in
              let (doa', a'') = castTo a' at t.vtype doa in
              let (doresta, resta') = doArgs resta restt in
              (doa' @ doresta,  a'' :: resta')
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

    | Asm(tmpls, isvol, outputs, inputs, clobs) ->
        let rec checkOutputs = function
            [] -> ()
          | (c, vi) :: rest -> 
              if isFatType vi.vtype then
                ignore (E.log "Warning: fat output %s in %a\n"
                          vi.vname 
                          d_instr ins);
              checkOutputs rest
        in
        checkOutputs outputs;
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
        mkSeq (doins @ 
               [Instruction(Asm(tmpls, isvol, outputs, inputs', clobs))])
            
  with e -> begin
    ignore (E.log "boxinstr (%s)\n" (Printexc.to_string e));
    dStmt (dprintf "booo_instruction(%a)" d_instr ins)
  end


 (* Given an lvalue, generate the type of denoted element, a list of 
  * statements to prepare to replacement lval, the replacement lval, and an 
  * expression that denotes the base of the memory area containing the lval *)
and boxlval (b, off) : (typ * stmt list * lval * exp) = 
  let (b', dob, tbase, baseaddr) = 
    match b with
      Var vi -> 
        let tbase = 
          match vi.vtype with
            TArray(t, _, _) -> t
          | x -> x
        in
        let baseaddr = 
          match vi.vtype with
            TArray _ -> StartOf(Var vi, NoOffset)
          | _ -> AddrOf((Var vi, NoOffset), lu)
        in
        (b, [], tbase, baseaddr)
    | Mem addr -> 
        let (addrt, doaddr, addr', addr'base) = boxexpSplit addr in
        let addrt' = 
          match addrt with
            TPtr(t, _) -> t
          | TArray(t, _, _) -> t
          | _ -> E.s (E.unimp "Reading from a non-pointer type: %a\n"
                        d_plaintype addrt)
        in
        (Mem addr', doaddr, addrt', addr'base)
  in
  let (t, dooff, off') = boxoffset off tbase in
  (t, dob @ dooff, (b', off'), baseaddr)


and boxoffset (off: offset) (basety: typ) : offsetRes = 
(*
  if !currentFunction.svar.vname = "releaseHashes" then
    ignore (E.log "boxoffset %a@!of %a\n"
              d_plainlval (Mem(zero,off,lu))
              d_plaintype basety);
*)
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
        
        

and boxexp (e : exp) : expRes = 
  match e with
  | Lval (lv) -> 
      let rest, dolv, lv', _ = boxlval lv in
      (rest, dolv, Lval(lv'))

  | Const ((CInt _ | CChr _), _) -> (intType, [], e)
  | Const (CReal _, _) -> (doubleType, [], e)
  | CastE (t, e, l) -> begin
      let t' = fixupType t in
      let (et, doe, e') = boxexp e in
      (* Put e into a variable *)
      let doe', rese = castTo e' et t' doe in
      (t', doe', rese)
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
      | (Plus), true, false -> 
          let doset, reslv =
            setFatPointer restyp' 
                          (fun t -> 
                            BinOp(bop, readPtrField e1' et1, e2', t, l))
                          (readBaseField e1' et1) in
          (restyp', doe1 @ doe2 @ doset, Lval(reslv))
      | (Plus), false, true -> 
          let doset, reslv =
            setFatPointer restyp' 
                          (fun t -> 
                            BinOp(bop, e1', readPtrField e2' et2, t, l))
                          (readBaseField e2' et2) in
          (restyp', doe1 @ doe2 @ doset, Lval(reslv))
      | (Minus|Eq|Ne|Le|Lt|Ge|Gt), true, true -> 
          (restyp', doe1 @ doe2, 
           BinOp(bop, readPtrField e1' et1, readPtrField e2' et2, restyp', l))

      | _, false, false -> (restyp', doe1 @ doe2, 
                            BinOp(bop,e1',e2',restyp', l))

      | _, _, _ -> E.s (E.unimp "boxBinOp")
  end

  | SizeOf (t, l) -> 
      let t' = fixupType t in
      (intType, [], SizeOf(t', l))

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
      let (doset, reslv) = 
        setFatPointer ptrtype
          (fun _ -> AddrOf(lv', l))
          (CastE(voidPtrType, baseaddr, lu))
      in
      (ptrtype, dolv @ doset, Lval(reslv))
(*      
      
  | AddrOf ((Var vi, off), l) -> 
      if not vi.vaddrof then 
      let tbase = 
        match vi.vtype with
          TArray(t, _, _) -> t
        | x -> x
      in
      let (rest, dooff, off') = boxoffset off tbase in
      let ptrtype = 
        match rest with
          TArray(t, _, a) -> TPtr(t, a)
        | _ -> TPtr(rest, []) in
      let tres = fixupType ptrtype in
      let (doset, reslv) = 
        setFatPointer tres 
          (fun _ -> AddrOf((Var vi, off'), l))
          (CastE(voidPtrType, 
                 AddrOf((Var vi, NoOffset), l),
                 lu)) in
      (tres, dooff @ doset, Lval(reslv))
        
  | AddrOf ((Mem addr, off), l) -> 
      let (addrt, doaddr, addr', addr'base) = boxexpSplit addr in
      let addrt' = 
        match addrt with
          TPtr(t, _) -> t
        | TArray(t, _, _) -> t
        | _ -> E.s (E.unimp "Reading from a non-pointer type: %a\n"
                      d_plaintype addrt)
      in
      let (rest, dooff, off') = boxoffset off addrt' in
      let ptrtype = 
        match rest with
          TArray(t, _, a) -> TPtr(t, a)
        | _ -> TPtr(rest, []) in
      let tres = fixupType ptrtype in
      let (doset, reslv) = 
        setFatPointer tres 
          (fun _ -> AddrOf((Mem addr', off'), l))
          addr'base in
      (tres, doaddr @ dooff @ doset, Lval(reslv))
        
*)
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
        match lvt with
          TArray(t, _, _) -> fixupType (TPtr(t, [])) 
        | _ -> E.s (E.unimp "StartOf on a non-array")
      in
      let (doset, reslv) = 
        setFatPointer ptrtype
          (fun _ -> AddrOf(addOffset (Index(zero, NoOffset)) lv', lu))
          (CastE(voidPtrType, baseaddr, lu))
      in
      (ptrtype, dolv @ doset, Lval(reslv))

(*
  | StartOf (Var vi, off) -> 
      if not vi.vaddrof then 
        E.s (E.bug "addrof not set for %s" vi.vname);
      let tbase = 
        match vi.vtype with
          TArray(t, _, _) -> t
        | x -> x
      in
      let (rest, dooff, off') = boxoffset off tbase in
      let ptrtype = 
        match rest with
          TArray(t, _, a) -> TPtr(t, a)
        | _ -> E.s (E.bug "StartOf on a non-array")
      in
      let tres = fixupType ptrtype in
      let (doset, reslv) = 
        setFatPointer tres 
          (fun _ -> AddrOf(addOffset (Index(zero,NoOffset)) 
                                     (Var vi, off'), lu))
          (CastE(voidPtrType, 
                 (match vi.vtype with
                   TArray _ -> StartOf(Var vi, NoOffset)
                 | _ -> AddrOf((Var vi, NoOffset), lu)),
                 lu)) in
      (tres, dooff @ doset, Lval(reslv))
      
  | StartOf (Mem addr, off) -> 
      let (addrt, doaddr, addr', addr'base) = boxexpSplit addr in
      let addrt' = 
        match addrt with
          TPtr(t, _) -> t
        | TArray(t, _, _) -> t
        | _ -> E.s (E.unimp "Reading from a non-pointer type: %a\n"
                      d_plaintype addrt)
      in
      let (rest, dooff, off') = boxoffset off addrt' in
      let ptrtype = 
        match rest with
          TArray(t, _, a) -> TPtr(t, a)
        | _ -> E.s (E.bug "StartOf on a non-array")
      in
      let tres = fixupType ptrtype in
      let (doset, reslv) = 
        setFatPointer tres 
          (fun _ -> Lval (Mem addr', off'))
          addr'base 
      in
      (tres, doaddr @ dooff @ doset, Lval(reslv))
*)        
      
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
    in
    let ptre, basee = 
      match e with
        Lval(Var vi, o) -> 
          let po, bo = compOffsets o in
          Lval(Var vi, po), Lval(Var vi, bo)
      | Lval(Mem e'', o) -> 
          let po, bo = compOffsets o in
          Lval(Mem e'', po), Lval(Mem e'', bo)
      | _ -> E.s (E.unimp "split _p field offset")
    in
    (fptr.ftype, ptre, basee)
  else
    (et, e, e)

and castTo e et newt doe = 
(*
  if !currentFunction.svar.vname = "releaseHashes" then
    ignore (E.log "cast %a@!from %a@! to %a\n" 
              d_plainexp e d_plaintype et d_plaintype newt);
*)
  match isFatType et, isFatType newt with
    true, true ->                       (* Cast from struct to struct. Put 
                                         * the expression in a variable first*)
      let caste, tmp = 
        match e with
          Lval tmp -> doe, tmp
        | _ -> 
            let tmp = var (makeTempVar !currentFunction et) in
            (doe @ [mkSet tmp e], tmp)
      in
      (caste, 
       Lval(Mem (CastE(TPtr(newt, []), 
                       AddrOf (tmp, lu), lu)),
            Index(zero, NoOffset)))

  | true, false -> (doe, CastE(newt, readPtrField e et, lu))

  | false, false -> 
      if typeSig et = typeSig newt then 
        (doe, e)
      else
        (doe, CastE(newt, e, lu))

  | false, true -> begin 
(*
      if !currentFunction.svar.vname = "MapHash" then
        ignore (E.log "Casting %a@!from %a@!to %a\n" 
                  d_plainexp e d_plaintype et d_plaintype newt);
*)
      let docast, reslv = setFatPointer newt 
          (fun t -> CastE(t, e, lu))
          (CastE(voidPtrType, zero, lu)) in
      (doe @ docast, Lval(reslv))
  end

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
  theFile := GVar (checkSafeRetFat.svar, None) :: !theFile;
  theFile := GVar (checkSafeRetLean.svar, None) :: !theFile;
  let doGlobal g = 
    match g with
    | GVar (vi, init) as g -> begin
        if debug then
          ignore (E.log "Boxing GVar(%s)\n" vi.vname); 
        if (match vi.vtype with TFun _ -> true | _ -> false) &&
           List.exists (fun x -> x = vi.vname) leaveAlone then begin
             (* Leave alone these functions *)
             theFile := g :: !theFile
        end else begin
          vi.vtype <- fixupType vi.vtype;
          if vi.vstorage <> Extern &&
            (match vi.vtype with TFun _ -> false | _ -> true) then begin
              let sz = intSizeOfNoExc vi.vtype in
              let (tagtype, taginit) = tagTypeInit sz in
              let tagvar = makeGlobalVar ("__tag_of_" ^ vi.vname) tagtype in
              tagvar.vstorage <- Static;
              theFile := GVar (tagvar, Some (taginit)) :: !theFile
            end;
          let init' = 
            match init with
              None -> None
            | Some e -> 
                let (et, doe, e', e'base) = boxexpSplit e in
                if doe <> [] then
                  E.s (E.unimp "Non-pure initializer %a\n"
                         d_exp e);
                if isFatType vi.vtype then
                  Some (Compound(vi.vtype, [ e'; e'base]))
                else
                  Some e'
          in
          theFile := GVar(vi, init') :: !theFile
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
        (* Fixup the types of the remaining locals *)
        List.iter (fun l -> l.vtype <- fixupType l.vtype) f.slocals;
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
      
