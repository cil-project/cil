open Cil

module H = Hashtbl

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
let typeName = function
    Typedef (n, _, _, _) -> 
      let l = String.length n in
      if l >= 7 && String.sub n 0 7 = "struct " then
        "struct_" ^ String.sub n 7 (l - 7)
      else if l >= 6 && String.sub n 0 6 = "union " then
        "union_" ^ String.sub n 7 (l - 6) 
      else n
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
  | TEnum (n, _, _, _) -> 
      if n = "" then makeNewTypeName "enum"
      else "enum_" ^ n
  | TStruct (n, _, _, _) -> 
      if n = "" then makeNewTypeName "struct"
      else "struct_" ^ n
  | TUnion (n, _, _, _) -> 
      if n = "" then makeNewTypeName "union"
      else "union_" ^ n
  | TFun _ -> makeNewTypeName "fun"
  | _ -> makeNewTypeName "type"
      

 (* Define and return a fat pointer type to a given base type *)
                                        (* Keep a mapping from types to their 
                                         * fat pointer *)
let fatPtrTypes : (typ, typ) H.t = H.create 17
let fatPointerType base attrs = 
  try
    H.find fatPtrTypes base             (* Already done for this type *)
  with Not_found -> begin
    let tname = "fatp_" ^ (typeName base) in (* The name *)
    let t = 
      TStruct(tname, [ { fstruct = tname;
                         fname   = "_p";
                         ftype   = TPtr(base, attrs);
                         fattr   = [];
                       };
                       { fstruct = tname;
                         fname   = "_b";
                         ftype   = voidPtrType;
                         fattr   = [];
                       }; ], 0, []) 
    in
    let t' = Typedef(tname, 0, ref t, []) in
    H.add fatPtrTypes base t';
    theFile := GType(tname, t) :: !theFile;
    t'
  end
    

 (* Define and return a tagged type for a given base type *)
                                        (* Keep a mapping from types to their 
                                         * tagged versions *)
let taggedTypes : (typ, typ) H.t = H.create 17
let taggedType base attrs = 
  try
    H.find taggedTypes base
  with Not_found -> begin
    let tname = "tag_" ^ (typeName base) in (* The name *)
    let sz = intSizeOfNoExc base in
    let words = (sz + 3) lsr 2 in
    let tagbytes = (words + 7) lsr 3 in
    let t = 
      TStruct(tname, [ { fstruct = tname;
                         fname   = "_t";
                         ftype   = TArray(charType, 
                                          Some (integer tagbytes), []);
                         fattr   = [];
                       };
                       { fstruct = tname;
                         fname   = "_l";
                         ftype   = intType;
                         fattr   = [];
                       };
                       { fstruct = tname;
                         fname   = "_d";
                         ftype   = base;
                         fattr   = attrs;
                       }; ], 0, []) 
    in
    let t' = Typedef(tname, 0, ref t, []) in
    H.add taggedTypes base t';
    theFile := GType(tname, t) :: !theFile;
    t'
  end



(**** FIXUP TYPE ***)
let rec fixupType t = 
  match t with 
    (TInt _|TEnum _|TFloat _|TVoid _|TBitfield _) -> t
  | TPtr (t', a) -> fatPointerType (fixupType t') a
                                        (* Hopefully all circularities are 
                                         * through a pointer *)
  | Typedef (_, _, rt, _) -> (* rt := fixupType (!rt);*)  t

  | TStruct(n, flds, _, a) -> 
      List.iter (fun fi -> fi.ftype <- fixupType fi.ftype) flds;
      t

  | TUnion (n, flds, _, a) -> 
      List.iter (fun fi -> fi.ftype <- fixupType fi.ftype) flds;
      t

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
    TStruct(_, [p;b], _,_) when p.fname = "_p" && b.fname = "_b" -> true
  | _ -> false

let getPtrFieldOfFat t = 
  match unrollType t with
    TStruct(_, [p;b], _,_) when p.fname = "_p" && b.fname = "_b" -> p
  | _ -> E.s (E.bug "getPtrFieldOfFat %a\n" d_type t)

   (* Test if we must check the return value *)
let mustCheckReturn tret =
  match unrollType tret with
    TPtr _ -> true
  | TArray _ -> true
  | _ -> isFatType tret






(*************************************88
*****************************************
          (* A fat pointer type to be used for returning pointers *)
let fatPointerPtrField = 
  { fstruct = "__fatptr";
    fname   = "ptr";
    ftype   = voidptr; 
    fattr   = [];
  }      
let fatPointerBaseField = 
  { fstruct = "__fatptr";
    fname   = "base";
    ftype   = voidptr; 
    fattr   = []
  }
let fatPointerType = 
  TStruct ("__fatptr",
           [ fatPointerPtrField; fatPointerBaseField; ], 0, [])

                                        (* Apply a boxing transformation to 
                                         * an entire program *)
let boxFile (fl: file) = 
          (* Hash of bases for global variables *)
  let globBaseHash : (int, varinfo) H.t = H.create 17 in
  List.iter (fun vi ->
    if hasBase vi.vtype then 
      let bvi = newGlobalVariable prog (vi.vname ^ "__base") (TPtr TVoid) in
      H.add globBaseHash vi.vid bvi) prog.globalVars;

  (************ FUNCTIONS *******************)
  (* Apply the transformation to a function declaration *)
  let rec boxFunc (fdec : fun_decl) : fundec = 
    (* Hash of bases for local variables *)
    let localBaseHash : (int, varinfo) H.t = H.create 17 in 
    let addBaseVar vi = 
      if hasBase vi.vtype then 
        let bvi = newLocalVariable fdec (vi.vname ^ "__base") (TPtr TVoid) in
        H.add localBaseHash vi.vid bvi 
    in
    (* Create the bases for the local variables *)
    List.iter addBaseVar fdec.fformals;
    List.iter addBaseVar fdec.flocals;

    (* A way to get the base variable, as an lval *)
    let getBaseVar vi = 
      try 
        if vi.isglobal then 
          H.find globBaseHash vi.vid
        else
          H.find localBaseHash vi.vid
      with Not_found -> ignore (bug "I thought this has a companion")
    in

    (* A way to create temporary variables *)
    let newTempVar typ = 
      let vi = newTempVariable fdec typ in
      addBaseVar vi;
      vi
    in

    (************ EXPRESSIONS **************)
    let rec boxexp (e : exp) : expRes = 
      match e with 

                                        (* Reading a memory address *)
      | LVal (Mem(addr, off, l)) -> 
          let (addrt, doaddr, addr', addr'base) = boxexp addr in
          let (rest, dooff, off', off'base) = boxoffset off addrt in
          let newlval = Mem(addr', off', l) in
          let newlvalbase, check = 
            if hasBase rest then
              LVal(Mem(addr', off'base, l)), "CHECK_SAFERDFAT"
            else
              zero, "CHECK_SAFERDLEAN"
          in
          (rest, 
           doaddr @ dooff @ [Call (None, Const (Str check, l),
                                   [ AddrOf(LVal(newlval)); 
                                     addr'base ])],
           LVal(newlval), newlvalbase)

      | UnOp (uop, e, restyp, l) -> 
          let (_, doe, e', _) = boxexp e in
          (* The result is never a pointer *)
          (restyp, doe, UnOp(uop, e', l), zero)

      | BinOp (bop, e1, e2, restyp, l) ->
        let (et1, doe1, e1', e1'base) = boxexp e1 in
        let (et2, doe2, e2', e2'base) = boxexp e2 in
        (restyp, doe1 @ doe2, 
         BinOp(bop, e1', e2', restyp, l),
         if hasBase restyp then
           (* If the result is a pointer then it must be that either e1 or e2 
            * are pointers but not both ! *)
           if hasBase et1 then e1'base else e2'base
         else
           zero)
          
      | CastE (newt, e) -> 
        let (et, doe, e', e'base) = boxexp e in
        begin
          match et, newt with 
            (TInt _|TFloat _|TDouble _|TPtr _|TArray _), 
            (TInt _|TFloat _|TDouble _) -> 
              (newt, doe, CastE(newt, e'), zero)
          | TPtr (tp, _), TPtr (newtp, _) -> 
              (newt, doe, CastE(newt, e'), e'base)
          | (TInt _|TFloat _|TDouble _), TPtr (newt', _) -> 
              (newt, doe, CastE(newt, e'), zero)
          | _ -> E.i (E.unimp "caste")
        end

      | AddrOf (Var(vi,off,l), l') -> 
          let dataField = addressOf vi in
          let (rest, dooff, off', off'base) = boxoffset off vi.vtype in
          (TPtr (rest, []), dooff, 
           AddrOf (Var(vi, Field(dataField, off'), l)), 
           AddrOf (Var(vi, Field(dataField, NoOffset), l)))

      | AddrOf (Mem(e, off, l), l') -> 
          let (et, doe, e', e'base) = boxexp e in
          let (rest, dooff, off', off'base) = boxoffset off et in
          (TPtr (rest, []), doe @ dooff, 
           AddrOf (Mem(e', off', l), l'), 
           e'base)

*)

    (************* STATEMENTS **************)
let rec boxstmt (s : stmt) : stmt = 
  try
    match s with 
      Sequence sl -> mkSeq (List.map boxstmt sl)
          
    | (Label _ | Goto _ | Case _ | Default | Skip | Return None) -> s
          
    | Loop s -> Loop (boxstmt s)
          
    | IfThenElse (e, st, sf) -> 
        let (_, doe, e') = boxexp (CastE(intType, e, lu)) in
          (* We allow casts from pointers to integers here *)
        mkSeq (doe @ [IfThenElse (e', boxstmt st, boxstmt sf)])
          
    | Return (Some e) -> 
        let (_, doe, e') = boxexp e in
        let doe' = 
          match !currentFunction.stype with 
            TFun(tRes, _, _, _) when mustCheckReturn tRes -> 
              let whichcheck = 
                if isFatType tRes then 
                  "CHECK_SAFERETFAT" 
                else 
                  "CHECK_SAFERET"
              in
              doe @ 
                (* check the safety of the access *)
              [call None (Const (CStr whichcheck, lu)) [ e' ]]
          | _ -> doe
        in
        mkSeq (doe' @ [Return (Some e')])
          
    | Instruction (Set(Var(vi, off, l), e, l')) ->
        let (rest, dooff, off') = boxoffset off vi.vtype in
        let (_, doe, e') = boxexp (CastE (rest, e, l)) in
        mkSeq (dooff @ doe @ [Instruction(Set(Var(vi, off', l), e', l'))])
(*        
  | Instruction (Set(Mem(addr, off, l), e, l')) ->
      let (addrt, doaddr, addr') = boxexp addr in
      let (rest, dooff, off') = boxoffset off addrt in
      let newlval = Mem(addr', off', l) in
      let setbase, check = 
        if hasBase rest then
          [Set(Mem(addr',off'base,l), e'base, l')], "CHECK_SAFEWRFAT"
        else
          [], "CHECK_SAFEWRLEAN"
      in
      makeStatement (doaddr @ dooff @ 
                     (Call (None, Const (Str check, l),
                            [ AddrOf(LVal(newlval)); 
                              addr'base;
                              e']) ::
                      Set(newlval, e', l') :: setbase))
        
*)        
    | Instruction (Call(vi, f, args, l)) ->
        let (ft, dof, f') = boxexp f in
        let (ftret, ftargs) =
          match ft with 
            TFun(fret, fargs, _, _) -> (fret, fargs) 
          | _ -> E.s (E.unimp "call of a non-function") 
        in
        let rec doArgs restargs restargst = 
          match restargs, restargst with
            [], [] -> [], []
          | a :: resta, t :: restt -> 
              let (_, doa, a') = boxexp a in
              let (doresta, resta') = doArgs resta restt in
              (doa @ doresta,  a' :: resta')
          | _ -> E.s (E.unimp "vararg")
        in
        let (doargs, args') = doArgs args ftargs in
        mkSeq (dof @ doargs @ [call vi f' args'])
          
    | s -> E.s (E.unimp "boxstmt: %a\n" d_stmt s)
  with e -> begin
    ignore (E.log "boxstmt (%s)\n" (Printexc.to_string e));
    Instruction(Asm(["booo statement"], false, [], [], []))
  end

and boxoffset (off : offset) (basety : typ) : offsetRes = 
  match off with 
    NoOffset -> 
      if isFatType basety then
        let fi = getPtrFieldOfFat basety in
        (fi.ftype, [], Field (getPtrFieldOfFat basety, NoOffset))
      else
        (basety, [], NoOffset)
             
  | Index (e, resto) ->
      let rest =                    (* The result type *)
        match basety with
          TPtr (t, _) -> t
        | TArray (t, _, _) -> t
        | _ -> E.s (E.bug "Index")
      in
      let (_, doe, e') = boxexp e in
      let (rest', doresto, off') = boxoffset resto rest in
      (rest', doe @ doresto, Index(e', off'))
        
  | Field (fi, resto) ->
      let (rest, doresto, off') = boxoffset resto fi.ftype in
      (rest, doresto, Field(fi, off'))
        
  | _ -> begin
      ignore (E.log "boxoffset\n");
      (charPtrType, [], NoOffset) 
  end 
        

and boxexp (e : exp) : expRes = 
  match e with
  | Lval (Var(vi, off, l)) ->       (* Reading a variable *)
      let (rest, dooff, off') = boxoffset off vi.vtype in
      (rest, dooff, Lval(Var(vi, off', l)))

  | Const ((CInt _ | CChr _), _) -> (intType, [], e)
  | Const (CReal _, _) -> (doubleType, [], e)
(*
  | Const (CStr s, l) -> 
          (* Add a tag to the string *)
      let l = 1 + String.length s in 
      let wrds = (l + 3) lsr 2 in   (* Number of words *)
      let tagbytes = wrds lsr 2 in  (* Tag bytes *)
      let tag = String.create (tagbytes + 4 + l) in
          (* Zero out the tag *)
      String.fill tag 0 tagbytes (Char.chr 0);
          (* Write the length *)
      String.set tag (tagbytes + 0) (Char.chr ((wrds)       land 255));
      String.set tag (tagbytes + 1) (Char.chr ((wrds lsr  8) land 255));
      String.set tag (tagbytes + 2) (Char.chr ((wrds lsr  16) land 255));
      String.set tag (tagbytes + 3) (Char.chr ((wrds lsr  24) land 255));
          (* Copy the original string *)
      String.blit s 0 tag (tagbytes + 4) l;
      let t1 = newTempVar fatStringType in
      let t1ptr = Var(t1, Field(fatStringPtrField, NoOffset), l) in
      let t1base = Var(t1, Field(fatStringBaseField, NoOffset), l) in
      (charPtrType, 
       [ mkSet t1ptr (mkString tag) ;
         mkSet t1ptr 
              AddrOf (Var(t1, Field(fatStringPtrField, 
                                    Index(Const(Int (tagbytes + 4), l),
                                          NoOffset)))), l) ;
         Set (t1base, LVar(t1ptr), l)],
       LVal(t1ptr), LVal(t1base))
*)
  | _ -> begin
      ignore (E.log "boxexp: %a\n" d_exp e);
      (charPtrType, [], mkString "booo expression")
  end

let boxFile globals =
  let doGlobal = function
    | GVar (vi, init) as g -> 
        let newType = 
          let fixtype = fixupType vi.vtype in
          match fixtype with
            TFun _ -> fixtype           (* No tagging on prototypes *)
          | _ -> taggedType fixtype []
        in
        vi.vtype <- newType;
        theFile := g :: !theFile
    | GType (n, t) as g -> 
        theFile := GType (n, fixupType t) :: !theFile

    | GFun f -> 
        (* Fixup the types of locals *)
        List.iter (fun l -> l.vtype <- fixupType l.vtype) f.slocals;
        (* Fixup the return type as well *)
        f.stype <- fixupType f.stype;
        currentFunction := f;           (* so that maxid and locals can be 
                                         * updated in place *)
        f.sbody <- boxstmt f.sbody;
        theFile := GFun f :: !theFile

    | (GAsm s) as g -> theFile := g :: !theFile
  in
  List.iter doGlobal globals;
  List.rev (!theFile)
      
