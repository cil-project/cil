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

let leaveAlone = 
  ["printf"; "fprintf"; "sprintf"; "snprintf";
   "_CrtDbgReport"]


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


    (* For each new type name, keep track of various versions, usually due 
     * to varying attributes *)
let typeNames : (string, int ref) H.t = H.create 17

let rec newTypeName prefix t = 
  let n = prefix ^ (baseTypeName t) in
  try
    let r = H.find typeNames n in
    incr r;
    n ^ (string_of_int !r)
  with Not_found -> 
    H.add typeNames n (ref 0);
    n
 (* Make a type name, for use in type defs *)
and baseTypeName = function
    TForward (comp, _)  -> baseTypeName (TComp comp)
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
      if String.sub n 0 1 = "@" then "enum"
      else "enum_" ^ n
  | TComp comp -> 
      let su = if comp.cstruct then "struct" else "union" in
      if String.sub comp.cname 0 1 = "@" then su
      else su ^ "_" ^ comp.cname
  | TFun _ -> "fun"
  | _ -> "type"


(**** Make new string names *)
let stringId = ref 0 
let newStringName () = 
  incr stringId;
  "__string" ^ (string_of_int !stringId)

(**** FIXUP TYPE ***)
let fixedTypes : (typsig, typ) H.t = H.create 17


(***** Convert all pointers in types for fat pointers ************)
let rec fixupType t = 
  match t with
    TForward _ -> t
  | TNamed (n, t, a) -> TNamed(n, fixupType t, a) (* Keep the Named types *)

    (* Sometimes we find a function type without arguments or with arguments 
     * with different names (a prototype that we have done before). Do the 
     * regular fixit and then put the argument names back.  *)
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

      | TPtr (t', a) -> begin
        (* Now do the base type *)
          let fixed' = fixupType t' in
          let tname  = newTypeName "fatp_" fixed' in (* The name *)
          let fixed = 
            TComp 
              (mkCompInfo true tname 
                 (fun _ -> [ ("_p", TPtr(fixed', a), []); 
                             ("_b", voidPtrType, [])]) 
                 [])
          in
          let tres = TNamed(tname, fixed, []) in
          H.add fixedTypes (typeSig fixed) fixed; (* We add fixed ourselves. 
                                                   * The TNamed will be added 
                                                   * after doit  *)
          H.add fixedTypes (typeSig (TPtr(fixed',a))) fixed;
          theFile := GType(tname, fixed) :: !theFile;
          tres
      end
            
      | TForward _ ->  t              (* Don't follow TForward, since these 
                                       * fill be taken care of when the 
                                       * definition is encountered  *)
      | TNamed (n, t', a) -> TNamed (n, fixupType t', a)
          
      | TComp comp -> 
          (* Change the fields in place, so that everybody sees the change *)
          List.iter (fun fi -> fi.ftype <- fixupType fi.ftype) comp.cfields;
          t
            
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
    TComp comp when comp.cstruct -> begin
      match comp.cfields with 
        [p;b] when p.fname = "_p" && b.fname = "_b" -> true
      | _ -> false
    end
  | _ -> false

let getPtrFieldOfFat t : fieldinfo = 
  match unrollType t with
    TComp comp when comp.cstruct -> begin
      match comp.cfields with 
        [p;b] when p.fname = "_p" && b.fname = "_b" -> p
      | _ -> E.s (E.bug "getPtrFieldOfFat %a\n" d_type t)
    end
  | _ -> E.s (E.bug "getPtrFieldOfFat %a\n" d_type t)

let getBaseFieldOfFat t : fieldinfo  = 
  match unrollType t with
    TComp comp when comp.cstruct -> begin
      match comp.cfields with 
        [p;b] when p.fname = "_p" && b.fname = "_b" -> b
      | _ -> E.s (E.bug "getBaseFieldOfFat %a\n" d_type t)
    end
  | _ -> E.s (E.bug "getBaseFieldOfFat %a\n" d_type t)

let rec readPtrBaseField (e: exp) et =     
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
      
let readPtrField (e: exp) (t: typ) : exp = 
  let (tptr, ptr, base) = readPtrBaseField e t in ptr
      
let readBaseField (e: exp) (t: typ) : exp = 
  let (tptr, ptr, base) = readPtrBaseField e t in base

let fromPtrToBase e : exp = 
  let rec replacePtrBase = function
      Field(fip, NoOffset) when fip.fname = "_p" ->
        (* Find the fat type that this belongs to *)
        let bfield = getBaseFieldOfFat (TComp(fip.fcomp)) in
        Field(bfield, NoOffset)

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
    TComp comp -> begin
      match comp.cfields with
        [p;b] when comp.cstruct && p.fname = "_p" && b.fname = "_b" -> true
      | _ -> List.exists (fun f -> typeHasChanged f.ftype) comp.cfields
    end
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
    | TComp comp -> 
        List.exists (fun f -> containsArray f.ftype) comp.cfields
    | TPtr _ -> false
    | (TInt _ | TEnum _ | TFloat _ | TBitfield _ ) -> false
    | _ -> E.s (E.unimp "containsArray: %a" d_plaintype t)
  in
  if v.vglob then 
    match v.vtype with 
      TFun _ -> false 
    | _ -> true
  else v.vaddrof || containsArray v.vtype
    
    
(* We need to avoid generating multiple copies of the same tagged type 
 * because we run into trouble if a variable is defined twice (once with 
 * extern). *)
             
let taggedTypes: (typsig, typ) H.t = H.create 123

let tagType (t: typ) : typ = 
  let tsig = typeSig t in
  try
    H.find taggedTypes tsig
  with Not_found -> begin
    let newtype = 
      try
        let bytes = (bitsSizeOf t) lsl 3 in (* Might raise not-found for 
                                             * incomplete types  *)
        let words = (bytes + 3) lsr 2 in
        let tagwords = (words + 15) lsr 4 in
        TComp 
          (mkCompInfo true ""
             (fun _ -> 
               [ ("_len", uintType, []);
                 ("_data", t, []);
                 ("_tags", TArray(intType, 
                                  Some (integer tagwords), []), []);
               ])
             [])
      with Not_found -> begin (* An incomplete type *)
        TComp 
          (mkCompInfo true ""
             (fun _ -> 
               [ ("_len", uintType, []);
                 ("_data", t, []); ]) [])
      end
    in
    let tname = newTypeName "_tagged_" t in
    let named = TNamed (tname, newtype, []) in
    theFile := GType (tname, newtype) :: !theFile;
    H.add taggedTypes tsig named;
    H.add taggedTypes (typeSig named) named;
    named
  end

(* Create a compound initializer for a tagged type *)
let splitTagType tagged = 
  (* Get the data field, the length field, and a tag field *)
  let dfld, lfld, tfld = 
    match unrollType tagged with
      TComp comp -> begin
        match comp.cfields with 
          [lfld; dfld; tfld] -> dfld, lfld, tfld
        | _ -> E.s (E.bug "splitTagType. No tags: %a\n" d_plaintype tagged)
      end
    | _ -> E.s (E.bug "splitTagType. No tags: %a\n" d_plaintype tagged)
  in
  let bytes = 
    try (bitsSizeOf dfld.ftype) lsr 3
    with _ -> E.s (E.bug "splitTagType: incomplete type") in
  let words = (bytes + 3) lsr 2 in
  let tagwords = (words + 15) lsr 4 in
            (* Now create the tag initializer *)
  dfld, lfld, tfld, words, tagwords

let makeTagCompoundInit tagged datainit = 
  let dfld, lfld, tfld, words, tagwords = splitTagType tagged in
  let rec loopTags idx = 
    if idx >= tagwords then [] else
    (None, zero) :: loopTags (idx + 1)
  in
  Compound (tagged, 
                  (* Now the length *)
            (None, Const(CInt(words, IUInt, None), lu)) ::
            (match datainit with 
              None -> []
            | Some e -> [(None, e)]))
            (* Leave the rest alone since it will be initializer with 0 *)
    ,
  dfld


let makeTagAssignInit tagged vi = 
  let dfld, lfld, tfld, words, tagwords = splitTagType tagged in
  let rec loopTags idx = 
    if idx >= tagwords then 
      [mkSet (Var vi, Field(lfld, NoOffset)) 
          (Const(CInt(words, IUInt, None), lu))] 
    else
      (mkSet (Var vi, Field(tfld, First (Index (integer idx, NoOffset)))) zero)
      :: loopTags (idx + 1)
  in
  loopTags 0

(****** the CHECKERS ****)
let fatVoidPtr = fixupType voidPtrType
let castVoidStar e = doCast e (typeOf e) voidPtrType

let checkSafeRetFatFun = 
  let fdec = emptyFunction "CHECK_SAFERETFAT" in
  let arg  = makeLocalVar fdec "x" fatVoidPtr in
  fdec.svar.vtype <- TFun(voidType, [ arg ], false, []);
  fdec
    
let checkSafeFatLeanCastFun = 
  let fdec = emptyFunction "CHECK_SAFEFATLEANCAST" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec

let checkFunctionPointer = 
  let fdec = emptyFunction "CHECK_FUNCTIONPOINTER" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fun whatp whatb -> 
    call None (Lval(var fdec.svar)) [ castVoidStar whatp; 
                                      castVoidStar whatb]
  
let checkFetchLength = 
  let fdec = emptyFunction "CHECK_FETCHLENGTH" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(uintType, [ argb ], false, []);
  fun tmplen base -> 
    call (Some tmplen) (Lval (var fdec.svar))
      [ castVoidStar base ]

let checkFetchTagStart = 
  let fdec = emptyFunction "CHECK_FETCHTAGSTART" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argl  = makeLocalVar fdec "l" uintType in
  fdec.svar.vtype <- TFun(voidPtrType, [ argb; argl ], false, []);
  fun tmplen base len -> 
    call (Some tmplen) (Lval (var fdec.svar))
      [ castVoidStar base; 
        len ]

(* Since we cannot take the address of a bitfield we treat accesses to a 
 * bitfield like an access to the entire host that contains it (for the 
 * purpose of checking). This is only Ok if the host does not contain pointer 
 * fields *)
let getHostIfBitfield lv t = 
  match unrollType t with
    TBitfield (ik, wd, a) -> begin
      let lvbase, lvoff = lv in
      let rec getHost = function
          Field(fi, NoOffset) -> NoOffset
        | Field(fi, off) -> Field(fi, getHost off)
        | Index(e, off) -> Index(e, getHost off)
        | First(off) -> First(getHost off)
        | NoOffset -> E.s (E.bug "a TBitfield that is not a bitfield")
      in
      let lv' = lvbase, getHost lvoff in
      let lv't = typeOfLval lv' in
      (match unrollType lv't with 
        TComp comp when comp.cstruct -> 
          if List.exists (fun f -> typeHasChanged f.ftype) comp.cfields then
            E.s (E.unimp "%s contains both bitfields and pointers" 
                   (compFullName comp))
      | _ -> E.s (E.bug "getHost: bitfield not in a struct"));
      lv', lv't
    end
  | _ -> lv, t

let checkBounds = 
  let fdec = emptyFunction "CHECK_CHECKBOUNDS" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argl  = makeLocalVar fdec "l" uintType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argl; argp; argpl ], false, []);
  fun tmplen base lv t ->
    let lv', lv't = getHostIfBitfield lv t in
    call None (Lval (var fdec.svar))
      [ castVoidStar base; 
        tmplen; 
        castVoidStar (AddrOf(lv', lu));
        SizeOf(lv't, lu) ]

  
let checkZeroTags = 
  let fdec = emptyFunction "CHECK_ZEROTAGS" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  let argt  = makeLocalVar fdec "t" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argp; argpl; argt ], false, []);
  fun base tagStart lv t ->
    let lv', lv't = getHostIfBitfield lv t in
    call None (Lval (var fdec.svar))
      [ castVoidStar base; 
        castVoidStar (AddrOf(lv', lu)); 
        SizeOf(lv't, lu); 
        castVoidStar tagStart ]
  
let doCheckFat which arg argt = 
  (* Take the argument and break it apart *)
  let (_, ptr, base) = readPtrBaseField arg argt in 
  call None (Lval(var which.svar)) [ castVoidStar ptr; 
                                     castVoidStar base; ]

let doCheckLean which arg  = 
  call None (Lval(var which.svar)) [ castVoidStar arg; ]


(* Check a read *)
let checkFatPointerRead = 
  let fdec = emptyFunction "CHECK_FATPOINTERREAD" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argt  = makeLocalVar fdec "tags" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argp; argt ], false, []);
  
  fun base where tagstart -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar base; castVoidStar where; castVoidStar tagstart]

let checkFatPointerWrite = 
  let fdec = emptyFunction "CHECK_FATPOINTERWRITE" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argwb  = makeLocalVar fdec "wb" voidPtrType in
  let argwp  = makeLocalVar fdec "wp" voidPtrType in
  let argt  = makeLocalVar fdec "tags" voidPtrType in
  fdec.svar.vtype <- 
     TFun(voidType, [ argb; argp; argwb; argwp; argt ], false, []);
  
  fun base where whatbase whatp tagstart -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar base; 
        castVoidStar where; 
        castVoidStar whatbase; castVoidStar whatp; castVoidStar tagstart]
  
  
let checkMem (towrite: exp option) 
             (lv: lval) (base: exp) (t: typ) : stmt list = 
  (* Fetch the length field in a temp variable *)
  let len = makeTempVar !currentFunction ~name:"_tlen" uintType in
  let lenExp = Lval(var len) in
  (* And the start of tags in another temp variable *)
  let tagStart = makeTempVar !currentFunction ~name:"_ttags" voidPtrType in
  let tagStartExp = Lval(var tagStart) in
  (* Now the tag checking. We only care about pointers. We keep track of what 
   * we write in each field and we check pointers in a special way. *)
  let rec doCheckTags (towrite: exp option) (where: lval) t acc = 
    match unrollType t with 
    | (TInt _ | TFloat _ | TEnum _ | TBitfield _ ) -> acc
    | TComp comp 
        when (comp.cstruct && 
              (match comp.cfields with 
                [p;b] when p.fname = "_p" && b.fname = "_b" -> true
        | _ -> false)) -> begin (* A fat pointer *)
            match towrite with
              None -> (* a read *)
                (checkFatPointerRead base 
                   (AddrOf(where, lu)) tagStartExp) :: acc
            | Some towrite -> (* a write *)
                let _, whatp, whatb = readPtrBaseField towrite t in
                (checkFatPointerWrite base (AddrOf(where, lu)) 
                   whatb whatp tagStartExp) :: acc
        end 
    | TComp comp when comp.cstruct -> 
        let doOneField acc fi = 
          let newwhere = addOffset (Field(fi, NoOffset)) where in
          let newtowrite = 
            match towrite with 
              None -> None
            | Some (Lval whatlv) -> 
                Some (Lval (addOffset (Field(fi, NoOffset)) whatlv))
            | _ -> E.s (E.unimp "doCheckTags")
          in
          doCheckTags newtowrite newwhere fi.ftype acc
        in
        List.fold_left doOneField acc comp.cfields
    | TArray(bt, _, a) 
        when (match unrollType bt with
          (TInt _ | TFloat _ | TEnum _ | TBitfield _ ) -> true | _ -> false) 
      -> acc

(*            
    | TFun _ when towrite = None -> acc
*)
    | _ -> E.s (E.unimp "unexpected type in doCheckTags: %a\n" d_type t)
  in
  let zeroTags = 
    match towrite with 
      None -> Skip
    | Some _ -> checkZeroTags base tagStartExp lv t
  in
  (checkFetchLength len base) ::
  (checkBounds lenExp base lv t) ::
  (checkFetchTagStart tagStart base lenExp) ::
  zeroTags ::
  (doCheckTags towrite lv t [])
          
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
              doe @ [doCheckFat checkSafeRetFatFun e' et]
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
          | Var vi, off when mustBeTagged vi -> checkWrite e' lv' lvbase rest
          | _ -> []
        in
        mkSeq (dolv @ doe @ check @ [Instr(Set(lv', e', l))])

    | Call(vi, f, args, l) ->
        let (ft, dof, f') = boxfunctionexp f in
        let (ftret, ftargs, isva) =
          match ft with 
            TFun(fret, fargs, isva, _) -> (fret, fargs, isva) 
          | _ -> E.s (E.unimp "call of a non-function: %a @!: %a" 
                        d_plainexp f' d_plaintype ft) 
        in
        let leavealone = 
          match f' with
            Lval(Var vf, NoOffset) 
              when List.exists (fun s -> s = vf.vname) leaveAlone -> true
          | _ -> false 
        in
        let (doargs, args') =
          if leavealone then (* We leave some functions alone. But we check 
                              * all arguments and, if needed we pack the 
                              * result  *)
            let rec doArgs = function
              | [] -> ([], [])
              | a :: resta -> 
                  let (at, doa, a') = boxexp a in
                  let (doresta, resta') = doArgs resta in
                  let (checka, a'') = 
                    if isFatType at then 
                      ([doCheckFat checkSafeFatLeanCastFun a' at],
                       readPtrField a' at)
                    else ([], a')
                  in
                  (doa @ checka @ doresta, a'' :: resta')
            in
            doArgs args
          else
            let rec doArgs restargs restargst = (* The types of functions 
                                                 * have been fixed already  *) 
              match restargs, restargst with
                [], [] -> [], []
              | a :: resta, t :: restt -> 
                  let (doa, fa') = boxexpf a in
                  let (doa', fa'') = castTo fa' t.vtype doa in
                  let (_, doa'', a2) = fexp2exp fa'' doa' in
                  let (doresta, resta') = doArgs resta restt in
                (doa'' @ doresta,  a2 :: resta')
              | a :: resta, [] when isva -> 
                  let (doa, fa') = boxexpf a in
                  let (_, doa'', a2) = fexp2exp fa' doa in
                  let (doresta, resta') = doArgs resta [] in
                  (doa'' @ doresta, a2 :: resta')
              | _ -> E.s (E.unimp "vararg in call to %a" d_exp f)
            in
            doArgs args ftargs  
        in
        (* Maybe the result is tagged *)
        let vi', setvi = 
          match vi with
            None -> vi, []
          | Some vi -> begin
              match boxlval (Var vi, NoOffset) with
                (_, _, (Var _, NoOffset), _) -> Some vi, []
              | (tv, [], ((Var _, Field(dfld, NoOffset)) as newlv), _) -> 
                  let tmp = makeTempVar !currentFunction dfld.ftype in
                  Some tmp, [boxinstr (Set((Var vi, NoOffset), 
                                           Lval (var tmp), lu))]
              | _ -> E.s (E.bug "Result of call is not a variable")
          end
        in
        mkSeq (dof @ doargs @ ((call vi' f' args') :: setvi))

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
                TComp comp when comp.cstruct -> begin
                  match comp.cfields with 
                    _ :: df :: _ when df.fname = "_data" -> df
                  | _ -> E.s (E.bug "addrOf but no tagged type")
                end 
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
          match unrollType addrt with
            TPtr(t, _) -> t
          | _ -> E.s (E.unimp "Mem but no pointer type: %a@!addr= %a@!"
                        d_plaintype addrt d_plainexp addr)
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
      (* The type of fi has been changed already *)
      let (rest, doresto, off') = boxoffset resto fi.ftype in
      (rest, doresto, Field(fi, off'))
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
  try
    match e with
    | Lval (lv) -> 
        let rest, dolv, lv', lvbase = boxlval lv in
        let check = (* Check a read if it is in memory of if it comes from a 
                     * tagged variable *)
          match lv' with
            Mem _, _ -> checkRead lv' lvbase rest
          | Var vi, off when mustBeTagged vi -> checkRead lv' lvbase rest
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
        let newt = tagType (TArray(charType, Some (integer l), [])) in
        let gvar = makeGlobalVar (newStringName ()) newt in
        gvar.vstorage <- Static;
      (* Build an initializer *)
        let varinit, dfield = 
          makeTagCompoundInit newt (Some (Const(CStr s, cloc))) in
        theFile := GVar (gvar, Some varinit) :: !theFile;
        let fatChrPtrType = fixupType charPtrType in
        let result = Lval(Var gvar, Field(dfield, NoOffset)) in
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
        | (PlusPI|MinusPI), true, false -> 
            let ptype = (getPtrFieldOfFat et1).ftype in
            (doe1 @ doe2, F2 (restyp', 
                              BinOp(bop, readPtrField e1' et1, e2', ptype, l),
                              readBaseField e1' et1))
        | (MinusPP|EqP|NeP|LeP|LtP|GeP|GtP), true, true -> 
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
          
   (* Intercept references of _iob. A major hack !!!!! *)
    | AddrOf ((Var vi,
               First(Index(Const(CInt _, _) as n, NoOffset))) as lv, 
              _) when !msvcMode && vi.vname = "_iob_fp_" 
      -> 
        let (lvt, _, _, _) = boxlval lv in  (* Just to get the type*)
        let tres = fixupType (TPtr(lvt, [])) in
        let tmp1 = makeTempVar !currentFunction fatVoidPtr in
        let tmp2 = makeTempVar !currentFunction tres in
        let seq  = 
          [ boxstmt (call (Some tmp1) (Lval(var getIOBFunction.svar)) [ n ]);
            boxstmt (assign tmp2 (CastE(tres, Lval(var tmp1), lu))) ] in
        (seq, F1(tres, Lval(var tmp2)))
          
    | AddrOf (lv, l) ->
        let (lvt, dolv, lv', baseaddr) = boxlval lv in
      (* Check that variables whose address is taken are flagged as such, or 
       * are globals *)
        (match lv' with
          (Var vi, _) when not vi.vaddrof && not vi.vglob -> 
            E.s (E.bug "addrof not set for %s (addrof)" vi.vname)
        | _ -> ());
      (* The result type. *)
        let ptrtype = 
(*          match lvt with
            TArray(t, _, _) -> fixupType (TPtr(t, [])) 
          | _ -> *) fixupType (TPtr(lvt, []))
        in
        (dolv, F2 (ptrtype, AddrOf(lv', l), baseaddr))
          
    (* StartOf is like an AddrOf except for typing issues. *)
    | StartOf lv -> 
        let (lvt, dolv, lv', baseaddr) = boxlval lv in
      (* Check that variables whose address is taken are flagged *)
        (match lv' with
          (Var vi, _) when not vi.vaddrof && not vi.vglob -> 
            E.s (E.bug "addrof not set for %s (startof)" vi.vname)
        | _ -> ());
      (* The result type. *)
        let ptrtype, res = 
          match unrollType lvt with
            TArray(t, _, _) -> 
              fixupType (TPtr(t, [])),
              AddrOf(addOffset (First(Index(zero, NoOffset))) lv', lu) 
          | TFun _ -> 
              fixupType (TPtr(lvt, [])),
              StartOf lv'
          | _ -> E.s (E.unimp "StartOf on a non-array and non-function: %a"
                        d_plaintype lvt)
        in
        (dolv, F2 (ptrtype, res, baseaddr))

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
          
    | Compound (t, initl) as t' -> 
        let t' = fixupType t in
        let doOneInit (oo, ei) = (oo, boxGlobalInit ei) in
        ([], L(t', Compound(t', List.map doOneInit initl)))
  with exc -> begin
    ignore (E.log "boxexpf (%s)\n" (Printexc.to_string exc));
    ([], L(charPtrType, dExp (dprintf "booo_exp: %a" d_exp e)))
  end 
            
          
and boxGlobalInit e = 
  let et = fixupType (typeOf e) in
  let (e't, doe, e', e'base) = boxexpSplit e in
  if doe <> [] then
    E.s (E.unimp "Non-pure initializer %a\n"  d_exp e);
  if isFatType et then
    Compound(et, [ (None, e'); (None, e'base)])
  else
    e'

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


and boxfunctionexp (f : exp) = 
  match f with
    Lval(Var vi, NoOffset) -> boxexp f
  | Lval(Mem base, NoOffset) -> 
      let rest, dolv, lv', lvbase = boxlval (Mem base, NoOffset) in
      (rest, dolv @ [checkFunctionPointer (AddrOf(lv', lu)) lvbase], 
       Lval(lv'))
      
  | _ -> E.s (E.unimp "Unexpected function expression")

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
      Field(fip, NoOffset) when fip.fname = "_p" ->
        (* Find the fat type that this belongs to *)
        let bfield = getBaseFieldOfFat (TComp(fip.fcomp)) in
        Field(bfield, NoOffset)

    | Field(f', o) -> Field(f',replacePtrBase o)
    | _ -> E.s (E.unimp "Cannot find the _p field to replace in %a\n"
                  d_plainexp e)
  in
  match e with
    Lval (b, off) -> Lval(b, replacePtrBase off)
  | _ -> E.s (E.unimp "replacing _p with _b in a non-lval")


let fixupGlobName vi = 
  if vi.vglob && vi.vstorage <> Static && typeHasChanged vi.vtype &&
    not (List.exists (fun la -> la = vi.vname) leaveAlone) then
    let nlen = String.length vi.vname in
    if nlen <= 4 || String.sub vi.vname (nlen - 4) 4 <> "_fp_" then
      vi.vname <- vi.vname ^ "_fp_"

let boxFile globals =
  ignore (E.log "Boxing file\n");
  let rec doGlobal g = 
    match g with
                                        (* We ought to look at pragmas to see 
                                         * if they talk about alignment of 
                                         * structure fields *)
    | GPragma s -> theFile := g :: !theFile
    | GDecl vi as g -> boxglobal vi false None
    | GVar (vi, init) -> boxglobal vi true init
    | GType (n, t) as g -> 
        if debug then
          ignore (E.log "Boxing GType(%s)\n" n);
        let tnew = fixupType t in (* Do this first *)
        theFile := GType (n, tnew) :: !theFile

    | GFun f -> 
        if debug then
          ignore (E.log "Boxing GFun(%s)\n" f.svar.vname);
        (* Fixup the return type as well, except if it is a vararg *)
        f.svar.vtype <- fixupType f.svar.vtype;
          (* If the type has changed and this is a global function then we
           * also change its name *)
        fixupGlobName f.svar;
        (* Fixup the types of the locals  *)
        List.iter (fun l -> l.vtype <- fixupType l.vtype) f.slocals;
        (* We fix the formals only if the function is not vararg *)
        List.iter (fun l -> l.vtype <- fixupType l.vtype) f.sformals;
        currentFunction := f;           (* so that maxid and locals can be 
                                         * updated in place *)
        (* Check that we do not take the address of a formal. If we actually 
         * do then we must make that formal a true local and create another 
         * formal *)
        let newformals, newbody = 
          let rec loopFormals = function
              [] -> [], [f.sbody]
            | form :: restf -> 
                let r1, r2 = loopFormals restf in
                if form.vaddrof then begin
                  let tmp = makeTempVar f form.vtype in 
                    (* Now take it out of the locals and replace it with the 
                       * current formal. It is not worth optimizing this one *)
                  f.slocals <- 
                     form :: 
                     (List.filter (fun x -> x.vid <> tmp.vid) f.slocals);
                    (* Now replace form with the temporary in the formals *)
                  tmp :: r1, (mkSet (var form) (Lval(var tmp)) :: r2)
                end else
                  form :: r1, r2
          in
          loopFormals f.sformals
        in
        f.sformals <- newformals;
        f.sbody <- mkSeq newbody;
        (* Now we must take all locals whose address is taken and turn their 
           types into structures with tags and length field *)
        let tagLocal stmacc l = 
          if not (mustBeTagged l) then stmacc
          else
            let newtype = tagType l.vtype in
            l.vtype <- newtype;
            (makeTagAssignInit newtype l) @ stmacc
        in
        let inilocals = List.fold_left tagLocal [] f.slocals in
        f.sbody <- mkSeq (inilocals @ [boxstmt f.sbody]);
        theFile := GFun f :: !theFile

    | (GAsm s) as g -> theFile := g :: !theFile

  and boxglobal vi isdef init = 
    if debug then
      ignore (E.log "Boxing GVar(%s)\n" vi.vname); 
        (* Leave alone some functions *)
    if not (List.exists (fun s -> s = vi.vname) leaveAlone) then
      vi.vtype <- fixupType vi.vtype;
          (* If the type has changed and this is a global variable then we
           * also change its name *)
    fixupGlobName vi;
      (* Tag all globals, except function prototypes *)
    if not (mustBeTagged vi) then
      if isdef then
        theFile := GVar(vi, init) :: !theFile
      else
        theFile := GDecl vi :: !theFile
    else if not isdef && vi.vstorage <> Extern then
      theFile := GDecl vi :: !theFile
    else begin
          (* Make the initializer *)
          (* tag the type, but don't change it yet *)
      let newtyp = tagType vi.vtype in
          (* Add it to the tag initializer *)
      let varinit = 
        if vi.vstorage = Extern then None 
        else
              (* prepare the data initializer. *)
          let init' = 
            match init with
              None -> None
            | Some e -> Some (boxGlobalInit e)
          in
          let (x, _) = makeTagCompoundInit newtyp init' in
          Some x
      in
      vi.vtype <- newtyp;
      theFile := GVar(vi, varinit) :: !theFile
    end
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
      







