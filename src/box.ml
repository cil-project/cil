open Cil
open Pretty 

module H = Hashtbl
module E = Errormsg
module P = Ptrnode

let debugType = false
let debug = false

let checkReturn = true

let allAreWild = false

let interceptCasts = ref false  (* If true it will insert calls to 
                                 * __scalar2pointer when casting scalars to 
                                 * pointers.  *)

let lu = locUnknown

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
    L  of typ * P.pointerkind * exp     (* A one-word expression of a given 
                                         * kind (P.Scalar or PSafe) and type *)
  | F1 of typ * P.pointerkind * exp    (* A two-word expression that is 
                                         * already converted to an expression 
                                         * *)
  | F2 of typ * P.pointerkind * exp * exp(* A two-word expression of a given 
                                         * (fat) type made out of two lean 
                                         * expression (the pointer and the 
                                         * base)  *)
  | FC of typ * P.pointerkind * typ * P.pointerkind * exp               
                                        (* A two-word expression that is a 
                                         * cast of to a fat type from another 
                                         * fat type. The inner expression is 
                                         * an F1 whose type and value are 
                                         * given  *)


let leaveAlone = 
  ["printf"; "fprintf"; "sprintf"; "snprintf"; "sscanf"; "_snprintf";
   "_CrtDbgReport"; ]


            (* Same for offsets *)
type offsetRes = 
    typ * stmt list * offset * exp * P.pointerkind


(*** Helpers *)            

let prefix p s = 
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p

let rec isZero = function
    Const(CInt(0, _, _), _) -> true
  | CastE(_, e, _) -> isZero e
  | _ -> false

let rec isInteger = function
  | Const(CInt _, _) -> true
  | CastE(_, e, _) -> isInteger e
  | _ -> false

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


(**** Inspect the boxing style attribute *)
let extractPointerTypeAttribute al = 
  let pkind = ref P.Unknown in
  let rec loop = function
      [] -> []
    | a :: al -> begin
        match a with
          AId("safe") -> pkind := P.Safe; al
        | AId("wild") -> pkind := P.Wild; al
        | AId("index") -> pkind := P.Index; al
        | AId("seq") -> pkind := P.FSeq; al
        | _ -> a :: loop al
    end
  in
  let res = loop al in
  if !pkind = P.Unknown then (!pkind, al) else (!pkind, res)
          

let kindOfType t = 
  let pkind, _ = extractPointerTypeAttribute (typeAttrs t) in
  if pkind = P.Unknown then
    P.Scalar
  else
    pkind


let extractArrayTypeAttribute al = 
  let sized = ref false in
  let rec loop = function
      [] -> []
    | a :: al -> begin
        match a with
          AId("sized") -> sized := true; al
        | _ -> a :: loop al
    end
  in
  let res = loop al in
  if !sized then (true, res) else (false, al)

(**** Make new string names *)
let stringId = ref 0 
let newStringName () = 
  incr stringId;
  "__string" ^ (string_of_int !stringId)



(* Since we cannot take the address of a bitfield we will do bounds checking 
 * and tag zeroeing for an access to a bitfield as if the access were to the 
 * entire struct that contains the field. But this might lead to problems if 
 * the same struct contains both pointers and bitfields. In that case we 
 * coalesce all consecutive bitfields into a substructure *)

(* For each bitfield that was coalesced we map (the id of the host and the 
 * field name) to the fieldinfo for the host and the fieldinfo inside the 
 * host *)
let hostsOfBitfields : (int * string, fieldinfo * fieldinfo) H.t = H.create 17
let bundleid = ref 0
let bitfieldCompinfo comp = 
  let containsPointer t = 
    existsType (function TPtr _ -> ExistsTrue | _ -> ExistsMaybe) t in
  if comp.cstruct &&
    List.exists (fun fi -> 
      match fi.ftype with 
        TBitfield _ -> true | _ -> false) comp.cfields &&
    List.exists (fun fi -> containsPointer fi.ftype) comp.cfields 
  then begin
    (* Go over the fields and collect consecutive bitfields *)
    let rec loopFields prev prevbits = function
        [] -> List.rev (bundleBitfields prevbits prev)
      | f :: rest -> begin
          match f.ftype with
            TBitfield _ -> loopFields prev (f :: prevbits) rest
          | _ -> loopFields (f :: (bundleBitfields prevbits prev)) [] rest
      end
    and bundleBitfields bitfields prev = 
      if bitfields = [] then prev else
      let bname = "bits_" ^ (string_of_int !bundleid) in
      incr bundleid;
      let bitfields = List.rev bitfields in
      let bundle = 
        mkCompInfo true bname
          (fun _ -> 
            List.map (fun f -> f.fname, f.ftype, f.fattr) bitfields) [] 
      in
      let bfinfo = 
        { fname = bname; ftype = TComp bundle; fattr = []; fcomp = comp } in
      (* Go over the previous bitfields and add them to the host map *)
      List.iter2 (fun oldbf newbf -> 
        H.add hostsOfBitfields (comp.ckey, oldbf.fname) (bfinfo, newbf))
        bitfields
        bundle.cfields;
      bfinfo :: prev
    in
    comp.cfields <- List.rev (loopFields [] [] comp.cfields)
  end

(* Change the field accesses to take into accound the extra structs *)
let doField (fi: fieldinfo) (off: offset) =
  if off = NoOffset then
    try
      let host, this = H.find hostsOfBitfields (fi.fcomp.ckey, fi.fname) in
      Field (host, Field (this, NoOffset))
    with Not_found ->
      Field (fi, off)
  else
    Field (fi, off)

(***** Convert some pointers in types to fat pointers ************)
let sizedArrayTypes : (typsig, typ) H.t = H.create 123
(* We need to avoid generating multiple copies of the same tagged type 
 * because we run into trouble if a variable is defined twice (once with 
 * extern). *)             
let taggedTypes: (typsig, typ) H.t = H.create 123
(**** FIXUP TYPE ***)
let fixedTypes : (typsig, typ) H.t = H.create 17

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
               List.map2 (fun a a' -> {a' with vname = a.vname;}) args args',
               isva, dropAttribute a (ACons("__format__",[])))
      | _ -> E.s (E.bug "fixupType")
  end
  | _ -> fixit t

and fixit t = 
  let ts = typeSig t in
  try
    H.find fixedTypes ts 
  with Not_found -> begin
    let fixed = 
      match t with 
        (TInt _|TEnum _|TFloat _|TVoid _|TBitfield _) -> t

      | TPtr (t', a) -> begin
          (* Now do the base type *)
          let fixed' = fixupType t' in
          (* Extract the boxing style attribute *)
          let pkind, newa = extractPointerTypeAttribute a in
          let fixed = 
            match pkind with
              (P.Safe|P.Unknown) -> TPtr(fixed', a)
            | (P.Wild|P.Index) -> 
                let tname  = newTypeName "fatp_" fixed' in (* The name *)
                let fixed = 
                  TComp 
                    (mkCompInfo true tname 
                       (fun _ -> [ ("_p", TPtr(fixed', newa), []); 
                                   ("_b", voidPtrType, [])]) 
                       [])
                in
                let pattr = 
                  match pkind with P.Wild -> AId("wild") | _ -> AId("index") in
                theFile := GType(tname, fixed) :: !theFile;
                TNamed(tname, fixed, [pattr])

            | P.FSeq -> 
                let tname  = newTypeName "seq_" fixed' in (* The name *)
                let fixed = 
                  TComp 
                    (mkCompInfo true tname 
                       (fun _ -> [ ("_p", TPtr(fixed', newa), []); 
                                   ("_b", uintType, [])]) 
                       [])
                in
                theFile := GType(tname, fixed) :: !theFile;
                TNamed(tname, fixed, [AId("fseq")])

            | _ -> E.s (E.unimp "Don't know how to fix a %a" 
                          P.d_pointerkind pkind)
          in
          (* We add fixed ourselves. The TNamed will be added after doit  *)
          H.add fixedTypes (typeSig fixed) fixed; 
          H.add fixedTypes (typeSig (TPtr(fixed', a))) fixed;
          fixed
      end
            
      | TForward _ ->  t              (* Don't follow TForward, since these 
                                       * fill be taken care of when the 
                                       * definition is encountered  *)
      | TNamed (n, t', a) -> TNamed (n, fixupType t', a)
          
      | TComp comp -> 
          (* Change the fields in place, so that everybody sees the change *)
          List.iter 
            (fun fi -> 
              let newa, newt = moveAttrsFromDataToType  fi.fattr fi.ftype in
              fi.fattr <- newa;
              fi.ftype <- fixupType newt) 
            comp.cfields;
          bitfieldCompinfo comp;
          t
            
      | TArray(t', l, a) -> 
          let sized, newa = extractArrayTypeAttribute a in
          let newarray = TArray(fixupType t', l, newa) in
          if sized then
            addArraySize newarray
          else
            newarray
            
            
      | TFun(rt,args,isva,a) ->
          let args' = 
            List.map
              (fun argvi -> {argvi with vtype = fixupType argvi.vtype}) args in
          let res = TFun(fixupType rt, args', isva, a) in
          res
    in
    H.add fixedTypes ts fixed;
    H.add fixedTypes (typeSig fixed) fixed;
    fixed
  end

and moveAttrsFromDataToType attrs typ = 
  let mustMove = function
      AId("sized") -> true
    | _ -> false
  in
  match List.filter mustMove attrs with
  | [] -> attrs, typ
  | tomove -> List.filter (fun a -> not (mustMove a)) attrs,
              typeAddAttributes tomove typ

(****** Generate sized arrays *)
and addArraySize t = 
  let tsig = typeSig t in
  try
    H.find sizedArrayTypes tsig
  with Not_found -> begin
	(* GCC does not like fields to have incomplete types *)
    let complt = 
      if isCompleteType t then t 
      else begin
        match unrollType t with
	  TArray(bt, None, a) -> TArray(bt, Some zero, a)
        | TComp ci when ci.cfields = [] -> TArray(charType, Some zero, [])
        | _ -> 
            E.s (E.unimp "Don't know how to tag incomplete type %a" 
                   d_plaintype t)
      end
    in
    let newtype = 
      TComp 
        (mkCompInfo true ""
           (fun _ -> 
             [ ("_size", uintType, []);
               ("_array", complt, []); ]) [])
    in
    let tname = newTypeName "_sized_" t in
    let named = TNamed (tname, newtype, [AId("sized")]) in
    theFile := GType (tname, newtype) :: !theFile;
    H.add sizedArrayTypes tsig named;
    H.add sizedArrayTypes (typeSig named) named;
    named
  end
  
and tagType (t: typ) : typ = 
  let tsig = typeSig t in
  try
    H.find taggedTypes tsig
  with Not_found -> begin
    let newtype = 
      if isCompleteType t then 
        (* ignore (E.log "Type %a -> bytes=%d, words=%d, tagwords=%d\n"
                  d_type t bytes words tagwords); *)
        let _, tagWords = tagLength t in
        TComp 
          (mkCompInfo true ""
             (fun _ -> 
               [ ("_len", uintType, []);
                 ("_data", t, []);
                 ("_tags", TArray(intType, 
                                  Some tagWords, []), []);
               ])
             [])
      else begin (* An incomplete type *)
	(* GCC does not like fields to have incomplete types *)
	let complt = 
	  match unrollType t with
	    TArray(bt, None, a) -> TArray(bt, Some zero, a)
	  | TComp ci when ci.cfields = [] -> TArray(charType, Some zero, [])
	  | _ -> E.s (E.unimp "Don't know how to tag incomplete type %a" 
                        d_plaintype t)
	in
        TComp 
          (mkCompInfo true ""
             (fun _ -> 
               [ ("_len", uintType, []);
                 ("_data", complt, []); ]) [])
      end
    in
    let tname = newTypeName "_tagged_" t in
    let named = TNamed (tname, newtype, []) in
    theFile := GType (tname, newtype) :: !theFile;
    H.add taggedTypes tsig named;
    H.add taggedTypes (typeSig named) named;
    named
  end

and tagLength (t: typ) : (exp * exp) = (* Call only for a isCompleteType *)
(*        let bytes = (bitsSizeOf t) lsr 3 in
          let words = (bytes + 3) lsr 2 in
          let tagwords = (words + 15) lsr 4 in
*)
  (* First the number of words *)
  BinOp(Shiftrt, 
        BinOp(PlusA, 
              SizeOf(t, lu),
              kinteger IUInt 3, uintType, lu),
        integer 2, uintType, lu),
  (* Now the number of tag words. At 1 tag bit/ word we can fit the tags for 
   * 128 bytes into one tag word. *)
  BinOp(Shiftrt, 
        BinOp(PlusA, 
              SizeOf(t, lu),
              kinteger IUInt 127, uintType, lu),
        integer 7, uintType, lu)
    
    
(**** We know in what function we are ****)
let currentFunction : fundec ref  = ref dummyFunDec
let currentFile     : file ref = ref dummyFile
let currentFileId     = ref 0

let isFatComp (comp: compinfo) = 
  (comp.cstruct && 
   (match comp.cfields with 
     [p;b] when p.fname = "_p" && b.fname = "_b" -> true
   | _ -> false))


    (* Test if a type is FAT *)
let isFatType t = 
  match unrollType t with
    TComp comp when isFatComp comp -> true 
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
      | Compound (t, [_, p; _, b]) when isFatType t -> 
          p, b
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



   (* Test if we must check the return value *)
let mustCheckReturn tret =
  checkReturn &&
  match unrollType tret with
    TPtr _ -> true
  | TArray _ -> true
  | _ -> isFatType tret


   (* Test if we have changed the type *)
let rec typeContainsFats t =
  existsType 
    (function TComp comp -> 
      begin
        match comp.cfields with
          [p;b] when comp.cstruct && p.fname = "_p" && b.fname = "_b" -> 
            ExistsTrue
        | _ -> ExistsMaybe
      end 
      | _ -> ExistsMaybe)
    t

(* Create tags for types along with the newly created fields and initializers 
 * for tags and for the length  *)
(* Check whether the type contains an embedded array *)
let mustBeTagged v = 
  if allAreWild then
    let rec containsArray t =
      existsType 
        (function 
            TArray _ -> ExistsTrue 
          | TPtr _ -> ExistsFalse
          | _ -> ExistsMaybe) t
    in
    if v.vglob then 
      match v.vtype with 
        TFun _ -> false (* Do not tag functions!! Mainly because we don't know 
                        * how to put the tag. Plus, function pointers should 
                        * have a length = 0 so we cannot write there *)
      | _ -> 
          if v.vstorage = Static then v.vaddrof || containsArray v.vtype
          else true  (* We tag all externals because we might 
                        take their address somewhere else *)
    else v.vaddrof || containsArray v.vtype
  else
    (filterAttributes "tagged" v.vattr) <> []


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
  let words, tagwords = tagLength dfld.ftype in
            (* Now create the tag initializer *)
  dfld, lfld, tfld, words, tagwords

let makeTagCompoundInit tagged datainit = 
  let dfld, lfld, tfld, words, _ = splitTagType tagged in
  Compound (tagged, 
                  (* Now the length *)
            (None, words) ::
            (match datainit with 
              None -> []
            | Some e -> [(None, e)]))
            (* Leave the rest alone since it will be initialized with 0 *)
    ,
  dfld


(* Generates code that initializes vi. Needs "iter", an integer variable to 
 * be used as a for loop index *)
let makeTagAssignInit (iter: varinfo) vi : stmt list = 
  let dfld, lfld, tfld, words, tagwords = splitTagType vi.vtype in
  (* Write the length *)
  mkSet (Var vi, Field(lfld, NoOffset)) words ::
  (* And the loop *)
  mkForIncr iter zero tagwords one 
    [mkSet (Var vi, Field(tfld, Index (Lval(var iter), NoOffset))) 
        zero ]
  ::
  []

(****** the CHECKERS ****)
let fatVoidPtr     = fixupType voidPtrType
let castVoidStar e = doCast e (typeOf e) voidPtrType

let checkSafeRetFatFun = 
  let fdec = emptyFunction "CHECK_SAFERETFAT" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  fdec
    
let checkIndexFun = 
  let fdec = emptyFunction "CHECK_INDEX" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argl  = makeLocalVar fdec "l" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb; argl ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  fdec
  
    
let checkSafeFatLeanCastFun = 
  let fdec = emptyFunction "CHECK_SAFEFATLEANCAST" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  fdec

let checkFunctionPointer = 
  let fdec = emptyFunction "CHECK_FUNCTIONPOINTER" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  theFile := GDecl fdec.svar :: !theFile;
  fdec.svar.vstorage <- Static;
  fun whatp whatb -> 
    call None (Lval(var fdec.svar)) [ castVoidStar whatp; 
                                      castVoidStar whatb]
  
let checkFetchLength = 
  let fdec = emptyFunction "CHECK_FETCHLENGTH" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(uintType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  fun tmplen base -> 
    let ptr = 
      let rec replaceBasePtr = function
          Field(fip, NoOffset) when fip.fname = "_b" ->
             (* Find the fat type that this belongs to *)
            let pfield = getPtrFieldOfFat (TComp(fip.fcomp)) in
            Field(pfield, NoOffset)

        | Field(f', o) -> Field(f',replaceBasePtr o)
        | _ -> raise Not_found
      in
      match base with
        Lval (b, off) -> 
          begin try Lval(b, replaceBasePtr off) with Not_found -> base end
      | _ -> base
    in
    call (Some tmplen) (Lval (var fdec.svar))
      [ castVoidStar ptr; 
        castVoidStar base ]

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
(*
        | Index(e, off) -> Index(e, getHost off)
        | First(off) -> First(getHost off)
*)
        | NoOffset -> E.s (E.bug "a TBitfield that is not a bitfield")
      in
      let lv' = lvbase, getHost lvoff in
      let lv't = typeOfLval lv' in
      (match unrollType lv't with 
        TComp comp when comp.cstruct -> 
          if List.exists (fun f -> typeContainsFats f.ftype) comp.cfields then
            E.s (E.unimp "%s contains both bitfields and pointers" 
                   (compFullName comp))
      | _ -> E.s (E.bug "getHost: bitfield not in a struct"));
      lv', lv't
    end
  | _ -> lv, t

(* Pass a thunk for computing the length expression *)
type checkWhat = 
    CheckNull of exp
  | CheckBounds
  | CheckNothing

let checkBounds : (unit -> exp) -> exp -> lval -> typ 
                  -> P.pointerkind -> stmt = 
  let fdec = emptyFunction "CHECK_CHECKBOUNDS" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argl  = makeLocalVar fdec "l" uintType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argl; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  let checkBoundsFun = fdec in

  let fdec = emptyFunction "CHECK_NULL" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  let checkNullFun = fdec in

  (* And now the null check *)
  fun mktmplen base lv lvt pkind ->
    let lv', lv't = getHostIfBitfield lv lvt in
    (* Do not check the bounds when we access variables without array 
     * indexing  *)
    let mustCheck =
      match pkind with
        (P.Wild|P.Index) -> CheckBounds
      | P.Safe -> begin
          match lv' with
            Mem addr, _ -> CheckNull addr
          | _, _ -> CheckNothing
      end
      | _ -> E.s (E.bug "Unexpected pointer kind in checkBounds")
    in
    match mustCheck with
      CheckNothing -> Skip
    | CheckNull addr ->
        call None (Lval (var checkNullFun.svar)) [ castVoidStar addr ]
    | CheckBounds -> 
        call None (Lval (var checkBoundsFun.svar))
          [ castVoidStar base; 
            mktmplen (); 
            castVoidStar (AddrOf(lv', lu));
            SizeOf(lv't, lu) ]

(* Compute the offset of first scalar field in a thing to be written. Raises 
 * Not_found if there is no scalar *)
let offsetOfFirstScalar (t: typ) : exp = 
  let rec theOffset sofar t = 
    match unrollType t with
      (TInt _ | TFloat _ | TEnum _) -> Some sofar
    | TPtr _ -> None
    | TComp comp when isFatComp comp -> None
    | TComp comp when comp.cstruct -> begin
        let containsBitfield = ref false in
        let doOneField acc fi = 
          match acc, fi.ftype with
            None, TBitfield _ -> containsBitfield := true; None
          | None, _ -> 
              theOffset (addOffset (Field(fi, NoOffset)) sofar) fi.ftype
          | Some _, _ -> acc
        in
        List.fold_left doOneField None comp.cfields
    end
    | _ -> E.s (E.unimp "offsetOfFirstScalar")
  in
  match theOffset NoOffset t with
    None -> raise Not_found
  | Some NoOffset -> CastE(uintType, zero, lu)
  | Some off -> 
      let scalar = Mem (doCast zero intType (TPtr (t, []))), off in
      let addrof = mkAddrOf scalar in
      CastE(uintType, addrof, lu)

  
let checkZeroTags = 
  let fdec = emptyFunction "CHECK_ZEROTAGS" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argbl = makeLocalVar fdec "bl" uintType in
  let argp  = makeLocalVar fdec "p" charPtrType in
  let argsize  = makeLocalVar fdec "size" uintType in
  let offset  = makeLocalVar fdec "offset" uintType in
  fdec.svar.vtype <- 
     TFun(voidType, [ argb; argbl; argp; argsize; offset ], false, []);
  theFile := GDecl fdec.svar :: !theFile;
  fdec.svar.vstorage <- Static;
  fun base lenExp lv t ->
    let lv', lv't = getHostIfBitfield lv t in
    try
      let offexp = offsetOfFirstScalar lv't in
      call None (Lval (var fdec.svar))
        [ castVoidStar base; lenExp ;
          castVoidStar (AddrOf(lv', lu)); 
          SizeOf(lv't, lu); offexp ] 
    with Not_found -> 
      Skip
  
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
  let arglen  = makeLocalVar fdec "nrWords" uintType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argb; arglen; argp; ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  
  fun base where len -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar base; len; castVoidStar where]

let checkFatPointerWrite = 
  let fdec = emptyFunction "CHECK_FATPOINTERWRITE" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let arglen  = makeLocalVar fdec "nrWords" uintType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argwb  = makeLocalVar fdec "wb" voidPtrType in
  let argwp  = makeLocalVar fdec "wp" voidPtrType in
  fdec.svar.vtype <- 
     TFun(voidType, [ argb; arglen; argp; argwb; argwp; ], false, []);
  theFile := GDecl fdec.svar :: !theFile;
  fdec.svar.vstorage <- Static;
  
  fun base where whatbase whatp len -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar base; len; 
        castVoidStar where; 
        castVoidStar whatbase; castVoidStar whatp;]
  
  
let checkMem (towrite: exp option) 
             (lv: lval) (base: exp) 
             (lvt: typ) (pkind: P.pointerkind) : stmt list = 
  (* Fetch the length field in a temp variable. But do not create the 
   * variable until certain that it is needed *)
  let lenExp : exp option ref = ref None in
  let getLenExp = 
    fun () -> begin
      match !lenExp with
        Some x -> x
      | None -> begin
          let len = makeTempVar !currentFunction ~name:"_tlen" uintType in
          let x = Lval(var len) in
          lenExp := Some x;
          x
      end
    end
  in
  let getVarOfExp e = 
    match e with
      Lval(Var vi, NoOffset) -> vi
    | _ -> E.s (E.bug "getLen");
  in
  (* Now the tag checking. We only care about pointers. We keep track of what 
   * we write in each field and we check pointers in a special way. *)
  let rec doCheckTags (towrite: exp option) (where: lval) t acc = 
    match unrollType t with 
    | (TInt _ | TFloat _ | TEnum _ | TBitfield _ ) -> acc
    | TComp comp when isFatComp comp -> begin (* A fat pointer *)
        match towrite with
          None -> (* a read *)
            (checkFatPointerRead base 
               (AddrOf(where, lu)) (getLenExp ())) :: acc
        | Some towrite -> (* a write *)
            let _, whatp, whatb = readPtrBaseField towrite t in
            (checkFatPointerWrite base (AddrOf(where, lu)) 
               whatb whatp (getLenExp ())) :: acc
    end 
    | TComp comp when comp.cstruct -> 
        let doOneField acc fi = 
          let newwhere = addOffsetLval (Field(fi, NoOffset)) where in
          let newtowrite = 
            match towrite with 
              None -> None
            | Some (Lval whatlv) -> 
                Some (Lval (addOffsetLval (Field(fi, NoOffset)) whatlv))
                  (* sometimes in Asm outputs we pretend that we write 0 *)
            | Some (Const(CInt(0, _, _), _)) -> None
            | Some e -> E.s (E.unimp "doCheckTags (%a)" d_exp e)
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
  (* See first what we need in order to check tags *)
  let zeroAndCheckTags = 
    if pkind = P.Wild then
      let zeroTags = 
        match towrite with 
          None -> Skip
        | Some _ -> checkZeroTags base (getLenExp ()) lv lvt
      in
      zeroTags :: 
      (doCheckTags towrite lv lvt [])
    else
      []
  in
  (* Now see if we need to do bounds checking *)
  let checkb = (checkBounds getLenExp base lv lvt pkind) :: zeroAndCheckTags in
  (* See if we need to generate the length *)
  (match !lenExp with
    None -> checkb
  | Some _ -> 
      (checkFetchLength (getVarOfExp (getLenExp ())) base) :: checkb)
  
  
          
    (* Check a write *)
let checkRead = checkMem None
let checkWrite e = checkMem (Some e)

(* A major hack for MSVC *)
let getIOBFunction = 
  let fdec = emptyFunction "__get_iob_fp" in
  let argn = makeLocalVar fdec "n" intType in
  fdec.svar.vtype <- TFun(fatVoidPtr, [ argn ], false, []);
  theFile := GDecl fdec.svar :: !theFile;
  fdec

(* A run-time function to coerce scalars into pointers. Scans the heap and 
 * (in the future the stack) *)
let interceptId = ref 0
let interceptCastFunction = 
  let fdec = emptyFunction "__scalar2pointer" in
  let argl = makeLocalVar fdec "l" ulongType in
  let argf = makeLocalVar fdec "fid" intType in
  let argid = makeLocalVar fdec "lid" intType in
  fdec.svar.vtype <- TFun(voidPtrType, [ argl; argf; argid ], false, []);
  theFile := GDecl fdec.svar :: !theFile;
  fdec

(* Check if an offset contains a non-zero index *)
let rec containsIndex = function
    NoOffset -> false
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
            doe @ [doCheckFat checkSafeRetFatFun e' et]
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
        let (lvt, lvkind, lv', lvbase, dolv) = boxlval lv in
        let (et, doe, e') = boxexp e in (* Assume et is the same as lvt *)
        let check = 
          match lv' with
            Mem _, _ -> 
              checkWrite e' lv' lvbase lvt lvkind
          | Var vi, off when mustBeTagged vi -> 
              checkWrite e' lv' lvbase lvt lvkind
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
                  let (doa', fa'') = 
                    castTo fa' t.vtype doa in
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
                (_, _, (Var _, NoOffset), _, _) -> Some vi, []
              | (tv, _, ((Var _, Field(dfld, NoOffset)) as newlv), _, []) -> 
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
              let (lvt, lvkind, lv', lvbase, dolv) = boxlval lv in
              let check = 
                match lv' with
                  Mem _, _ -> checkWrite (integer 0) lv' lvbase lvt lvkind
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

(* Given an lvalue, generate all the stuff needed to construct a pointer to 
 * it: a base type and a pointer type kind, an lvalue whose address makes the 
 * first component of the pointer and an exp to be used as the second 
 * component (for pointer kinds other than Safe). We also compute a list of 
 * statements that must be executed to check the bounds. *)
and boxlval (b, off) : (typ * P.pointerkind * lval * exp * stmt list) = 

  (* As we go along the offset we keep track of the basetype and the pointer 
   * kind, along with the current base expression and a function that can be 
   * used to recreate the lval. *)
  let (btype, pkind, mklval, base, stmts) as startinput = 
    match b with
      Var vi -> 
          vi.vtype, P.Safe, (fun o -> (Var vi, o)), zero, []
    | Mem addr -> 
        let (addrt, doaddr, addr', addr'base) = boxexpSplit addr in
        let addrt', pkind = 
          match unrollType addrt with
            TPtr(t, a) -> t, kindOfType addrt
          | _ -> E.s (E.unimp "Mem but no pointer type: %a@!addr= %a@!"
                        d_plaintype addrt d_plainexp addr)
        in
        (addrt', pkind, (fun o -> (Mem addr', o)), addr'base, doaddr)
  in
  (* Check index when we switch from Index to Safe *)
  let toSafe ((btype, pkind, mklval, base, stmts) as input) = 
    match pkind with
      P.Wild -> input (* No change if we are in a tagged area *)
    | P.Safe -> input (* No change if already safe *)
    | P.Index -> 
        (btype, P.Safe, mklval, zero, 
         stmts @ [call None (Lval (var checkIndexFun.svar))
                     [ castVoidStar (mkAddrOf (mklval NoOffset));
                       castVoidStar base;
                       SizeOf (btype, lu)]])
    | _ -> E.s (E.unimp "toSafe on unexpected pointer kind")
  in
  (* As we go along we need to go into tagged and sized types. Never call 
   * this on an Index pointer *)
  let goIntoTypes ((btype, pkind, mklval, base, stmts) as input) = 
    if pkind = P.Index then
      E.s (E.bug "goIntoTypes is called on an Index pointer");
    match 
      (match unrollType btype with
        TComp comp when comp.cstruct -> comp.cfields
      | _ -> []) with
      f1 :: f2 :: [] when (f1.fname = "_size" && f2.fname = "_array") -> begin
        (* A sized array *)
        if pkind = P.Wild then
          E.s (E.bug "Sized array in tagged area");
        (f2.ftype, P.Index, (fun o -> mklval (Field(f2, o))), 
         mkAddrOf (mklval(Field(f2,NoOffset))), stmts)
      end
    | f1 :: f2 :: _ when (f1.fname = "_len" && f2.fname = "_data") -> begin
        (* A tagged data. Only wild pointers inside *)
        if pkind = P.Wild then
          E.s (E.bug "Tagged data inside a tagged area");
        (f2.ftype, P.Wild, (fun o -> mklval (Field(f2, o))),
          mkAddrOf (mklval(Field(f2,NoOffset))), stmts)
    end 
    | _ -> input
  in
  (* Now do the offsets *)
  let rec doOffset ((btype, pkind, mklval, base, stmts) as input) = function
      NoOffset -> input

    | Field (f, resto) -> 
        let (_, pkind, mklval, base, stmts) = toSafe input in
        (* Prepare for the rest of the offset *)
        let next = 
          (f.ftype, pkind, (fun o -> mklval (Field(f, o))), base, stmts) in
        let next' = goIntoTypes next in
        doOffset next' resto

    | Index (e, resto) -> 
        if pkind != P.Index then
          E.s (E.bug "Expecting P.Index for Index");
        (* Do the index *)
        let (_, doe, e') = boxexp e in
        (* Grab the result type *)
        let elemtype = 
          match unrollType btype with
            TArray(x, _, _) -> x
          | _ -> E.s (E.bug "Expecting array in doOffset:Index")
        in
        (* Prepare for the rest of the offset *)
        let next = 
          (elemtype, pkind, (fun o -> mklval (Index(e', o))), base, stmts) in
        (* Now switch to Safe and check the index *)
        let next' = toSafe next in
        (* Now go into the tagged types if necessary *)
        goIntoTypes next'
  in
  let start' = goIntoTypes startinput in
  let (btype, pkind, mklval, base, stmts) = doOffset start' off in
  (btype, pkind, mklval NoOffset, base, stmts)
      
    (* Box an expression and return the fexp version of the result. If you do 
     * not care about an fexp, you can call the wrapper boxexp *)
and boxexpf (e: exp) : stmt list * fexp = 
  try
    match e with
    | Lval (lv) -> 
        let lvt, lvkind, lv', baseaddr, dolv = boxlval lv in
        let check = (* Check a read if it is in memory of if it comes from a 
                     * tagged variable *)
          match lv' with
            Mem _, _ -> 
              checkRead lv' baseaddr lvt lvkind
          | Var vi, off when mustBeTagged vi -> 
              checkRead lv' baseaddr lvt lvkind
          | _, _ -> []
        in
        if isFatType lvt then
          (dolv @ check, F1(lvt, kindOfType lvt, Lval(lv')))
        else
          (dolv @ check, L(lvt, kindOfType lvt, Lval(lv')))
            
    | Const (CInt (_, ik, _), _) -> ([], L(TInt(ik, []), P.Scalar, e))
    | Const ((CChr _), _) -> ([], L(charType, P.Scalar, e))
    | Const (CReal (_, fk, _), _) -> ([], L(TFloat(fk, []), P.Scalar, e))
    | CastE (t, e, l) -> begin
        let t' = fixupType t in
        let (doe, fe') = boxexpf e in
      (* Put e into a variable *)
        castTo fe' t' doe
    end
    | Const (CStr s, cloc) -> 
       (* Make a global variable that stores this one, so that we can attach 
        * a tag to it  *)
        let l = 1 + String.length s in 
        let newt = tagType (TArray(charType, Some (integer l), [])) in
        let gvar = makeGlobalVar (newStringName ()) newt in
        gvar.vstorage <- Static;
        (* Build an initializer *)
        let varinit, dfield = 
          makeTagCompoundInit newt (Some (Const(CStr s, cloc))) in
        theFile := GVar (gvar, Some varinit) :: !theFile;
        let fatChrPtrType = fixupType charPtrType in
        let result = StartOf (Var gvar, Field(dfield, NoOffset)) in
        ([], F2 (fatChrPtrType, P.Wild,
                 result, 
                 doCast result (typeOf result) voidPtrType))
          
          
    | UnOp (uop, e, restyp, l) -> 
        let restyp' = fixupType restyp in
        let (et, doe, e') = boxexp e in
        assert (not (isFatType restyp'));
          (* The result is never a pointer *)
        (doe, L(restyp', P.Scalar, UnOp(uop, e', restyp', l)))
          
    | BinOp (bop, e1, e2, restyp, l) -> begin
        let restyp' = fixupType restyp in
        let (et1, doe1, e1') = boxexp e1 in
        let (et2, doe2, e2') = boxexp e2 in
        match bop, isFatType et1, isFatType et2 with
        | (PlusPI|MinusPI), true, false -> 
            let ptype = (getPtrFieldOfFat et1).ftype in
            (doe1 @ doe2, F2 (restyp', P.Wild,
                              BinOp(bop, readPtrField e1' et1, e2', ptype, l),
                              readBaseField e1' et1))
        | (MinusPP|EqP|NeP|LeP|LtP|GeP|GtP), true, true -> 
            (doe1 @ doe2, 
             L(restyp', P.Scalar,
               BinOp(bop, readPtrField e1' et1, 
                     readPtrField e2' et2, restyp', l)))
              
        | _, false, false -> 
            (doe1 @ doe2, L(restyp', P.Scalar, BinOp(bop,e1',e2',restyp', l)))
              
        | _, _, _ -> E.s (E.unimp "boxBinOp: %a@!et1=%a@!et2=%a@!" 
                            d_binop bop d_plaintype et1 d_plaintype et2)
    end
          
    | SizeOf (t, l) -> 
        let t' = fixupType t in
        ([], L(intType, P.Scalar, SizeOf(t', l)))
          
   (* Intercept references of _iob. A major hack !!!!! *)
    | AddrOf ((Var vi,
               Index(Const(CInt _, _) as n, NoOffset)) as lv, 
              _) when !msvcMode && vi.vname = "_iob_fp_" 
      -> 
        let (lvt, _, _, _, _) = boxlval lv in  (* Just to get the type*)
        let tres = fixupType (TPtr(lvt, [])) in
        let tmp1 = makeTempVar !currentFunction fatVoidPtr in
        let tmp2 = makeTempVar !currentFunction tres in
        let seq  = 
          [ boxstmt (call (Some tmp1) (Lval(var getIOBFunction.svar)) [ n ]);
            boxstmt (assign tmp2 (CastE(tres, Lval(var tmp1), lu))) ] in
        (seq, F1(tres, P.Wild, Lval(var tmp2)))
          
    | AddrOf (lv, l) ->
        let (lvt, lvkind, lv', baseaddr, dolv) = boxlval lv in
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
        (dolv, F2 (ptrtype, P.Wild, AddrOf(lv', l), baseaddr))
          
    (* StartOf is like an AddrOf except for typing issues. *)
    | StartOf lv -> 
        let (lvt, lvkind, lv', baseaddr, dolv) = boxlval lv in
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
              AddrOf(addOffsetLval (Index(zero, NoOffset)) lv', lu) 
          | TFun _ -> 
              fixupType (TPtr(lvt, [])),
              StartOf lv'
          | _ -> E.s (E.unimp "StartOf on a non-array and non-function: %a"
                        d_plaintype lvt)
        in
        (dolv, F2 (ptrtype, P.Wild, res, baseaddr))

    | Question (e1, e2, e3, l) ->       
        let (_, doe1, e1') = boxexp (CastE(intType, e1, lu)) in
        let (et2, doe2, e2') = boxexp e2 in
        let (et3, doe3, e3') = boxexp e3 in
        let result = 
          if isFatType et2 then 
            F1 (et2, P.Wild, Question (e1', e2', e3', l))
          else
            L  (et2, P.Scalar, Question (e1', e2', e3', l))
        in
        (doe1 @ doe2 @ doe3, result)
          
    | Compound (t, initl) as t' -> 
        let t' = fixupType t in
        (* Construct a new initializer list *)
        let doOneInit (off: offset) (ei: exp) (tei: typ) acc = 
          (None, boxGlobalInit ei tei) :: acc
        in
        let newinitl = List.rev (foldLeftCompound doOneInit t initl []) in
        ([], L(t', P.Scalar, Compound(t', newinitl)))
  with exc -> begin
    ignore (E.log "boxexpf (%s)\n" (Printexc.to_string exc));
    ([], L(charPtrType, P.Wild, dExp (dprintf "booo_exp: %a" d_exp e)))
  end 
            
          
and boxGlobalInit e et = 
  let et' = fixupType et in
  let (e't, doe, e', e'base) = boxexpSplit e in
  if doe <> [] then
    E.s (E.unimp "Non-pure initializer %a\n"  d_exp e);
  let comptype = 
  match unrollType et' with
    TComp comp when comp.cstruct -> begin
      match comp.cfields with 
        [p;b] when p.fname = "_p" && b.fname = "_b" -> Some et'
      | l :: d :: _ when l.fname = "_len" && d.fname = "_data" ->
          if isFatType d.ftype then Some d.ftype else None
      | _ -> None
    end
  | _ -> None
  in
  match comptype with
    None -> e'
  | Some ct -> 
      Compound(ct, [ (None, e'); (None, 
                                  doCast e'base (typeOf e'base) voidPtrType)])

and fexp2exp (fe: fexp) (doe: stmt list) : expRes = 
  match fe with
    L (t, pk, e') -> (t, doe, e')       (* Done *)
  | F1 (t, pk, e') -> (t, doe, e')      (* Done *)
  | F2 (ft, pk, ep, eb) -> 
      let (doset, lv) = setFatPointer ft (fun _ -> ep) eb in
      (ft, doe @ doset, Lval(lv))
  | FC (newt, pk, oldt, _, e') -> 
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
            NoOffset))


    (* Box an expression and resolve the fexp into statements *)
and boxexp (e : exp) : expRes = 
  let (doe, fe) = boxexpf e in
  fexp2exp fe doe

and boxexpSplit (e: exp) = 
  let (doe, fe') = boxexpf e in
  match fe' with
    L(lt, _, e') -> (lt, doe, e', doCast e' lt voidPtrType)
  | F2(ft,_,p,b) -> ((getPtrFieldOfFat ft).ftype, doe, p, b)
  | _  ->
      let (et, caste, e'') = fexp2exp fe' doe in
      let (tptr, ptr, base) = readPtrBaseField e'' et in
      (tptr, doe @ caste, ptr, base)


and boxfunctionexp (f : exp) = 
  match f with
    Lval(Var vi, NoOffset) -> boxexp f
  | Lval(Mem base, NoOffset) -> 
      let rest, lvkind, lv', lvbase, dolv = boxlval (Mem base, NoOffset) in
      (rest, dolv @ [checkFunctionPointer (AddrOf(lv', lu)) lvbase], 
       Lval(lv'))
      
  | _ -> E.s (E.unimp "Unexpected function expression")

    (* Cast an fexp to another one. Accumulate necessary statements to doe *)
and castTo (fe: fexp) (newt: typ)
           (doe: stmt list) : stmt list * fexp =
  let newkind = kindOfType newt in
  match fe, isFatType newt with
  | L(lt, _, e) , false -> (* LEAN -> LEAN *)
      (doe, L(newt, P.Scalar, doCast e lt newt))
  | L(lt, pk, e), true -> (* LEAN -> FAT *)
      let ptype = (getPtrFieldOfFat newt).ftype in
      if not (isZero e) then
        ignore (E.warn "Casting scalar (%a) to pointer (in %s)!" 
                  d_exp e !currentFunction.svar.vname);
      let newp = 
        if typeSig lt = typeSig ptype then e else CastE (ptype, e, lu) in
      let newbase, doe' = 
        if !interceptCasts && not (isInteger e) then begin
          incr interceptId;
          let tmp = makeTempVar !currentFunction voidPtrType in
           Lval(var tmp),
          doe @
          [call (Some tmp) (Lval(var interceptCastFunction.svar)) 
              [ e ;
                integer !currentFileId;
                integer !interceptId
              ]
          ]
        end else CastE(voidPtrType, zero, lu), doe
      in
      (doe', F2 (newt, newkind, newp, newbase))
  
  (* FAT -> LEAN *)
  | F1(oldt, _, e), false ->
      (doe, L(newt, newkind, CastE(newt, readPtrField e oldt, lu)))
  | F2(oldt, _, ep, eb), false ->
      (doe, L(newt, newkind, CastE(newt, ep, lu)))
  | FC(oldt, _, prevt, _, e), false ->
      (doe, L(newt, newkind, CastE(newt, readPtrField e prevt, lu)))

  (* FAT -> FAT *)
  | F1(oldt, prevk, e), true -> (doe, FC (newt, newkind, oldt, prevk, e))
  | F2(oldt, oldk, ep, eb), true -> 
      let ptype = (getPtrFieldOfFat newt).ftype in
      (doe, F2 (newt, newkind, CastE(ptype, ep, lu), eb))
  | FC(oldt, oldk, prevt, prevk, e), true -> 
      (doe, FC(newt, newkind, prevt, prevk, e))
      

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


let fixupGlobName vi = 
  if vi.vglob && vi.vstorage <> Static && typeContainsFats vi.vtype &&
    not (List.exists (fun la -> la = vi.vname) leaveAlone) then
    let nlen = String.length vi.vname in
    if nlen <= 4 || String.sub vi.vname (nlen - 4) 4 <> "_fp_" then
      vi.vname <- vi.vname ^ "_fp_"


(* Create the preamble (in reverse order) *)
let preamble = 
  (** Create some more fat types *)
  ignore (fixupType (charPtrType));
  ignore (fixupType (TPtr(TInt(IChar, [AId("const")]), [])));
  ignore (fixupType (TPtr(TVoid([AId("const")]), [])));
  let startFile = !theFile in
  GText ("#include \"safec.h\"\n") :: 
  GText ("// Include the definition of the checkers\n") ::
  startFile

(* In some cases we might need to create a function to initialize some 
 * globals *)
let fileInit : (fundec * varinfo) option ref = ref None
let addGlobalInitializer (glob:varinfo) 
                         (startoff: offset) (startt: typ) (init: exp) = 
  (* See if we have created an initializer already *)
  let finit, itervar = 
    match !fileInit with 
      Some (f, v) -> f, v
    | None -> begin
        let f = emptyFunction ("__boxinit_" ^ 
                               (Filename.chop_extension
                                  (Filename.basename !currentFile.fileName))) 
        in
        (* Now make an iterator variable *)
        let iter = makeTempVar f ~name:"iter" intType in
        fileInit := Some (f, iter);
        f, iter
    end
  in
  (* Set the current function to be the initialization function. We know that 
   * we must be outside any functions now *)
  currentFunction := finit;
  (* Compute the base for this global *)
  let globbase = 
    match startt with
      TArray _ -> StartOf(Var glob, startoff)
    | _ -> AddrOf((Var glob, startoff), lu)
  in
  (* If we are here the initializer better be a Compound since it contains 
   * fat initializers inside (which are Compounds themselves) *)
  let rec initone (baseoff: offset) off what t acc = 
    match what with
      Compound (t, initl) -> 
        (* If this is a fat thing then we must call FATPOINTERWRITE, except 
         * if this global does not have tags *)
        let acc' = 
          if isFatType t && startoff <> NoOffset then
            (List.rev 
               (checkMem (Some what)
                  (Var glob, (addOffset off baseoff))
                  globbase
                  t P.Wild)) @ acc
          else acc 
        in
        foldLeftCompound (initone (addOffset off baseoff)) t initl acc'
    | _ -> mkSet (Var glob, addOffset off baseoff) what :: acc
  in
  let inits = 
    initone NoOffset startoff init startt [finit.sbody] 
  in 
(*    match init with 
    Compound(t, initl) -> 
      foldLeftCompound (initone startoff) t initl [finit.sbody]
    | _ -> E.s (E.bug "global initializer not a Compound")
  in *)
  finit.sbody <- mkSeq (List.rev inits)
             
let boxFile file =
  ignore (E.log "Boxing file\n");
  fileInit := None;
  currentFile := file;
  (* Compute a small file ID *)
  let _ = 
    let h = H.hash file.fileName in
    let h16 = (h lxor (h lsr 16)) land 0xFFFF in
    currentFileId := h16;
    ignore (E.log "File %s has id 0x%04x\n" file.fileName h16)
  in
  let rec doGlobal g = 
    match g with
                                        (* We ought to look at pragmas to see 
                                         * if they talk about alignment of 
                                         * structure fields *)
    | GPragma a -> begin
        (match a with
          ACons("interceptCasts", [ AId("on") ]) -> interceptCasts := true
        | ACons("interceptCasts", [ AId("off") ]) -> interceptCasts := false
        | _ -> ());
        theFile := g :: !theFile
    end

    | GText _  -> theFile := g :: !theFile
    | GDecl vi -> boxglobal vi false None
    | GVar (vi, init) -> boxglobal vi true init
    | GType (n, t) -> 
        if debug then
          ignore (E.log "Boxing GType(%s)\n" n);
        let tnew = fixupType t in
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
        let tagIterator : varinfo option ref = ref None in
        let tagLocal stmacc l = 
          if not (mustBeTagged l) then stmacc
          else
            let iter = 
              match !tagIterator with
                Some i -> i
              | None -> begin
                  let i = makeTempVar f ~name:"iter" intType in
                  tagIterator := Some i;
                  i
              end
            in
            let newtype = tagType l.vtype in
            l.vtype <- newtype;
            (makeTagAssignInit iter l) @ stmacc
        in
        let inilocals = List.fold_left tagLocal [] f.slocals in
        f.sbody <- mkSeq (inilocals @ [boxstmt f.sbody]);
        theFile := GFun f :: !theFile

    | (GAsm s) as g -> theFile := g :: !theFile

  and boxglobal vi isdef init = 
    if debug then
      ignore (E.log "Boxing GVar(%s)\n" vi.vname); 
        (* Leave alone some functions *)
    let origType = vi.vtype in
    if not (List.exists (fun s -> s = vi.vname) leaveAlone) then begin
      (* Remove the format attribute from functions that we do not leave 
       * alone  *)
      vi.vtype <- fixupType vi.vtype;
      vi.vattr <- dropAttribute vi.vattr (ACons("__format__", []));
    end;
          (* If the type has changed and this is a global variable then we
           * also change its name *)
    fixupGlobName vi;
    (* Prepare the data initializer. Catch the case when we are initialing an 
     * array of char with a string. If the initializer contains fats then we 
     * set fats to Some _ and init' to None  *)
    let init', fats = 
      match init with
        None -> None, None
      | Some e -> begin
          match e with
            Const(CStr _, _) when 
            (match unrollType origType with 
              TArray(TInt((IUChar|ISChar|IChar), _), _, _) -> true 
            | _ -> false) -> Some e, None
          | _ ->
              let e' = boxGlobalInit e origType in
              (* See if init' contains fat pointers *)
              let rec expContainsFats = function
                  Compound(t, [_, p; _, b]) when isFatType t ->
                    not (isZero b)
                | Compound(t, ilist) -> 
                    List.exists (fun (_, e) -> expContainsFats e) ilist
                | _ -> false
              in
              if expContainsFats e' then
                (* But we do not consider NULL a fat, since it is Ok for it 
                 * to have tags = 0 *)
                None, Some e'
              else
                Some e', None
      end
    in
    (* Tag some globals *)
    if not (mustBeTagged vi) then
      if isdef then begin
        (* See if we have fats *)
        (match fats with None -> () 
        | Some fats -> addGlobalInitializer vi NoOffset vi.vtype fats);
        theFile := GVar(vi, init') :: !theFile
      end else
        theFile := GDecl vi :: !theFile
    else begin
      vi.vtype <- tagType vi.vtype;
      if not isdef && vi.vstorage <> Extern then
        theFile := GDecl vi :: !theFile
      else begin
          (* Make the initializer *)
          (* Add it to the tag initializer *)
        let varinit = 
          if vi.vstorage = Extern 
          then None 
          else begin
            (match fats with
              Some fats' ->  
                let dfld, lfld, _, _, _ = splitTagType vi.vtype in
                addGlobalInitializer 
                  vi (Field(dfld, NoOffset)) dfld.ftype fats'
            | _ -> ()); 
            let (x, _) = makeTagCompoundInit vi.vtype init' in
            Some x
          end
        in
        theFile := GVar(vi, varinit) :: !theFile
      end
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
  (* Create the preamble *)
  theFile := preamble;
  interceptCasts := false;
  (* Now the orgininal file *)
  List.iter doGlobal file.globals;
  (* See if we must append the initializer *)
  (match !fileInit with 
    None -> ()
  | Some (f, _) -> 
      ignore (E.warn "Added global initializer %s" f.svar.vname);
      theFile := GFun f :: !theFile);
  let res = List.rev (!theFile) in
  (* Clean up global hashes to avoid retaining garbage *)
  H.clear hostsOfBitfields;
  H.clear typeNames;
  H.clear fixedTypes;
  H.clear taggedTypes;
  {file with globals = res}

  
      
let customAttrPrint a = 
  Ptrnode.ptrAttrCustom false a






