open Cil 
open Pretty 
open Trace

module H = Hashtbl
module E = Errormsg
module N = Ptrnode


let debugType = false
let debug = false


let interceptCasts = ref false  (* If true it will insert calls to 
                                 * __scalar2pointer when casting scalars to 
                                 * pointers.  *)

let lu = locUnknown
let isSome = function Some _ -> true | _ -> false


(**** Stuff that we use while converting to new CIL *)
let mkSet (lv:lval) (e: exp) : stmt 
    = mkStmt (Instr [Set(lv, e, lu)])
let call lvo f args : stmt = mkStmt (Instr [Call(lvo,f,args, lu)])
let mkAsm tmpls isvol outputs inputs clobs = 
  mkStmt (Instr [Asm(tmpls, isvol, outputs, inputs, clobs, lu)])
let mkInstr i : stmt = mkStmt (Instr [i])


(*** End stuff for old CIL *)

(**** We know in what function we are ****)
let currentFunction : fundec ref  = ref dummyFunDec
let currentFile     : file ref = ref dummyFile
let currentFileId     = ref 0

let extraGlobInit : stmt list ref = ref []

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
    L  of typ * N.opointerkind * exp     (* A one-word expression of a given 
                                         * kind (N.Scalar or PSafe) and type *)
  | FS of typ * N.opointerkind * exp     (* A multi-word expression that is 
                                         * named by a single expresion *)
  | FM of typ * N.opointerkind * exp * exp * exp 
                                         (* A multi-word expression that is 
                                          * made out of multiple single-word 
                                          * expressions: ptr, base and bend. 
                                          * bend might be "zero" if not 
                                          * needeed *)
  | FC of typ * N.opointerkind * typ * N.opointerkind * exp               
                                        (* A multi-word expression that is a 
                                         * cast of a FS to another fat type *)

let d_fexp () = function
    L(t, k, e) -> dprintf "L1(%a, %a:%a)" N.d_opointerkind k d_exp e d_type t
  | FS(_, k, e) -> dprintf "FS(%a, %a)" N.d_opointerkind k d_exp e
  | FM(_, k, ep, eb, ee) ->  
      dprintf "FM(%a, %a, %a, %a)" 
        N.d_opointerkind k d_exp ep d_exp eb d_exp ee
  | FC(_, k, _, _, e) ->  dprintf "FC(%a, %a)" N.d_opointerkind k d_exp e
  
let leaveAlone : (string, bool) H.t =
  let h = H.create 17 in
  List.iter (fun s -> H.add h s true)
    [ "sscanf"; "scanf";
      "fscanf"; "_CrtDbgReport"; 
      "fprintf"; "printf"; "sprintf" ];
  h


type allocInfo = {
  mutable aiZeros: bool;              (* Whether the allocator initializes the 
                                      * memory it allocates *)
  mutable aiGetSize: exp list -> exp; (* Extract the size argument out of a 
                                     * list of arguments *)
  mutable aiNewSize: exp -> exp list -> exp list;
                                    (* Rewrite the argument list with a new 
                                     * size *)
  } 

let allocFunctions : (string, allocInfo) H.t = H.create 13 

(* Now a constructor of allocation information from boxalloc pragmas *)
let boxallocPragma (name: string) (args: attrarg list) : unit =
  let getArg n args = 
    try List.nth args n 
    with _ -> E.s (E.bug "no size arguments in call to allocator %s\n" name) 
  in
  let replaceArg n what args = 
    let rec loop n = function
        _ :: rest when n = 0 -> what :: rest
      | a :: rest when n > 0 -> a :: loop (n - 1) rest
      | _ -> E.s (E.bug "cannot replace size argument for allocator %s\n" name)
    in
    loop n args
  in
  (* Initialize like for malloc *)
  let ai = 
    { aiZeros = false; 
      aiGetSize = getArg 0;
      aiNewSize = replaceArg 0;
    } 
  in
  let rec loop = function
      [] -> ()
    | AId("nozero") :: rest -> ai.aiZeros <- false; loop rest
    | AId("zero") :: rest -> ai.aiZeros <- true; loop rest
    | ACons("sizein", [AInt n]) :: rest -> 
        ai.aiGetSize <- getArg n; ai.aiNewSize <- replaceArg n;
        loop rest
    | ACons("sizemul", [AInt n1; AInt n2]) :: rest -> 
        ai.aiGetSize <-
           (fun args -> BinOp(Mult, getArg n1 args, getArg n2 args,
                              intType));
        ai.aiNewSize <-
           (fun what args -> 
             (replaceArg n1 one 
                (replaceArg n2 what args)));
        loop rest
    | a :: rest -> 
        (ignore (E.warn "Don't understand boxalloc atrtibute: %a@!"
                   d_attrarg a));
        loop rest
  in
  loop args;
  (* Add to the hash *)
  H.add allocFunctions name ai



let getAllocInfo fname = 
  try
  (* See if the function name starts with /* ... */ *)
    let fname' = 
      let l = String.length fname in
      if String.sub fname 0 2 = "/*" then
        let endpoly = String.index_from fname 2 '/' in
        String.sub fname (endpoly + 1) (l - endpoly - 1)
      else
        fname
    in
    (* ignore (E.log "Getting alloc info for %s\n" fname'); *)
    Some (H.find allocFunctions fname') 
  with _ -> None
    
let isAllocFunction name =
  isSome (getAllocInfo name)



(********************************************************************)


            (* Same for offsets *)
type offsetRes = 
    typ * stmt list * offset * exp * N.opointerkind
      

(*** Helpers *)            
let castVoidStar e = doCast e voidPtrType

let prefix p s = 
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p


  (* We collect here the new file *)
let theFile : global list ref = ref []

let checkFunctionDecls : global list ref 
    = ref [GText("#define __WILD\n#define __FSEQ\n#define __SAFE")]

(**** Make new types ****)


    (* For each new type name, keep track of various versions, usually due 
     * to varying attributes *)
let typeNames : (string, int ref) H.t = H.create 17

let rec newTypeName prefix t = 
  let n = prefix ^ (baseTypeName t) in
(*  ignore (E.log "newTypeName: prefix = %s, n=%s\n" prefix n); *)
  let n' = newAlphaName typeNames n in
(*  ignore (E.log " = %s@!" n'); *)
  n'

 (* Make a type name, for use in type defs *)
and baseTypeName = function
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
  | TComp (_, comp, _) -> 
      let su = if comp.cstruct then "s_" else "u_" in
      if String.sub comp.cname 0 1 = "@" then su
      else su ^ comp.cname
  | TFun _ -> "fun"
  | TPtr(t, _) -> "p_" ^ baseTypeName t
  | TArray(t, _, _) -> "a_" ^ baseTypeName t
  | _ -> "type"


(**** Inspect the boxing style attribute *)
let extractPointerTypeAttribute al = 
  let k, why = N.kindOfAttrlist al in
  k
          

let extractArrayTypeAttribute al = 
  filterAttributes "sized" al <> []

(**** Make new string names *)
let stringId = ref 0 
let newStringName () = 
  incr stringId;
  "__string" ^ (string_of_int !stringId)


let isNullTerm = function
    N.SeqN | N.FSeqN | N.SeqNT | N.FSeqNT -> true
  | _ -> false

(* sm: scan a list of strings for one element
 * (never know where to put this kind of stuff in ML) *)
let stringListContains (str:string) (sl:string list) : bool =
  List.exists (fun s -> s = str) sl


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
(*    ignore (E.log "Bundling the bitfields of %s\n"
              (compFullName comp)); *)
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
        { fname = bname; ftype = TComp (false, bundle,[]); 
          fattr = []; fcomp = comp } in
      (* Go over the previous bitfields and add them to the host map *)
      List.iter2 (fun oldbf newbf -> 
        H.add hostsOfBitfields (comp.ckey, oldbf.fname) (bfinfo, newbf))
        bitfields
        bundle.cfields;
      bfinfo :: prev
    in
    comp.cfields <- (loopFields [] [] comp.cfields)
  end


(***** Convert some pointers in types to fat pointers ************)
let sizedArrayTypes : (typsig, typ) H.t = H.create 123
(* We need to avoid generating multiple copies of the same tagged type 
 * because we run into trouble if a variable is defined twice (once with 
 * extern). *)             
let taggedTypes: (typsig, typ) H.t = H.create 123
(**** FIXUP TYPE ***)
let fixedTypes : (typsig, typ) H.t = H.create 17

(* Search in the type attributes for the node and get from the node the type 
 * qualifier. In the process also get rid of the const attributes. *)
let getNodeAttributes t =
  let dropit where a = 
    N.replacePtrNodeAttrList where 
      (dropAttribute a (Attr("const", [])))
  in
  let rec loop t = 
    match t with 
      TVoid a -> TVoid (dropit N.AtOther a)
    | TInt (i, a) -> TInt (i, dropit N.AtOther a)
    | TFloat (f, a) -> TFloat (f, dropit N.AtOther a)
    | TBitfield (i, s, a) -> TBitfield (i, s, dropit N.AtOther a)
    | TNamed (n, t, a) -> 
        let isptr = 
          match unrollType t with TPtr _ -> N.AtPtr | _ -> N.AtOther 
        in
        TNamed(n, loop t, dropit isptr a)
    | TPtr (t', a) -> TPtr(loop t', dropit N.AtPtr a)
    | TArray (t', (Some l as lo), a) -> 
        let at = if isZero l then N.AtOpenArray else N.AtArray in
        TArray(loop t', lo, dropit at a)

    | TArray (t', None, a) -> TArray(loop t', None, dropit N.AtOpenArray a)
    | TComp (isf, comp, a) -> TComp (isf, comp, dropit N.AtOther a) 
    | TEnum (n, f, a) -> TEnum (n, f, dropit N.AtOther a)
    | TFun (r, args, v, a) -> 
        List.iter (fun a -> a.vtype <- loop a.vtype) args;
        TFun(loop r, args, v, dropit N.AtOther a)
  in
  loop t

(***************** Handling of pointer qualifiers *****************)
let isFatComp (comp: compinfo) = 
  (comp.cstruct && 
   (match comp.cfields with 
     p :: b :: rest when p.fname = "_p" 
                 && (b.fname = "_b" || b.fname = "_e") -> 
       (match rest with
         [] -> true (* A two word pointer *)
       | [e] when e.fname = "_e" -> true
       | _ -> false)
   | _ -> false))


    (* Test if a type is FAT *)
let isFatType t = 
  match unrollType t with
    TComp (_, comp, _) when isFatComp comp -> true 
  | _ -> false

(* Given a fat type, return the three fieldinfo corresponding to the ptr, 
 * base and (optional) end *)
let getFieldsOfFat (t: typ) 
    : fieldinfo * (fieldinfo option) * (fieldinfo option) = 
  match unrollType t with
    TComp (_, comp, _) when isFatComp comp -> begin
      match comp.cfields with 
        p :: b :: e :: _ -> p, Some b, Some e
      | p :: b :: [] -> 
          (* b could either be the base field or the end field *)
          if b.fname = "_b" then p, Some b, None else p, None, Some b
      | _ -> E.s (E.bug "getFieldsOfFat")
    end
  | _ -> E.s (E.bug "getFieldsOfFat %a\n" d_type t)
        
(* Given an expression of a fat type, return three expressions, encoding the 
 * pointer, the base and the end. Also return the type of the first 
 * expression *)
let rec readFieldsOfFat (e: exp) (et: typ) 
    : typ * exp * exp * exp =     
  if isFatType et then
    let fptr, fbaseo, fendo = getFieldsOfFat et in
    let rec compOffsets = function
        NoOffset -> 
          Field(fptr, NoOffset), 
          (match fbaseo with Some fbase -> Field(fbase, NoOffset) 
                            | _ -> Field(fptr, NoOffset)),
          (match fendo with Some x -> Field(x, NoOffset) 
                            | _ -> Field(fptr, NoOffset))
            
      | Field(fi, o) -> 
          let po, bo, lo = compOffsets o in
          Field(fi, po), Field(fi, bo), Field(fi, lo)
            
      | Index(e, o) ->
          let po, bo, lo = compOffsets o in
          Index(e, po), Index(e, bo), Index(e, lo)
    in
    let ptre, basee, ende = 
      match e with
        Lval(Var vi, o) -> 
          let po, bo, eo = compOffsets o in
          Lval(Var vi, po), Lval(Var vi, bo), Lval(Var vi, eo)
      | Lval(Mem e'', o) -> 
          let po, bo, eo = compOffsets o in
          Lval(Mem e'', po), Lval(Mem e'', bo), Lval(Mem e'', eo)
      | Question (e1, e2, e3) ->
          let e2t, e2', e2'', e2e = readFieldsOfFat e2 et in
          let   _, e3', e3'', e3e = readFieldsOfFat e3 et in
          (Question(e1,e2',e3'), 
           Question(e1,e2'',e3''), 
           Question(e1, e2e,e3e))
(*
      | Compound (t, [p; b]) when isFatType t -> 
          p, b, zero
      | Compound (t, [p; b; e]) when isFatType t -> 
          p, b, e
*)
      | _ -> E.s (E.unimp "split _p field offset: %a" d_plainexp e)
    in
    (fptr.ftype, ptre, basee, ende)
  else
    (et, e, zero, zero)

    (* Create a new temporary of a fat type and set its pointer and base 
     * fields *)
let setFatPointer (t: typ) (p: typ -> exp) (b: exp) (e: exp)
    : stmt list * lval = 
  let tmp = makeTempVar !currentFunction t in
  let fptr, fbaseo, fendo = getFieldsOfFat t in
  let p' = p fptr.ftype in
  let setbase = 
    match fbaseo with
      None -> []
    | Some fbase -> [mkSet (Var tmp, Field(fbase,NoOffset)) (castVoidStar b)]
  in
  let setend = 
    match fendo with
      None -> []
    | Some fend -> [mkSet (Var tmp, Field(fend,NoOffset)) (castVoidStar e)]
  in
  ( mkSet (Var tmp, Field(fptr,NoOffset)) p' ::
    setbase @ setend, 
    (Var tmp, NoOffset))
      
let readPtrField (e: exp) (t: typ) : exp = 
  let (tptr, ptr, base, bend) = readFieldsOfFat e t in ptr
      
let readBaseField (e: exp) (t: typ) : exp = 
  let (tptr, ptr, base, bend) = readFieldsOfFat e t in base

let kindOfType t = 
  (* Since t was fixed up, it has a qualifier if it is a pointer *)
  match extractPointerTypeAttribute (typeAttrs t) with
    N.Unknown -> begin
      match unrollType t with
        TPtr _ -> if !N.defaultIsWild then N.Wild else N.Safe
      | t when isFatType t -> N.Wild
      | _ -> N.Scalar
    end
  | res -> res


let breakFexp (fe: fexp) : typ * N.opointerkind * exp * exp * exp = 
  match fe with
    L(oldt, oldk, e) -> oldt, oldk, e, zero, zero
  | FS(oldt, oldk, e) -> 
      let (_, p, b, bend) = readFieldsOfFat e oldt in
      oldt, oldk, p, b, bend
  | FM(oldt, oldk, p, b, e) -> oldt, oldk, p, b, e
  | FC(oldt, oldk, prevt, prevk, e) -> 
      let (_, p, b, bend) = readFieldsOfFat e prevt in
      oldt, oldk, p, b, bend (* Drop the cast *)
    

(**** Pointer representation ****)
let pkNrFields = function
    N.Safe | N.Scalar | 
    N.WildT | N.SeqT | N.FSeqT | N.SeqNT | N.FSeqNT | N.IndexT -> 1
  | N.String | N.ROString -> 1
  | N.Wild | N.FSeq | N.FSeqN | N.Index -> 2
  | N.Seq | N.SeqN -> 3
  | k -> E.s (E.bug "pkNrFields: %a" N.d_opointerkind k)

let pkFields (pk: N.opointerkind) : (string * (typ -> typ)) list = 
  match pkNrFields pk with
    1 -> [ ("", fun x -> x) ]
  | 2 -> begin
      match pk with
        N.FSeq | N.FSeqN -> [ ("_p", fun x -> x); 
                              ("_e", fun _ -> voidPtrType) ]
      | _ -> [ ("_p", fun x -> x); 
               ("_b", fun _ -> voidPtrType) ]
  end
  | 3 -> [ ("_p", fun x -> x); 
           ("_b", fun _ -> voidPtrType);
           ("_e", fun _ -> voidPtrType) ]
  | _ -> E.s (E.bug "pkFields")
  
(* Make an fexp out of a single expression. Either the type is fat and a 
 * composite value is denoted by a single expression *)
let mkFexp1 (t: typ) (e: exp) : fexp  = 
  let k = kindOfType t in
  match pkNrFields k with 
    1 -> L (t, k, e)
  | _ -> FS (t, k, e)

(* Make an fexp out of three expressions representing a pointer, the base and 
 * the end. The end, or the base and the end might be disregarded, depending 
 * on the type of the pointer beng created *)
let mkFexp3 (t: typ) (ep: exp) (eb: exp) (ee: exp) : fexp  = 
  let k = kindOfType t in
  match pkNrFields k with
    1 -> L (t, k, ep)
  | _ -> FM (t, k, ep, eb, ee)


let pkTypePrefix (pk: N.opointerkind) = 
  match pk with
    N.Wild | N.FSeq | N.FSeqN | N.Index -> "fatp_"
  | N.Seq | N.SeqN -> "seq_"
  | _ -> E.s (E.bug "pkTypeName")
  

let pkQualName (pk: N.opointerkind) 
               (acc: string list) 
               (dobasetype: string list -> string list) : string list = 
  if pkNrFields pk = 1 then dobasetype ("s" :: acc) else
  match pk with
  | N.Wild -> "w" :: acc (* Don't care about what it points to *)
  | N.Index -> dobasetype ("i" :: acc)
  | N.Seq -> dobasetype ("q" :: acc)
  | N.SeqN -> dobasetype ("q" :: acc)
  | N.FSeq -> dobasetype ("f" :: acc)
  | N.FSeqN -> dobasetype ("f" :: acc)
  | N.Scalar -> acc
  | _ -> E.s (E.bug "pkQualName")
  
(****** the CHECKERS ****)


let checkNullFun =   
  let fdec = emptyFunction "CHECK_NULL" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec

let checkSafeRetFatFun = 
  let fdec = emptyFunction "CHECK_SAFERETFAT" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "isptr" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec
    
  
    
    
let checkSafeFatLeanCastFun = 
  let fdec = emptyFunction "CHECK_SAFEFATLEANCAST" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec

let checkFunctionPointer = 
  let fdec = emptyFunction "CHECK_FUNCTIONPOINTER" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fun whatp whatb whatkind -> 
    if whatkind = N.Safe then
      call None (Lval(var checkNullFun.svar)) [ castVoidStar whatp ]
    else
      call None (Lval(var fdec.svar)) [ castVoidStar whatp; 
                                        castVoidStar whatb]

(* Compute the ptr corresponding to a base. This is used only to pass an 
 * argument to the intercept functionin the case when the base = 0 *)
let ptrOfBase (base: exp) =  
  let rec replaceBasePtr = function
      Field(fip, NoOffset) when fip.fname = "_b" ->
             (* Find the fat type that this belongs to *)
        let pfield, _, _ = getFieldsOfFat (TComp(false, fip.fcomp, [])) in
        Field(pfield, NoOffset)
          
    | Field(f', o) -> Field(f',replaceBasePtr o)
    | _ -> raise Not_found
  in
  match base with
    Lval (b, off) -> 
      begin try Lval(b, replaceBasePtr off) with Not_found -> base end
  | _ -> base


let checkFetchLength = 
  let fdec = emptyFunction "CHECK_FETCHLENGTH" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vstorage <- Static;
  fdec.svar.vtype <- TFun(uintType, [ argp; argb ], false, []);
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fun tmplen ptr base -> 
    call (Some (tmplen, false)) (Lval (var fdec.svar))
      [ castVoidStar ptr; 
        castVoidStar base ]

let checkFetchStringEnd = 
  let fdec = emptyFunction "CHECK_FETCHSTRINGEND" in
  let args  = makeLocalVar fdec "s" charPtrType in
  fdec.svar.vstorage <- Static;
  fdec.svar.vtype <- TFun(voidPtrType, [ args; ], false, []);
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec

let checkStringMax = 
  let fdec = emptyFunction "CHECK_STRINGMAX" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vstorage <- Static;
  fdec.svar.vtype <- TFun(uintType, [ argp; argb ], false, []);
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec

let checkFetchEnd = 
  let fdec = emptyFunction "CHECK_FETCHEND" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidPtrType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fun tmplen base -> 
    let ptr = ptrOfBase base in (* we used to use this and worked, but when 
                                 * we added tables stoped working *)
    call (Some (tmplen, false)) (Lval (var fdec.svar))
      [ castVoidStar ptr; 
        castVoidStar base ]

let checkLBoundFun = 
  let fdec = emptyFunction "CHECK_LBOUND" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argp; ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  theFile := GDecl (fdec.svar, lu) :: !theFile;
  fdec

let checkUBoundFun = 
  let fdec = emptyFunction "CHECK_UBOUND" in
  let argbend  = makeLocalVar fdec "bend" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argbend; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec

let checkBoundsFun = 
  let fdec = emptyFunction "CHECK_BOUNDS" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argbend  = makeLocalVar fdec "bend" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argbend; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec

let checkBoundsLenFun = 
  let fdec = emptyFunction "CHECK_BOUNDS_LEN" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argbl  = makeLocalVar fdec "bl" uintType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argbl; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
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
  theFile := GDecl (fdec.svar, lu) :: !theFile;
  fdec


(* Check a read *)
let checkFatPointerRead = 
  let fdec = emptyFunction "CHECK_FATPOINTERREAD" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let arglen  = makeLocalVar fdec "nrWords" uintType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argb; arglen; argp; ], false, []);
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec.svar.vstorage <- Static;
  
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
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec.svar.vstorage <- Static;
  
  fun base where whatbase whatp len -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar base; len; 
        castVoidStar where; 
        castVoidStar whatbase; castVoidStar whatp;]
  
let checkFatStackPointer = 
  let fdec = emptyFunction "CHECK_FATSTACKPOINTER" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argp  = makeLocalVar fdec "isptr" voidPtrType in
  fdec.svar.vtype <- 
     TFun(voidType, [ argp; argb; ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  
  fun whatp nullIfInt -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar whatp; castVoidStar nullIfInt;]
  

let checkLeanStackPointer = 
  let fdec = emptyFunction "CHECK_LEANSTACKPOINTER" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- 
     TFun(voidType, [ argp; ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  
  fun whatp -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar whatp;]

let checkZeroTagsFun =
  let fdec = emptyFunction "CHECK_ZEROTAGS" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argbl = makeLocalVar fdec "bl" uintType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argsize  = makeLocalVar fdec "size" uintType in
  let offset  = makeLocalVar fdec "offset" uintType in
  fdec.svar.vtype <- 
     TFun(voidType, [ argb; argbl; argp; argsize; offset ], false, []);
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec.svar.vstorage <- Static;
  fdec

let checkFindHomeFun =
  let fdec = emptyFunction "CHECK_FINDHOME" in
  let argk  = makeLocalVar fdec "kind" intType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- 
     TFun(voidPtrType, [ argk; argp ], false, []);
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec.svar.vstorage <- Static;
  fdec


let checkFindHomeEndFun =
  let fdec = emptyFunction "CHECK_FINDHOMEEND" in
  let argk  = makeLocalVar fdec "kind" intType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argea  = makeLocalVar fdec "ea" (TPtr(voidPtrType,[])) in
  fdec.svar.vtype <- 
     TFun(voidPtrType, [ argk; argp; argea ], false, []);
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec.svar.vstorage <- Static;
  fdec


(* When we compute attributes we ignore the ptrnode attribute *)
let ignorePtrNode al = 
  let rec loop = function
      [] -> []
    | Attr("_ptrnode", _) :: rest -> loop rest
    | (a :: rest) as al -> 
        let rest' = loop rest in
        if rest' == rest then al else a :: rest'
  in
  loop al
let typeSigBox t = typeSigAttrs ignorePtrNode t

(***** Pointer arithemtic *******)
let checkPositiveFun = 
  let fdec = emptyFunction "CHECK_POSITIVE" in
  let argx  = makeLocalVar fdec "x" intType in
  fdec.svar.vtype <- TFun(voidType, [ argx; ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec


let rec fixupType t = 
  match t with
    TComp (true, _, _) -> t (* Leave alone the forward definitions *)
    (* Keep the Named types
  | TNamed _ -> begin
     match getNodeAttributes t with
       TNamed(n, t', a) -> TNamed(n, fixupType t', a)
      | _ -> E.s (E.bug "fixupType")
   end  *)

  (* Do not hash function types because they contain arguments whose types 
   * change later *)
  | TFun (rt, args, isva, a) -> begin
      List.iter (fun argvi -> argvi.vtype <- fixupType argvi.vtype) args;
      let res = TFun(fixupType rt, args, isva, a) in
      res
(*
      match fixit t with
        TFun (rt, args', isva, a) -> 
          TFun(rt,
               List.map2 (fun a a' -> {a' with vname = a.vname;}) args args',
               isva, dropAttribute a (Attr("__format__",[]))) 
      | _ -> E.s (E.bug "fixupType")
*)
  end 
  | _ -> fixit t

and fixit t = 
  (* First drop the Const attribute and replace the _ptrnode attribute *)
(*  ignore (E.log "fixit: %a@!" d_plaintype t); *)
  let t = getNodeAttributes t in
  let ts = typeSigBox t in
  let res = 
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
            let pkind = kindOfType t in 
            let newType = TPtr(fixed', a) in
            let fixed = 
              if pkNrFields pkind = 1 then newType 
              else
                let tname  = newTypeName (pkTypePrefix pkind) fixed' in
                let tstruct = 
                  TComp 
                    (false, 
                     mkCompInfo true tname 
                       (fun _ -> 
                         List.map (fun (n,tf) -> (n, tf newType, []))
                           (pkFields pkind))
                       [],
                     [])
                in
                theFile := GType(tname, tstruct, lu) :: !theFile;
                let tres = TNamed(tname, tstruct, [N.k2attr pkind]) in
                (* Add this to ensure that we do not try to box it twice *)
                H.add fixedTypes (typeSigBox tstruct) tres;
                (* And to make sure that for all identical pointer types we 
                 * create identical structure *)
                H.add fixedTypes (typeSigBox newType) tres;
                tres
            in
            (* We add fixed ourselves. The TNamed will be added after doit  *)
            (* H.add fixedTypes (typeSigBox fixed) fixed; *)
            fixed
        end
              
        | TComp (true, _, _) ->  t    (* Don't follow TForward, since these 
                                       * fill be taken care of when the 
                                       * definition is encountered  *)
        | TNamed (n, t', a) -> TNamed (n, fixupType t', a)
              
        | TComp (_, comp, a) -> 
          (* Change the fields in place, so that everybody sees the change *)
            List.iter 
              (fun fi -> 
                let newa, newt = moveAttrsFromDataToType  fi.fattr fi.ftype in
                fi.fattr <- newa ; 
                fi.ftype <- fixupType newt) 
              comp.cfields;
            bitfieldCompinfo comp;
            (* Save the fixed comp so we don't redo it later *)
            H.add fixedTypes (typeSigBox t) t;
            t
              
        | TArray(t', l, a) -> 
            let sized = extractArrayTypeAttribute a in
            let newarray = TArray(fixupType t', l, a) in
            if sized then begin
              addArraySize newarray
            end else begin
              newarray
            end
                
                
        | TFun(rt,args,isva,a) ->
(*          let args' = 
            List.map
            (fun argvi -> {argvi with vtype = fixupType argvi.vtype}) args 
            * in
            
            *)
            List.iter (fun argvi -> argvi.vtype <- fixupType argvi.vtype) args;
            let res = TFun(fixupType rt, args, isva, a) in
            res
      in
(*    H.add fixedTypes ts fixed; *)
(*      H.add fixedTypes (typeSigBox fixed) fixed; *)
(*    ignore (E.log "Id of %a\n is %s\n" d_plaintype t (N.typeIdentifier t));*)
      fixed
    end
  in
(*  ignore (E.log " :%a\n" d_plaintype res);*)
  res

and moveAttrsFromDataToType attrs typ = 
  let mustMove = function
      Attr("sized", []) -> true
    | Attr("nullterm", []) -> true
    | _ -> false
  in
  match List.filter mustMove attrs with
  | [] -> attrs, typ
  | tomove -> List.filter (fun a -> not (mustMove a)) attrs,
              typeAddAttributes tomove typ

(****** Generate sized arrays *)
and addArraySize t = 
  let tsig = typeSigBox t in
  try
    H.find sizedArrayTypes tsig
  with Not_found -> begin
	(* GCC does not like fields to have incomplete types *)
    let complt = 
      if isCompleteType t then typeAddAttributes [Attr("sized", [])] t 
      else begin
        match unrollType t with
	  TArray(bt, None, a) -> TArray(bt, Some zero, 
                                        addAttribute (Attr("sized", [])) a)
        | TArray(bt, Some z, a) when isZero z -> 
            TArray(bt, Some z, addAttribute (Attr("sized", [])) a)
        | TComp (_, ci, a) when ci.cfields = [] -> TArray(charType, Some zero, 
                                                          [Attr("sized", [])])
        | _ -> 
            E.s (E.unimp "Don't know how to tag incomplete type %a" 
                   d_plaintype t)
      end
    in
    let packAttr = if !msvcMode then [] else [Attr("packed", [])] in
    let newtype = 
      TComp 
        (false, 
         mkCompInfo true ""
           (fun _ -> 
             [ ("_size", uintType, []); (* Don't pack the first field or else 
                                         * the whole variable will be packed 
                                         * against the preceeding one *)
               ("_array", complt, packAttr); ]) [],
         [])
    in
    let tname = newTypeName "_sized_" t in
    let named = TNamed (tname, newtype, [Attr("sized", [])]) in
    theFile := GType (tname, newtype, lu) :: !theFile;
    H.add sizedArrayTypes tsig named;
    (* Since maybe we added a zero length when there was no length, we should 
     * compute the new signature
    (match tsig with
      TSArray(t,None,al) -> 
        H.add sizedArrayTypes (TSArray(t,Some zero,al)) named
    | _ -> ());  *)
    (* Maybe we are adding too many types here *)
    H.add sizedArrayTypes (typeSigBox named) named;
    H.add sizedArrayTypes (typeSigBox newtype) named;  
    H.add sizedArrayTypes (typeSigBox complt) named;  
    named
  end
  
and tagType (t: typ) : typ = 
  let tsig = typeSigBox t in
  try
    H.find taggedTypes tsig
  with Not_found -> begin
    let newtype = 
      if isCompleteType t then 
        (* ignore (E.log "Type %a -> bytes=%d, words=%d, tagwords=%d\n"
                  d_type t bytes words tagwords); *)
        let _, tagWords = tagLength (SizeOf(t)) in
        let tagAttr = if !msvcMode then [] else [Attr("packed", [])]
        in
        TComp 
          (false, 
           mkCompInfo true ""
             (fun _ -> 
               [ ("_len", uintType, []); (* Don't pack the first field,or 
                                          * else the entire thing will be 
                                          * packed against the preceeding one *)
                 ("_data", t, tagAttr);
                 ("_tags", TArray(intType, 
                                  Some tagWords, []), tagAttr);
               ])
             [],
           [])
      else begin (* An incomplete type *)
	(* GCC does not like fields to have incomplete types *)
	let complt = 
	  match unrollType t with
	    TArray(bt, None, a) -> TArray(bt, Some zero, a)
	  | TArray(bt, Some z, a) when isZero z -> t
	  | TComp (_, ci, _) when ci.cfields = [] -> 
              TArray(charType, Some zero, [])
	  | _ -> t (* E.s (E.unimp "Don't know how to tag incomplete type %a" 
                        d_plaintype t) *)
	in
        TComp 
          (false, 
           mkCompInfo true ""
             (fun _ -> 
               [ ("_len", uintType, []);
                 ("_data", complt, []); ]) [],
           [])
      end
    in
    let tname = newTypeName "_tagged_" t in
    let named = TNamed (tname, newtype, []) in
    theFile := GType (tname, newtype, lu) :: !theFile;
    H.add taggedTypes tsig named;
    H.add taggedTypes (typeSigBox named) named;
    named
   end

(* Compute the number of data words and the number of tag words, given a raw 
 * area size (in bytes) *)
and tagLength (sz: exp) : (exp * exp) =
  (* First the number of words *)
   BinOp(Shiftrt, 
        BinOp(PlusA, doCast sz uintType, kinteger IUInt 3, uintType),
        integer 2, uintType),
  (* Now the number of tag words. At 1 tag bit/ word we can fit the tags for 
   * 128 bytes into one tag word. *)
  BinOp(Shiftrt, 
        BinOp(PlusA, 
              doCast sz uintType, kinteger IUInt 127, uintType),
        integer 7, uintType)
    


(**** Make a pointer type of a certain kind *)
let mkPointerTypeKind (bt: typ) (k: N.opointerkind) = 
   fixupType (TPtr(bt, [N.k2attr k]))

(***** Conversion functions *******)


(***** Address of ******)
let pkAddrOf (lv: lval)
             (lvt: typ)
             (lvk: N.opointerkind)  (* The kind of the AddrOf pointer *)
             (fb: exp)
             (fe: exp) : (fexp * stmt list) = 
  let ptrtype = mkPointerTypeKind lvt lvk in
  match lvk with
    N.Safe -> mkFexp1 ptrtype (mkAddrOf lv), []
  | (N.Index | N.Wild | N.FSeq | N.FSeqN | N.Seq | N.SeqN ) -> 
      mkFexp3 ptrtype (AddrOf(lv)) fb fe, []
  | _ -> E.s (E.bug "pkAddrOf(%a)" N.d_opointerkind lvk)
         
         
(* Given an array type return the element type, pointer kind, base and bend *)
let arrayPointerToIndex (t: typ) 
                        (k: N.opointerkind) 
                        (lv: lval) 
                        (base: exp) = 
  match unrollType t with
    TArray(elemt, _, a) when k = N.Wild || k = N.WildT -> 
      (elemt, N.Wild, base, zero)

  | TArray(elemt, _, a) when (filterAttributes "sized" a <> []) -> 
      (elemt, N.Index, mkAddrOf lv, zero)

    (* If it is not sized then better have a length *)
  | TArray(elemt, Some alen, a) -> 
      let knd, alen' =
        if filterAttributes "nullterm" a <> [] then begin
          (match unrollType elemt with
            TInt((IChar|IUChar|ISChar), _) -> ()
          | _ -> E.s (E.warn "NULLTERM array of %a\n" d_type elemt));
          (* Leave null for the null character *)
          N.SeqN, BinOp(MinusA, alen, one, intType)
        end else N.Seq, alen 
      in
      (elemt, knd, mkAddrOf lv, 
       BinOp(IndexPI, mkAddrOf lv, alen', TPtr(elemt, [])))

  | TArray(elemt, None, a) -> 
      (* Not WILD and not SIZED *)
      E.s (E.bug "arrayPointIndex on a unsized array: %a\n"
             d_lval lv)

  | _ -> E.s (E.bug "arrayPointerToIndex on a non-array (%a)" 
                d_plaintype t)





(************* END of pointer qualifiers *************)
  



   (* Test if we have changed the type *)
let rec typeContainsFats t =
   existsType 
   (function TComp (false, comp, _) -> 
      begin
        match comp.cfields with
          [p;b] when comp.cstruct && p.fname = "_p" && b.fname = "_b" -> 
            ExistsTrue
        | _ -> ExistsMaybe
      end 
      | _ -> ExistsMaybe)
    t

(* See if a type contains arrays *)
let containsArray t =
  existsType 
    (function 
        TArray _ -> ExistsTrue 
      | TPtr _ -> ExistsFalse
      | TFun _ -> ExistsFalse
      | _ -> ExistsMaybe) t

(* Create tags for types along with the newly created fields and initializers 
 * for tags and for the length  *)
(* Check whether the type contains an embedded array *)
let mustBeTagged v =
  (* sm: if it is a global (locals get passed to this fn too), and
   * is among the special list of untagged globals, leave it alone *
  if (v.vglob &&
      (stringListContains v.vname leaveAloneGlobVars)) then
    false else     * return false *
*)
  let isFunction = 
    match v.vglob, v.vtype with 
      true, TFun _ -> true
    |  _ -> false
  in
  if isFunction then false
                       (* Do not tag functions!! Mainly because we don't know 
                        * how to put the tag. Plus, function pointers should 
                        * have a length = 0 so we cannot write there *)
  else
    (* See if it make sense to tag this one. We look at the address-of flag 
     * and whether it contains arrays. *)
    let taggable = 
      if v.vglob then 
        if v.vstorage = Static then 
          v.vaddrof || containsArray v.vtype
        else 
          true  (* We tag all externals because we might 
                   take their address somewhere else *)
      else
        v.vaddrof || containsArray v.vtype
    in
    taggable &&
    (!N.defaultIsWild || (filterAttributes "tagged" v.vattr) <> [])


(*** Check to see if a variable must be registered as the scope is entered *)
let mustBeRegistered (vi: varinfo) : bool = 
  (* For now *)
  mustBeTagged vi || containsArray vi.vtype

(* A few constants *)
let registerAreaTaggedInt = 0
let registerAreaSizedInt  = 1
let registerAreaSeqInt    = 2

let registerAreaFun =   
  let fdec = emptyFunction "CHECK_REGISTERAREA" in
  let argi  = makeLocalVar fdec "k" intType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let arge  = makeLocalVar fdec "e" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argi; argb; arge; ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec

let unregisterFrameFun =   
  let fdec = emptyFunction "CHECK_UNREGISTERFRAME" in
  fdec.svar.vtype <- TFun(voidType, [ ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := GDecl (fdec.svar, lu) :: !checkFunctionDecls;
  fdec


(* Everytime you register a local variable, remember here *)  
let hasRegisteredAreas = ref true

(* Produce a statement to register an area and saves the code to unregister 
 * the area *)
let registerArea (args: exp list) 
                 (acc: stmt list) : stmt list = 
  if !N.useLeanFats then begin
    hasRegisteredAreas := true;
    let reg = call None (Lval(var registerAreaFun.svar)) args in
    reg :: acc
  end else
    acc

let unregisterStmt () = 
  if !hasRegisteredAreas then 
    call None (Lval(var unregisterFrameFun.svar)) []
  else
    mkEmptyStmt ()


(* Create a compound initializer for a tagged type *)
let splitTagType (tagged: typ) 
    : fieldinfo * fieldinfo * fieldinfo * exp * exp  = 
  (* Get the data field, the length field, and a tag field *)
  let dfld, lfld, tfld = 
    match unrollType tagged with
      TComp (_, comp, _) -> begin
        match comp.cfields with 
          [lfld; dfld; tfld] -> dfld, lfld, tfld
        | _ -> E.s (E.bug "splitTagType. No tags: %a\n" d_plaintype tagged)
      end
    | _ -> E.s (E.bug "splitTagType. No tags: %a\n" d_plaintype tagged)
  in
  let words, tagwords = tagLength (SizeOf(dfld.ftype)) in
            (* Now create the tag initializer *)
  dfld, lfld, tfld, words, tagwords

let makeTagCompoundInit (tagged: typ) 
                        (datainit: init option) : init * fieldinfo = 
  let dfld, lfld, tfld, words, _ = splitTagType tagged in
  CompoundInit (tagged, 
                  (* Now the length *)
                SingleInit words ::
                (match datainit with 
                  None -> []
                | Some e -> [e]))
            (* Leave the rest alone since it will be initialized with 0 *)
    ,
  dfld



(* Since we cannot take the address of a bitfield we treat accesses to a 
 * bitfield like an access to the entire host that contains it (for the 
 * purpose of checking). This is only Ok if the host does not contain pointer 
 * fields *)
let getHostIfBitfield (lv: lval) (t: typ) : lval * typ = 
  match unrollType t with
    TBitfield (ik, wd, a) -> begin
      let lvbase, lvoff = lv in
      let rec getHost = function
          Field(fi, NoOffset) -> NoOffset
        | Field(fi, off) -> Field(fi, getHost off)
        | Index(e, off) -> Index(e, getHost off)
        | NoOffset -> E.s (E.bug "a TBitfield that is not a field")
      in
      let lv' = lvbase, getHost lvoff in
      let lv't = typeOfLval lv' in
      (match unrollType lv't with 
        TComp (_, comp, _) when comp.cstruct -> 
          if List.exists (fun f -> typeContainsFats f.ftype) comp.cfields then
            E.s (E.unimp "%s contains both bitfields and pointers.@!LV=%a@!T=%a@!" 
                   (compFullName comp) d_plainlval lv d_plaintype t)
      | _ -> E.s (E.bug "getHost: bitfield not in a struct"));
      lv', lv't
    end
  | _ -> lv, t


(* Now a routine to take the address of a field *)
let takeAddressOfBitfield (lv: lval) (t: typ) : exp = 
  let lv', t' = getHostIfBitfield lv t in
  mkAddrOf lv'

(* Compute the offset of first scalar field in a thing to be written. Raises 
 * Not_found if there is no scalar *)
let offsetOfFirstScalar (t: typ) : exp = 
  let rec theOffset sofar t = 
    match unrollType t with
      (TInt _ | TFloat _ | TEnum _) -> Some sofar
    | TPtr _ -> None
    | TComp (_, comp, _) when isFatComp comp -> None
    | TComp (_, comp, _) when comp.cstruct -> begin
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
    | TArray (bt, _, _) -> 
        theOffset (addOffset (Index(zero, NoOffset)) sofar) bt
    | _ -> E.s (E.unimp "offsetOfFirstScalar")
  in
  match theOffset NoOffset t with
    None -> raise Not_found
  | Some NoOffset -> kinteger IUInt 0 
  | Some off -> 
      let scalar = Mem (doCastT zero intType (TPtr (t, []))), off in
      let addrof = mkAddrOf scalar in
      doCast addrof uintType

  
let checkZeroTags base lenExp lv t = 
  let lv', lv't = getHostIfBitfield lv t in
  try
    let offexp = offsetOfFirstScalar lv't in
    call None (Lval (var checkZeroTagsFun.svar))
      [ castVoidStar base; lenExp ;
        castVoidStar (mkAddrOf lv'); 
        SizeOf(lv't); offexp ] 
  with Not_found -> 
    mkEmptyStmt ()

(*  
let doCheckFat which arg argt = 
  let (_, ptr, base, end) = readFieldsOfFat arg argt in 
  match kindOfType argt with
    N.FSeq | N.FSeqN 
  call None (Lval(var which.svar)) [ castVoidStar ptr; 
                                     castVoidStar base; ]
*)




(****** CONVERSION FUNCTIONS *******)

(* Accumulate the statements in reverse order *)
let seqToFSeq (p: exp) (b: exp) (bend: exp) (acc: stmt list)
    : exp * exp * exp * stmt list =   
  p, bend, zero, 
  call None (Lval (var checkLBoundFun.svar))
    [ castVoidStar b; castVoidStar p; ] :: acc

let indexToSeq (p: exp) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  let tmp = makeTempVar !currentFunction voidPtrType in
  p, b, Lval(var tmp), checkFetchEnd tmp b :: acc

let indexToFSeq (p: exp) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  let p', b', bend', acc' = indexToSeq p b bend acc in
  seqToFSeq p' b' bend' acc'
  
let fseqToSafe (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  let baset =
      match unrollType desttyp with
        TPtr(x, _) -> x
      | _ -> E.s (E.bug "fseqToSafe: expected pointer type")
  in
  p, zero, zero, 
  call None (Lval (var checkUBoundFun.svar))
    [ castVoidStar b;  
      castVoidStar p; SizeOf (baset)] :: acc
    
let seqToSafe (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
(*
  let p', b', bend', acc' = seqToFSeq p b bend acc in
  fseqToSafe p' desttyp b' bend' acc'
*)
  (* An alternative way that collapses the two bounds checks *)
  let baset =
      match unrollType desttyp with
        TPtr(x, _) -> x
      | _ -> E.s (E.bug "seqToSafe: expected pointer type")
  in
  p, zero, zero,
  call None (Lval (var checkBoundsFun.svar))
    [ castVoidStar b;  castVoidStar bend;
      castVoidStar p; SizeOf (baset)] :: acc
  

let indexToSafe (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  let p', b', bend', acc' = indexToSeq p b bend acc in
  seqToSafe p' desttyp b' bend' acc'
    

let stringToSeq (p: exp) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  (* Make a new temporary variable *)
  let tmpend = makeTempVar !currentFunction voidPtrType in
  p, p,  (Lval (var tmpend)),
  call (Some (tmpend, false)) (Lval (var checkFetchStringEnd.svar))
    [ p ] :: acc

let stringToFseq (p: exp) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  (* Make a new temporary variable *)
  let tmpend = makeTempVar !currentFunction voidPtrType in
  p, (Lval (var tmpend)), zero,
  call (Some (tmpend, false)) (Lval (var checkFetchStringEnd.svar))
    [ p ] :: acc

  
let seqNToString (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  (* Conversion to a string is with a bounds check *)
  seqToSafe p desttyp b bend acc

let fseqNToString (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  (* Conversion to a string is with a bounds check *)
  fseqToSafe p desttyp b bend acc

let wildToROString (p: exp) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  p, zero, zero, 
  call None (Lval (var checkStringMax.svar))
    [ castVoidStar p; b ] :: acc

(* weimer: is this right?! *)
let indexToROString (p: exp) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  p, zero, zero, 
  call None (Lval (var checkStringMax.svar))
    [ castVoidStar p; b ] :: acc

let fromTable (oldk: N.opointerkind) 
              (p: exp) 
  (* Returns a base, and an end *) 
  : exp * exp * stmt list =
  let checkAreas () = 
    if not !N.useLeanFats then 
      E.s (E.bug "I thought that we weren't using lean fats\n")
  in
  let fetchHomeEnd (kind: int) (p: exp) : varinfo * varinfo * stmt = 
    let tmpb = makeTempVar !currentFunction voidPtrType in
    let tmpe = makeTempVar !currentFunction voidPtrType in
    tmpb, tmpe,
    call (Some (tmpb, false)) (Lval (var checkFindHomeEndFun.svar))
      [ integer kind ; castVoidStar p; mkAddrOf (var tmpe) ]
  in
  let fetchHome (kind: int) (p: exp) : varinfo * stmt = 
    let tmpb = makeTempVar !currentFunction voidPtrType in
    tmpb,
    call (Some (tmpb, false)) (Lval (var checkFindHomeFun.svar))
      [ integer kind; castVoidStar p ]
  in
  match oldk with
    N.WildT -> 
      let b, s = fetchHome registerAreaTaggedInt p in
      (Lval(var b)), zero, [ s ]
  | N.IndexT ->
      let b, s = fetchHome registerAreaSizedInt p in
      (Lval(var b)), zero, [ s ]

  | N.SeqT | N.SeqNT | N.FSeqT | N.FSeqNT -> 
      let b, e, s = fetchHomeEnd registerAreaSeqInt p in
      (Lval(var b)), (Lval(var e)), [ s ]
  | _ -> E.s (E.bug "Called fromTable on a non-table")

           

(* from table *)
let fromTableFexp (fe: fexp) : stmt list * fexp =
  let oldt, oldk, p, b, bend = breakFexp fe in
  let newk = N.stripT oldk in
  if newk = oldk then
    [], fe
  else
    let bt = 
      match unrollType oldt with
        TPtr(bt, _) -> bt
      | _ -> voidType
    in
    let newt = mkPointerTypeKind bt newk in
    let b, e, s = fromTable oldk p in
    s, mkFexp3 newt p b e
      

let checkWild (p: exp) (basetyp: typ) (b: exp) (blen: exp) : stmt = 
  (* This is almost like indexToSafe, except that we have the length already 
   * fetched *)
  call None (Lval (var checkBoundsLenFun.svar))
    [ castVoidStar b; blen;
      castVoidStar p; SizeOf (basetyp)]
      
  (* Check index when we switch from a sequence type to Safe, in preparation 
   * for accessing a field.  *)
let beforeField ((btype, pkind, mklval, base, bend, stmts) as input) = 
  match pkind with
    (* The kind is never a table type *)
    N.Wild -> input (* No change if we are in a tagged area *)
  | N.Safe -> input (* No change if already safe *)
  | N.Index -> 
      let _, _, _, docheck = 
        indexToSafe (mkAddrOf (mklval NoOffset)) 
          (TPtr(btype, [])) base bend []
      in
      (btype, N.Safe, mklval, zero, zero,
       stmts @ docheck)
        
  | (N.Seq|N.SeqN) -> 
      let _, _, _, docheck = 
        seqToSafe (mkAddrOf (mklval NoOffset)) (TPtr(btype,[])) base bend []
      in
      (btype, N.Safe, mklval, zero, zero,
       stmts @ docheck)
        
  | (N.FSeq|N.FSeqN) -> 
      let _, _, _, docheck = 
        fseqToSafe (mkAddrOf (mklval NoOffset)) (TPtr(btype,[])) base bend []
      in
      (btype, N.Safe, mklval, zero, zero,
       stmts @ docheck)
        
  | _ -> E.s (E.unimp "beforeField on unexpected pointer kind %a"
                N.d_opointerkind pkind)
        
    
let rec beforeIndex ((btype, pkind, mklval, base, bend, stmts) as input) = 
  (* The table is never a table type *)
  match pkind with
  | (N.Safe|N.Wild) -> 
      let (elemtype, pkind, base, bend) = 
        arrayPointerToIndex btype pkind (mklval NoOffset) base in
      (elemtype, pkind, mklval, base, bend, stmts)

  | (N.FSeq|N.FSeqN|N.Seq|N.SeqN|N.Index) ->   
      (* Convert to safe first *)
      let (_, pkind1, _, _, _, _) as res1 = beforeField input in
      if pkind1 != N.Safe then
        E.s (E.bug "beforeIndex: should be Safe\n");
      (* Now try again *)
      beforeIndex res1

  | _ -> E.s (E.unimp "beforeIndex on unexpected pointer kind %a"
                N.d_opointerkind pkind)

(******* Start of *******)
let rec pkStartOf (lv: lval)
              (lvt: typ)
              (lvk: N.opointerkind)  (* The kind of the StartOf pointer *)
              (fb: exp)
              (fe: exp) : (fexp * stmt list) = 
  match unrollType lvt with
    TArray(t, _, _) -> begin
      let newp = AddrOf(addOffsetLval (Index(zero, NoOffset)) lv) in
      match lvk with
        N.Safe -> 
          let (_, pkind, base, bend) = 
            arrayPointerToIndex lvt lvk lv fb
          in
          let pres = mkPointerTypeKind t pkind in
          mkFexp3 pres newp base bend, []

      | N.Wild -> 
          mkFexp3 (mkPointerTypeKind t lvk) newp fb zero, []

      | N.Seq|N.FSeq|N.Index -> 
          (* multi-dim arrays. Convert to SAFE first *)
          let (lvt', lvk', mklval', base', bend', stmts') = 
            beforeField (lvt,lvk, (fun o -> addOffsetLval o lv), fb, fe, []) in
          if lvk' <> N.Safe then
            E.s (E.bug "pkStartOf: I expected a safe here\n");
          let (res, stmts'') = pkStartOf lv lvt lvk' base' bend' in
          (res, stmts' @ stmts'')
          
      | _ -> E.s (E.unimp "pkStartOf: %a" N.d_opointerkind lvk)
    end
  | TFun _ -> begin
              (* Taking the address of a function is a special case. Since 
               * fuctions are not tagged the type of the the pointer is Safe. 
               * If we are in defaultIsWild then we must make a Wild pointer 
               * out of it *)
      let start = StartOf lv in
      match lv with
        Var vi, NoOffset when !N.defaultIsWild -> 
          mkFexp3 (mkPointerTypeKind lvt N.Wild) start start zero, []
      | _ -> 
          mkFexp3 (mkPointerTypeKind lvt lvk) start fb zero, []
  end
        
  | _ -> E.s (E.unimp "pkStartOf on a non-array and non-function: %a"
                d_plaintype lvt)

let varStartInput (vi: varinfo) = 
  vi.vtype, N.Safe, (fun o -> (Var vi, o)), zero, zero, []
  


let pkArithmetic (ep: exp)
                 (et: typ)
                 (ek: N.opointerkind) (* kindOfType et *)
                 (bop: binop)  (* Either PlusPI or MinusPI or IndexPI *)
                 (e2: exp) : (fexp * stmt list) = 
  let ptype, ptr, fb, fe = readFieldsOfFat ep et in
  match ek with
    N.Wild|N.Index|N.WildT|N.IndexT -> 
      mkFexp3 et (BinOp(bop, ptr, e2, ptype)) fb zero, []
  | (N.Seq|N.SeqN|N.SeqT|N.SeqNT) -> 
      mkFexp3 et (BinOp(bop, ptr, e2, ptype)) fb fe, []
  | (N.FSeq|N.FSeqN|N.FSeqT|N.FSeqNT) ->
      mkFexp3 et (BinOp(bop, ptr, e2, ptype)) fb fe, 
      [call None (Lval (var checkPositiveFun.svar)) [ e2 ]]
      
  | N.Safe ->
      E.s (E.bug "pkArithmetic: pointer arithmetic on safe pointer: %a@!"
             d_exp ep)
  | N.String|N.ROString -> 
      (* Arithmetic on strings is tricky. We must first convert to a FSeq and 
       * then do arithmetic. We leave it a SeqN to be converted back to 
       * string late if necessary *)
      let p', b', bend', acc' = stringToSeq ptr fb fe [] in
      (* Change the type from String into a SeqN pointer *)
      let ptype' = 
        match ptype with
          TPtr((TInt((IChar|ISChar|IUChar), _) as bt), ptra) -> 
            TPtr(bt, 
                 addAttribute (N.k2attr N.SeqN)
                   (dropAttribute ptra (N.k2attr N.String)))
        | _ -> E.s (E.bug "String pointer kind but base type is not char")
      in
      (* And recompute the right type for the result *)
      let et' = fixupType ptype' in
(*      ignore (E.log "pkArith: %a\n" d_plaintype ptype'); *)
      let p'' = BinOp(bop, p', e2, ptype') in
      mkFexp3 et' p'' b' bend', List.rev acc'
      
  | _ -> E.s (E.bug "pkArithmetic(%a)" N.d_opointerkind ek)
        


let rec checkBounds 
                (iswrite: bool) 
                (mktmplen: unit -> exp)
                (base: exp)
                (bend: exp)
                (lv: lval)
                (lvt: typ) 
                (pkind: N.opointerkind) : stmt list = 
  begin
    let lv', lv't = getHostIfBitfield lv lvt in
    (* Do not check the bounds when we access variables without array 
     * indexing  *)
    match pkind with
    | N.Wild -> (* We'll need to read the length anyway since we need it for 
                   * working with the tags *)
        let docheck = 
          checkWild (AddrOf(lv')) lv't base (mktmplen ()) in
        [docheck]
          
    | N.Index -> 
        let _, _, _, docheck = 
          indexToSafe (AddrOf(lv')) (TPtr(lv't, [])) base bend [] in
        List.rev docheck
          
    | (N.FSeq|N.FSeqN) ->
        let base' = 
          if pkind = N.FSeqN && not iswrite then 
          (* Allow reading of the trailing 0 *)
            castVoidStar (BinOp(PlusPI, 
                                doCast base charPtrType, one, charPtrType))
          else
            base
        in
        let _, _, _, docheck = 
          fseqToSafe (AddrOf(lv')) (TPtr(lv't, [])) base' bend [] in
        List.rev docheck
          
    | (N.Seq|N.SeqN) ->
        let bend' = 
          if pkind = N.SeqN && not iswrite then 
          (* Allow reading of the trailing 0 *)
            castVoidStar (BinOp(PlusPI, 
                                doCast bend charPtrType, one, charPtrType))
          else
            bend
        in
        let _, _, _, docheck = 
          seqToSafe (AddrOf(lv')) (TPtr(lv't, [])) base bend' [] in
        List.rev docheck
          
    | N.Safe | N.String | N.ROString -> begin
        match lv' with
          Mem addr, _ -> 
            [call None (Lval (var checkNullFun.svar)) [ castVoidStar addr ]]
        | _, _ -> []
    end

    | _ -> E.s (E.bug "Unexpected pointer kind in checkBounds(%a)"
                  N.d_opointerkind pkind)
  end



  

(****************************************************)


    (* Cast an fexp to another one. Accumulate necessary statements to doe *)
let rec castTo (fe: fexp) (newt: typ)
               (doe: stmt list) : stmt list * fexp =
  let newkind = kindOfType newt in
  match fe, newkind with
  (***** Catch the simple casts **********)
  | FS(oldt, oldk, e), _ when oldk = newkind -> 
      doe, FC(newt, newkind, oldt, oldk, e)
  | FC(oldt, oldk, prevt, prevk, e), _ when oldk = newkind -> 
      doe, FC(newt, newkind, prevt, prevk, e)
  (***** Now convert the source to an FM *****)
  | _, _ -> begin
      (* Get the pointer type of the new pointer type. Get inside fat 
       * pointers  *)
      let newPointerType =
        match newkind with
          N.Safe | N.Scalar | N.String | N.ROString |
          N.WildT | N.SeqT | N.FSeqT | N.SeqNT | N.FSeqNT | N.IndexT -> newt
        | _ -> 
            let pfield, _, _ = getFieldsOfFat newt in
            pfield.ftype 
      in
      (* Conver the tables to normal *)
      let doe1, fe = fromTableFexp fe in
      let doe = doe @ doe1 in
      (* Cast the pointer expression to the new pointer type *)
      let castP (p: exp) = doCast p newPointerType in
      (* Converts a reversed accumulator to doe *)
      let finishDoe (acc: stmt list) = doe @ (List.rev acc) in
      let oldt, oldk, p, b, bend = breakFexp fe in
      let is_zero fexp = 
        let rec is_zero_exp e = match e with
          Const(CInt(0,_,_)) -> true
        | CastE(_,e) -> is_zero_exp e
        | _ -> false
        in 
        match fexp with
          L(t,k,e) -> is_zero_exp e
        | _ -> false
      in
      match oldk, newkind with
        (* Catch the cases when the destination is a table *)
      | _, (N.WildT|N.SeqT|N.FSeqT|N.SeqNT|N.FSeqNT|N.IndexT) ->
          let newk' = N.stripT newkind in
          let newt' = 
            match unrollType newt with
              TPtr(bt, _) -> mkPointerTypeKind bt newk'
            | _ -> E.s (E.bug "castTo: strip table")
          in
          let doe', fe' = castTo fe newt' doe in
          let _, _, p', _, _ = breakFexp fe in
          (doe', mkFexp1 newt p')
          
        (* SCALAR, SAFE -> SCALAR, SAFE *)
      | (N.Scalar|N.Safe|N.String|N.ROString), 
        (N.Scalar|N.Safe|N.String|N.ROString) -> 
          (doe, L(newt, newkind, castP p))

        (* SAFE -> WILD. Only allowed for function pointers because we do not 
         * know how to tag functions, yet. But create a wild pointer with the 
         * base = to the value of the pointer. This is unsafe since it still 
         * allows pointer arthmetic and memory operations. *)
      | N.Safe, N.Wild 
            when (match unrollType oldt 
                     with TPtr(TFun _, _) -> true | _ -> false) -> 
              (doe, mkFexp3 newt (castP p) (castP p) zero)

        (* SAFE -> FSEQ *)          
      | N.Safe, N.FSeq -> 
          let p' = castP p in
          (* If the pointer type is a void ptr then do not add one to get the 
           * end since that is illegal C *)
          let theend = 
            match unrollType newPointerType with
              TPtr(TVoid _, _) -> p'
            | _ -> BinOp(PlusPI, p', one, newPointerType)
          in
          (doe, FM (newt, newkind, p', theend, zero))

        (* weimer: SAFE -> FSEQN only when the SAFE is 0 *)
      | N.Safe, N.FSeqN when is_zero fe  ->
          let p' = castP p in
          (doe, FM (newt, newkind, p', zero, zero))
      | N.Safe, N.SeqN when is_zero fe ->
          let p' = castP p in
          (doe, FM (newt, newkind, p', zero, zero))

        (* SAFE -> SEQ *)          
      | N.Safe, N.Seq -> 
          let p' = castP p in
          (* If the pointer type is a void ptr then do not add one to get the 
           * end since that is illegal C *)
          let theend = 
            match unrollType newPointerType with
              TPtr(TVoid _, _) -> p'
            | _ -> BinOp(PlusPI, p', one, newPointerType)
          in
          (doe, FM (newt, newkind, p', p', theend))
          
        (* SCALAR -> INDEX, WILD, SEQ, FSEQ *)
      | N.Scalar, (N.Index|N.Wild|N.Seq|N.FSeq|N.FSeqN|N.SeqN) ->
          if not (isZero p) then
            ignore (E.warn "Casting scalar (%a) to pointer in %s!"
                      d_exp p !currentFunction.svar.vname);
          let newbase, doe' = 
            if !interceptCasts && (isInteger p = None) then begin
              incr interceptId;
              let tmp = makeTempVar !currentFunction voidPtrType in
              Lval(var tmp),
              doe @
              [call (Some (tmp, false)) (Lval(var interceptCastFunction.svar)) 
                  [ p ;integer !currentFileId; integer !interceptId ]
              ]
            end else 
              doCast zero voidPtrType, doe
          in
          (doe', FM (newt, newkind, castP p, newbase, zero))


       (* WILD, INDEX, SEQ, FSEQ -> SCALAR *)
      | (N.Index|N.Wild|N.FSeq|N.Seq|N.FSeqN|N.SeqN), N.Scalar ->
          (doe, L(newt, newkind, castP p))

       (* WILD, INDEX, SEQ, FSEQ -> same_kind *)  
      | (N.Index|N.Wild|N.FSeq|N.Seq|N.FSeqN|N.SeqN), _ when newkind =oldk -> 
          (doe, FM (newt, newkind, castP p, b, bend))

       (* INDEX -> SAFE. Must do bounds checking *)
      | N.Index, N.Safe ->
          let p', _, _, acc' = indexToSafe p newPointerType b bend [] in
          finishDoe acc', L(newt, newkind, castP p')      
       (* INDEX -> SEQ *)
      | N.Index, N.Seq ->
          let p', b', bend', acc' = indexToSeq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      
       (* INDEX -> FSEQ *)
      | N.Index, N.FSeq ->
          let p', b', bend', acc' = indexToFSeq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      

       (* SEQ -> SAFE. Must do bounds checking *)
      | (N.Seq|N.SeqN), N.Safe ->
          let p', _, _, acc' = seqToSafe p newPointerType b bend [] in
          finishDoe acc', L(newt, newkind, castP p')      
       (* SEQ -> FSEQ *)
      | (N.Seq|N.SeqN), N.FSeq ->
          let p', b', bend', acc' = seqToFSeq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      
      | N.SeqN, N.FSeqN ->
          let p', b', bend', acc' = seqToFSeq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      

       (* FSEQ -> SAFE. Must do bounds checking *)
      | (N.FSeq|N.FSeqN), N.Safe ->
          let p', _, _, acc' = fseqToSafe p newPointerType b bend [] in
          finishDoe acc', L(newt, newkind, castP p')      

       (* FSEQ -> SEQ. *)
      | (N.FSeq|N.FSeqN), N.Seq ->
          doe, FM(newt, newkind, castP p, b, b)
      | N.FSeqN, (N.Seq|N.SeqN) ->
          doe, FM(newt, newkind, castP p, b, b)
      | N.FSeqN, N.FSeq -> 
          (doe, FM (newt, newkind, castP p, b, bend))

      (* SeqN -> SEQ *)
      | N.SeqN, N.Seq -> 
          doe, FM(newt, newkind, castP p, b, bend)
          
      | N.SeqN, (N.String|N.ROString) ->
          let p', b', bend', acc' = seqNToString p newPointerType b bend [] in
          finishDoe acc', L(newt, newkind, castP p')  

      | N.FSeqN, (N.String|N.ROString) ->
          let p', b', bend', acc' = fseqNToString p newPointerType b bend [] in
          finishDoe acc', L(newt, newkind, castP p')  

      | N.String, (N.FSeqN|N.FSeq) ->
          let p', b', bend', acc' = stringToFseq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend') 
          (* wes: was ( p', b', bend') at the end *)

      | N.String, (N.SeqN|N.Seq) ->
          let p', b', bend', acc' = stringToSeq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')  

      | N.Wild, N.ROString -> 
          let p', b', bend', acc' = wildToROString p b bend [] in
          finishDoe acc', L(newt, newkind, castP p')

      | N.Index, N.ROString -> 
          let p', b', bend', acc' = indexToROString p b bend [] in
          finishDoe acc', L(newt, newkind, castP p')

      | N.ROString, (N.FSeq|N.FSeqN) -> 
        ignore (E.warn "Warning: wes-is-lazy cast from ROSTRING -> FSEQ[N]") ;
        ignore (E.warn "castTo(%a -> %a.@!%a@!%a)" 
                 N.d_opointerkind oldk N.d_opointerkind newkind 
                 d_fexp fe
                 d_plaintype oldt)       ;
          let p', b', bend', acc' = stringToFseq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', bend', zero) 

(*
      | N.Safe, N.SeqN -> 
          ignore (E.warn "Warning: wishful thinking cast from SAFE -> SEQN");
          (doe, FM(newt, newkind, castP p, zero, zero))
          *)

       (******* UNIMPLEMENTED ********)
      | N.String, N.Wild 
            when 
          (match p with 
            Const(CStr s) when prefix "booo_exp: " s -> true 
          | _ -> false) -> (* This occurs because such strings are generated 
                            * in case of error *)
              (doe, FM(newt, newkind, castP p, zero, zero))

      | _, _ -> 
          E.s (E.unimp "castTo(%a -> %a.@!%a@!%a)" 
                 N.d_opointerkind oldk N.d_opointerkind newkind 
                 d_fexp fe
                 d_plaintype oldt)      
  end




(* For each function cache some iterator variables. *)
let iterVars : (int, varinfo list ref) H.t = H.create 13
let withIterVar (f: fundec) (doit: varinfo -> 'a) : 'a = 
  let avail = 
    try  H.find iterVars f.svar.vid
    with Not_found -> begin
      let iters = ref [] in
      H.add iterVars f.svar.vid iters; 
      iters
    end
  in
  let newv = 
    match !avail with
      v :: resta -> avail := resta; v
    | [] -> makeTempVar f ~name:"iter" intType
  in
  let res = doit newv in
  avail := newv :: !avail;
  res
  

(* Various reasons why we might want to check an LV *)  
type checkLvWhy = 
    ToWrite of exp
  | ToRead
  | ToSizeOf  (* Like ToRead but we do not need to check anything *)

let rec checkMem (why: checkLvWhy) 
                 (lv: lval) (base: exp) (bend: exp)
                 (lvt: typ) (pkind: N.opointerkind) : stmt list = 
  (* ignore (E.log "checkMem: lvt: %a\n" d_plaintype lvt); *)
  (* Maybe it is a table. In that case, get the true base and end *)
  (* See if a table pointer *)
  let newk = N.stripT pkind in 
  if newk <> pkind then begin (* A table pointer *)
    let base, bend, stmts = fromTable pkind (mkAddrOf lv) in
    stmts @ (checkMem why lv base bend lvt newk)
  end else begin
    (* Fetch the length field in a temp variable. But do not create the 
     * variable until certain that it is needed  *)
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
    (* Now the tag checking. We only care about pointers. We keep track of 
     * what we write in each field and we check pointers in a special way.  *)
    let rec doCheckTags (why: checkLvWhy) (where: lval) 
        (t: typ) (pkind: N.opointerkind) acc = 
      match unrollType t with 
      | (TInt _ | TFloat _ | TEnum _ | TBitfield _ ) -> acc
(*    | TFun _ -> acc *)
      | TComp (_, comp, _) when isFatComp comp -> begin (* A fat pointer *)
          match why with
            ToRead -> (* a read *)
              if pkind = N.Wild then
                (checkFatPointerRead base 
                   (AddrOf(where)) (getLenExp ())) :: acc
              else
                acc
          | ToWrite towrite -> (* a write *)
              let _, whatp, whatb, _ = readFieldsOfFat towrite t in
              if pkind = N.Wild then
                (checkFatPointerWrite base (AddrOf(where)) 
                   whatb whatp (getLenExp ())) :: acc
              else
                checkFatStackPointer whatp whatb :: acc
          | ToSizeOf -> acc
      end 
      | TComp (_, comp, _) when comp.cstruct -> 
          let doOneField acc fi = 
            let newwhere = addOffsetLval (Field(fi, NoOffset)) where in
            let newwhy = 
              match why with 
                ToRead -> ToRead
              | ToWrite (Lval whatlv) -> 
                  ToWrite (Lval (addOffsetLval (Field(fi, NoOffset)) whatlv))
                  (* sometimes in Asm outputs we pretend that we write 0 *)
              | ToWrite (Const(CInt(0, _, _))) -> ToRead
              | ToWrite e -> E.s (E.unimp "doCheckTags (%a)" d_exp e)
              | ToSizeOf -> why
            in
            doCheckTags newwhy newwhere fi.ftype pkind acc
          in
          List.fold_left doOneField acc comp.cfields
            
      | TArray(bt, lo, a) -> begin
          match unrollType bt with
            TInt _ | TFloat _ | TEnum _ | TBitfield _ -> acc
          | _ -> begin (* We are reading or writing an array *)
              let len = 
                match lo with Some len -> len 
                | _ -> E.s (E.unimp "Reading or writing an incomplete type") in
            (* Make an interator variable for this function *)
              withIterVar !currentFunction
                (fun it -> 
                  let itvar = Lval (var it) in
                (* make the body to initialize one element *)
                  let initone = 
                    let whyelem = 
                      match why with
                        ToRead -> ToRead
                      | ToWrite (Lval whatlv) -> 
                          ToWrite (Lval (addOffsetLval (Index(itvar, 
                                                              NoOffset)) 
                                           whatlv))
                      | ToWrite e -> 
                          E.s (E.unimp "doCheckTags: write (%a)" d_exp e)
                      | ToSizeOf -> why
                    in
                    doCheckTags whyelem
                      (addOffsetLval (Index(itvar, NoOffset)) where)
                      bt
                      pkind (* ??? *)
                      []
                  in
                  (mkForIncr 
                     ~iter: it
                     ~first: zero
                     ~stopat: len
                     ~incr: one
                     ~body: initone) @ acc)
          end
      end
            
      | TPtr(_, _) -> (* This can only happen if we are writing to an untagged 
                         * area. All other areas contain only fat pointers *)
          begin
            match why with
              ToWrite x -> checkLeanStackPointer x :: acc
            | _ -> acc
          end
      | _ -> E.s (E.unimp "unexpected type in doCheckTags: %a\n" d_type t)
    in
  (* See first what we need in order to check tags *)
    let zeroAndCheckTags = 
    (* Call doCheckTags anyway because even for safe writes it needs to check 
       * when pointers are written *)
      let dotags = doCheckTags why lv lvt pkind [] in
      if pkind = N.Wild then
        match why with 
        | ToWrite _ -> (checkZeroTags base (getLenExp ()) lv lvt) :: dotags
        | _ -> dotags
      else
        dotags
    in
  (* Now see if we need to do bounds checking *)
    let iswrite = (match why with ToWrite _ -> true | _ -> false) in
    let checkb = 
      (checkBounds iswrite getLenExp base bend lv lvt pkind) 
      @ zeroAndCheckTags in
  (* See if we need to generate the length *)
    (match !lenExp with
      None -> checkb
    | Some _ -> 
        let ptr = ptrOfBase base in
        (checkFetchLength (getVarOfExp (getLenExp ())) 
                          (takeAddressOfBitfield lv lvt) base) :: checkb)
  end
  
          
    (* Check a write *)
let checkRead = checkMem ToRead
let checkWrite e = checkMem (ToWrite e)



(***** Check the return value *)
let rec checkReturnValue 
    (typ: typ) 
    (e: exp)
    (acc: stmt list) : 

    (* Return the accumulated statements *)
    stmt list = 
  match unrollType typ with
    TInt _ | TBitfield _ | TEnum _ | TFloat _ | TVoid _ -> acc
  | TPtr (t, _) -> 
      (* This is a lean pointer *) 
      checkLeanStackPointer e :: acc

  | TComp (_, comp, _) when isFatComp comp -> 
      let ptype, ptr, fb, fe = readFieldsOfFat e typ in
      (* Get the component that is null if an integer *)
      let nullIfInt = 
        match kindOfType ptype with
          N.Wild|N.Index|N.Seq|N.SeqN -> fb
        | N.FSeq|N.FSeqN -> fe
        | _ -> E.s (E.unimp "checkReturn: unexpected kind of fat type")
      in
      checkFatStackPointer ptr nullIfInt :: acc

    (* A regular struct *)                                          
  | TComp (_, comp, _) when comp.cstruct ->
      (* Better have an lvalue *)
      let lv = match e with
        Lval lv -> lv
      | _ -> E.s (E.unimp "checkReturnValue: return comp not an lval")
      in
      List.fold_left 
        (fun acc f -> 
          checkReturnValue f.ftype 
            (Lval (addOffsetLval (Field(f, NoOffset)) lv)) acc)
        acc
        comp.cfields
        
  | _ -> E.s (E.unimp "checkReturnValue: type\n")
      

(********** Initialize variables ***************)
let rec initializeType
    (t: typ)   (* The type of the lval to initialize *)
    (withivar: (varinfo -> 'a) -> 'a) (* Allocate temporarily an iteration 
                                       * variable  *)
    (mustZero: bool)   (* The area is not zeroed already *)
    (endo: exp option) (* The end of the home area. To be used for 
                        * initializing arrays of size 0  *)

    (* Produces a function that, when given an lval of the given type, 
     * accumulates (prepends) some initialization statements to a give 
     * accumulator  *)
    : (lval -> stmt list -> stmt list) =
  match unrollType t with
    TInt _ | TFloat _ | TBitfield _ | TEnum _ -> (fun lv acc -> acc)
  | TFun _ -> (fun lv acc -> acc) (* Probably a global function prototype *)
  | TVoid _ -> fun lv acc -> acc (* allocating and returning a void* *)
  | TPtr (bt, a) -> begin
          (* If a non-wild pointer then initialize to zero *)
      let mustinit = 
        mustZero &&
        (match N.kindOfAttrlist a with
          N.Wild, _ -> false
        | N.Unknown, _ when !N.defaultIsWild -> false
        | _ -> true) 
      in
      if mustinit then
        
        fun lv acc -> mkSet lv (doCastT zero intType t) :: acc
      else 
        fun lv acc -> acc
  end
  | TComp (_, comp, a) when comp.cstruct -> begin (* A struct *)
      match comp.cfields with
        [s; a] when s.fname = "_size" && a.fname = "_array" ->
              (* Sized arrays *)
          let bt, sizeo = 
            match unrollType a.ftype with
              TArray(bt, lo,_) -> bt,lo
              | _ -> E.s (E.bug "SIZED array is not an array\n")
          in
          (* Construct the array initializer *)
          (* Prepare the initializer for one element *)
          let initone = initializeType bt withivar mustZero None in
              (* ignore (E.log "Initializing sized for %s\n" v.vname); *)
          fun lv acc -> 
            let thesizelv = addOffsetLval (Field(s, NoOffset)) lv in
            let thearraylv = addOffsetLval (Field(a, NoOffset)) lv in
            let l, thissize = 
              match sizeo with
                Some l when not (isZero l) -> 
                  l, (BinOp(Mult, doCast l uintType, SizeOf(bt), uintType))
                    
              | _ -> begin
                  match endo with
                    Some e -> 
                          (* We know the end of the area *)
                      let sz = 
                        BinOp(MinusA, e, 
                              doCast (mkAddrOf thearraylv) uintType, 
                              uintType) in
                    (BinOp(Div, sz, SizeOf(bt), uintType)), 
                    sz
                      
                | None -> 
                    ignore 
                      (E.warn "Initializing SIZED open array with len 0: %a" 
                         d_exp (mkAddrOf thesizelv));
                    zero, zero
              end
            in
             (* Register the sized array *)
            let acc1 = 
              registerArea [ integer registerAreaSizedInt;
                             castVoidStar (mkAddrOf thearraylv);
                             castVoidStar zero ] acc 
            in
             (* Set the size *)
            let acc2 = mkSet thesizelv thissize :: acc1 in
            withivar 
              (fun iter -> 
                (mkForIncr iter zero l one
                   (initone 
                      (addOffsetLval (Index (Lval(var iter), NoOffset)) 
                         thearraylv)
                      [])) @ acc2)

      | _ -> (* A regular struct. Do all the fields in sequence *)
          List.fold_left 
            (fun initsofar fld -> 
              let initone = initializeType fld.ftype withivar mustZero endo in
              fun lv acc ->
                initone (addOffsetLval (Field(fld, NoOffset)) lv) 
                  (initsofar lv acc))
            (fun lv acc -> acc)
            comp.cfields
  end
  | TArray(bt, Some l, a) -> 
      if filterAttributes "nullterm" a <> [] && mustZero then begin
            (* Write a zero at the very end *)
        (match unrollType bt with
          TInt((IChar|ISChar|IUChar), _) -> ()
        | _ -> E.s (E.unimp "NULLTERM array of base type %a" d_type bt));
        fun lv acc -> 
          mkSet (addOffsetLval 
                   (Index(BinOp(MinusA, l, one, intType), NoOffset)) lv)
            zero :: acc
      end else begin
        (* Prepare the initializer for one element *)
        let initone = initializeType bt withivar mustZero endo in
        (* Register the array begining and the end *)
        fun lv acc ->
          (* Register the array *)
          let acc1 = 
            registerArea 
              [ integer registerAreaSeqInt;
                castVoidStar (mkAddrOf lv);
                castVoidStar (mkAddrOf (addOffsetLval 
                                          (Index(l, NoOffset)) lv)) ] acc
          in
          withivar 
            (fun iter -> 
              (mkForIncr iter zero l one
                 (initone 
                    (addOffsetLval (Index (Lval(var iter), NoOffset)) lv)
                    [])) @ acc1)
      end
    (* A union type *)
  | TComp (_, comp, a) -> begin
      (* Go through all of the fields and find the one that is largest. 
       * Initialize that one. *)
      let (maxfld, themax) = 
        List.fold_left 
          (fun (bestsofar, bestval) f -> 
            let bitsthis = 
              try bitsSizeOf f.ftype with Not_found -> 
                E.s (E.unimp "initializing union with open fields")
            in
            if bitsthis > bestval then
              (Some f, bitsthis)
            else
              (bestsofar, bestval)) (None, -1) comp.cfields in
      let toinit = 
        match maxfld with Some f -> f 
        | _ -> E.s (E.unimp "cannot find widest field in union %s" comp.cname)
      in
      (* ignore (E.log "Will initialize field %s (with size %d)\n" 
                toinit.fname themax); *)
      let initone = initializeType toinit.ftype withivar mustZero endo in
      fun lv acc ->
        initone (addOffsetLval (Field(toinit, NoOffset)) lv) acc 
    end

  | _ -> E.s (E.unimp "initializeType (for type %a)" d_plaintype t)

    
    

(* Create and accumulate the initializer for a variable *)
let initializeVar (withivar: (varinfo -> 'a) -> 'a) (* Allocate an iteration 
                                                     * variable temporarily *)
                  (acc: stmt list)
                  (v: varinfo) 
                   : stmt list = 
  (* Maybe it must be tagged *)
  if mustBeTagged v then begin
   (* Generates code that initializes vi. Needs "iter", an integer variable 
    * to be used as a for loop index  *)
    withivar
      (fun iter -> 
        let dfld, lfld, tfld, words, tagwords = splitTagType v.vtype in
        (* Prepare the registration *)
        let acc' = 
          registerArea
            [ integer registerAreaTaggedInt; 
              castVoidStar (mkAddrOf (Var v, Field(dfld, NoOffset)));
              castVoidStar zero ]
            acc
        in
        (* Write the length *)
        mkSet (Var v, Field(lfld, NoOffset)) words ::
        (* And the loop to initialize the tags with zero *)
        (if not v.vglob then
          mkForIncr iter zero (doCast tagwords intType) one 
            [mkSet (Var v, Field(tfld, Index (Lval(var iter), NoOffset))) 
                zero ]
          @
          acc'
        else
          acc'))
  end else begin
    let doinit = initializeType v.vtype withivar (not v.vglob) None in
    doinit (Var v, NoOffset) acc
  end

let rec stringLiteral (s: string) (strt: typ) : stmt list * fexp = 
  let fixChrPtrType = fixupType strt in
  let k = kindOfType fixChrPtrType in
  match  k with 
    N.Wild -> 
          (* Make a global variable that stores this one, so that we can 
           * attach a tag to it  *)
      let l = 1 + String.length s in 
      let newt = tagType (TArray(charType, Some (integer l), [])) in
      let gvar = makeGlobalVar (newStringName ()) newt in
      gvar.vstorage <- Static;
      let varinit, dfield = 
        makeTagCompoundInit newt (Some (SingleInit(Const(CStr s)))) in
      theFile := GVar (gvar, Some varinit, lu) :: !theFile;
      let result = StartOf (Var gvar, Field(dfield, NoOffset)) in
      let voidStarResult = castVoidStar result in
      (* Register the area *)
      let regarea = 
        registerArea
          [ integer registerAreaTaggedInt;
            voidStarResult; zero ] [] 
      in
      (* Add the registration to the global initializer *)
      extraGlobInit := regarea @ !extraGlobInit;
      ([], FM (fixChrPtrType, N.Wild,
               result, 
               castVoidStar result, zero))
  | N.Seq | N.Safe | N.FSeq | N.String | N.ROString | N.SeqN | N.FSeqN -> 
      let l = (if isNullTerm k then 0 else 1) + String.length s in
      let tmp = makeTempVar !currentFunction charPtrType in
      (* Make it a SEQ for now *)
      let theend = BinOp(IndexPI, Lval (var tmp), integer l, charPtrType) in
      (* Register the area *)
      let regarea = 
        registerArea
          [ integer registerAreaSeqInt;
            castVoidStar (Lval (var tmp)); 
            castVoidStar theend ] []
      in
      (* Add the registration to the global initializer *)
      extraGlobInit := regarea @ !extraGlobInit;
      let res = 
        match k with 
          N.Safe | N.String | N.ROString -> 
            mkFexp1 fixChrPtrType (Lval (var tmp))
        | N.Seq | N.SeqN | N.FSeq | N.FSeqN -> 
            mkFexp3  fixChrPtrType 
              (Lval (var tmp))
              (Lval (var tmp))
              theend 

        | _ -> E.s (E.bug "stringLiteral")
      in
      (mkSet (var tmp) (Const (CStr s)) :: [], res)
        
  | N.WildT | N.SeqT | N.FSeqT | N.SeqNT | N.FSeqNT -> 
      let kno_t = N.stripT k in
      let strtno_t = 
        match strt with
          TPtr(chrt, a) -> TPtr(chrt, [N.k2attr kno_t])
        | _ -> E.s (E.bug "Making a string of a non char type\n")
      in
      let s1, fe = stringLiteral s strtno_t in
      (* Now cast it to the desired string type *)
      castTo fe fixChrPtrType s1

  | _ -> E.s (E.unimp "String literal to %a" N.d_opointerkind k)


(*************** Handle Allocation ***********)
let pkAllocate (ai:  allocInfo) (* Information about the allocation function *)
               (vi:  varinfo)   (* Where to put the result *)
               (f:  exp)        (* The allocation function *)
               (args: exp list) (* The arguments passed to the allocation *) 
    : stmt list = 
(*  ignore (E.log "Allocation call of %a. type(vi) = %a@! vtype = %a@!" 
            d_exp f d_plaintype vi.vtype
            d_plaintype vtype);  *)
  let k = kindOfType vi.vtype in
  let kno_t = N.stripT k in
  (* Get the size *)
  let sz = ai.aiGetSize args in
  (* See if we must zero *)
  let mustZero = not ai.aiZeros in
  (* Round up the size to be allocated *)
  let nrdatawords, nrtagwords = tagLength sz in
  (* Words to bytes converter *)
  let wrdsToBytes wrds = 
    BinOp(Shiftlt, doCast wrds uintType, integer 2, uintType) in
  let nrdatabytes = wrdsToBytes nrdatawords in

  (* Find the pointer type and the offset where to save it *)
  let ptrtype, ptroff = 
    match k with 
      N.Wild | N.Seq | N.FSeq | N.SeqN | N.FSeqN | N.Index -> 
        let fptr, fbase, fendo = getFieldsOfFat vi.vtype in 
        fptr.ftype, Field(fptr, NoOffset)
    | N.Safe | N.String 
    | N.WildT | N.SeqT | N.FSeqT | N.SeqNT | N.FSeqNT | N.IndexT 
      -> vi.vtype, NoOffset
    | _ -> E.s (E.unimp "pkAllocate: ptrtype (%a)" N.d_opointerkind k)
  in
  (* Get the base type *)
  let basetype = 
    match unrollType ptrtype with
      TPtr(bt, _) -> bt
    | _ -> E.s (E.bug "Result of allocation is not a pointer type\n")
  in

  (* Compute the size argument to be passed to the allocator *)
  let allocsz = 
    match kno_t with 
      N.Wild -> 
        wrdsToBytes (BinOp(PlusA, nrdatawords,
                           BinOp(PlusA, nrtagwords, kinteger IUInt 1, 
                                 uintType), uintType))
    | N.Index -> 
        wrdsToBytes (BinOp(PlusA, nrdatawords, kinteger IUInt 1, uintType))
    | _ -> nrdatabytes
  in
      (* Call the allocation function and put the result in a temporary *)
  let tmpp = makeTempVar !currentFunction ptrtype in
  let tmpvar = Lval(var tmpp) in
  let alloc = call (Some (tmpp, true)) f (ai.aiNewSize allocsz args) in
  (* Adjust the allocation pointer *)
  let adjust_ptr = 
    match kno_t with
      N.Index | N.Wild -> 
        mkSet (var tmpp) (doCast (BinOp(IndexPI, 
                                        doCast tmpvar charPtrType, 
                                        integer 4, charPtrType))
                            ptrtype)
    | _ -> mkEmptyStmt ()
  in

  (* Save the pointer value *)
  let assign_p = mkSet (Var vi, ptroff) tmpvar in
  (* And the base, if necessary *)
  let assign_base = 
    match k with 
      N.Wild | N.Seq | N.SeqN | N.Index -> begin
        let fptr, fbaseo, fendo = getFieldsOfFat vi.vtype in
        match fbaseo with
          Some fbase -> (mkSet (Var vi, Field(fbase, NoOffset))
                           (doCast tmpvar voidPtrType))
        | _ -> mkEmptyStmt ()
      end
    | _ -> mkEmptyStmt ()
  in

  (* Set the size if necessary *)
  let setsz = 
    match kno_t with
      N.Wild | N.Index -> 
        mkSet (Mem(BinOp(PlusA, 
                         doCast tmpvar uintPtrType,
                         mone, uintPtrType)), 
               NoOffset) 
          nrdatawords
    | _ -> mkEmptyStmt ()
  in

  (* Now the remainder of the initialization *)
  let init = 
    (* Put nullterm *)
    let putnullterm = 
      mkSet (Mem(BinOp(PlusPI,
                       doCast tmpvar charPtrType,
                       BinOp(MinusA, nrdatabytes, one, intType), 
                       charPtrType)),
             NoOffset)
        (doCast zero charType)
    in
    match kno_t with
      N.Wild -> 
        (* Zero the tags *)
        if mustZero then
          call None
            (Lval (var checkZeroTagsFun.svar))
            [ tmpvar;                      (* base *)
              nrdatawords;                 (* basenrwords *)
              tmpvar;                      (* where to start *)
              nrdatabytes;                 (* size of area to zero *)
              zero (* offset *) ] ::
          []  
        else
          [mkEmptyStmt ()]
    | N.Safe -> 
        (* Check that we have allocated enough for at least 1 elem. *)
        let check_enough = 
          call None (Lval (var checkPositiveFun.svar))
            [ BinOp(MinusA, doCast nrdatabytes intType, 
                    doCast (SizeOf(basetype)) intType, intType) ] in
        (* Compute the end *)
        let theend = BinOp(PlusPI, doCast tmpvar uintType,
                           nrdatabytes, uintType) in
        (* Now initialize. *)
        let inits = 
          initializeType basetype (withIterVar !currentFunction) mustZero
            (Some theend) (Mem tmpvar, NoOffset) []
        in
        check_enough :: inits

    | N.Seq | N.FSeq | N.SeqN | N.FSeqN | N.Index ->
        (* Compute and save the end of the type *)
        let savetheend, theend = 
          let tmpend = makeTempVar !currentFunction uintType in
          mkSet (var tmpend)
            (BinOp(PlusPI, doCast tmpvar uintType,
                   nrdatabytes, uintType)),
          Lval (var tmpend)
        in
        (* Now initialize. Use the tmp variable to iterate over a number of 
         * copies  *)
        let initone = 
          initializeType basetype
            (withIterVar !currentFunction) mustZero None
            (Mem tmpvar, NoOffset) []
            
        in
        let initializeAll = 
          if initone = [] then 
            [ mkSet (var tmpp) (doCast theend ptrtype) ]
          else 
            mkFor 
              ~start:[mkEmptyStmt ()]
              ~guard:(BinOp(Le, BinOp(PlusA, 
                                      doCast tmpvar uintType, 
                                      SizeOf(ptrtype), uintType),
                            doCast theend uintType, intType))
              ~next:[(mkSet (var tmpp) 
                        (BinOp(IndexPI, tmpvar, one, ptrtype)))]
              ~body:initone
        in 
        savetheend :: initializeAll
        @
        (if k = N.FSeqN || k = N.SeqN then [putnullterm] else [])

    | N.String -> (* Allocate this as SeqN, with a null term *)
        ignore (E.warn "Allocation of string. Use FSEQN instead. (%a)\n"
                  d_lval (var vi));
        [putnullterm]

    | _ -> E.s (E.bug "pkAllocate: init")
  in
  (* Now assign the end if necessary. We do it this late because in the case 
   * of sequences we now know the precise end of the allocated sequence  *)
  let assign_end = 
    match k with 
      N.Seq | N.SeqN | N.FSeq | N.FSeqN -> begin
        let fptr, fbase, fendo = getFieldsOfFat vi.vtype in
        match fendo with
          None -> mkEmptyStmt ()
        | Some fend -> mkSet (Var vi, Field(fend, NoOffset)) tmpvar
      end
    | _ -> mkEmptyStmt ()
  in
  (* Now see if we must register the whole area *)
  let register_area = 
    match kno_t with
    | N.Safe -> [] 
    | N.Wild | N.Index -> 
        let areaKind = 
          if kno_t = N.Wild then 
            registerAreaTaggedInt else registerAreaSizedInt
        in
        registerArea [ integer areaKind;
                       castVoidStar (Lval (Var vi, ptroff));
                       zero ] []
    | N.Seq | N.SeqN | N.FSeq | N.FSeqN -> 
        registerArea [ integer registerAreaSeqInt;
                       castVoidStar (Lval (Var vi, ptroff));
                       castVoidStar tmpvar ] []
    | _ -> E.s (E.bug "pkAllocate: register_area: %a" N.d_opointerkind k)
  in        
  alloc :: adjust_ptr :: assign_p :: 
  assign_base :: setsz :: (init @ (assign_end :: register_area))

(* Given a sized array type, return the size and the array field *)
let getFieldsOfSized (t: typ) : fieldinfo * fieldinfo = 
  match unrollType t with
   TComp (_, comp, _) when comp.cstruct -> begin
      match comp.cfields with 
        s :: a :: [] when s.fname = "_size" && a.fname = "_array" -> s, a
      | _ -> E.s (E.bug "getFieldsOfSized")
    end
   | _ -> E.s (E.bug "getFieldsOfSized %a\n" d_type t)
  


(* Remember names that we have already mangled *)
let mangledNames : (string, unit) H.t = H.create 123
(* Remeber if we mangled the name of main *)
let mangledMainName : string ref = ref "main"
let fixupGlobName vi =
  (* Scan a type and compute a list of qualifiers that distinguish the
   * various possible combinations of qualifiers *)
   let rec qualNames acc = function
      TInt _ | TFloat _ | TBitfield _ | TVoid _ | TEnum _ -> acc
    | TPtr(t', _) as t -> 
        let pk = kindOfType t in
        pkQualName pk acc (fun acc' -> qualNames acc' t')
    | TArray(t', _, a) ->
        let acc' =
          (* Choose the attributes so that "s" is always the C represent *)
          if filterAttributes "sized" a <> [] then "l" :: acc else "s" :: acc
        in
        qualNames acc' t'
    | TFun(tres, args, _, _) -> 
        let acc' = qualNames acc tres in
        List.fold_left (fun acc a -> qualNames acc a.vtype) acc' args 

    | TNamed (_, t, _) -> qualNames acc t

    (* We only go into struct that we created as part of "sized" or "seq" or 
     * "fatp" *)
    | TComp (false, comp, _) -> begin
        try
          let data_type = 
            match comp.cfields with
              [p;b] when p.fname = "_p" && b.fname = "_b" -> p.ftype
            | [p;b;e] when p.fname = "_p" && b.fname = "_b" && e.fname = "_e" 
              -> p.ftype
            | [s;a] when s.fname = "_size" && a.fname = "_array" -> a.ftype
            | _ -> raise Not_found
          in
          qualNames acc data_type
        with Not_found -> acc
    end
    | TComp _ -> acc  (* Do not go into recursive structs *)
  in
  (* weimer: static things too! *)
  if vi.vglob && (* vi.vstorage <> Static &&  *)
    not (H.mem leaveAlone vi.vname) &&
    not (isAllocFunction vi.vname) &&
    not (H.mem mangledNames vi.vname) then
    begin
      let quals = qualNames [] vi.vtype in
      let rec allSafe = function (* Only default qualifiers *)
          [] -> true
        | "s" :: rest -> allSafe rest
        | _ -> false
      in
      let newname =
        if allSafe quals then vi.vname
        else
          vi.vname ^ "_" ^ (List.fold_left (fun acc x -> x ^ acc) "" quals)
      in
      H.add mangledNames newname ();
      if vi.vname = "main" && vi.vstorage <> Static then
        mangledMainName := newname;
      vi.vname <- newname
    end

(*** Intercept some function calls *****)
let interceptCall 
    (reso: (varinfo * bool) option)
    (func: exp)
    (args: exp list) : stmt = 
  call reso func args
                 


    (************* STATEMENTS **************)
let rec boxblock (b: block) : block = 
  compactBlock (List.fold_left (fun acc s -> acc @ (boxstmt s)) [] b)

and boxstmt (s: Cil.stmt) : block = 
   (* Keep the original statement, but maybe modify its kind. This way we 
    * maintain the labels and we have no need to change the Gotos and the 
    * cases in the Switch *)
  try
    match s.skind with 
    | Break _ | Continue _ | Goto _ -> [s]
    | Return (None, l) -> unregisterStmt () :: [ s ]

    | Return (Some e, l) -> 
        let retType =
          match !currentFunction.svar.vtype with 
            TFun(tRes, _, _, _) -> tRes
          | _ -> E.s (E.bug "Current function's type is not TFun")
        in 
        let (doe', e') = boxexpf e in
        let (doe'', e'') = castTo e' retType doe' in
        let (et, doe2, e2) = fexp2exp e'' doe'' in
        let doe3 = checkReturnValue et e2 doe2 in
        s.skind <- Instr [];  
        s :: doe3 @ [ unregisterStmt (); mkStmt (Return (Some e2, l)) ]
                      
    | Loop (b, l) -> 
        s.skind <- Loop (boxblock b, l);
        [ s ] 
          
    | If(be, t, e, l) -> 
        let (_, doe, e') = boxexp (CastE(intType, be)) in
        s.skind <- Instr [];
        s :: doe @ [ mkStmt (If(e', boxblock t, boxblock e, l)) ]
    | Instr il -> 
        (* Do each instruction in turn *)
        let b = List.fold_left (fun acc i -> acc @ boxinstr i) [] il in
        s.skind <- Instr [];
        compactBlock (s :: b)
    | Switch (e, b, cases, l) -> 
      (* Cases are preserved *)
        let (_, doe, e') = boxexp (CastE(intType, e)) in
        s.skind <- Instr [];
        s :: doe @ [ mkStmt (Switch (e', boxblock b, cases, l)) ]
  with e -> begin
    ignore (E.log "boxstmt (%s) in %s\n" 
              (Printexc.to_string e) !currentFunction.svar.vname);
    [mkStmt(Instr [dInstr (dprintf "booo_statement(%a)" d_stmt s)])]
  end


and boxinstr (ins: instr) : stmt list = 
  if debug then
    ignore (E.log "Boxing %a\n" d_instr ins);
  try
    match ins with
    | Set (lv, e, l) -> 
        let (lvt, lvkind, lv', lvbase, lvend, dolv) = boxlval lv in
        let (doe, e') = boxexpf e in (* Assume et is the same as lvt *)
        (* Now do a cast, just in case some qualifiers are different *)
        let (doe', e2) = castTo e' lvt doe in
        let (_, doe3, e3) = fexp2exp e2 doe' in
        let check = 
          match lv' with
            Mem _, _ -> 
              checkWrite e3 lv' lvbase lvend lvt lvkind
          | Var vi, off when (vi.vglob || lvkind != N.Safe) -> 
              checkWrite e3 lv' lvbase lvend lvt lvkind
          | _ -> []
        in
        dolv @ doe3 @ check @ [mkSet lv' e3]

    | Call(vio, f, args, l) ->
        let (ft, dof, f') = boxfunctionexp f in
        let (ftret, ftargs, isva) =
          match ft with 
            TFun(fret, fargs, isva, _) -> (fret, fargs, isva) 
          | _ -> E.s (E.unimp "call of a non-function: %a @!: %a" 
                        d_plainexp f' d_plaintype ft) 
        in
        let leavealone, isallocate = 
          match f' with
            Lval(Var vf, NoOffset) -> 
              H.mem leaveAlone vf.vname,
              getAllocInfo vf.vname

          | _ -> false, None
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
(* !!!! remove this *)         
                    if isFatType at then 
                      ([], readPtrField a' at)
                    else  
                      ([], a')
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
              | a :: resta, [] (* when isva *) -> 
                  let (doa, fa') = boxexpf a in
                  let (_, doa'', a2) = fexp2exp fa' doa in
                  let (doresta, resta') = doArgs resta [] in
                  (doa'' @ doresta, a2 :: resta')
              | _ -> E.s (E.unimp "too few arguments in call to %a" d_exp f)
            in
            doArgs args ftargs  
        in
        let finishcall = 
          match vio with 
            None -> [call None f' args']
          | Some (vi, _) -> begin
             (* If the destination variable gets tags then we must put the 
              * result of the call into a temporary first  *)
             (* Compute the destination of the call and some code to use 
              * after the call. Use boxinstr to get the code after the call 
              * to ensure that we use all the proper checks.  *)
              let (vi1: varinfo), (setvi1: stmt list) = 
                match boxlval (Var vi, NoOffset) with
                  (_, _, (Var _, NoOffset), _, _, _) -> 
                    vi, []
                      
                | (_, _, 
                   ((Var vi', Field(dfld, NoOffset)) as newlv), _, _,[]) -> 
                     let tmp = makeTempVar !currentFunction dfld.ftype in
                     tmp, 
                     boxinstr (Set ((Var vi, NoOffset), Lval (var tmp), l))
(*                     [ mkSet newlv (Lval (var tmp)) ] *)
                | _ ->  E.s (E.bug "Result of call is not a variable")
              in
              (* If the function is not an allocation function then we must 
               * watch for the case when the return type or the variable type 
               * is a struct. In that case we cannot use direct casts. For 
               * allocation functions pkAllocate knows how to handle casts  *)
              match isallocate with
                None -> 
                  (* See if at least one of the types is a composite type. In 
                   * that case we cannot use casts.  *)
                  let somecomp = 
                    match unrollType vi1.vtype, unrollType ftret with
                      TComp _, _ -> true
                    | _, TComp _ -> true
                    | _ -> false
                  in
                  let iscast = typeSigBox(ftret) <> typeSigBox vi1.vtype in
                  if somecomp && iscast then 
                    let tmp = makeTempVar !currentFunction ftret in
                    interceptCall (Some(tmp,false)) f' args' ::
                    (* Use boxinstr to do the proper cast and the proper 
                     * checks *)
                    boxinstr (Set((Var vi1, NoOffset), 
                                  Lval (var tmp), l)) @ setvi1
                  else
                    interceptCall (Some(vi1,iscast)) f' args' :: setvi1
                                                          
              | Some ai -> (pkAllocate ai vi1 f' args') @ setvi1
          end
        in
        dof @ doargs @ finishcall

    | Asm(tmpls, isvol, outputs, inputs, clobs, l) ->
        let rec doOutputs = function
            [] -> [], []
          | (c, lv) :: rest -> 
              let (lvt, lvkind, lv', lvbase, lvend, dolv) = boxlval lv in
              let check = 
                match lv' with
                  Mem _, _ -> 
                    checkWrite (integer 0) lv' lvbase lvend lvt lvkind
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
        doouts @ doins @ 
        [mkAsm tmpls isvol outputs inputs clobs]
            
  with e -> begin
    ignore (E.log "boxinstr (%s):%a (in %s)\n" 
              (Printexc.to_string e) d_instr ins !currentFunction.svar.vname);
    [mkInstr (dInstr (dprintf "booo_instruction(%a)" d_instr ins))]
  end

(* Given an lvalue, generate all the stuff needed to construct a pointer to 
 * it: a base type and a pointer type kind, an lvalue whose address makes the 
 * first component of the pointer and two exp's to be used as the second 
 * component (for pointer kinds other than Safe) and the third component (for 
 * pointer kinds Seq). We also compute a list of statements that must be 
 * executed to check the bounds.  *)
and boxlval (b, off) : (typ * N.opointerkind * lval * exp * exp * stmt list) = 
  let debuglval = false in
  (* As we go along the offset we keep track of the basetype and the pointer 
   * kind, along with the current base expression and a function that can be 
   * used to recreate the lval. *)
  let (btype, pkind, mklval, base, bend, stmts) as startinput = 
    match b with
      Var vi -> varStartInput vi
    | Mem addr -> 
        let (addrt, doaddr, addr', addr'base, addr'end) = boxexpSplit addr in
        let (addrt1, doaddr1, addrbase1, addrend1, addrkind) = 
          match unrollType addrt with
            TPtr(t, a) -> 
              let ptrk = kindOfType addrt in
              let newk = N.stripT ptrk in
              if newk <> ptrk then 
                let addrbase1, addrend1, doaddr2 = fromTable ptrk addr' in
                t, doaddr @ doaddr2, addrbase1, addrend1, newk
              else
               (* Make sure it is not a table type *)
                t, doaddr, addr'base, addr'end, newk
          | _ -> E.s (E.unimp "Mem but no pointer type: %a@!addr= %a@!"
                        d_plaintype addrt d_plainexp addr)
        in
        (addrt1, addrkind, (fun o -> (Mem addr', o)), 
         addrbase1, addrend1, doaddr1)
  in
  if debuglval then
    ignore (E.log "Lval=%a@!startinput=%a\n" 
              d_lval (b, off) N.d_opointerkind pkind); 
  (* As we go along we need to go into tagged and sized types. *)
  let goIntoTypes ((btype, pkind, mklval, base, bend, stmts) as input) = 
    if debuglval then
        ignore (E.log "goIntoTypes: btype=%a\n" d_plaintype btype);
    match unrollType btype with
      TComp (_, comp, _) when comp.cstruct -> begin
        match comp.cfields with
          f1 :: f2 :: [] when (f1.fname = "_size" && f2.fname = "_array") -> 
            begin
            (* A sized array *)
              if pkind != N.Safe then
                E.s (E.bug "Sized array in a non-safe area");
              (f2.ftype, N.Safe, (fun o -> mklval (Field(f2, o))), 
               zero, zero, stmts)
            end
        | f1 :: f2 :: _ when (f1.fname = "_len" && f2.fname = "_data") ->
            (* A tagged data. Only wild pointers inside *)
            if pkind = N.Wild then
              E.s (E.bug "Tagged data inside a tagged area");
            (f2.ftype, N.Wild, (fun o -> mklval (Field(f2, o))),
             mkAddrOf (mklval(Field(f2,NoOffset))), zero, stmts)

        | _ -> input
      end 
    | _ -> input
  in
  (* Now do the offsets *)
  let startinput = goIntoTypes startinput in
  let rec doOffset ((btype, _, _, _, _, _) as input) = function
      NoOffset -> input

    | Field (f, resto) -> 
        let (_, pkind, mklval, base, bend, stmts) = beforeField input in
        let addf o = 
          try
            let host, this = H.find hostsOfBitfields (f.fcomp.ckey, f.fname) in
            Field (host, Field(this, o))
          with Not_found -> Field (f, o)
        in
        (* Prepare for the rest of the offset *)
        let next = 
          (f.ftype, pkind, (fun o -> mklval (addf o)), base, bend, 
           stmts) in
        doOffset (goIntoTypes next) resto

    | Index (e, resto) -> 
        let (btype, pkind, mklval, base, bend, stmts) = beforeIndex input in
        (* Do the index *)
        let (_, doe, e') = boxexp e in
        (* Prepare for the rest of the offset *)
        let next = 
          (btype, pkind, (fun o -> mklval (Index(e', o))), base, bend, stmts) 
        in
        doOffset (goIntoTypes next) resto
  in
  let (btype, pkind, mklval, base, bend, stmts) = doOffset startinput off in
  let lvalres = mklval NoOffset in
  if debuglval then
    ignore (E.log "Done lval: pkind=%a@! lvalres=%a@!" 
              N.d_opointerkind pkind d_plainlval lvalres);
  (btype, pkind, lvalres, base, bend, stmts)
      
    (* Box an expression and return the fexp version of the result. If you do 
     * not care about an fexp, you can call the wrapper boxexp *)
and boxexpf (e: exp) : stmt list * fexp = 
  try
    match e with
    | Lval (lv) -> 
        (* ignore (E.log "boxexpf: %a\n" d_plainlval lv); *)
        let lvt, lvkind, lv', baseaddr, len, dolv = boxlval lv in
        let check = (* Check a read if it is in memory or if it comes from a 
                     * variable that contains arrays or that is tagged 
                       *)
          match lv' with
            Mem _, _ -> 
              checkRead lv' baseaddr len lvt lvkind
          | Var vi, off when containsArray vi.vtype || mustBeTagged vi -> 
              checkRead lv' baseaddr len lvt lvkind
          | _, _ -> []
        in
        (dolv @ check, mkFexp1 lvt (Lval(lv')))
            
    | Const (CInt (_, ik, _)) -> ([], L(TInt(ik, []), N.Scalar, e))
    | Const ((CChr _)) -> ([], L(charType, N.Scalar, e))
    | Const (CReal (_, fk, _)) -> ([], L(TFloat(fk, []), N.Scalar, e))

     (* All strings appear behind a CastE. The pointer node in the CastE 
      * tells us how to represent the string *)
    | CastE ((TPtr(TInt(IChar, _), a) as strt), 
             Const (CStr s)) -> stringLiteral s strt

    | Const (CStr _) -> 
        (* means that we have not yet run markptr. *)
        boxexpf (CastE(TPtr(TInt(IChar, []), 
                            if !N.defaultIsWild then 
                              [Attr("wild",[])] else [Attr("fseq", [])]),
                       e))

    | CastE (t, e) -> begin
        let t' = fixupType t in
        let (doe, fe') = boxexpf e in
        (* Put e into a variable *)
        castTo fe' t' doe
    end
          
          
    | UnOp (uop, e, restyp) -> 
        let restyp' = fixupType restyp in
        let (et, doe, e') = boxexp e in
        assert (not (isFatType restyp'));
          (* The result is never a pointer *)
        (doe, L(restyp', N.Scalar, UnOp(uop, e', restyp')))
          
    | BinOp (bop, e1, e2, restyp) -> begin
        let restyp' = fixupType restyp in
        let (et1, doe1, e1') = boxexp e1 in
        let (et2, doe2, e2') = boxexp e2 in
        match bop, kindOfType et1, kindOfType et2 with
        | (PlusPI|MinusPI|IndexPI), pk1, N.Scalar -> 
            let (res, doarith) = pkArithmetic e1' et1 pk1 bop e2' in
            (doe1 @ doe2 @ doarith, res)
        | (MinusPP|EqP|NeP|LeP|LtP|GeP|GtP), _, _ -> 
            (doe1 @ doe2, 
             L(restyp', N.Scalar,
               BinOp(bop, 
                     readPtrField e1' et1, 
                     readPtrField e2' et2, restyp')))
              
        | _, N.Scalar, N.Scalar -> 
            (doe1 @ doe2, L(restyp', N.Scalar, BinOp(bop,e1',e2',restyp')))
              
        | _, _, _ -> E.s (E.unimp "boxBinOp: %a@!et1=%a@!et2=%a@!" 
                            d_binop bop d_plaintype et1 d_plaintype et2)
    end
          
    | SizeOf (t) -> 
        let containsExposedPointers t = 
          existsType 
            (function 
                TPtr _ -> ExistsTrue
                    (* Pointers inside named structures are not exposed *)
              | TComp (false, comp, _) when (String.length comp.cname > 1 &&
                                 String.get comp.cname 0 <> '@') -> ExistsFalse
              | _ -> ExistsMaybe) t 
        in
        if containsExposedPointers t then 
          ignore (E.warn "Boxing sizeof(%a) when type contains pointers. Use sizeof expression\n" d_type t);
        let t' = fixupType t in
        ([], L(uintType, N.Scalar, SizeOf(t')))

    (* Intercept the case when we do sizeof an lvalue. This way we can avoid 
     * trying to check the safety of reads that might be triggered if we view 
     * the lvalue as an expression *)
          
    | SizeOfE (Lval lv) -> 
        (* ignore (E.log "boxexpf: %a\n" d_plainlval lv); *)
        let lvt, lvkind, lv', baseaddr, len, dolv = boxlval lv in
        ([], L(uintType, N.Scalar, SizeOfE(Lval lv')))

    | SizeOfE (e) -> begin
        let (et, doe, e') = boxexp e in
        (* Drop all size-effects from this SizeOf *)
        ([], L(uintType, N.Scalar, SizeOfE(e')))
    end

    | AddrOf (lv) ->
        let (lvt, lvkind, lv', baseaddr, bend, dolv) = boxlval lv in
        (* Check that variables whose address is taken are flagged as such, 
         * or are globals  *)
        (match lv' with
          (Var vi, _) when not vi.vaddrof && not vi.vglob -> 
            E.s (E.bug "addrof not set for %s (addrof)" vi.vname)
        | _ -> ());
        let res, doaddrof = pkAddrOf lv' lvt lvkind baseaddr bend in
        (dolv @ doaddrof, res)
          
    (* StartOf is like an AddrOf except for typing issues. *)
    | StartOf lv -> begin
        let (lvt, lvkind, lv', baseaddr, bend, dolv) = boxlval lv in
        (* Check that variables whose address is taken are flagged *)
        (match lv' with
          (Var vi, _) when not vi.vaddrof && not vi.vglob -> 
            E.s (E.bug "addrof not set for %s (startof)" vi.vname)
        | _ -> ());
        let res, dostartof = pkStartOf lv' lvt lvkind baseaddr bend in
(*        ignore (E.log "result of StartOf: %a@!" d_fexp res); *)
        (dolv, res)
    end
    | Question (e1, e2, e3) ->       
        let (_, doe1, e1') = boxexp (CastE(intType, e)) in
        let (et2, doe2, e2') = boxexp e2 in
        let (et3, doe3, e3') = boxexp e3 in
        let result = mkFexp1 et2 (Question (e1', e2', e3')) in
        (doe1 @ doe2 @ doe3, result)
  with exc -> begin
    ignore (E.log "boxexpf (%s): %a in %s\n" 
              (Printexc.to_string exc) d_exp e !currentFunction.svar.vname);
    ([], L(charPtrType, N.String, dExp (dprintf "booo_exp: %a" d_exp e)))
  end 
            
      
and boxinit (ei: init) : init =
  try
    match ei with
      SingleInit e ->
        let e't, doe, e', e'base, e'len = boxexpSplit e in
        if doe <> [] then
          E.s (E.unimp "Non-pure initializer %a\n"  d_exp e);
        SingleInit e'

    | CompoundInit (t, initl) -> 
        let t' = fixupType t in
        (* Construct a new initializer list *)
        let doOneInit (off: offset) (ei: init) (tei: typ) acc = 
          boxinit ei :: acc
        in
        let newinitl = List.rev (foldLeftCompound doOneInit t initl []) in
        CompoundInit (t', newinitl)

  with exc -> begin
    ignore (E.log "boxinit (%s): %a in %s\n" 
              (Printexc.to_string exc) d_init ei !currentFunction.svar.vname);
    SingleInit (dExp (dprintf "booo_init: %a" d_init ei))
  end 


and fexp2exp (fe: fexp) (doe: stmt list) : expRes = 
  match fe with
    L (t, pk, e') -> (t, doe, e')       (* Done *)
  | FS (t, pk, e') -> (t, doe, e')      (* Done *)
  | FM (wt, _, ep, eb, el) -> 
      let (doset, lv) = setFatPointer wt (fun _ -> ep) eb el in
      (wt, doe @ doset, Lval(lv))
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
       Lval(Mem (doCast (mkAddrOf tmp) (TPtr(newt, []))), 
            NoOffset))

    (* Box an expression and resolve the fexp into statements *)
and boxexp (e : exp) : expRes = 
  let (doe, fe) = boxexpf e in
  fexp2exp fe doe

    (* Box an expression and split it into three components. *)
and boxexpSplit (e: exp) = 
  let (doe, fe') = boxexpf e in
  match fe' with
    L(lt, _, e') -> (lt, doe, e', zero, zero)
  | FM(ft,_,p,b,e) -> 
      let pfield, _, _ = getFieldsOfFat ft in
      (pfield.ftype, doe, p, b, e)
  | (FS _ | FC _) ->
      let (et, caste, e'') = fexp2exp fe' doe in
      let (tptr, ptr, base, bend) = readFieldsOfFat e'' et in
      (tptr, doe @ caste, ptr, base, bend)


and boxfunctionexp (f : exp) = 
  match f with
    Lval(Var vi, NoOffset) -> begin
      (* Sometimes it is possible that we have not seen this varinfo. Maybe 
       * it was introduced by the type inferencer to mark an independent copy 
       * of the function *)
      if not (H.mem leaveAlone vi.vname) &&
         not (isAllocFunction vi.vname)
      then begin
        vi.vtype <- fixupType vi.vtype;
        fixupGlobName vi
      end;
      boxexp f
   end
  | Lval(Mem base, NoOffset) -> 
      let rest, lvkind, lv', lvbase, lvend, dolv = 
        boxlval (Mem base, NoOffset) in
      (rest, dolv @ [checkFunctionPointer (AddrOf(lv')) lvbase lvkind], 
       Lval(lv'))
      
  | _ -> E.s (E.unimp "Unexpected function expression")




(* Create the preamble (in reverse order). Must create it every time because 
 * we must consider the effect of "defaultIsWild" *)
let preamble () = 
  (* Define WILD away *)
  theFile := !checkFunctionDecls;
  (** Create some more fat types *)
  ignore (fixupType (TPtr(TInt(IChar, []), [Attr("wild",[])])));
(*  ignore (fixupType (TPtr(TInt(IChar, [AId("const")]), [AId("wild")]))); *)
  ignore (fixupType (TPtr(TVoid([]), [Attr("wild",[])])));
(*  ignore (fixupType (TPtr(TVoid([AId("const")]), [AId("wild")]))); *)
  let startFile = !theFile in
  theFile := 
     GText ("#include \"safec.h\"\n") :: 
     GText ("// Include the definition of the checkers\n") ::
     startFile

(* a hashtable of functions that we have already made wrappers for *)
let wrappedFunctions = H.create 15

let boxFile file =
  ignore (E.log "Boxing file\n");
  E.hadErrors := false;
  currentFile := file;
  let boxing = ref true in
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
      GPragma (a, _) -> begin
        (match a with
          Attr("interceptCasts", [ AId("on") ]) -> interceptCasts := true
        | Attr("interceptCasts", [ AId("off") ]) -> interceptCasts := false
        | Attr("boxalloc",  AStr(s) :: rest) -> 
            if not (H.mem allocFunctions s) then begin
                ignore (E.log "Will treat %s as an allocation function\n" s);
                boxallocPragma s rest
            end
        | Attr("boxprintf",  AStr(s) :: rest) -> 
            H.add leaveAlone s true
        | Attr("box", [AId("on")]) -> boxing := true
        | Attr("box", [AId("off")]) -> boxing := false
        | Attr("boxtext", [AStr s]) ->
            theFile := GText s :: !theFile
        | _ -> ());
        theFile := g :: !theFile
      end
    | _ -> begin
        if not !boxing then theFile := g :: !theFile else
        match g with

        | GDecl (vi, l) -> boxglobal vi false None l
        | GVar (vi, init, l) -> boxglobal vi true init l
        | GType (n, t, l) -> 
            if debug then
              ignore (E.log "Boxing GType(%s)\n" n);
            let tnew = fixupType t in
            theFile := GType (n, tnew, l) :: !theFile
                                               
        | GFun (f, l) -> 
            if debug then
              ignore (E.log "Boxing GFun(%s)\n" f.svar.vname);
            (* Run the oneret first so that we have always a single return 
             * where to place the finalizers  *)
            Oneret.oneret f;
            hasRegisteredAreas := false;
            (* Fixup the return type as well, except if it is a vararg *)
            f.svar.vtype <- fixupType f.svar.vtype;
            (* If the type has changed and this is a global function then we 
             * also change its name  *)
            fixupGlobName f.svar;
            (* Check that we do not take the address of a formal. If we 
             * actually do then we must make that formal a true local and 
             * create another formal  *)
            let newformals, (newbody : stmt list) =
              let rec loopFormals = function
                  [] -> [], f.sbody
                | form :: restf ->
                    let r1, r2 = loopFormals restf in
                    if form.vaddrof then begin
                      let tmp = makeTempVar f form.vtype in
                  (* Now take it out of the locals and replace it with the 
                     * current formal. It is not worth optimizing this one  *)
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
            setFormals f newformals;
            (* We fix the formals *)
            List.iter (fun l -> 
              l.vattr <- N.replacePtrNodeAttrList N.AtVar l.vattr;
              l.vtype <- fixupType l.vtype) f.sformals;
            (* Fixup the types of the locals  *)
            List.iter 
              (fun l -> 
                let newa, newt = moveAttrsFromDataToType l.vattr l.vtype in
                l.vattr <- N.replacePtrNodeAttrList N.AtVar newa;
(*                ignore (E.log "Fixing the type of local %s\n" l.vname);*)
                l.vtype <- fixupType newt;
                if mustBeTagged l then begin
                  l.vtype <- tagType l.vtype;
                end
                (* ignore (E.log "Local %s: %a. A=%a\n" l.vname
                   d_plaintype l.vtype
                   (d_attrlist true) l.vattr); *)
                )
              f.slocals;
            currentFunction := f;           (* so that maxid and locals can be
                                               * updated in place *)
            f.sbody <- newbody;
            (* Initialize and register the locals. Since we do this before 
             * boxing we will not initialize the temporaries created during 
             * boxing. But then we know that those are always defiend before 
             * use. We must initialize the locals before we do the body 
             * because the initialization produces the code for unregistering 
             * the locals, which we need when we encounter the Return  *)
            let inilocals = 
              List.fold_left 
                (initializeVar (withIterVar f)) 
                [] f.slocals in

            (* sm/gn: for testing the removeTemps module: add some extra temps *)
            if (traceActive "gratuitousTemps") then (
              for i = 0 to 10 do
                (trace "gratuitousTemps" (dprintf "Making a temp\n"));
                ignore (makeTempVar f intType);
              done;
            );

            (* Do the body now *)
            let boxbody : block = boxblock f.sbody in
            f.sbody <- inilocals @ boxbody;
            theFile := GFun (f, l) :: !theFile
                                        
        | (GAsm _ | GText _ | GPragma _) as g -> theFile := g :: !theFile
    end

  and boxglobal vi isdef init (l: location) =
    if debug then
      ignore (E.log "Boxing GVar(%s)\n" vi.vname);
    (* Leave alone some functions *)
    let origType = vi.vtype in
    if not (H.mem leaveAlone vi.vname) &&
      (* Leave alone the allocation functions !!!*)
       not (isAllocFunction vi.vname)
    then begin
      (* Remove the format attribute from functions that we do not leave
       * alone  *)
      let newa, newt = moveAttrsFromDataToType vi.vattr vi.vtype in
      vi.vattr <- N.replacePtrNodeAttrList N.AtVar 
            (dropAttribute newa (Attr("__format__", [])))
            ;
      vi.vtype <- fixupType newt;
      if mustBeTagged vi then begin
        vi.vtype <- tagType vi.vtype
      end
    end;
    (* If the type has changed and this is a global variable then we also 
     * change its name  *)
    fixupGlobName vi;
    (* Prepare the data initializer. *)
    let init' = 
      match init with
        None -> None
      | Some e -> Some (boxinit e)
    in
    (* Initialize the global *)
    if isdef && vi.vstorage <> Extern then begin
      extraGlobInit := 
         initializeVar 
           (fun x ->
             let gi = getGlobInit file in
             withIterVar gi x) !extraGlobInit vi;
    end;
    (* Tag some globals. We should probably move this code into the 
     * initializeVar but for now we keep it here *)
    if not (mustBeTagged vi) then
      if isdef then begin
        theFile := GVar(vi, init',l) :: !theFile
      end else
        theFile := GDecl (vi, l) :: !theFile
    else begin
      if not isdef && vi.vstorage <> Extern then
        theFile := GDecl (vi, l) :: !theFile
      else begin
        (* Make the initializer *)
        (* Add it to the tag initializer *)
        let varinit = 
          if vi.vstorage = Extern then None 
          else
            let (x, _) = makeTagCompoundInit vi.vtype init' in
            Some x
        in
        theFile := GVar(vi, varinit,l) :: !theFile
      end
    end
  in
  if debug then
    ignore (E.log "Boxing file\n");
  let doGlobal x = 
    try doGlobal x with e -> begin
      ignore (E.log "boxglobal (%s)\n" (Printexc.to_string e));
      theFile := 
         GAsm (sprint 2 (dprintf "booo_global %a" d_global x), lu) :: !theFile
    end 
  in
  extraGlobInit := [];
  H.clear taggedTypes;
  (* Create the preamble *)
  preamble ();
  interceptCasts := false;
  (* Now the orgininal file, including the global initializer *)
  iterGlobals file doGlobal;
  (* Now finish the globinit *)
  let newglobinit = 
    match file.globinit with
      None -> 
        if !extraGlobInit <> [] then
          let gi = getGlobInit file in
          gi.sbody <- !extraGlobInit;
          Some gi
        else
          None
    | Some g -> begin
        match !theFile with
          GFun(gi, _) :: rest -> 
            theFile := rest; (* Take out the global initializer (last thing 
                                added) *)
            gi.sbody <- compactBlock (!extraGlobInit @ gi.sbody);
            Some gi
        | _ -> E.s (E.bug "box: Cannot find global initializer\n")
    end
  in
  let res = List.rev (!theFile) in
  (* Clean up global hashes to avoid retaining garbage *)
   H.clear hostsOfBitfields;
   H.clear typeNames;
   H.clear fixedTypes;
   H.clear taggedTypes;
   H.clear sizedArrayTypes;
   extraGlobInit := [];
   let res = {file with globals = res; globinit = newglobinit} in
   Globinit.insertGlobInit ~mainname:!mangledMainName res ;
   res

  
      
let customAttrPrint a = 
  Ptrnode.ptrAttrCustom false a

