open Cil
open Pretty
open Trace

open Clist

module H = Hashtbl
module E = Errormsg
module N = Ptrnode 


let debugType = false
let showGlobals = false
let debugInstr = false

let interceptCasts = ref false  (* If true it will insert calls to 
                                 * __scalar2pointer when casting scalars to 
                                 * pointers.  *)

let stackChecks = ref true	    (* include checks like CHECK_STOREPTR and
															   * CHECK_RETURNPTR? *)

let compactBlocks = true

let lu = locUnknown
let isSome = function Some _ -> true | _ -> false

(* Have a loop constructor that yields nothing if the body is empty *)
let mkForIncrOptim ~iter:(iter:varinfo) ~first:(first: exp) 
                   ~past:(past: exp) ~incr:(incr: exp) 
                   ~body:(body: stmt list) : stmt list = 
  if body = [] then []
  else mkForIncr iter first past incr body
 
let wildpVoidType = ref voidType

(* Match two names, ignoring the polymorphic prefix *)
let matchPolyName (lookingfor: string) (lookin: string) = 
  let inl = String.length lookin in
  if inl = 0 then false else
  if String.get lookin 0 = '/' then
    let rec loop i = (* Search for the second / *)
      if i >= inl - 1 then false else 
      if String.get lookin i = '/' then 
        String.sub lookin (i + 1) (inl - i - 1) = lookingfor
      else loop (i + 1)
    in
    loop 1
  else lookin = lookingfor

(**** Stuff that we use while converting to new CIL *)
let mkSet (lv:lval) (e: exp) : stmt 
    = mkStmtOneInstr (Set(lv, e, !currentLoc))
let call lvo f args : stmt = mkStmtOneInstr (Call(lvo,f,args, !currentLoc))
let mkAsm tmpls isvol outputs inputs clobs = 
  mkStmtOneInstr (Asm(tmpls, isvol, outputs, inputs, clobs, !currentLoc))


(*** End stuff for old CIL *)

(**** We know in what function we are ****)
let currentFunction : fundec ref  = ref dummyFunDec
let currentFile     : file ref = ref dummyFile
let currentFileId     = ref 0

let extraGlobInit : stmt clist ref = ref empty


(* For each local that is moved to the heap we keep the field of the heap 
 * structure where it lives *)
let heapifiedLocals: (string, lval) H.t = H.create 7

(* Expresssions denoting the things to free at the end of the function *)
let heapifiedFree: stmt list ref = ref []

           (* After processing an expression, we create its type, a list of 
            * instructions that should be executed before this exp is used, 
            * and a replacement exp *)
type expRes = 
    typ * stmt clist * exp



(* We cure expressions and we keep the following fields *)
type cureexp = 
    { _pk: N.opointerkind; (* The pointer kind or N.Scalar if not a pointer *)
      _p : exp; (* The actual pointer value. Note that _pk = kindOfType (_p) *)
      _b: exp; (* The base of the home area in which the pointer points. zero 
                * if not meaningful *)
      _e: exp; (* The end of the home area in which the poitner points. zero 
                * if not meaningful *)
      _pt: typ; (* typeOf(_p) *)
      _typ: typ; (* The type of the whole cureexp. This is _pt for 
                  * single-word pointers or a structure for multi-word 
                  * pointers *)
      estmts: stmt clist; (* A list of statements that must be run to 
                           * make/check this exp *)
    } 

(* We cure lvals in preparation for taking their address. We keep the lval 
 * itself and its type, the base and end of the containing home area, and the 
 * kind of the pointer TO THE lval (if we actually take its address). The 
 * base and end fields might not be used for certain pointer kinds *)
type curelval = 
    { lv: lval;
      lvt: typ; (* typeOfLval(lv) *)
      lvb: exp; (* The base of the home area containing the lval *)
      lve: exp; (* The end of the home area containing the lval *)
      plvk: N.opointerkind; (* The kind of pointer to this lval that we 
                             * should create *) 
      lvstmts: stmt clist; (* A list of statements that must be run to 
                            * make/check this lval *)
}

let d_curelval () (lv: curelval) : doc = 
  dprintf "(K=%a,LV=%a,LVT=%a,B=%a,E=%a)" 
    N.d_opointerkind lv.plvk
    d_lval lv.lv
    d_type lv.lvt
    d_exp lv.lvb
    d_exp lv.lve

          (* When we create fat expressions we want to postpone some 
           * operations because they might involve creation of temporaries 
           * and statements. In some situations (e.g. cast from constant to 
           * fat pointer and then back to lean pointer, or initialization of 
           * a fat constant) we want to have as few statements as possible *)
type fexp = 
    L  of typ * N.opointerkind * exp     (* A one-word expression of a given 
                                         * kind (N.Scalar or PSafe) and type *)
  | FM of typ * N.opointerkind * exp * exp * exp 
                                         (* A multi-word expression that is 
                                          * made out of multiple single-word 
                                          * expressions: ptr, base and bend. 
                                          * bend might be "zero" if not 
                                          * needeed *)

let d_fexp () = function
    L(t, k, e) -> dprintf "L1(%a, %a:%a)" N.d_opointerkind k d_exp e d_type t
  | FM(_, k, ep, eb, ee) ->  
      dprintf "FM(%a, %a, %a, %a)" 
        N.d_opointerkind k d_exp ep d_exp eb d_exp ee
  

let kindOfFexp (fe: fexp) : N.opointerkind = 
  match fe with
    L (_, k, _) -> k
  | FM (_, k, _, _, _) -> k
    

let leaveAlone : (string, bool) H.t =
  let h = H.create 17 in
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
    with _ -> E.s (bug "no size arguments in call to allocator %s\n" name) 
  in
  let replaceArg n what args = 
    let rec loop n = function
        _ :: rest when n = 0 -> what :: rest
      | a :: rest when n > 0 -> a :: loop (n - 1) rest
      | _ -> E.s (bug "cannot replace size argument for allocator %s\n" name)
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
        ai.aiGetSize <- getArg (n - 1); ai.aiNewSize <- replaceArg (n - 1);
        loop rest
    | ACons("sizemul", [AInt n1; AInt n2]) :: rest -> 
        ai.aiGetSize <-
           (fun args -> BinOp(Mult, getArg (n1 - 1) args, getArg (n2 - 1) args,
                              intType));
        ai.aiNewSize <-
           (fun what args -> 
             (replaceArg (n1 - 1) one 
                (replaceArg (n2 - 1) what args)));
        loop rest
    | a :: rest -> 
        (ignore (E.warn "Don't understand boxalloc atrtibute: %a@!"
                   d_attrarg a));
        loop rest
  in
  loop args;
  (* Add to the hash *)
  H.add allocFunctions name ai


    (* See if the function name starts with /* ... */ *)
let stripPolyName (fname: string) : string = 
  let l = String.length fname in
  if l > 2 && String.sub fname 0 2 = "/*" then
    let endpoly = String.index_from fname 2 '/' in
    String.sub fname (endpoly + 1) (l - endpoly - 1)
  else
    fname
        
let getAllocInfo fname = 
  try
    let fname' = stripPolyName fname in
    (* ignore (E.log "Getting alloc info for %s\n" fname'); *)
    Some (H.find allocFunctions fname') 
  with _ -> None
    
let isAllocFunction name =
  isSome (getAllocInfo name)



(********************************************************************)


            (* Same for offsets *)
type offsetRes = 
    typ * stmt clist * offset * exp * N.opointerkind
      

(*** Helpers *)            
let castVoidStar e = doCast e voidPtrType

let prefix p s = 
  let lp = String.length p in
  let ls = String.length s in
  lp <= ls && String.sub s 0 lp = p


  (* We collect here the new file *)
let theFile : global list ref = ref []
let consGlobal (x : global) l = x :: l

let checkFunctionDecls : global list ref 
    = ref [GText("#define __WILD\n#define __FSEQ\n#define __SAFE")]

(**** Make new types ****)


    (* For each new type name, keep track of various versions, usually due 
     * to varying attributes *)
let typeNames : (string, int ref) H.t = H.create 17

let newTypeName (prefix: string) = newAlphaName typeNames prefix

let rec newTypeNameFromType prefix t = 
  let n = prefix ^ (baseTypeName t) in
  newTypeName n

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
  | TEnum (enum, _) -> "enum_" ^ enum.ename
  | TComp (comp, _) -> 
      let su = if comp.cstruct then "s_" else "u_" in
      if String.sub comp.cname 0 1 = "@" then su
      else su ^ comp.cname
  | TFun _ -> "fun"
  | TPtr(t, _) -> "p_" ^ baseTypeName t
  | TArray(t, _, _) -> "a_" ^ baseTypeName t


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
    | TComp (comp, a) -> TComp (comp, dropit N.AtOther a) 
    | TEnum (enum, a) -> TEnum (enum, dropit N.AtOther a)
    | TFun (r, args, v, a) -> 
        List.iter (fun a -> a.vtype <- loop a.vtype) (argsToList args);
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
    TComp (comp, _) when isFatComp comp -> true 
  | _ -> false

(* Given a fat type, return the three fieldinfo corresponding to the ptr, 
 * base and (optional) end *)
let getFieldsOfFat (t: typ) 
    : fieldinfo * (fieldinfo option) * (fieldinfo option) = 
  match unrollType t with
    TComp (comp, _) when isFatComp comp -> begin
      match comp.cfields with 
        p :: b :: e :: _ -> p, Some b, Some e
      | p :: b :: [] -> 
          (* b could either be the base field or the end field *)
          if b.fname = "_b" then p, Some b, None else p, None, Some b
      | _ -> E.s (bug "getFieldsOfFat")
    end
  | _ -> E.s (bug "getFieldsOfFat %a\n" d_type t)
     

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
(*
      | Question (e1, e2, e3) ->
          let e2t, e2', e2'', e2e = readFieldsOfFat e2 et in
          let   _, e3', e3'', e3e = readFieldsOfFat e3 et in
          (Question(e1,e2',e3'), 
           Question(e1,e2'',e3''), 
           Question(e1, e2e,e3e))

      | Compound (t, [p; b]) when isFatType t -> 
          p, b, zero
      | Compound (t, [p; b; e]) when isFatType t -> 
          p, b, e
*)
      | _ -> E.s (unimp "split _p field offset: %a" d_plainexp e)
    in
    (fptr.ftype, ptre, basee, ende)
  else
    (et, e, zero, zero)

    (* Create a new temporary of a fat type and set its pointer and base 
     * fields *)
let setFatPointer (t: typ) (p: typ -> exp) (b: exp) (e: exp)
    : stmt clist * lval = 
  let tmp = makeTempVar !currentFunction t in
  let fptr, fbaseo, fendo = getFieldsOfFat t in
  let p' = p fptr.ftype in
  let setbase : stmt clist = 
    match fbaseo with
      None -> empty
    | Some fbase -> 
        single (mkSet (Var tmp, Field(fbase,NoOffset)) (castVoidStar b))
  in
  let setend : stmt clist = 
    match fendo with
      None -> empty
    | Some fend -> 
        single (mkSet (Var tmp, Field(fend,NoOffset)) (castVoidStar e))
  in
  ( CConsL (mkSet (Var tmp, Field(fptr,NoOffset)) p', 
            append setbase setend), 
   (Var tmp, NoOffset))
      
let readPtrField (e: exp) (t: typ) : exp = 
  let (tptr, ptr, base, bend) = readFieldsOfFat e t in ptr
      
let readBaseField (e: exp) (t: typ) : exp = 
  let (tptr, ptr, base, bend) = readFieldsOfFat e t in base

let rec kindOfType t = 
  (* Since t was fixed up, it has a qualifier if it is a pointer *)
  (* If it is a named type, look at the real type *)
  match t with
    TPtr (_, a) -> begin
      match extractPointerTypeAttribute a with
        N.Unknown -> if !N.defaultIsWild then N.Wild else N.Safe
      | res -> res
    end
  | TNamed (_, nt, _) -> kindOfType nt(* Ignore the attributes of the TNamed *)
  | TComp (comp, _) when comp.cstruct -> begin
      match comp.cfields with
        p :: _ when p.fname = "_p" -> kindOfType p.ftype  (* A fat type *)
      | _ -> N.Scalar
  end
  | _ -> N.Scalar
 
(*

  let t' = unrollType t in
  match extractPointerTypeAttribute (typeAttrs t) with
    N.Unknown -> begin
      match t' with
        TPtr _ -> 
      | t' when isFatType t' -> N.Wild
      | _ -> N.Scalar
    end
  | res -> res
*)

let breakFexp (fe: fexp) : typ * N.opointerkind * exp * exp * exp = 
  match fe with
    L(oldt, oldk, e) -> oldt, oldk, e, zero, zero
  | FM(oldt, oldk, p, b, e) -> oldt, oldk, p, b, e
    

(**** Pointer representation ****)
let pkNrFields = function
    N.Safe | N.Scalar | 
    N.WildT | N.SeqT | N.FSeqT | N.SeqNT | N.FSeqNT | N.IndexT -> 1
  | N.String | N.ROString -> 1
  | N.Wild | N.FSeq | N.FSeqN | N.Index -> 2
  | N.Seq | N.SeqN -> 3
  | k -> E.s (bug "pkNrFields: %a" N.d_opointerkind k)

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
  | _ -> E.s (bug "pkFields")
  
(* Make an fexp out of a single expression. Either the type is fat and a 
 * composite value is denoted by a single expression *)
let mkFexp1 (t: typ) (e: exp) : fexp  = 
  let k = kindOfType t in
  match pkNrFields k with 
    1 -> L (t, k, e)
  | _ -> 
      let pt, _p, _b, _e = readFieldsOfFat e t in
      FM (t, k, _p, _b, _e)

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
    N.Wild -> "wildp_"
  | N.FSeq | N.FSeqN -> "fseqp_"
  | N.Index -> "indexp_"
  | N.Seq | N.SeqN -> "seq_"
  | _ -> E.s (bug "pkTypeName")
  

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
  | _ -> E.s (bug "pkQualName")
  
(****** the CHECKERS ****)


let mallocFun = 
  let fdec = emptyFunction "malloc" in
  let argl  = makeVarinfo "len" uintType in
  fdec.svar.vtype <- TFun(voidPtrType, Some [ argl ], false, []);
  fdec

let freeFun = 
  let fdec = emptyFunction "free" in
  let argp  = makeVarinfo "area" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, Some [ argp ], false, []);
  fdec

(* sm: for tagged heapified areas *)
let freeMinus4Fun =
  let fdec = emptyFunction "free_minus4" in
  let argp  = makeVarinfo "area" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, Some [ argp ], false, []);
  fdec

let mainWrapper =
  let fdec = emptyFunction "_mainWrapper" in
  let argc  = makeVarinfo "argc" intType in
  let argv  = makeVarinfo "argv" (TPtr(charPtrType, [])) in
  fdec.svar.vtype <- TFun(intType, Some [ argc; argv ], false, []);
  checkFunctionDecls := 
     consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  fdec

let mainWrapper_w =   
  let fdec = emptyFunction "_mainWrapper_w" in
  let argc  = makeVarinfo "argc" intType in
  let argv  = makeVarinfo "argv" (TPtr(charPtrType, [])) in
  fdec.svar.vtype <- TFun(intType, Some [ argc; argv ], false, []);
  checkFunctionDecls := 
     consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  fdec

let mainWrapper_fs =   
  let fdec = emptyFunction "_mainWrapper_fs" in
  let argc  = makeVarinfo "argc" intType in
  let argv  = makeVarinfo "argv" (TPtr(charPtrType, [])) in
  fdec.svar.vtype <- TFun(intType, Some [ argc; argv ], false, []);
  checkFunctionDecls := 
     consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  fdec

let mainWrapper_ff =   
  let fdec = emptyFunction "_mainWrapper_ff" in
  let argc  = makeVarinfo "argc" intType in
  let argv  = makeVarinfo "argv" (TPtr(charPtrType, [])) in
  fdec.svar.vtype <- TFun(intType, Some [ argc; argv ], false, []);
  checkFunctionDecls := 
     consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  fdec

let mainWrapper_fq =   
  let fdec = emptyFunction "_mainWrapper_fq" in
  let argc  = makeVarinfo "argc" intType in
  let argv  = makeVarinfo "argv" (TPtr(charPtrType, [])) in
  fdec.svar.vtype <- TFun(intType, Some [ argc; argv ], false, []);
  checkFunctionDecls := 
     consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  fdec

let mainWrapper_qw =   
  let fdec = emptyFunction "_mainWrapper_qw" in
  let argc  = makeVarinfo "argc" intType in
  let argv  = makeVarinfo "argv" (TPtr(charPtrType, [])) in
  fdec.svar.vtype <- TFun(intType, Some [ argc; argv ], false, []);
  checkFunctionDecls := 
     consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  fdec

let mainWrapper_fw =   
  let fdec = emptyFunction "_mainWrapper_fw" in
  let argc  = makeVarinfo "argc" intType in
  let argv  = makeVarinfo "argv" (TPtr(charPtrType, [])) in
  fdec.svar.vtype <- TFun(intType, Some [ argc; argv ], false, []);
  checkFunctionDecls := 
     consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  fdec

let declareGlobalChecker fdec =
  (* all the CHECK_* things are macros now, so don't emit prototypes *)
  if false then
    checkFunctionDecls :=
       consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls
  else
    ()

let checkNull =
  let fdec = emptyFunction "CHECK_NULL" in
  let argp  = makeVarinfo "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, Some [ argp ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fun (what: exp) ->
    call None (Lval(var fdec.svar)) [ castVoidStar what ]

let checkSafeRetFatFun =
  let fdec = emptyFunction "CHECK_SAFERETFAT" in
  let argp  = makeVarinfo "p" voidPtrType in
  let argb  = makeVarinfo "isptr" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, Some [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fdec



let checkFunctionPointer =
  let fdec = emptyFunction "CHECK_FUNCTIONPOINTER" in
  let argp  = makeVarinfo "p" voidPtrType in
  let argb  = makeVarinfo "b" voidPtrType in
  let argnr  = makeVarinfo "nr" intType in
  fdec.svar.vtype <- TFun(voidType, Some [ argp; argb; argnr ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fun whatp whatb whatkind nrargs ->
    if whatkind = N.Safe then
      checkNull whatp
    else
      call None (Lval(var fdec.svar)) [ castVoidStar whatp;
                                        castVoidStar whatb; integer nrargs ]

(* Compute the ptr corresponding to a base. This is used only to pass an
 * argument to the intercept functionin the case when the base = 0 *)
let ptrOfBase (base: exp) =
  let rec replaceBasePtr = function
      Field(fip, NoOffset) when fip.fname = "_b" ->
             (* Find the fat type that this belongs to *)
        let pfield, _, _ = getFieldsOfFat (TComp(fip.fcomp, [])) in
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
  let argp  = makeVarinfo "p" voidPtrType in
  let argb  = makeVarinfo "b" voidPtrType in
  fdec.svar.vstorage <- Static;
  fdec.svar.vtype <- TFun(uintType, Some [ argp; argb ], false, []);
  (declareGlobalChecker fdec);
  fun tmplen ptr base ->
    call (Some (var tmplen)) (Lval (var fdec.svar))
      [ castVoidStar ptr;
        castVoidStar base ]

let checkFetchStringEnd =
  let fdec = emptyFunction "CHECK_FETCHSTRINGEND" in
  let args  = makeVarinfo "s" charPtrType in
  fdec.svar.vstorage <- Static;
  fdec.svar.vtype <- TFun(voidPtrType, Some [ args; ], false, []);
  (declareGlobalChecker fdec);
  fdec

let checkStringMax =
  let fdec = emptyFunction "CHECK_STRINGMAX" in
  let argp  = makeVarinfo "p" voidPtrType in
  let argb  = makeVarinfo "b" voidPtrType in
  fdec.svar.vstorage <- Static;
  fdec.svar.vtype <- TFun(uintType, Some [ argp; argb ], false, []);
  (declareGlobalChecker fdec);
  fdec

let checkFetchEnd =
  let fdec = emptyFunction "CHECK_FETCHEND" in
  let argp  = makeVarinfo "p" voidPtrType in
  let argb  = makeVarinfo "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidPtrType, Some [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fun tmplen base ->
    let ptr = ptrOfBase base in (* we used to use this and worked, but when
                                 * we added tables stoped working *)
    call (Some (var tmplen)) (Lval (var fdec.svar))
      [ castVoidStar ptr;
        castVoidStar base ]

let checkLBoundFun =
  let fdec = emptyFunction "CHECK_LBOUND" in
  let argb  = makeVarinfo "b" voidPtrType in
  let argp  = makeVarinfo "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, Some [ argb; argp; ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  theFile := consGlobal (GDecl (fdec.svar, lu)) !theFile;
  fdec

let checkUBoundFun =
  let fdec = emptyFunction "CHECK_UBOUND" in
  let argbend  = makeVarinfo "bend" voidPtrType in
  let argp  = makeVarinfo "p" voidPtrType in
  let argpl  = makeVarinfo "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, Some [ argbend; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fdec

(* sm: check ubound, or allow NULL pointer (modified from above) *)
let checkUBoundOrNullFun =
  let fdec = emptyFunction "CHECK_UBOUNDNULL" in
  let argbend  = makeVarinfo "bend" voidPtrType in
  let argp  = makeVarinfo "p" voidPtrType in
  let argpl  = makeVarinfo "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, Some [ argbend; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fdec

(* sm: check bounds, or allow NULL pointer *)
let checkBoundsNullFun =
  let fdec = emptyFunction "CHECK_BOUNDSNULL" in
  let argb  = makeVarinfo "b" voidPtrType in
  let argbend  = makeVarinfo "bend" voidPtrType in
  let argp  = makeVarinfo "p" voidPtrType in
  let argpl  = makeVarinfo "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, Some [ argb; argbend; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fdec

let checkBoundsLenFun =
  let fdec = emptyFunction "CHECK_BOUNDS_LEN" in
  let argb  = makeVarinfo "b" voidPtrType in
  let argbl  = makeVarinfo "bl" uintType in
  let argp  = makeVarinfo "p" voidPtrType in
  let argpl  = makeVarinfo "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, Some [ argb; argbl; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fdec

(* A run-time function to coerce scalars into pointers. Scans the heap and
 * (in the future the stack) *)
let interceptId = ref 0
let interceptCastFunction =
  let fdec = emptyFunction "__scalar2pointer" in
  let argl = makeVarinfo "l" ulongType in
  let argf = makeVarinfo "fid" intType in
  let argid = makeVarinfo "lid" intType in
  fdec.svar.vtype <- TFun(voidPtrType, Some [ argl; argf; argid ], false, []);
  theFile :=
     consGlobal (GDecl (fdec.svar, lu)) !theFile;
  fdec


(* Check a read *)
let checkFatPointerRead =
  let fdec = emptyFunction "CHECK_FATPOINTERREAD" in
  let argb  = makeVarinfo "b" voidPtrType in
  let arglen  = makeVarinfo "nrWords" uintType in
  let argp  = makeVarinfo "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, Some [ argb; arglen; argp; ], false, []);
  (declareGlobalChecker fdec);
  fdec.svar.vstorage <- Static;

  fun base where len ->
    call None (Lval(var fdec.svar))
      [ castVoidStar base; len; castVoidStar where]

let checkFatPointerWrite =
  let fdec = emptyFunction "CHECK_FATPOINTERWRITE" in
  let argb  = makeVarinfo "b" voidPtrType in
  let arglen  = makeVarinfo "nrWords" uintType in
  let argp  = makeVarinfo "p" voidPtrType in
  let argwb  = makeVarinfo "wb" voidPtrType in
  let argwp  = makeVarinfo "wp" voidPtrType in
  fdec.svar.vtype <-
     TFun(voidType, Some [ argb; arglen; argp; argwb; argwp; ], false, []);
  (declareGlobalChecker fdec);
  fdec.svar.vstorage <- Static;

  fun base where whatbase whatp len ->
    call None (Lval(var fdec.svar))
      [ castVoidStar base; len;
        castVoidStar where;
        castVoidStar whatbase; castVoidStar whatp;]

let checkStoreFatPtr =
  let fdec = emptyFunction "CHECK_STOREFATPTR" in
  let argb  = makeVarinfo "b" voidPtrType in
  let argp  = makeVarinfo "isptr" voidPtrType in
  fdec.svar.vtype <-
     TFun(voidType, Some [ argp; argb; ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);

  fun whatp nullIfInt ->
    if !stackChecks then 
      call None (Lval(var fdec.svar))
	[ castVoidStar whatp; castVoidStar nullIfInt;]
    else mkEmptyStmt ()
        
let checkStorePtr =
  let fdec = emptyFunction "CHECK_STOREPTR" in
  let argp  = makeVarinfo "p" voidPtrType in
  fdec.svar.vtype <-
     TFun(voidType, Some [ argp; ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);

  fun whatp -> 
    if !stackChecks then 
      call None (Lval(var fdec.svar)) [ castVoidStar whatp;]
    else mkEmptyStmt ()

let checkReturnPtr =
  let fdec = emptyFunction "CHECK_RETURNPTR" in
  let argp  = makeVarinfo "p" voidPtrType in
  fdec.svar.vtype <-
     TFun(voidType, Some [ argp; ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);

  fun whatp ->
    if !stackChecks then 
      call None (Lval(var fdec.svar)) [ castVoidStar whatp;]
    else mkEmptyStmt ()
        
let checkReturnFatPtr =
  let fdec = emptyFunction "CHECK_RETURNFATPTR" in
  let argp  = makeVarinfo "p" voidPtrType in
  let argb  = makeVarinfo "b" voidPtrType in
  fdec.svar.vtype <-
     TFun(voidType, Some [ argp; argb; ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);

  fun whatp nullIfInt ->
    if !stackChecks then 
      call None (Lval(var fdec.svar))
	[ castVoidStar whatp; castVoidStar nullIfInt;]
    else mkEmptyStmt ()

let checkZeroTagsFun =
  let fdec = emptyFunction "CHECK_ZEROTAGS" in
  let argb  = makeVarinfo "b" voidPtrType in
  let argbl = makeVarinfo "bl" uintType in
  let argp  = makeVarinfo "p" voidPtrType in
  let argsize  = makeVarinfo "size" uintType in
  let offset  = makeVarinfo "offset" uintType in
  fdec.svar.vtype <-
     TFun(voidType, Some [ argb; argbl; argp; argsize; offset ], false, []);
  (declareGlobalChecker fdec);
  fdec.svar.vstorage <- Static;
  fdec

let checkFindHomeFun =
  let fdec = emptyFunction "CHECK_FINDHOME" in
  let argk  = makeVarinfo "kind" intType in
  let argp  = makeVarinfo "p" voidPtrType in
  fdec.svar.vtype <-
     TFun(voidPtrType, Some [ argk; argp ], false, []);
  (declareGlobalChecker fdec);
  fdec.svar.vstorage <- Static;
  fdec


let checkFindHomeEndFun =
  let fdec = emptyFunction "CHECK_FINDHOMEEND" in
  let argk  = makeVarinfo "kind" intType in
  let argp  = makeVarinfo "p" voidPtrType in
  let argea  = makeVarinfo "ea" (TPtr(voidPtrType,[])) in
  fdec.svar.vtype <-
     TFun(voidPtrType, Some  [ argk; argp; argea ], false, []);
  (declareGlobalChecker fdec);
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
let typeSigBox t = typeSigWithAttrs ignorePtrNode t

(***** Pointer arithemtic *******)
let checkPositiveFun =
  let fdec = emptyFunction "CHECK_POSITIVE" in
  let argx  = makeVarinfo "x" intType in
  fdec.svar.vtype <- TFun(voidType, Some [ argx; ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fdec

let checkAdvanceFun = 
  let fdec = emptyFunction "CHECK_ADVANCE" in
  let argp = makeVarinfo "p" charPtrType in
  let argx  = makeVarinfo "x" intType in
  fdec.svar.vtype <- TFun(voidType, Some [ argp; argx; ], false, []);
  fdec.svar.vstorage <- Static;
  (declareGlobalChecker fdec);
  fdec


(* All functions of WILD type take only arguments of WILD pointer type.
 * Thus turn all other kinds of arguments into WILD pointers *)

(* Keep track of the return values and formal arguments that have been
 * changed. A map indexed by a pair consisting of the function id and the
 * argument id (or -1 for the return value). *)
let boxedArguments: (int * int, typ) H.t = H.create 15

let rec fixupFunctionType (funcid: int) (fkind: N.opointerkind) (t: typ) =
  if fkind <> N.Wild then t else
  match unrollType t with
    TFun (rt, args, va, a) ->
      List.iter
        (fun a -> a.vtype <- fixupOneArgumentType (funcid, a.vid) a.vtype)
        (argsToList args);
      (* Leave alone the return type if it is not a pointer type since it 
       * creates more problems than we need (due to the restrictions on 
       * representing function calls in CIL)  *)
      let rt' = 
        if isPointerType rt then 
          fixupOneArgumentType (funcid, -1) rt
        else rt
      in
      TFun(rt', args, va, a)

  | _ -> t

and fixupOneArgumentType (funcid, argid) (t: typ) : typ = 
  match unrollType t with
    TComp _ when kindOfType t = N.Wild -> t (* Already WILD pointer *)
  | (TInt _ | TEnum _ | TFloat _) when (try bitsSizeOf t <= 32 with _ -> false) -> 
      if funcid >= 0 then 
        H.add boxedArguments (funcid, argid) t;
      !wildpVoidType 

  | TVoid _ -> t (* This is a missing return or argument type *)

  | _ -> begin
      (* Leave these things alone *)
      ignore (warn "argument too wide (%a) in a WILD function. Leaving alone!"
                d_type t);
      t
  end
           

(* Keep track of the fixed composite types. Index by full name *)
let fixedComps : (int, unit) H.t = H.create 113

let rec fixupType t = 
  match t with
    TComp (_, _) -> t (* Ignore the forward references *)

  (* Do not hash function types because they contain arguments whose types 
   * change later *)
  | TFun (rt, args, isva, a) -> begin
      List.iter (fun argvi -> argvi.vtype <- fixupType argvi.vtype) 
                (argsToList args);
      let res = TFun(fixupType rt, args, isva, a) in
      res
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
          (TInt _|TEnum _|TFloat _|TVoid _) -> t

        | TPtr (t', a) -> begin
            (* Extract the boxing style attribute *)
            let pkind = kindOfType t in
            (* Now do the base type *)
            let fixed' = fixupType t' in
            (* Maybe it is a function with a WILD type. We must chage all
             * arguments to be WILD pointers *)
            let fixed' = fixupFunctionType (-1) pkind fixed' in
            let newType = TPtr(fixed', a) in
            let fixed = 
              if pkNrFields pkind = 1 then newType 
              else
                let tname  = newTypeNameFromType (pkTypePrefix pkind) fixed' in
                let tcomp = 
                    mkCompInfo true tname 
                    (fun _ -> 
                      List.map (fun (n,tf) -> (n, tf newType, None, []))
                        (pkFields pkind))
                    []
                in
                let tstruct = TComp (tcomp, []) in
                (* Register the struct *)
                theFile := 
                   consGlobal (GCompTag (tcomp, !currentLoc)) !theFile;
                (* Now define a type name *)
                theFile := 
                   consGlobal (GType(tname, tstruct, !currentLoc)) !theFile;
                let tres = TNamed(tname, tstruct, [N.k2attr pkind]) in
                (* Add this to ensure that we do not try to box it twice *)
                H.add fixedTypes (typeSigBox tres) tres;
                (* H.add fixedTypes (typeSigBox tstruct) tres; *)
                (* And to make sure that for all identical pointer types we 
                 * create identical structure *)
                H.add fixedTypes (typeSigBox newType) tres;
                tres
            in
            (* We add fixed ourselves. The TNamed will be added after doit  *)
            (* H.add fixedTypes (typeSigBox fixed) fixed; *)
            fixed
        end
              
        | TNamed (n, t', a) -> TNamed (n, fixupType t', a)

        | TComp (_, _) -> t (* Leave the forward alone *)              
              
        | TArray(t', l, a) -> 
            let sized = extractArrayTypeAttribute a in
            let newarray = TArray(fixupType t', l, a) in
            let res =
              if sized then begin
                addArraySize newarray
              end else begin
                (match l with Some z when isZero z ->
                  ignore (warn "Unsized array of length 0\n");
                | _ -> ());
                newarray
              end
            in
            (* Save the fixed comp so we don't redo it later. Important since
             * redoing it means that we change the fields in place. *)
            H.add fixedTypes (typeSigBox res) res;
            res
                
                
        | TFun(rt,args,isva,a) ->
(*          let args' = 
            List.map
            (fun argvi -> {argvi with vtype = fixupType argvi.vtype}) args 
            * in
            
            *)
            List.iter (fun argvi -> argvi.vtype <- fixupType argvi.vtype) 
                      (argsToList args);
            let res = TFun(fixupType rt, args, isva, a) in
            res
      in
      H.add fixedTypes ts fixed;
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
        | TComp (ci, a) when ci.cfields = [] -> TArray(charType, Some zero, 
                                                       [Attr("sized", [])])
        | _ -> 
            E.s (unimp "Don't know how to tag incomplete type %a" 
                   d_plaintype t)
      end
    in
    let packAttr = if !msvcMode then [] else [Attr("packed", [])] in
    let tname = newTypeNameFromType "_sized_" t in
    let newtypecomp = 
         mkCompInfo true tname
           (fun _ -> 
             [ ("_size", uintType, None, []); (* Don't pack the first field 
                                               * or else the whole variable 
                                               * will be packed against the 
                                               * preceeding one  *)
               ("_array", complt, None, packAttr); ]) []
    in
    (* Register the new tag *)
    theFile := consGlobal (GCompTag (newtypecomp, !currentLoc)) !theFile;
    let newtype = TComp (newtypecomp, []) in
    let named = TNamed (tname, newtype, [Attr("sized", [])]) in
    theFile := consGlobal (GType (tname, newtype, !currentLoc)) !theFile;
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
    let tname = newTypeNameFromType "_tagged_" t in
    let newtype = 
      if isCompleteType t then begin
        (* ignore (E.log "Type %a -> bytes=%d, words=%d, tagwords=%d\n"
                  d_type t bytes words tagwords); *)
        let _, tagWords = tagLength (SizeOf(t)) in
        let tagAttr = if !msvcMode then [] else [Attr("packed", [])] in
        let tagComp = 
           mkCompInfo true tname
             (fun _ -> 
               [ ("_len", uintType, None, []); (* Don't pack the first 
                                                * field,or else the entire 
                                                * thing will be packed 
                                                * against the preceeding one  *)
                 ("_data", t, None, tagAttr);
                 ("_tags", TArray(intType, 
                                  Some tagWords, []), None, tagAttr);
               ])
             []
        in
        (* Register the type *)
        theFile := consGlobal (GCompTag (tagComp, !currentLoc)) !theFile;
        TComp (tagComp, [])

      end else begin (* An incomplete type *)
	(* GCC does not like fields to have incomplete types *)
	let complt = 
	  match unrollType t with
	    TArray(bt, None, a) -> TArray(bt, Some zero, a)
	  | TArray(bt, Some z, a) when isZero z -> t
	  | TComp (ci, _) when ci.cfields = [] -> 
              TArray(charType, Some zero, [])
	  | _ -> t (* E.s (unimp "Don't know how to tag incomplete type %a" 
                        d_plaintype t) *)
	in
        let tagComp = 
           mkCompInfo true tname
             (fun _ -> 
               [ ("_len", uintType, None, []);
                 ("_data", complt, None, []); ]) [] in
        (* Register the type *)
        theFile := consGlobal (GCompTag (tagComp, !currentLoc)) !theFile;
        TComp (tagComp, [])
      end
    in
    let named = TNamed (tname, newtype, []) in
    theFile := consGlobal (GType (tname, newtype, !currentLoc)) !theFile;
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
    



(* Create the preamble (in reverse order). Must create it every time because 
 * we must consider the effect of "defaultIsWild" *)
let preamble () =
  (* Define WILD away *)
  theFile := !checkFunctionDecls;
  (** Create some more fat types *)
  ignore (fixupType (TPtr(TInt(IChar, []), [Attr("wild",[])])));
(*  ignore (fixupType (TPtr(TInt(IChar, [AId("const")]), [AId("wild")]))); *)
  wildpVoidType := fixupType (TPtr(TVoid([]), [Attr("wild",[])]));
(*  ignore (fixupType (TPtr(TVoid([AId("const")]), [AId("wild")]))); *)
  let startFile = !theFile in
  theFile :=
     (consGlobal (GText ("#include \"safec.h\"\n"))
       (consGlobal (GText ("// Include the definition of the checkers\n"))
         startFile
       ))


(**** Make a pointer type of a certain kind *)
let mkPointerTypeKind (bt: typ) (k: N.opointerkind) = 
   fixupType (TPtr(bt, [N.k2attr k]))

(***** Conversion functions *******)


(***** Address of ******)
let pkAddrOf (lv: curelval) : (fexp * stmt clist) = 
  match unrollType lv.lvt with
  | TFun _ -> begin
      (* Taking the address of a function is a special case. Since fuctions 
       * are not tagged the type of the the pointer is Safe. If we are in 
       * defaultIsWild then we must make a Wild pointer out of it  *)
      let start = AddrOf lv.lv in
      let thetype = mkPointerTypeKind lv.lvt lv.plvk in
      match lv.plvk with
        N.Safe -> mkFexp3 thetype start zero zero, lv.lvstmts
      | N.Wild -> mkFexp3 thetype start lv.lvb zero, lv.lvstmts
      | _ -> E.s (bug "pkAddrOf function: %a" N.d_opointerkind lv.plvk) 
  end
  | _ -> begin      
      let ptrtype = mkPointerTypeKind lv.lvt lv.plvk in
      match lv.plvk with
        N.Safe -> mkFexp1 ptrtype (mkAddrOf lv.lv), lv.lvstmts
      | (N.Index | N.Wild | N.FSeq | N.FSeqN | N.Seq | N.SeqN ) -> 
          mkFexp3 ptrtype (mkAddrOf lv.lv) lv.lvb lv.lve, lv.lvstmts
      | _ -> E.s (bug "pkAddrOf(%a)" N.d_opointerkind lv.plvk)
  end

(* sm: return whether given type is {unsigned,signed,} char *)
let isCharType (t : typ) : bool =
  match unrollType t with
  | TInt((IChar|ISChar|IUChar), _) -> true
  | _ -> false


(* sm: localize the -1 that goes on in calculating array ends, to *)
(* facilitate experimenting with policy *)
let charArrayEndpOffset (arrayLen: exp) : exp =
  if false then
    (* old policy: let the end be length-1, cutting off access to last element *)
    (BinOp(MinusA, arrayLen, one, intType))
  else
    (* new policy: allow access to complete array; we add one more byte *)
    (* during a post-process after boxing proper *)
    arrayLen

(* Given an array lval obtain an lval corresponding to the first element *)
let arrayPointerToIndex (lv: curelval)
                        (* (t: typ)
                        (k: N.opointerkind)
                        (lv: lval)
                        (base: exp) *) : curelval =
  match unrollType lv.lvt with
    TArray(elemt, leno, a) ->
      let lv' = addOffsetLval (Index(zero, NoOffset)) lv.lv in
      if  lv.plvk = N.Wild || lv.plvk = N.WildT then
        { lv = lv'; lvt = elemt; plvk = N.Wild;
          lvb = lv.lvb; lve = zero; lvstmts = lv.lvstmts }
      else if (filterAttributes "sized" a <> []) then
        { lv = lv'; lvt = elemt; plvk = N.Index;
          lvb = StartOf lv.lv; lve = zero; lvstmts = lv.lvstmts }
      else begin
        match leno with
          Some alen -> (* If it is not sized then better have a length *)
            let knd, alen' =
              if filterAttributes "nullterm" a <> [] then begin
                if not (isCharType elemt) then
                  E.s (E.warn "NULLTERM array of %a\n" d_type elemt);
                (* Leave null for the null character *)
                N.SeqN, (charArrayEndpOffset alen)
              end else N.Seq, alen 
            in
            { lv = lv'; lvt = elemt; plvk = knd; 
              lvb = StartOf lv.lv;
              lve = BinOp(IndexPI, StartOf lv.lv, alen', TPtr(elemt, []));
              lvstmts = lv.lvstmts }
        | None -> (* Not WILD and not SIZED *)
            E.s (bug "arrayPointIndex on a unsized array: %a\n" d_lval lv.lv)
      end

  | _ -> E.s (bug "arrayPointerToIndex on a non-array (%a)" 
                d_plaintype lv.lvt)





(************* END of pointer qualifiers *************)
  



   (* Test if we have changed the type *)
let rec typeContainsFats t =
   existsType 
   (function TComp (comp, _) -> 
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
                       (* Do not tag functions!! We handle them separately *)
  else
    (* See if it make sense to tag this one. We look at the address-of flag 
     * and whether it contains arrays. *)
    let taggable = 
      v.vaddrof || containsArray v.vtype 
    || (match N.nodeOfAttrlist v.vattr with
        Some n when n.N.kind = N.Wild && n.N.why_kind = N.UserSpec -> true 
        | _ -> false)
        (* !!! Do not tag the other globals. If you want them tagged then put 
         * a __TAGGED attribute  *)
    in
    (* But do not tag certain variables *)
    let taggable' = taggable && (v.vname <> "__ccured_va_tags") in
    taggable' &&
    (!N.defaultIsWild || hasAttribute "tagged" v.vattr)



(* A few constants *)
let registerAreaTaggedInt = 0
let registerAreaSizedInt  = 1
let registerAreaSeqInt    = 2

let registerAreaFun =   
  let fdec = emptyFunction "CHECK_REGISTERAREA" in
  let argi  = makeVarinfo "k" intType in
  let argb  = makeVarinfo "b" voidPtrType in
  let arge  = makeVarinfo "e" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, Some [ argi; argb; arge; ], false, []);
  fdec.svar.vstorage <- Static;
  checkFunctionDecls := 
     consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  fdec

let unregisterFrameFun =
  let fdec = emptyFunction "CHECK_UNREGISTERFRAME" in
  fdec.svar.vtype <- TFun(voidType, Some [ ], false, []);
  fdec.svar.vstorage <- Static;
  (* sm: this is a macro; don't declare it; avoids a gcc warning *)
  (*
    checkFunctionDecls :=
       consGlobal (GDecl (fdec.svar, lu)) !checkFunctionDecls;
  *)
  fdec

(* Everytime you register a local variable, remember here *)  
let hasRegisteredAreas = ref true

(* Produce a statement to register an area and saves the code to unregister 
 * the area *)
let registerArea (args: exp list) 
                 (acc: stmt clist) : stmt clist = 
  if !N.useLeanFats then begin
    hasRegisteredAreas := true;
    let reg = call None (Lval(var registerAreaFun.svar)) args in
    CConsL (reg, acc)
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
      TComp (comp, _) -> begin
        match comp.cfields with 
          [lfld; dfld; tfld] -> dfld, lfld, tfld
        | _ -> E.s (bug "splitTagType. No tags: %a\n" d_plaintype tagged)
      end
    | _ -> E.s (bug "splitTagType. No tags: %a\n" d_plaintype tagged)
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


let debugBitfields = false
(* Since we cannot take the address of a bitfield we treat accesses to a 
 * bitfield like an access to the entire sequence of adjacent bitfields in 
 * the host structure. Return a starting address and a length of the range *)
let getRangeOfBitfields (lv: lval) (t: typ) : exp * exp = 
  let lvbase, lvoff = lv in
  try (* Will throw Not_found if not a bitfield *)
    let rec getHostOffset = function
        Field (fi, NoOffset) -> 
          if fi.fbitfield <> None then fi, NoOffset else raise Not_found
      | Field (fi, o) -> 
          let fibitfield, o' = getHostOffset o in
          fibitfield, Field (fi, o')
      | Index (e, o) -> 
          let fibitfield, o' = getHostOffset o in
          fibitfield, Index (e, o')
      | NoOffset -> raise Not_found
    in
    let fibitfield, hostoff = getHostOffset lvoff in
    if debugBitfields then 
      ignore (E.log "Found bitfield %s with host %s\n" fibitfield.fname
                fibitfield.fcomp.cname );
    (* Now find the last non-bitfield field before fi *)
    let rec findRange (before: fieldinfo option) (pastus: bool) = 
      function
        | [] -> before, None  (* Bitfields all the way to the end *)
        | f :: rest -> 
            if f.fname = fibitfield.fname then 
              findRange before true rest (* We past our bitfield *)
            else
              if pastus then 
                if f.fbitfield == None then 
                  before, Some f (* We found the end of the range *)
                else
                  findRange before true rest
              else
                if f.fbitfield == None then
                  findRange (Some f) false rest
                else
                  findRange before false rest
    in
    let before, after = findRange None false  fibitfield.fcomp.cfields in
    let start = 
      match before with
        None -> (* We are the first in the host.Take the address of the host *)
          if debugBitfields then 
            ignore (E.log "  first in host\n");
          mkAddrOf (lvbase, hostoff)
      | Some beforefi -> 
          if debugBitfields then 
            ignore (E.log "  bitfields start after %s\n" beforefi.fname);
          let beforelval = 
            lvbase, (addOffset (Field(beforefi, NoOffset)) 
                       hostoff)
          in
          BinOp(PlusPI, 
                doCast (mkAddrOf beforelval) charPtrType, 
                SizeOfE (Lval(beforelval)),
                charPtrType)
    in
    let after = 
      match after with 
        Some afterfi -> 
          if debugBitfields then 
            ignore (E.log "  bitfields end before %s\n" afterfi.fname);
          mkAddrOf (lvbase, addOffset (Field(afterfi, NoOffset)) hostoff)
      | None -> (* No more bitfields after us *)
          if debugBitfields then 
            ignore (E.log "  last in host\n");
          BinOp(PlusPI, 
                doCast (mkAddrOf (lvbase, hostoff)) charPtrType, 
                SizeOfE (Lval(lvbase, hostoff)),
                charPtrType)
    in
    
    (castVoidStar start, 
     BinOp(MinusPP, 
           doCast after charPtrType,
           doCast start charPtrType, intType))
  with Not_found ->
    (mkAddrOf lv, SizeOf t)

(* Compute the offset of first scalar field in a thing to be written. Raises 
 * Not_found if there is no scalar *)
let offsetOfFirstScalar (t: typ) : exp = 
  let rec theOffset (sofar: offset) t : offset option = 
    match unrollType t with
      (TInt _ | TFloat _ | TEnum _) -> Some sofar
    | TPtr _ -> None
    | TComp (comp, _) when isFatComp comp -> None
    | TComp (comp, _) when comp.cstruct -> begin
        let doOneField acc fi = 
          match acc, fi.ftype with
            None, TInt _ when fi.fbitfield <> None -> None
          | None, _ -> 
              theOffset (addOffset (Field(fi, NoOffset)) sofar) fi.ftype
          | Some _, _ -> acc
        in
        List.fold_left doOneField None comp.cfields
    end
    | TComp (comp, _) -> begin (* UNION *)
        (* Do it for the first field only *)
        match comp.cfields with 
          [] -> None
        | fi :: _ -> theOffset (addOffset (Field(fi, NoOffset)) sofar) fi.ftype
    end

    | TArray (bt, _, _) -> 
        theOffset (addOffset (Index(zero, NoOffset)) sofar) bt
    | _ -> E.s (unimp "offsetOfFirstScalar")
  in
  match theOffset NoOffset t with
    None -> raise Not_found
  | Some NoOffset -> kinteger IUInt 0 
  | Some off -> 
      let scalar = mkMem (doCastT zero intType (TPtr (t, []))) off in
      let addrof = mkAddrOf scalar in
      doCast addrof uintType

  
let checkZeroTags base lenExp lv t = 
  let start, size = getRangeOfBitfields lv t in
  try
    let offexp = offsetOfFirstScalar t in
    call None (Lval (var checkZeroTagsFun.svar))
      [ castVoidStar base; lenExp ;
        castVoidStar start; 
        size; offexp ] 
  with Not_found -> 
    mkEmptyStmt ()



(****** CONVERSION FUNCTIONS *******)

(* Accumulate the statements in reverse order *)
let seqToFSeq (p: exp) (b: exp) (bend: exp) (acc: stmt clist)
    : exp * exp * exp * stmt clist =   
  p, p, bend, 
  CConsL
    (call None (Lval (var checkLBoundFun.svar))
       [ castVoidStar b; castVoidStar p; ],
     acc)

let indexToSeq (p: exp) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  let tmp = makeTempVar !currentFunction voidPtrType in
  p, b, Lval(var tmp), CConsL (checkFetchEnd tmp b, acc)

let indexToFSeq (p: exp) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  let p', b', bend', acc' = indexToSeq p b bend acc in
  seqToFSeq p' b' bend' acc'
  
let fseqToSafe (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  let baset =
      match unrollType desttyp with
        TPtr(x, _) -> x
      | _ -> E.s (bug "fseqToSafe: expected pointer type")
  in
  p, zero, zero, 
  (* sm: changed to the OrNull variant so we allow casts of *)
  (* NULL FSEQs to NULL SAFEs *)
  CConsL (call None (Lval (var checkUBoundOrNullFun.svar))
            [ castVoidStar bend;   (* sm: bugfix: was 'b' *)
              castVoidStar p; SizeOf (baset)],
          acc)
    
let seqToSafe (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
(*
  let p', b', bend', acc' = seqToFSeq p b bend acc in
  fseqToSafe p' desttyp b' bend' acc'
*)
  (* An alternative way that collapses the two bounds checks *)
  let baset =
      match unrollType desttyp with
        TPtr(x, _) -> x
      | _ -> E.s (bug "seqToSafe: expected pointer type")
  in
  p, zero, zero,
  CConsL
    (call None (Lval (var checkBoundsNullFun.svar))
       [ castVoidStar b;  castVoidStar bend;
         castVoidStar p; SizeOf (baset)],
     acc)
  

let indexToSafe (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  let p', b', bend', acc' = indexToSeq p b bend acc in
  seqToSafe p' desttyp b' bend' acc'
    

let stringToSeq (p: exp) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  (* Make a new temporary variable *)
  let tmpend = makeTempVar !currentFunction voidPtrType in
  p, p,  (Lval (var tmpend)),
  CConsL (call (Some (var tmpend)) (Lval (var checkFetchStringEnd.svar))
            [ p ],
          acc)

let stringToFseq (p: exp) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  (* Make a new temporary variable *)
  let tmpend = makeTempVar !currentFunction voidPtrType in
  p, p, (Lval (var tmpend)), 
  CConsL (call (Some (var tmpend)) (Lval (var checkFetchStringEnd.svar))
            [ p ],
          acc)

  
let seqNToString (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  (* Conversion to a string is with a bounds check *)
  seqToSafe p desttyp b bend acc

let fseqNToString (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  (* Conversion to a string is with a bounds check *)
  fseqToSafe p desttyp b bend acc

let wildToROString (p: exp) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  p, zero, zero, 
  CConsL (call None (Lval (var checkStringMax.svar))
            [ castVoidStar p; b ],
          acc)

(* weimer: is this right?! *)
let indexToROString (p: exp) (b: exp) (bend: exp) (acc: stmt clist) 
    : exp * exp * exp * stmt clist =
  p, zero, zero, 
  CConsL (call None (Lval (var checkStringMax.svar))
            [ castVoidStar p; b ], acc)

let fromTable (oldk: N.opointerkind) 
              (p: exp) 
  (* Returns a base, and an end *) 
  : exp * exp * stmt clist =
  let checkAreas () = 
    if not !N.useLeanFats then 
      E.s (bug "I thought that we weren't using lean fats\n")
  in
  let fetchHomeEnd (kind: int) (p: exp) : varinfo * varinfo * stmt = 
    let tmpb = makeTempVar !currentFunction voidPtrType in
    let tmpe = makeTempVar !currentFunction voidPtrType in
    tmpb, tmpe,
    call (Some (var tmpb)) (Lval (var checkFindHomeEndFun.svar))
      [ integer kind ; castVoidStar p; mkAddrOf (var tmpe) ]
  in
  let fetchHome (kind: int) (p: exp) : varinfo * stmt = 
    let tmpb = makeTempVar !currentFunction voidPtrType in
    tmpb,
    call (Some (var tmpb)) (Lval (var checkFindHomeFun.svar))
      [ integer kind; castVoidStar p ]
  in
  match oldk with
    N.WildT -> 
      let b, s = fetchHome registerAreaTaggedInt p in
      (Lval(var b)), zero, single s
  | N.IndexT ->
      let b, s = fetchHome registerAreaSizedInt p in
      (Lval(var b)), zero, single s

  | N.SeqT | N.SeqNT | N.FSeqT | N.FSeqNT -> 
      let b, e, s = fetchHomeEnd registerAreaSeqInt p in
      (Lval(var b)), (Lval(var e)), single s
  | _ -> E.s (bug "Called fromTable on a non-table")

           

(* from table *)
let fromTableFexp (fe: fexp) : stmt clist * fexp =
  let oldt, oldk, p, b, bend = breakFexp fe in
  let newk = N.stripT oldk in
  if newk = oldk then
    empty, fe
  else
    let bt = 
      match unrollType oldt with
        TPtr(bt, _) -> bt
      | _ -> voidType
    in
    let newt = mkPointerTypeKind bt newk in
    let b, e, s = fromTable oldk p in
    s, mkFexp3 newt p b e
      

let checkWild (p: exp) (size: exp) (b: exp) (blen: exp) : stmt = 
  (* This is almost like indexToSafe, except that we have the length already 
   * fetched *)
  call None (Lval (var checkBoundsLenFun.svar))
    [ castVoidStar b; blen;
      castVoidStar p; size]
      
  (* Check index when we switch from a sequence type to Safe, in preparation 
   * for accessing a field.  *)
let beforeField (inlv: curelval) : curelval =
  match inlv.plvk with
    (* The kind is never a table type *)
    N.Wild -> inlv (* No change if we are in a tagged area *)
  | N.Safe -> inlv (* No change if already safe *)

  | N.Index -> 
      let _, _, _, docheck = 
        indexToSafe (mkAddrOf inlv.lv) 
          (TPtr(inlv.lvt, [])) inlv.lvb inlv.lve empty
      in
      { inlv with plvk = N.Safe; lvb = zero; lve = zero; 
                  lvstmts = append inlv.lvstmts docheck }
        
  | (N.Seq|N.SeqN) -> 
      let _, _, _, docheck = 
        seqToSafe (mkAddrOf inlv.lv) 
          (TPtr(inlv.lvt,[])) inlv.lvb inlv.lve empty
      in
      { inlv with plvk = N.Safe; lvb = zero; lve = zero;
                  lvstmts = append inlv.lvstmts docheck }
        
  | (N.FSeq|N.FSeqN) -> 
      let _, _, _, docheck = 
        fseqToSafe (mkAddrOf inlv.lv) 
          (TPtr(inlv.lvt,[])) inlv.lvb inlv.lve empty
      in
      { inlv with plvk = N.Safe; lvb = zero; lve = zero;
                  lvstmts = append inlv.lvstmts docheck }
        
  | _ -> E.s (unimp "beforeField on unexpected pointer kind %a"
                N.d_opointerkind inlv.plvk)
        
(* Prepare for indexing an lval. Obtain an lval that corresponds to the first 
 * element of the array, but with a Safe or Wild pointer *)    
let rec beforeIndex (inlv: curelval) : curelval =
  match inlv.plvk with
  | (N.Safe|N.Wild) -> arrayPointerToIndex inlv

  | (N.FSeq|N.FSeqN|N.Seq|N.SeqN|N.Index) ->   
      (* Convert to safe first *)
      let res1 = beforeField inlv in
      if res1.plvk != N.Safe then E.s (bug "beforeIndex: should be Safe\n");
      (* Now try again *)
      beforeIndex res1

  | _ -> E.s (unimp "beforeIndex on unexpected pointer kind %a"
                N.d_opointerkind inlv.plvk)



(***** Create function descriptors ******)
let functionDescriptors : (int, exp) H.t = H.create 13
(* Memoize the type of a function descriptor *)
let descriptorTypeInfo : compinfo option ref = ref None
(* We store the decriptor definitions for the end of the file *)
let descriptorDefinitions : global list ref = ref []
let getFunctionDescriptor (vi: varinfo) : exp = 
  try H.find functionDescriptors vi.vid 
  with Not_found -> begin
    let descrInfo = 
      match !descriptorTypeInfo with
        Some di -> di
      | None -> begin
          let descrInfo = 
            mkCompInfo true "__functionDescriptor"
              (fun _ -> 
                [ ("_len", uintType, None, []);
                  ("_pfun", TPtr(TFun(voidType, None,false, []), []), 
                  None, []);
                  ("_nrargs", uintType, None, []) ]) 
              []
          in
          (* Register the tag *)
          theFile := consGlobal (GCompTag (descrInfo, !currentLoc)) !theFile;
          descriptorTypeInfo := Some descrInfo;
          descrInfo
      end
    in
    (* Need to know the number of arguments *)
    let nrformals = 
      match vi.vtype with
        TFun (_, formals, _, _) -> List.length (argsToList formals)
      | _ -> E.s (bug "getFunctionDescriptor: %s not a function type" vi.vname)
    in
    let descr = makeGlobalVar (vi.vname ^ "__descriptor") 
                              (TComp (descrInfo, [])) in
    (* Register it *)
    theFile := consGlobal (GDecl (descr, !currentLoc)) !theFile;
    descriptorDefinitions :=
      (GVar (descr,
             Some (CompoundInit (TComp (descrInfo, []),
                                 [ SingleInit zero;
                                   SingleInit 
                                     (doCast 
                                        (AddrOf (Var vi, 
                                                  NoOffset))
                                     (TPtr(TFun(voidType,None,false, []),[])));
                                   SingleInit (integer nrformals) ])),
             !currentLoc)) :: !descriptorDefinitions;
    let pfunfld = List.nth descrInfo.cfields 1 in
    let res = mkAddrOf (Var descr, Field(pfunfld, NoOffset)) in
    H.add functionDescriptors vi.vid res;
    res
  end

(******* Start of *******)
let rec pkStartOf (lv: curelval) : (fexp * stmt clist) = 
  match unrollType lv.lvt with
    TArray(t, _, _) -> begin
      match lv.plvk with
        N.Safe | N.Wild | N.WildT -> 
          let lv' = arrayPointerToIndex lv in
          let pres = mkPointerTypeKind t lv'.plvk in
          mkFexp3 pres (mkAddrOf lv'.lv) lv'.lvb lv'.lve, lv'.lvstmts

      | N.Seq|N.FSeq|N.Index -> 
          (* multi-dim arrays. Convert to SAFE first *)
          let safe = beforeField lv in
          if safe.plvk <> N.Safe then
            E.s (bug "pkStartOf: I expected a safe here\n");
          let (res, stmts'') = pkStartOf safe in
          (res, stmts'')
          
      | _ -> E.s (unimp "pkStartOf: %a" N.d_opointerkind lv.plvk)
    end
  | _ -> E.s (unimp "pkStartOf on a non-array: %a"
                d_plaintype lv.lvt)

let varStartInput (vi: varinfo) : curelval = 
  (* Look out for wild function pointers *)
  match vi.vtype with
    TFun _ when 
      (match N.nodeOfAttrlist vi.vattr with
        Some n when n.N.kind = N.Wild -> true | _ -> false) ->
          let descr = getFunctionDescriptor vi in
          { lv = (Var vi, NoOffset); lvb = descr; lve = zero; 
            lvt = vi.vtype; plvk = N.Wild; lvstmts = empty }
  | _ -> 
      { lv = (Var vi, NoOffset); lvb = zero; lve = zero; 
        lvt = vi.vtype; plvk = N.Safe; lvstmts = empty }
  


let pkArithmetic (ep: exp)
                 (et: typ)
                 (ek: N.opointerkind) (* kindOfType et *)
                 (bop: binop)  (* Either PlusPI or MinusPI or IndexPI *)
                 (e2: exp) : (fexp * stmt clist) = 
  let ptype, ptr, fb, fe = readFieldsOfFat ep et in
  match ek with
    N.Wild|N.Index|N.WildT|N.IndexT -> 
      mkFexp3 et (BinOp(bop, ptr, e2, ptype)) fb zero, empty
  | (N.Seq|N.SeqN|N.SeqT|N.SeqNT) -> 
      mkFexp3 et (BinOp(bop, ptr, e2, ptype)) fb fe, empty
  | (N.FSeq|N.FSeqN|N.FSeqT|N.FSeqNT) ->
      mkFexp3 et (BinOp(bop, ptr, e2, ptype)) fb fe, 
      single (call None (Lval (var checkAdvanceFun.svar)) 
                [ doCastT ptr ptype charPtrType; e2 ])
      
  | N.Safe ->
      if isZero e2 then 
        mkFexp3 et ptr fb fe, empty
      else
        E.s (bug "pkArithmetic: pointer arithmetic on safe pointer: %a@!"
               d_exp ep)

  | N.String|N.ROString -> 
      (* Arithmetic on strings is tricky. We must first convert to a FSeq and 
       * then do arithmetic. We leave it a SeqN to be converted back to 
       * string late if necessary *)
      let p', b', bend', acc' = stringToSeq ptr fb fe empty in
      (* Change the type from String into a SeqN pointer *)
      let ptype' = 
        match ptype with
          TPtr((TInt((IChar|ISChar|IUChar), _) as bt), ptra) -> 
            TPtr(bt, 
                 addAttribute (N.k2attr N.SeqN)
                   (dropAttribute ptra (N.k2attr N.String)))
        | _ -> E.s (bug "String pointer kind but base type is not char")
      in
      (* And recompute the right type for the result *)
      let et' = fixupType ptype' in
(*      ignore (E.log "pkArith: %a\n" d_plaintype ptype'); *)
      let p'' = BinOp(bop, p', e2, ptype') in
      mkFexp3 et' p'' b' bend', rev acc'
      
  | _ -> E.s (bug "pkArithmetic(%a)" N.d_opointerkind ek)
        


let rec checkBounds 
                (iswrite: bool) 
                (mktmplen: unit -> exp)
                (lv: curelval) : stmt clist = 
(*
                (base: exp)
                (bend: exp)
                (lv: lval)
                (lvt: typ) 
                (pkind: N.opointerkind) : stmt clist = 
*)
  begin
    (* Do not check the bounds when we access variables without array 
     * indexing  *)
    (* ignore (E.log "Check bounds: pkind=%a\n" N.d_opointerkind pkind); *)
    match lv.plvk with
    | N.Wild -> (* We'll need to read the length anyway since we need it for 
                 * working with the tags *)
        let start, size = getRangeOfBitfields lv.lv lv.lvt in 
        let docheck = checkWild start size lv.lvb (mktmplen ()) in
        CConsR(lv.lvstmts, docheck)
          
    | N.Index -> 
        let _, _, _, docheck = 
          indexToSafe (mkAddrOf lv.lv) (TPtr(lv.lvt, [])) 
                      lv.lvb lv.lve empty in
        append lv.lvstmts (rev docheck)
          
    | (N.FSeq|N.FSeqN) ->
        let base' = 
          if lv.plvk = N.FSeqN && not iswrite then 
          (* Allow reading of the trailing 0 *)
            castVoidStar (BinOp(PlusPI, 
                                doCast lv.lvb charPtrType, one, charPtrType))
          else
            lv.lvb
        in
        let p, _, _, docheck = 
          fseqToSafe (mkAddrOf lv.lv) (TPtr(lv.lvt, [])) base' lv.lve empty in
        append lv.lvstmts (rev docheck)
          
    | (N.Seq|N.SeqN) ->
        let bend' = 
          if lv.plvk = N.SeqN && not iswrite then 
          (* Allow reading of the trailing 0 *)
            castVoidStar (BinOp(PlusPI, 
                                doCast lv.lve charPtrType, one, charPtrType))
          else
            lv.lve
        in
        let p, _, _, docheck = 
          seqToSafe (mkAddrOf lv.lv) (TPtr(lv.lvt, [])) lv.lvb bend' empty in
        append lv.lvstmts (rev docheck)
          
    | N.Safe | N.String | N.ROString -> lv.lvstmts

    | _ -> E.s (bug "Unexpected pointer kind in checkBounds(%a)"
                  N.d_opointerkind lv.plvk)
  end


(* Whether we must do a null check (after we have done bounds checking) *)
let nullCheckAfterBoundsCheck (k: N.opointerkind) = 
  match k with 
    N.Wild | N.WildT | N.Index | N.IndexT -> false
  | _ -> true
  

(****************************************************)


    (* Cast an fexp to another one. Accumulate necessary statements to doe *)
(* To debug rename the next function !!! *)
let rec castTo (fe: fexp) (newt: typ)
               (doe: stmt clist) : stmt clist * fexp =
  let newkind = kindOfType newt in
  match fe, newkind with
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
      let doe = append doe doe1 in
      (* Cast the pointer expression to the new pointer type *)
      let castP (p: exp) = doCast p newPointerType in
      (* Converts a reversed accumulator to doe *)
      let finishDoe (acc: stmt clist) = append doe (rev acc) in
      let oldt, oldk, p, b, bend = breakFexp fe in
      let is_zero fexp = 
        match fexp with
          L(t,k,e) -> isZero e
        | _ -> false
      in
      match oldk, newkind with
        (* Catch the cases when the destination is a table *)
      | _, (N.WildT|N.SeqT|N.FSeqT|N.SeqNT|N.FSeqNT|N.IndexT) ->
          let newk' = N.stripT newkind in
          let newt' = 
            match unrollType newt with
              TPtr(bt, _) -> mkPointerTypeKind bt newk'
            | _ -> E.s (bug "castTo: strip table")
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
          (* If the pointer type is a void ptr then do not add one to get the 
           * end since that is illegal C *)
          let theend = 
            let tp = typeOf p in
            match unrollType tp with
              TPtr(TVoid _, _) -> 
                ignore (warn "Casting SAFE void* to FSEQ");
                p
            | _ -> BinOp(PlusPI, p, one, tp)
          in
          let p' = castP p in
          (doe, FM (newt, newkind, p', p', castVoidStar theend))

        (* weimer: SAFE -> FSEQN only when the SAFE is 0 *)
      | N.Safe, N.FSeqN when is_zero fe  ->
          let p' = castP p in
          (doe, FM (newt, newkind, p', zero, zero))
      | N.Safe, N.SeqN when is_zero fe ->
          let p' = castP p in
          (doe, FM (newt, newkind, p', zero, zero))

        (* SAFE -> SEQ *)          
      | N.Safe, N.Seq -> 
          (* If the old pointer type is a void ptr then do not add one to get 
           * the end since that is illegal C  *)
          let theend = 
            let tp = typeOf p in
            match unrollType tp with
              TPtr(TVoid _, _) -> 
                ignore (warn "Casting SAFE void* to SEQ");
                p
            | _ -> BinOp(PlusPI, p, one, tp)
          in
          let p' = castP p in
          (doe, FM (newt, newkind, p', p', castVoidStar theend))
          
        (* SCALAR -> INDEX, WILD, SEQ, FSEQ *)
      | N.Scalar, (N.Index|N.Wild|N.Seq|N.FSeq|N.FSeqN|N.SeqN) ->
          if not (isZero p) && newkind <> N.Wild then
            ignore (warn "Casting scalar (%a) to non-WILD pointer in %s!"
                      d_exp p !currentFunction.svar.vname);
          let newbase, doe' = 
            if !interceptCasts && (isInteger p = None) then begin
              incr interceptId;
              let tmp = makeTempVar !currentFunction voidPtrType in
              Lval(var tmp),
              CConsR (doe,
                      call (Some (var tmp)) 
                        (Lval(var interceptCastFunction.svar)) 
                        [ p ;integer !currentFileId; integer !interceptId ])
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
          let p', _, _, acc' = indexToSafe p newPointerType b bend empty in
          finishDoe acc', L(newt, newkind, castP p')      
       (* INDEX -> SEQ *)
      | N.Index, N.Seq ->
          let p', b', bend', acc' = indexToSeq p b bend empty in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      
       (* INDEX -> FSEQ *)
      | N.Index, N.FSeq ->
          let p', b', bend', acc' = indexToFSeq p b bend empty in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      

       (* SEQ -> SAFE. Must do bounds checking *)
      | (N.Seq|N.SeqN), N.Safe ->
          let p', _, _, acc' = seqToSafe p newPointerType b bend empty in
          finishDoe acc', L(newt, newkind, castP p')      
       (* SEQ -> FSEQ *)
      | (N.Seq|N.SeqN), N.FSeq ->
          let p', b', bend', acc' = seqToFSeq p b bend empty in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      
      | N.SeqN, N.FSeqN ->
          let p', b', bend', acc' = seqToFSeq p b bend empty in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      

       (* FSEQ -> SAFE. Must do bounds checking *)
      | (N.FSeq|N.FSeqN), N.Safe ->
          let p', _, _, acc' = fseqToSafe p newPointerType b bend empty in
          finishDoe acc', L(newt, newkind, castP p')      

       (* FSEQ -> SEQ. *)
      | (N.FSeq|N.FSeqN), N.Seq ->
          doe, FM(newt, newkind, castP p, b, bend)
      | N.FSeqN, (N.Seq|N.SeqN) ->
          doe, FM(newt, newkind, castP p, b, bend)
      | N.FSeqN, N.FSeq -> 
          (doe, FM (newt, newkind, castP p, b, bend))

      (* SeqN -> SEQ *)
      | N.SeqN, N.Seq -> 
          doe, FM(newt, newkind, castP p, b, bend)
          
      | N.SeqN, (N.String|N.ROString) ->
          let p', b', bend', acc' = 
            seqNToString p newPointerType b bend empty in
          finishDoe acc', L(newt, newkind, castP p')  

      | N.FSeqN, (N.String|N.ROString) ->
          let p', b', bend', acc' = 
            fseqNToString p newPointerType b bend empty in
          finishDoe acc', L(newt, newkind, castP p')  

      | N.String, (N.FSeqN|N.FSeq) ->
          let p', b', bend', acc' = stringToFseq p b bend empty in
          finishDoe acc', FM(newt, newkind, castP p', b', bend') 
          (* wes: was ( p', b', bend') at the end *)

      | N.String, (N.SeqN|N.Seq) ->
          let p', b', bend', acc' = stringToSeq p b bend empty in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')  

      | N.Wild, N.ROString -> 
          let p', b', bend', acc' = wildToROString p b bend empty in
          finishDoe acc', L(newt, newkind, castP p')

      | N.Index, N.ROString -> 
          let p', b', bend', acc' = indexToROString p b bend empty in
          finishDoe acc', L(newt, newkind, castP p')

      | N.ROString, (N.FSeq|N.FSeqN) -> 
        ignore (warn "Warning: wes-is-lazy cast from ROSTRING -> FSEQ[N]") ;
        ignore (warn "castTo(%a -> %a.@!%a@!%a)" 
                 N.d_opointerkind oldk N.d_opointerkind newkind 
                 d_fexp fe
                 d_plaintype oldt)       ;
          let p', b', bend', acc' = stringToFseq p b bend empty in
          finishDoe acc', FM(newt, newkind, castP p', b', bend') 

       (******* UNIMPLEMENTED ********)
      | N.String, N.Wild 
            when 
          (match p with 
            Const(CStr s) when prefix "booo_exp: " s -> true 
          | _ -> false) -> (* This occurs because such strings are generated 
                            * in case of error *)
              (doe, FM(newt, newkind, castP p, zero, zero))

      | _, _ -> 
          E.s (unimp "castTo(%a -> %a.@!%a@!:%a@!->%a)" 
                 N.d_opointerkind oldk N.d_opointerkind newkind 
                 d_fexp fe
                 d_plaintype oldt d_plaintype newt)      
  end

(* Rename this as castTo *)
let castToDebug (fe: fexp) (newt: typ)
                    (doe: stmt clist) : stmt clist * fexp =
  let (doe', fe') as res = castTo fe newt doe in
  ignore (E.log "castToDebug:\n  fe=%a\n  newt= %a\n fe'=%a\n\n"
            d_fexp fe d_plaintype newt d_fexp fe');
  res


(* Cache some iterator variables for the current function. *)
let iterVars: varinfo list ref = ref [] (* Clean this when you start a new 
                                         * function *)
let globInitIterVars: varinfo list ref = ref [] (* A special list of iterator 
                                                 * variables for the global 
                                                 * initializer *)
let withIterVar (doit: varinfo -> 'a) : 'a = 
  let newv = 
    match !iterVars with
      v :: resta -> 
        iterVars := resta; 
        v

    | [] -> makeTempVar !currentFunction ~name:"iter" intType
  in
  let res = doit newv in
  (* Make it available again *)
  iterVars := newv :: !iterVars;
  res
  

(* Various reasons why we might want to check an LV *)  
type checkLvWhy = 
    ToWrite of exp
  | ToRead

let rec checkMem (why: checkLvWhy) 
                 (inlv: curelval) (* The lval that we are reading or writing *)
(*
                 (lv: lval) (base: exp) (bend: exp)
                 (lvt: typ) 
                 (pkind: N.opointerkind)
*)
 : stmt clist = 
(*  ignore (E.log "checkMem: lv=%a@!  lvt: %a@!" 
            d_plainlval lv d_plaintype lvt); *)
  (* Maybe it is a table. In that case, get the true base and end *)
  (* See if a table pointer *)
  let newk = N.stripT inlv.plvk in 
  if newk <> inlv.plvk then begin (* A table pointer *)
    let base, bend, stmts = fromTable inlv.plvk (mkAddrOf inlv.lv) in
    append stmts 
      (checkMem why { inlv with lvb = base; lve = bend; 
                                plvk = newk; 
                                lvstmts = append inlv.lvstmts stmts})
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
      | _ -> E.s (bug "getLen");
    in
    (* Now the tag checking. We only care about pointers. We keep track of 
     * what we write in each field and we check pointers in a special way.  *)
    let rec doCheckTags (why: checkLvWhy) (where: lval) 
                        (t: typ) (pkind: N.opointerkind) 
                        (acc: stmt clist) : stmt clist = 
      match unrollType t with 
      | (TInt _ | TFloat _ | TEnum _) -> acc
      | TComp (comp, _) when isFatComp comp -> begin (* A fat pointer *)
          match why with
            ToRead -> (* a read *)
              if pkind = N.Wild then
                CConsL 
                  (checkFatPointerRead inlv.lvb 
                     (mkAddrOf(where)) (getLenExp ()),
                   acc)
              else
                acc
          | ToWrite towrite -> (* a write *)
              let _, whatp, whatb, _ = readFieldsOfFat towrite t in
              if pkind = N.Wild then
                CConsL
                  (checkFatPointerWrite inlv.lvb (mkAddrOf(where)) 
                     whatb whatp (getLenExp ()),
                   acc)
              else
                CConsL (checkStoreFatPtr whatp whatb, acc)
      end 
      | TComp (comp, _) -> (* A Struct or a union. Note that this means that 
                            * for unions we check all pointers in all fields, 
                            * even if we end up checking some more that once *)
          let doOneField acc fi = 
            let newwhere = addOffsetLval (Field(fi, NoOffset)) where in
            let newwhy = 
              match why with 
                ToRead -> ToRead
              | ToWrite (Lval whatlv) -> 
                  ToWrite (Lval (addOffsetLval (Field(fi, NoOffset)) whatlv))
                  (* sometimes in Asm outputs we pretend that we write 0 *)
              | ToWrite (Const(CInt64(z, _, _))) when z = Int64.zero -> ToRead
              | ToWrite e -> E.s (unimp "doCheckTags (%a)" d_exp e)
            in
            doCheckTags newwhy newwhere fi.ftype pkind acc
          in
          List.fold_left doOneField acc comp.cfields
            
      | TArray(bt, lo, a) -> begin
          match unrollType bt with
            TInt _ | TFloat _ | TEnum _ -> acc
          | _ -> begin (* We are reading or writing an array *)
              let len = 
                match lo with Some len -> len 
                | _ -> E.s (unimp "Reading or writing an incomplete type") in
            (* Make an interator variable for this function *)
              withIterVar
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
                          E.s (unimp "doCheckTags: write (%a)" d_exp e)
                    in
                    doCheckTags whyelem
                      (addOffsetLval (Index(itvar, NoOffset)) where)
                      bt
                      pkind (* ??? *)
                      empty
                  in
                  append
                    (fromList
                       (mkForIncrOptim 
                          ~iter: it
                          ~first: zero
                          ~past: len
                          ~incr: one
                          ~body: (toList initone)))
                    acc)
          end
      end
            
      | TPtr(_, _) -> (* This can only happen if we are writing to an untagged 
                       * area. All other areas contain only fat pointers *)
          begin
            match why with
              ToWrite x -> CConsL (checkStorePtr x, acc)
            | _ -> acc
          end
      | _ -> E.s (unimp "unexpected type in doCheckTags: %a\n" d_type t)
    in
    (* See first what we need in order to check tags *)
    let checkTags = 
      (* Take a look at the type involved. If it does not contain pointers 
       * then we don't need to check anything *)
      if existsType (function TPtr _ -> ExistsTrue | _ -> ExistsMaybe) 
                    inlv.lvt then begin
        (* Call doCheckTags anyway because even for safe writes it needs to 
         * check when pointers are written  *)
        doCheckTags why inlv.lv inlv.lvt inlv.plvk inlv.lvstmts
      end else
        inlv.lvstmts
    in
    (* For a write through a WILD pointer also zero the tags *)
    let zeroAndCheckTags = 
      match inlv.plvk, why with 
        N.Wild, ToWrite _ -> 
          CConsL(checkZeroTags inlv.lvb (getLenExp ()) inlv.lv inlv.lvt, 
                 checkTags)
      | _ -> checkTags
    in
    (* Now see if we need to do bounds checking *)
    let iswrite = (match why with ToWrite _ -> true | _ -> false) in
    let checkb = 
      append 
        (checkBounds iswrite getLenExp inlv) 
        zeroAndCheckTags 
    in
    (* See if we need to generate the length *)
    (match !lenExp with
      None -> checkb
    | Some _ -> 
        let ptr = ptrOfBase inlv.lvb in
        let start, _ = getRangeOfBitfields inlv.lv inlv.lvt in
        CConsL(checkFetchLength (getVarOfExp (getLenExp ())) 
                 start inlv.lvb,
               checkb))
  end
  
          


(***** Check the return value *)
let rec checkReturnValue 
    (typ: typ) 
    (e: exp)
    (before: stmt clist) : 

    (* Return the accumulated statements *)
    stmt clist = 
  match unrollType typ with
    TInt _ | TEnum _ | TFloat _ | TVoid _ -> before
  | TPtr (t, _) -> 
      (* This is a lean pointer *) 
      CConsR (before, checkReturnPtr e)

  | TComp (comp, _) when isFatComp comp -> 
      let ptype, ptr, fb, fe = readFieldsOfFat e typ in
      (* Get the component that is null if an integer *)
      let nullIfInt = 
        match kindOfType ptype with
          N.Wild|N.Index|N.Seq|N.SeqN -> fb
        | N.FSeq|N.FSeqN -> fe
        | _ -> E.s (unimp "checkReturn: unexpected kind of fat type")
      in
      CConsR (before, checkReturnFatPtr ptr nullIfInt)

    (* A regular struct *)                                          
  | TComp (comp, _) when comp.cstruct ->
      (* Better have an lvalue *)
      let lv = match e with
        Lval lv -> lv
      | _ -> E.s (unimp "checkReturnValue: return comp not an lval")
      in
      List.fold_left 
        (fun before f -> 
          checkReturnValue f.ftype 
            (Lval (addOffsetLval (Field(f, NoOffset)) lv)) before)
        before
        comp.cfields
        
  | _ -> E.s (unimp "checkReturnValue: unexpected return type\n")
      

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
    : (lval -> stmt clist -> stmt clist) =
  match unrollType t with
    TInt _ | TFloat _ | TEnum _ -> (fun lv acc -> acc)
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
        
        fun lv acc -> CConsL (mkSet lv (doCastT zero intType t), acc)
      else 
        fun lv acc -> acc
  end
  | TComp (comp, a) when comp.cstruct -> begin (* A struct *)
      match comp.cfields with
        [s; a] when s.fname = "_size" && a.fname = "_array" ->
              (* Sized arrays *)
          let bt, sizeo = 
            match unrollType a.ftype with
              TArray(bt, lo,_) -> bt,lo
              | _ -> E.s (bug "SIZED array is not an array\n")
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
            let acc2 = CConsL (mkSet thesizelv thissize, acc1) in
            withivar 
              (fun iter -> 
                append
                  (fromList
                     (mkForIncrOptim iter zero l one
                        (toList
                           (initone 
                              (addOffsetLval (Index (Lval(var iter), 
                                                     NoOffset)) 
                                 thearraylv)
                              empty))))
                  acc2)

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
        if not (isCharType bt) then
          E.s (unimp "NULLTERM array of base type %a" d_type bt);
        fun lv acc ->
          CConsL
            (mkSet (addOffsetLval
                      (Index((charArrayEndpOffset l), NoOffset)) lv)
               zero,
             acc)
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
              append
                (fromList
                   (mkForIncrOptim iter zero l one
                      (toList
                         (initone 
                            (addOffsetLval (Index (Lval(var iter), 
                                                   NoOffset)) lv)
                            empty))))
                acc1)
      end
    (* A union type *)
  | TComp (comp, a) -> begin
      (* Go through all of the fields and find the one that is largest. 
       * Initialize that one. *)
      let (maxfld, themax) = 
        List.fold_left 
          (fun (bestsofar, bestval) f -> 
            let bitsthis = 
              try bitsSizeOf f.ftype with SizeOfError _ -> 
                E.s (unimp "initializing union with open fields")
            in
            if bitsthis > bestval then
              (Some f, bitsthis)
            else
              (bestsofar, bestval)) (None, -1) comp.cfields in
      let toinit = 
        match maxfld with Some f -> f 
        | _ -> E.s (unimp "cannot find widest field in union %s" comp.cname)
      in
      (* ignore (E.log "Will initialize field %s (with size %d)\n" 
                toinit.fname themax); *)
      let initone = initializeType toinit.ftype withivar mustZero endo in
      fun lv acc ->
        initone (addOffsetLval (Field(toinit, NoOffset)) lv) acc 
    end

  | _ -> E.s (unimp "initializeType (for type %a)" d_plaintype t)

    
    

(* Create and accumulate the initializer for a variable *)
let initializeVar (withivar: (varinfo -> 'a) -> 'a) (* Allocate an iteration 
                                                     * variable temporarily *)
                  (acc: stmt clist)
                  (v: varinfo) 
                   : stmt clist = 
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
        CConsL
          (mkSet (Var v, Field(lfld, NoOffset)) words,
           (* And the loop to initialize the tags with zero *)
           (if not v.vglob then
             append
               (fromList
                  (mkForIncr iter zero (doCast tagwords intType) one 
                     [mkSet (Var v, Field(tfld, 
                                          Index (Lval(var iter), 
                                                 NoOffset))) 
                         zero]))
               acc'
           else
             acc')))
  end else begin
    let doinit = initializeType v.vtype withivar (not v.vglob) None in
    doinit (Var v, NoOffset) acc
  end

let rec stringLiteral (s: string) (strt: typ) : stmt clist * fexp = 
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
      theFile := consGlobal (GVar (gvar, Some varinit, !currentLoc)) !theFile;
      let result = StartOf (Var gvar, Field(dfield, NoOffset)) in
      let voidStarResult = castVoidStar result in
      (* Register the area *)
      let regarea = 
        registerArea
          [ integer registerAreaTaggedInt;
            voidStarResult; zero ] !extraGlobInit
      in
      (* Add the registration to the global initializer *)
      if regarea != !extraGlobInit then extraGlobInit := regarea;
      (empty, FM (fixChrPtrType, N.Wild,
               result, 
               castVoidStar result, zero))
  | N.Seq | N.Safe | N.FSeq | N.String | N.ROString | N.SeqN | N.FSeqN ->
      (* sm: to allow FSeq and ROStrings to coexist, always add another *)
      (* null byte, and let the user access everything up to, but *not* *)
      (* including, that last byte *)
      (* old: let l = (if isNullTerm k then 0 else 1) + String.length s in *)
      let (fatString, l) = 
        if true then
          (s ^ "\000"), ((String.length s)+1)
        else
          s, (String.length s)
      in
      (*(trace "sm" (dprintf "boxing string[%d]: %s\n" l (String.escaped fatString)));*)

      (* Make a global variable that points to this string. This way we can
       * register the area just once in the global initializer. *)
      let gvar = makeGlobalVar (newStringName ()) charPtrType in
      gvar.vstorage <- Static;
      theFile :=
         consGlobal (GVar (gvar, Some (SingleInit(Const(CStr fatString))),
                           !currentLoc)) !theFile;
      (* Get the end so that we can make a SEQ *)
      let theend = BinOp(IndexPI, Lval (var gvar), integer l, charPtrType) in
      (* Register the area *)
      let regarea =
        registerArea
          [ integer registerAreaSeqInt;
            castVoidStar (Lval (var gvar));
            castVoidStar theend ] !extraGlobInit
      in
      (* Add the registration to the global initializer *)
      if regarea != !extraGlobInit then extraGlobInit := regarea;
      let res =
        match k with
          N.Safe | N.String | N.ROString ->
            mkFexp1 fixChrPtrType (Lval (var gvar))
        | N.Seq | N.SeqN | N.FSeq | N.FSeqN ->
            mkFexp3  fixChrPtrType
              (Lval (var gvar))
              (Lval (var gvar))
              theend

        | _ -> E.s (bug "stringLiteral")
      in
      (empty (* single (mkSet (var tmp) (Const (CStr fatString)))*), res)
        
  | N.WildT | N.SeqT | N.FSeqT | N.SeqNT | N.FSeqNT -> 
      let kno_t = N.stripT k in
      let strtno_t = 
        match strt with
          TPtr(chrt, a) -> TPtr(chrt, [N.k2attr kno_t])
        | _ -> E.s (bug "Making a string of a non char type\n")
      in
      let s1, fe = stringLiteral s strtno_t in
      (* Now cast it to the desired string type *)
      castTo fe fixChrPtrType s1

  | _ -> E.s (unimp "String literal to %a" N.d_opointerkind k)


(*************** Handle Allocation ***********)
let pkAllocate (ai:  allocInfo) (* Information about the allocation function *)
               (dest:  lval)   (* Where to put the result *)
               (f:  exp)        (* The allocation function *)
               (args: exp list) (* The arguments passed to the allocation *) 
    : stmt clist =
  (*(trace "malloc" (dprintf "Al@[location call of %a.@?type(vi) = %a@!@] vtype = %a@!"*)
  (*                         d_exp f d_plaintype vi.vtype*)
  (*                         d_plaintype vtype));*)
  let destt = typeOfLval dest in
  let k = kindOfType destt in
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
  let ptrtype, dest_ptr = 
    match k with 
      N.Wild | N.Seq | N.FSeq | N.SeqN | N.FSeqN | N.Index -> 
        let fptr, fbase, fendo = getFieldsOfFat destt in 
        fptr.ftype, addOffsetLval (Field(fptr, NoOffset)) dest
    | N.Safe | N.String | N.ROString
    | N.WildT | N.SeqT | N.FSeqT | N.SeqNT | N.FSeqNT | N.IndexT 
      -> destt, dest
    | _ -> E.s (unimp "pkAllocate: ptrtype (%a)" N.d_opointerkind k)
  in
  (* Get the base type *)
  let basetype = 
    match unrollType ptrtype with
      TPtr(bt, _) -> bt
    | _ -> E.s (bug "Result of allocation is not a pointer type")
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
  let alloc = call (Some (var tmpp)) f (ai.aiNewSize allocsz args) in
  (* Adjust the allocation pointer past the size prefix (if any) *)
  let adjust_ptr = 
    match kno_t with
      N.Index | N.Wild -> 
        mkSet (var tmpp) (doCast (BinOp(IndexPI, 
                                        doCast tmpvar charPtrType, 
                                        integer 4, charPtrType))
                            ptrtype)
    | _ -> mkEmptyStmt ()
  in

  (* Save the pointer value into the final result *)
  let assign_p = mkSet dest_ptr tmpvar in
  (* And the base, if necessary. This one is equal to the pointer value *)
  let assign_base = 
    match k with 
      N.Wild | N.Seq | N.SeqN | N.Index -> begin
        let fptr, fbaseo, fendo = getFieldsOfFat destt in
        match fbaseo with
          Some fbase -> (mkSet (addOffsetLval (Field(fbase, NoOffset)) dest)
                           (doCast tmpvar voidPtrType))
        | _ -> mkEmptyStmt ()
      end
    | _ -> mkEmptyStmt ()
  in

  (* Store the size in memory if necessary *)
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
    let putnullterm (theend: exp) = 
      mkSet (mkMem (doCast theend charPtrType) NoOffset)
        (doCast zero charType)
    in
    match kno_t with
      N.Wild -> 
        (* Zero the tags *)
        if mustZero then
          single (call None
                    (Lval (var checkZeroTagsFun.svar))
                    [ tmpvar;                      (* base *)
                      nrdatawords;                 (* basenrwords *)
                      tmpvar;                      (* where to start *)
                      nrdatabytes;                 (* size of area to zero *)
                      zero (* offset *) ])
        else
          single (mkEmptyStmt ())
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
          initializeType basetype withIterVar mustZero
            (Some theend) (mkMem tmpvar NoOffset) empty
        in
        CConsL (check_enough, inits)

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
          initializeType basetype withIterVar mustZero None
            (mkMem tmpvar NoOffset) empty
            
        in
        let initializeAll = 
          if initone = empty then 
            single (mkSet (var tmpp) (doCast theend ptrtype))
          else 
            fromList
              (mkFor 
                 ~start:[mkEmptyStmt ()]
                 ~guard:(BinOp(Le, BinOp(PlusA, 
                                         doCast tmpvar upointType, 
                                         SizeOf(ptrtype), upointType),
                               doCast theend upointType, intType))
                 ~next:[mkSet (var tmpp) 
                           (BinOp(IndexPI, tmpvar, one, ptrtype))]
                 ~body:(toList initone))
        in 
        CConsL(savetheend, 
               append initializeAll
                 (if k = N.FSeqN || k = N.SeqN then 
                   single 
                     (mkSet (Mem(BinOp(MinusPI,
                                       doCast theend charPtrType,
                                       one, charPtrType)), NoOffset)
                        (doCast zero charType))
                 else empty))

    | N.String | N.ROString -> (* Allocate this as SeqN, with a null term *)
        ignore (warn "Allocation of string. Use FSEQN instead. (%a)"
                  d_lval dest);
        single (mkSet (Mem(BinOp(PlusPI,
                                   doCast tmpvar charPtrType,
                                   (charArrayEndpOffset nrdatabytes),
                                   charPtrType)), NoOffset)
                  (doCast zero charType))

    | _ -> E.s (bug "pkAllocate: init")
  in
  (* Now assign the end if necessary. We do it this late because in the case 
   * of sequences we now know the precise end of the allocated sequence. For 
   * them the tmp variable has iterated over a number of instances of the 
   * type and has stopped when there is not more room for one more instance. *)
  let assign_end = 
    match k with 
      N.Seq | N.SeqN | N.FSeq | N.FSeqN -> begin
        let fptr, fbase, fendo = getFieldsOfFat destt in
        match fendo with
          None -> mkEmptyStmt ()
        | Some fend -> 
            mkSet (addOffsetLval (Field(fend, NoOffset)) dest) tmpvar
      end
    | _ -> mkEmptyStmt ()
  in
  (* Now see if we must register the whole area *)
  let register_area = 
    match kno_t with
    | N.Safe -> empty
    | N.Wild | N.Index -> 
        let areaKind = 
          if kno_t = N.Wild then 
            registerAreaTaggedInt else registerAreaSizedInt
        in
        registerArea [ integer areaKind;
                       castVoidStar (Lval dest_ptr);
                       zero ] empty
    | N.Seq | N.SeqN | N.FSeq | N.FSeqN | N.String | N.ROString -> 
        registerArea [ integer registerAreaSeqInt;
                       castVoidStar (Lval dest_ptr);
                       castVoidStar tmpvar ] empty
    | _ -> E.s (bug "pkAllocate: register_area: %a" N.d_opointerkind k)
  in        
  CConsL(alloc,
         CConsL(adjust_ptr,
                CConsL(assign_p,
                       CConsL(assign_base,
                              CConsL(setsz,
                                     append init
                                       (CConsL (assign_end, 
                                                register_area)))))))

(* Given a sized array type, return the size and the array field *)
let getFieldsOfSized (t: typ) : fieldinfo * fieldinfo = 
  match unrollType t with
   TComp (comp, _) when comp.cstruct -> begin
      match comp.cfields with 
        s :: a :: [] when s.fname = "_size" && a.fname = "_array" -> s, a
      | _ -> E.s (bug "getFieldsOfSized")
    end
   | _ -> E.s (bug "getFieldsOfSized %a\n" d_type t)
  



(* Remember names that we have already mangled *)
let mangledNames : (string, unit) H.t = H.create 123
(* Remeber if we mangled the name of main *)
let mangledMainName : string ref = ref "" (* We'll set this to the mangled 
                                           * name for main, if we see one *)
let fixupGlobName vi =
  (* Scan a type and compute a list of qualifiers that distinguish the
   * various possible combinations of qualifiers *)
   let rec qualNames acc = function
      TInt _ | TFloat _ | TVoid _ | TEnum _ -> acc
    | TPtr(t', _) as t -> 
        let pk = kindOfType t in
        (* See if this is a modified va_list *)
        let acc' = 
          match unrollType t' with 
            TComp(ci, _) when ci.cstruct && ci.cname = "__ccured_va_list" -> 
              "v" :: acc
          | _ -> acc
        in
        pkQualName pk acc' (fun acc'' -> qualNames acc'' t')
    | TArray(t', _, a) ->
        let acc' =
          (* Choose the attributes so that "s" is always the C represent *)
          if filterAttributes "sized" a <> [] then "l" :: acc else "s" :: acc
        in
        qualNames acc' t'
    | TFun(tres, args, _, _) -> 
        let acc' = qualNames acc tres in
        List.fold_left 
          (fun acc a -> qualNames acc a.vtype) acc' (argsToList args)

    | TNamed (_, t, _) -> qualNames acc t

    (* We only go into struct that we created as part of "sized" or "seq" or 
     * "fatp" *)
    | (TComp (comp, _) as t) -> begin
        if isFatComp comp then 
          let pf, _, _ = getFieldsOfFat t in
          qualNames acc pf.ftype
        else 
          match comp.cfields with
          | [s;a] when s.fname = "_size" && a.fname = "_array" -> 
              qualNames acc a.ftype
          | _ -> acc
    end
  in
  (* weimer: static things too! *)
  if vi.vglob && (* vi.vstorage <> Static &&  *)
    not (H.mem leaveAlone vi.vname) &&
    not (isAllocFunction vi.vname) &&
    not (H.mem mangledNames vi.vname) then
    begin
      (* For vararg functions we pretend that the type also contains the 
       * union elements *)
      let vitype = 
        match vi.vtype with 
          TFun (rt, args, true, a) -> begin
            match filterAttributes "boxvararg" a with 
              [Attr(_, [ASizeOf t])] -> 
                let types = 
                  match t with
                    TComp(ci, _) -> List.map (fun f -> f.ftype) ci.cfields
                  | _ -> [t]
                in
                let args' = 
                  match args with
                    Some a -> 
                      Some (a @ (List.map (fun t -> makeVarinfo "" t) types))
                  | _ -> E.s (bug "vararg function without prototype")
                in
                TFun(rt, args', true, a)
            | _ -> vi.vtype
          end
        | _ -> vi.vtype
      in
      let quals = qualNames [] vitype in
      let suffix =
        let rec allSafe = function (* Only default qualifiers *)
            [] -> true
          | "s" :: rest -> allSafe rest
          | _ -> false
        in
        if allSafe quals then ""
        else (List.fold_left (fun acc x -> x ^ acc) "" quals)
      in
      let suffix = if mustBeTagged vi then "t" ^ suffix else suffix in
      let newname =if suffix = "" then vi.vname else vi.vname ^ "_" ^ suffix in
      H.add mangledNames newname ();
      if vi.vname = "main" && vi.vstorage <> Static then 
        begin
          (* Change the name of "main" to "trueMain" *)
          mangledMainName := newname;
          vi.vname <- "trueMain";
        end else
        vi.vname <- newname
    end


class unsafeVisitorClass = object
  inherit nopCilVisitor

  method vlval (lv: lval) : lval visitAction =
    (* Do everything after we handle the children *)
    (* Add offset to go into fat types *)
    let rec fixLastOffset (lv: lval) : lval = 
      let t = typeOfLval lv in
      match unrollType t with
        TComp (comp, _) when comp.cstruct -> begin
          match comp.cfields with
            (* A sized array *)
            f1 :: f2 :: [] when (f1.fname = "_size" && f2.fname = "_array") -> 
              fixLastOffset (addOffsetLval (Field(f2, NoOffset)) lv)
            (* A tagged struct *)
          | f1 :: f2 :: _ when (f1.fname = "_len" && f2.fname = "_data") ->
              fixLastOffset (addOffsetLval (Field(f2, NoOffset)) lv)
          | f1 :: _ when f1.fname = "_p" -> 
              fixLastOffset (addOffsetLval (Field(f1, NoOffset)) lv)
          | _ -> lv
        end
      | _ -> lv
    in
    let rec fixOffsets (lv: lval) (off: offset) = 
      match off with 
        NoOffset -> lv
      | Field (fi, off) -> 
          fixOffsets 
            (fixLastOffset (addOffsetLval (Field (fi, NoOffset)) lv))
            off
      | Index (e, off) -> 
          fixOffsets 
            (fixLastOffset (addOffsetLval (Index (e, NoOffset)) lv))
            off
    in
    let doafter (lv: lval) = 
      match lv with
        Var v, off -> 
          let lv0 = fixLastOffset (Var v, NoOffset) in
          fixOffsets lv0 off
      | Mem e, off -> 
          let lv0 = fixLastOffset (mkMem e NoOffset) in
          fixOffsets lv0 off
    in
    ChangeDoChildrenPost (lv, doafter)
end

let unsafeVisitor = new unsafeVisitorClass


    (************* STATEMENTS **************)
let rec boxblock (b: block) : block = 
  if hasAttribute "nobox" b.battrs then
    visitCilBlock unsafeVisitor b
  else begin
    let res = 
      toList 
        (List.fold_left 
           (fun acc s -> append acc (boxstmt s)) empty b.bstmts)
    in
    { bstmts = if compactBlocks then compactStmts res else res;
      battrs = b.battrs
    } 
  end

and boxstmt (s: Cil.stmt) : stmt clist = 
   (* Keep the original statement, but maybe modify its kind. This way we 
    * maintain the labels and we have no need to change the Gotos and the 
    * cases in the Switch *)
  try
    match s.skind with 
    | Break _ | Continue _ | Goto _ -> single s
    | Return (None, l) -> 
        currentLoc := l; 
        CConsL(unregisterStmt (), CSeq(CList !heapifiedFree, single s))

    | Return (Some e, l) -> 
        currentLoc := l;
        let retType =
          match !currentFunction.svar.vtype with 
            TFun(tRes, _, _, _) -> tRes
          | _ -> E.s (bug "Current function's type is not TFun")
        in 
        let (doe', e') = boxexpf e in
        let (doe'', e'') = castTo e' retType doe' in
        let (et, doe2, e2) = fexp2exp e'' doe'' in
        let doe3 = checkReturnValue et e2 doe2 in
        s.skind <- Instr [];  
        CConsL(s, 
               append doe3 
                 (CConsL(unregisterStmt (), 
                         CSeq(CList !heapifiedFree, 
                              single (mkStmt (Return (Some e2, l)))))))
                      
    | Loop (b, l) -> 
        currentLoc := l;
        s.skind <- Loop (boxblock b, l);
        single s
   
    | Block b -> 
        s.skind <- Block (boxblock b);
        single s

    | If(be, t, e, l) -> 
        currentLoc := l;
        let (_, doe, e') = boxexp (CastE(intType, be)) in
        s.skind <- Instr [];
        CConsL(s, 
               CConsR(doe, mkStmt (If(e', boxblock t, boxblock e, l))))
    | Instr il -> 
        (* Do each instruction in turn *)
        let b = 
          List.fold_left (fun acc i -> append acc (boxinstr i)) empty il in
        s.skind <- Instr [];
        CConsL (s, b)

    | Switch (e, b, cases, l) -> 
        currentLoc := l;
        (* Cases are preserved *)
        let (_, doe, e') = boxexp (CastE(intType, e)) in
        s.skind <- Instr [];
        CConsL(s, CConsR(doe, mkStmt (Switch (e', boxblock b, cases, l))))

  with e -> begin
    ignore (E.log "boxstmt (%s) in %s\n" 
              (Printexc.to_string e) !currentFunction.svar.vname);
    single (mkStmtOneInstr (dInstr (dprintf "booo_statement(%a)" d_stmt s) 
                              !currentLoc))
  end


and boxinstr (ins: instr) : stmt clist = 
  if debugInstr then ignore (E.log "Boxing %a\n" d_instr ins);
  try
    match ins with
    | Set (lv, e, l) -> 
        currentLoc := l;
        let blv = boxlval lv in
        let (doe, e') = boxexpf e in (* Assume et is the same as lvt *)
        (* Now do a cast, just in case some qualifiers are different *)
        let (doe', e2) = castTo e' blv.lvt doe in
        let (_, doe3, e3) = fexp2exp e2 doe' in
        let check = 
          match blv.lv with
            Mem _, _ -> 
              checkMem (ToWrite e3) blv
          | Var vi, off when (vi.vglob || blv.plvk != N.Safe) -> 
              checkMem (ToWrite e3) blv
          | _ -> empty
        in
        append blv.lvstmts (append doe3 (CConsR (check, mkSet blv.lv e3)))

        (* Check if the result is a heapified variable *)
    | Call (Some (Var vi, NoOffset), f, args, l) 
        when H.mem heapifiedLocals vi.vname -> 
          currentLoc := l;
          let newb, newoff = H.find heapifiedLocals vi.vname in
          let stmt1 = boxinstr (Call (Some (newb, newoff), f, args, l)) in
          stmt1

    | Call(vio, f, args, l) ->
        currentLoc := l;
        let (ft, dof, f', fkind) = 
          match f with
            Lval(Var vi, NoOffset) -> begin
              (* Sometimes it is possible that we have not seen this varinfo. 
               * Maybe it was introduced by the type inferencer to mark an 
               * independent copy of the function  *)
              if not (H.mem leaveAlone vi.vname) && not (isAllocFunction vi.vname) then begin
                vi.vtype <- fixupType vi.vtype;
                fixupGlobName vi
              end;
              let (ft, dof, f') = boxexp f in
              let fkind = 
                match N.nodeOfAttrlist vi.vattr with
                  None -> N.Safe
                | Some n -> n.N.kind
              in
              (ft, dof, f', fkind)
            end
          | Lval(Mem base, NoOffset) -> 
              let blv (* rest, lvkind, lv', lvbase, lvend, dolv *) = 
                boxlval (Mem base, NoOffset) in
              (blv.lvt, CConsR (blv.lvstmts, 
                                checkFunctionPointer 
                                  (mkAddrOf blv.lv) 
                                  blv.lvb blv.plvk (List.length args)), 
               Lval(blv.lv), 
               blv.plvk)
                
          | _ -> E.s (unimp "Unexpected function expression")
        in
        let (ftret, ftargs, isva) =
          match unrollType ft with 
            TFun(fret, fargs, isva, _) -> (fret, fargs, isva) 
          | _ -> E.s (unimp "call of a non-function: %a @!: %a" 
                        d_plainexp f' d_plaintype ft) 
        in
        let leavealone, isallocate = 
          match f' with
            Lval(Var vf, NoOffset) -> 
              H.mem leaveAlone vf.vname,   getAllocInfo vf.vname

          | _ -> false, None
        in
        let (doargs, args') =
          if leavealone then (* We leave some functions alone. But we check 
                              * all arguments and, if needed we pack the 
                              * result  *)
            let rec doArgs = function
              | [] -> (empty, [])
              | a :: resta -> 
                  let (at, doa, a') = boxexp a in
                  let (doresta, resta') = doArgs resta in
                  let (checka, a'') = 
(* !!!! remove this *)         
                    if isFatType at then 
                      (empty, readPtrField a' at)
                    else  
                      (empty, a')
                  in
                  (append doa (append checka doresta), a'' :: resta')
            in
            doArgs args
          else
            let rec doArgs restargs restargst = (* The types of functions 
                                                 * have been fixed already  *) 
              match restargs, restargst with
                [], [] -> empty, []
              | a :: resta, t :: restt -> 
                  let (doa, fa') = boxexpf a in
(*                  ignore (E.log "boxCall: (fun = %a) a=%a\n  fa'=%a\n\n"
                            d_exp f
                            d_plainexp a d_fexp fa');  *)
                  let (doa', fa'') = castTo fa' t.vtype doa in
                  let (_, doa'', a2) = fexp2exp fa'' doa' in
                  let (doresta, resta') = doArgs resta restt in
                (append doa'' doresta,  a2 :: resta')
              | a :: resta, [] -> 
                  (* This is a case when we call with more args than the 
                   * prototype has. We better be calling a vararg or 
                     a WILD function *)
                  if fkind <> N.Wild && not isva then 
                    E.s (bug "Calling non-wild %a with too many args"
                           d_exp f);
                  let (doa, fa') = boxexpf a in
                  let (doa', fa'') = 
                    if isva then 
                      (doa, fa') 
                    else (* A WILD function *)
                      (* Do not cast if already a WILD thing *)
                      if kindOfFexp fa' = N.Wild then (doa, fa') 
                      else castTo fa' !wildpVoidType doa 
                  in
                  let (_, doa'', a2) = fexp2exp fa'' doa' in
                  let (doresta, resta') = doArgs resta [] in
                  (append doa'' doresta, a2 :: resta')
              | _ -> E.s (unimp "too few arguments in call to %a" d_exp f)
            in
            doArgs args (argsToList ftargs)
            
        in
        let finishcall = 
          match vio with 
            None -> interceptCall None f' args'

          | Some destlv -> begin
              (* Always put the result of the call in a temporary variable so 
               * that the actual store into the destination occurs with a Set 
               * and the right checks are inserted *)
              let tmp = makeTempVar !currentFunction 
                  (if isallocate <> None then 
                    (* For allocation we make the temporary the same type as 
                     * the destination. The allocation routine will know what 
                     * to do with it. *)
                    let bdestlv = boxlval destlv in
                    bdestlv.lvt
                  else
                    (* If it is not allocation we make the temporary have the 
                     * same type as the function return type. This is to 
                     * prevent the situation when we would need a cast 
                     * between non-scalar types *)
                    ftret) in
              (* Now do the call itself *)
              let thecall = 
                match isallocate with
                  None -> interceptCall (Some (var tmp)) f' args'
                | Some ai -> pkAllocate ai (var tmp) f' args'
              in
              (* Now use boxinstr to do the code after Call properly *)
              let aftercall = boxinstr (Set(destlv, Lval (var tmp), l)) in
              (* Now put them together *)
              append thecall aftercall
          end
        in
        append dof (append doargs finishcall)

    | Asm(tmpls, isvol, outputs, inputs, clobs, l) ->
        currentLoc := l;
        let rec doOutputs = function
            [] -> empty, []
          | (c, lv) :: rest -> 
              let blv = boxlval lv in
              let check = 
                match blv.lv with
                  Mem _, _ -> 
                    checkMem (ToWrite zero) blv
                | _ -> empty
              in
              if isFatType blv.lvt then
                ignore (E.log "Warning: fat output in %a\n"
                          d_instr ins);
              let (doouts, outs) = doOutputs rest in
              (append blv.lvstmts (append check doouts), (c, blv.lv) :: outs)
        in
        let (doouts, outputs') = doOutputs outputs in
        let rec doInputs = function
            [] -> empty, []
          | (c, ei) :: rest -> 
              let (et, doe, e') = boxexp ei in
              if isFatType et then
                ignore (E.log "Warning: fat input %a in %a\n"
                          d_exp ei d_instr ins);
              let (doins, ins) = doInputs rest in
              (append doe doins, (c, e') :: ins)
        in
        let (doins, inputs') = doInputs inputs in
        append doouts (CConsR(doins,
                              mkAsm tmpls isvol outputs inputs clobs))
            
  with e -> begin
    ignore (E.log "boxinstr (%s):%a (in %s)\n" 
              (Printexc.to_string e) d_instr ins !currentFunction.svar.vname);
    single (mkStmtOneInstr (dInstr (dprintf "booo_instruction(%a) at %t" 
                                      d_instr ins d_thisloc) !currentLoc))
  end


(*** Intercept some function calls *****)
and interceptCall 
    (reso: lval option)
    (func: exp)
    (args: exp list) : stmt clist = 
(*  try
    match func with
      Lval(Var fv, NoOffset) -> 
        ignore (E.log "intercepted call to %s\n" fv.vname);
        if matchPolyName "ccured_kind_of" fv.vname then begin
          match reso with
            None -> empty
          | Some dest -> begin
              let kndstr = 
                match args with 
                  [ a ] -> 
                    sprint 80 (N.d_opointerkind () (kindOfType (typeOf a)))
                | _ -> begin
                    ignore (warn "Invalid call to %s\n" fv.vname);
                    "scalar"
                end
              in
              let (_, dostr, knd) = boxexp (CastE(typeOfLval dest, 
                                                  Const(CStr(kndstr)))) in
              CConsR (dostr, 
                      mkStmtOneInstr
                        (Set(dest, knd, !currentLoc)))
          end 
        end else
          raise Not_found

    | _ -> raise Not_found
  with Not_found -> *)
    single (call reso func args)
                 


(* Given an lvalue, generate all the stuff needed to construct a pointer to 
 * it *)
and boxlval (b, off) : curelval = 
  (* Maybe we have heapified this one *)
  match b, off with
    Var vi, off -> begin
      try
        let newb, newoff = H.find heapifiedLocals vi.vname in
        boxlval (newb, addOffset off newoff)
      with Not_found -> boxlval1 (b, off)
    end
        (* Itercept the case (T* )0->f *)
  | Mem z, Field(f, NoOffset) when isZero z -> 
      { lv = (b, off); lvt = typeOfLval (b, off);
        lvb = zero; lve = zero; plvk = N.Wild; 
        lvstmts = empty }

  | _ -> boxlval1 (b, off)

and boxlval1 (b, off) : curelval =
  let debuglval = false in 
  (* As we go along the offset we keep track of the basetype and the pointer 
   * kind, along with the current base expression and a function that can be 
   * used to recreate the lval. *)
  if debuglval then
    ignore (E.log "Boxlval: %a\n" d_lval (b, off));
  let startinput : curelval = 
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
                t, append doaddr doaddr2, addrbase1, addrend1, newk
              else
               (* Make sure it is not a table type *)
                t, doaddr, addr'base, addr'end, newk
          | _ -> E.s (unimp "Mem but no pointer type: %a@!addr= %a@!"
                        d_plaintype addrt d_plainexp addr)
        in
        (* If the kind of the address is not WILD or INDEX we must do a null 
         * check. For WILD and INDEX the bounds check will also take care of 
         * the null *)
        let doaddr2 = 
          if nullCheckAfterBoundsCheck addrkind then 
            CConsR (doaddr1, checkNull addr')
          else doaddr1
        in
        { lv = mkMem addr' NoOffset; lvt = addrt1; plvk = addrkind; 
          lvb = addrbase1; lve = addrend1; lvstmts = doaddr2 }
  in
  (* As we go along we need to go into tagged and sized types. *)
  let goIntoTypes (inlv: curelval) : curelval =
    match unrollType inlv.lvt with
      TComp (comp, _) when comp.cstruct -> begin
        match comp.cfields with
          f1 :: f2 :: [] when (f1.fname = "_size" && f2.fname = "_array") -> 
            begin
            (* A sized array *)
              if inlv.plvk != N.Safe then
                E.s (bug "Sized array in a non-safe area");
              { lv = addOffsetLval  (Field(f2,NoOffset)) inlv.lv;
                lvt = f2.ftype; plvk = N.Safe;
                lvb = zero; lve = zero; lvstmts = inlv.lvstmts }
            end
        | f1 :: f2 :: _ when (f1.fname = "_len" && f2.fname = "_data") ->
            (* A tagged data. Only wild pointers inside *)
            if inlv.plvk = N.Wild then
              E.s (bug "Tagged data inside a tagged area");
            let lv' = addOffsetLval (Field(f2,NoOffset)) inlv.lv  in
            { lv = lv';
              lvt = f2.ftype; plvk = N.Wild;
              lvb = mkAddrOf lv'; lve = zero; lvstmts = inlv.lvstmts }

        | _ -> inlv
      end 

    | _ -> inlv
  in
  (* Now do the offsets *)
  let startinput = goIntoTypes startinput in
  if debuglval then
    ignore (E.log "goIntoTypes: %a\n" d_curelval startinput);
  let rec doOffset (inlv: curelval) (off: offset) : curelval = 
    match off with
      NoOffset -> inlv

    | Field (f, resto) -> 
        if debuglval then 
          ignore (E.log "doingField(%s): %a\n" f.fname d_curelval inlv);
        let bflv = beforeField inlv in
        let addf = Field (f, NoOffset) in
        (* Prepare for the rest of the offset *)
        let next = { bflv with lv = addOffsetLval addf bflv.lv; 
                               lvt = f.ftype; } in
        doOffset (goIntoTypes next) resto

    | Index (e, resto) -> 
        if debuglval then 
          ignore (E.log "doingIndex(%a): %a\n" d_exp e d_curelval inlv);
        let bilv = beforeIndex inlv in
        (* Do the index *)
        let (_, doe, e') = boxexp e in
        (* Prepare for the rest of the offset. Notice that beforeIndex gives 
         * us the first element of the array *)
        let rec loopOff = function
            Index(z, NoOffset) when isZero z -> Index(e', NoOffset)
          | Index(i, off) -> Index(i, loopOff off)
          | Field(f, off) -> Field(f, loopOff off)
          | _ -> E.s (bug "doingIndex: expected an Index(0) offset: %a"
                        d_plainlval bilv.lv)
        in
        let next = 
          { bilv with lv = (fst bilv.lv, loopOff (snd bilv.lv));
                      lvstmts = append bilv.lvstmts doe }
        in
        doOffset (goIntoTypes next) resto
  in
  let lvoffset = doOffset startinput off in
  if debuglval then
    ignore (E.log "Done lval: %a\n" d_curelval lvoffset);
  lvoffset
      
    (* Box an expression and return the fexp version of the result. If you do 
     * not care about an fexp, you can call the wrapper boxexp *)
and boxexpf (e: exp) : stmt clist * fexp = 
  try
    match e with
    | Lval (lv) -> 
        (* ignore (E.log "boxexpf: %a\n" d_plainlval lv); *)
        let blv (* lvt, lvkind, lv', baseaddr, len, dolv *) = boxlval lv in
        let check = (* Check a read if it is in memory or if it comes from a 
                     * variable that contains arrays or that is tagged 
                       *)
          match blv.lv with
            Mem _, _ -> 
              checkMem ToRead blv
          | Var vi, off when containsArray vi.vtype || mustBeTagged vi -> 
              checkMem ToRead blv
          | _, _ -> empty
        in
        (append blv.lvstmts check, mkFexp1 blv.lvt (Lval(blv.lv)))
            
    | Const (CInt64 (_, ik, _)) -> (empty, L(TInt(ik, []), N.Scalar, e))
    | Const ((CChr _)) -> (empty, L(charType, N.Scalar, e))
    | Const (CReal (_, fk, _)) -> (empty, L(TFloat(fk, []), N.Scalar, e))

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
            (append doe1 (append doe2 doarith), res)
        | (MinusPP|EqP|NeP|LeP|LtP|GeP|GtP), _, _ -> 
            (append doe1 doe2, 
             L(restyp', N.Scalar,
               BinOp(bop, 
                     readPtrField e1' et1, 
                     readPtrField e2' et2, restyp')))
              
        | _, N.Scalar, N.Scalar -> 
            (append doe1 doe2, 
             L(restyp', N.Scalar, BinOp(bop,e1',e2',restyp')))
              
        | _, _, _ -> E.s (unimp "boxBinOp: %a@!et1=%a@!et2=%a@!" 
                            d_binop bop d_plaintype et1 d_plaintype et2)
    end
          
    | SizeOf (t) -> 
        let containsExposedPointers t = 
          existsType 
            (function 
                TPtr _ -> ExistsTrue
                    (* Pointers inside named structures are not exposed *)
              | TComp (comp, _) 
                  when (String.length comp.cname > 1 &&
                        String.get comp.cname 0 <> '@') -> ExistsFalse
                    (* Pointers behind names are not exposed *)
              | TNamed (n, _, _) -> ExistsFalse
              | _ -> ExistsMaybe) t 
        in
        if containsExposedPointers t then 
          ignore (warn "Boxing sizeof(%a) when type contains pointers. Use sizeof expression." d_type t);
        let t' = fixupType t in
        (empty, L(uintType, N.Scalar, SizeOf(t')))

          
    | SizeOfE (Lval lv) -> 
        (* Intercept the case when we do sizeof an lvalue. This way we can 
         * avoid trying to check the safety of reads that might be triggered 
         * if we view the lvalue as an expression  *)
        (* ignore (E.log "boxexpf: %a\n" d_plainlval lv); *)
        let blv (* lvt, lvkind, lv', baseaddr, len, dolv *) = boxlval lv in
        (* DRop the side effects *)
        (empty, L(uintType, N.Scalar, SizeOfE(Lval blv.lv)))

    | SizeOfE (e) -> begin
        let (et, doe, e') = boxexp e in
        (* Drop all size-effects from this SizeOf *)
        (empty, L(uintType, N.Scalar, SizeOfE(e')))
    end

    | AlignOf (t) -> 
        let containsExposedPointers t = 
          existsType 
            (function 
                TPtr _ -> ExistsTrue
                    (* Pointers inside named structures are not exposed *)
              | TComp (comp, _) 
                  when (String.length comp.cname > 1 &&
                        String.get comp.cname 0 <> '@') -> ExistsFalse
              | _ -> ExistsMaybe) t 
        in
        if containsExposedPointers t then 
          ignore (warn "Boxing __alignof__(%a) when type contains pointers. Use __alignof__ expression." d_type t);
        let t' = fixupType t in
        (empty, L(uintType, N.Scalar, AlignOf(t')))

          
    | AlignOfE (Lval lv) -> 
        (* Intercept the case when we do sizeof an lvalue. This way we can 
         * avoid trying to check the safety of reads that might be triggered 
         * if we view the lvalue as an expression  *)
        (* ignore (E.log "boxexpf: %a\n" d_plainlval lv); *)
        let blv (* lvt, lvkind, lv', baseaddr, len, dolv *) = boxlval lv in
        (* Drop the side effect *)
        (empty, L(uintType, N.Scalar, AlignOfE(Lval blv.lv)))

    | AlignOfE (e) -> begin
        let (et, doe, e') = boxexp e in
        (* Drop all size-effects from this SizeOf *)
        (empty, L(uintType, N.Scalar, AlignOfE(e')))
    end

    | AddrOf (lv) ->
        let blv = boxlval lv in
        (* Check that variables whose address is taken are flagged as such, 
         * or are globals  *)
        (match blv.lv with
          (Var vi, _) when not vi.vaddrof && not vi.vglob -> 
            E.s (bug "addrof not set for %s (addrof)" vi.vname)
        | _ -> ());
        let res, doaddrof = pkAddrOf blv in
(*        ignore (E.log "%a -> %a\n" d_exp e d_fexp res); *)
        (doaddrof, res)
          
          (* StartOf is like an AddrOf except for typing issues. *)
    | StartOf lv -> begin
        let blv (* (lvt, lvkind, lv', baseaddr, bend, dolv) *) = boxlval lv in
        (* Check that variables whose address is taken are flagged *)
        (match blv.lv with
          (Var vi, _) when not vi.vaddrof && not vi.vglob -> 
            E.s (bug "addrof not set for %s (startof)" vi.vname)
        | _ -> ());
        let res, dostartof = pkStartOf blv in
        (*        ignore (E.log "result of StartOf: %a@!" d_fexp res); *)
        (dostartof, res)
    end
(*
    | Question (e1, e2, e3) ->       
        let (_, doe1, e1') = boxexp (CastE(intType, e1)) in
        let (et2, doe2, e2') = boxexp e2 in
        let (et3, doe3, e3') = boxexp e3 in
        let result = mkFexp1 et2 (Question (e1', e2', e3')) in
        (append doe1 (append doe2 doe3), result)
*)
  with exc -> begin
    ignore (E.log "boxexpf (%s): %a in %s\n" 
              (Printexc.to_string exc) d_exp e !currentFunction.svar.vname);
    (empty, L(charPtrType, N.String, dExp (dprintf "booo_exp: %a" d_exp e)))
  end 
            
      
and boxinit (ei: init) : init =
  try
    match ei with
      SingleInit e ->
        let e't, doe, e', e'base, e'len = boxexpSplit e in
        if doe <> empty then
          E.s (unimp "Non-pure initializer %a\n"  d_exp e);
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


and fexp2exp (fe: fexp) (doe: stmt clist) : expRes = 
  match fe with
    L (t, pk, e') -> (t, doe, e')       (* Done *)
  | FM (wt, _, ep, eb, el) -> 
      let (doset, lv) = setFatPointer wt (fun _ -> ep) eb el in
      (wt, append doe doset, Lval(lv))

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


(* sm: make a pass over the file and change declared types of arrays *)
(* of chars to be 1 byte longer *)
class expandCharArrays = object (self)
  inherit nopCilVisitor

  (* given a type, if it's a char array, return a similar array type *)
  (* that is 1 byte bigger; 'fname/loc' is passed for debugging help *)
  method replaceType (fname:string) (loc:location) (t:typ) : typ =
    match t with
    | TArray(baseType, Some length, attrs) when (isCharType(baseType)) -> (
        (trace "expand" (dprintf "expanding array variable/field %s at %a\n" 
                                 fname d_loc loc));
        TArray(baseType, Some(BinOp(PlusA, length, one, intType)), attrs)
      )
    | _ -> t    (* leave unchanged *)

  method vvdec (v:varinfo) = (
    v.vtype <- (self#replaceType v.vname v.vdecl v.vtype);
    DoChildren
  )
  method vexpr (e:exp) = (
    match e with
    | Const(CStr s) when false ->
        (* tack one more null byte onto the end *)
        ChangeTo(Const(CStr (s ^ "\000")))
    | SizeOfE(innerExp) -> (
        match (typeOf innerExp) with
        | TArray(baseType, len, attrs) when (isCharType baseType) ->
            (* all arrays of chars get expanded by 1 char, but I want *)
            (* the program to still see the unexpanded size, so make this *)
            (* yield a value one less *)
            (trace "expand" (dprintf "subtracting 1 from %a\n" d_exp e));
            ChangeTo(BinOp(MinusA, e, one, intType))
        | _ ->
            (* leave other sizeofs alone *)
            DoChildren
      )
    | _ -> DoChildren
  )
  method vglob (g:global) : global list visitAction = (
    match g with
    | GCompTag(cinfo, loc) -> (
        let vfield (f:fieldinfo) : unit =
          f.ftype <- (self#replaceType f.fname loc f.ftype)
        in
        (List.iter vfield cinfo.cfields);
        DoChildren
      )
    | _ -> DoChildren
  )
end

let doExpandCharArrays (f: file) : unit = (
  (tracei "sm" (dprintf "array-expand post-process..\n"));
  (ignore (visitCilFile (new expandCharArrays :> cilVisitor) f));
  (traceu "sm" (dprintf "array-expand post-process finished\n"))
)  

(* a hashtable of functions that we have already made wrappers for *)
let wrappedFunctions = H.create 15

exception DeepExit
let definedFunctions : (string, string) H.t = H.create 111

let boxFile file =
  if !E.verboseFlag then
    ignore (E.log "Boxing file\n");
  E.hadErrors := false;
  H.clear definedFunctions;
  currentFile := file;
  mangledMainName := ""; (* We have not yet seen main *)
  let boxing = ref true in
  (* Compute a small file ID *)
  let _ = 
    let h = H.hash file.fileName in
    let h16 = (h lxor (h lsr 16)) land 0xFFFF in
    currentFileId := h16;
    if !E.verboseFlag then 
      ignore (E.log "File %s has id 0x%04x\n" file.fileName h16)
  in
  let rec doGlobal g = 
    try match g with
      (* We ought to look at pragmas to see if they talk about alignment of 
       * structure fields  *)
      GPragma (a, _) -> begin
        (match a with
          Attr("interceptCasts", [ AId("on") ]) -> interceptCasts := true
        | Attr("interceptCasts", [ AId("off") ]) -> interceptCasts := false
        | Attr("boxalloc",  AStr(s) :: rest) -> 
            if not (H.mem allocFunctions s) then begin
              if !E.verboseFlag then
                ignore (E.log "Will treat %s as an allocation function\n" s);
              boxallocPragma s rest
            end
        | Attr("box", [AId("on")]) -> boxing := true
        | Attr("box", [AId("off")]) -> boxing := false
        | Attr("boxtext", [AStr s]) ->
            theFile := consGlobal (GText s) !theFile
        | _ -> ());
        theFile := consGlobal g !theFile
      end
    | _ -> begin
        if not !boxing then theFile := consGlobal g !theFile else
        match g with

        | GDecl (vi, l) -> 
            boxglobal vi false None l;
            (* Comment out all the polymorphic versions from the file  *)
            if prefix "/*" vi.vname then begin
              match !theFile with
                ((GDecl _) as g) :: rest -> 
                  theFile := g :: rest
              | _ -> 
                  E.s (E.bug "Cannot find declaration of polymorphic func.")
            end

              
              
        | GVar (vi, init, l) -> boxglobal vi true init l
        | GType (n, t, l) -> 
            currentLoc := l;
            if showGlobals then ignore (E.log "Boxing GType(%s) at %a\n" 
                                          n d_loc l);
(*            ignore (E.log "before GType(%s -> %a)@!"
                      n d_plaintype t); *)
            let tnew = fixupType t in
(*
            ignore (E.log "after GType(%s -> %a)@!"
                      n d_plaintype tnew);
*)
            theFile := consGlobal (GType (n, tnew, l)) !theFile
                       
        | GCompTag (comp, l) ->
            if showGlobals then ignore (E.log "Boxing GCompTag(%s) at %a\n"
                                          (compFullName comp) d_loc l);
            currentLoc := l;
            (* Change the fields in place, so that everybody sees the change *)
            List.iter 
              (fun fi -> 
                let newa, newt = moveAttrsFromDataToType fi.fattr fi.ftype in
                fi.fattr <- newa ; 
                fi.ftype <- fixupType newt) 
              comp.cfields;
            theFile := consGlobal g !theFile

        | GFun (f, l) when hasAttribute "nobox" f.svar.vattr -> 
            f.sbody <- visitCilBlock unsafeVisitor f.sbody;
            theFile := consGlobal g !theFile
            
        | GFun (f, l) -> 
            currentLoc := l;
            if showGlobals then ignore (E.log "Boxing GFun(%s) at %a\n" 
                                          f.svar.vname d_loc l);
            (* Drop functions that are just modeledbodies *)
            if hasAttribute "modeledbody" f.svar.vattr then begin
              theFile := consGlobal 
                   (GText ("// Dummy body of " ^ f.svar.vname ^ " was here"))
                   !theFile;
              raise DeepExit
            end;
            (* See if is a vararg function *)
            let isva = 
              match f.svar.vtype with
              TFun (_, _, isva, _) -> isva
              | _ -> false
            in
            (* Run the oneret first so that we have always a single return 
             * where to place the finalizers  *)
            Oneret.oneret f;
            hasRegisteredAreas := false;
            (* Fixup the return type as well, except if it is a vararg *)
            f.svar.vtype <- fixupType f.svar.vtype;
            (* Maybe we need to box some locals *)
            (match N.nodeOfAttrlist f.svar.vattr with
            | Some n when n.N.kind = N.Wild ->
                f.svar.vtype <- 
                   fixupFunctionType f.svar.vid N.Wild f.svar.vtype
            | _ -> ());
            (* If the type has changed and this is a global function then we 
             * also change its name  *)
            fixupGlobName f.svar;
            (* This might be a polymorphic instance function. See if we have 
             * another one with the same mangling *)
            (let stripname = stripPolyName f.svar.vname in
             try
              let already = H.find definedFunctions stripname in
              theFile := 
                 consGlobal 
                   (GText (sprint 80 (dprintf "// %s coalesced with %s"
                                        f.svar.vname already))) 
                   !theFile;
              raise DeepExit
             with Not_found -> 
               H.add definedFunctions stripname f.svar.vname);

(*
            ignore (E.log "The boxedFunctions:\n");
            H.iter (fun (fid, aid) t -> 
              ignore (E.log " f%d.%d -> %a\n" fid aid d_type t)) 
              boxedArguments;
*)

            (* Check that we do not take the address of a formal. If we 
             * actually do then we must make that formal a true local and 
             * create another formal  *)

            let newformals, (newbody : stmt clist) =
              let islastva = ref isva in (* To detect last argument in va 
                                          * functions *)
              let rec loopFormals = function
                  [] -> [], fromList f.sbody.bstmts
                | form :: restf ->
                    let r1, r2 = loopFormals restf in
                    let islastva = !islastva && (islastva := false; true) in
                    if form.vaddrof && (not islastva) then begin
                      let tmp = makeTempVar f form.vtype in
                      (* Now take it out of the locals and replace it with 
                       * the current formal. It is not worth optimizing this 
                       * one  *)
                      f.slocals <-
                         form ::
                         (List.filter (fun x -> x.vid <> tmp.vid) f.slocals);
                    (* Now replace form with the temporary in the formals *)
                      tmp :: r1, CConsL(mkSet (var form) (Lval(var tmp)), r2)
                    end else
                      try
                        let origt = 
                          H.find boxedArguments (f.svar.vid, form.vid) in
                        (* Make a replacement in the formals *)
                        let tmp = makeTempVar f form.vtype in
                        (* Restore the type of the formal *)
                        form.vtype <- origt;
                        (* Add it to the locals *)
                        f.slocals <-
                           form ::
                           (List.filter (fun x -> x.vid <> tmp.vid) f.slocals);
                        let ptmp = readPtrField (Lval (var tmp)) form.vtype in
                        tmp :: r1, CConsL(mkSet (var form) ptmp, r2)
                      with Not_found ->
                        form :: r1, r2
              in
              loopFormals f.sformals
            in
            setFormals f newformals;
            (* We fix the formals *)
            List.iter (fun l -> 
              l.vattr <- N.replacePtrNodeAttrList N.AtVar l.vattr;
              l.vtype <- fixupType l.vtype) f.sformals;

            (* Now go and collect a list of fieldinfo for all variables with 
             * the heapify attribute set *)
            let heapifiedFields_safe, heapifiedFields_tagged = 
              List.fold_right
                (fun v (acc_s, acc_t) -> 
                  if hasAttribute "heapify" v.vattr then begin
                    ignore (warn "Moving local %s to the heap." v.vname);
                    let newfield = (v.vname, fixupType v.vtype, 
                                    None, v.vattr) in
                    let istagged = 
                      match N.nodeOfAttrlist v.vattr with 
                        Some n -> n.N.kind = N.Wild
                      | _ -> false
                    in
                    if istagged then 
                      (acc_s,  newfield :: acc_t)
                    else
                      (newfield :: acc_s, acc_t)
                  end else 
                    (acc_s, acc_t))
                f.slocals
                ([], [])
            in
            let newbody = 
              let doHeapify (istagged: bool) 
                            (hFields: (string * typ * 
                                       int option * attribute list) list) 
                            (body: stmt clist) : stmt clist = 
                if hFields = [] then body 
                else begin
                  let kind = if istagged then "tagged" else "" in
                  (* Make a type for all of them *)
                  let tname  = newTypeName ("heapified" ^ kind) in
                  let hCompInfo = 
                    mkCompInfo true tname (fun _ -> hFields) [] in
                  (* Add it to the file *)
                  theFile := 
                     consGlobal (GCompTag (hCompInfo, !currentLoc)) !theFile;
                  (* Create a new local variable *)
                  let heapVar = 
                    makeLocalVar f 
                      ("__heapified" ^ kind) 
                      (TPtr(TComp (hCompInfo, []), 
                            if istagged then [N.k2attr N.Wild] else [])) in
                  (* Now insert the call to malloc. It will be processed 
                   * properly when the body is boxed later *)
                  let callmalloc = 
                    call (Some (var heapVar)) 
                      (Lval(var mallocFun.svar)) 
                      [ SizeOfE (Lval (mkMem (Lval (var heapVar)) NoOffset)) ] 
                  in
                  (* Now go over all the fields and register their names in a 
                  * hash table *)
                  List.iter (fun fi -> 
                    H.add heapifiedLocals 
                      fi.fname (mkMem (Lval (var heapVar))
                                  (Field(fi, NoOffset)))) 
                    hCompInfo.cfields;
                  (* Initialize the things to free *)
                  heapifiedFree :=
                     (* sm: a better solution is to say heapVar._p-4, but I 
                      * don't  *)
                     (* quite know how in cil (it's getting ahold of _p 
                      * that's hard)  *)
                     call None 
                       (Lval (var 
                                (if istagged then freeMinus4Fun.svar 
                                else freeFun.svar)))
                       [Lval (var heapVar)] 
                     :: !heapifiedFree;
                  CConsL (callmalloc, body)
                end
              in
              doHeapify true heapifiedFields_tagged 
                (doHeapify false heapifiedFields_safe newbody)
            in
            (* Remove the heapified locals *)
            f.slocals <- 
               List.filter 
                 (fun l -> not (H.mem heapifiedLocals l.vname)) 
                 f.slocals;
            (* Fixup the types of the remaining locals  *)
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
            currentFunction := f;           (* so that maxid and locals can 
                                             * be updated in place *)
            (* Clean up the iterator variables *)
            iterVars := [];

            f.sbody.bstmts <- toList newbody;

            (* Initialize and register the locals. Since we do this before 
             * boxing we will not initialize the temporaries created during 
             * boxing. But then we know that those are always defiend before 
             * use. We must initialize the locals before we do the body 
             * because the initialization produces the code for unregistering 
             * the locals, which we need when we encounter the Return  *)
            let inilocals = 
              List.fold_left (initializeVar withIterVar) empty f.slocals in

            (* sm/gn: for testing the removeTemps module: add some extra temps *)
            if (traceActive "gratuitousTemps") then (
              for i = 0 to 10 do
                (trace "gratuitousTemps" (dprintf "Making a temp\n"));
                ignore (makeTempVar f intType);
              done;
            );

            (* Do the body now *)
            f.sbody <- boxblock f.sbody;
            f.sbody.bstmts <- 
               toList (append inilocals (fromList f.sbody.bstmts));
            H.clear heapifiedLocals;
            heapifiedFree := [];
            (* Drop it if it is just a model *)
            if not (hasAttribute "boxmodel" f.svar.vattr) then 
              theFile := consGlobal (GFun (f, l)) !theFile
                                        
        | (GAsm _ | GText _ | GPragma _ | GEnumTag _ ) as g -> 
            theFile := consGlobal g !theFile 
    end 
    with DeepExit -> ()

  and boxglobal vi isdef init (l: location) =
    currentLoc := l; 
    if showGlobals then ignore (E.log "Boxing GVar(%s) at %a\n" 
                                  vi.vname d_loc l);
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
            (dropAttribute newa (Attr("__format__", [])));
      vi.vtype <- fixupType newt;
      (* Now see if we must change the type of the function *)
      (match N.nodeOfAttrlist vi.vattr with
      | Some n when n.N.kind = N.Wild ->
          vi.vtype <- fixupFunctionType vi.vid N.Wild vi.vtype
      | _ -> ());
      if mustBeTagged vi then begin
        vi.vtype <- tagType vi.vtype
      end;
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
             let oldCurrentFunction = !currentFunction in
             currentFunction := gi;
             let oldIterVars = !iterVars in
             iterVars := !globInitIterVars;
             let res = withIterVar x in
             iterVars := oldIterVars;
             currentFunction := oldCurrentFunction;
             res) !extraGlobInit vi;
    end;
    (* Tag some globals. We should probably move this code into the 
     * initializeVar but for now we keep it here *)
    if not (mustBeTagged vi) then
      if isdef then begin
        theFile := consGlobal (GVar(vi, init',l)) !theFile
      end else
        theFile := consGlobal (GDecl (vi, l)) !theFile
    else begin
      if not isdef && vi.vstorage <> Extern then
        theFile := consGlobal (GDecl (vi, l)) !theFile
      else begin
        (* Make the initializer *)
        (* Add it to the tag initializer *)
        let varinit = 
          if vi.vstorage = Extern then None 
          else
            let (x, _) = makeTagCompoundInit vi.vtype init' in
            Some x
        in
        theFile := consGlobal (GVar(vi, varinit,l)) !theFile
      end
    end
  in
  if showGlobals then ignore (E.log "Boxing file\n");
  let doGlobal x = 
    try doGlobal x with e -> begin
      ignore (E.log "boxglobal (%s)\n" (Printexc.to_string e));
      theFile := 
         consGlobal (GAsm (sprint 2 (dprintf "booo_global %a" d_global x), 
                           !currentLoc)) !theFile
    end 
  in
  extraGlobInit := empty;
  H.clear taggedTypes;
  (* Create the preamble *)
  preamble ();
  interceptCasts := false;
  (* Now the original file, including the global initializer *)
  iterGlobals file doGlobal;
  (* Now finish the globinit *)
  let newglobinit = 
    match file.globinit with
      None -> 
        if !extraGlobInit <> empty then
          let gi = getGlobInit file in
          gi.sbody.bstmts <- toList !extraGlobInit;
          Some gi
        else
          None
    | Some g -> begin
        match !theFile with
          GFun(gi, _) :: rest -> 
            theFile := rest; (* Take out the global initializer (last thing 
                              * added)  *)
            let res = 
              toList (append !extraGlobInit (fromList gi.sbody.bstmts)) in
            gi.sbody.bstmts <- if compactBlocks then compactStmts res else res;
            Some gi
        | _ -> E.s (bug "box: Cannot find global initializer")
    end
  in
  (* Now if we have mangled the "main" we must add a dummy main that goes 
   * into a library function *)
  if !mangledMainName <> "" then begin
    let mainWrapperFun = 
      match !mangledMainName with
      | "main" -> mainWrapper
      | "main_w" -> mainWrapper_w
      | "main_fs" -> mainWrapper_fs
      | "main_qw" -> mainWrapper_qw
      | "main_fw" -> mainWrapper_fw
      | "main_fq" -> mainWrapper_fq
      | "main_ff" -> mainWrapper_ff
      | _ -> E.s (E.unimp "Din't expect to mangle the name of main to %s"
                    !mangledMainName)
    in
    let main = emptyFunction "main" in
    let argc  = makeLocalVar main "argc" intType in
    let argv  = makeLocalVar main "argv" (TPtr(charPtrType, [])) in
    main.svar.vtype <- TFun(intType, Some [ argc; argv ], false, []);
    setFormals main [argc; argv];
    (* And remove them from the locals *)
    main.slocals <- [];
    let exitcode = makeLocalVar main "exitcode" intType in
    (* Now build a body that calls the wrapper *)
    main.sbody <-  
       mkBlock (mkStmtOneInstr 
         (Call (Some (var exitcode), Lval (var mainWrapperFun.svar),
                [ Lval (var argc); Lval (var argv) ], lu))
       ::
       mkStmt (Return (Some (Lval (var exitcode)), lu)) 
       ::
       []);
    theFile := consGlobal (GFun (main, lu)) !theFile
  end;
  (* Now add the function descriptor definitions *)
  theFile := !descriptorDefinitions @ !theFile;
  let res = List.rev (!theFile) in
  (* Clean up global hashes to avoid retaining garbage *)
  H.clear typeNames;
  H.clear fixedTypes;
  H.clear fixedComps;
  H.clear taggedTypes;
  H.clear sizedArrayTypes;
  H.clear boxedArguments;
  H.clear definedFunctions;
  extraGlobInit := empty;
  globInitIterVars := [];
  iterVars := [];
  theFile := [];
  let res = {file with globals = res; globinit = newglobinit} in
  Globinit.insertGlobInit res ;
  if showGlobals then ignore (E.log "Finished boxing file\n");
  let res' = Stats.time "split" Boxsplit.splitLocals res in
  (* sm: after everything else runs, make char arrays 1 byte longer *)
  (doExpandCharArrays res');
  res'



let customAttrPrint a =
  Ptrnode.ptrAttrCustom false a

