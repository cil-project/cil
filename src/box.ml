open Cil
open Pretty 

module H = Hashtbl
module E = Errormsg
module P = Ptrnode

let debugType = false
let debug = false

let checkReturn = true

let defaultIsWild  = ref false

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
  | FS of typ * P.pointerkind * exp     (* A multi-word expression that is 
                                         * named by a single expresion *)
  | FM of typ * P.pointerkind * exp * exp * exp 
                                         (* A multi-word expression that is 
                                          * made out of multiple single-word 
                                          * expressions: ptr, base and bend. 
                                          * bend might be "zero" if not 
                                          * needeed *)
  | FC of typ * P.pointerkind * typ * P.pointerkind * exp               
                                        (* A multi-word expression that is a 
                                         * cast of a FS to another fat type *)

let d_fexp () = function
    L(t, k, e) -> dprintf "L1(%a, %a:%a)" P.d_pointerkind k d_exp e d_type t
  | FS(_, k, e) -> dprintf "FS(%a, %a)" P.d_pointerkind k d_exp e
  | FM(_, k, ep, eb, ee) ->  
      dprintf "FM(%a, %a, %a, %a)" P.d_pointerkind k d_exp ep d_exp eb d_exp ee
  | FC(_, k, _, _, e) ->  dprintf "FC(%a, %a)" P.d_pointerkind k d_exp e
  
let leaveAlone = 
  ["printf"; "fprintf"; "sprintf"; "snprintf"; "sscanf"; "_snprintf";
   "_CrtDbgReport"; ]


            (* Same for offsets *)
type offsetRes = 
    typ * stmt list * offset * exp * P.pointerkind


(*** Helpers *)            
let castVoidStar e = doCast e (typeOf e) voidPtrType

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
      let su = if comp.cstruct then "s_" else "u_" in
      if String.sub comp.cname 0 1 = "@" then su
      else su ^ comp.cname
  | TFun _ -> "fun"
  | TPtr(t, _) -> "p_" ^ baseTypeName t
  | TArray(t, _, _) -> "a_" ^ baseTypeName t
  | _ -> "type"


(**** Inspect the boxing style attribute *)
let extractPointerTypeAttribute al = 
  let k, why = P.kindOfAttrlist al in
  k
          

let kindOfType t = 
  (* Since t was fixed up, it has a qualifier if it is a pointer *)
  match extractPointerTypeAttribute (typeAttrs t) with
    P.Unknown -> P.Scalar
  | res -> res


let extractArrayTypeAttribute al = 
  filterAttributes "sized" al <> []

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

(* Get rid of all Const attributes *)
let dropConst t =
 let dropit a = dropAttribute a (AId("const")) in
 let rec loop = function
    TVoid a -> TVoid (dropit a)
  | TInt (i, a) -> TInt (i, dropit a)
  | TFloat (f, a) -> TFloat (f, dropit a)
  | TBitfield (i, s, a) -> TBitfield (i, s, dropit a)
  | TNamed (n, t, a) -> TNamed(n, loop t, dropit a)
  | TPtr (t', a) -> TPtr(loop t', dropit a)
  | TArray (t', l, a) -> TArray(loop t', l, dropit a)
  | TComp comp as t -> t
  | TForward (comp, a) -> TForward (comp, dropit a)
  | TEnum (n, f, a) -> TEnum (n, f, dropit a)
  | TFun (r, args, v, a) -> 
      List.iter (fun a -> a.vtype <- loop a.vtype) args;
      TFun(loop r, args, v, dropit a)
 in
 loop t



let rec fixupType t = 
  match t with
    TForward _ -> t
    (* Keep the Named types *)
  | TNamed (n, t, a) -> TNamed(n, fixupType t, dropAttribute a (AId("const")))

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
  | TPtr(t', a) -> begin
      (* Add the default pointer qualifier if none present *)
      match extractPointerTypeAttribute a with
        P.Unknown -> 
          let kres = if !defaultIsWild then P.Wild else P.Safe in
          fixit (TPtr(t', addAttribute (P.k2attr kres) a))
      | k -> fixit t
  end
  | _ -> fixit t

and fixit t = 
  (* First drop the Const attribute *)
  let tdrop = dropConst t in
  let ts = typeSig tdrop in
  try
    H.find fixedTypes ts 
  with Not_found -> begin
    let fixed = 
      match tdrop with 
        (TInt _|TEnum _|TFloat _|TVoid _|TBitfield _) -> tdrop

      | TPtr (t', a) -> begin
          (* Now do the base type *)
          let fixed' = fixupType t' in
          (* Extract the boxing style attribute *)
          let pkind = kindOfType tdrop in 
          let fixed = 
            match pkind with
               P.Safe -> TPtr(fixed', a)
            | (P.Wild|P.Index|P.FSeq) -> 
                let tname  = newTypeName "fatp_" fixed' in (* The name *)
                let fixed = 
                  TComp 
                    (mkCompInfo true tname 
                       (fun _ -> [ ("_p", TPtr(fixed', a),
                                    []); 
                                   ("_b", voidPtrType, [])]) 
                       [])
                in
                theFile := GType(tname, fixed) :: !theFile;
                let tres = TNamed(tname, fixed, [P.k2attr pkind]) in
                H.add fixedTypes (typeSig fixed) tres;
                tres

            | P.Seq -> 
                let tname  = newTypeName "seq_" fixed' in (* The name *)
                let fixed = 
                  TComp 
                    (mkCompInfo true tname 
                       (fun _ -> [ ("_p", TPtr(fixed', a), []); 
                                   ("_b", voidPtrType, []);
                                   ("_e", voidPtrType, []) ]) 
                       [])
                in
                theFile := GType(tname, fixed) :: !theFile;
                let tres = TNamed(tname, fixed, [AId("seq")]) in
                H.add fixedTypes (typeSig fixed) tres;
                tres

            | _ -> E.s (E.unimp "Don't know how to fix a %a" 
                          P.d_pointerkind pkind)
          in
          (* We add fixed ourselves. The TNamed will be added after doit  *)
          H.add fixedTypes (typeSig fixed) fixed; 
          H.add fixedTypes (typeSig (TPtr(fixed', a))) fixed;
          fixed
      end
            
      | TForward _ ->  tdrop          (* Don't follow TForward, since these 
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
          let sized = extractArrayTypeAttribute a in
          let newarray = TArray(fixupType t', l, a) in
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
      if isCompleteType t then typeAddAttributes [AId("sized")] t 
      else begin
        match unrollType t with
	  TArray(bt, None, a) -> TArray(bt, Some zero, 
                                        addAttribute (AId("sized")) a)
        | TComp ci when ci.cfields = [] -> TArray(charType, Some zero, 
                                                  [AId("sized")])
        | _ -> 
            E.s (E.unimp "Don't know how to tag incomplete type %a" 
                   d_plaintype t)
      end
    in
    let arrayAttr = 
      if !msvcMode then [] else [AId("packed")]
    in
    let newtype = 
      TComp 
        (mkCompInfo true ""
           (fun _ -> 
             [ ("_size", uintType, arrayAttr);
               ("_array", complt, arrayAttr); ]) [])
    in
    let tname = newTypeName "_sized_" t in
    let named = TNamed (tname, newtype, [AId("sized")]) in
    theFile := GType (tname, newtype) :: !theFile;
    H.add sizedArrayTypes tsig named;
    H.add sizedArrayTypes (typeSig named) named;
    H.add sizedArrayTypes (typeSig newtype) named;
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
        let tagAttr = if !msvcMode then [] else [AId("packed")]
        in
        TComp 
          (mkCompInfo true ""
             (fun _ -> 
               [ ("_len", uintType, tagAttr);
                 ("_data", t, tagAttr);
                 ("_tags", TArray(intType, 
                                  Some tagWords, []), tagAttr);
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
    



(**** Make a pointer type of a certain kind *)
let mkPointerTypeKind (bt: typ) (k: P.pointerkind) = 
  fixupType (TPtr(bt, [P.k2attr k]))



let mkFexp1 (t: typ) (e: exp) = 
  let k = kindOfType t in
  match k with
    (P.Safe|P.Scalar) -> L  (t, k, e)
  | (P.Index|P.Wild|P.FSeq|P.Seq) -> FS (t, k, e)
  | _ -> E.s (E.bug "mkFexp1")

let mkFexp2 (t: typ) (ep: exp) (eb: exp) = 
  let k = kindOfType t in
  match k with
    (P.Safe|P.Scalar) -> L  (t, k, ep)
  | (P.Index|P.Wild|P.FSeq) -> FM (t, k, ep, eb, zero)
  | _ -> E.s (E.bug "mkFexp2")

let mkFexp3 (t: typ) (ep: exp) (eb: exp) (ee: exp) = 
  let k = kindOfType t in
  match k with
    P.Seq -> FM (t, k, ep, eb, ee)
  | _ -> E.s (E.bug "mkFexp3")

(**** We know in what function we are ****)
let currentFunction : fundec ref  = ref dummyFunDec
let currentFile     : file ref = ref dummyFile
let currentFileId     = ref 0

let isFatComp (comp: compinfo) = 
  (comp.cstruct && 
   (match comp.cfields with 
     p :: b :: rest when p.fname = "_p" && b.fname = "_b" -> 
       (match rest with
         [] -> true (* A two word pointer *)
       | [e] when e.fname = "_e" -> true
       | _ -> false)
   | _ -> false))


    (* Test if a type is FAT *)
let isFatType t = 
  match unrollType t with
    TComp comp when isFatComp comp -> true 
  | _ -> false

(* Given a fat type, return the three fieldinfo corresponding to the ptr, 
 * base and (optional) end *)
let getFieldsOfFat (t: typ) : fieldinfo * fieldinfo * (fieldinfo option) = 
  match unrollType t with
    TComp comp when isFatComp comp -> begin
      match comp.cfields with 
        p :: b :: e :: _ -> p, b, Some e
      | p :: b :: [] -> p, b, None
      | _ -> E.s (E.bug "getFieldsOfFat")
    end
  | _ -> E.s (E.bug "getFieldsOfFat %a\n" d_type t)
  
(* Given a sized array type, return the size and the array field *)
let getFieldsOfSized (t: typ) : fieldinfo * fieldinfo = 
  match unrollType t with
    TComp comp when comp.cstruct -> begin
      match comp.cfields with 
        s :: a :: [] when s.fname = "_size" && a.fname = "_array" -> s, a
      | _ -> E.s (E.bug "getFieldsOfSized")
    end
  | _ -> E.s (E.bug "getFieldsOfSized %a\n" d_type t)
  

let rec readFieldsOfFat (e: exp) et =     
  if isFatType et then
    let fptr, fbase, fendo = getFieldsOfFat et in
    let rec compOffsets = function
        NoOffset -> 
          Field(fptr, NoOffset), Field(fbase, NoOffset), 
          (match fendo with Some x -> Field(x, NoOffset) | _ -> NoOffset)

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
      | Question (e1, e2, e3, lq) ->
          let e2t, e2', e2'', e2e = readFieldsOfFat e2 et in
          let   _, e3', e3'', e3e = readFieldsOfFat e3 et in
          (Question(e1,e2',e3', lq), 
           Question(e1,e2'',e3'', lq), 
           Question(e1, e2e,e3e, lq))
      | Compound (t, [_, p; _, b]) when isFatType t -> 
          p, b, zero
      | Compound (t, [_, p; _, b; _, e]) when isFatType t -> 
          p, b, e
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
  let fptr, fbase, fendo = getFieldsOfFat t in
  let p' = p fptr.ftype in
  let setend = 
    match fendo with
      None -> []
    | Some fend -> [mkSet (Var tmp, Field(fend,NoOffset)) (castVoidStar e)]
  in
  ( mkSet (Var tmp, Field(fptr,NoOffset)) p' ::
   mkSet (Var tmp, Field(fbase,NoOffset)) (castVoidStar b)  :: setend, 
   (Var tmp, NoOffset))
      
let readPtrField (e: exp) (t: typ) : exp = 
  let (tptr, ptr, base, bend) = readFieldsOfFat e t in ptr
      
let readBaseField (e: exp) (t: typ) : exp = 
  let (tptr, ptr, base, bend) = readFieldsOfFat e t in base



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
  if !defaultIsWild then
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


let checkNullFun =   
  let fdec = emptyFunction "CHECK_NULL" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp ], false, []);
  fdec.svar.vstorage <- Static;
  fdec

let checkSafeRetFatFun = 
  let fdec = emptyFunction "CHECK_SAFERETFAT" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  fdec
    
let checkPositiveFun = 
  let fdec = emptyFunction "CHECK_POSITIVE" in
  let argx  = makeLocalVar fdec "x" intType in
  fdec.svar.vtype <- TFun(voidType, [ argx; ], false, []);
  fdec.svar.vstorage <- Static;
  fdec
  
    
    
let checkSafeFatLeanCastFun = 
  let fdec = emptyFunction "CHECK_SAFEFATLEANCAST" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  fdec

let checkFunctionPointer = 
  let fdec = emptyFunction "CHECK_FUNCTIONPOINTER" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  fun whatp whatb whatkind -> 
    if whatkind = P.Safe then
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
        let pfield, _, _ = getFieldsOfFat (TComp(fip.fcomp)) in
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
  theFile := GDecl fdec.svar :: !theFile;
  fun tmplen base -> 
    let ptr = ptrOfBase base in
    call (Some tmplen) (Lval (var fdec.svar))
      [ castVoidStar ptr; 
        castVoidStar base ]

let checkFetchEnd = 
  let fdec = emptyFunction "CHECK_FETCHEND" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  fdec.svar.vtype <- TFun(voidPtrType, [ argp; argb ], false, []);
  fdec.svar.vstorage <- Static;
  fun tmplen base -> 
    let ptr = ptrOfBase base in
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

let checkLBoundFun = 
  let fdec = emptyFunction "CHECK_LBOUND" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argp; ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  fdec

let checkUBoundFun = 
  let fdec = emptyFunction "CHECK_UBOUND" in
  let argbend  = makeLocalVar fdec "bend" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argbend; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  fdec

let checkBoundsFun = 
  let fdec = emptyFunction "CHECK_BOUNDS" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argbend  = makeLocalVar fdec "bend" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argbend; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  fdec

let checkBoundsLenFun = 
  let fdec = emptyFunction "CHECK_BOUNDS_LEN" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argbl  = makeLocalVar fdec "bl" uintType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  let argpl  = makeLocalVar fdec "pl" uintType in
  fdec.svar.vtype <- TFun(voidType, [ argb; argbl; argp; argpl ], false, []);
  fdec.svar.vstorage <- Static;
  theFile := GDecl fdec.svar :: !theFile;
  fdec



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
      castVoidStar p; SizeOf (baset, lu)] :: acc
    
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
      castVoidStar p; SizeOf (baset, lu)] :: acc
  

let indexToSafe (p: exp) (desttyp: typ) (b: exp) (bend: exp) (acc: stmt list) 
    : exp * exp * exp * stmt list =
  let p', b', bend', acc' = indexToSeq p b bend acc in
  seqToSafe p' desttyp b' bend' acc'
    
  

let checkWild (p: exp) (basetyp: typ) (b: exp) (blen: exp) : stmt = 
  (* This is almost like indexToSafe, except that we have the length already 
   * fetched *)
  call None (Lval (var checkBoundsLenFun.svar))
    [ castVoidStar b; blen;
      castVoidStar p; SizeOf (basetyp, lu)]
      
    

let checkBounds : (unit -> exp) -> exp -> exp -> lval -> typ 
                  -> P.pointerkind -> stmt list = 

  (* And now the null check *)
  fun mktmplen base bend lv lvt pkind ->
    let lv', lv't = getHostIfBitfield lv lvt in
    (* Do not check the bounds when we access variables without array 
     * indexing  *)
    match pkind with
    | P.Wild -> (* We'll need to read the length anyway since we need it for 
                 * working with the tags *)
        let docheck = 
          checkWild (AddrOf(lv', lu)) lv't base (mktmplen ()) in
        [docheck]

    | P.Index -> 
        let _, _, _, docheck = 
          indexToSafe (AddrOf(lv', lu)) (TPtr(lv't, [])) base bend [] in
        List.rev docheck
    | P.FSeq ->
        let _, _, _, docheck = 
          fseqToSafe (AddrOf(lv', lu)) (TPtr(lv't, [])) base bend [] in
        List.rev docheck
    | P.Seq ->
        let _, _, _, docheck = 
          seqToSafe (AddrOf(lv', lu)) (TPtr(lv't, [])) base bend [] in
        List.rev docheck
          
    | P.Safe -> begin
        match lv' with
          Mem addr, _ -> 
            [call None (Lval (var checkNullFun.svar)) [ castVoidStar addr ]]
        | _, _ -> []
    end
    | _ -> E.s (E.bug "Unexpected pointer kind in checkBounds")


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
    | TArray (bt, _, _) -> 
        theOffset (addOffset (Index(zero, NoOffset)) sofar) bt
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
  let (_, ptr, base, _) = readFieldsOfFat arg argt in 
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
  
  fun base where whatbase whatp len -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar base; len; 
        castVoidStar where; 
        castVoidStar whatbase; castVoidStar whatp;]
  
let checkFatStackPointer = 
  let fdec = emptyFunction "CHECK_FATSTACKPOINTER" in
  let argb  = makeLocalVar fdec "b" voidPtrType in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- 
     TFun(voidType, [ argp; argb; ], false, []);
  theFile := GDecl fdec.svar :: !theFile;
  fdec.svar.vstorage <- Static;
  
  fun whatp whatbase -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar whatbase; castVoidStar whatp;]
  

let checkLeanStackPointer = 
  let fdec = emptyFunction "CHECK_LEANSTACKPOINTER" in
  let argp  = makeLocalVar fdec "p" voidPtrType in
  fdec.svar.vtype <- 
     TFun(voidType, [ argp; ], false, []);
  theFile := GDecl fdec.svar :: !theFile;
  fdec.svar.vstorage <- Static;
  
  fun whatp -> 
    call None (Lval(var fdec.svar))
      [ castVoidStar whatp;]
  
  
let checkMem (towrite: exp option) 
             (lv: lval) (base: exp) (bend: exp)
             (lvt: typ) (pkind: P.pointerkind) : stmt list = 
  (* Fetch the length field in a temp variable. But do not create the 
   * variable until certain that it is needed *)
  (* ignore (E.log "checkMem: lvt: %a\n" d_plaintype lvt); *)
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
  let rec doCheckTags (towrite: exp option) (where: lval) 
                      (t: typ) (pkind: P.pointerkind) acc = 
    match unrollType t with 
    | (TInt _ | TFloat _ | TEnum _ | TBitfield _ ) -> acc
    | TComp comp when isFatComp comp -> begin (* A fat pointer *)
        match towrite with
          None -> (* a read *)
            if pkind = P.Wild then
              (checkFatPointerRead base 
                 (AddrOf(where, lu)) (getLenExp ())) :: acc
            else
              acc
        | Some towrite -> (* a write *)
            let _, whatp, whatb, _ = readFieldsOfFat towrite t in
            if pkind = P.Wild then
              (checkFatPointerWrite base (AddrOf(where, lu)) 
                 whatb whatp (getLenExp ())) :: acc
            else
              checkFatStackPointer whatp whatb :: acc
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
          doCheckTags newtowrite newwhere fi.ftype pkind acc
        in
        List.fold_left doOneField acc comp.cfields

    | TArray(bt, _, a) 
        when (match unrollType bt with
          (TInt _ | TFloat _ | TEnum _ | TBitfield _ ) -> true | _ -> false) 
      -> acc

    | TPtr(_, _) -> (* This can only happen if we are writing to an untagged 
                     * area. All other areas contain only fat pointers *)
        begin
          match towrite with
            Some x -> checkLeanStackPointer x :: acc
          | None -> acc
        end
(*            
    | TFun _ when towrite = None -> acc
*)
    | _ -> E.s (E.unimp "unexpected type in doCheckTags: %a\n" d_type t)
  in
  (* See first what we need in order to check tags *)
  let zeroAndCheckTags = 
    (* Call doCheckTags anyway because even for safe writes it needs to check 
     * when pointers are written *)
    let dotags = doCheckTags towrite lv lvt pkind [] in
    if pkind = P.Wild then
      match towrite with 
        None -> dotags
      | Some _ -> (checkZeroTags base (getLenExp ()) lv lvt) :: dotags
    else
      dotags
  in
  (* Now see if we need to do bounds checking *)
  let checkb = 
    (checkBounds getLenExp base bend lv lvt pkind) @ zeroAndCheckTags in
  (* See if we need to generate the length *)
  (match !lenExp with
    None -> checkb
  | Some _ -> 
      (checkFetchLength (getVarOfExp (getLenExp ())) base) :: checkb)
  
  
          
    (* Check a write *)
let checkRead = checkMem None
let checkWrite e = checkMem (Some e)


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
        let (lvt, lvkind, lv', lvbase, lvend, dolv) = boxlval lv in
        let (doe, e') = boxexpf e in (* Assume et is the same as lvt *)
        (* Now do a cast, just in case some qualifiers are different *)
        let (doe', e2) = castTo e' lvt doe in
        let (_, doe3, e3) = fexp2exp e2 doe' in
        let check = 
          match lv' with
            Mem _, _ -> 
              checkWrite e3 lv' lvbase lvend lvt lvkind
          | Var vi, off when (vi.vglob || lvkind != P.Safe) -> 
              checkWrite e3 lv' lvbase lvend lvt lvkind
          | _ -> []
        in
        mkSeq (dolv @ doe3 @ check @ [Instr(Set(lv', e3, l))])

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
                (_, _, (Var _, NoOffset), _, _, _) -> Some vi, []
              | (tv, _, ((Var _, Field(dfld, NoOffset)) as newlv), _, _,[]) -> 
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
        mkSeq (doouts @ doins @ 
               [Instr(Asm(tmpls, isvol, outputs', inputs', clobs))])
            
  with e -> begin
    ignore (E.log "boxinstr (%s)\n" (Printexc.to_string e));
    dStmt (dprintf "booo_instruction(%a)" d_instr ins)
  end

(* Given an lvalue, generate all the stuff needed to construct a pointer to 
 * it: a base type and a pointer type kind, an lvalue whose address makes the 
 * first component of the pointer and two exp's to be used as the second 
 * component (for pointer kinds other than Safe) and the third component (for 
 * pointer kinds Seq). We also compute a list of statements that must be 
 * executed to check the bounds.  *)
and boxlval (b, off) : (typ * P.pointerkind * lval * exp * exp * stmt list) = 

  (* As we go along the offset we keep track of the basetype and the pointer 
   * kind, along with the current base expression and a function that can be 
   * used to recreate the lval. *)
  let (btype, pkind, mklval, base, bend, stmts) as startinput = 
    match b with
      Var vi -> vi.vtype, P.Safe, (fun o -> (Var vi, o)), zero, zero, []
    | Mem addr -> 
        let (addrt, doaddr, addr', addr'base, addr'len) = boxexpSplit addr in
        let addrt', pkind = 
          match unrollType addrt with
            TPtr(t, a) -> t, kindOfType addrt
          | _ -> E.s (E.unimp "Mem but no pointer type: %a@!addr= %a@!"
                        d_plaintype addrt d_plainexp addr)
        in
        (addrt', pkind, (fun o -> (Mem addr', o)), addr'base, addr'len, doaddr)
  in
(*  ignore (E.log "Lval=%a@!startinput=%a\n" 
            d_lval (b, off) P.d_pointerkind pkind); *)
  (* Check index when we switch from Index to Safe *)
  let toSafe ((btype, pkind, mklval, base, bend, stmts) as input) = 
    match pkind with
      P.Wild -> input (* No change if we are in a tagged area *)
    | P.Safe -> input (* No change if already safe *)
    | P.Index -> 
        let _, _, _, docheck = 
          indexToSafe (mkAddrOf (mklval NoOffset)) 
            (TPtr(btype, [])) base bend []
        in
        (btype, P.Safe, mklval, zero, zero,
         stmts @ docheck)

    | P.Seq -> 
        let _, _, _, docheck = 
          seqToSafe (mkAddrOf (mklval NoOffset)) (TPtr(btype,[])) base bend []
        in
        (btype, P.Safe, mklval, zero, zero,
         stmts @ docheck)

    | P.FSeq -> 
        let _, _, _, docheck = 
          fseqToSafe (mkAddrOf (mklval NoOffset)) (TPtr(btype,[])) base bend []
        in
        (btype, P.Safe, mklval, zero, zero,
         stmts @ docheck)

    | _ -> E.s (E.unimp "toSafe on unexpected pointer kind %a"
                  P.d_pointerkind pkind)
  in
  let toIndexSeq ((btype, pkind, mklval, base, bend, stmts) as input) = 
    match pkind with
    | (P.Safe|P.Wild) -> 
        let (elemtype, pkind, base, bend) = 
          arrayPointerToIndex btype pkind (mklval NoOffset) base in
        (elemtype, pkind, mklval, base, bend, stmts)

    | _ -> E.s (E.unimp "toIndex on unexpected pointer kind %a"
                  P.d_pointerkind pkind)
  in
  (* As we go along we need to go into tagged and sized types. *)
  let goIntoTypes ((btype, pkind, mklval, base, bend, stmts) as input) = 
    match 
      (match unrollType btype with
        TComp comp when comp.cstruct -> comp.cfields
      | _ -> []) 
    with
      f1 :: f2 :: [] when (f1.fname = "_size" && f2.fname = "_array") -> 
        begin
        (* A sized array *)
          if pkind != P.Safe then
            E.s (E.bug "Sized array in a non-safe area");
          (f2.ftype, P.Safe, (fun o -> mklval (Field(f2, o))), 
           zero, zero, stmts)
        end
    | f1 :: f2 :: _ when (f1.fname = "_len" && f2.fname = "_data") -> begin
        (* A tagged data. Only wild pointers inside *)
        if pkind = P.Wild then
          E.s (E.bug "Tagged data inside a tagged area");
        (f2.ftype, P.Wild, (fun o -> mklval (Field(f2, o))),
          mkAddrOf (mklval(Field(f2,NoOffset))), zero, stmts)
    end 
    | _ -> input
  in
  (* Now do the offsets *)
  let rec doOffset ((btype, _, _, _, _, _) as input) = function
      NoOffset -> goIntoTypes input

    | Field (f, resto) -> 
        let (_, pkind, mklval, base, bend, stmts) = 
          toSafe (goIntoTypes input) in
        (* Prepare for the rest of the offset *)
        let next = 
          (f.ftype, pkind, (fun o -> mklval (Field(f, o))), base, bend, 
           stmts) in
        doOffset next resto

    | Index (e, resto) -> 
        let (btype, pkind, mklval, base, bend, stmts) = 
          toIndexSeq (goIntoTypes input) in
        (* Do the index *)
        let (_, doe, e') = boxexp e in
        (* Prepare for the rest of the offset *)
        let next = 
          (btype, pkind, (fun o -> mklval (Index(e', o))), base, bend, stmts) 
        in
        doOffset next resto
  in
  let (btype, pkind, mklval, base, bend, stmts) = doOffset startinput off in
(*  ignore (E.log "Done lval: pkind=%a@!" P.d_pointerkind pkind); *)
  (btype, pkind, mklval NoOffset, base, bend, stmts)
      
(* Given an array type return the element type, pointer kind, base and bend *)
and arrayPointerToIndex (t: typ) (k: P.pointerkind) 
                        (lv: lval) (base: exp) = 
  match unrollType t with
    TArray(elemt, _, a) when k = P.Wild -> 
      (elemt, P.Wild, base, zero)

  | TArray(elemt, _, a) when (filterAttributes "sized" a <> []) -> 
      (elemt, P.Index, mkAddrOf lv, zero)

    (* If it is not sized then better have a length *)
  | TArray(elemt, Some alen, a) -> 
      (elemt, P.Seq, mkAddrOf lv, 
       BinOp(PlusPI, mkAddrOf lv, alen, TPtr(elemt, []), lu))

  | _ -> E.s (E.bug "arrayPointerToIndex on a non-array (%a)" 
                d_plaintype t)

    (* Box an expression and return the fexp version of the result. If you do 
     * not care about an fexp, you can call the wrapper boxexp *)
and boxexpf (e: exp) : stmt list * fexp = 
  try
    match e with
    | Lval (lv) -> 
        let lvt, lvkind, lv', baseaddr, len, dolv = boxlval lv in
        let check = (* Check a read if it is in memory or if it comes from a 
                     * tagged variable *)
          match lv' with
            Mem _, _ -> 
              checkRead lv' baseaddr len lvt lvkind
          | Var vi, off when mustBeTagged vi -> 
              checkRead lv' baseaddr len lvt lvkind
          | _, _ -> []
        in
        (dolv @ check, mkFexp1 lvt (Lval(lv')))
            
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
        if !defaultIsWild then
          let l = 1 + String.length s in 
          let newt = tagType (TArray(charType, Some (integer l), [])) in
          let gvar = makeGlobalVar (newStringName ()) newt in
          gvar.vstorage <- Static;
          let varinit, dfield = 
            makeTagCompoundInit newt (Some (Const(CStr s, cloc))) in
          theFile := GVar (gvar, Some varinit) :: !theFile;
          let fatChrPtrType = fixupType charPtrType in
          let result = StartOf (Var gvar, Field(dfield, NoOffset)) in
          ([], FM (fatChrPtrType, P.Wild,
                   result, 
                   doCast result (typeOf result) voidPtrType, zero))
        else
          (* For now all string constants are transformed into FSEQ pointers *)
          let l = ((1 + String.length s) + 3) land (lnot 3) in
          let tmp = makeTempVar !currentFunction charPtrType in
          let fseqChrPtrType     = 
            fixupType (TPtr(TInt(IChar, []), [AId("fseq")]))
          in
          ([mkSet (var tmp) (Const (CStr s, cloc))], 
           FM(fseqChrPtrType, P.FSeq,
              Lval (var tmp), 
              BinOp(PlusPI, Lval (var tmp), integer l, charPtrType, lu), 
              zero))
                
          
          
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
        match bop, kindOfType et1, kindOfType et2 with
        | (PlusPI|MinusPI), ((P.Wild|P.Index|P.Seq) as k1), P.Scalar -> 
            let ptype, ptr, base, bend = readFieldsOfFat e1' et1 in
            (doe1 @ doe2, FM (restyp', k1,
                              BinOp(bop, ptr, e2', ptype, l),
                              base, bend))
        | (PlusPI|MinusPI), P.FSeq, P.Scalar -> 
            let ptype, ptr, base, _ = readFieldsOfFat e1' et1 in
            let docheck = 
              [call None (Lval (var checkPositiveFun.svar)) [ e2' ]]
            in
            (doe1 @ doe2 @ docheck, 
             FM (restyp', P.FSeq, BinOp(bop, ptr, e2', ptype, l), base, zero))

        | (MinusPP|EqP|NeP|LeP|LtP|GeP|GtP), _, _ -> 
            (doe1 @ doe2, 
             L(restyp', P.Scalar,
               BinOp(bop, readPtrField e1' et1, 
                     readPtrField e2' et2, restyp', l)))
              
        | _, P.Scalar, P.Scalar -> 
            (doe1 @ doe2, L(restyp', P.Scalar, BinOp(bop,e1',e2',restyp', l)))
              
        | (PlusPI|MinusPI), P.Safe, P.Scalar ->
            E.s (E.bug "boxbinOp: pointer arithmetic on safe pointer: %a@!"
                   d_exp e1)
        | _, _, _ -> E.s (E.unimp "boxBinOp: %a@!et1=%a@!et2=%a@!" 
                            d_binop bop d_plaintype et1 d_plaintype et2)
    end
          
    | SizeOf (t, l) -> 
        let t' = fixupType t in
        ([], L(intType, P.Scalar, SizeOf(t', l)))
          
          
    | AddrOf (lv, l) ->
        let (lvt, lvkind, lv', baseaddr, bend, dolv) = boxlval lv in
        (* Check that variables whose address is taken are flagged as such, 
         * or are globals  *)
        (match lv' with
          (Var vi, _) when not vi.vaddrof && not vi.vglob -> 
            E.s (E.bug "addrof not set for %s (addrof)" vi.vname)
        | _ -> ());
        (* The result type. *)
        let ptrtype = mkPointerTypeKind lvt lvkind in
        let res = 
          match lvkind with
            P.Safe -> mkFexp1 ptrtype (AddrOf(lv', l))
          | (P.Index | P.Wild | P.FSeq) -> 
              mkFexp2 ptrtype (AddrOf(lv', l)) baseaddr
          | P.Seq ->  mkFexp3 ptrtype (AddrOf(lv', l)) baseaddr bend
          | _ -> E.s (E.bug "boxexp: AddrOf(%a)" P.d_pointerkind lvkind)
        in
        (dolv, res)
          
    (* StartOf is like an AddrOf except for typing issues. *)
    | StartOf lv -> begin
        let (lvt, lvkind, lv', baseaddr, len, dolv) = boxlval lv in
        (* Check that variables whose address is taken are flagged *)
        (match lv' with
          (Var vi, _) when not vi.vaddrof && not vi.vglob -> 
            E.s (E.bug "addrof not set for %s (startof)" vi.vname)
        | _ -> ());
        let res = 
          match unrollType lvt with
            TArray(t, _, _) -> begin
              let newp = 
                AddrOf(addOffsetLval (Index(zero, NoOffset)) lv', lu) in
              match lvkind with
                P.Safe -> 
                  let (_, pkind, base, bend) = 
                    arrayPointerToIndex lvt lvkind lv' baseaddr
                  in
                  let pres = mkPointerTypeKind t pkind in
                  if pkind = P.Seq then
                    mkFexp3 pres newp base bend
                  else
                    mkFexp2 pres newp base
              | P.Wild -> 
                  mkFexp2 (mkPointerTypeKind t lvkind) newp baseaddr
              | _ -> E.s (E.unimp "StartOf")
            end
          | TFun _ -> begin
              (* Taking the address of a function is a special case. Since 
               * fuctions are not tagged the type of the the pointer is Safe. 
               * If we are in defaultIsWild then we must make a Wild pointer 
               * out of it *)
              let start = StartOf lv' in
              match lv' with
                Var vi, NoOffset when !defaultIsWild -> 
                  mkFexp2 (mkPointerTypeKind lvt P.Wild) start start
              | _ -> 
                  mkFexp2 (mkPointerTypeKind lvt lvkind) start baseaddr
          end

          | _ -> E.s (E.unimp "StartOf on a non-array and non-function: %a"
                        d_plaintype lvt)
        in
(*        ignore (E.log "result of StartOf: %a@!" d_fexp res); *)
        (dolv, res)
    end
    | Question (e1, e2, e3, l) ->       
        let (_, doe1, e1') = boxexp (CastE(intType, e1, lu)) in
        let (et2, doe2, e2') = boxexp e2 in
        let (et3, doe3, e3') = boxexp e3 in
        let result = mkFexp1 et2 (Question (e1', e2', e3', l)) in
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
    ([], FS(charPtrType, P.Wild, dExp (dprintf "booo_exp: %a" d_exp e)))
  end 
            
          
and boxGlobalInit e et = 
  let et' = fixupType et in
  let (e't, doe, e', e'base, e'len) = boxexpSplit e in
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
       Lval(Mem (CastE(TPtr(newt, []), 
                       AddrOf (tmp, lu), lu)),
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
    Lval(Var vi, NoOffset) -> boxexp f
  | Lval(Mem base, NoOffset) -> 
      let rest, lvkind, lv', lvbase, lvend, dolv = 
        boxlval (Mem base, NoOffset) in
      (rest, dolv @ [checkFunctionPointer (AddrOf(lv', lu)) lvbase lvkind], 
       Lval(lv'))
      
  | _ -> E.s (E.unimp "Unexpected function expression")


    
    (* Cast an fexp to another one. Accumulate necessary statements to doe *)
and castTo (fe: fexp) (newt: typ)
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
          P.Safe | P.Scalar -> newt
        | _ -> 
            let pfield, _, _ = getFieldsOfFat newt in
            pfield.ftype 
      in
      (* Cast the pointer expression to the new pointer type *)
      let castP (p: exp) = doCast p (typeOf p) newPointerType in
      (* Converts a reversed accumulator to doe *)
      let finishDoe (acc: stmt list) = doe @ (List.rev acc) in
      let oldt, oldk, p, b, bend = 
        match fe with
          L(oldt, oldk, e) -> oldt, oldk, e, zero, zero
        | FS(oldt, oldk, e) -> 
            let (_, p, b, bend) = readFieldsOfFat e oldt in
            oldt, oldk, p, b, bend
        | FM(oldt, oldk, p, b, e) -> oldt, oldk, p, b, e
        | FC(oldt, oldk, prevt, prevk, e) -> 
            let (_, p, b, bend) = readFieldsOfFat e prevt in
            oldt, oldk, p, b, bend (* Drop the cast *)
      in
      match oldk, newkind with
        (* SCALAR, SAFE -> SCALAR, SAFE *)
        (P.Scalar|P.Safe), (P.Scalar|P.Safe) -> 
          (doe, L(newt, P.Scalar, castP p))

        (* SCALAR -> INDEX, WILD, SEQ *)
      | P.Scalar, (P.Index|P.Wild|P.Seq) ->
          if not (isZero p) then
            ignore (E.warn "Casting scalar (%a) to pointer (in %s)!" 
                      d_exp p !currentFunction.svar.vname);
          let newbase, doe' = 
            if !interceptCasts && not (isInteger p) then begin
              incr interceptId;
              let tmp = makeTempVar !currentFunction voidPtrType in
              Lval(var tmp),
              doe @
              [call (Some tmp) (Lval(var interceptCastFunction.svar)) 
                  [ p ;integer !currentFileId; integer !interceptId ]
              ]
            end else 
              CastE(voidPtrType, zero, lu), doe
          in
          (doe', FM (newt, newkind, castP p, newbase, zero))

       (* SCALAR -> FSEQ *)
      | P.Scalar, P.FSeq when isZero p ->
          (doe, FM(newt, newkind, castP p, zero, zero))

       (* WILD, INDEX, SEQ, FSEQ -> SCALAR *)
      | (P.Index|P.Wild|P.FSeq|P.Seq), P.Scalar ->
          (doe, L(newt, newkind, castP p))

       (* WILD, INDEX, SEQ, FSEQ -> same_kind *)  
      | (P.Index|P.Wild|P.FSeq|P.Seq), _ when newkind =oldk -> 
          (doe, FM (newt, newkind, castP p, b, bend))

       (* INDEX -> SAFE. Must do bounds checking *)
      | P.Index, P.Safe ->
          let p', _, _, acc' = indexToSafe p newPointerType b bend [] in
          finishDoe acc', L(newt, newkind, castP p')      
       (* INDEX -> SEQ *)
      | P.Index, P.Seq ->
          let p', b', bend', acc' = indexToSeq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      
       (* INDEX -> FSEQ *)
      | P.Index, P.FSeq ->
          let p', b', bend', acc' = indexToFSeq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      

       (* SEQ -> SAFE. Must do bounds checking *)
      | P.Seq, P.Safe ->
          let p', _, _, acc' = seqToSafe p newPointerType b bend [] in
          finishDoe acc', L(newt, newkind, castP p')      
       (* SEQ -> FSEQ *)
      | P.Seq, P.FSeq ->
          let p', b', bend', acc' = seqToFSeq p b bend [] in
          finishDoe acc', FM(newt, newkind, castP p', b', bend')      

       (* FSEQ -> SAFE. Must do bounds checking *)
      | P.FSeq, P.Safe ->
          let p', _, _, acc' = fseqToSafe p newPointerType b bend [] in
          finishDoe acc', L(newt, newkind, castP p')      

       (* FSEQ -> SEQ. *)
      | P.FSeq, P.Seq ->
          doe, FM(newt, newkind, castP p, b, b)

       (******* UNIMPLEMENTED ********)
      | _, _ -> 
          E.s (E.unimp "castTo(%a -> %a.@!%a@!%a)" 
                 P.d_pointerkind oldk P.d_pointerkind newkind 
                 d_fexp fe
                 d_plaintype oldt)      
  end
      

let mangledNames : (string, unit) H.t = H.create 123
let fixupGlobName vi = 
  (* Scan a type and compute a list of qualifiers that distinguish the 
   * various possible combinations of qualifiers *)
  let rec qualNames acc = function
      TInt _ | TFloat _ | TBitfield _ | TVoid _ | TEnum _ -> acc
    | TPtr(t', _) as t -> begin
        match kindOfType t with
          P.Safe -> qualNames ("s" :: acc) t' 
        | P.Wild -> "w" :: acc (* Don't care about what it points to *)
        | P.Index -> qualNames ("i" :: acc) t' 
        | P.Seq -> qualNames ("q" :: acc) t' 
        | P.FSeq -> qualNames ("f" :: acc) t' 
        | P.Scalar -> acc
        | _ -> E.s (E.bug "qualNames")
    end
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
    | TComp comp -> begin
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
    | TForward _ -> acc (* Do not go into recursive structs *)
  in
  if vi.vglob && vi.vstorage <> Static && 
    not (List.exists (fun la -> la = vi.vname) leaveAlone) &&
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
      vi.vname <- newname
    end


(* Create the preamble (in reverse order). Must create it every time because 
 * we must consider the effect of "defaultIsWild" *)
let checkFunctionDecls = [] (* !theFile *)
let preamble () = 
  (* Define WILD away *)
  theFile := GText("#define WILD\n#define FSEQ") :: checkFunctionDecls;
  (** Create some more fat types *)
  ignore (fixupType (TPtr(TInt(IChar, []), [AId("wild")])));
(*  ignore (fixupType (TPtr(TInt(IChar, [AId("const")]), [AId("wild")]))); *)
  ignore (fixupType (TPtr(TVoid([]), [AId("wild")])));
(*  ignore (fixupType (TPtr(TVoid([AId("const")]), [AId("wild")]))); *)
  let startFile = !theFile in
  theFile := 
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
                  globbase zero
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
        List.iter 
          (fun l -> 
            let newa, newt = moveAttrsFromDataToType l.vattr l.vtype in
            l.vattr <- newa;
            l.vtype <- fixupType newt) f.slocals;
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
      let newa, newt = moveAttrsFromDataToType vi.vattr vi.vtype in
      vi.vattr <- dropAttribute newa (ACons("__format__", []));
      vi.vtype <- fixupType newt;
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
  preamble ();
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
