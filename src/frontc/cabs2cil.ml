(* Convert CABS to CIL *)
module A = Cabs
module E = Errormsg
module H = Hashtbl

open Pretty
open Cil



(*** EXPRESSIONS *************)
                                        (* We collect here the program *)
let theFile : global list ref = ref []

(********* ENVIRONMENTS ***************)

(* The environment is kept in two distinct data structures. A hash table maps 
 * each original variable name into a varinfo. (Note that the varinfo might 
 * contain an alpha-converted name.) The Ocaml hash tables can keep multiple 
 * mappings for a single key. Each time the last mapping is returned and upon 
 * deletion the old mapping is restored. To keep track of local scopes we 
 * also maintain a list of scopes (represented as lists). *)

let env : (string, varinfo) H.t = H.create 307

 (* In the scope we keep the original name, so we can remove them from the 
  * hash table easily *)
let scopes :  string list ref list ref = ref []


 (* We also keep a hash table of the new variable names so that we can verify 
  * that the new names we are creating are indeed new. We add all globals 
  * in scope there and all locals, whether in scope or not. This is necessary 
  * because we will coalesce all locals at the start of the function. We 
  * could reuse locals with the same type but that might complicate some 
  * analyses later. *)
 (* Each name is split into a prefix and a maximal suffix consisting only of 
  * digits. For each prefix we store a ref cell containing the maximal 
  * integer value of the suffix for which the name is defined. A suffix of -1 
  * means that only the version with no suffix is defined *)
let alphaTable : (string, int ref) H.t = H.create 307
let localId = ref (-1)   (* All locals get id's starting at 0 *)
let locals : varinfo list ref = ref []

let resetLocals () = 
  localId := (-1); locals := []

let startFile () = 
  H.clear env;
  H.clear alphaTable;
  resetLocals ()


     (* Eliminate all locals from the alphaTable. Return the maxlocal id and 
      * the list of locals *)
let endFunction () : (int * varinfo list) = 
  let rec loop revlocals = function
      [] -> revlocals
    | lvi :: t -> H.remove alphaTable lvi.vname; loop (lvi :: revlocals) t
  in
  let revlocals = loop [] !locals in
  let maxid = !localId + 1 in
  resetLocals ();
  (maxid, revlocals)

let startScope () = 
  scopes := (ref []) :: !scopes

     (* Exit a scope and clean the environment. We do not yet delete from 
      * the name table *)
let exitScope () = 
  let this, rest = 
    match !scopes with
      car :: cdr -> car, cdr
    | [] -> E.s (E.bug "Not in a scope")
  in
  scopes := rest;
  let rec loop = function
      [] -> ()
    | n :: t -> H.remove env n; loop t
  in
  loop !this

(* Lookup a variable name. Might raise Not_found *)
let lookup n = H.find env n 

let docEnv () = 
  let acc : (string * varinfo) list ref = ref [] in
  H.iter (fun k d -> acc := (k, d) :: !acc) env;
  docList line (fun (k, d) -> dprintf "  %s -> %s" k d.vname) () !acc


(* Create a new variable ID *)
let newVarId name isglobal = 
  if isglobal then H.hash name
  else begin
    incr localId;
    !localId
  end

(* Create a new variable name. Give the original source name *)
let newVarName lookupname = 
  (* Split the lookup name into a prefix and a suffix. *)
  let l = String.length lookupname in
  let rec split i = 
    assert (i >= 0 && i < l);
    let last = Char.code (String.get lookupname i) - Char.code '0' in
    if last >= 0 && last <= 9 then
      let (prefix, suffix) = split (i - 1) in
      (prefix, if suffix >= 0 then suffix * 10 + last else last)
    else
      (String.sub lookupname 0 (i + 1), -1)
  in
  let (prefix, suffix) = split (l - 1) in
  try
    let rc = H.find alphaTable prefix in
    let newsuffix = if suffix > !rc then suffix else !rc + 1 in
    rc := newsuffix;
    prefix ^ (string_of_int newsuffix)
  with Not_found -> begin (* First variable with this prefix *)
    H.add alphaTable prefix (ref suffix);
    lookupname  (* Return the original name *)
  end

let docAlphaTable () = 
  let acc : (string * int) list ref = ref [] in
  H.iter (fun k d -> acc := (k, !d) :: !acc) alphaTable;
  docList line (fun (k, d) -> dprintf "  %s -> %d" k d) () !acc


(* Add a new variable. Do alpha-conversion if necessary *)
let addNewVar vi = 
  let alphaConvertAndAdd vi = 
    let newname = newVarName vi.vname in
    let newvi = if vi.vname = newname then vi else {vi with vname = newname} in
    H.add env vi.vname newvi; 
    if not vi.vglob then begin
      locals := newvi :: !locals;
      (match !scopes with
        [] -> E.s (E.bug "Adding a local %s but not in a scope" vi.vname)
      | s :: _ -> s := vi.vname :: !s)
    end;
    newvi
  in
  if vi.vglob then (* Some special cases apply to global variables *)
    try (* See if already defined *)
      let oldvi = H.find env vi.vname in
      if oldvi.vstorage = Extern  && vi.vstorage = NoStorage then begin
          (* We'll add the new version instead. *)
        H.remove env vi.vname;
        let newvi' = {vi with vname = oldvi.vname} in
        H.add env vi.vname newvi';
        newvi'
      end else begin
        if vi.vstorage = Extern && (oldvi.vstorage = NoStorage ||
                                    oldvi.vstorage = Extern) then
          oldvi               (* Don't add a new version *)
        else
          match vi.vtype, oldvi.vtype with
                                        (* function prototypes are not 
                                         * definitions *)
            TFun _, TFun _ -> oldvi
          | TPtr (TFun _, _), TPtr (TFun _, _) -> oldvi
          | _ -> E.s (E.unimp "Global variable redefinition: %s\n" vi.vname)
      end
    with Not_found -> alphaConvertAndAdd vi
  else (* A local *) 
    alphaConvertAndAdd vi
  

(* Create a new temporary variable *)
let newTempVar typ = 
  (* Strip the "const" from the type. It is unfortunate that const variables 
    can only be set in initialization. Once we decided to move all 
    declarations to the top of the functions, we have no way of setting a 
    "const" variable *)
  let stripConst t =
    let a = typeAttrs t in
    let rec loop = function
        [] -> []
      | AId("const") :: rest -> loop rest
      | (x :: rest) as i -> 
          let rest' = loop rest in
          if rest' == rest then i else x :: rest'
    in
    let a' = loop a in
    if a == a' then
      t
    else
      setTypeAttrs t a'
  in
  addNewVar 
    { vname = "tmp";  (* addNewVar will make the name fresh *)
      vid   = newVarId "tmp" false;
      vglob = false;
      vtype = stripConst typ;
      vdecl = locUnknown;
      vattr = [];
      vaddrof = false;
      vstorage = NoStorage;
    } 

(*** In order to process GNU_BODY expressions we must record that a given 
 *** COMPUTATION is interesting *)
let gnu_body_result : (A.statement * ((exp * typ) option ref)) ref 
    = ref (A.NOP, ref None)

(*** When we do statements we need to know the current return type *)
let currentReturnType : typ ref = ref (TVoid([]))
let currentFunctionName : string ref = ref "no_function"

(******** GLOBAL TYPES **************)
let typedefs : (string, typ) H.t = H.create 113

   (* Keep a list of unresolved types. Maybe they are forward references *)
let forwardTypes : string list ref = ref []

let recordTypeName n t = 
  H.add typedefs n t

let findTypeName n = 
  try
    H.find typedefs n
  with Not_found -> begin
    E.s (E.unimp "Cannot find type %s\n" n)
  end

let recordCompType t = 
  let key = 
    match t with
      TStruct(n, _, _) -> "struct " ^ n
    | TUnion(n, _, _) -> "union " ^ n
    | _ -> E.s (E.bug "recordCompType")
  in
  H.add typedefs key t;
  (* Resolve some incomplete types *)
  if (List.exists (fun x -> x = key) !forwardTypes) then
    addForwardType key t

   (* kind is either "struct" or "union" and n is a name *)
let findCompType kind n = 
  let key = kind ^ " " ^ n in
  try
    H.find typedefs key
  with Not_found -> begin
    forwardTypes := key :: !forwardTypes;
    TForward key
  end
  



(**** Occasionally we see structs with no name and no fields *)
(* Sometimes we need to lookup enum fields *)
let enumFields : (string, (int * typ)) H.t = H.create 17
let recordEnumField n idx typ = 
  try 
    let (idx', typ') = H.find enumFields n in
    if idx <> idx' then 
      E.s (E.unimp "Enum key %s was encoutered before with index %d in type %a"
             n idx' d_type typ')
  with Not_found -> 
    H.add enumFields n (idx, typ)

let lookupEnumField n = 
  H.find enumFields n                   (* might raise Not_found *)
  

(************ Labels ***********)
(* Since we turn dowhile and for loops into while we need to take care in 
 * processing the continue statement. For each loop that we enter we place a 
 * marker in a list saying what kinds of loop it is. When we see a continue 
 * for a Non-while loop we must generate a label for the continue *)
type loopstate = 
    While
  | NotWhile of string ref

let continues : loopstate list ref = ref []

let startLoop iswhile = 
  continues := (if iswhile then While else NotWhile (ref "")) :: !continues

let labelId = ref 0
let doContinue () : stmt = 
  match !continues with
    [] -> E.s (E.bug "continue not in a loop")
  | While :: _ -> Continue
  | NotWhile lr :: _ -> 
      if !lr = "" then begin
        incr labelId;
        lr := "Cont" ^ (string_of_int !labelId)
      end;
      Goto !lr
  
let labContinue () = 
  match !continues with
    [] -> E.s (E.bug "labContinue not in a loop")
  | While :: rest -> Skip
  | NotWhile lr :: rest -> if !lr = "" then Skip else Label !lr

let exitLoop () = 
  match !continues with
    [] -> E.s (E.bug "exit Loop not in a loop")
  | _ :: rest -> continues := rest
      

(**** EXP actions ***)
type expAction = 
    ADrop                               (* Drop the result. Only the 
                                         * side-effect is interesting *)
  | ASet of lval * typ                  (* Put the result in a given lval, 
                                         * provided it matches the type  *)
  | AExp of typ option                  (* Return the exp as usual. 
                                         * Optionally we can specify an 
                                         * expected type. This is useful for 
                                         * constants *)



(******** CASTS *********)
let integralPromotion (t : typ) : typ = (* c.f. ISO 6.3.1.1 *)
  match unrollType t with
          (* We assume that an IInt can hold even an IUShort *)
    TInt ((IShort|IUShort|IChar|ISChar|IUChar), a) -> TInt(IInt, a)
  | TInt _ -> t
  | TEnum (_, _, a) -> TInt(IInt, a)
  | TBitfield((IShort|IChar|ISChar), _, a) -> TInt(IInt, a)
  | TBitfield((IUShort|IUChar), _, a) -> TInt(IUInt, a)
  | TBitfield(i, _, a) -> TInt(ILong, a)
  | _ -> E.s (E.unimp "integralPromotion")
  

let arithmeticConversion    (* c.f. ISO 6.3.1.8 *)
    (t1: typ)
    (t2: typ) : typ = 
  let checkToInt _ = () in  (* dummies for now *)
  let checkToFloat _ = () in
  match unrollType t1, unrollType t2 with
    TFloat(FLongDouble, _), _ -> checkToFloat t2; t1
  | _, TFloat(FLongDouble, _) -> checkToFloat t1; t2
  | TFloat(FDouble, _), _ -> checkToFloat t2; t1
  | _, TFloat (FDouble, _) -> checkToFloat t1; t2
  | TFloat(FFloat, _), _ -> checkToFloat t2; t1
  | _, TFloat (FFloat, _) -> checkToFloat t1; t2
  | _, _ -> begin
      let t1' = integralPromotion t1 in
      let t2' = integralPromotion t2 in
      match unrollType t1', unrollType t2' with
        TInt(IULongLong, _), _ -> checkToInt t2'; t1'
      | _, TInt(IULongLong, _) -> checkToInt t1'; t2'
            
      | TInt(ILongLong, _), _ -> checkToInt t2'; t1'
      | _, TInt(ILongLong, _) -> checkToInt t1'; t2'
            
      | TInt(IULong, _), _ -> checkToInt t2'; t1'
      | _, TInt(IULong, _) -> checkToInt t1'; t2'
            
      | TInt(ILong, _), _ -> checkToInt t2'; t1'
      | _, TInt(ILong, _) -> checkToInt t1'; t2'
            
      | TInt(IUInt, _), _ -> checkToInt t2'; t1'
      | _, TInt(IUInt, _) -> checkToInt t1'; t2'
            
      | TInt(IInt, _), TInt (IInt, _) -> t1'

      | _, _ -> E.s (E.unimp "arithmetic Conversion.@!T1=%a@!T2=%a@!"
                       d_plaintype t1 d_plaintype t2)
  end

let conditionalConversion (e2: exp) (t2: typ) (e3: exp) (t3: typ) : typ =
  let tresult =  (* ISO 6.5.15 *)
    match unrollType t2, unrollType t3 with
      (TInt _ | TEnum _ | TBitfield _ | TFloat _), 
      (TInt _ | TEnum _ | TBitfield _ | TFloat _) -> 
        arithmeticConversion t2 t3 
    | TStruct(n2, _, _), TStruct(n3, _, _) when n2 = n3 -> t2
    | TUnion(n2, _, _), TUnion(n3, _, _) when n2 = n3 -> t2
    | TPtr(_, _), TPtr(TVoid _, _) -> t2
    | TPtr(TVoid _, _), TPtr(_, _) -> t3
    | TPtr(t2'', _), TPtr(t3'', _) 
          when typeSig t2'' = typeSig t3'' -> t2
    | TPtr(_, _), TInt _ when 
            (match e3 with Const(CInt(0,_,_),_) -> true | _ -> false) -> t2
    | TInt _, TPtr _ when 
              (match e2 with Const(CInt(0,_,_),_) -> true | _ -> false) -> t3
    | _, _ -> E.s (E.unimp "A.QUESTION")
  in
  tresult

  
let rec castTo (ot : typ) (nt : typ) (e : exp) : (typ * exp ) = 
  if typeSig ot = typeSig nt then (ot, e) else
  match ot, nt with
    TNamed(_,r, _), _ -> castTo r nt e
  | _, TNamed(_,r, _) -> castTo ot r e
  | TForward(rt), _ -> castTo (resolveForwardType rt) nt e
  | _, TForward(rt) -> castTo ot (resolveForwardType rt) e
  | TInt(ikindo,_), TInt(ikindn,_) -> 
      (nt, if ikindo == ikindn then e else CastE(nt, e, lu))

  | TPtr (told, _), TPtr(tnew, _) -> (nt, CastE(nt, e, lu))

  | TInt _, TPtr _ when
      (match e with Const(CInt(0,_,_),_) -> true | _ -> false) -> 
        (nt, (CastE(nt, e, lu)))

  | TInt _, TPtr _ -> (nt, (CastE(nt,e,lu)))

  | TPtr _, TInt _ -> (nt, (CastE(nt,e,lu)))

  | TArray _, TPtr _ -> (nt, (CastE(nt,e,lu)))

  | TArray(t1,_,_), TArray(t2,None,_) when typeSig t1 = typeSig t2 -> (nt, e)

  | TPtr _, TArray(_,_,_) -> (nt, e)

  | TEnum _, TInt _ -> (nt, e)
  | TFloat _, TInt _ -> (nt, (CastE(nt,e,lu)))
  | TInt _, TFloat _ -> (nt, (CastE(nt,e,lu)))
  | TFloat _, TFloat _ -> (nt, (CastE(nt,e,lu)))
  | TInt _, TEnum _ -> (intType, e)
  | TEnum _, TEnum _ -> (intType, e)

  | TFun _, TPtr(TFun _, _) -> (nt, e)


  | TBitfield _, TInt _ -> (nt, e)
  | TInt _, TBitfield _ -> (nt, e)


  | _ -> E.s (E.unimp "castTo %a -> %a@!" d_type ot d_type nt)

(* A cast that is used for conditional expressions. Pointers are Ok *)
let checkBool (ot : typ) (e : exp) : bool =
  match unrollType ot with
    TInt _ -> true
  | TPtr _ -> true
  | TEnum _ -> true
  | TBitfield _ -> true
  | TFloat _ -> true
  |  _ -> E.s (E.unimp "castToBool %a" d_type ot)



(* Do types *)
(* Process a name group *)
let doNameGroup (sng: A.single_name -> 'a) 
                ((bt,s,nl) : A.name_group) : 'a list =
  List.map (fun n -> sng (bt,s,n)) nl


let rec makeFieldInfo (host: string) 
                     ((bt,st,(n,nbt,a,e)) : A.single_name) : fieldinfo = 
  let rec removeNamed = function
      TNamed (_, t, _) -> removeNamed t
    | TPtr(t,a) -> TPtr(removeNamed t, a)
    | t -> t
  in
  { fstruct  =  host;
    fname    =  n;
    ftype    =  doType [] nbt;
    fattr    =  List.map doAttr a;
  } 

and makeVarInfo (isglob: bool) 
                (ldecl: location)
                ((bt,st,(n,nbt,a,e)) : A.single_name) : varinfo = 
  { vname    = n;
    vid      = newVarId n isglob;
    vglob    = isglob;
    vstorage = doStorage st;
    vattr    = List.map doAttr a;
    vdecl    = ldecl;
    vtype    = doType [] nbt;
    vaddrof  = false;
  } 


and doAttr : A.attribute -> attribute = function
    (s, []) -> AId s
  | (s, el) -> 
      let attrOfExp = function
          A.VARIABLE n -> begin
            try 
              let vi = lookup n in
              AVar vi
            with Not_found -> 
              AId n
          end
        | A.CONSTANT (A.CONST_STRING s) -> AStr s

        | A.CONSTANT (A.CONST_INT str) -> AInt (int_of_string str)
        | _ -> E.s (E.unimp "ACons")
      in
      ACons (s, List.map attrOfExp el)

and doStorage = function
    A.NO_STORAGE -> NoStorage
  | A.AUTO -> NoStorage
  | A.REGISTER -> Register
  | A.INLINE -> NoStorage
  | A.STATIC _ -> Static
  | A.EXTERN _ -> Extern

and doType (a : attribute list) = function
    A.NO_TYPE -> intType
  | A.VOID -> TVoid a
  | A.INT (sz, sgn) -> 
      let ikind = 
        match sz, sgn with
          A.CHAR, A.NO_SIGN -> IChar
        | A.CHAR, A.SIGNED -> ISChar
        | A.CHAR, A.UNSIGNED -> IUChar
        | A.NO_SIZE, (A.NO_SIGN|A.SIGNED) -> IInt
        | A.NO_SIZE, A.UNSIGNED -> IUInt
        | A.SHORT, (A.NO_SIGN|A.SIGNED) -> IShort
        | A.SHORT, A.UNSIGNED -> IUShort
        | A.LONG, (A.NO_SIGN|A.SIGNED) -> ILong
        | A.LONG, A.UNSIGNED -> IULong
        | A.LONG_LONG, (A.NO_SIGN|A.SIGNED) -> ILongLong
        | A.LONG_LONG, A.UNSIGNED -> IULongLong
      in
      TInt (ikind, a)
  | A.BITFIELD (bt, e) -> 
      let bt' = doType [] bt in
      let ikind = 
        match unrollType bt' with 
          TInt (ikind, _) -> ikind
        | _ -> E.s (E.unimp "Base type for bitfield is not an integer type")
      in
      let width = match doExp e (AExp None) with
        ([], Const(CInt(i,_,_),_), _) -> i
      | _ -> E.s (E.unimp "bitfield width is not an integer")
      in
      TBitfield (ikind, width, a)
  | A.FLOAT lng -> TFloat ((if lng then FDouble else FFloat), a)
  | A.DOUBLE lng -> TFloat ((if lng then FLongDouble else FDouble), a)
  | A.PTR bt -> TPtr (doType [] bt, a)
  | A.ARRAY (bt, len) ->
      let lo = 
        match len with 
          A.NOTHING -> None 
        | _ -> Some (doPureExp len)
      in
      TArray (doType [] bt, lo, a)

  | A.STRUCT n -> 
      if n = "" then E.s (E.unimp "Missing struct tag");
      findCompType "struct" n

  | A.STRUCTDEF (n, nglist) -> (* This introduces a new type always *)
      let n = if n = "" then newTypeName "struct" else n in
      let flds = 
        List.concat (List.map (doNameGroup (makeFieldInfo n)) nglist) in
      (* Drop volatile from struct *)
      let a = dropAttribute a (AId("volatile")) in
      let a = dropAttribute a (AId("const")) in
      let tp = TStruct (n, flds, a) in
      recordCompType tp; 
      tp

  | A.UNION n -> 
      if n = "" then E.s (E.unimp "Missing union tag");
      findCompType "union" n

  | A.UNIONDEF (n, nglist) -> (* This introduces a new type always *)
      let n = if n = "" then newTypeName "union" else n in
      let flds = 
        List.concat (List.map (doNameGroup (makeFieldInfo n)) nglist) in
      let tp = TUnion (n, flds, a) in
      recordCompType tp; 
      tp


  | A.PROTO (bt, snlist, isvararg, _) ->
      (* Turn [] types into pointers in the arguments and the result type. 
       * This simplifies our life a lot  *)
      let arrayToPtr = function
          TArray(t,_,attr) -> TPtr(t, AId("const") :: attr)
        | x -> x
      in
      let targs = 
        match List.map (makeVarInfo false locUnknown) snlist  with
          [t] when (match t.vtype with TVoid _ -> true | _ -> false) -> []
        | l -> l
      in
      List.iter (fun a -> a.vtype <- arrayToPtr a.vtype) targs;
      (* See if the bt has some attributes that actually belong to the 
         function *)
      let bt', a' = 
        match bt with
          A.ATTRTYPE (bt', ["stdcall", []]) -> bt', AId("stdcall") :: a
        | A.ATTRTYPE (bt', ["cdecl", []]) -> bt', AId("cdecl") :: a
        | _ -> bt, a
      in
      let tres = arrayToPtr (doType [] bt') in
      TFun (tres, targs, isvararg, a')

  | A.OLD_PROTO _ -> E.s (E.unimp "oldproto")
  | A.ENUM n -> TEnum (n, [], a)
  | A.ENUMDEF (n, eil) -> 
      let n = if n = "" then newTypeName "enum" else n in
      let rec loop i = function
          [] -> []
        | (kname, A.NOTHING) :: rest -> 
            recordEnumField kname i intType;
            (kname, i) :: loop (i + 1) rest

        | (kname, e) :: rest ->
            let i = 
              match doExp e (AExp None) with
                [], Const(CInt(i,_,_),_), _ -> i
              | [], UnOp(Neg,Const(CInt(i,_,_), _),_,_), _ -> - i
              | _ -> E.s (E.unimp "enum with initializer")
            in
            recordEnumField kname i intType;
            (kname, i) :: loop (i + 1) rest
      in
      let fields = loop 0 eil in
      let res = TEnum (n, loop 0 eil, a) in
      List.iter (fun (n,fieldidx) -> recordEnumField n fieldidx res) fields;
      res
(*
  | A.CONST bt -> doType (AId("const") :: a) bt
  | A.VOLATILE bt -> doType (AId("volatile") :: a) bt
*)
  | A.ATTRTYPE (bt, a') -> 
      let rec doAttribute = function
          (s, []) -> AId s
        | (s, args) -> 
            let rec doArg = function
                A.CONSTANT (A.CONST_INT str) ->
                  AInt (try int_of_string str with _ -> 
                    E.s (E.unimp "integer attribute"))
              | A.CONSTANT (A.CONST_STRING str) -> AStr str
              | A.VARIABLE n -> begin
                  try 
                    AVar (lookup n)
                  with Not_found -> 
                    AId n
                end
              | _ -> E.s (E.unimp "constructed attribute")
            in
            ACons (s, List.map doArg args)
      in
      doType ((List.map doAttribute a') @ a) bt

  | A.NAMED_TYPE n -> begin
      match findTypeName n with
        (TNamed _) as x -> x
      | typ -> TNamed(n, typ, a)
  end
  | A.TYPEOF e -> 
      let (se, _, t) = doExp e (AExp None) in
      if se <> [] then
        E.s (E.unimp "typeof for a non-pure expression\n");
      t

  
     (* Process an expression and in the process do some type checking, 
      * extract the effects as separate statements  *)
and doExp (e : A.expression) (what: expAction) : (stmt list * exp * typ) = 
  (* A subexpression of array type is automatically turned into StartOf(e) *)
  let processArray e t = 
    match e, unrollType t with
      Lval(lv), TArray(t, _, a) -> StartOf lv, TPtr(t, AId("const") :: a)
    | Compound _, TArray(t', _, a) -> e, t
    | _, TArray _ -> E.s (E.unimp "Array expression is not lval")
    | _ -> e, t
  in
  (* Before we return we call finishExp *)
  let finishExp se e t = 
    match what with 
      ADrop -> (se, e, t)
    | AExp _ -> 
        let (e', t') = processArray e t in
        (se, e', t')

    | ASet (lv, lvt) -> begin
        (* See if the set was done already *)
        match e with 
          Lval(lv') when lv == lv' -> (se, integer 0, intType)
        | _ -> 
            let (e', t') = processArray e t in
            let (t'', e'') = castTo t' lvt e' in
            (se @ [mkSet lv e''], integer 0, intType)
    end
  in
  let findField strname n fidlist = 
    try
      List.find (fun fid -> n = fid.fname) fidlist
    with Not_found -> E.s (E.unimp "Cannot find field %s in %s" n strname)
  in
  try
    match e with
    | A.NOTHING when what = ADrop -> finishExp [] (integer 0) intType
    | A.NOTHING ->
        ignore (E.log "doExp nothing\n");
        finishExp [] (Const(CStr("exp_nothing"),lu)) (TPtr(TInt(IChar,[]),[]))

    (* Do the potential lvalues first *)          
    | A.VARIABLE n -> begin
        (* See if this is an enum field *)
        try 
          let (idx, typ) = lookupEnumField n in
          finishExp [] (integer idx) typ    (* It is *)
        with Not_found -> 
          try                           (* It must be a real variable *)
            let vi = lookup n in
            finishExp [] (Lval(var vi)) vi.vtype
          with Not_found -> begin 
            ignore (E.log "Cannot resolve variable %s\n" n);
            raise Not_found
          end
    end
    | A.INDEX (e1, e2) -> begin
      (* Recall that doExp turns arrays into StartOf pointers *)
        let (se1, e1', t1) = doExp e1 (AExp None) in
        let (se2, e2', t2) = doExp e2 (AExp None) in
        let se = se1 @ se2 in
        let (e1'', e2'', tresult) = 
          match unrollType t1, unrollType t2 with
            TPtr(t1e,_), (TInt _|TEnum _ |TBitfield _) -> e1', e2', t1e
          | (TInt _|TEnum _|TBitfield _), TPtr(t2e,_) -> e2', e1', t2e
          | _ -> 
              E.s (E.unimp 
                     "Expecting a pointer type in index:@! t1=%a@!t2=%a@!"
                     d_plaintype t1 d_plaintype t2)
        in
        (* Do some optimization of StartOf *)
        finishExp se (mkMem e1'' (Index(e2'',NoOffset))) tresult

    end      
    | A.UNARY (A.MEMOF, e) -> 
        let (se, e', t) = doExp e (AExp None) in
        let tresult = 
          match unrollType t with
            TPtr(te, _) -> te
          | _ -> E.s (E.unimp "Expecting a pointer type in * ")
        in
        finishExp se 
                  (mkMem e' NoOffset)
                  tresult

           (* e.str = (& e + off(str)). If e = (be + beoff) then e.str = (be 
            * + beoff + off(str))  *)
    | A.MEMBEROF (e, str) -> 
        let (se, e', t') = doExp e (AExp None) in
        let lv = 
          match e' with Lval x -> x 
          | _ -> E.s (E.unimp "Expected an lval in MEMDEROF")
        in
        let fid = 
          match unrollType t' with
            TStruct (n, fil, _) -> findField n str fil
          | TUnion (n, fil, _) -> findField n str fil
          | _ -> E.s (E.unimp "expecting a struct with field %s" str)
        in
        let lv' = Lval(addOffset (Field(fid, NoOffset)) lv) in
        finishExp se lv' fid.ftype
          
       (* e->str = * (e + off(str)) *)
    | A.MEMBEROFPTR (e, str) -> 
        let (se, e', t') = doExp e (AExp None) in
        let pointedt = 
          match unrollType t' with
            TPtr(t1, _) -> t1
          | TArray(t1,_,_) -> t1
          | _ -> E.s (E.unimp "expecting a pointer to a struct")
        in
        let fid = 
          match unrollType pointedt with 
            TStruct (n, fil, _) -> findField n str fil
          | TUnion (n, fil, _) -> findField n str fil
          | x -> 
              E.s (E.unimp 
                     "expecting a struct with field %s. Found %a. t1 is %a" 
                     str d_type x d_type t')
        in
        finishExp se (mkMem e' (Field(fid, NoOffset))) fid.ftype
          
          
    | A.CONSTANT ct -> begin
        let finishCt c t = finishExp [] (Const(c, lu)) t in
        let hasSuffix str = 
          let l = String.length str in
          fun s -> 
            let ls = String.length s in
            l >= ls && s = String.uppercase (String.sub str (l - ls) ls)
        in
        match ct with 
          A.CONST_INT str -> begin
            let l = String.length str in
            (* See if it is octal or hex *)
            let octalhex = (l >= 1 && String.get str 0 = '0') in 
            (* The length of the suffix and a list of possible kinds. See ISO 
             * 6.4.4.1 *)
            let hasSuffix = hasSuffix str in
            let suffixlen, kinds = 
              if hasSuffix "ULL" || hasSuffix "LLU" then
                3, [IULongLong]
              else if hasSuffix "LL" then
                2, if octalhex then [ILongLong; IULongLong] else [ILongLong]
              else if hasSuffix "UL" || hasSuffix "LU" then
                2, [IULong; IULongLong]
              else if hasSuffix "L" then
                1, if octalhex then [ILong; IULong; ILongLong; IULongLong] 
                               else [ILong; ILongLong]
              else if hasSuffix "U" then
                1, [IUInt; IULong; IULongLong]
              else
                0, if octalhex 
                   then [IInt; IUInt; ILong; IULong; ILongLong; IULongLong]
                   else [IInt; ILong; ILongLong]
            in
            let baseint = String.sub str 0 (l - suffixlen) in
            try
              let i = int_of_string baseint in
              let res = integerKinds i kinds (Some str) in
              finishCt res (typeOf (Const(res, lu)))
            with e -> begin
              ignore (E.log "int_of_string %s (%s)\n" str 
                        (Printexc.to_string e));
              finishCt (CStr("booo CONS_INT")) (TPtr(TInt(IChar,[]),[]))
            end
          end
        | A.CONST_STRING s -> 
            (* Maybe we burried __FUNCTION__ in there *)
            let s' = 
              try
                let start = String.index s (Char.chr 0) in
                let l = String.length s in
                let tofind = (String.make 1 (Char.chr 0)) ^ "__FUNCTION__" in
                let past = start + String.length tofind in
                if past <= l &&
                   String.sub s start (String.length tofind) = tofind then
                  (if start > 0 then String.sub s 0 start else "") ^
                  !currentFunctionName ^
                  (if past < l then String.sub s past (l - past) else "")
                else
                  s
              with Not_found -> s
            in
            finishCt (CStr(s')) charPtrType
              
        | A.CONST_CHAR s ->
            let chr = 
              if String.length s = 0 then
                Char.chr 0
              else
                String.get s 0
            in
            finishCt (CChr(chr)) (TInt(IChar,[]))
              
        | A.CONST_FLOAT str -> begin
            (* Maybe it ends in U or UL. Strip those *)
            let l = String.length str in
            let hasSuffix = hasSuffix str in
            let baseint, kind = 
              if  hasSuffix "L" then
                String.sub str 0 (l - 1), FLongDouble
              else if hasSuffix "F" then
                String.sub str 0 (l - 1), FFloat
              else if hasSuffix "D" then
                String.sub str 0 (l - 1), FDouble
              else
                str, FDouble
            in
            try
              finishCt (CReal(float_of_string baseint, kind,
                              Some str)) (TFloat(kind,[]))
            with e -> begin
              ignore (E.log "float_of_string %s (%s)\n" str 
                        (Printexc.to_string e));
              finishCt (CStr("booo CONS_FLOAT")) (TPtr(TInt(IChar,[]),[]))
            end
        end
          (* This is not intended to be a constant. It can have expressions 
           * with side-effects inside *)
        | A.CONST_COMPOUND el -> begin
            let slist : stmt list ref = ref [] in
            let doPureExp t e = 
              let (se, e', t') = doExp e (AExp(Some t)) in
              if se <> [] then
                slist := !slist @ se;
              e'
            in
            match what with (* Peek at the expected return type *)
              AExp (Some typ) -> begin
                match unrollType typ with 
                  TStruct(n,flds,_) -> 
                    let rec loopFlds = function
                        [], [] -> []
                      | (f :: flds), (e :: el) -> 
                          (doPureExp f.ftype e) :: loopFlds (flds, el)
                      | (f :: flds), [] -> (* initialize with 0 *)
                          (CastE(f.ftype, integer 0, lu)) :: loopFlds 
                                                               (flds, [])
                      | _ -> E.s (E.unimp 
                                    "Too many initializers for struct %s"  n)
                    in
                    finishExp !slist (Compound(typ, loopFlds (flds, el))) typ
                      
                | TArray(elt,n,a) as oldt -> 
                    let newt = 
                      match n with 
                        None -> TArray(elt, Some(integer (List.length el)),a)
                      | Some _ -> oldt 
                    in
                    finishExp !slist 
                      (Compound(newt, List.map (doPureExp elt) el))
                      newt
                | _ -> E.s (E.unimp "bad initializer type")
              end
            | _ -> E.s (E.unimp "CONST_COMPUND. Not AExp")
        end
    end
          
    | A.TYPE_SIZEOF bt -> 
        let typ = doType [] bt in
        finishExp [] (SizeOf(typ, lu)) uintType
          
    | A.EXPR_SIZEOF e -> 
        let (se, e', t) = doExp e (AExp None) in
        (* !!!! The book says that the expression is not evaluated, so we 
           * drop the potential size-effects *)
        if se <> [] then 
          ignore (E.log "Warning: Dropping side-effect in EXPR_SIZEOF\n");
        let t' = 
          match e' with                 (* If we are taking the sizeof an 
                                         * array we must drop the StartOf  *)
            StartOf(lv) -> typeOfLval lv
          | _ -> t
        in
        finishExp [] (SizeOf(t', lu)) uintType
          
    | A.CAST (bt, e) -> 
        let se1, typ = 
          match bt with
            A.TYPEOF et ->              (* might have side-effects *)
              let (se1, _, typ) = doExp e (AExp None) in
              se1, typ
          | _ -> [],  doType [] bt
        in
        (* We treat the case when e is COMPOUND differently *)
        let what' = 
          match e with 
            A.CONSTANT (A.CONST_COMPOUND _) -> Some typ
          | _ -> None
        in
        let (se, e', t) = doExp e (AExp what') in
        let (t'', e'') = 
          match typ with
            TVoid _ when what = ADrop -> (t, e') (* strange GNU thing *)
          |  _ -> castTo t typ e' 
        in
        finishExp (se1 @ se) e'' t''
          
    | A.UNARY(A.MINUS, e) -> 
        let (se, e', t) = doExp e (AExp None) in
        if isIntegralType t then
          let tres = integralPromotion t in
          let e'' = 
            match e' with
            | Const(CInt(i, _, _), _) -> integer (- i)
            | _ -> UnOp(Neg, doCast e' t tres, tres, lu)
          in
          finishExp se e'' tres
        else
          if isArithmeticType t then
            finishExp se (UnOp(Neg,e',t,lu)) t
          else
            E.s (E.unimp "Unary - on a non-arithmetic type")
        
    | A.UNARY(A.BNOT, e) -> 
        let (se, e', t) = doExp e (AExp None) in
        if isIntegralType t then
          let tres = integralPromotion t in
          let e'' = UnOp(BNot, doCast e' t tres, tres, lu) in
          finishExp se e'' tres
        else
          E.s (E.unimp "Unary ~ on a non-integral type")
          
    | A.UNARY(A.PLUS, e) -> doExp e what 
          
          
    | A.UNARY(A.ADDROF, e) -> begin
        let (se, e', t) = doExp e (AExp None) in
        match e' with 
          Lval x -> finishExp se (AddrOf(x, lu)) (TPtr(t, []))
        | CastE (t', Lval x, _) -> 
            finishExp se (CastE(TPtr(t', []),
                                AddrOf(x, lu), lu)) (TPtr(t', []))
        | StartOf (lv) -> (* !!! is this correct ? *)
            let tres = TPtr(typeOfLval lv, []) in
            finishExp se (AddrOf(lv, lu)) tres

            
        | _ -> E.s (E.unimp "Expected lval for ADDROF. Got %a@!"
                      d_plainexp e')
    end
    | A.UNARY((A.PREINCR|A.PREDECR) as uop, e) -> 
        let uop' = if uop = A.PREINCR then Plus else Minus in
        let (se, e', t) = doExp e (AExp None) in
        let lv = 
          match e' with 
            Lval x -> x
          | CastE (_, Lval x, _) -> x
          | _ -> E.s (E.unimp "Expected lval for ++ or --")
        in
        let tres = checkTypeAdd t intType in
        finishExp (se @ [mkSet lv (BinOp(uop', 
                                         Lval(lv), integer 1, tres, lu))])
          (Lval(lv))
          tres
          
    | A.UNARY((A.POSINCR|A.POSDECR) as uop, e) -> 
      (* If we do not drop the result then we must save the value *)
        let uop' = if uop = A.POSINCR then Plus else Minus in
        let (se, e', t) = doExp e (AExp None) in
        let lv = 
          match e' with 
            Lval x -> x
          | CastE (_, Lval x, _) -> x
          | _ -> E.s (E.unimp "Expected lval for ++ or --")
        in
        let tres = checkTypeAdd t intType in
        let se', result = 
          if what <> ADrop then 
            let tmp = newTempVar t in
            se @ [mkSet (var tmp) (Lval(lv))], Lval(var tmp)
          else
            se, Lval(lv)
        in
        finishExp (se' @ [mkSet lv (BinOp(uop', Lval(lv), 
                                          integer 1, tres, lu))])
          result
          tres
          
    | A.BINARY(A.ASSIGN, e1, e2) -> 
        let (se1, e1', lvt) = doExp e1 (AExp None) in
        let lv, lvt' = 
          match e1' with 
            Lval x -> x, lvt
          | CastE (_, Lval x, _) -> x, typeOfLval x
          | _ -> E.s (E.unimp "Expected lval for assignment. Got %a\n"
                        d_plainexp e1')
        in
        let (se2, e'', t'') = doExp e2 (ASet(lv, lvt')) in
        finishExp (se1 @ se2) (Lval(lv)) lvt'
          
    | A.BINARY((A.ADD|A.SUB|A.MUL|A.DIV|A.MOD|A.BAND|A.BOR|A.XOR|
      A.SHL|A.SHR|A.EQ|A.NE|A.LT|A.GT|A.GE|A.LE) as bop, e1, e2) -> 
        let bop' = match bop with
          A.ADD -> Plus
        | A.SUB -> Minus
        | A.MUL -> Mult
        | A.DIV -> Div
        | A.MOD -> Mod
        | A.BAND -> BAnd
        | A.BOR -> BOr
        | A.XOR -> BXor
        | A.SHL -> Shiftlt
        | A.SHR -> Shiftrt
        | A.EQ -> Eq
        | A.NE -> Ne
        | A.LT -> Lt
        | A.LE -> Le
        | A.GT -> Gt
        | A.GE -> Ge
        | _ -> E.s (E.bug "binary +")
        in
        let (se1, e1', t1) = doExp e1 (AExp None) in
        let (se2, e2', t2) = doExp e2 (AExp None) in
        let tresult, result = doBinOp bop' e1' t1 e2' t2 in
        finishExp (se1 @ se2) result tresult
          
    | A.BINARY((A.ADD_ASSIGN|A.SUB_ASSIGN|A.MUL_ASSIGN|A.DIV_ASSIGN|
      A.MOD_ASSIGN|A.BAND_ASSIGN|A.BOR_ASSIGN|A.SHL_ASSIGN|
      A.SHR_ASSIGN|A.XOR_ASSIGN) as bop, e1, e2) -> 
        let bop' = match bop with          
          A.ADD_ASSIGN -> Plus
        | A.SUB_ASSIGN -> Minus
        | A.MUL_ASSIGN -> Mult
        | A.DIV_ASSIGN -> Div
        | A.MOD_ASSIGN -> Mod
        | A.BAND_ASSIGN -> BAnd
        | A.BOR_ASSIGN -> BOr
        | A.XOR_ASSIGN -> BXor
        | A.SHL_ASSIGN -> Shiftlt
        | A.SHR_ASSIGN -> Shiftrt
        | _ -> E.s (E.bug "binary +=")
        in
        let (se1, e1', t1) = doExp e1 (AExp None) in
        let lv1 = 
          match e1' with Lval x -> x
          | _ -> E.s (E.unimp "Expected lval for assignment")
        in
        let (se2, e2', t2) = doExp e2 (AExp None) in
        let tresult, result = doBinOp bop' e1' t1 e2' t2 in
        finishExp (se1 @ se2 @ [mkSet lv1 result])
          (Lval(lv1))
          tresult
          
    | A.BINARY((A.AND|A.OR), e1, e2) ->
        let tmp = var (newTempVar intType) in
        finishExp (doCondition e [mkSet tmp (integer 1)] 
                                 [mkSet tmp (integer 0)]) 
          (Lval tmp)
          intType
          
    | A.UNARY(A.NOT, e) -> 
        let tmp = var (newTempVar intType) in
        finishExp 
          (doCondition e [mkSet tmp (integer 0)] [mkSet tmp (integer 1)])
          (Lval tmp)
          intType
          
    | A.CALL(f, args) -> 
        let (sf, f', ft') = 
          match f with                  (* Treat the VARIABLE case separate 
                                         * becase we might be calling a 
                                         * function that does not have a 
                                         * prototype. In that case assume it 
                                         * takes INTs as arguments  *)
            A.VARIABLE n -> begin
              try
                let vi = lookup n in
                ([], Lval(var vi), vi.vtype) (* Found. Do not use finishExp. 
                                              * Simulate what = AExp None *)
              with Not_found -> begin
                ignore (E.log 
                          "Warning: Calling function %s without prototype\n"
                          n);
                let ftype = TFun(intType, [], true, []) in
                (* Add a prototype *)
                let proto = makeGlobalVar n ftype in 
                (* let proto = addNewVar proto in
                   theFile := GVar (proto, None) :: !theFile; *)
                ([], Lval(var proto), ftype)
              end
            end
          | _ -> doExp f (AExp None) 
        in
      (* Get the result type and the argument types *)
        let (resType, argTypes, isvar, f'') = 
          match unrollType ft' with
            TFun(rt,at,isvar,a) -> (rt,at,isvar,f')
          | TPtr(TFun(rt,at,isvar,a),_) -> (* Make the function pointer 
                                              * explicit *)
              (rt,at,isvar, mkMem f' NoOffset)
          | x -> E.s (E.unimp 
                        "Unexpected type of the called function %a: %a" 
                        d_exp f' d_type x)
        in
      (* Do the arguments *)
        let rec loopArgs 
            : varinfo list * A.expression list 
          -> (stmt list * exp list) = function
            | ([], []) -> ([], [])
            | (varg :: atypes, a :: args) -> 
                let (sa, a', att) = doExp a (AExp None) in
                let (at'', a'') = castTo att varg.vtype a' in
                let (ss, args') = loopArgs (atypes, args) in
                (sa @ ss, a'' :: args')
                  
            | ([], a :: args) when isvar -> (* No more types *)
                let (sa, a', at) = doExp a (AExp None) in
                let (ss, args') = loopArgs ([], args) in
                (sa @ ss, a' :: args')
            | _ -> E.s (E.unimp 
                          "Too few or too many arguments in call to %a" 
                          d_exp f')
        in
        let (sargs, args') = loopArgs (argTypes, args) in
        begin
          match what with 
            ADrop -> 
              finishExp 
                (sf @ sargs @ [Instr(Call(None,f'',args',lu))])
                (integer 0) intType
              (* Set to a variable of corresponding type *)
          | ASet((Var vi, NoOffset) as lv, vtype) 
              when (typeSig resType = typeSig vtype) -> 
                finishExp 
                  (sf @ sargs @ [Instr(Call(Some vi,f'',args',lu))])
                  (Lval(lv))
                  vtype
          | _ -> begin
              (* Must create a temporary *)
              match f'', args' with     (* Some constant folding *)
                Lval(Var fv, NoOffset), [Const _] 
                  when fv.vname = "__builtin_constant_p" ->
                    finishExp (sf @ sargs) (integer 1) intType
              | _ -> 
                  let tmp = newTempVar resType in
                  let i = Instr(Call(Some tmp,f'',args',lu)) in
                  finishExp (sf @ sargs @ [i]) (Lval(var tmp)) resType
          end
        end
          
    | A.COMMA el -> 
        let rec loop sofar = function
            [e] -> 
              let (se, e', t') = doExp e what in
              finishExp (sofar @ se) e' t'
          | e :: rest -> 
              let (se, e', t') = doExp e ADrop in
              loop (sofar @ se) rest
          | [] -> E.s (E.unimp "empty COMMA expression")
        in
        loop [] el
          
    | A.QUESTION (e1,e2,e3) when what = ADrop -> 
        let (se3,_,_) = doExp e3 ADrop in
        let se2 = 
          match e2 with 
            A.NOTHING -> [Skip]
          | _ -> let (se2,_,_) = doExp e2 ADrop in se2
        in
        finishExp (doCondition e1 se2 se3) (integer 0) intType
          
    | A.QUESTION (e1, e2, e3) -> begin (* what is not ADrop *)
                    (* Do these only to collect the types  *)
        let se2, e2', t2' = 
          match e2 with 
            A.NOTHING -> (* A GNU thing. Use e1 as e2 *) doExp e1 (AExp None)
          | _ -> doExp e2 (AExp None) in 
        let se3, e3', t3' = doExp e3 (AExp None) in
        let tresult = conditionalConversion e2' t2' e3' t3' in
        match e2 with 
          A.NOTHING -> 
              let tmp = var (newTempVar tresult) in
              let (se1, _, _) = doExp e1 (ASet(tmp, tresult)) in
              let (se3, _, _) = doExp e3 (ASet(tmp, tresult)) in
              finishExp (se1 @ [IfThenElse(Lval(tmp), Skip, 
                                           mkSeq se3)])
                (Lval(tmp))
                tresult
        | _ -> 
            if se2 = [] && se3 = [] then begin (* Use the Question *)
              let se1, e1', t1 = doExp e1 (AExp None) in
              ignore (checkBool t1 e1');
              let e2'' = doCast e2' t2' tresult in
              let e3'' = doCast e3' t3' tresult in
              let resexp = 
                match e1' with
                  Const(CInt(i, _, _), _) when i <> 0 -> e2''
                | Const(CInt(0, _, _), _) -> e3''
                | _ -> Question(e1', e2'', e3'', lu)
              in
              finishExp se1 resexp tresult
            end else (* Use a conditional *)
              let lv, lvt = 
                match what with
                | ASet (lv, lvt) -> lv, lvt
                | _ -> 
                    let tmp = newTempVar tresult in
                    var tmp, tresult
              in
              (* Now do e2 and e3 for real *)
              let (se2, _, _) = doExp e2 (ASet(lv, lvt)) in
              let (se3, _, _) = doExp e3 (ASet(lv, lvt)) in
              finishExp (doCondition e1 se2 se3) (Lval(lv)) tresult
    end
(*              


    | A.QUESTION (e1, e2, e3) -> begin (* what is not ADrop *)
        let se3, lv, tlv = 
          let (se3,e3',t3') = doExp e3 what in (* e2 might be NOTHING *)
          match what with 
            ASet (lv, tlv) -> se3, lv, tlv
          | AExp _ -> begin 
              (* Get the place where e3 was placed *) 
              match e3' with 
                Lval(lv) -> se3, lv, t3'
              | _ -> 
                  let tmp = var (newTempVar t3') in
                  se3 @ [mkSet tmp e3'], tmp, t3'
          end
          | ADrop -> E.s (E.bug "question")
        in
        let stats = 
          if e2 = A.NOTHING then (* A GNU C thing *)
            (* Now store e1 *)
            let (se1, _, _) = doExp e1 (ASet(lv, tlv)) in
            se1 @ [IfThenElse(Lval(lv), Skip, mkSeq se3)]
          else
            let (se2,_,_) = doExp e2 (ASet(lv,tlv)) in
            doCondition e1 se2 se3
        in
        (* Do some constant folding *)
        match stats, what with
          [Instr(Set(lv', e', _))], AExp _ when lv' == lv -> 
            finishExp [] e' tlv
        | _, _ -> finishExp stats (Lval(lv)) tlv
    end
*)

    | A.GNU_BODY b -> begin
        (* Find the last A.COMPUTATION *)
        let rec findFirstStm = function
            A.BSTM s :: _  -> 
              let rec findLast = function
                  A.SEQUENCE (_, s) -> findLast s
                | (A.COMPUTATION _) as s -> s
                | _ -> E.s (E.unimp "Cannot find COMPUTATION in GNU_BODY\n")
              in
              findLast s
          | _ :: rest -> findFirstStm rest
          | [] -> E.s (E.unimp "Cannot find COMPUTATION in GNU_BODY\n")
        in
        (* Save the previous data *)
        let old_gnu = ! gnu_body_result in
        let lastComp = findFirstStm (List.rev b) in
        (* Prepare some data to be filled by doExp *)
        let data : (exp * typ) option ref = ref None in
        gnu_body_result := (lastComp, data);
        let se = doBody b in
        gnu_body_result := old_gnu;
        match !data with
          None -> E.s (E.unimp "GNU_BODY did not end with a COMPUTATION")
        | Some (e, t) -> finishExp se e t
    end
  with e -> begin
    ignore (E.log "error in doExp (%s)@!" (Printexc.to_string e));
    ([dStmt (dprintf "booo_exp(%s)" (Printexc.to_string e))], 
     integer 0, intType)
  end
    
and doBinOp (bop: binop) (e1: exp) (t1: typ) (e2: exp) (t2: typ) : typ * exp =
  let constFold e1' e2' tres = 
    if isIntegralType tres then
      let newe = 
        let rec mkInt = function
            Const(CChr c, _) -> Const(CInt(Char.code c, IInt, None),lu)
          | CastE(TInt _, e, _) -> mkInt e
          | e -> e
        in
        match bop, mkInt e1', mkInt e2' with
          Plus, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 + i2)
        | Minus, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 - i2)
        | Mult, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 * i2)
        | Div, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 / i2)
        | Mod, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 mod i2)
        | BAnd, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 land i2)
        | BOr, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 lor i2)
        | BXor, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 lxor i2)
        | Shiftlt, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 lsl i2)
        | Shiftrt, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (i1 lsr i2)
        | Eq, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (if i1 = i2 then 1 else 0)
        | Ne, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (if i1 <> i2 then 1 else 0)
        | Le, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (if i1 <= i2 then 1 else 0)
        | Ge, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (if i1 >= i2 then 1 else 0)
        | Lt, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (if i1 < i2 then 1 else 0)
        | Gt, Const(CInt(i1,_,_),_),Const(CInt(i2,_,_),_) -> 
            integer (if i1 > i2 then 1 else 0)
        | _ -> BinOp(bop, e1', e2', tres, lu)
      in
      tres, newe
    else
      tres, BinOp(bop, e1', e2', tres, lu)
  in
  let doArithmetic () = 
    let tres = arithmeticConversion t1 t2 in
    constFold (doCast e1 t1 tres) (doCast e2 t2 tres) tres
  in
  let doArithmeticComp () = 
    let tres = arithmeticConversion t1 t2 in
    constFold (doCast e1 t1 tres) (doCast e2 t2 tres) intType
  in
  let doIntegralArithmetic () = 
    let tres = unrollType (arithmeticConversion t1 t2) in
    match tres with
      TInt _ -> constFold (doCast e1 t1 tres) (doCast e2 t2 tres) tres
    | _ -> E.s (E.unimp "%a operator on a non-integer type" d_binop bop)
  in
  match bop with
    (Mult|Div) -> doArithmetic ()
  | (Mod|BAnd|BOr|BXor|Shiftlt|Shiftrt) -> doIntegralArithmetic ()
  | (Plus|Minus) 
      when isArithmeticType t1 && isArithmeticType t2 -> doArithmetic ()
  | (Eq|Ne|Lt|Le|Ge|Gt) 
      when isArithmeticType t1 && isArithmeticType t2 -> doArithmeticComp ()
  | Plus when isPointerType t1 && isIntegralType t2 -> 
      constFold e1 (doCast e2 t2 (integralPromotion t2)) t1
  | Plus when isIntegralType t1 && isPointerType t2 -> 
      constFold (doCast e1 t1 (integralPromotion t1)) e2 t2
  | Minus when isPointerType t1 && isIntegralType t2 -> 
      constFold e1 (doCast e2 t2 (integralPromotion t2)) t1
  | (Minus|Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isPointerType t2 ->
      constFold e1 e2 intType
  | (Eq|Ne) when isPointerType t1 && 
                 (match e2 with Const(CInt(0,_,_),_) -> true | _ -> false) -> 
      constFold e1 (doCast e2 t2 t1) intType
  | (Eq|Ne) when isPointerType t2 && 
                 (match e1 with Const(CInt(0,_,_),_) -> true | _ -> false) -> 
      constFold (doCast e1 t1 t2) e2 intType
  | _ -> E.s (E.unimp "doBinOp: %a\n" d_plainexp (BinOp(bop,e1,e2,intType,lu)))

(* A special case for conditionals *)
and doCondition (e: A.expression) 
                (st: stmt list)
                (sf: stmt list) : stmt list = 
  let canDuplicate sl = (* We can duplicate a statement if it is small and 
                        * does not contain label definitions  *)
    let rec costOne = function
        Skip -> 0
      | Sequence sl -> costMany sl
      | Loop stmt -> 100
      | IfThenElse (_, _, _) -> 100
      | Label _ -> 10000
      | Switch _ -> 100
      | (Goto _|Return _|Case _|Default|Break|Continue|Instr _) -> 1
    and costMany sl = List.fold_left (fun acc s -> acc + costOne s) 0 sl
    in
    costMany sl <= 3
  in 
  let canDrop sl = (* We can drop a statement only if it does not contain 
                    * label definitions *)
    let rec dropOne = function
        (Skip | Goto _ | Return _| Case _ | Default | 
        Break | Continue | Instr _) -> true
      | Sequence sl -> List.for_all dropOne sl
      | _ -> false
    in
    List.for_all dropOne sl
  in
  match e with 
  | A.BINARY(A.AND, e1, e2) ->
      let (sf1, sf2) = 
        (* If sf is small then will copy it *)
        if canDuplicate sf then
          (sf, sf)
        else begin
          incr labelId;
          let lab = "L" ^ (string_of_int !labelId) in
          ([Goto lab], Label lab :: sf)
        end
      in
      let st' = doCondition e2 st sf1 in
      let sf' = sf2 in
      doCondition e1 st' sf'

  | A.BINARY(A.OR, e1, e2) ->
      let (st1, st2) = 
        (* If st is small then will copy it *)
        if canDuplicate st then
          (st, st)
        else begin
          incr labelId;
          let lab = "L" ^ (string_of_int !labelId) in
          ([Goto lab], Label lab :: st)
        end
      in
      let st' = st1 in
      let sf' = doCondition e2 st2 sf in
      doCondition e1 st' sf'

  | A.UNARY(A.NOT, e) -> doCondition e sf st

  | _ -> begin
      let (se, e, t) as rese = doExp e (AExp None) in
      ignore (checkBool t e);
      match e with 
        Const(CInt(i,_,_),_) when i <> 0 && canDrop sf -> se @ st
      | Const(CInt(0,_,_),_) when canDrop st -> se @ sf
      | _ -> se @ [IfThenElse(e, mkSeq st, mkSeq sf)]
  end

and checkTypeAdd t1 t2 = 
  match unrollType t1, unrollType t2 with
    TInt _, TInt _ -> t1
  | TPtr _, TInt _ -> t1
  | TInt _, TPtr _ -> t2
  | TFloat _, TFloat _ -> t1
  | TFloat _, TInt _ -> t1
  | TInt _, TFloat _ -> t2
  | _ -> E.s (E.unimp "checkTypeAdd %a + %a\n" d_type t1 d_type t2)


and doPureExp (e : A.expression) : exp = 
  let (se, e', _) = doExp e (AExp None) in
  if se <> [] then
   E.s (E.unimp "doPureExp: not pure");
  e'


and doDecl : A.definition -> stmt list = function
  | A.DECDEF ng ->
      let createLocal
          ((bt,st,(n,nbt,a,e)) as sname : A.single_name) 
          : stmt list = 
        let vi = makeVarInfo false locUnknown sname in
        let vi = addNewVar vi in        (* Replace vi *)
        if e = A.NOTHING then
          [Skip]
        else
          let (se, e', et) = doExp e (AExp (Some vi.vtype)) in
          (match vi.vtype, et with (* We have a length now *)
            TArray(_,None, _), TArray(_, Some _, _) -> vi.vtype <- et
          | _, _ -> ());
          let (_, e'') = castTo et vi.vtype e' in
          se @ (doAssign (Var vi, NoOffset) e'')
      in
      let stmts = doNameGroup createLocal ng in
      List.concat stmts

  | _ -> E.s (E.unimp "doDecl")
    

and doAssign (lv: lval) : exp -> stmt list = function   
                             (* We must break the compound assignment into 
                              * atomic ones  *)
  | Compound (t, el) -> begin
      match unrollType t with 
        TArray(t, _, _) -> 
          let rec loop = function
              _, [] -> []
            | i, e :: el -> 
                let res = loop ((i + 1), el) in
                let newlv = mkMem (Lval(lv)) (Index(integer i, NoOffset)) in
                let newlv = 
                  match newlv with 
                    Lval x -> x | _ -> E.s (E.bug "doAssign: mem")
                in
                (doAssign newlv e) @ res
          in
          loop (0, el)

      | TStruct(_, fil, _) ->  
          let rec loop = function
              [], [] -> []
            | f :: fil, e :: el -> 
                let res = loop (fil, el) in
                (doAssign (addOffset (Field(f, NoOffset)) lv) e) @ res
            | _, _ -> E.s (E.unimp "fields in doAssign")
          in
          loop (fil, el)
      | _ -> E.s (E.bug "Unexpected type of Compound")
  end

  | e -> [Instr(Set(lv, e, lu))]

  (* Now define the processors for body and statement *)
and doBody (b : A.body) : stmt list = 
  startScope ();
    (* Do the declarations and the initializers and the statements. *)
  let rec loop = function
      [] -> []
    | A.BDEF d :: rest -> 
        let res = doDecl d in  (* !!! @ eveluates its arguments backwards *)
        res @ loop rest
    | A.BSTM s :: rest -> 
        let res = doStatement s in
        res @ loop rest
  in
  let res = loop b in
  exitScope ();
  res
      
and doStatement (s : A.statement) : stmt list = 
  try
    match s with 
      A.NOP -> [Skip]
    | A.COMPUTATION e -> 
        let (lasts, data) = !gnu_body_result in
        if lasts == s then begin      (* This is the last in a GNU_BODY *)
          let (s', e', t') = doExp e (AExp None) in
          data := Some (e', t');      (* Record the result *)
          s'
        end else
          let (s', _, _) = doExp e ADrop in
            (* drop the side-effect free expression *)
          s'
            
    | A.BLOCK b -> doBody b
    | A.SEQUENCE (s1, s2) -> 
        (doStatement s1) @ (doStatement s2)
    | A.IF(e,st,sf) -> 
        doCondition e (doStatement st) (doStatement sf)
    | A.WHILE(e,s) ->  
        startLoop true;
        let s' = doStatement s in
        exitLoop ();
        [Loop(mkSeq ((doCondition e [Skip] [Break]) @ s'))]
          
    | A.DOWHILE(e,s) -> 
        startLoop false;
        let s' = doStatement s in
        let s'' = labContinue () :: (doCondition e [Skip] [Break])
        in
        exitLoop ();
        [Loop(mkSeq (s' @ s''))]
          
    | A.FOR(e1,e2,e3,s) -> begin
        let (se1, _, _) = doExp e1 ADrop in
        let (se3, _, _) = doExp e3 ADrop in
        startLoop false;
        let s' = doStatement s in
        let s'' = labContinue () :: se3 in
        exitLoop ();
        match e2 with
          A.NOTHING -> (* This means true *)
            se1 @ [Loop(mkSeq (s' @ s''))]
        | _ -> 
            se1 @ [Loop(mkSeq ((doCondition e2 [Skip] [Break])
                               @ s' @ s''))]
    end
    | A.BREAK -> [Break]
          
    | A.CONTINUE -> [doContinue ()]
          
    | A.RETURN A.NOTHING -> [Return None]
    | A.RETURN e -> 
        let (se, e', et) = doExp e (AExp None) in
        let (et'', e'') = castTo et (!currentReturnType) e' in
        se @ [Return (Some e'')]
               
    | A.SWITCH (e, s) -> 
        let (se, e', et) = doExp e (AExp None) in
        let (et'', e'') = castTo et (TInt(IInt,[])) e' in
        let s' = doStatement s in
        se @ [Switch (e'', mkSeq s')]
               
    | A.CASE (e, s) -> 
        let (se, e', et) = doExp e (AExp None) in
          (* let (et'', e'') = castTo et (TInt(IInt,[])) e' in *)
        let i = 
          match se, e' with
            [], Const (CInt (i,_, _), _) -> i
          | [], Const (CChr c, _) -> Char.code c
          | _ -> E.s (E.unimp "non-int case")
        in
        Case i :: (doStatement s)
                    
    | A.DEFAULT s -> 
        Default :: (doStatement s)
                     
    | A.LABEL (l, s) -> 
        Label l :: (doStatement s)
                     
    | A.GOTO l -> 
        [Goto l]
          
    | A.ASM (tmpls, isvol, outs, ins, clobs) -> 
      (* Make sure all the outs are variables *)
        let temps : (lval * varinfo) list ref = ref [] in
        let stmts : stmt list list ref = ref [] in
        let outs' = 
          List.map 
            (fun (c, e) -> 
              let (se, e', t) = doExp e (AExp None) in
              let lv = 
                match e' with Lval x -> x
                | _ -> E.s (E.unimp "Expected lval for ASM outputs")
              in
              stmts := se :: !stmts;
              (c, lv)) outs 
        in
      (* Get the side-effects out of expressions *)
        let ins' = 
          List.map 
            (fun (c, e) -> 
              let (se, e', et) = doExp e (AExp None) in
              stmts := se :: !stmts;
              (c, e'))
            ins
        in
        List.concat (List.rev !stmts) @ 
        [Instr(Asm(tmpls, isvol, outs', ins', clobs))]
  with e -> begin
    (ignore (E.log "Error in doStatement (%s)\n" (Printexc.to_string e)));
    [Label "booo_statement"]
  end


    
(* Translate a file *)
let convFile dl = 
  ignore (E.log "Cabs2cil conversion\n");
  (* Clean up the global types *)
  theFile := [];
  H.clear typedefs;
  H.clear enumFields;
  (* Setup the built-ints *)
  ignore (addNewVar { vname = "__builtin_constant_p";
                      vglob = true;
                      vid   = newVarId "__builtin_constant_p" true;
                      vtype = TFun(intType, 
                                   [{ vname = "x";
                                      vglob = false;
                                      vtype = intType;
                                      vid      = 0;(* The first local*)
                                        vdecl    = lu;
                                      vstorage = NoStorage;
                                      vaddrof = false;
                                      vattr = [];
                                    } ], false, []);
                      vstorage = NoStorage;
                      vattr = [];
                      vaddrof = false;
                      vdecl  = lu;
                    });
  (* Now do the globals *)
  let doOneGlobal = function
      A.TYPEDEF ng -> 
        let createTypedef ((_,_,(n,nbt,a,_)) : A.single_name) = 
          try
            let newTyp = doType (List.map doAttr a) nbt in
          (* Register the type *)
            recordTypeName n newTyp;
            theFile := GType (n, newTyp) :: !theFile
          with e -> begin
            ignore (E.log "Error on A.TYPEDEF (%s)\n"
                      (Printexc.to_string e));
            theFile := GAsm ("booo_typedef:" ^ n) :: !theFile
          end
        in
        ignore (doNameGroup createTypedef ng)

    | A.ONLYTYPEDEF (A.NO_TYPE, _, _) -> ()

    | A.ONLYTYPEDEF (bt,_,_) -> begin
        try
          let newTyp = doType [] bt in
        (* doType will register the type. Put a special GType in the file *)
          theFile := GType ("", newTyp) :: !theFile
        with e -> begin
          ignore (E.log "Error on A.ONLYTYPEDEF (%s)\n"
                    (Printexc.to_string e));
          theFile := GAsm ("booo_typedef") :: !theFile
        end
    end
    | A.DECDEF ng -> 
        let createGlobal ((_,_,(n,nbt,a,e)) as sname : A.single_name) =
          begin
            try
              let vi = makeVarInfo true locUnknown sname in
              let vi = addNewVar vi in    (* Overwrite vi *)
              let init, vi' = 
                if e = A.NOTHING then 
                  None, vi
                else 
                  let (se, e', et) = doExp e (AExp (Some vi.vtype)) in
                  let (_, e'') = castTo et vi.vtype e' in
                  (match et with (* We have a length now *)
                    TArray(_, Some _, _) -> vi.vtype <- et
                  | _ -> ());
                  if se <> [] then 
                    E.s (E.unimp "global initializer");
                  Some e'', vi
              in
              theFile := GVar(vi', init) :: !theFile
            with e -> begin
              ignore (E.log "error in CollectGlobal (%s)\n" 
                        (Printexc.to_string e));
              theFile := GAsm("booo - error in global " ^ n) :: !theFile
            end
          end
(*
          ignore (E.log "Env after processing global %s is:@!%t@!" 
                    n docEnv);
          ignore (E.log "Alpha after processing global %s is:@!%t@!" 
                    n docAlphaTable)
*)
        in
        ignore (doNameGroup createGlobal ng)
          
    | A.GLOBASM s -> theFile := GAsm s :: !theFile
    | A.PRAGMA s -> theFile := GPragma s :: !theFile

    | A.FUNDEF (((bt,st,(n,bt',funattr,_)) : A.single_name), 
                 (body : A.body)) -> 
        begin
          try
           (* Reset the local identifier so that formals are created with the 
            * proper IDs  *)
            resetLocals ();
           (* See if this is inline 
            let isinline, funattr' = 
              match List.partition 
                     (function ("inline", []) -> true 
                              | _ -> false) funattr with
                [], _ -> false, funattr
              | _, rest -> true, rest
            in *)
                                        (* Do the type *)
            let (returnType, formals, isvararg, a) = 
              match unrollType (doType [] bt') with 
                TFun(rType, formals, isvararg, a) -> 
                  (rType, formals, isvararg, a)
              | x -> E.s (E.bug "non-function type: %a." d_type x)
            in
            (* Record the returnType for doStatement *)
            currentFunctionName := n;
            currentReturnType   := returnType;
            (* Setup the environment. Add the formals to the locals. Maybe 
             * they need alpha-conv *)
            startScope ();
            let formals' = List.map addNewVar formals in
            let ftype = TFun(returnType, formals', isvararg, a) in
            let fattr = List.map doAttr funattr in
            let fstorage = doStorage st in
            (* Add the function itself to the environment. Just in case we 
             * have recursion and no prototype.  *)
            (* Make a variable out of it and put it in the environment *)
            let thisFunctionVI = 
              { vname = n;
                vtype = ftype;
                vglob = true;
                vid   = newVarId n true;
                vdecl = lu;
                vattr = fattr;
                vaddrof = false;
                vstorage = fstorage;
              } 
            in
            ignore (addNewVar thisFunctionVI);
            (* Now do the body *)
            let s = doBody body in
            (* Finish everything *)
            exitScope ();
            let (nrlocals, locals) = endFunction () in
            let fdec = { svar      = thisFunctionVI;
                         slocals  = locals;
                         smaxid   = nrlocals;
                         sbody    = (match mkSeq s with (Sequence _) as x -> x 
                                     | x -> Sequence [x]);
                       } 
            in
            (* Fix the vaddrof flag *)
            let fixAddrExp = function
                AddrOf ((Var vi, _), _) -> vi.vaddrof <- true
              | StartOf (Var vi, _) -> vi.vaddrof <- true
              | _ -> ()
            in
            iterExp fixAddrExp fdec.sbody;
            theFile := GFun fdec :: !theFile
          with e -> begin
            ignore (E.log "error in collectFunction %s: %s\n" 
                      n (Printexc.to_string e));
            theFile := GAsm("error in function ") :: !theFile
          end
        end
    | A.OLDFUNDEF _ -> E.s (E.unimp "OLDFUNDEF")
  in
  List.iter doOneGlobal dl;
  (* We are done *)
  List.rev (! theFile)


