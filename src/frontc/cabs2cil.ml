(* Convert CABS to CIL *)
module A = Cabs
module E = Errormsg
module H = Hashtbl

open Pretty
open Cil


(*** Helper ***)
let var vi = Var(vi,NoOffset,locUnknown)
let lu = locUnknown
let integer n = Const(CInt(n, None), lu)
let mkSet lv e = Instruction(Set(lv,e,lu))
let intType = TInt(IInt,[])
let assign vi e = mkSet (var vi) e

let mkSeq sl = 
  let rec removeSkip = function 
      [] -> []
    | Skip :: rest -> removeSkip rest
    | Sequence (sl) :: rest -> removeSkip (sl @ rest)
    | s :: rest -> s :: removeSkip rest
  in
  match removeSkip sl with 
    [] -> Skip
  | [s] -> s
  | sl' -> Sequence(sl')


(*** EXPRESSIONS *************)
(*** As we translate expressions we need an environment ***)
type enventry = 
  | EBlock                              (* start of a block *)
  | EVar of string * varinfo            (* a variable name and its associated 
                                         * varinfo *)

(*** In order to process GNU_BODY expressions we must record that a given 
 *** COMPUTATION is interesting *)
let gnu_body_result : (A.statement * ((exp * typ) option ref)) ref 
    = ref (A.NOP, ref None)

(*** When we do statements we need to know the current return type *)
let currentReturnType : typ ref = ref (TVoid([]))

let env : enventry list ref = ref []    (* This is the environment *)

(* Lookup a variable name *)
let lookup n = 
  let rec loop = function
      [] -> raise Not_found
    | EVar (n', vi) :: _ when n = n' -> vi
    | _ :: rest -> loop rest
  in
  loop (!env)



(* Add a local declaration. Alpha convert in the process *)
let localId = ref (-1)
let locals : varinfo list ref = ref []

let alphaId = ref (-1)                  (* Indices for alpha-conversion *)
let newVarId name isglobal = 
  if isglobal then 
    H.hash name
  else begin
    incr localId;
    !localId
  end

let resetLocals () = 
  localId := -1; locals := []

let getLocals () = 
  (!localId + 1, List.rev !locals)

let addNewVar vi = 
  let origname = vi.vname in
  let (isnew, newvi) = 
    try begin
      let oldvi = lookup vi.vname in
      (* The variable already exists *)
      (* ignore  (E.log "addNewVar.New=%a vglob=%b, old=%a vglob=%b\n"
                 d_videcl vi vi.vglob d_videcl oldvi oldvi.vglob);
         *)
      if vi.vglob && oldvi.vglob then
        if oldvi.vstorage = Extern  && vi.vstorage = NoStorage then begin
          oldvi.vstorage <- NoStorage;
          (false, oldvi)
        end else
          if vi.vstorage = Extern && (oldvi.vstorage = NoStorage ||
                                      oldvi.vstorage = Extern) then
            (false, oldvi)
          else
            match vi.vtype, oldvi.vtype with
                                        (* function prototypes are not 
                                         * definitions *)
              TFun _, TFun _ -> (false, oldvi)
            | _ -> E.s (E.unimp "Variable redefinition: %s\n" vi.vname)
      else begin
        incr alphaId;
        let newname = origname ^ (string_of_int (!alphaId)) in
        ignore (E.log "Alpha-converting %s to %s\n" origname newname);
        (true, {vi with vname = newname})
      end
    end with Not_found -> (true, vi)
  in
  if isnew then begin
    env := EVar (origname, newvi) :: !env;
    if not newvi.vglob then
      locals := newvi :: !locals
  end;
  newvi


(* Create a new temporary variable *)
let newTempVar typ = 
  incr localId;
  addNewVar 
    { vname = "tmp" ^ (string_of_int (!localId));
      vid   = !localId;
      vglob = false;
      vtype = typ;
      vdecl = locUnknown;
      vattr = [];
      vaddrof = false;
      vstorage = NoStorage;
    } 

(* Start a block *)
let startBlock () = 
  env := EBlock :: !env

(* Exit a block *)
let endBlock () = 
  let rec loop = function
      [] -> E.s (E.bug "cannot find block start")
    | EBlock :: rest -> env := rest
    | _ :: rest -> loop rest
  in
  loop (!env)



(******** GLOBAL TYPES **************)
let typedefs : (string, typ) H.t = H.create 113

   (* Keep a list of unresolved types. Maybe they are forward references *)
let unresolvedTypes : (string * typ ref) list ref = ref []

let findType n = 
  try
    H.find typedefs n
  with Not_found -> begin
    let typref = ref (TVoid([])) in     (* To be patched later *)
    unresolvedTypes := (n, typref) :: !unresolvedTypes;
    Typedef (n, 0, typref, [])
  end

let recordType n t = 
  H.add typedefs n t;
  (* Resolve some unresolved types *)
  let rec loop still = function
      [] -> unresolvedTypes := still
    | (n', tref) :: rest when n = n' -> begin
        tref := t;
        loop still rest
    end
    | x :: rest -> loop (x :: still) rest
  in
  loop [] !unresolvedTypes

let typeId : int ref = ref 0
let newTypeId () = 
  incr typeId;
  !typeId

let newTypeName n = 
  incr typeId;
  n ^ (string_of_int (!typeId))

(**** Occasionally we see structs with no name and no fields *)
let typeEmptyAnonymousStruct = 
  TStruct ("", [], 0, [])

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
let rec castTo (ot : typ) (nt : typ) (e : exp) : (typ * exp ) = 
  if ot = nt then (ot, e) else
  match ot, nt with
    Typedef(_,_,r,_), _ -> castTo !r nt e
  | _, Typedef(_,_,r,_) -> castTo ot !r e
  | TInt(ikindo,_), TInt(ikindn,_) -> 
      (nt, if ikindo == ikindn then e else CastE(nt, e, lu))

  | TPtr (told, _), TPtr(tnew, _) -> (nt, CastE(nt, e,lu))

  | TInt _, TPtr _ when
      (match e with Const(CInt(0,_),_) -> true | _ -> false) -> 
        (nt, (CastE(nt, e, lu)))

  | TInt _, TPtr _ -> (nt, (CastE(nt,e,lu)))

  | TPtr _, TInt _ -> (nt, (CastE(nt,e,lu)))

  | TArray _, TPtr _ -> (nt, (CastE(nt,e,lu)))

  | TArray(t1,_,_), TArray(t2,None,_) when t1 == t2 -> (nt, e)

  | TPtr _, TArray(_,None,_) -> (nt, e)

  | TEnum _, TInt _ -> (nt, e)

  | TFun _, TPtr(TFun _, _) -> (nt, e)

  | TInt _, TFloat _ -> (nt, (CastE(nt,e,lu)))

  | TFloat _, TFloat _ -> (nt, (CastE(nt,e,lu)))

  | TBitfield _, TInt _ -> (nt, e)

  | TInt _, TEnum _ -> (intType, e)

  | TEnum _, TEnum _ -> (intType, e)

  | _ -> E.s (E.unimp "castTo %a -> %a@!" d_type ot d_type nt)

(* A cast that is used for conditional expressions. Pointers are Ok *)
let checkBool (ot : typ) (e : exp) : bool =
  match unrollType ot with
    TInt _ -> true
  | TPtr _ -> true
  | TEnum _ -> true
  |  _ -> E.s (E.unimp "castToBool %a" d_type ot)



(* Do types *)
(* Process a name group *)
let doNameGroup (sng: A.single_name -> 'a) 
                ((bt,s,nl) : A.name_group) : 'a list =
  List.map (fun n -> sng (bt,s,n)) nl


let rec makeFieldInfo (host: string) 
                     ((bt,st,(n,nbt,a,e)) : A.single_name) : fieldinfo = 
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
    A.NO_TYPE -> E.s (E.unimp "NO_TYPE")
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
  | A.BITFIELD (sgn, e) -> 
      let ikind = 
        match sgn with
          (A.NO_SIGN | A.SIGNED) -> IInt
        | A.UNSIGNED -> IUInt
      in
      let width = match doExp e (AExp None) with
        ([], Const(CInt(i,_),_), _) -> i
      | _ -> E.s (E.unimp "bitfield width is not an integer")
      in
      TBitfield (ikind, width, a)
  | A.FLOAT lng -> TFloat ((if lng then FFloat else FDouble), a)
  | A.DOUBLE lng -> TFloat ((if lng then FDouble else FLongDouble), a)
  | A.PTR bt -> TPtr (doType [] bt, a)
  | A.ARRAY (bt, len) ->
      let lo = 
        match len with 
          A.NOTHING -> None 
        | _ -> Some (doPureExp len)
      in
      TArray (doType [] bt, lo, a)
  | A.STRUCT (n, nglist) -> 
      let flds = 
        List.concat (List.map (doNameGroup (makeFieldInfo n)) nglist) in
      if flds <> [] then begin
                                        (* This introduces a new type *)
        let tp = TStruct (n, flds, newTypeId (), a) in
        if n <> "" then recordType ("struct " ^ n) tp; 
        tp
      end else begin
        if n = "" then 
          typeEmptyAnonymousStruct
        else
          findType ("struct " ^ n)
      end
  | A.UNION (n, nglist) -> 
      let flds = 
        List.concat (List.map (doNameGroup (makeFieldInfo n)) nglist) in
      if flds <> [] then begin
                                        (* This introduces a new type *)
        let tp = TUnion (n, flds, newTypeId (), a) in
        if n <> "" then recordType ("union " ^ n) tp; 
        tp
      end else begin
        if n = "" then E.s (E.unimp "Type with no fields and no name");
        findType ("union " ^ n)
      end
  | A.PROTO (bt, snlist, isvararg) ->
      TFun (doType [] bt, 
            List.map (makeVarInfo false locUnknown) snlist,
            isvararg, 
            a)
  | A.OLD_PROTO _ -> E.s (E.unimp "oldproto")
  | A.ENUM (n, eil) -> 
      let rec loop i = function
          [] -> []
        | (kname, A.NOTHING) :: rest -> 
            recordEnumField kname i intType;
            (kname, i) :: loop (i + 1) rest

        | (kname, e) :: rest ->
            let i = 
              match doExp e (AExp None) with
                [], Const(CInt(i,_),_), _ -> i
              | [], UnOp(Neg,Const(CInt(i,_), _),_,_), _ -> - i
              | _ -> E.s (E.unimp "enum with initializer")
            in
            recordEnumField kname i intType;
            (kname, i) :: loop (i + 1) rest
      in
      let fields = loop 0 eil in
      let res = TEnum (n, loop 0 eil, newTypeId (), a) in
      List.iter (fun (n,fieldidx) -> recordEnumField n fieldidx res) fields;
      res

  | A.CONST bt -> doType (AId("const") :: a) bt
  | A.VOLATILE bt -> doType (AId("volatile") :: a) bt
  | A.NAMED_TYPE n -> begin
      match findType n with
        (Typedef _) as x -> x
      | typ -> Typedef(n, 0, ref typ, [])
  end
  | A.TYPEOF e -> 
      let (se, _, t) = doExp e (AExp None) in
      if se <> [] then
        E.s (E.unimp "typeof for a non-pure expression\n");
      t

  
     (* Process an expression and in the process do some type checking, 
      * extract the effects as separate statements  *)
and doExp (e : A.expression) (what: expAction) : (stmt list * exp * typ) = 
  let finishExp se e t = 
    match what with 
      ADrop -> (se, e, t)
    | AExp _ -> (se, e, t)
    | ASet (lv, lvt) -> 
        let (t', e') = castTo t lvt e in
        (se @ [mkSet lv e'], integer 0, intType)
  in
  let wrapLval e = 
    let (se, lv, t) = doLval e in
    finishExp se (Lval(lv)) t
  in
  try
    match e with
    | A.NOTHING when what = ADrop -> finishExp [] (integer 0) intType
    | A.NOTHING ->
        ignore (E.log "doExp nothing\n");
        finishExp [] (Const(CStr("exp_nothing"),lu)) (TPtr(TInt(IChar,[]),[]))
          
    | A.VARIABLE n -> begin
        try wrapLval e 
        with Not_found -> 
          try 
            let (idx, typ) = lookupEnumField n in
          (* Maybe it is an enum field *)
            finishExp [] (integer idx) typ
          with Not_found -> begin
            ignore (E.log "Cannot resolve variable %s\n" n);
            raise Not_found
          end
    end
    | A.MEMBEROFPTR _ -> wrapLval e
    | A.MEMBEROF _ -> wrapLval e
    | A.UNARY(A.MEMOF, _) -> wrapLval e
    | A.INDEX _ -> wrapLval e
          
    | A.CONSTANT ct -> begin
        let finishCt c t = finishExp [] (Const(c, lu)) t in
        match ct with 
          A.CONST_INT str -> begin
            (* Maybe it ends in U or UL. Strip those *)
            let l = String.length str in
            let baseint, kind = 
              if      l >= 3 && String.sub str (l - 3) 3 = "ULL" then
                String.sub str 0 (l - 3), IULongLong
              else if l >= 2 && String.sub str (l - 2) 2 = "LL" then
                String.sub str 0 (l - 2), ILongLong
              else if l >= 2 && String.sub str (l - 2) 2 = "UL" then
                String.sub str 0 (l - 2), IULong
              else if l >= 1 && String.sub str (l - 1) 1 = "L" then
                String.sub str 0 (l - 1), ILong
              else if l >= 1 && String.sub str (l - 1) 1 = "U" then
                String.sub str 0 (l - 1), IUInt
              else
                str, IInt
            in
            try
              finishCt (CInt(int_of_string baseint, Some str)) (TInt(kind,[]))
            with e -> begin
              ignore (E.log "int_of_string %s (%s)\n" str 
                        (Printexc.to_string e));
              finishCt (CStr("booo CONS_INT")) (TPtr(TInt(IChar,[]),[]))
            end
          end
        | A.CONST_STRING s -> 
            finishCt (CStr(s)) (TPtr(TInt(IChar,[]),[]))

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
            let baseint, kind = 
              if      l >= 2 && String.sub str (l - 2) 2 = "LD" then
                String.sub str 0 (l - 2), FLongDouble
              else if l >= 1 && String.sub str (l - 1) 1 = "F" then
                String.sub str 0 (l - 1), FFloat
              else if l >= 1 && String.sub str (l - 1) 1 = "D" then
                String.sub str 0 (l - 1), FDouble
              else
                str, FFloat
            in
            try
              finishCt (CReal(float_of_string baseint, 
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
            match what with 
              AExp (Some typ) -> begin
                match unrollType typ with 
                  TStruct(n,flds,_,_) -> 
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
        finishExp [] (sizeOf typ) intType
          
    | A.EXPR_SIZEOF e -> 
        let (se, e', t) = doExp e (AExp None) in
        finishExp se (sizeOf t) intType
          
    | A.CAST (bt, e) -> 
        let se1, typ = 
          match bt with
            A.TYPEOF et ->              (* might have side-effects *)
              let (se1, _, typ) = doExp e (AExp None) in
              se1, typ
          | _ -> [],  doType [] bt
        in
        let (se, e', t) = doExp e (AExp None) in
        let (t'', e'') = 
          match typ with
            TVoid _ when what = ADrop -> (t, e')(* strange GNU thing *)
          |  _ -> castTo t typ e' 
        in
        finishExp (se1 @ se) e'' t''
          
    | A.UNARY((A.MINUS|A.NOT|A.BNOT) as uop, e) -> 
        let uop' = match uop with
          A.MINUS -> Neg
        | A.NOT -> LNot
        | A.BNOT -> BNot
        | _ -> E.s (E.bug "")
        in
        let (se, e', t) = doExp e (AExp None) in
        let (t'', e'') = 
          if uop = A.NOT then 
            begin ignore (checkBool t e'); 
              (t, e') 
            end 
          else castTo t intType e' in
        let result = 
          match uop', e'' with
            Neg, Const(CInt(i,_),_) -> integer (- i)
          | _ -> UnOp(uop',e'', t'', lu)
        in
        finishExp se result t'' 

    | A.UNARY(A.PLUS, e) -> doExp e what 
          
          
    | A.UNARY(A.ADDROF, e) -> 
        let (se, lv, t) = doLval e in
        finishExp se (AddrOf(lv, lu)) (TPtr(t, []))
          
    | A.UNARY((A.PREINCR|A.PREDECR) as uop, e) -> 
        let uop' = if uop = A.PREINCR then Plus else Minus in
        let (se, lv, t) = doLval e in
        let tres = checkTypeAdd t intType in
        finishExp (se @ [mkSet lv (BinOp(uop', 
                                         Lval(lv), integer 1, tres, lu))])
          (Lval(lv))
          tres
          
    | A.UNARY((A.POSINCR|A.POSDECR) as uop, e) -> 
      (* If we do not drop the result then we must save the value *)
        let uop' = if uop = A.POSINCR then Plus else Minus in
        let (se, lv, t) = doLval e in
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
        let (se1, lv, lvt) = doLval e1 in
        let (se2, e'', t'') = doExp e2 (ASet(lv, lvt)) in
        finishExp (se1 @ se2) (Lval(lv)) lvt
          
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
        let tresult = 
          match bop', t1, t2 with
            Plus, TPtr _, TInt _ -> t1
          | Plus, TInt _, TPtr _ -> t2
          | Minus, TPtr _, TInt _ -> t1
          | Minus, TPtr _, TPtr _ -> intType
          | _ -> intType   (*** !!! this is only temporary *)
        in
        let result = 
          let mkInt = function
              Const(CChr c, _) -> Const(CInt(Char.code c, None),lu)
            | e -> e
          in
          match bop', mkInt e1', mkInt e2' with
            Plus, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> integer (i1 + i2)
          | Minus, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> integer (i1 - i2)
          | Mult, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> integer (i1 * i2)
          | Div, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> integer (i1 / i2)
          | Mod, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> integer (i1 mod i2)
          | BAnd, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (i1 land i2)
          | BOr, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (i1 lor i2)
          | BXor, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (i1 lxor i2)
          | Shiftlt, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (i1 lsl i2)
          | Shiftrt, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (i1 lsr i2)
          | Eq, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (if i1 = i2 then 1 else 0)
          | Ne, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (if i1 <> i2 then 1 else 0)
          | Le, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (if i1 <= i2 then 1 else 0)
          | Ge, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (if i1 >= i2 then 1 else 0)
          | Lt, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (if i1 < i2 then 1 else 0)
          | Gt, Const(CInt(i1,_),_),Const(CInt(i2,_),_) -> 
              integer (if i1 > i2 then 1 else 0)
          | _ -> BinOp(bop', e1', e2', tresult, lu)
        in
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
        let (se1, lv1, t1) = doLval e1 in
        let (se2, e2', t2) = doExp e2 (AExp None) in
        let tresult = intType in (* !!!! this is temporary *)
        finishExp (se1 @ [mkSet lv1 (BinOp(bop',Lval(lv1),e2',tresult,lu))])
          (Lval(lv1))
          tresult
          
    | A.BINARY((A.AND|A.OR), e1, e2) ->
        let tmp = var (newTempVar intType) in
        (doCondition e [mkSet tmp (integer 1)] [mkSet tmp (integer 0)], 
         Lval(tmp), intType)
          
    | A.CALL(f, args) -> 
        let (sf, f', ft') = doExp f (AExp None) in
      (* Get the result type and the argument types *)
        let (resType, argTypes, isvar, f'') = 
          match unrollType ft' with
            TFun(rt,at,isvar,a) -> (rt,at,isvar,f')
          | TPtr(TFun(rt,at,isvar,a),_) -> (* Make the function pointer 
                                            * explicit *)
              (rt,at,isvar,Lval(Mem(f',NoOffset,lu)))
          | x -> E.s (E.unimp "Unexpected type of the called function %a: %a" 
                        d_exp f' d_type x)
        in
      (* Do the arguments *)
        let rec loopArgs 
            : varinfo list * A.expression list 
          -> (stmt list * exp list) = function
            | ([], []) -> ([], [])
            | ([varg], []) when 
                 (match varg.vtype with TVoid _ -> true | _ -> false) 
                            -> ([], [])
            | (varg :: atypes, a :: args) -> 
                let (sa, a', att) = doExp a (AExp None) in
                let (at'', a'') = castTo att varg.vtype a' in
                let (ss, args') = loopArgs (atypes, args) in
                (sa @ ss, a'' :: args')
                  
            | ([], a :: args) when isvar -> (* No more types *)
                let (sa, a', at) = doExp a (AExp None) in
                let (ss, args') = loopArgs ([], args) in
                (sa @ ss, a' :: args')
            | _ -> E.s (E.unimp "Too few or too many arguments in call to %a" 
                          d_exp f')
        in
        let (sargs, args') = loopArgs (argTypes, args) in
        begin
          match what with 
            ADrop -> 
              (sf @ sargs @ [Instruction(Call(None,f'',args',lu))],
               integer 0, intType)
        (* Set to a variable of corresponding type *)
          | ASet(Var(vi,NoOffset,_) as lv, vtype) 
              when (let (t'', e'') = castTo resType vtype (integer 0) in
              e'' = integer 0) ->
                (sf @ sargs @ [Instruction(Call(Some vi,f'',args',lu))],
                 integer 0,
                 intType)
          | _ -> begin
              (* Must create a temporary *)
              match f'', args' with     (* Some constant folding *)
                Lval(Var(fv,NoOffset,_)), [Const _] 
                  when fv.vname = "__builtin_constant_p" ->
                    finishExp (sf @ sargs) (integer 1) intType
              | _ -> 
                  let tmp = newTempVar resType in
                  let i = Instruction(Call(Some tmp,f'',args',lu)) in
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
        (doCondition e1 se2 se3, integer 0, intType)

            
    | A.QUESTION (e1, e2, e3) -> begin (* what is not ADrop*)
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
          [Instruction(Set(lv', e', _))], AExp _ when lv' == lv -> 
            finishExp [] e' tlv
        | _, _ -> (stats, (Lval(lv)), tlv)
    end
    | A.GNU_BODY ((_, s) as b) -> begin
        (* Find the last A.COMPUTATION *)
        let rec findLast = function
            A.SEQUENCE (_, s) -> findLast s
          | (A.COMPUTATION _) as s -> s
          | _ -> E.s (E.unimp "Cannot find COMPUTATION in GNU_BODY\n")
        in
        (* Save the previous data *)
        let old_gnu = ! gnu_body_result in
        let lastComp = findLast s in
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
    ([Instruction(Asm(["booo_exp"], false, [], [], []))], integer 0, intType)
  end

    (* Process expressions that should be l-values *)
and doLval (e : A.expression) : (stmt list * lval * typ) = 
  let lu = locUnknown in
  let findField n fidlist = 
    try
      List.find (fun fid -> n = fid.fname) fidlist
    with Not_found -> E.s (E.unimp "Cannot find field %s" n)
  in
  let addOffset addit lv =
    let rec loop = function
        NoOffset -> addit
      | Field(fid', offset) -> Field(fid', loop offset)
      | Index(e', offset) -> Index(e', loop offset)
      | CastO(t, offset) -> CastO(t, loop offset)
    in
    match lv with 
      Var(vi,offset,l) -> Var(vi,loop offset,l)
    | Mem(e,offset,l) -> Mem(e,loop offset,l)
  in
  match e with
    A.VARIABLE n -> 
      let vi = lookup n in
      ([], Var(vi,NoOffset,lu), vi.vtype)

  | A.MEMBEROFPTR (e, str) -> 
      let (se, e', t') = doExp e (AExp None) in begin
        match unrollType t' with
          TPtr(t1, _) ->
            let fid = 
              match unrollType t1 with 
                TStruct (n, fil, _, _) -> findField str fil
              | TUnion (n, fil, _, _) -> findField str fil
              | x -> E.s (E.unimp "expecting a struct with field %s. Found %a. t1 is %a" 
                            str d_type x d_type t')
            in
            (se, Mem(e', Field(fid, NoOffset), lu), fid.ftype)
        | _ -> E.s (E.unimp "expecting a pointer to a struct")
      end

  | A.MEMBEROF (e, str) -> 
      let (se, lv, t') = doLval e in
      let fid = 
        match unrollType t' with
          TStruct (n, fil, _, _) -> findField str fil
        | TUnion (n, fil, _, _) -> findField str fil
        | _ -> E.s (E.unimp "expecting a struct with field %s" str)
      in
      let lv' = addOffset (Field(fid, NoOffset)) lv in
      (se, lv', fid.ftype)

  | A.UNARY (A.MEMOF, e) -> 
      let (se, e', t) = doExp e (AExp None) in
      let t' = 
        match unrollType t with 
          TPtr (t', _) -> t'
        | TArray (t', _, _) -> t'
        | _ -> E.s (E.unimp "expecting a pointer type but found %a in *(%a)\n"
                      d_type t d_exp e')
      in
      (se, Mem(e', NoOffset, lu), t')

  | A.INDEX (e1, e2) -> 
      let (se1, lv1', t1') = doLval e1 in
      let (se2, e2', t2') = doExp e2 (AExp None) in
      let (_, e2'') = castTo t2' intType e2' in
      let tres = 
        match unrollType t1' with 
          TPtr(tres, _) -> tres
        | TArray(tres, _, _) -> tres
        | _ -> E.s (E.unimp "expecting a pointer type but found %a in %a[%a]\n"
                      d_type t1' d_lval lv1' d_exp e2')
      in
      (se1 @ se2, addOffset (Index(e2'', NoOffset)) lv1', tres)

  | A.CAST (bt, e) -> 
      let t = doType [] bt in
      let (se, lv, t1) = doLval e in
      (se, addOffset (CastO(t, NoOffset)) lv, t)

  | e ->
      let (_, e', _) = doExp e (AExp None) in
      E.s (E.unimp "lval. The exp is: %a\n" d_exp e')


(* A special case for conditionals *)
and doCondition (e: A.expression) 
                (st: stmt list)
                (sf: stmt list) : stmt list = 
  match e with 
  | A.BINARY(A.AND, e1, e2) ->
      let (sf1, sf2) = 
        (* If sf is small then will copy it *)
        let szf = List.length sf in
        if szf <= 3 then
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
        let szt = List.length st in
        if szt <= 3 then
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

  | _ -> begin
      let (se, e, t) as rese = doExp e (AExp None) in
      ignore (checkBool t e);
      match e with 
        Const(CInt(i,_),_) -> 
          if i <> 0 then
            se @ st
          else
            se @ sf
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
  | _ -> E.s (E.unimp "checkTypeAdd")


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
        let vi' = addNewVar vi in
        if e = A.NOTHING then
          [Skip]
        else
          let (se, e', et) = doExp e (AExp (Some vi.vtype)) in
          (match et with (* We have a length now *)
            TArray(_, Some _, _) -> vi'.vtype <- et
          | _ -> ());
          let (_, e'') = castTo et vi'.vtype e' in
          se @ [assign vi' e'']
      in
      let stmts = doNameGroup createLocal ng in
      List.concat stmts

  | _ -> E.s (E.unimp "doDecl")
    
  
  (* Now define the processors for body and statement *)
and doBody (decls, s) : stmt list = 
    startBlock ();
    (* Do the declarations and the initializers *)
          let init = List.concat (List.map doDecl decls) in
          let s' = doStatement s in
    endBlock ();
    init @ s'
      
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
            
      | A.FOR(e1,e2,e3,s) ->
          let (se1, _, _) = doExp e1 ADrop in
          let (se3, _, _) = doExp e3 ADrop in
          startLoop false;
          let s' = doStatement s in
          let s'' = labContinue () :: se3 in
          exitLoop ();
          se1 @ [Loop(mkSeq ((doCondition e2 [Skip] [Break])
                              @ s' @ s''))]
                  
      | A.BREAK -> [Break]
            
      | A.CONTINUE -> [doContinue ()]
            
      | A.RETURN A.NOTHING -> [Return None]
      | A.RETURN e -> 
          let (se, e', et) = doExp e (AExp None) in
          let (et'', e'') = castTo et (!currentReturnType) e' in
          se @ [Return (Some e')]
                 
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
              [], Const (CInt (i, _), _) -> i
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
                let (se, lv, t) = doLval e in
                stmts := se :: !stmts;
                match lv with 
                  Var(vi, NoOffset, _) -> (c, vi)(* already var *)
                | _ -> begin
                    let tmp = newTempVar t in
                    temps := (lv, tmp) :: !temps;
                    (c, tmp)
                end)
              outs
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
          (Instruction(Asm(tmpls, isvol, outs', ins', clobs)) ::
           (List.map (fun (lv, vi) -> 
             Instruction(Set(lv,Lval(var vi),locUnknown))) !temps))
    with e -> begin
      (ignore (E.log "Error in doStatement (%s)\n" (Printexc.to_string e)));
      [Label "booo_statement"]
    end


(*** Take a statement and fix the vaddrof fields or variables *)
let fixAddrOf body = 
  let rec fExp = function
      (Const _|SizeOf _) -> ()
    | Lval lv -> fLval lv
    | UnOp(_,e,_,_) -> fExp e
    | BinOp(_,e1,e2,_,_) -> fExp e1; fExp e2
    | CastE(_, e,_) -> fExp e
    | Compound (_, el) -> List.iter fExp el
    | AddrOf (Var(vi,off,_),_) -> fOff off; vi.vaddrof <- true
    | AddrOf (Mem(e,off,_),_) -> fExp e; fOff off
  and fLval = function
      Var(_,off,_) -> fOff off
    | Mem(e,off,_) -> fExp e; fOff off
  and fOff = function
      Field (_, o) -> fOff o
    | Index (e, o) -> fExp e; fOff o
    | CastO (_, o) -> fOff o
    | NoOffset -> ()
  and fStmt = function
      (Skip|Break|Continue|Label _|Goto _|Case _|Default|Return None) -> ()
    | Sequence s -> List.iter fStmt s
    | Loop s -> fStmt s
    | IfThenElse (e, s1, s2) -> fExp e; fStmt s1; fStmt s2
    | Return(Some e) -> fExp e
    | Switch (e, s) -> fExp e; fStmt s
    | Instruction(Set(lv,e,_)) -> fLval lv; fExp e
    | Instruction(Call(_,f,args,_)) -> fExp f; List.iter fExp args
    | Instruction(Asm(_,_,_,ins,_)) -> 
        List.iter (fun (_, e) -> fExp e) ins
  in
  fStmt body
    
(* Translate a file *)
let convFile dl = 
  ignore (E.log "Cabs2cil conversion\n");
                                        (* We collect here the program *)
  let theFile : global list ref = ref [] in
  (* Clean up the global types *)
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
            recordType n newTyp;
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
          try
            let vi = makeVarInfo true locUnknown sname in
            let vi' = addNewVar vi in
            let init, vi'' = 
              if e = A.NOTHING then 
                None, vi'
              else 
                let (se, e', et) = doExp e (AExp (Some vi.vtype)) in
                let (_, e'') = castTo et vi'.vtype e' in
                (match et with (* We have a length now *)
                  TArray(_, Some _, _) -> vi'.vtype <- et
                | _ -> ());
                if se <> [] then 
                  E.s (E.unimp "global initializer");
                Some e'', vi'
            in
            theFile := GVar(vi'', init) :: !theFile
          with e -> begin
            ignore (E.log "error in CollectGlobal (%s)\n" 
                      (Printexc.to_string e));
            theFile := GAsm("booo - error in global " ^ n) :: !theFile
          end
        in
        ignore (doNameGroup createGlobal ng)
          
    | A.GLOBASM s -> theFile := GAsm s :: !theFile

    | A.FUNDEF (((bt,st,(n,bt',funattr,_)) : A.single_name), 
                 (body : A.body)) -> 
        begin
          try
           (* Reset the local identifier so that formals are created with the 
            * proper IDs  *)
            resetLocals ();
            alphaId := -1;
                                        (* Do the type *)
            let (returnType, formals, isvararg, a) = 
              match unrollType (doType [] bt') with 
                TFun(rType, formals, isvararg, a) -> 
                  (rType, formals, isvararg, a)
              | x -> E.s (E.bug "non-function type: %a." d_type x)
            in
            (* Record the returnType for doStatement *)
            currentReturnType := returnType;
            (* Setup the environment. Add the formals to the locals. Maybe 
             * they need alpha-conv *)
            startBlock ();
            let formals' = List.map addNewVar formals in
            let ftype = TFun(returnType, formals', isvararg, a) in
            let fattr = List.map doAttr funattr in
            let fstorage = doStorage st in
            (* Add the function itself to the environment. Just in case we 
             * have recursion and no prototype.  *)
            (* Make a variable out of it and put it in the environment *)
            let thisFunction = 
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
            ignore (addNewVar thisFunction);
            (* Now do the body *)
            let s = doBody body in
            (* Finish everything *)
            endBlock ();
            (* Now add the function to the environment again. This time it 
             * will go into the global environment  *)
            ignore (addNewVar thisFunction);
            let (nrlocals, locals) = getLocals () in
            let fdec = { sname    = n;
                         slocals  = locals;
                         smaxid   = nrlocals;
                         sbody    = (match mkSeq s with (Sequence _) as x -> x 
                                     | x -> Sequence [x]);
                         sstorage = fstorage;
                         sattr    = fattr;
                         stype    = ftype;
                       } 
            in
            fixAddrOf fdec.sbody;
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

