(* Convert CABS to CIL *)
module A = Cabs
module E = Errormsg
module H = Hashtbl

open Pretty
open Cil
open Trace


let lu = locUnknown

(*** EXPRESSIONS *************)
                                        (* We collect here the program *)
let theFile : global list ref = ref []

(********* ENVIRONMENTS ***************)

(* The environment is kept in two distinct data structures. A hash table maps 
 * each original variable name into a varinfo (for variables, or an 
 * enumeration tag, or a type). (Note that the varinfo might contain an 
 * alpha-converted name different from that of the lookup name.) The Ocaml 
 * hash tables can keep multiple mappings for a single key. Each time the 
 * last mapping is returned and upon deletion the old mapping is restored. To 
 * keep track of local scopes we also maintain a list of scopes (represented 
 * as lists).  *)
type envdata = 
    EnvVar of varinfo                   (* The name refers to a variable 
                                         * (which could also be a function) *)
  | EnvEnum of exp * typ                (* The name refers to an enumeration 
                                         * tag for which we know the value 
                                         * and the host type *)
  | EnvTyp of typ                       (* The name is of the form  "struct 
                                         * foo", or "union foo" or "enum foo" 
                                         * and refers to a type. Note that 
                                         * the name of the actual type might 
                                         * be different from foo due to alpha 
                                         * conversion *)


let env : (string, envdata) H.t = H.create 307

 (* In the scope we keep the original name, so we can remove them from the 
  * hash table easily *)
let scopes :  string list ref list ref = ref []


(* When you add to env, you also add it to the current scope *)
let addLocalToEnv (n: string) (d: envdata) = 
  H.add env n d;
    (* If we are in a scope, then it means we are not at top level. Add the 
     * name to the scope *)
  (match !scopes with
    [] -> ()
  | s :: _ -> s := n :: !s)


let addGlobalToEnv (k: string) (d: envdata) : unit = 
  H.add env k d
  
  

(* Create a new name based on a given name. The new name is formed from a 
 * prefix (obtained from the given name as the longest prefix that ends with 
 * a non-digit), followed by a '_' and then by a positive integer suffix. The 
 * first argument is a table mapping name prefixes with the largest suffix 
 * used so far for that prefix. The largest suffix is one when only the 
 * version without suffix has been used. *)
let alphaTable : (string, int ref) H.t = H.create 307 (* vars and enum tags. 
                                                       * For composite types 
                                                       * we have names like 
                                                       * "struct foo" or 
                                                       * "type bar" *)

(* To keep different name scopes different, we add prefixes to names 
 * specifying the kind of name: the kind can be one of "" for variables or 
 * enum tags, "struct" for structures, "enum" for enumerations, "union" for 
 * union types, or "type" for types *)
let kindPlusName (kind: string)
                 (origname: string) : string =
  if kind = "" then origname else
  kind ^ " " ^ origname
                   
let newAlphaName (kind: string) 
                 (origname: string) : string = 
  let lookupname = kindPlusName kind origname in
  (* Split the lookup name into a prefix and a suffix. The suffix is 
   * numberic and is separated by _ from the prefix *)
  let l = String.length lookupname in
  let (prefix, suffix) = 
    try
      let under = String.rindex lookupname '_' in
      (String.sub lookupname 0 under, 
       int_of_string (String.sub lookupname under (l - under)))
    with _ -> (lookupname, -1)
  in
  let newname = 
    try
      let rc = H.find alphaTable prefix in
      let newsuffix = if suffix > !rc then suffix else !rc + 1 in
      rc := newsuffix;
      prefix ^ "_" ^ (string_of_int newsuffix)
    with Not_found -> begin (* First variable with this prefix *)
      H.add alphaTable prefix (ref suffix);
      lookupname  (* Return the original name *)
    end
  in
  (* Now strip the kind prefix *)
  let n' = 
    let l = 1 + String.length kind in
    if l > 1 then 
      String.sub newname l (String.length newname - l)
    else
      newname
  in
  n'
  

let docAlphaTable () = 
  let acc : (string * int) list ref = ref [] in
  H.iter (fun k d -> acc := (k, !d) :: !acc) alphaTable;
  docList line (fun (k, d) -> dprintf "  %s -> %d" k d) () !acc


let localId = ref (-1)   (* All locals get id's starting at 0 *)
let locals : varinfo list ref = ref []

let resetLocals () = 
  localId := (-1); locals := []

let startFile () = 
  H.clear env;
  H.clear alphaTable;
  resetLocals ()


     (* Eliminate all locals from the alphaTable. Return the maxlocal id and 
      * the list of locals (but remove the arguments first) *)
let endFunction (formals: varinfo list) : (int * varinfo list) = 
  let rec loop revlocals = function
      [] -> revlocals
    | lvi :: t -> (* H.remove alphaTable lvi.vname; *) 
        loop (lvi :: revlocals) t
  in
  let revlocals = loop [] !locals in
  let maxid = !localId + 1 in
  resetLocals ();
  let rec drop formals locals = 
    match formals, locals with
      [], l -> l
    | f :: formals, l :: locals -> 
        if f != l then 
          E.s (E.bug "formals are not in locals");
        drop formals locals
    | _ -> E.s (E.bug "Too few locals")
  in
  (maxid, drop formals revlocals)

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
let lookupVar n = 
  match H.find env n with
    EnvVar vi -> vi
  | _ -> raise Not_found
        
let docEnv () = 
  let acc : (string * envdata) list ref = ref [] in
  let doone () = function
      EnvVar vi -> text vi.vname
    | EnvEnum (tag, typ) -> text ("enum tag")
    | EnvTyp t -> text "typ"
  in
  H.iter (fun k d -> acc := (k, d) :: !acc) env;
  docList line (fun (k, d) -> dprintf "  %s -> %a" k doone d) () !acc


(* Create a new variable ID *)
let newVarId name isglobal = 
  if isglobal then H.hash name
  else begin
    incr localId;
    !localId
  end



(* Add a new variable. Do alpha-conversion if necessary *)
let alphaConvertVarAndAddToEnv (addtoenv: bool) (vi: varinfo) : varinfo = 
  let newname = newAlphaName "" vi.vname in
  let newvi = 
    if vi.vname = newname then vi else 
    {vi with vname = newname; 
             vid = if vi.vglob then H.hash newname else vi.vid} in
  if not vi.vglob then
    locals := newvi :: !locals;

  (if addtoenv then 
    if vi.vglob then
      addGlobalToEnv vi.vname (EnvVar newvi)
    else
      addLocalToEnv vi.vname (EnvVar newvi));
  (* ignore (E.log "After adding %s alpha table is: %t\n"
            newvi.vname docAlphaTable); *)
  newvi


(* Create a new temporary variable *)
let newTempVar typ = 
  (* Strip the "const" from the type. It is unfortunate that const variables 
    can only be set in initialization. Once we decided to move all 
    declarations to the top of the functions, we have no way of setting a 
    "const" variable *)
  let stripConst t =
    let a = typeAttrs t in
    setTypeAttrs t 
      (dropAttribute (dropAttribute a (AId("const")))
         (AId("restrict")))
  in
  alphaConvertVarAndAddToEnv false  (* Do not add to the environment *)
    { vname = "tmp";  (* addNewVar will make the name fresh *)
      vid   = newVarId "tmp" false;
      vglob = false;
      vtype = stripConst typ;
      vdecl = locUnknown;
      vattr = [];
      vaddrof = false;
      vreferenced = false;   (* sm *)
      vstorage = NoStorage;
    } 


let mkAddrOfAndMark ((b, off) as lval) : exp = 
  (* Mark the vaddrof flag if b is a variable *)
  (match b with 
    Var vi -> vi.vaddrof <- true
  | _ -> ());
  mkAddrOf lval
  


   (* Keep a set of self compinfo for composite types *)
let compInfoNameEnv : (string, compinfo) H.t = H.create 113

let lookupTypeNoError (kind: string) 
                      (n: string) : typ = 
  let kn = kindPlusName kind n in
  match H.find env kn with
    EnvTyp t -> t
  | _ -> raise Not_found

let lookupType (kind: string) 
               (n: string) : typ = 
  try
    lookupTypeNoError kind n
  with Not_found -> 
    E.s (E.unimp "Cannot find type %s (kind:%s)\n" n kind)

(* Create the self ref cell and add it to the map *)
let createCompInfo (iss: bool) (n: string) : compinfo = 
  (* Add to the self cell set *)
  let key = (if iss then "struct " else "union ") ^ n in
  try
    H.find compInfoNameEnv key (* Only if not already in *)
  with Not_found -> begin
    (* Create a compinfo *)
    let res = mkCompInfo iss n (fun _ -> []) [] in
    H.add compInfoNameEnv key res;
    res
  end

   (* kind is either "struct" or "union" or "enum" and n is a name *)
let findCompType kind n a = 
  let key = kind ^ " " ^ n in
  let makeForward () = 
    (* This is a forward reference, either because we have not seen this 
     * struct already or because we want to create a version with different 
     * attributes  *)
    let iss =  (* is struct or union *)
      if kind = "enum" then E.s (E.unimp "Forward reference for enum %s" n)
      else if kind = "struct" then true else false
    in
    let self = createCompInfo iss n in
    TForward (self, a)
  in
  try
    let old = lookupTypeNoError kind n in (* already defined  *)
    let olda = typeAttrs old in
    if olda = a then old else makeForward ()
  with Not_found -> makeForward ()
  
  

(**** To initialize some local arrays we need strncpy ****)
let strncpyFun = 
  let fdec = emptyFunction "strncpy" in
  let argd  = makeLocalVar fdec "dst" charPtrType in
  let args  = makeLocalVar fdec "src" charConstPtrType in
  let arglen  = makeLocalVar fdec "len" uintType in
  fdec.svar.vtype <- TFun(charPtrType, [ argd; args; arglen ], false, []);
  fdec

let explodeString (nullterm: bool) (s: string) : char list =  
  let rec allChars i acc = 
    if i < 0 then acc
    else allChars (i - 1) ((String.get s i) :: acc)
  in
  allChars (-1 + String.length s) 
    (if nullterm then [Char.chr 0] else [])
    
(*** In order to process GNU_BODY expressions we must record that a given 
 *** COMPUTATION is interesting *)
let gnu_body_result : (A.statement * ((exp * typ) option ref)) ref 
    = ref (A.NOP, ref None)

(*** When we do statements we need to know the current return type *)
let currentReturnType : typ ref = ref (TVoid([]))
let currentFunctionVI : varinfo ref = ref dummyFunDec.svar

(**** Occasionally we see structs with no name and no fields *)


module BlockChunk = 
  struct
    type chunk = {
        stmts: stmt list;
        postins: instr list;              (* Some instructions to append at 
                                           * the ends of statements (in 
                                           * reverse order)  *)
                                        (* A list of case statements at the 
                                         * outer level *)
        cases: (label * stmt) list
      } 

    let empty = 
      { stmts = []; postins = []; cases = [];
        (* fixbreak = (fun _ -> ()); fixcont = (fun _ -> ()) *) }

    let isEmpty (c: chunk) = 
      c.postins == [] && c.stmts == []

    let isNotEmpty (c: chunk) = not (isEmpty c)

    let i2c (i: instr) = 
      { empty with postins = [i] }
        
    (* Occasionally, we'll have to push postins into the statements *)
    let pushPostIns (c: chunk) : stmt list = 
      if c.postins = [] then c.stmts
      else
        let rec toLast = function
            [{skind=Instr il} as s] as stmts -> 
              s.skind <- Instr (il @ (List.rev c.postins));
              stmts

          | [] -> [mkStmt (Instr (List.rev c.postins))]

          | a :: rest -> a :: toLast rest
        in
        compactBlock (toLast c.stmts)


    (* Add an instruction at the end. Never refer to this instruction again 
     * after you call this *)
    let (+++) (c: chunk) (i : instr) =
      {c with postins = i :: c.postins}

    (* Append two chunks. Never refer to the original chunks after you call 
     * this. And especially never share c2 with somebody else *)
    let (@@) (c1: chunk) (c2: chunk) = 
      { stmts = compactBlock (pushPostIns c1 @ c2.stmts);
        postins = c2.postins;
        cases = c1.cases @ c2.cases;
      } 

    let skipChunk = empty
        
    let returnChunk (e: exp option) (l: location) : chunk = 
      { stmts = [ mkStmt (Return(e, l)) ];
        postins = [];
        cases = []
      }

    let ifChunk (be: exp) (l: location) (t: chunk) (e: chunk) : chunk = 
      
      { stmts = [ mkStmt(If(be, pushPostIns t, pushPostIns e, l))];
        postins = [];
        cases = t.cases @ e.cases;
      } 

    let canDuplicate (c: chunk) = 
                        (* We can duplicate a statement if it is small and 
                         * does not contain label definitions  
      let rec cost = function
          [] -> 0
        | s :: rest -> 
            let one = 
              if s.labels <> [] then 100 else
              match s.skind with
                Instr il -> List.length il
              | Goto _ | Return _ -> 1
              | _ -> 100
            in
            one + cost rest
      in
      cost c.stmts + List.length c.postins <= 3
                           *)
      false (* Do not duplicate because we get too much sharing *)

    let canDrop (c: chunk) = false

    let loopChunk (body: chunk) : chunk = 
      (* Make the statement *)
      let loop = mkStmt (Loop (pushPostIns body, lu)) in
      { stmts = [ loop (* ; n *) ];
        postins = [];
        cases = body.cases;
      } 
      
    let breakChunk (l: location) : chunk = 
      { stmts = [ mkStmt (Break l) ];
        postins = [];
        cases = [];
      } 
      
    let continueChunk (l: location) : chunk = 
      { stmts = [ mkStmt (Continue l) ];
        postins = [];
        cases = []
      } 

        (* Keep track of the gotos *)
    let backPatchGotos : (string, stmt ref list ref) H.t = H.create 17
    let addGoto (lname: string) (bref: stmt ref) : unit = 
      let gotos = 
        try
          H.find backPatchGotos lname
        with Not_found -> begin
          let gotos = ref [] in
          H.add backPatchGotos lname gotos;
          gotos
        end
      in
      gotos := bref :: !gotos

        (* Keep track of the labels *)
    let labelStmt : (string, stmt) H.t = H.create 17
    let initLabels () = 
      H.clear backPatchGotos;
      H.clear labelStmt
      
    let resolveGotos () = 
      H.iter
        (fun lname gotos ->
          try
            let dest = H.find labelStmt lname in
            List.iter (fun gref -> gref := dest) !gotos
          with Not_found -> begin
            E.s (E.bug "Label %s not found\n" lname)
          end)
        backPatchGotos

        (* Get the first statement in a chunk. Might need to change the 
         * statements in the chunk *)
    let getFirstInChunk (c: chunk) : stmt * block = 
      (* Get the first statement and add the label to it *)
      match c.stmts with
        s :: _ -> s, c.stmts
      | [] -> (* Add a statement *)
          let n = mkEmptyStmt () in
          n, n :: c.stmts
      
    let consLabel (l: string) (c: chunk) : chunk = 
      (* Get the first statement and add the label to it *)
      let labstmt, stmts' = getFirstInChunk c in
      (* Add the label *)
      labstmt.labels <- Label (l, lu) :: labstmt.labels;
      H.add labelStmt l labstmt;
      if c.stmts == stmts' then c else {c with stmts = stmts'}

    let gotoChunk (ln: string) (l: location) : chunk = 
      let gref = ref dummyStmt in
      addGoto ln gref;
      { stmts = [ mkStmt (Goto (gref, l)) ];
        postins = [];
        cases = [];
      }

    let caseChunk (i: int) (l: location) (next: chunk) = 
      let fst, stmts' = getFirstInChunk next in
      let lb = Case (i, l) in
      fst.labels <- lb :: fst.labels;
      { next with stmts = stmts'; cases = (lb, fst) :: next.cases}
        
    let defaultChunk (l: location) (next: chunk) = 
      let fst, stmts' = getFirstInChunk next in
      let lb = Default l in
      fst.labels <- lb :: fst.labels;
      { next with stmts = stmts'; cases = (lb, fst) :: next.cases}

        
    let switchChunk (e: exp) (body: chunk) (l: location) =
      (* Make the statement *)
      let switch = mkStmt (Switch (e, pushPostIns body, 
                                   List.map (fun (_, s) -> s) body.cases, 
                                   l)) in
      { stmts = [ switch (* ; n *) ];
        postins = [];
        cases = [];
      } 

    let mkFunctionBody (c: chunk) : block = 
      resolveGotos (); initLabels ();
      if c.cases <> [] then
        E.s (E.bug "Switch cases not inside a switch statement\n");
      pushPostIns c
      
  end

open BlockChunk 


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
let continueOrLabelChunk (l: location) : chunk = 
  match !continues with
    [] -> E.s (E.bug "continue not in a loop")
  | While :: _ -> continueChunk l
  | NotWhile lr :: _ -> 
      if !lr = "" then begin
        incr labelId;
        lr := "Cont" ^ (string_of_int !labelId)
      end;
      gotoChunk !lr l

let consLabContinue (c: chunk) = 
  match !continues with
    [] -> E.s (E.bug "labContinue not in a loop")
  | While :: rest -> c
  | NotWhile lr :: rest -> if !lr = "" then c else consLabel !lr c

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
            
      (* We assume a long long is always larger than a long  *)
      | TInt(ILongLong, _), _ -> checkToInt t2'; t1'  
      | _, TInt(ILongLong, _) -> checkToInt t1'; t2'
            
      | TInt(IULong, _), _ -> checkToInt t2'; t1'
      | _, TInt(IULong, _) -> checkToInt t1'; t2'

                    
      | TInt(ILong,_), TInt(IUInt,_) when not !ilongFitsUInt -> TInt(IULong,[])
      | TInt(IUInt,_), TInt(ILong,_) when not !ilongFitsUInt -> TInt(IULong,[])
            
      | TInt(ILong, _), _ -> checkToInt t2'; t1'
      | _, TInt(ILong, _) -> checkToInt t1'; t2'

      | TInt(IUInt, _), _ -> checkToInt t2'; t1'
      | _, TInt(IUInt, _) -> checkToInt t1'; t2'
            
      | TInt(IInt, _), TInt (IInt, _) -> t1'

      | _, _ -> E.s (E.bug "arithmeticConversion")
  end

let conditionalConversion (e2: exp) (t2: typ) (e3: exp) (t3: typ) : typ =
  let tresult =  (* ISO 6.5.15 *)
    match unrollType t2, unrollType t3 with
      (TInt _ | TEnum _ | TBitfield _ | TFloat _), 
      (TInt _ | TEnum _ | TBitfield _ | TFloat _) -> 
        arithmeticConversion t2 t3 
    | TComp comp2, TComp comp3 when comp2.ckey = comp3.ckey -> t2 
    | TPtr(_, _), TPtr(TVoid _, _) -> t2
    | TPtr(TVoid _, _), TPtr(_, _) -> t3
    | TPtr(t2'', _), TPtr(t3'', _) 
          when typeSig t2'' = typeSig t3'' -> t2
    | TPtr(_, _), TInt _ when 
            (match e3 with Const(CInt(0,_,_)) -> true | _ -> false) -> t2
    | TInt _, TPtr _ when 
              (match e2 with Const(CInt(0,_,_)) -> true | _ -> false) -> t3
    | _, _ -> E.s (E.unimp "A.QUESTION")
  in
  tresult

  
let rec castTo (ot : typ) (nt : typ) (e : exp) : (typ * exp ) = 
  if typeSig ot = typeSig nt then (ot, e) else
  match ot, nt with
    TNamed(_,r, _), _ -> castTo r nt e
  | _, TNamed(_,r, _) -> castTo ot r e
  | TForward(comp, _), _ -> castTo (TComp comp) nt e
  | _, TForward(comp, _) -> castTo ot (TComp comp) e
  | TInt(ikindo,_), TInt(ikindn,_) -> 
      (nt, if ikindo == ikindn then e else doCastT e ot nt)

  | TPtr (told, _), TPtr(tnew, _) -> (nt, doCastT e ot nt)

  | TInt _, TPtr _ when
      (match e with Const(CInt(0,_,_)) -> true | _ -> false) -> 
        (nt, doCastT e ot nt)

  | TInt _, TPtr _ -> (nt, doCastT e ot nt)

  | TPtr _, TInt _ -> (nt, doCastT e ot nt)

  | TArray _, TPtr _ -> (nt, doCastT e ot nt)

  | TArray(t1,_,_), TArray(t2,None,_) when typeSig t1 = typeSig t2 -> (nt, e)

  | TPtr _, TArray(_,_,_) -> (nt, e)

  | TEnum _, TInt _ -> (nt, e)
  | TFloat _, TInt _ -> (nt, doCastT e ot nt)
  | TInt _, TFloat _ -> (nt, doCastT e ot nt)
  | TFloat _, TFloat _ -> (nt, doCastT e ot nt)
  | TInt _, TEnum _ -> (nt, e)
  | TEnum _, TEnum _ -> (nt, e)

  | TBitfield _, (TInt _ | TEnum _ | TBitfield _)-> (nt, e)
  | (TInt _ | TEnum _), TBitfield _ -> (nt, e)


    (* The expression is evaluated for its side-effects *)
  | (TInt _ | TEnum _ | TBitfield _ | TPtr _ ), TVoid _ -> (ot, e)

  | _ -> E.s (E.unimp "cabs2cil: castTo %a -> %a@!" d_type ot d_type nt)

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



(* Create and cache varinfo's for globals. Returns the varinfo and whether 
 * there exists already a definition *)
let makeGlobalVarinfo (vi: varinfo) =
  try (* See if already defined *)
    let oldvi = lookupVar vi.vname in
    (* It was already defined. We must reuse the varinfo. But clean up the 
     * storage.  *)
    let _ = 
      if vi.vstorage = oldvi.vstorage then ()
      else if vi.vstorage = Extern then ()
      else if oldvi.vstorage = Extern then 
        oldvi.vstorage <- vi.vstorage 
      else E.s (E.unimp "Unexpected redefinition")
    in
    (* Union the attributes *)
    oldvi.vattr <- addAttributes oldvi.vattr vi.vattr;
    (* Maybe we had an incomplete type before and it is complete now  *)
    let _ = 
      let oldts = typeSig oldvi.vtype in
      let newts = typeSig vi.vtype in
      if oldts = newts then ()
      else 
        match oldts, newts with
                                        (* If the new type is complete, I'll 
                                         * trust it  *)
        | TSArray(_, Some _, _), TSArray(_, None, _) -> ()
        | TSArray(_, _, _), TSArray(_, Some _, _) 
          -> oldvi.vtype <- vi.vtype
         (* If one is a function with no arguments and the other has some 
          * arguments, believe the one with the arguments *)
        | TSFun(_, [], va1, _), TSFun(r2, _ :: _, va2, a2)
               when va1 = va2 -> oldvi.vtype <- vi.vtype
        | TSFun(r1, _ , va1, _), 
          TSFun(_, [], va2, a2)
               when va1 = va2 -> ()
        | _, _ -> E.s (E.unimp "Redefinition of %s with different types.@!Before=%a@!After= %a@!" 
                         vi.vname d_plaintype oldvi.vtype d_plaintype vi.vtype)
    in
    let rec alreadyDef = ref false in
    let rec loop = function
        [] -> []
      | (GVar (vi', i', l) as g) :: rest when vi'.vid = vi.vid -> 
          if i' = None then
            GDecl (vi', l) :: loop rest
          else begin
            alreadyDef := true;
            g :: rest (* No more defs *)
          end
      | g :: rest -> g :: loop rest
    in
    theFile := loop !theFile;
    oldvi, !alreadyDef
      
  with Not_found -> begin (* A new one. It is a definition unless it is 
                           * Extern  *)

    alphaConvertVarAndAddToEnv true vi, false
  end



(**** PEEP-HOLE optimizations ***)
let afterConversion (c: chunk) : chunk = 
  (* Now scan the statements and find Instr blocks *)
  let collapseCallCast = function
      Call(Some(vi, false), f, args, l),
      Set((Var destv, NoOffset), 
                CastE (newt, Lval(Var vi', NoOffset)), _) 
      when (not vi.vglob && 
            String.length vi.vname >= 3 &&
            String.sub vi.vname 0 3 = "tmp" &&
            vi' == vi) 
      -> Some [Call(Some(destv, true), f, args, l)]
    | _ -> None
  in
  (* First add in the postins *)
  let sl = pushPostIns c in
  peepHole2 collapseCallCast sl;
  { c with stmts = sl; postins = [] }

let rec makeVarInfo (isglob: bool) 
                (ldecl: location)
                ((bt,st,(n,nbt,a,e)) : A.single_name) : varinfo = 
  { vname    = n;
    vid      = newVarId n isglob;
    vglob    = isglob;
    vstorage = doStorage st;
    vattr    = doAttrList a;
    vdecl    = ldecl;
    vtype    = doType [] nbt;
    vaddrof  = false;
    vreferenced = false;   (* sm *)
  }


and doAttr : A.attribute -> attribute = function
    (s, []) -> AId s
  | (s, el) -> 
      let attrOfVar n = 
        try 
          let vi = lookupVar n in
          AVar vi
        with Not_found -> 
          AId n
      in
      let rec attrOfExp = function
          A.VARIABLE n -> attrOfVar n
        | A.CONSTANT (A.CONST_STRING s) -> AStr s
        | A.CONSTANT (A.CONST_INT str) -> AInt (int_of_string str)
        | A.CALL(A.VARIABLE n, args) -> ACons (n, List.map attrOfExp args)
        | _ -> E.s (E.unimp "attrOfExp")
      in
      ACons (s, List.map attrOfExp el)

and doAttrList (al: A.attribute list) : attribute list = 
  dropAttribute (dropAttribute
  (List.fold_left (fun acc a -> addAttribute (doAttr a) acc) [] al)
	(* weimer: I need to drop this attribute before it gets anywhere *)
	(ACons("__mode__",[]))) (AId("__transparent_union__"))


and doStorage = function
    A.NO_STORAGE -> NoStorage
  | A.AUTO -> NoStorage
  | A.REGISTER -> Register
  | A.STATIC -> Static
  | A.EXTERN -> Extern

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
      let width = match doExp true e (AExp None) with
        (c, Const(CInt(i,_,_)),_) when isEmpty c -> i
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
        | _ -> 
            let len' = doPureExp len in
            let _, len'' = castTo (typeOf len') intType len' in
            Some len''
      in
      TArray (doType [] bt, lo, a)

  | A.STRUCT n -> 
      if n = "" then E.s (E.bug "Missing struct tag");
      findCompType "struct" n a

  | A.STRUCTDEF (n, nglist) -> (* This introduces a new type always *)
      makeCompType true n nglist a

  | A.UNION n -> 
      if n = "" then E.s (E.bug "Missing union tag");
      findCompType "union" n a

  | A.UNIONDEF (n, nglist) -> (* This introduces a new type always *)
      makeCompType false n nglist a

  | A.PROTO (bt, snlist, isvararg, _) ->
      (* Turn [] types into pointers in the arguments and the result type. 
       * This simplifies our life a lot  *)
      let arrayToPtr t = 
        match unrollType t with
          TArray(t,_,attr) -> TPtr(t, attr)
        | _ -> t
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
          A.ATTRTYPE (bt', ["stdcall", []]) -> 
            bt', (* addAttribute (AId("stdcall")) *) a
        | A.ATTRTYPE (bt', ["cdecl", []]) -> 
            bt', a (* addAttribute (AId("cdecl")) *)
        | A.ATTRTYPE (bt', ["__cdecl__", []]) -> 
            bt', a (* addAttribute (AId("cdecl")) *)
        | _ -> bt, a
      in
      let tres = arrayToPtr (doType [] bt') in
      TFun (tres, targs, isvararg, a')

  | A.ENUM n ->
      if n = "" then E.s (E.bug "Missing enum tag");
      findCompType "enum" n a

  | A.ENUMDEF (n, eil) -> 
      if n = "" then E.s (E.bug "Missing enum tag");
      
      (* make a new name for this enumeration *)
      let n' = newAlphaName "enum" n in

      (* sm: start a scope for the enum tag values, since they *)
      (* can refer to earlier tags *)
      (startScope ());

      (* as each name,value pair is determined, this is called *)
      let rec processName kname i rest = begin
        (* add the name to the environment, but with a faked 'typ' field; *)
        (* we don't know the full type yet (since that includes all *)
        (* of the tag values), but we won't need them in here *)
        (addLocalToEnv kname (EnvEnum (i, TEnum (n', [], a))));

        (* add this tag to the list so that it ends up in the real *)
        (* environment when we're finished *)
        let newname = newAlphaName "" kname in
        (kname, (newname, i)) :: loop (increm i 1) rest
      end

      and loop i = function
          [] -> []
        | (kname, A.NOTHING) :: rest ->
            (* use the passed-in 'i' as the value, since none specified *)
            (processName kname i rest)

        | (kname, e) :: rest ->
            (* constant-eval 'e' to determine tag value *)
            let i =
              match doExp true e (AExp None) with
                c, e', _ when isEmpty c -> e'
              | _ -> E.s (E.unimp "enum with non-const initializer")
            in
            (processName kname i rest)
      in

      (* sm: now throw away the environment we built for eval'ing *)
      (* the enum tags, so we can add to the new one properly *)
      (exitScope ());

      let fields = loop zero eil in
      let res = TEnum (n', List.map (fun (_, x) -> x) fields, a) in
      (* Now we have the real host type. Set the environment properly *)
      List.iter
        (fun (n, (newname, fieldidx)) -> 
          addLocalToEnv n (EnvEnum (fieldidx, res)))
        fields;
      (* Record the enum name in the environment *)
      addLocalToEnv (kindPlusName "enum" n) (EnvTyp res);
      res

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
                    AVar (lookupVar n)
                  with Not_found -> 
                    AId n
                end
              | _ -> E.s (E.unimp "constructed attribute")
            in
            ACons (s, List.map doArg args)
      in
      doType ((List.map doAttribute a') @ a) bt

  | A.NAMED_TYPE n -> begin
      match lookupType "type" n with 
        (TNamed _) as x -> x
      | typ -> E.s (E.bug "Named type %s is not mapped correctly\n" n)
          (* TNamed(n, typ, a) *)
  end
  | A.TYPEOF e -> 
      let (c, _, t) = doExp false e (AExp None) in
      if not (isEmpty c) then
        E.s (E.unimp "typeof for a non-pure expression\n");
      t

and makeCompType (iss: bool)
                 (n: string)
                 (nglist: A.name_group list) 
                 (a: attribute list) = 
      (* Create the self cell for use in fields and forward references. Or 
       * maybe one exists already from a forward reference *)
  (* Make a new name for the structure *)
  let kind = if iss then "struct" else "union" in
  let n' = newAlphaName kind n in
  let comp = createCompInfo iss n' in
      (* Do the fields *)
  let makeFieldInfo ((bt,st,(n,nbt,a,e)) : A.single_name) : fieldinfo = 
    { fcomp    =  comp;
      fname    =  n;
      ftype    =  doType [] nbt;
      fattr    =  doAttrList a;
    } 
  in
  let flds = List.concat (List.map (doNameGroup makeFieldInfo) nglist) in
  if comp.cfields <> [] then begin
    (* This appears to be a multiply defined structure. This can happen from 
     * a construct like "typedef struct foo { ... } A, B;". This is dangerous 
     * because at the time B is processed some forward references in { ... } 
     * appear as backward references, which coild lead to circularity in 
     * the type structure. We do a thourough check and then we reuse the type 
     * for A *)
    let fieldsSig fs = List.map (fun f -> typeSig f.ftype) fs in 
    if fieldsSig comp.cfields <> fieldsSig flds then
      ignore (E.warn "%s seems to be multiply defined" (compFullName comp))
  end else 
    comp.cfields <- flds;

      (* Drop volatile from struct *)
  let a = dropAttribute a (AId("volatile")) in
  let a = dropAttribute a (AId("const")) in
  let a = dropAttribute a (AId("restrict")) in
  comp.cattr <- a;
  let res = TComp comp in
      (* There must be a self cell create for this already *)
  addLocalToEnv (kindPlusName kind n) (EnvTyp res);
  res

  
     (* Process an expression and in the process do some type checking, 
      * extract the effects as separate statements  *)
and doExp (isconst: bool)    (* In a constant *)
          (e : A.expression) 
          (what: expAction) : (chunk * exp * typ) = 
  (* A subexpression of array type is automatically turned into StartOf(e). 
   * Similarly an expression of function type is turned into StartOf *)
  let processStartOf e t = 
    match e, unrollType t with
      Lval(lv), TArray(t, _, a) -> mkAddrOfAndMark lv, TPtr(t, a)
    | Lval(lv), TFun _  -> begin
        match lv with 
          Mem(addr), NoOffset -> addr, TPtr(t, [])
        | _, _ -> mkAddrOfAndMark lv, TPtr(t, [])
    end
    | Compound _, TArray(t', _, a) -> e, t
    | _, (TArray _ | TFun _) -> 
        E.s (E.unimp "Array or function expression is not lval: %a@!"
               d_plainexp e)
    | _ -> e, t
  in
  (* Before we return we call finishExp *)
  let finishExp se e t = 
    match what with 
      ADrop -> (se, e, t)
    | AExp _ -> 
        let (e', t') = processStartOf e t in
        (se, e', t')

    | ASet (lv, lvt) -> begin
        (* See if the set was done already *)
        match e with 
          Lval(lv') when lv == lv' -> (se, e, t)
        | _ -> 
            let (e', t') = processStartOf e t in
            let (t'', e'') = castTo t' lvt e' in
            (se +++ (Set(lv, e'', lu)), e'', t'')
    end
  in
  let findField n fidlist = 
    try
      List.find (fun fid -> n = fid.fname) fidlist
    with Not_found -> E.s (E.unimp "Cannot find field %s" n)
  in
  try
    match e with
    | A.NOTHING when what = ADrop -> finishExp empty (integer 0) intType
    | A.NOTHING ->
        ignore (E.log "doExp nothing\n");
        finishExp empty 
          (Const(CStr("exp_nothing"))) (TPtr(TInt(IChar,[]),[]))

    (* Do the potential lvalues first *)          
    | A.VARIABLE n -> begin
        (* Look up in the environment *)
        try 
          let envdata = H.find env n in
          match envdata with
            EnvVar vi -> 
              finishExp empty (Lval(var vi)) vi.vtype
          | EnvEnum (tag, typ) -> 
            finishExp empty tag typ  
          | _ -> raise Not_found
        with Not_found -> 
          ignore (E.log "Cannot resolve variable %s.\n" n);
          raise Not_found
    end
    | A.INDEX (e1, e2) -> begin
        (* Recall that doExp turns arrays into StartOf pointers *)
        let (se1, e1', t1) = doExp false e1 (AExp None) in
        let (se2, e2', t2) = doExp false e2 (AExp None) in
        let se = se1 @@ se2 in
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
        finishExp se (mkMem e1'' (Index(e2'', NoOffset))) tresult

    end      
    | A.UNARY (A.MEMOF, e) -> 
        if isconst then
          E.s (E.unimp "MEMOF in constant");
        let (se, e', t) = doExp false e (AExp None) in
        let tresult = 
          match unrollType t with
          | TPtr(te, _) -> te
          | _ -> E.s (E.unimp "Expecting a pointer type in *. Got %a@!"
                        d_plaintype t)
        in
        finishExp se 
                  (mkMem e' NoOffset)
                  tresult

           (* e.str = (& e + off(str)). If e = (be + beoff) then e.str = (be 
            * + beoff + off(str))  *)
    | A.MEMBEROF (e, str) -> 
        (* member of is actually allowed if we only take the address *)
        (* if isconst then
          E.s (E.unimp "MEMBEROF in constant");  *)
        let (se, e', t') = doExp false e (AExp None) in
        let lv = 
          match e' with Lval x -> x 
          | _ -> E.s (E.unimp "Expected an lval in MEMDEROF")
        in
        let fid = 
          match unrollType t' with
            TComp comp -> findField str comp.cfields
          | _ -> E.s (E.unimp "expecting a struct with field %s" str)
        in
        let lv' = Lval(addOffsetLval (Field(fid, NoOffset)) lv) in
        finishExp se lv' fid.ftype
          
       (* e->str = * (e + off(str)) *)
    | A.MEMBEROFPTR (e, str) -> 
        if isconst then
          E.s (E.unimp "MEMBEROFPTR in constant");
        let (se, e', t') = doExp false e (AExp None) in
        let pointedt = 
          match unrollType t' with
            TPtr(t1, _) -> t1
          | TArray(t1,_,_) -> t1
          | _ -> E.s (E.unimp "expecting a pointer to a struct")
        in
        let fid = 
          match unrollType pointedt with 
            TComp comp -> findField str comp.cfields
          | x -> 
              E.s (E.unimp 
                     "expecting a struct with field %s. Found %a. t1 is %a" 
                     str d_type x d_type t')
        in
        finishExp se (mkMem e' (Field(fid, NoOffset))) fid.ftype
          
          
    | A.CONSTANT ct -> begin
        let finishCt c t = finishExp empty (Const(c)) t in
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
              finishCt res (typeOf (Const(res)))
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
                  !currentFunctionVI.vname ^
                  (if past < l then String.sub s past (l - past) else "")
                else
                  s
              with Not_found -> s
            in
            finishCt (CStr(s')) charPtrType
              
        | A.CONST_CHAR s ->
            let chr = 
              (* Convert the characted into the ASCII code *)
              match explodeString false s with
                [ c ] -> c
              | ['\\'; 'n'] -> '\n'
              | ['\\'; 't'] -> '\t'
              | ['\\'; 'r'] -> '\r'
              | ['\\'; 'b'] -> '\b'
              | ['\\'; '\\'] -> '\\'
              | ['\\'; '\''] -> '\''
              | ['\\'; '\034'] -> '\034'  (* The double quote *)
              | ['\\'; c ] when c >= '0' && c <= '9' -> 
                  Char.chr (Char.code c - Char.code '0')
              | ['\\'; c2; c1 ] when 
                c1 >= '0' && c1 <= '9' && c2 >= '0' && c2 <= '9' -> 
                  Char.chr ((Char.code c1 - Char.code '0') +
                            (Char.code c2 - Char.code '0') * 8)  
              | ['\\'; c3; c2; c1 ] when 
                c1 >= '0' && c1 <= '9' && c2 >= '0' && c2 <= '9'  
                  && c3 >= '0' && c3 <= '9' -> 
                  Char.chr ((Char.code c1 - Char.code '0') +
                            (Char.code c2 - Char.code '0') * 8 + 
                            (Char.code c3 - Char.code '0') * 64)  
              | _ -> E.s (E.unimp "Cannot transform \"%s\" into a char\n" s)
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
        | A.CONST_COMPOUND initl -> begin
            let slist : chunk ref = ref empty in
            let doPureExp t e = 
              let (se, e', t') = doExp true e (AExp(Some t)) in
              slist := !slist @@ se;
              doCastT e' t' t
            in
            (* Now recurse to find the complete offset and the type  *)
            let rec initToOffset (bt: typ) = function
                A.NO_INIT -> NoOffset, bt
              | A.INDEX_INIT (ei, i) -> 
                  let elt = 
                    match unrollType bt with 
                      TArray(bt, _, _) -> bt
                    | _ -> E.s (E.unimp "Index designator for a non-array")
                  in
                  let off, t = initToOffset elt i in
                  Index (doPureExp intType ei, off), t

              | A.FIELD_INIT (fn, i) ->
                  let fld = 
                    match unrollType bt with 
                      TComp comp when comp.cstruct -> begin
                        try
                          List.find (fun f -> f.fname = fn) comp.cfields
                        with Not_found ->
                          E.s (E.unimp "Invalid field designator %s" fn)
                      end
                    | _ -> E.s (E.unimp "Field designator %s on a non-struct"
                                  fn)
                  in 
                  let off, t = initToOffset fld.ftype i in
                  Field (fld, off), t
            in
            let rec initFields (allflds: fieldinfo list) 
                               (nextflds: fieldinfo list) 
                               (initl: (A.init * A.expression) list) 
                  : (offset option * exp) list = 
              match initl with 
                [] -> [] (* Leave remaining fields uninitialized *)
              | (i, ie) :: restinitl ->
                  let nextfields, thisoffset, thisexpt = 
                    match i with
                      A.NO_INIT -> begin
                        match nextflds with
                          [] -> E.s (E.unimp "Too many initializers")
                        | x :: xs -> xs, None, x.ftype
                      end
                    | A.INDEX_INIT _ -> 
                        E.s (E.unimp "INDEX designator for struct")
                    | A.FIELD_INIT (fn, i') -> 
                        let rec findField = function
                            [] -> E.s 
                                (E.unimp "Cannot find designated field %s"
                                   fn)
                          | f :: restf when f.fname = fn -> 
                              let off, t = initToOffset f.ftype i' in
                              restf, Some (Field(f, off)), t

                          | _ :: restf -> findField restf
                        in
                        findField allflds
                  in
                  (* Now do the expression *)
                  let ie' = doPureExp thisexpt ie in
                  (thisoffset, ie') :: 
                  initFields allflds nextfields restinitl
            in
            let rec initArray (elt: typ)   (* The element type *)
                              (nextidx: exp) 
                              (initl: (A.init * A.expression) list)
                     (* Return also the nextidx *)
                : (offset option * exp) list * exp = 
              let incrementIdx = function
                  Const(CInt(n, ik, _)) -> Const(CInt(n + 1, ik, None))
                | e -> BinOp(PlusA, e, one, intType)
              in
              match initl with
                [] -> [], nextidx
              | (i, ie) :: restinitl -> begin
                  (* If the element type is Char and the initializer is a 
                   * string literal, split the string into characters, 
                   * including the terminal 0 *)
                  match unrollType elt, ie with 
                    TInt((IChar|ISChar|IUChar), _), 
                    A.CONSTANT (A.CONST_STRING s) when i = A.NO_INIT ->
                      let chars = explodeString true s in
                      let inits' = 
                        List.map 
                          (fun c -> 
                            let cs = String.make 1 c in
                            let cs = Cprint.escape_string cs in 
                            (A.NO_INIT, A.CONSTANT (A.CONST_CHAR cs))) chars in
                      initArray elt nextidx (inits' @ restinitl)
                  | _ ->   
                      let nextidx', thisoffset, thisexpt = 
                        match i with
                          A.NO_INIT -> 
                            incrementIdx nextidx, None, elt
                        | A.FIELD_INIT _ -> 
                            E.s (E.unimp "FIELD designator for array")
                        | A.INDEX_INIT (idxe, i') -> 
                            let idxe' = doPureExp intType idxe in
                            let off, t = initToOffset elt i' in
                            incrementIdx idxe', Some (Index(idxe', off)), t
                      in
                      (* Now do the initializer *)
                      let ie'= doPureExp thisexpt ie in
                      let restoffinits, nextidx''  = 
                        initArray elt nextidx' restinitl in
                      (thisoffset, ie') :: restoffinits, nextidx''
              end
            in
            match what with (* Peek at the expected return type *)
              AExp (Some typ) -> begin
                match unrollType typ with 
                  TComp comp when comp.cstruct ->  
                    let offinits = 
                      initFields comp.cfields comp.cfields initl in
                    finishExp !slist
                              (Compound(typ, offinits)) 
                              typ
                | TArray(elt,n,a) as oldt -> 
                    let offinits, nextidx = initArray elt zero initl in
                    let newt = (* Maybe we have a length now *)
                      match n with 
                        None -> TArray(elt, Some(nextidx), a)
                      | Some _ -> oldt 
                    in
                    finishExp !slist 
                      (Compound(newt, offinits))
                      newt
                | _ -> E.s (E.unimp "bad initializer type")
              end
            | _ -> E.s (E.unimp "CONST_COMPUND. Not AExp")
        end
    end
          
    | A.TYPE_SIZEOF bt -> 
        let typ = doType [] bt in
        finishExp empty (SizeOf(typ)) uintType
          
    | A.EXPR_SIZEOF e -> 
        let (se, e', t) = doExp isconst e (AExp None) in
        (* !!!! The book says that the expression is not evaluated, so we 
           * drop the potential size-effects *)
        if isNotEmpty se then 
          ignore (E.log "Warning: Dropping side-effect in EXPR_SIZEOF\n");
        let e'' = 
          match e' with                 (* If we are taking the sizeof an 
                                         * array we must drop the StartOf  *)
            StartOf(lv) -> Lval(lv)
          | _ -> e'
        in
        finishExp empty (SizeOfE(e'')) uintType

    | A.CAST (bt, e) -> 
        let se1, typ = 
          match bt with
            A.TYPEOF et ->              (* might have side-effects *)
              let (se1, _, typ) = doExp isconst e (AExp None) in
              se1, typ
          | _ -> empty,  doType [] bt
        in
        let what' = 
          match e with 
            (* We treat the case when e is COMPOUND differently *)
            A.CONSTANT (A.CONST_COMPOUND _) -> AExp (Some typ)
          | _ -> begin
              match what with
                AExp (Some _) -> AExp (Some typ)
              | _ -> AExp None
          end
        in
        let (se, e', t) = doExp isconst e what' in
        let (t'', e'') = 
          match typ with
            TVoid _ when what = ADrop -> (t, e') (* strange GNU thing *)
          |  _ -> castTo t typ e'
        in
        finishExp (se1 @@ se) e'' t''
          
    | A.UNARY(A.MINUS, e) -> 
        let (se, e', t) = doExp isconst e (AExp None) in
        if isIntegralType t then
          let tres = integralPromotion t in
          let e'' = 
            match e' with
            | Const(CInt(i, _, _)) -> integer (- i)
            | _ -> UnOp(Neg, doCastT e' t tres, tres)
          in
          finishExp se e'' tres
        else
          if isArithmeticType t then
            finishExp se (UnOp(Neg,e',t)) t
          else
            E.s (E.unimp "Unary - on a non-arithmetic type")
        
    | A.UNARY(A.BNOT, e) -> 
        let (se, e', t) = doExp isconst e (AExp None) in
        if isIntegralType t then
          let tres = integralPromotion t in
          let e'' = UnOp(BNot, doCastT e' t tres, tres) in
          finishExp se e'' tres
        else
          E.s (E.unimp "Unary ~ on a non-integral type")
          
    | A.UNARY(A.PLUS, e) -> doExp isconst e what 
          
          
    | A.UNARY(A.ADDROF, e) -> begin
        let (se, e', t) = doExp isconst e (AExp None) in
        match e' with 
          Lval x -> finishExp se (mkAddrOfAndMark x) (TPtr(t, []))
        | CastE (t', Lval x) -> 
            finishExp se (CastE(TPtr(t', []),
                                (mkAddrOfAndMark x))) (TPtr(t', []))
        | StartOf (lv) -> (* !!! is this correct ? *)
            let tres = TPtr(typeOfLval lv, []) in
            finishExp se (mkAddrOfAndMark lv) tres

            
        | _ -> E.s (E.unimp "Expected lval for ADDROF. Got %a@!"
                      d_plainexp e')
    end
    | A.UNARY((A.PREINCR|A.PREDECR) as uop, e) -> 
        let uop' = if uop = A.PREINCR then PlusA else MinusA in
        if isconst then
          E.s (E.unimp "PREINCR or PREDECR in constant");
        let (se, e', t) = doExp false e (AExp None) in
        let lv = 
          match e' with 
            Lval x -> x
          | CastE (_, Lval x) -> x
          | _ -> E.s (E.unimp "Expected lval for ++ or --")
        in
        let tresult, result = doBinOp uop' (Lval(lv)) t one intType in
        finishExp (se +++ (Set(lv, doCastT result tresult t, lu)))
          (Lval(lv))
          tresult   (* Should this be t instead ??? *)
          
    | A.UNARY((A.POSINCR|A.POSDECR) as uop, e) -> 
        if isconst then
          E.s (E.unimp "POSTINCR or POSTDECR in constant");
        (* If we do not drop the result then we must save the value *)
        let uop' = if uop = A.POSINCR then PlusA else MinusA in
        let (se, e', t) = doExp false e (AExp None) in
        let lv = 
          match e' with 
            Lval x -> x
          | CastE (_, Lval x) -> x
          | _ -> E.s (E.unimp "Expected lval for ++ or --")
        in
        let tresult, opresult = doBinOp uop' (Lval(lv)) t one intType in
        let se', result = 
          if what <> ADrop then 
            let tmp = newTempVar t in
            se +++ (Set(var tmp, Lval(lv), lu)), Lval(var tmp)
          else
            se, Lval(lv)
        in
        finishExp (se' +++ (Set(lv, doCastT opresult tresult t, lu)))
          result
          tresult   (* Should this be t instead ??? *)
          
    | A.BINARY(A.ASSIGN, e1, e2) -> 
        if isconst then
          E.s (E.unimp "ASSIGN in constant");
        let (se1, e1', lvt) = doExp false e1 (AExp None) in
        let lv, lvt' = 
          match e1' with 
            Lval x -> x, lvt
          | CastE (_, Lval x) -> x, typeOfLval x
          | _ -> E.s (E.unimp "Expected lval for assignment. Got %a\n"
                        d_plainexp e1')
        in
        let (se2, e'', t'') = doExp false e2 (ASet(lv, lvt')) in
        finishExp (se1 @@ se2) (Lval(lv)) lvt'
          
    | A.BINARY((A.ADD|A.SUB|A.MUL|A.DIV|A.MOD|A.BAND|A.BOR|A.XOR|
      A.SHL|A.SHR|A.EQ|A.NE|A.LT|A.GT|A.GE|A.LE) as bop, e1, e2) -> 
        let bop' = match bop with
          A.ADD -> PlusA
        | A.SUB -> MinusA
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
        let (se1, e1', t1) = doExp isconst e1 (AExp None) in
        let (se2, e2', t2) = doExp isconst e2 (AExp None) in
        let tresult, result = doBinOp bop' e1' t1 e2' t2 in
        finishExp (se1 @@ se2) result tresult
          
    | A.BINARY((A.ADD_ASSIGN|A.SUB_ASSIGN|A.MUL_ASSIGN|A.DIV_ASSIGN|
      A.MOD_ASSIGN|A.BAND_ASSIGN|A.BOR_ASSIGN|A.SHL_ASSIGN|
      A.SHR_ASSIGN|A.XOR_ASSIGN) as bop, e1, e2) -> 
        if isconst then
          E.s (E.unimp "op_ASSIGN in constant");
        let bop' = match bop with          
          A.ADD_ASSIGN -> PlusA
        | A.SUB_ASSIGN -> MinusA
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
        let (se1, e1', t1) = doExp false e1 (AExp None) in
        let lv1 = 
          match e1' with Lval x -> x
          | _ -> E.s (E.unimp "Expected lval for assignment")
        in
        let (se2, e2', t2) = doExp false e2 (AExp None) in
        let tresult, result = doBinOp bop' e1' t1 e2' t2 in
        finishExp (se1 @@ se2 +++ (Set(lv1, result, lu)))
          (Lval(lv1))
          tresult
          
    | A.BINARY((A.AND|A.OR), e1, e2) ->
        let tmp = var (newTempVar intType) in
        finishExp (doCondition e (empty +++ (Set(tmp, integer 1, lu)))
                                 (empty +++ (Set(tmp, integer 0, lu))))
          (Lval tmp)
          intType
          
    | A.UNARY(A.NOT, e) -> 
        let tmp = var (newTempVar intType) in
        let (se, e', t) as rese = doExp isconst e (AExp None) in
        ignore (checkBool t e');
        finishExp se (UnOp(LNot, e', intType)) intType
(*   We could use this code but it confuses the translation validation
        finishExp 
          (doCondition e [mkSet tmp (integer 0)] [mkSet tmp (integer 1)])
          (Lval tmp)
          intType
*)
          
    | A.CALL(f, args) -> 
        if isconst then
          E.s (E.unimp "CALL in constant");
        let (sf, f', ft') = 
          match f with                  (* Treat the VARIABLE case separate 
                                         * becase we might be calling a 
                                         * function that does not have a 
                                         * prototype. In that case assume it 
                                         * takes INTs as arguments  *)
            A.VARIABLE n -> begin
              try
                let vi = lookupVar n in
                (empty, Lval(var vi), vi.vtype) (* Found. Do not use 
                                                 * finishExp. Simulate what = 
                                                 * AExp None  *)
              with Not_found -> begin
                ignore (E.log 
                          "Warning: Calling function %s without prototype\n"
                          n);
                let ftype = TFun(intType, [], false, []) in
                (* Add a prototype to the environment *)
                let proto, _ = makeGlobalVarinfo (makeGlobalVar n ftype) in 
                (* Add it to the file as well *)
                theFile := GDecl (proto, lu) :: !theFile;
                (empty, Lval(var proto), ftype)
              end
            end
          | _ -> doExp false f (AExp None) 
        in
        (* Get the result type and the argument types *)
        let (resType, argTypes, isvar, f'') = 
          match unrollType ft' with
            TFun(rt,at,isvar,a) -> (rt,at,isvar,f')
          | TPtr (t, _) -> begin
              match unrollType t with 
                TFun(rt,at,isvar,a) -> (* Make the function pointer 
                                            * explicit  *)
                  let f'' = 
                    match f' with
                      StartOf lv -> Lval(lv)
                    | _ -> Lval(Mem(f'), NoOffset)
                  in
                  (rt,at,isvar, f'')
              | x -> E.s (E.unimp 
                            "Unexpected type of the called function %a: %a" 
                            d_exp f' d_type x)
          end
          | x ->  E.s (E.unimp 
                         "Unexpected type of the called function %a: %a" 
                         d_exp f' d_type x)
        in
        (* Drop certain qualifiers from the result type *)
        let resType' =  
          typeRemoveAttributes [AId("cdecl"); AId("__cdecl__")] resType in
        (* Do the arguments. In REVERSE order !!! Both GCC and MSVC do this *)
        let rec loopArgs 
            : varinfo list * A.expression list 
          -> (chunk * exp list) = function
            | (args, []) -> 
                if args <> [] then
                  ignore (E.warn "Too few arguments in call to %a" d_exp f');
                (empty, [])

            | (varg :: atypes, a :: args) -> 
                let (ss, args') = loopArgs (atypes, args) in
                let (sa, a', att) = doExp false a (AExp (Some varg.vtype)) in
                let (at'', a'') = castTo att varg.vtype a' in
                (ss @@ sa, a'' :: args')
                  
            | ([], args) -> (* No more types *)
                if not isvar then 
                  ignore (E.warn "Too many arguments in call to %a" d_exp f');
                let rec loop = function
                    [] -> (empty, [])
                  | a :: args -> 
                      let (ss, args') = loop args in
                      let (sa, a', at) = doExp false a (AExp None) in
                      (ss @@ sa, a' :: args')
                in
                loop args
        in
        let (sargs, args') = loopArgs (argTypes, args) in
        begin
          match what with 
            ADrop -> 
              finishExp 
                (sf @@ sargs +++ (Call(None,f'',args', lu)))
                (integer 0) intType
              (* Set to a variable of corresponding type *)
          | ASet((Var vi, NoOffset) as lv, vtype) -> 
              let mustCast = typeSig resType' <> typeSig vtype in
              finishExp 
                (sf @@ sargs 
                 +++ (Call(Some (vi, mustCast),f'',args', lu)))
                (Lval(lv))
                vtype

          | _ -> begin
              (* Must create a temporary *)
              match f'', args' with     (* Some constant folding *)
                Lval(Var fv, NoOffset), [Const _] 
                  when fv.vname = "__builtin_constant_p" ->
                    finishExp (sf @@ sargs) (integer 1) intType
              | _ -> 
                  let tmp, restyp', iscast = 
                    match what with
                      AExp (Some t) -> 
                        newTempVar t, t, 
                        typeSig t <> typeSig resType'
                    | _ -> newTempVar resType', resType', false
                  in
                  let i = Call(Some (tmp, iscast),f'',args', lu) in
                  finishExp (sf @@ sargs +++ i) (Lval(var tmp)) restyp'
          end
        end
          
    | A.COMMA el -> 
        if isconst then 
          E.s (E.unimp "COMMA in constant");
        let rec loop sofar = function
            [e] -> 
              let (se, e', t') = doExp false e what in (* Pass on the action *)
              finishExp (sofar @@ se) e' t' (* does not hurt to do it twice *)
          | e :: rest -> 
              let (se, _, _) = doExp false e ADrop in
              loop (sofar @@ se) rest
          | [] -> E.s (E.unimp "empty COMMA expression")
        in
        loop empty el
          
    | A.QUESTION (e1,e2,e3) when what = ADrop -> 
        if isconst then
          E.s (E.bug "QUESTION with ADrop in constant");
        let (se3,_,_) = doExp false e3 ADrop in
        let se2 = 
          match e2 with 
            A.NOTHING -> skipChunk
          | _ -> let (se2,_,_) = doExp false e2 ADrop in se2
        in
        finishExp (doCondition e1 se2 se3) (integer 0) intType
          
    | A.QUESTION (e1, e2, e3) -> begin (* what is not ADrop *)
                    (* Do these only to collect the types  *)
        let se2, e2', t2' = 
          match e2 with 
            A.NOTHING -> (* A GNU thing. Use e1 as e2 *) 
              doExp isconst e1 (AExp None)
          | _ -> doExp isconst e2 (AExp None) in 
        let se3, e3', t3' = doExp isconst e3 (AExp None) in
        let tresult = conditionalConversion e2' t2' e3' t3' in
        match e2 with 
          A.NOTHING -> 
              let tmp = var (newTempVar tresult) in
              let (se1, _, _) = doExp isconst e1 (ASet(tmp, tresult)) in
              let (se3, _, _) = doExp isconst e3 (ASet(tmp, tresult)) in
              finishExp (se1 @@ ifChunk (Lval(tmp)) lu
                                  skipChunk se3)
                (Lval(tmp))
                tresult
        | _ -> 
            if isEmpty se2 && isEmpty se3 && isconst then begin 
                                       (* Use the Question *)
              let se1, e1', t1 = doExp isconst e1 (AExp None) in
              ignore (checkBool t1 e1');
              let e2'' = doCastT e2' t2' tresult in
              let e3'' = doCastT e3' t3' tresult in
              let resexp = 
                match e1' with
                  Const(CInt(i, _, _)) when i <> 0 -> e2''
                | Const(CInt(0, _, _)) -> e3''
                | _ -> Question(e1', e2'', e3'')
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
              let (se2, _, _) = doExp isconst e2 (ASet(lv, lvt)) in
              let (se3, _, _) = doExp isconst e3 (ASet(lv, lvt)) in
              finishExp (doCondition e1 se2 se3) (Lval(lv)) tresult
    end

    | A.GNU_BODY b -> begin
        (* Find the last A.COMPUTATION *)
        let rec findLastComputation = function
            A.BSTM s :: _  -> 
              let rec findLast = function
                  A.SEQUENCE (_, s) -> findLast s
                | (A.COMPUTATION _) as s -> s
                | _ -> raise Not_found
              in
              findLast s
          | _ :: rest -> findLastComputation rest
          | [] -> raise Not_found
        in
        (* Save the previous data *)
        let old_gnu = ! gnu_body_result in
        let lastComp, isvoidbody = 
          try findLastComputation (List.rev b), false 
          with Not_found -> A.NOP, true
        in
        (* Prepare some data to be filled by doExp *)
        let data : (exp * typ) option ref = ref None in
        gnu_body_result := (lastComp, data);
        let se = doBody b in
        gnu_body_result := old_gnu;
        match !data with
          None when isvoidbody -> finishExp se zero voidType
        | None -> E.s (E.unimp "Cannot find COMPUTATION in GNU.body")
        | Some (e, t) -> finishExp se e t
    end
  with e -> begin
    ignore (E.log "error in doExp (%s)@!" (Printexc.to_string e));
    (i2c (dInstr (dprintf "booo_exp(%s)" (Printexc.to_string e))), 
     integer 0, intType)
  end
    
(* bop is always the arithmetic version. Change it to the appropriate pointer 
 * version if necessary *)
and doBinOp (bop: binop) (e1: exp) (t1: typ) (e2: exp) (t2: typ) : typ * exp =
  let constFold bop' e1' e2' tres = 
    if isIntegralType tres then
      let newe = 
        let rec mkInt = function
            Const(CChr c) -> Const(CInt(Char.code c, IInt, None))
          | CastE(TInt _, e) -> mkInt e
          | e -> e
        in
        match bop', mkInt e1', mkInt e2' with
          PlusA, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 + i2)
        | MinusA, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 - i2)
        | Mult, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 * i2)
        | Div, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 / i2)
        | Mod, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 mod i2)
        | BAnd, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 land i2)
        | BOr, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 lor i2)
        | BXor, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 lxor i2)
        | Shiftlt, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 lsl i2)
        | Shiftrt, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (i1 lsr i2)
        | Eq, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (if i1 = i2 then 1 else 0)
        | Ne, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (if i1 <> i2 then 1 else 0)
        | Le, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (if i1 <= i2 then 1 else 0)
        | Ge, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (if i1 >= i2 then 1 else 0)
        | Lt, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (if i1 < i2 then 1 else 0)
        | Gt, Const(CInt(i1,_,_)),Const(CInt(i2,_,_)) -> 
            integer (if i1 > i2 then 1 else 0)
        | _ -> BinOp(bop', e1', e2', tres)
      in
      tres, newe
    else
      tres, BinOp(bop', e1', e2', tres)
  in
  let doArithmetic () = 
    let tres = arithmeticConversion t1 t2 in
    (* Keep the operator since it is arithmetic *)
    constFold bop (doCastT e1 t1 tres) (doCastT e2 t2 tres) tres
  in
  let doArithmeticComp () = 
    let tres = arithmeticConversion t1 t2 in
    (* Keep the operator since it is arithemtic *)
    constFold bop (doCastT e1 t1 tres) (doCastT e2 t2 tres) intType
  in
  let doIntegralArithmetic () = 
    let tres = unrollType (arithmeticConversion t1 t2) in
    match tres with
      TInt _ -> constFold bop (doCastT e1 t1 tres) (doCastT e2 t2 tres) tres
    | _ -> E.s (E.unimp "%a operator on a non-integer type" d_binop bop)
  in
  let bop2point = function
      MinusA -> MinusPP
    | Eq -> EqP | Ge -> GeP | Ne -> NeP | Gt -> GtP | Le -> LeP | Lt -> LtP
    | _ -> E.s (E.bug "bop2point")
  in
  let pointerComparison e1 e2 = 
    (* Cast both sides to the same kind of pointer, that is preferably not 
     * void* *)
    let commontype = 
      match unrollType t1, unrollType t2 with
        TPtr(TVoid _, _), _ -> t2
      | _, TPtr(TVoid _, _) -> t1
      | _, _ -> t1
    in
    constFold (bop2point bop) (doCastT e1 t1 commontype) 
                              (doCastT e2 t2 commontype) intType
  in

  match bop with
    (Mult|Div) -> doArithmetic ()
  | (Mod|BAnd|BOr|BXor) -> doIntegralArithmetic ()
  | (Shiftlt|Shiftrt) -> (* ISO 6.5.7. Only integral promotions. The result 
                          * has the same type as the left hand side *)
      if !msvcMode then
        (* MSVC has a bug. We duplicate it here *)
        doIntegralArithmetic ()
      else
        let t1' = integralPromotion t1 in
        let t2' = integralPromotion t2 in
        constFold bop (doCastT e1 t1 t1') (doCastT e2 t2 t2') t1'

  | (PlusA|MinusA) 
      when isArithmeticType t1 && isArithmeticType t2 -> doArithmetic ()
  | (Eq|Ne|Lt|Le|Ge|Gt) 
      when isArithmeticType t1 && isArithmeticType t2 -> 
        doArithmeticComp ()
  | PlusA when isPointerType t1 && isIntegralType t2 -> 
      constFold PlusPI e1 (doCastT e2 t2 (integralPromotion t2)) t1
  | PlusA when isIntegralType t1 && isPointerType t2 -> 
      constFold PlusPI e2 (doCastT e1 t1 (integralPromotion t1)) t2
  | MinusA when isPointerType t1 && isIntegralType t2 -> 
      constFold MinusPI e1 (doCastT e2 t2 (integralPromotion t2)) t1
  | (MinusA|Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isPointerType t2 ->
      pointerComparison e1 e2
  | (Eq|Ne) when isPointerType t1 && 
                 (match e2 with Const(CInt(0,_,_)) -> true | _ -> false) -> 
      pointerComparison e1 (doCastT e2 t2 t1)
  | (Eq|Ne) when isPointerType t2 && 
                 (match e1 with Const(CInt(0,_,_)) -> true | _ -> false) -> 
      pointerComparison (doCastT e1 t1 t2) e2


  | (Eq|Ne|Le|Lt|Ge|Gt|Eq|Ne) when isPointerType t1 && isArithmeticType t2 ->
      ignore (E.warn "Comparison of pointer and non-pointer");
      doBinOp bop (doCastT e1 t1 t2) t2 e2 t2
  | (Eq|Ne|Le|Lt|Ge|Gt|Eq|Ne) when isArithmeticType t1 && isPointerType t2 ->
      ignore (E.warn "Comparison of pointer and non-pointer");
      doBinOp bop e1 t1 (doCastT e2 t2 t1) t1

  | _ -> E.s (E.unimp "doBinOp: %a\n" d_plainexp (BinOp(bop,e1,e2,intType)))

(* A special case for conditionals *)
and doCondition (e: A.expression) 
                (st: chunk)
                (sf: chunk) : chunk = 
  match e with 
  | A.BINARY(A.AND, e1, e2) ->
      let (sf1, sf2) = 
        (* If sf is small then will copy it *)
        if canDuplicate sf then
          (sf, sf)
        else begin
          incr labelId;
          let lab = "L" ^ (string_of_int !labelId) in
          (gotoChunk lab lu, consLabel lab sf)
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
          (gotoChunk lab lu, consLabel lab st)
        end
      in
      let st' = st1 in
      let sf' = doCondition e2 st2 sf in
      doCondition e1 st' sf'

  | A.UNARY(A.NOT, e) -> doCondition e sf st

  | _ -> begin
      let (se, e, t) as rese = doExp false e (AExp None) in
      ignore (checkBool t e);
      match e with 
        Const(CInt(i,_,_)) when i <> 0 && canDrop sf -> se @@ st
      | Const(CInt(0,_,_)) when canDrop st -> se @@ sf
      | _ -> se @@ ifChunk e lu st sf
  end

and doPureExp (e : A.expression) : exp = 
  let (se, e', _) = doExp true e (AExp None) in
  if isNotEmpty se then
   E.s (E.unimp "doPureExp: not pure");
  e'


and createGlobal ((_,_,(n,nbt,a,e)) as sname : A.single_name) =
  try
            (* Make a first version of the varinfo *)
    let vi = makeVarInfo true locUnknown sname in
            (* Do the initializer and complete the array type if necessary *)
    let init = 
      if e = A.NOTHING then 
        None
      else 
        let (se, e', et) = doExp true e (AExp (Some vi.vtype)) in
        if isNotEmpty se then 
          E.s (E.unimp "global initializer");
        let (_, e'') = castTo et vi.vtype e' in
        match vi.vtype, e', et with
          TArray(TInt((IChar|IUChar|ISChar), _) as bt, oldl, a),
          Const(CStr s), _ -> 
                    (* Change arrays initialized with strings into array 
                     * initializers  *)
            (match oldl with 
              None ->  (* See if we have a length now *)
                vi.vtype <- TArray(bt, 
                                   Some (integer 
                                           (String.length s + 1)),
                                   a)
            | _ -> ());
            let chars = explodeString true s in
            let inits = 
              List.map (fun c -> 
                (None, 
                 Const(CInt(Char.code c, IChar, 
                            Some ("'" ^ Char.escaped c ^ "'"))))) chars
            in
            Some (Compound(vi.vtype, inits))
              
        | TArray(bt, None, a), _, TArray(_, Some _, _) -> 
            vi.vtype <- et;
            Some e''
              
        | _ ->  Some e''
    in                   

    (* sm: if it's a function prototype, and the storage class *)
    (* isn't specified, make it 'extern'; this fixes a problem *)
    (* with no-storage prototype and static definition *)
    if (isFunctionType vi.vtype &&
        vi.vstorage = NoStorage) then (
      (*(trace "sm" (dprintf "adding extern to prototype of %s\n" n));*)
      vi.vstorage <- Extern
    );

    let vi, alreadyDef = makeGlobalVarinfo vi in
    if not alreadyDef then begin(* Do not add declarations after def *)
      if vi.vstorage = Extern then 
        if init = None then 
          theFile := GDecl (vi, lu) :: !theFile
        else
          E.s (E.unimp "%s is extern and with initializer" vi.vname)
      else
                (* If it has fucntion type it is a declaration *)
        if isFunctionType vi.vtype then begin
          if init <> None then
            E.s (E.bug "Function declaration with initializer (%s)\n"
                   vi.vname);
          theFile := GDecl(vi, lu) :: !theFile
        end else
          theFile := GVar(vi, init, lu) :: !theFile
    end
  with e -> begin
    ignore (E.log "error in CollectGlobal (%s)\n" 
              (Printexc.to_string e));
    theFile := GAsm("booo - error in global " ^ n,lu) :: !theFile
  end
(*
          ignore (E.log "Env after processing global %s is:@!%t@!" 
                    n docEnv);
          ignore (E.log "Alpha after processing global %s is:@!%t@!" 
                    n docAlphaTable)
*)

(* Must catch the Static local variables.Make them global *)
and createLocal = function
    ((bt,A.STATIC,(n,nbt,a,e)) as sname : A.single_name) -> 
      let vi = makeVarInfo true locUnknown sname in (* Make it global *)
      (* Now alpha convert it to make sure that it does not conflict with 
       * existing globals or locals from this function. Make it local 
       * temporarily so that it will be added to the scope cleanup tables *)
      vi.vglob <- false;
      let vi = alphaConvertVarAndAddToEnv true vi in
      ignore (E.log "static local: %s\n" vi.vname);
      vi.vglob <- true;
      let init = 
        if e = A.NOTHING then 
          None
        else begin 
          let (se, e', et) = doExp true e (AExp (Some vi.vtype)) in
          if isNotEmpty se then 
            E.s (E.unimp "global static initializer");
          let (_, e'') = castTo et vi.vtype e' in
          Some e''
        end
      in
      theFile := GVar(vi, init, lu) :: !theFile;
      empty

  | ((bt,st,(n,nbt,a,e)) as sname : A.single_name) -> 
      let vi = makeVarInfo false locUnknown sname in
      let vi = alphaConvertVarAndAddToEnv true vi in        (* Replace vi *)
      if e = A.NOTHING then
        skipChunk
      else begin
        let (se, e', et) = doExp false e (AExp (Some vi.vtype)) in
        (match vi.vtype, e', et with 
            (* We have a length now *)
          TArray(_,None, _), _, TArray(_, Some _, _) -> vi.vtype <- et
            (* Initializing a local array *)
        | TArray(TInt((IChar|IUChar|ISChar), _) as bt, None, a),
             Const(CStr s), _ -> 
               vi.vtype <- TArray(bt, 
                                  Some (integer (String.length s + 1)),
                                  a)
        | _, _, _ -> ());
        let (_, e'') = castTo et vi.vtype e' in
        se @@ (doAssign (Var vi, NoOffset) e'')
      end
          
          
and doDecl : A.definition -> chunk = function
  | A.DECDEF ng ->
      let stmts = doNameGroup createLocal ng in
      List.fold_left (fun acc c -> acc @@ c) empty stmts
        
  | A.TYPEDEF ng -> doTypedef ng; empty

  | A.ONLYTYPEDEF (bt, _, _) -> doOnlyTypedef bt; empty

  | _ -> E.s (E.unimp "doDecl")

and doTypedef (ng: A.name_group) = 
  let createTypedef ((_,_,(n,nbt,a,_)) : A.single_name) = 
    try
      let newTyp = doType (doAttrList a) nbt in
            (* Create a new name for the type *)
      let n' = newAlphaName "type" n in
      let namedTyp = TNamed(n', newTyp, []) in
            (* Register the type. register it as local because we might be in 
             * a local context *)
      addLocalToEnv (kindPlusName "type" n) (EnvTyp namedTyp);
      theFile := GType (n', newTyp, lu) :: !theFile
    with e -> begin
      ignore (E.log "Error on A.TYPEDEF (%s)\n"
                (Printexc.to_string e));
      theFile := GAsm ("booo_typedef:" ^ n, lu) :: !theFile
    end
  in
  ignore (doNameGroup createTypedef ng)
    

and doOnlyTypedef (bt: A.base_type) : unit = 
  try
    let newTyp = doType [] bt in
          (* doType will register the type. Put a special GType in the file *)
    theFile := GType ("", newTyp, lu) :: !theFile
  with e -> begin
    ignore (E.log "Error on A.ONLYTYPEDEF (%s)\n"
              (Printexc.to_string e));
    theFile := GAsm ("booo_typedef", lu) :: !theFile
  end

and doAssign (lv: lval) : exp -> chunk = function   
                             (* We must break the compound assignment into 
                              * atomic ones  *)
  | Compound (t, initl) -> begin
      match unrollType t with 
        TArray(t, _, _) -> 
          let rec loop = function
              _, [] -> empty
            | i, (None, e) :: el -> 
                let res = loop ((i + 1), el) in
                let newlv = mkMem (mkAddrOfAndMark lv) 
                                  (Index(integer i, NoOffset)) in
                let newlv = 
                  match newlv with 
                    Lval x -> x | _ -> E.s (E.bug "doAssign: mem")
                in
                (doAssign newlv e) @@ res
            | _ -> E.s (E.unimp "doAssign. Compound")
          in
          loop (0, initl)

      | TComp comp when comp.cstruct ->
          let rec loop = function
              [], [] -> empty
            | f :: fil, (None, e) :: el -> 
                let res = loop (fil, el) in
                (doAssign (addOffsetLval (Field(f, NoOffset)) lv) e) @@ res
            | _, _ -> E.s (E.unimp "fields in doAssign")
          in
          loop (comp.cfields, initl)
      | _ -> E.s (E.bug "Unexpected type of Compound")
  end

   (* An array initialized with a string *)
  | Const(CStr s) as e -> begin
      let lvt = typeOfLval lv in
      match unrollType lvt with 
        TArray(_, Some _, _) ->
          (* See if strncpy was already declared *)
          if not (H.mem env "strncpy") then begin
            theFile := GDecl (strncpyFun.svar, lu) :: !theFile;
            H.add env "strncpy" (EnvVar strncpyFun.svar)
          end;
          i2c (Call(None, Lval (var strncpyFun.svar),
                    [ StartOf lv; e; SizeOf (lvt) ], lu))

      | TArray(_, None, _) -> E.s (E.unimp "initialization with a string")
      | _ -> i2c (Set(lv, e, lu))
  end
  | e -> i2c (Set(lv, e, lu))

  (* Now define the processors for body and statement *)
and doBody (b : A.body) : chunk = 
  startScope ();
    (* Do the declarations and the initializers and the statements. *)
  let rec loop = function
      [] -> empty
    | A.BDEF d :: rest -> 
        let res = doDecl d in  (* !!! @ evaluates its arguments backwards *)
        res @@ loop rest
    | A.BSTM s :: rest -> 
        let res = doStatement s in
        res @@ loop rest
  in
  let res = afterConversion (loop b) in
  exitScope ();
  res
      
and doStatement (s : A.statement) : chunk = 
  try
    match s with 
      A.NOP -> skipChunk
    | A.COMPUTATION e -> 
        let (lasts, data) = !gnu_body_result in
        if lasts == s then begin      (* This is the last in a GNU_BODY *)
          let (s', e', t') = doExp false e (AExp None) in
          data := Some (e', t');      (* Record the result *)
          s'
        end else
          let (s', _, _) = doExp false e ADrop in
            (* drop the side-effect free expression *)
            (* And now do some peep-hole optimizations *)
          s'
            
    | A.BLOCK b -> doBody b

    | A.SEQUENCE (s1, s2) -> 
        (doStatement s1) @@ (doStatement s2)

    | A.IF(e,st,sf) -> 
        let st' = doStatement st in
        let sf' = doStatement sf in
        doCondition e st' sf'

    | A.WHILE(e,s) ->  
        startLoop true;
        let s' = doStatement s in
        exitLoop ();
        loopChunk ((doCondition e skipChunk
                      (breakChunk lu)) 
                   @@ s')
          
    | A.DOWHILE(e,s) -> 
        startLoop false;
        let s' = doStatement s in
        let s'' = consLabContinue (doCondition e skipChunk (breakChunk lu))
        in
        exitLoop ();
        loopChunk (s' @@ s'')
          
    | A.FOR(e1,e2,e3,s) -> begin
        let (se1, _, _) = doExp false e1 ADrop in
        let (se3, _, _) = doExp false e3 ADrop in
        startLoop false;
        let s' = doStatement s in
        let s'' = consLabContinue se3 in
        exitLoop ();
        match e2 with
          A.NOTHING -> (* This means true *)
            se1 @@ loopChunk (s' @@ s'')
        | _ -> 
            se1 @@ loopChunk ((doCondition e2 skipChunk (breakChunk lu))
                              @@ s' @@ s'')
    end
    | A.BREAK -> breakChunk lu
          
    | A.CONTINUE -> continueOrLabelChunk lu
          
    | A.RETURN A.NOTHING -> returnChunk None lu
    | A.RETURN e -> 
        let (se, e', et) = doExp false e (AExp (Some !currentReturnType)) in
        let (et'', e'') = castTo et (!currentReturnType) e' in
        se @@ (returnChunk (Some e'') lu)
               
    | A.SWITCH (e, s) -> 
        let (se, e', et) = doExp false e (AExp (Some intType)) in
        let (et'', e'') = castTo et intType e' in
        let s' = doStatement s in
        se @@ (switchChunk e'' s' lu)
               
    | A.CASE (e, s) -> 
        let (se, e', et) = doExp false e (AExp None) in
          (* let (et'', e'') = castTo et (TInt(IInt,[])) e' in *)
        let i = 
          match se, e' with
            ch, Const (CInt (i,_, _)) when isEmpty ch -> i
          | ch, Const (CChr c) when isEmpty ch -> Char.code c
          | _ -> E.s (E.unimp "non-int case")
        in
        caseChunk i lu (doStatement s)
                    
    | A.DEFAULT s -> defaultChunk lu (doStatement s)
                     
    | A.LABEL (l, s) -> 
        consLabel l (doStatement s)
                     
    | A.GOTO l -> gotoChunk l lu
          
    | A.ASM (tmpls, isvol, outs, ins, clobs) -> 
      (* Make sure all the outs are variables *)
        let temps : (lval * varinfo) list ref = ref [] in
        let stmts : chunk ref = ref empty in
        let outs' = 
          List.map 
            (fun (c, e) -> 
              let (se, e', t) = doExp false e (AExp None) in
              let lv = 
                match e' with Lval x -> x
                | _ -> E.s (E.unimp "Expected lval for ASM outputs")
              in
              stmts := !stmts @@ se;
              (c, lv)) outs 
        in
      (* Get the side-effects out of expressions *)
        let ins' = 
          List.map 
            (fun (c, e) -> 
              let (se, e', et) = doExp false e (AExp None) in
              stmts := !stmts @@ se;
              (c, e'))
            ins
        in
        !stmts @@ 
        (i2c (Asm(tmpls, isvol, outs', ins', clobs, lu)))
  with e -> begin
    (ignore (E.log "Error in doStatement (%s)\n" (Printexc.to_string e)));
    consLabel "booo_statement" empty
  end


    
(* Translate a file *)
let convFile fname dl = 
  ignore (E.log "Cabs2cil conversion\n");
  (* Clean up the global types *)
  E.hadErrors := false;
  theFile := [];
  H.clear compInfoNameEnv;
  (* Setup the built-ins *)
  let _ = 
    let fdec = emptyFunction "__builtin_constant_p" in
    let argp  = makeLocalVar fdec "x" intType in
    fdec.svar.vtype <- TFun(intType, [ argp ], false, []);
    alphaConvertVarAndAddToEnv true fdec.svar
  in
  (* Now do the globals *)
  let doOneGlobal = function
      A.TYPEDEF ng -> doTypedef ng

    | A.ONLYTYPEDEF (A.NO_TYPE, _, _) -> ()

    | A.ONLYTYPEDEF (bt,_,_) -> doOnlyTypedef bt

    | A.DECDEF ng -> 
        ignore (doNameGroup createGlobal ng)
          
    | A.GLOBASM s -> theFile := GAsm (s, lu) :: !theFile
    | A.PRAGMA a -> theFile := GPragma (doAttr a, lu) :: !theFile

    | A.FUNDEF (((bt,st,(n,bt',funattr,_)) : A.single_name), 
                 (body : A.body)) -> 
        E.withContext
          (fun _ -> dprintf "2cil: %s" n)
          (fun _ ->
            try
             (* Reset the local identifier so that formals are created with 
              * the proper IDs  *)
              resetLocals ();
                                        (* Do the type *)
              let (returnType, formals, isvararg, a) = 
                match unrollType (doType [] bt') with 
                  TFun(rType, formals, isvararg, a) -> 
                    (rType, formals, isvararg, a)
                | x -> E.s (E.bug "non-function type: %a." d_type x)
              in
              (* Record the returnType for doStatement *)
              currentReturnType   := returnType;
              (* Setup the environment. Add the formals to the locals. Maybe 
               * they need alpha-conv  *)
              startScope ();
              let formals' = List.map (alphaConvertVarAndAddToEnv true) 
                  formals in
              let ftype = TFun(returnType, formals', isvararg, a) in
              (* Add the function itself to the environment. Just in case we 
               * have recursion and no prototype.  *)
              (* Make a variable out of it and put it in the environment *)
              let thisFunctionVI, alreadyDef = 
                makeGlobalVarinfo 
                  { vname = n;
                    vtype = ftype;
                    vglob = true;
                    vid   = newVarId n true;
                    vdecl = lu;
                    vattr = doAttrList funattr;
                    vaddrof = false;
                    vstorage = doStorage st;
                    vreferenced = false;   (* sm *)
                  }
              in
              if alreadyDef then
                E.s (E.unimp "There is a definition already for %s" n);
              currentFunctionVI := thisFunctionVI;
              (* Now do the body *)
              let s = doBody body in
              (* Finish everything *)
              exitScope ();
              let (maxid, locals) = endFunction formals' in
              let fdec = { svar     = thisFunctionVI;
                           slocals  = locals;
                           sformals = formals';
                           smaxid   = maxid;
                           sbody    = mkFunctionBody s;
                         } 
              in
              setFormals fdec formals'; (* To make sure sharing is proper *)
              theFile := GFun (fdec,lu) :: !theFile
            with e -> begin
              ignore (E.log "error in collectFunction %s: %s\n" 
                        n (Printexc.to_string e));
              theFile := GAsm("error in function ", lu) :: !theFile
            end)
          () (* argument of E.withContext *)
  in
  List.iter doOneGlobal dl;
  (* We are done *)
  { fileName = fname;
    globals  = List.rev (! theFile);
    globinit = None;
    globinitcalled = false;
  } 


