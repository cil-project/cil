open Pretty
module E = Errormsg
module H = Hashtbl

(*
 * CIL: An intermediate language for analyzing C progams.
 *
 * Version Tue Dec 12 15:21:52 PST 2000 
 * Scott McPeak, George Necula, Wes Weimer
 *
 *)
(* CIL is intended to be an IL for source-to-source transformations of C 
 * programs. In the process of converting preprocessed C source to CIL most 
 * of the syntactic sugar is removed and type checking is performed. CIL has 
 * the following properties:

 - has a pretty printer that should print valid source. There is also a 
 * pretty printer that prints abstract syntax (for debugging)

 - all local variables are pulled to the start of a function. Their names are 
 * changed to be unique and to avoid conflicts with global variables

 - all forms of source-level loops are changed into a single Loop construct 
 * that loops forever except if a break, return or goto is encountered. In 
 * many cases the continue statement is turned into a goto. 

 - all implicit integer promotions, argument promotions and arithmetic 
 * conversions are turned into explicit casts.

 - all integer constants have the proper type attached to them

*)
(*
 * Note: you may *NOT* change the order of the fields or the order in
 * which disjoint union choices are presented. The C translation code
 * has those values hard-wired.
 *)

(* TODO
   - inner struct/union/enum/typedef tags
   - clean up attributes
   - functions vs. function pointers
   - type of sizeof is hardwired to UInt
   - integerFits is hardwired to true
   - in cabs2cil we drop the volatile sometimes
*)

(* where did some construct originally appear in the source code? *)
type location = { 
    line: int;				(* -1 means "do not know" *)
    col: int;
    file: string; 
}

let locUnknown = { line = -1; col = -1; file = ""; }

(* Information about a variable. Use one of the makeLocalVar, makeTempVar or 
 * makeGlobalVar to create instances of this data structure. These structures a
 * re shared by all references to the variable. So, you can change the name
 * easily, for example *)
type varinfo = { 
    vid: int;		(* Unique integer indentifier. For globals this is a 
                         * hash of the name. Locals are numbered from 0 
                         * starting with the formal arguments. This field 
                         * will be set for you if you use one of the 
                         * makeLocalVar, makeTempVar or makeGlobalVar *)
    mutable vname: string;				
    vglob: bool;	(* Is this a global variable? *)

    mutable vtype: typ;                 (* The declared type *)
    mutable vdecl: location;            (* where was this variable declared? *)
    mutable vattr: attribute list;
    mutable vstorage: storage;
    mutable vaddrof: bool;              (* Has its address taken *)
} 

                                        (* Storage-class information *)
and storage = 
    NoStorage |                         (* The default storage *)
    Static | 
    Register | 
    Extern

(* Information about a struct/union field *)
and fieldinfo = { 
    fstruct: string;                    (* The name of the host struct/union*)
    fname: string;                      (* The name of the field. Might be 
                                         * "___missing_field_name" in which 
                                         * case it is not printed *)
    mutable ftype: typ;
    mutable fattr: attribute list;
}

(* what is the type of an expression? *)
and typ =
    TVoid of attribute list
  | TInt of ikind * attribute list
  | TBitfield of ikind * int * attribute list
  | TFloat of fkind * attribute list
           (* name, tags with values, attributes *)
  | TEnum of string * (string * int) list * attribute list

  | TPtr of typ * attribute list        (* Pointer type *)

              (* base type and length *)
  | TArray of typ * exp option * attribute list

               (* name, fields, attributes *) 
  | TStruct of string * fieldinfo list * attribute list 
  | TUnion of string * fieldinfo list * attribute list

               (* result, args, isVarArg, attributes *)
  | TFun of typ * varinfo list * bool * attribute list

           (* A reference to a struct or a union. The argument is "struct x" 
            * or "union x". The reference is resolved using the hash table 
            * "forwardTypeMap"  *)
  | TForward of string

  | TNamed of string * typ * attribute list (* From a typedef. The attributes 
                                             * are in addition to the 
                                             * attributes of the named type  *)

(* kinds of integers *)
and ikind = 
    IChar | ISChar | IUChar
  | IInt | IUInt
  | IShort | IUShort
  | ILong | IULong
  | ILongLong | IULongLong

and fkind = 
    FFloat | FDouble | FLongDouble

and attribute = 
    AId of string                       (* Atomic attributes *)
  | AInt of int
  | AStr of string 
  | AVar of varinfo
  | ACons of string * attribute list       (* Constructed attributes *)

(* literal constants *)
and constant =
  | CInt of int * ikind * string option  (* Give the ikind (see ISO9899 
                                          * 6.4.4.1) and the textual 
                                          * representation, if available. Use 
                                          * "integer" to create these  *)
  | CLInt of int64 * ikind * string option
  | CStr of string
  | CChr of char 
  | CReal of float * fkind * string option(* Give the fkind (see ISO 6.4.4.2) 
                                           * and also the textual 
                                           * representation, if available *)

(* unary operations *)
and unop =
    Neg                                 (* unary - *)
  | BNot                                (* ~ *)

(* binary operations *)
and binop =
    Plus
  | Minus
  | Mult
  | Div
  | Mod

  | Shiftlt                             (* shift left *)
  | Shiftrt                             (* shift right *)

  | Lt
  | Gt
  | Le
  | Ge

  | Eq
  | Ne

  | BAnd                                (* bitwise and *)
  | BXor                                (* exclusive-or *)
  | BOr                                 (* inclusive-or *)

                                        (* Comparison operations *)

(* expressions, no side effects *)
and exp =
    Const      of constant * location
  | Lval       of lval                  (* l-values *)
  | SizeOf     of typ * location        (* Has UInt type ! (ISO 6.5.3.4). 
                                         * Only sizeof for types is 
                                         * available. This is not turned into 
                                         * a constant because some 
                                         * transformations might want to 
                                         * change types *) 

                                        (* Give the type of the result *)
  | UnOp       of unop * exp * typ * location 

                                        (* Give the type of the result. The 
                                         * arithemtic conversions are made 
                                         * explicit for the arguments *)
  | BinOp      of binop * exp * exp * typ * location

  | Question   of exp * exp * exp * location (* e1 ? e2 : e3. Sometimes we 
                                              * cannot turn this into a 
                                              * conditional statement (e.g. 
                                              * in global initializers) *)
  | CastE      of typ * exp * location  (* Use doCast to make casts *)

  | Compound   of typ * exp list        (* Used only for initializers of 
                                         * structures and arrays *)
  | AddrOf     of lval * location

  | StartOf    of lval                  (* There is no C correspondent for 
                                         * this. To be used for lval's that 
                                         * denote arrays. The expression 
                                         * denotes the address of the first 
                                         * element of the array. Used only to 
                                         * simplify typechecking  *)

(* L-Values denote contents of memory addresses. A memory address is 
 * expressed as a base plus an offset. The base address can be the start 
 * address of storage for a local or global variable or, in general, any 
 * expression. We distinguish the two cases to avoid gratuituous introduction 
 * of the AddrOf operators on variables whose address would not be taken 
 * otherwise. *)

and lval =
    lbase * offset  

(* The meaning of an lval is expressed as a function "[lval] = (a, T)" that 
 * returns a memory address "a" and a type "T" of the object storred starting 
 * at the address "a".  *)

(* The meaning of an lbase is expressed as a similar function. *)

(* The meaning of an offset is expressed as a function "[offset](a, T) = (a', 
 * T')" whose result also depends on a base address "a" and a base type "T". 
 * The result is another address and another base type  *)

(* With this notation we define
  
      [(lbase, offset)] = [offset] [lbase]
*)
and lbase = 
  | Var        of varinfo               (* denotes the address & v, or if v 
                                         * is an array then just v *)
    (* [Var v] = (&v, typeOf(v)) *)


  | Mem        of exp                   (* denotes an address expressed as an 
                                         * expression. Use mkMem to make 
                                         * these  *)
    (* [Mem e] = (e, T) if typeOf(e) = Ptr(T) *)

and offset = 
  | NoOffset                            (* l *)
    (* [NoOffset](a, T) = (a, T) *)

  | Field      of fieldinfo * offset    (* l.f + offset. l must be a struct 
                                         * or an union and l.f is the element 
                                         * type *)
    (* [Field(f, off)](a, struct {f : T, ...}) = [off](a + offsetof(f), T) *)

  | First      of offset                (* An explicit cast from arrays to 
                                         * the first element. This has no 
                                         * source-level equivalent *)
    (* [First off](a, array (T)) = [off](a, T) *)

  | Index      of exp * offset          (* l + e + offset. *)
    (* [Index(e, off)](a, T) = [off](a + e * sizeof(T), T) *)


(* the following equivalences hold *)
(* Index(0, off) = off                                                 *)
(* Mem(StartOf(Mem a, aoff)), off = Mem(a, aoff + First + off)         *)
(* Mem(StartOf(Var v, aoff)), off = Var(a, aoff + First + off)         *)
(* Mem(AddrOf(Mem a, aoff)), off   = Mem(a, aoff + off)                *)
(* Mem(AddrOf(Var v, aoff)), off   = Var(v, aoff + off)                *)

(**** INSTRUCTIONS. May cause effects directly but may not have control flow.*)
and instr =
    Set        of lval * exp * location  (* An assignment. A cast is present 
                                          * if the exp has different type 
                                          * from lval *)
  | Call       of varinfo option * exp * exp list * location
			 (* result temporary variable, 
                            function value, argument list, location. Casts 
                          * are inserted for arguments *)

                         (* See the GCC specification for the meaning of ASM. 
                          * If the source is MS VC then only the templates 
                          * are used *)
  | Asm        of string list *         (* templates (CR-separated) *)
                  bool *                (* if it is volatile *)
                  (string * lval) list * (* outputs must be lvals with 
                                          * constraints. I would like these 
                                          * to be actually variables, but I 
                                          * run into some trouble with ASMs 
                                          * in the Linux sources  *)
                  (string * exp) list * (* inputs with constraints *)
                  string list           (* register clobbers *)

(**** STATEMENTS. Mostly structural information ****)
and stmt = 
  | Skip                                (* empty statement *)
  | Sequence of stmt list               (* Use mkSeq to make a Sequence. This 
                                         * will optimize the result and will 
                                         * make sure that there are no 
                                         * trailing Default of Label or Case *)
  | Loop of stmt                        (* A loop. When stmt is done the 
                                         * control starts back with stmt. 
                                         * Ends with break or a Goto outside.*)
  | IfThenElse of exp * stmt * stmt     (* if *)
  | Label of string 
  | Goto of string
  | Return of exp option
  | Switch of exp * stmt                (* no work done to break this appart *)
  | Case of int                         (* The case expressions are resolved *)
  | Default 
  | Break
  | Continue
  | Instr of instr
        
type fundec = 
    { svar:     varinfo;                (* Holds the name and type as a 
                                         * variable, so we can refer to it 
                                         * easily from the program *)
      mutable slocals: varinfo list;    (* locals, includes the arguments 
                                         * included in the type of svar *)
      mutable smaxid: int;              (* max local id. Starts at 0 *)
      mutable sbody: stmt;              (* the body *)
    } 

type global = 
    GFun of fundec
  | GType of string * typ               (* A typedef *)
  | GVar of varinfo * exp option        (* A global variable with 
                                         * initializer. Includes function 
                                         * prototypes  *)
  | GAsm of string                      (* Global asm statement. These ones 
                                         * can contain only a template *)
  | GPragma of string                   (* Pragmas at top level. Unparsed *)
    
type file = global list
	(* global function decls, global variable decls *)






let lu = locUnknown
let msvcOutput = ref false              (* Whether the pretty printer should 
                                         * print output for the MS VC 
                                         * compiler. Default is GCC *)

let integerFits (i: int) (k: ikind) =  true (* We know that i is less than 31 
                                             * bits so it fits even in an 
                                             * IInt *)

let integerKinds (i: int) (posskinds: ikind list) (s: string option) = 
  let rec loop = function
      [] -> E.s (E.bug "integerkinds exhausted kinds")
    | k :: rest -> 
        if integerFits i k then
          CInt(i, k, s)
        else loop rest
  in
  loop posskinds
          

let integer i = Const (integerKinds i [IInt] None, lu)(* For now only ints *)
let zero      = integer 0
let one       = integer 1
let mone      = integer (-1)

let voidType = TVoid([])
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let charType = TInt(IChar, [])
let charPtrType = TPtr(charType,[])
let voidPtrType = TPtr(voidType, [])
let doubleType = TFloat(FDouble, [])

let var vi : lval = (Var vi, NoOffset)
let mkSet lv e = Instr(Set(lv,e,lu))
let assign vi e = mkSet (var vi) e
let call res f args = Instr(Call(res,f,args,lu))

let mkString s = Const(CStr s, lu)

    (* Make a sequence out of a list of statements *)
let mkSeq sl = 
  let rec removeSkip = function 
      [] -> []
    | Skip :: rest -> removeSkip rest
    | Sequence (sl) :: rest -> removeSkip (sl @ rest)
    | ((Default | Label _ | Case _) as last) :: rest -> 
        let rest' = removeSkip rest in
        if rest' = [] then
          last :: [Skip]                (* Put a ; after default or a label*)
        else
          last :: rest'
    | s :: rest -> s :: removeSkip rest
  in
  match removeSkip sl with 
    [] -> Skip
  | [s] -> s
  | sl' -> Sequence(sl')


let forwardTypeMap : (string, typ) H.t = H.create 113
let clearForwardMap () = H.clear forwardTypeMap
let resolveForwardType n = 
  try
    H.find forwardTypeMap n
  with Not_found -> begin
    ignore (E.log "Warning: Cannot resolve forward type %s\n" n);
    voidType
  end

let addForwardType key t = 
  H.add forwardTypeMap key t

    (* Use this every time you redefine a struct or a union, to make sure all 
     * the TForward see the change *)
let replaceForwardType key t = 
  try
    H.remove forwardTypeMap key;
    H.add forwardTypeMap key t
  with Not_found -> ()                  (* Do nothing if not already in *)


(**** Utility functions ******)
let rec unrollType = function
    TNamed (_, r,_) -> unrollType r
  | TForward n -> unrollType (resolveForwardType n)
  | x -> x




(* the name of the C function we call to get ccgr ASTs
external parse : string -> file = "cil_main"
*)
(* 
  Pretty Printing
 *)

(* location *)
let d_loc () l =
  dprintf "/*(%s:%d:%d)*/" l.file l.line l.col

let escape_char c = 
  let conv v = 
    String.make 1 
      (Char.chr (v + (if v < 10 then (Char.code '0') 
      else (Char.code 'a' - 10)))) 
  in
  match c with
    '\n' -> "\\n"
  | '"' -> "\\\""  (* '"' *)
  | '\'' -> "\\'"
  | '\r' -> "\\r"
  | '\t' -> "\\t"
  | '\b' -> "\\b"
  | '\000' -> "\\0"
  | '\\' -> "\\\\"
  | _ -> 
      let esc = String.make 1 c in
      if esc = Char.escaped c then esc
      else 
        let code = Char.code c in
        "\\"
        ^ (conv (code / 64))
        ^ (conv ((code mod 64) / 8))
        ^ (conv (code mod 8))

let escape_string str =
  let lng = String.length str in
  let conv v = 
    String.make 1 
      (Char.chr (v + (if v < 10 then (Char.code '0') 
      else (Char.code 'a' - 10)))) 
  in
  let rec build idx =
    if idx >= lng then ""
    else
      (escape_char (String.get str idx)) ^ (build (idx + 1)) 
  in
  build 0	

  
let d_ikind () = function
    IChar -> text "char"
  | ISChar -> text "signed char"
  | IUChar -> text "unsigned char"
  | IInt -> text "int"
  | IUInt -> text "unsigned int"
  | IShort -> text "short"
  | IUShort -> text "unsigned short"
  | ILong -> text "long"
  | IULong -> text "unsigned long"
  | ILongLong -> 
      if !msvcOutput then text "__int64" else text "long long"
  | IULongLong -> 
      if !msvcOutput then text "unsigned __int64" 
      else text "unsigned long long"

let d_fkind () = function
    FFloat -> text "float"
  | FDouble -> text "double"
  | FLongDouble -> text "long double"

let d_storage () = function
    NoStorage -> nil
  | Static -> text "static "
  | Extern -> text "extern "
  | Register -> text "register "

        
(* constant *)
let d_const () c =
  match c with
    CInt(_, _, Some s) -> text s
  | CInt(i, _, None) -> num i
  | CLInt(l,h, Some s) -> text s
  | CStr(s) -> dprintf "\"%s\"" (escape_string s)
  | CChr(c) -> dprintf "'%s'" (escape_char c)
  | CReal(_, _, Some s) -> text s
  | CReal(f, _, None) -> dprintf "%f" f
  | _ -> E.s (E.unimp "constant")

(* Parentheses level. An expression "a op b" is printed parenthesized if its 
 * parentheses level is >= that that of its context. Identifiers have the 
 * lowest level and weakly binding operators (e.g. |) have the largest level 
 *)
let derefStarLevel = 20
let indexLevel = 20
let arrowLevel = 20
let addrOfLevel = 30
let bitwiseLevel = 75
let additiveLevel = 60
let getParenthLevel = function
  | Compound _ -> 100

  | Question _ -> 80
                                        (* Bit operations. *)
  | BinOp((BOr|BXor|BAnd),_,_,_,_) -> bitwiseLevel (* 75 *)

                                        (* Comparisons *)
  | BinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_,_,_) -> 70
                                        (* Additive. Shifts can have higher 
                                         * level but I want parentheses 
                                         * around them *)
  | BinOp((Minus|Plus|Shiftlt|Shiftrt),_,_,_,_)  -> additiveLevel (* 60 *)

                                        (* Multiplicative *)
  | BinOp((Div|Mod|Mult),_,_,_,_) -> 40

                                        (* Unary *)
  | CastE(_,_,_) -> 30
  | AddrOf(_,_) -> 30
  | StartOf(_) -> 30
  | UnOp((Neg|BNot),_,_,_) -> 30

                                        (* Lvals *)
  | Lval(Mem _ , _) -> 20                   
  | Lval(Var _, (Field _|Index _|First _)) -> 20
  | SizeOf _ -> 20

  | Lval(Var _, NoOffset) -> 0        (* Plain variables *)
  | Const _ -> 0                        (* Constants *)


(* types. Call with a function that when invoked will fill-in the declared name *)

  (* When we print types for consumption by another compiler we must be 
   * careful to avoid printing multiple type definitions *)
let noRedefinitions = ref false
let definedTypes : (string, bool) H.t = H.create 17
let canPrintName n =
  (not !noRedefinitions) || 
  (try begin
    ignore (H.find definedTypes n); false
  end with Not_found -> begin
    H.add definedTypes n true;
    true
  end)


(* Some attributes are printed before and others after. The before ones are 
 * typically the qualifiers  *)
let rec separateAttributes (pre, post) = function
    [] -> pre,post
  | ((AId "const" | AId "volatile" | AId "inline" |
      AId "cdecl" | AId "stdcall") as a) :: rest -> 
      separateAttributes (a :: pre, post) rest
  | a :: rest ->
      separateAttributes (pre, a :: post) rest
let separateAttributes a = separateAttributes ([], []) a    

let rec d_decl (docName: unit -> doc) () this = 
  let parenth outer_t doc = 
    let typ_strength = function         (* binding strength of type 
                                         * constructors  *)
      | TArray _ -> 11
      | TPtr _ -> 10
      | TFun _ -> 12
      | _ -> 1
    in
    if typ_strength outer_t > typ_strength this then 
      dprintf "(%a)" insert doc
    else
      doc
  in match this with 
    TVoid a -> dprintf "void%a %t" d_attrlistpost a docName
  | TInt (ikind,a) -> dprintf "%a%a %t" d_ikind ikind d_attrlistpost a docName
  | TBitfield(ikind,i,a) -> 
      dprintf "%a %t : %d%a" d_ikind ikind docName i d_attrlistpost a
  | TFloat(fkind, a) -> dprintf "%a%a %t" d_fkind fkind 
        d_attrlistpost a docName
  | TStruct (n,fi,a) -> 
      let n' = 
        if String.length n >= 5 && String.sub n 0 5 = "@anon" then "" else n in
      if n' = "" || canPrintName ("struct " ^ n') then
        dprintf "str@[uct %s%a {@!%a@]@!} %t" n' d_attrlistpost a
          (docList line (d_fielddecl ())) fi docName
      else
        dprintf "struct %s %t" n' docName
  | TUnion (n,fi,a) -> 
      let n' = 
        if String.length n >= 5 && String.sub n 0 5 = "@anon" then "" else n in
      if n' = "" || canPrintName ("union " ^ n') then
        dprintf "uni@[on %s%a {@!%a@]@!} %t" n' d_attrlistpost a
          (docList line (d_fielddecl ())) fi docName
      else
        dprintf "union %s %t" n' docName

  | TForward n -> dprintf "%s %t" n docName

  | TEnum (n, kinds, a) -> 
      let n' = 
        if String.length n >= 5 && String.sub n 0 5 = "@anon" then "" else n in
      if n' = "" || canPrintName ("enum " ^ n') then
        dprintf "enum@[ %s%a {%a@]@?} %t" n' d_attrlistpost a
          (docList line (fun (n,i) -> dprintf "%s = %d,@?" n i)) kinds
          docName
      else
        dprintf "enum %s %t" n' docName

  | TPtr (TFun(tres, args, isva, af) as t, ap) when !msvcOutput ->  (* !!! *)
      let rec stripCallAttr (call, notcall) = function
          [] -> call, notcall
        | ((AId("cdecl")|AId("stdcall")) as a) :: rest ->
            stripCallAttr (a :: call, notcall) rest
        | a :: rest -> stripCallAttr (call, a :: notcall) rest
      in
      let call, notcall = stripCallAttr ([], []) ap in
      d_decl (fun _ -> parenth t (dprintf "%a* %a%t" 
                                    d_attrlistpre call
                                    d_attrlistpost notcall docName )) 
             () (TFun(tres, args, isva, []))

  | TPtr (t, a)  -> 
      d_decl (fun _ -> parenth t (dprintf "* %a%t" d_attrlistpre a docName )) 
             () t

  | TArray (t, lo, a) -> 
      d_decl (fun _ -> parenth t
          (dprintf "%t[%a]%a" 
             docName
             insert (match lo with None -> nil 
             | Some e -> d_exp () e)
             d_attrlistpost a))
        ()
        t
  | TFun (restyp, args, isvararg, a) -> 
      let args' = 
        match args with 
            [] -> [ { vname = "";
                      vtype = if isvararg then voidPtrType else voidType;
                      vid   = 0;
                      vglob = false;
                      vattr = [];
                      vdecl = lu;
                      vaddrof = false; 
                      vstorage = NoStorage; } ] 
        | _ -> args
      in
      d_decl (fun _ -> 
        parenth restyp 
          (dprintf "%a %t(@[%a%a@])" 
             d_attrlistpost a
             docName
             (docList (chr ',' ++ break) (d_videcl ())) args'
             insert (if isvararg then text ", ..." else nil)))
        ()
        restyp

  | TNamed (n, _, a) -> dprintf "%a %s %t" d_attrlistpost a n docName


(* Only a type (such as for a cast) *)        
and d_type () t = d_decl (fun _ -> nil) () t

and d_fieldinfo () f =
  dprintf "/* %s:%s:%a */" f.fstruct f.fname d_type f.ftype
    

(* exp *)

                                        (* Rest *)

(* Print an expression assuming a precedence for the context. Use a small 
 * number to parenthesize the printed expression. 0 guarantees parentheses. 1 
 * will parenthesize everything but identifiers. *)
and d_expprec contextprec () e = 
  let thisLevel = getParenthLevel e in
                                 (* This is to quite down GCC warnings *)
  if thisLevel >= contextprec || (thisLevel = additiveLevel &&
                                  contextprec = bitwiseLevel) then
    dprintf "(%a)" d_exp e
  else
    d_exp () e

and d_exp () e = 
  let level = getParenthLevel e in
  match e with
    Const(c,l) -> dprintf "%a" d_const c
  | Lval(l) -> dprintf "%a" d_lval l
  | UnOp(u,e1,_,l) -> 
      let d_unop () u =
        match u with
          Neg -> text "-"
        | BNot -> text "~"
      in
      dprintf "%a %a" d_unop u (d_expprec level) e1

  | BinOp(b,e1,e2,_,l) -> 
      dprintf "@[%a %a@?%a@]" 
        (d_expprec level) e1 d_binop b (d_expprec level) e2
  | Question (e1, e2, e3, _) -> 
      dprintf "%a ? %a : %a"
        (d_expprec level) e1 (d_expprec level) e2 (d_expprec level) e3
  | CastE(t,e,l) -> dprintf "(%a)%a" d_type t (d_expprec level) e
  | SizeOf (t, l) -> dprintf "sizeof(%a)" d_type t
  | Compound (t, el) -> 
      let dcast = 
        if !msvcOutput then nil         (* MSVC does not list the cast *)
        else dprintf "(%a) " d_type t
      in
      dprintf "%a{@[%a@]}" insert dcast
        (docList (chr ',' ++ break) (d_exp ())) el
  | AddrOf(lv,lo) -> 
      dprintf "& %a" (d_lvalprec addrOfLevel) lv

  | StartOf(lv) -> d_lval () lv

and d_binop () b =
  match b with
    Plus -> text "+"
  | Minus -> text "-"
  | Mult -> text "*"
  | Div -> text "/"
  | Mod -> text "%"
  | Shiftlt -> text "<<"
  | Shiftrt -> text ">>"
  | Lt -> text "<"
  | Gt -> text ">"
  | Le -> text "<="
  | Ge -> text ">="
  | Eq -> text "=="
  | Ne -> text "!="
  | BAnd -> text "&"
  | BXor -> text "^"
  | BOr -> text "|"
        
(* attributes *)
and d_attr () = function
    AId s -> text s
  | AInt n -> num n
  | AStr s -> dprintf "\"%s\"" (escape_string s)
  | AVar vi -> text vi.vname
  | ACons(s,al) -> dprintf "%s(%a)" s
        (docList (chr ',') (d_attr ())) al
          
and d_attrlist pre al = (* Whether it comes before or after stuff *)
  (* Take out the special attributes *)
  let rec loop remaining = function
      [] -> begin
        match remaining with
          [] -> nil
        | _ -> 
            dprintf "__attribute__((%a)) "
              (docList (chr ',' ++ break) 
                 (fun a -> dprintf "%a" d_attr a)) al
      end
    | (AId("const")) :: rest -> 
        dprintf "const %a" insert (loop remaining rest)
    | (AId("inline")) :: rest -> 
        dprintf "inline %a" insert (loop remaining rest)
    | (AId("volatile")) :: rest -> 
        dprintf "volatile %a" insert (loop remaining rest)
    | (AId("cdecl")) :: rest when !msvcOutput -> 
        dprintf "__cdecl %a" insert (loop remaining rest)
    | (AId("stdcall")) :: rest when !msvcOutput -> 
        dprintf "__stdcall %a" insert (loop remaining rest)
    | x :: rest -> loop (x :: remaining) rest
  in
  let res = loop [] al in
  if res = nil then
    res
  else
    if pre then res ++ text " " else text " " ++ res
    
and d_attrlistpre () a = d_attrlist true a
and d_attrlistpost () a = d_attrlist false a

(* lvalue *)
and d_lvalprec contextprec () lv = 
  if getParenthLevel (Lval(lv)) >= contextprec then
    dprintf "(%a)" d_lval lv
  else
    d_lval () lv
  
and d_lval () lv = 
  let rec d_offset dobase = function
    | NoOffset -> dobase ()
    | Field (fi, o) -> 
        d_offset (fun _ -> dprintf "%t.%s" dobase fi.fname) o
    | Index (Const(CInt(0,_,_),_), NoOffset) -> dprintf "(*%t)" dobase
    | First (NoOffset) -> dprintf "(*%t)" dobase
    | First (Index _ as o) -> d_offset dobase o
    | First o -> d_offset (fun _ -> dprintf "%t[0]" dobase) o
    | Index (e, o) -> 
        d_offset (fun _ -> dprintf "%t[%a]" dobase d_exp e) o
  in
  match lv with
    Var vi, o -> d_offset (fun _ -> text vi.vname) o
  | Mem e, Field(fi, o) -> 
      d_offset (fun _ -> 
        dprintf "%a->%s" (d_expprec arrowLevel) e fi.fname) o
  | Mem e, NoOffset -> dprintf "(*%a)" (d_expprec derefStarLevel) e
  | Mem e, o -> 
      d_offset (fun _ -> dprintf "%a" (d_expprec indexLevel) e) o
        
and d_instr () i =
  match i with
  | Set(lv,e,lo) -> begin
      (* Be nice to some special cases *)
      match e with
        BinOp((Plus),Lval(lv'),Const(CInt(1,_,_),_),_,_) 
          when lv == lv' -> 
          dprintf "%a ++;" d_lval lv
      | BinOp(Minus,Lval(lv'),Const(CInt(1,_,_),_),_,_) when lv == lv' -> 
          dprintf "%a --;" d_lval lv
      | BinOp((Plus|Minus|BAnd|BOr|BXor|
               Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
              Lval(lv'),e,_,_) when lv == lv' -> 
          dprintf "%a %a= %a;" d_lval lv d_binop bop d_exp e
      | _ -> dprintf "%a = %a;" d_lval lv d_exp e
  end
  | Call(vio,e,args,loc) ->
      dprintf "%s%a(@[%a@]);" 
        (match vio with None -> "" | Some vi -> vi.vname ^ " = ") 
        insert (match e with Lval(Var _, _) -> d_exp () e 
                             | _ -> dprintf "(%a)" d_exp e)
	(docList (chr ',' ++ break) (d_exp ())) args
  | Asm(tmpls, isvol, outs, ins, clobs) ->
      if !msvcOutput then
        dprintf "__asm {@[%a@]};@!"  (docList line text) tmpls
      else
        dprintf "__asm__ %a(@[%a%a%a%a@]);@!"
          insert (if isvol then text "__volatile__" else nil)
          (docList line 
             (fun x -> dprintf "\"%s\"" (escape_string x))) tmpls
          insert 
          (if outs = [] && ins = [] && clobs = [] then 
            nil
          else 
            dprintf ": %a" (docList (chr ',' ++ break) 
                              (fun (c, lv) -> dprintf "\"%s\" (%a)"
                                  (escape_string c) d_lval lv)) outs)
          insert
          (if ins = [] && clobs = [] then
            nil
          else
            dprintf ": %a" (docList (chr ',' ++ break) 
                              (fun (c, e) -> dprintf "\"%s\" (%a)"
                                  (escape_string c) d_exp e)) ins)
          insert 
          (if clobs = [] then nil
          else
            dprintf ": %a" (docList (chr ',' ++ break) 
                              (fun x -> dprintf "\"%s\"" (escape_string x))) 
              clobs)
       
and d_stmt () s =
  let doThen a = (* Want to print { } in the "then" branch to avoid 
                  dangling elses. ASM gives some problems with MSVC so 
                  bracket them as well  *) 
    match a with
      IfThenElse _ | Loop _ | Instr(Asm _) | Switch _ -> 
        Sequence [a]
    | _ -> a
  in
  match s with
    Skip -> dprintf ";"
  | Sequence(lst) -> dprintf "@[{ @[@!%a@]@!}@]" 
        (docList line (d_stmt ())) lst
  | Loop(Sequence(IfThenElse(e,Skip,Break) :: rest)) -> 
      dprintf "wh@[ile (%a)@!%a@]" d_exp e d_stmt (Sequence rest)
  | Loop(stmt) -> 
      dprintf "wh@[ile (1)@!%a@]" d_stmt stmt
  | IfThenElse(e,a,(Skip|Sequence([Skip]))) -> 
      dprintf "if@[ (%a)@!%a@]" d_exp e d_stmt (doThen a)
  | IfThenElse(e,a,b) -> 
      dprintf "@[if@[ (%a)@!%a@]@!el@[se@!%a@]@]" 
        d_exp e d_stmt (doThen a) d_stmt b
  | Label(s) -> dprintf "%s:" s
  | Case(i) -> dprintf "case %d: " i
  | Goto(s) -> dprintf "goto %s;" s
  | Break  -> dprintf "break;"
  | Continue -> dprintf "continue;"
  | Return(None) -> text "return;"
  | Return(Some e) -> dprintf "return (%a);" d_exp e
  | Switch(e,s) -> dprintf "@[switch (%a)@!%a@]" d_exp e d_stmt s
  | Default -> dprintf "default:"
  | Instr(i) -> d_instr () i


        
and d_fun_decl () f = 
  let pre, post = separateAttributes f.svar.vattr in
  (* Now take out the inline *)
  let isinline, pre' = 
    match List.partition (fun a -> a = AId("inline")) pre with
      [], _ -> false, pre
    | _, pre' -> true, pre'
  in
  dprintf "%s%a%a %a@!{ @[%a@!%a@]@!}" 
    (if isinline then 
      if !msvcOutput then "__inline " else "inline " else "")
    d_storage f.svar.vstorage
    (* the prototype *)
    (d_decl (fun _ -> dprintf "%a%s" d_attrlistpre pre' f.svar.vname)) 
    f.svar.vtype
    d_attrlistpost post
    (* locals. But eliminate first the formal arguments *)
    (docList line (fun vi -> d_videcl () vi ++ text ";"))
       (let nrArgs = 
         match f.svar.vtype with
           TFun(_, args, _, _) -> List.length args
         | _ -> E.s (E.bug "non-function type")
       in
       let rec drop n = function
           l when n = 0 -> l
         | [] -> E.s (E.bug "Too few locals")
         | _ :: rest -> drop (n - 1) rest
       in
       drop nrArgs f.slocals)
    (* the body *)
    d_stmt f.sbody

and d_videcl () vi = 
  let pre, post = separateAttributes vi.vattr in 
  dprintf "%a%a %a" d_storage vi.vstorage
    (d_decl (fun _ -> dprintf "%a%s" d_attrlistpre pre vi.vname)) vi.vtype
    d_attrlistpost post
    
and d_fielddecl () fi = 
  dprintf "%a %a;"
    (d_decl (fun _ -> 
      text (if fi.fname = "___missing_field_name" then "" else fi.fname))) 
    fi.ftype
    d_attrlistpost fi.fattr
       
let printFile (out : out_channel) (globs : file) = 
  let print x = fprint out 80 x in
  print (text "/* Generated by safecc */\n\n");
  H.clear definedTypes;
  noRedefinitions := true;
  let d_global () = function
      GFun fundec -> d_fun_decl () fundec ++ line
    | GType (str, typ) -> 
        if str = "" then
          dprintf "%a;@!" (d_decl (fun _ -> nil)) typ
        else 
          dprintf "typedef %a;@!" (d_decl (fun _ -> text str)) typ

  | GVar (vi, eo) -> dprintf "%a %a;"
        d_videcl vi 
        insert (match eo with None -> nil | Some e -> 
                dprintf " = %a" d_exp e)
  | GAsm s -> dprintf "__asm__(\"%s\");@!" (escape_string s)
  | GPragma s -> dprintf "#pragma %s@!" s
  in
  List.iter (fun g -> print (d_global () g ++ line)) globs;
  noRedefinitions := false;
  H.clear definedTypes

    

   (* Some plain pretty-printers. Unlike the above these expose all the 
    * details of the internal representation *)
let rec d_plainexp () = function
    Const(c,_) -> dprintf "Const(%a)" d_const c
  | Lval(lv) -> dprintf "Lval(@[%a@])" d_plainlval lv
  | CastE(t,e,_) -> dprintf "CastE(@[%a,@?%a@])" d_plaintype t d_plainexp e
  | StartOf lv -> dprintf "StartOf(%a)" d_plainlval lv
  | e -> d_exp () e

and d_plainlval () = function
  | Var vi, o -> dprintf "Var(@[%s,@?%a@])" vi.vname d_plainoffset o
  | Mem e, o -> dprintf "Mem(@[%a,@?%a@])" d_plainexp e d_plainoffset o

and d_plainoffset () = function
    NoOffset -> text "NoOffset"
  | Field(fi,o) -> 
      dprintf "Field(@[%s:%a,@?%a@])" 
        fi.fname d_plaintype fi.ftype d_plainoffset o
  | First o -> dprintf "First(%a)" d_plainoffset o
  | Index(e, o) -> dprintf "Index(@[%a,@?%a@])" d_plainexp e d_plainoffset o

and d_plaintype () = function
    TVoid a -> dprintf "TVoid(@[%a@])" d_attrlistpost a
  | TInt(ikind, a) -> dprintf "TInt(@[%a,@?%a@])" d_ikind ikind d_attrlistpost a
  | TFloat(fkind, a) -> 
      dprintf "TFloat(@[%a,@?%a@])" d_fkind fkind d_attrlistpost a
  | TBitfield(ikind,i,a) -> 
      dprintf "TBitfield(@[%a,@?%d,@?%a@])" d_ikind ikind i d_attrlistpost a
  | TNamed (n, t, a) ->
      dprintf "TNamed(@[%s,@?%a,@?%a@@])" n d_plaintype t d_attrlistpost a
  | TForward n -> dprintf "TForward(%s)" n
  | TPtr(t, a) -> dprintf "TPtr(@[%a,@?%a@])" d_plaintype t d_attrlistpost a
  | TArray(t,l,a) -> 
      let dl = match l with 
        None -> text "None" | Some l -> dprintf "Some(@[%a@])" d_plainexp l in
      dprintf "TArray(@[%a,@?%a,@?%a@])" 
        d_plaintype t insert dl d_attrlistpost a
  | TEnum(n,_,a) -> dprintf "Enum(%s,@[%a@])" n d_attrlistpost a
  | TFun(tr,args,isva,a) -> 
      dprintf "TFun(@[%a,@?%a%s,@?%a@])"
        d_plaintype tr 
        (docList (chr ',' ++ break) 
           (fun a -> dprintf "%s: %a" a.vname d_plaintype a.vtype)) args
        (if isva then "..." else "") d_attrlistpost a
  | TStruct(n,flds,a) -> 
      dprintf "TStruct(@[%s,@?%a,@?%a@])" n
        (docList (chr ',' ++ break) 
           (fun f -> dprintf "%s : %a" f.fname d_plaintype f.ftype)) flds
        d_attrlistpost a
  | TUnion(n,flds,a) -> 
      dprintf "TUnion(@[%s,@?%a,@?%a@])" n
        (docList (chr ',' ++ break) 
           (fun f -> dprintf "%s : %a" f.fname d_plaintype f.ftype)) flds
        d_attrlistpost a

(******************
 ******************
 ******************)



  (* Compute some sizeOf to help with constant folding *)
let rec intSizeOf = function            (* Might raise Not_found *)
    TInt((IUChar|IChar|ISChar), _) -> 1
  | TInt((IShort|IUShort), _) -> 2
  | TInt((IInt|IUInt), _) -> 4
  | TInt((ILong|IULong), _) -> 4
  | TInt((ILongLong|IULongLong), _) ->  8
  | TFloat(FFloat, _) ->  4
  | TFloat(FDouble, _) ->  8
  | TFloat(FLongDouble, _) ->  10
  | TEnum _ ->  4
  | TPtr _ ->  4
  | TArray(t, Some (Const(CInt(l,_,_),_)),_) -> (intSizeOf t) * l
  | TNamed(_, r, _) -> intSizeOf r
  | TForward r -> intSizeOf (resolveForwardType r)
  | TStruct(_,flds,_) -> 
      let rec loop = function
          [] -> 0
        | f :: flds -> align (intSizeOf f.ftype) + loop flds
      in
      loop flds
  | _ -> raise Not_found

and intSizeOfNoExc t = 
  try
    intSizeOf t
  with Not_found -> 
    E.s (E.unimp "Cannot compute the sizeof(%a)\n" d_type t)

and sizeOf t = 
  try
    integer (intSizeOf t) 
  with Not_found -> SizeOf(t, lu)

and align n = ((n + 3) lsr 2) lsl 2


 (* Scan all the expressions in a statement *)
let iterExp (f: exp -> unit) (body: stmt) : unit = 
  let rec fExp e = f e; fExp' e
  and fExp' = function
      (Const _|SizeOf _) -> ()
    | Lval lv -> fLval lv
    | UnOp(_,e,_,_) -> fExp e
    | BinOp(_,e1,e2,_,_) -> fExp e1; fExp e2
    | Question (e1, e2, e3, _) -> fExp e1; fExp e2; fExp e3
    | CastE(_, e,_) -> fExp e
    | Compound (_, el) -> List.iter fExp el
    | AddrOf (lv,_) -> fLval lv
    | StartOf (lv) -> fLval lv

  and fLval = function
      Var _, off -> fOff off
    | Mem e, off -> fExp e; fOff off
  and fOff = function
      Field (_, o) -> fOff o
    | Index (e, o) -> fExp e; fOff o
    | First o -> fOff o
    | NoOffset -> ()
  and fStmt = function
      (Skip|Break|Continue|Label _|Goto _|Case _|Default|Return None) -> ()
    | Sequence s -> List.iter fStmt s
    | Loop s -> fStmt s
    | IfThenElse (e, s1, s2) -> fExp e; fStmt s1; fStmt s2
    | Return(Some e) -> fExp e
    | Switch (e, s) -> fExp e; fStmt s
    | Instr(Set(lv,e,_)) -> fLval lv; fExp e
    | Instr(Call(_,f,args,_)) -> fExp f; List.iter fExp args
    | Instr(Asm(_,_,_,ins,_)) -> 
        List.iter (fun (_, e) -> fExp e) ins
  in
  fStmt body



   (* Make a local variable and add it to a function *)
let makeLocalVar fdec name typ = 
  fdec.smaxid <- 1 + fdec.smaxid;
  let vi = { vname = name; 
             vid   = fdec.smaxid;
             vglob = false;
             vtype = typ;
             vdecl = lu;
             vattr = [];
             vstorage = NoStorage;
             vaddrof = false;
           }  in
  fdec.slocals <- fdec.slocals @ [vi];
  vi

let makeTempVar fdec typ = 
  let name = "tmp" ^ (string_of_int (1 + fdec.smaxid)) in
  makeLocalVar fdec name typ


   (* Make a global variable *) 
let makeGlobalVar name typ = 
  let vi = { vname = name; 
             vid   = H.hash name;
             vglob = true;
             vtype = typ;
             vdecl = lu;
             vattr = [];
             vstorage = NoStorage;
             vaddrof = false;
           }  in
  vi


   (* Make an empty function *)
let emptyFunction name = 
  { svar  = makeGlobalVar name (TFun(voidType, [], false,[]));
    smaxid = 0;
    slocals = [];
    sbody = Skip;
  } 

    (* A dummy function declaration handy for initialization *)
let dummyFunDec = emptyFunction "@dummy"



     (* Type signatures. Two types are identical iff they have identical 
      * signatures *)
type typsig = 
    TSArray of typsig * exp option * attribute list
  | TSPtr of typsig * attribute list
  | TSStruct of string * attribute list
  | TSUnion of string * attribute list
  | TSFun of typsig * (typsig * attribute list) list * bool * attribute list
  | TSBase of typ

(* Compute a type signature *)
let rec typeSig t = 
  match t with 
  | (TInt _ | TFloat _ | TEnum _ | TBitfield _ | TVoid _) -> TSBase t
  | TPtr (t, a) -> TSPtr (typeSig t, a)
  | TArray (t,l,a) -> TSArray(typeSig t, l, a)
  | TStruct (n, _, a) -> TSStruct (n, a)
  | TUnion (n, _, a) -> TSUnion (n, a)
  | TFun(rt,args,isva,a) -> TSFun(typeSig rt, 
                                  List.map (fun vi -> (typeSig vi.vtype, 
                                                       vi.vattr)) args,
                                  isva, a)
  | TNamed(_, t, _) -> typeSig t
  | TForward n -> begin
      let l = String.length n in
      try
        if String.sub n 0 1 = "s" then 
          TSStruct(String.sub n 7 (l - 7), [])
        else
          TSUnion(String.sub n 6 (l - 6), [])
      with _ -> E.s (E.bug "Invalid TForward(%s)" n)
  end
      
let dExp : doc -> exp = 
  function d -> Const(CStr(sprint 80 d),lu)

let dStmt : doc -> stmt = 
  function d -> Instr(Asm([sprint 80 d], false, [], [], []))


 (* Add an offset at the end of an lv *)      
let addOffset toadd (b, off) : lval =
 let rec loop = function
     NoOffset -> toadd
   | Field(fid', offset) -> Field(fid', loop offset)
   | First offset -> First (loop offset)
   | Index(e, offset) -> Index(e, loop offset)
 in
 b, loop off



  (* Make a Mem, while optimizing StartOf *)
let mkMem (addr: exp) (off: offset) : exp =  
  let res = 
    match addr with
      StartOf(lv) -> Lval(addOffset (First off) lv)
    | _ -> Lval(Mem addr, off)
  in
(*
  ignore (E.log "memof : %a\nresult = %a\n" 
            d_plainexp addr d_plainexp res);
*)
  res
          


(**** Compute the type of an expression ****)
let rec typeOf (e: exp) : typ = 
  match e with
    Const(CInt (_, ik, _), _) -> TInt(ik, [])
  | Const(CChr _, _) -> charType
  | Const(CStr _, _) -> charPtrType 
  | Const(CLInt (_, ik, _),_) -> TInt(ik, [])
  | Const(CReal (_, fk, _), _) -> TFloat(fk, [])
  | Lval(lv) -> typeOfLval lv
  | SizeOf _ -> intType
  | UnOp (_, _, t, _) -> t
  | BinOp (_, _, _, t, _) -> t
  | Question (_, e2, _, _) -> typeOf e2
  | CastE (t, _, _) -> t
  | Compound (t, _) -> t
  | AddrOf (lv, _) -> TPtr(typeOfLval lv, [])
  | StartOf (lv) -> begin
      match typeOfLval lv with
        TArray (t,_, _) -> TPtr(t, [])
      | _ -> E.s (E.bug "typeOf: StartOf on a non-array")
  end
      
and typeOfLval = function
    Var vi, off -> typeOffset vi.vtype off
  | Mem addr, off -> begin
      match typeOf addr with
        TPtr (t, _) -> typeOffset t off
      | _ -> E.s (E.bug "typeOfLval: Mem on a non-pointer")
  end

and typeOffset basetyp = function
    NoOffset -> basetyp
  | Index (_, o) -> typeOffset basetyp o
  | Field (fi, o) -> typeOffset fi.ftype o
  | First o -> begin
      match basetyp with
        TArray (t, _, _) -> typeOffset t o
      | _ -> E.s (E.bug "typeOfLval: First on a non-array")
  end


let rec doCast (e: exp) (oldt: typ) (newt: typ) = 
  match e with
    CastE(oldt', e', _) -> doCast e' (typeOf e') newt
  | _ -> 
      if typeSig oldt = typeSig newt then
        e
      else
        CastE(newt, e,lu)


let isIntegralType t = 
  match unrollType t with
    (TInt _ | TEnum _ | TBitfield _) -> true
  | _ -> false

let isArithmeticType t = 
  match unrollType t with
    (TInt _ | TEnum _ | TBitfield _ | TFloat _) -> true
  | _ -> false
    

let isPointerType t = 
  match unrollType t with
    (TPtr _ | TArray _) -> true
  | _ -> false


let typeAttrs = function
    TVoid a -> a
  | TInt (_, a) -> a
  | TFloat (_, a) -> a
  | TBitfield (_, _, a) -> a
  | TNamed (n, _, a) -> a
  | TPtr (_, a) -> a
  | TArray (_, _, a) -> a
  | TStruct (_, _, a) -> a
  | TUnion (_, _, a) -> a
  | TForward _ -> []
  | TEnum (_, _, a) -> a
  | TFun (_, _, _, a) -> a


let setTypeAttrs t a =
  match t with
    TVoid _ -> TVoid a
  | TInt (i, _) -> TInt (i, a)
  | TFloat (f, _) -> TFloat (f, a)
  | TBitfield (i, s, _) -> TBitfield (i, s, a)
  | TNamed (n, t, _) -> TNamed(n, t, a)
  | TPtr (t', _) -> TPtr(t', a)
  | TArray (t', l, _) -> TArray(t', l, a)
  | TStruct (n, f, _) -> TStruct(n,f,a)
  | TUnion (n, f, _) -> TUnion(n, f, a)
  | TForward _ -> t
  | TEnum (n, f, _) -> TEnum (n, f, a)
  | TFun (r, args, v, _) -> TFun(r,args,v,a)


let dropAttribute al a = 
  let rec amatch a a' = 
    match a, a' with
      AId s, AId s' when s = s' -> true
    | AInt n, AInt n' when n = n' -> true
    | AStr s, AStr s' when s = s' -> true
    | AVar vi, AVar vi' when vi.vid = vi'.vid -> true
    | ACons (s, args), ACons(s', args') when
        s = s' && (List.for_all2 amatch args args') -> true
    | _ -> false
  in
  List.filter (fun a' -> not (amatch a a')) al
