open Pretty
open Trace      (* sm: 'trace' function *)
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

 - the function arguments with array type are changed into pointers.
*)
(*
 * Note: you may *NOT* change the order of the fields or the order in
 * which disjoint union choices are presented. The C translation code
 * has those values hard-wired.
 *)

(* TODO
   - type of sizeof is hardwired to UInt
   - integerFits is hardwired to true
*)

(* A few globals that control the interpretation of C source *)
let msvcMode = ref false              (* Whether the pretty printer should 
                                       * print output for the MS VC 
                                       * compiler. Default is GCC *)
let charIsUnsigned = ref false        (* Whether CHAR is unsigned. Default 
                                       * false *)
let ilongFitsUInt = ref false         (* Whether a signed long can fit an 
                                       * unsigned integer. True only if a 
                                       * long uses more bits than an int  *)
let newCil = ref true

let printLn= ref true                 (* Whether to print line numbers *)

type location = { 
    line: int;				(* -1 means "do not know" *)
    file: string; 
}

let locUnknown = { line = -1; file = ""; }
(* A reference to the current location *)
let currentLoc : location ref = ref locUnknown


let printShortTypes = ref false

(* Information about a variable. Use one of the makeLocalVar, makeTempVar or 
 * makeGlobalVar to create instances of this data structure. These structures a
 * re shared by all references to the variable. So, you can change the name
 * easily, for example *)
type varinfo = { 
    mutable vid: int;	(* Unique integer indentifier. For globals this is a 
                         * hash of the name. Locals are numbered from 0 
                         * starting with the formal arguments. This field 
                         * will be set for you if you use one of the 
                         * makeLocalVar, makeTempVar or makeGlobalVar *)
    mutable vname: string;				
    mutable vglob: bool;	(* Is this a global variable? *)

    mutable vtype: typ;                 (* The declared type *)
    mutable vdecl: location;            (* where was this variable declared? *)
    mutable vattr: attribute list;
    mutable vstorage: storage;
    mutable vaddrof: bool;              (* Has its address taken. To insure 
                                         * that this is always set, always 
                                         * use mkAddrOf to construct an AddrOf
                                           *)

    (* sm: is this var referenced?  this is computed by removeUnusedVars *)
    (* it is safe to just initialize this to false *)
    mutable vreferenced: bool;
}

                                        (* Storage-class information *)
and storage = 
    NoStorage |                         (* The default storage *)
    Static | 
    Register | 
    Extern

(* Information about a struct/union field *)
and fieldinfo = { 
    mutable fcomp: compinfo;            (* The compinfo of the host. Note 
                                         * that this must be shared with the 
                                         * host since there can be only one 
                                         * compinfo for a given id *)
    mutable fname: string;              (* The name of the field. Might be 
                                         * "___missing_field_name" in which 
                                         * case it is not printed *)
    mutable ftype: typ;
    mutable fattr: attribute list;
}


(* Information about a composite type (a struct or a union). Use mkCompInfo 
 * to create non-recursive or (potentially) recursive versions of this. Make 
 * sure you have a GCompTag for each one of these.  *)
and compinfo = {
    mutable cstruct: bool;              (* true if struct *)
    mutable cname: string;              (* the name. Always non-empty. Use 
                                         * compSetName to set the name and 
                                         * the key. Use compFullName to get 
                                         * the full name of a comp  *)
    mutable ckey: int;                  (* A unique integer. Use Hashtbl.hash 
                                         * on the string returned by 
                                         * compFullName. All compinfo for a 
                                         * given key are shared.  *)
    mutable cfields: fieldinfo list;
    mutable cattr:   attribute list;    (* The attributes that are defined at 
                                         * the same time as the composite 
                                         * type *)
  } 
    
(* Information about an enumeration. This is shared by all references to an 
 * enumeration. Make sure you have a GEnumTag for each of of these.   *)
and enuminfo = { 
    mutable ename: string;             (* the name. Always non-empty *)
    mutable eitems: (string * exp) list;(* items with names and values. This 
                                         * list should be non-empty. The item 
                                         * values must be compile-time 
                                         * constants. *)
    mutable eattr: attribute list      (* attributes *)
}

(* what is the type of an expression? Keep all attributes sorted. Use 
 * addAttribute and addAttributes to construct list of attributes *)
and typ =
    TVoid of attribute list
  | TInt of ikind * attribute list
  | TBitfield of ikind * int * attribute list
  | TFloat of fkind * attribute list

           (* A reference to an enumeration type. All such references must 
            * share the enuminfo. Make sure you have a GEnumTag for each one 
            * of these. The attributes refer to this use of the enumeration. 
            * The attributes of the enumeration itself are stored inside the 
            * enumeration  *)
  | TEnum of enuminfo * attribute list

  | TPtr of typ * attribute list        (* Pointer type. The attributes refer 
                                         * to the  *)

              (* base type and length *)
  | TArray of typ * exp option * attribute list

               (* A reference to a struct or a union type. All references to 
                * the same struct or union must share the same compinfo. mak 
                * sure you have a GCompTag for each compinfo that you use. 
                * The attributes given are those pertaining to this use of 
                * the type. The attributes that were given at the definition 
                * of the type are stored in the compinfo. Always make sure 
                * there is a GTag for each structure or union that you use. *)
  | TComp of compinfo * attribute list

               (* result, args, isVarArg, attributes *)
  | TFun of typ * varinfo list * bool * attribute list

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

(* An attribute has a name and some optional arguments *)
and attribute = Attr of string * attrarg list

and attrarg = 
    AId of string                      
  | AInt of int
  | AStr of string 
  | AVar of varinfo
  | ACons of string * attrarg list       (* Constructed attributes *)

(* literal constants *)
and constant =
  | CInt32 of int32 * ikind * string option 
                 (* Give the ikind (see ISO9899 6.1.3.2) and the textual 
                  * representation, if available. Use "integer" or "kinteger" 
                  * to create these. Watch out for integers that cannot be 
                  * represented on 32 bits. OCAML does not give Overflow 
                  * exceptions. *)

  | CStr of string
  | CChr of char 
  | CReal of float * fkind * string option(* Give the fkind (see ISO 6.4.4.2) 
                                           * and also the textual 
                                           * representation, if available *)

(* unary operations *)
and unop =
    Neg                                 (* unary - *)
  | BNot                                (* ~ *)
  | LNot                                (* ! *)

(* binary operations *)
and binop =
    PlusA                               (* arithemtic + *)
  | PlusPI                              (* pointer + integer *)
  | IndexPI                             (* pointer[integer]. The difference 
                                         * form PlusPI is that in this case 
                                         * the integer is very likely 
                                         * positive *)
  | MinusA                              (* arithemtic - *)
  | MinusPI                             (* pointer - integer *)
  | MinusPP                             (* pointer - pointer *)
  | Mult
  | Div
  | Mod

  | Shiftlt                             (* shift left *)
  | Shiftrt                             (* shift right *)

  | Lt| Gt| Le| Ge| Eq | Ne             (* arithemtic comparisons *)

  | LtP| GtP| LeP| GeP| EqP| NeP        (* pointer comparisons *)

  | BAnd                                (* bitwise and *)
  | BXor                                (* exclusive-or *)
  | BOr                                 (* inclusive-or *)

                                        (* Comparison operations *)

(* expressions, no side effects *)
and exp =
    Const      of constant
  | Lval       of lval                  (* l-values *)
  | SizeOf     of typ                   (* Has UInt type ! (ISO 6.5.3.4).
                                         * Only sizeof for types is
                                         * available. This is not turned into
                                         * a constant because some
                                         * transformations might want to
                                         * change types *)

  | SizeOfE    of exp                   (* Like SizeOf but for expressions *)

                                        (* Give the type of the result *)
  | UnOp       of unop * exp * typ

                                        (* Give the type of the result. The
                                         * arithemtic conversions are made
                                         * explicit for the arguments *)
  | BinOp      of binop * exp * exp * typ

  | Question   of exp * exp * exp      (* e1 ? e2 : e3. Sometimes we cannot 
                                        * turn this into a conditional 
                                        * statement (e.g. in global 
                                        * initializers). This is only allowed 
                                        * inside constant initializers.!!! In 
                                        * all other places it must bne turned 
                                        * into IfThenElse  *)
  | CastE      of typ * exp            (* Use doCast to make casts *)

  | AddrOf     of lval                 (* Always use mkAddrOf to construct
                                         * one of these *)

  | StartOf    of lval                  (* There is no C correspondent for
                                         * this. C has implicit coercions
                                         * from an array to the address of
                                         * the first element and from a
                                         * function to the start address of
                                         * the function. StartOf is used in
                                         * CIL to simplify type checking and
                                         * is just an explicit form of the
                                         * above mentioned implicit
                                         * convertions. You can use mkAddrOf
                                         * to construct one of these *)

(* Initializers for global variables *)
and init = 
  | SingleInit   of exp                 (* A single initializer, might be of 
                                         * compound type *)
                                        (* Used only for initializers of 
                                         * structures, unions and arrays. For 
                                         * a structure we have a list of 
                                         * initializers for a prefix of all 
                                         * fields, for a union we have one 
                                         * initializer for the first field, 
                                         * and for an array we have some 
                                         * prefix of the initializers *)
  | CompoundInit   of typ * init list


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

  | Index    of exp * offset           (* l[e] + offset. l must be an array 
                                         * and l[e] has the element type *)
    (* [Index(e, off)](a, array(T)) = [off](a + e * sizeof(T), T) *)


(* the following equivalences hold *)
(* Mem(StartOf lv), NoOffset = StartOf (lv) if lv is a function *)
(* Mem(AddrOf(Mem a, aoff)), off   = Mem(a, aoff + off)                *)
(* Mem(AddrOf(Var v, aoff)), off   = Var(v, aoff + off)                *)

(**** INSTRUCTIONS. May cause effects directly but may not have control flow.*)
and instr =
    Set        of lval * exp * location  (* An assignment. A cast is present 
                                          * if the exp has different type 
                                          * from lval *)
  | Call       of (varinfo * bool) option * exp * exp list * location
 			 (* optional: result temporary variable and an 
                          * indication that a cast is necessary (the declared 
                          * type of the function is not the same as that of 
                          * the result), the function value, argument list, 
                          * location. If the function is declared then casts 
                          * are inserted for those arguments that correspond 
                          * to declared formals. (The actual number of 
                          * arguments might be smaller or larger than the 
                          * declared number of arguments. C allows this.) If 
                          * the type of the result variable is not the same 
                          * as the declared type of the function result then 
                          * an implicit cast exists.  *)

                         (* See the GCC specification for the meaning of ASM. 
                          * If the source is MS VC then only the templates 
                          * are used *)
                         (* sm: I've added a notes.txt file which contains more
                          * information on interpreting Asm instructions *)
  | Asm        of string list *         (* templates (CR-separated) *)
                  bool *                (* if it is volatile *)
                  (string * lval) list * (* outputs must be lvals with 
                                          * constraints. I would like these 
                                          * to be actually variables, but I 
                                          * run into some trouble with ASMs 
                                          * in the Linux sources  *)
                  (string * exp) list * (* inputs with constraints *)
                  string list *         (* register clobbers *)
                  location

(* STATEMENTS. 
 * The statement is the structural unit in the control flow graph. Use mkStmt 
 * to make a statement and then fill in the fields. *)
and stmt = {
    mutable labels: label list;        (* Whether the statement starts with 
                                        * some labels, case statements or 
                                        * default statement *)
    mutable skind: stmtkind;           (* The kind of statement *)

    (* Now some additional control flow information. Initially this is not 
     * filled in. *)
    mutable sid: int;                   (* A >= 0 identifier that is unique 
                                         * in a function. *)
    mutable succs: stmt list;          (* The successor statements. They can 
                                        * always be computed from the skind 
                                        * and the context in which this 
                                        * statement appears *)
    mutable preds: stmt list;           (* The inverse of the succs function*)
  } 

(* A few kinds of statements *)
and stmtkind = 
  | Instr  of instr list                (* A bunch of instructions that do not 
                                         * contain control flow stuff. 
                                         * Control flows falls through. *)
  | Return of exp option * location     (* The return statement. This is a 
                                         * leaf in the CFG. *)

  | Goto of stmt ref * location         (* A goto statement. Appears from 
                                         * actual goto's in the code. *)
  | Break of location                   (* A break to the end of the nearest 
                                         * enclosing Loop or Switch *)
  | Continue of location                (* A continue to the start of the 
                                         * nearest enclosing Loop *)
  | If of exp * block * block * location (* Two successors, the "then" and the 
                                          * "else" branches. Both branches 
                                          * fall-through to the successor of 
                                          * the If statement *)
  | Switch of exp * block * (stmt list) * location  
                                        (* A switch statement. The block 
                                         * contains within all of the cases. 
                                         * We also have direct pointers to the 
                                         * statements that implement the 
                                         * cases. Which cases they implement 
                                         * you can get from the labels of the 
                                         * statement *)

  | Loop of block * location            (* A "while(1)" loop *)


(* A block is a sequence of statements with the control falling through from 
 * one element to the next *)
and block = stmt list

and label = 
    Label of string * location          (* A real label *)
  | Case of exp * location              (* A case statement *)
  | Default of location                 (* A default statement *)


type fundec =
    { mutable svar:     varinfo;        (* Holds the name and type as a
                                         * variable, so we can refer to it
                                         * easily from the program *)
      mutable sformals: varinfo list;   (* These are the formals. These must 
                                         * be shared with the formals that 
                                         * appear in the type of the 
                                         * function. Use setFormals to set 
                                         * these formals and ensure that they 
                                         * are reflected in the function 
                                         * type. Do not make copies of 
                                         * these because the body refers to 
                                         * them. *)
      mutable slocals: varinfo list;    (* locals, DOES NOT include the
                                         * sformals. Do not make copies of
                                         * these because the body refers to
                                         * them  *)
      mutable smaxid: int;              (* max local id. Starts at 0 *)
      mutable sbody: block;             (* the body *)
      mutable sinline: bool;            (* Whether the function is inline or 
                                         * not *)
    }

type global =
    GFun of fundec * location           (* A function definition. Cannot have 
                                         * storage Extern *)
  | GType of string * typ * location    (* A typedef. The string should not 
                                         * be empty. *)
  | GEnumTag of enuminfo * location     (* Declares an enumeration tag with 
                                         * some fields. There must be one of 
                                         * these for each enumeration tag 
                                         * that you use since this is the 
                                         * only context in which the items 
                                         * are printed. *)

  | GCompTag of compinfo * location     (* Declares a struc/union tag with 
                                         * some fields. There must be one of 
                                         * these for each struct/union tag 
                                         * that you use since this is the 
                                         * only context in which the fields 
                                         * are printed. *)

  | GDecl of varinfo * location         (* A variable declaration. Might be a 
                                         * prototype. There might be at most 
                                         * one declaration and at most one 
                                         * definition for a given variable. 
                                         * If both forms appear then they 
                                         * must share the same varinfo. A 
                                         * prototype shares the varinfo with 
                                         * the fundec of the definition. 
                                         * Either has storage Extern or 
                                         * there must be a definition (Gvar 
                                         * or GFun) in this file  *)
  | GVar  of varinfo * init option * location
                                        (* A variable definition. Might have 
                                         * an initializer. There must be at 
                                         * most one definition for a variable 
                                         * in an entire program. Cannot have 
                                         * storage Extern *)
  | GAsm of string * location           (* Global asm statement. These ones 
                                         * can contain only a template *)
  | GPragma of attribute * location     (* Pragmas at top level. Use the same 
                                         * syntax as attributes *)
  | GText of string                     (* Some text (printed verbatim) at 
                                         * top level. E.g., this way you can 
                                         * put comments in the output.  *)
    

type file = 
    { mutable fileName: string;   (* the complete file name *)
      mutable globals: global list;
      mutable globinit: fundec option;  (* A global initializer. It 
                                                  * is not part of globals 
                                                  * and it is printed last. 
                                                  * Use getGlobInit to 
                                                  * create/get one.  *)
      mutable globinitcalled: bool;     (* Whether the global initialization 
                                         * function is called in main *)
    } 










(* sm: cil visitor interface for traversing Cil trees *)
(* no provision for modifying trees at this time *)
class type cilVisitor = object
  method vvrbl : varinfo -> bool     (* variable *)
  method vvdec : varinfo -> bool     (* variable declaration *)
  method vexpr : exp -> bool         (* expression *)
  method vlval : lval -> bool        (* lval (base is 1st field) *)
  method voffs : offset -> bool      (* lval offset *)
  method vinst : instr -> bool       (* imperative instruction *)
  method vstmt : stmt -> bool        (* constrol-flow statement *)
  method vfunc : fundec -> bool      (* function definition *)
  method vfuncPost : fundec -> bool  (*   postorder version *)
  method vglob : global -> bool      (* global (vars, types, etc.) *)
  method vinit : init -> bool        (* initializers for globals *)
  method vtype : typ -> bool         (* use of some type *)
  method vtdec : string -> typ -> bool    (* typedef *)
end

(* the default visitor does nothing at each node, but does *)
(* not stop; hence they return true *)
class nopCilVisitor = object
  method vvrbl (v:varinfo) = true     (* variable *)
  method vvdec (v:varinfo) = true     (* variable declaration *)
  method vexpr (e:exp) = true         (* expression *)
  method vlval (l:lval) = true        (* lval (base is 1st field) *)
  method voffs (o:offset) = true      (* lval offset *)
  method vinst (i:instr) = true       (* imperative instruction *)
  method vstmt (s:stmt) = true        (* constrol-flow statement *)
  method vfunc (f:fundec) = true      (* function definition *)
  method vfuncPost (f:fundec) = true  (*   postorder version *)
  method vglob (g:global) = true      (* global (vars, types, etc.) *)
  method vinit (i:init) = true        (* global initializers *)
  method vtype (t:typ) = true         (* use of some type *)
  method vtdec (s:string) (t:typ) = true    (* typedef *)
end

(* as an example, here is a visitor that visits expressions *)
(* note how objects capture constructor arguments for use later, *)
(* even though they are not stored explicitly in fields *)
class cilExprVisitor (ve : exp -> unit) = object
  inherit nopCilVisitor    (* get default nop actions *)
  method vexpr e =
    (ve e);                (* call the ctor arg *)
    true                   (* and keep going *)
end




let lu = locUnknown


let get_instrLoc (inst : instr) =
  match inst with
      Set(_, _, loc) -> loc
    | Call(_, _, _, loc) -> loc
    | Asm(_, _, _, _, _, loc) -> loc


let get_stmtLoc (statement : stmtkind) =
  match statement with 
      Instr([]) -> lu
    | Instr(hd::tl) -> get_instrLoc(hd)
    | Return(_, loc) -> loc
    | Goto(_, loc) -> loc
    | Break(loc) -> loc
    | Continue(loc) -> loc
    | If(_, _, _, loc) -> loc
    | Switch(_, _, _, loc) -> loc
    | Loop(_, loc) -> loc




    (* A special location that we use to mark that a BinOp was created from 
     * an index *)
let luindex = { line = -1000; file = ""; }


let printLine (l : location) : string =
  let str = ref "" in
    if !printLn && l.line > 0 then begin
      str := "#";
      if !msvcMode then str := !str ^ "line";
      if l.line > 0 then str := !str ^ " " ^ string_of_int(l.line);
      if l.file <> !currentLoc.file then
        str := !str ^ " \"" ^ l.file ^ "\"";
    end;
    currentLoc := l;
   !str

(* Construct an integer of a given kind. *)
let kinteger (k: ikind) (i: int) = Const (CInt32(Int32.of_int i, k,  None))
let kinteger32 (k: ikind) (i: int32) =  Const (CInt32(i, k,  None))

(* Construct an integer. Use only for values that fit on 31 bits *)
let integer (i: int) = kinteger IInt i
let integer32 (i: int32) = kinteger32 IInt i

let hexinteger (i: int) = 
    Const (CInt32(Int32.of_int i, IInt, Some (Printf.sprintf "0x%08X" i)))
             
let zero      = integer 0
let one       = integer 1
let mone      = integer (-1)

let rec isInteger = function
  | Const(CInt32 (n,_,_)) -> Some n
  | CastE(_, e) -> isInteger e
  | _ -> None
        

let rec isZero (e: exp) : bool = isInteger e = Some Int32.zero

let voidType = TVoid([])
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let longType = TInt(ILong,[])
let ulongType = TInt(IULong,[])
let charType = TInt(IChar, [])
let charPtrType = TPtr(charType,[])
let charConstPtrType = TPtr(charType,[Attr("const", [])])
let voidPtrType = TPtr(voidType, [])
let intPtrType = TPtr(intType, [])
let uintPtrType = TPtr(uintType, [])
let doubleType = TFloat(FDouble, [])


let mkStmt (sk: stmtkind) : stmt = 
  { skind = sk;
    labels = [];
    sid = -1; succs = []; preds = [] }

let mkEmptyStmt () = mkStmt (Instr [])
let mkStmtOneInstr (i: instr) = mkStmt (Instr [i])

let dummyStmt = 
  mkStmt (Instr [(Asm(["dummy statement!!"], false, [], [], [], lu))])

let compactBlock (b: block) : block =  
      (* Try to compress statements *)
  let rec compress (leftover: stmt) = function
      [] -> if leftover == dummyStmt then [] else [leftover]
    | ({skind=Instr il} as s) :: rest ->
        if leftover == dummyStmt then
          compress s rest
        else
          if s.labels == [] then
            match leftover.skind with 
              Instr previl -> 
                leftover.skind <- Instr (previl @ il);
                compress leftover rest
            | _ -> E.s (E.bug "cabs2cil: compress")
          else
                (* This one has labels. Cannot attach to prev *)
            leftover :: compress s rest
        | s :: rest -> 
            let res = s :: compress dummyStmt rest in
            if leftover == dummyStmt then
              res
            else
              leftover :: res
  in
  compress dummyStmt b

(*
let structId = ref 0 (* Find a better way to generate new names *)
let newTypeName n = 
  incr structId;
  "__anon" ^ n ^ (string_of_int (!structId))
*)


(** Construct sorted lists of attributes ***)
let rec addAttribute (Attr(an, _) as a: attribute) (al: attribute list) = 
    let rec insertSorted = function
        [] -> [a]
      | ((Attr(an0, _) as a0) :: rest) as l -> 
          if an < an0 then a :: l
          else if an > an0 then a0 :: insertSorted rest
          else if a = a0 then l else a :: l
    in
    insertSorted al

and addAttributes al0 al = 
    if al0 == [] then al else
    if al  == [] then al0 else
    List.fold_left (fun acc a -> addAttribute a acc) al al0

and dropAttribute (al: attribute list) (Attr(an, _): attribute) = 
  List.filter (fun (Attr(an', _)) -> an <> an') al

and dropAttributes (todrop: attribute list) (al : attribute list) =
  List.fold_left dropAttribute al todrop

and filterAttributes (s: string) (al: attribute list) = 
  List.filter (fun (Attr(an, _)) -> an = s) al

(* sm: *)
let hasAttribute s al =
  (filterAttributes s al <> [])


type attributeClass = 
    AttrName of bool 
        (* Attribute of a name. If argument is true and we are on MSVC then 
         * the attribute is printed using __declspec as part of the storage 
         * specifier  *)
  | AttrFunType of bool 
        (* Attribute of a function type. If argument is true and we are on 
         * MSVC then the attribute is printed just before the function name *)
  | AttrType  (* Attribute of a type *)

(* This table contains the mapping of predefined attributes to classes. 
 * Extend this table with more attributes as you need. This table is used to 
 * determine how to associate attributes with names or type during cabs2cil 
 * conversion *)
let attributeHash: (string, attributeClass) H.t = 
  let table = H.create 13 in
  List.iter (fun a -> H.add table a (AttrName false))
    [ "section"; "constructor"; "destructor"; "unused"; "weak"; 
      "no_instrument_function"; "alias"; "no_check_memory_usage";
      "exception"; "model"; "mode"; "aconst"; "__asm__" (* Gcc uses this to 
                                                         * specifiy the name 
                                                         * to be used in 
                                                         * assembly for a 
                                                         * global *)];
  List.iter (fun a -> H.add table a (AttrName true))
    [ "thread"; "naked"; "dllimport"; "dllexport"; "noreturn" ];
  List.iter (fun a -> H.add table a (AttrFunType false))
    [ "format"; "regparm"; "longcall" ];
  List.iter (fun a -> H.add table a (AttrFunType true))
    [ "stdcall";"cdecl"; "fastcall" ];
  List.iter (fun a -> H.add table a AttrType)
    [ "const"; "volatile"; "restrict" ];
  table
      

(* Partition the attributes into classes *)
let partitionAttributes 
    (default:attributeClass)  
    (attrs:  attribute list) :
    attribute list * attribute list * attribute list = 
  let rec loop (n,f,t) = function
      [] -> n, f, t
    | (Attr(an, _) as a) :: rest -> 
        match (try H.find attributeHash an with Not_found -> default) with 
          AttrName _ -> loop (addAttribute a n, f, t) rest
        | AttrFunType _ -> loop (n, addAttribute a f, t) rest
        | AttrType -> loop (n, f, addAttribute a t) rest
  in
  loop ([], [], []) attrs


(* Get the full name of a comp *)
let compFullName comp = 
  (if comp.cstruct then "struct " else "union ") ^ comp.cname

(* Set the name of a composite type. Also changes the key *)
let compSetName comp n = 
  comp.cname <- n;
  comp.ckey <- H.hash (compFullName comp)

 
(** Creates a a (potentially recursive) composite type. Make sure you add a 
  * GTag for it to the file! **)
let mkCompInfo
               (isstruct: bool) 
               (n: string)   (* empty for anonymous structures *)
               (* fspec is a function that when given a forward 
                * representation of the structure type constructs the type of 
                * the fields. The function can ignore this argument if not 
                * constructing a recursive type.  *)
               (mkfspec: typ -> (string * typ * attribute list) list) 
               (a: attribute list) : compinfo =
   (* make an new name for anonymous structs *)
   if n = "" then 
     E.s (E.bug "mkCompInfo: missing structure name\n");
(*
     newTypeName (if isstruct then "struct" else "union") else n in *)
   (* Make a new self cell and a forward reference *)
   let comp = 
     { cstruct = isstruct; cname = ""; ckey = 0; cfields = [];
       cattr = a; } in
   compSetName comp n;  (* fix the name and the key *)
   let self = ref voidType in
   let tforward = TComp (comp, []) in
   let flds = 
       List.map (fun (fn, ft, fa) -> { fcomp = comp;
                                       ftype = ft;
                                       fname = fn;
                                       fattr = fa }) (mkfspec tforward) in
   comp.cfields <- flds;
   comp

(**** Utility functions ******)
let rec unrollType = function   (* Might drop some attributes !! *)
    TNamed (_, r, _) -> unrollType r
  | x -> x



                                   
let var vi : lval = (Var vi, NoOffset)
(* let assign vi e = Instrs(Set (var vi, e), lu) *)

let mkString s = Const(CStr s)


let mkWhile (guard:exp) (body: stmt list) : stmt list = 
  (* Do it like this so that the pretty printer recognizes it *)
  [ mkStmt (Loop (mkStmt (If(guard, 
                             [ mkEmptyStmt () ], 
                             [ mkStmt (Break lu)], lu)) ::
                  compactBlock body, lu)) ]



let mkFor (start: stmt list) (guard: exp) (next: stmt list) 
          (body: stmt list) : stmt list = 
  compactBlock
    (start @ 
     (mkWhile guard (compactBlock (body @ next))))

    
let mkForIncr (iter: varinfo) (first: exp) (past: exp) (incr: exp) 
    (body: stmt list) : stmt list = 
      (* See what kind of operator we need *)
  let compop, nextop = 
    match unrollType iter.vtype with
      TPtr _ -> LtP, PlusPI
    | _ -> Lt, PlusA
  in
  mkFor 
    [ mkStmt (Instr [(Set (var iter, first, lu))]) ]
    (BinOp(compop, Lval(var iter), past, intType))
    [ mkStmt (Instr [(Set (var iter, 
                           (BinOp(nextop, Lval(var iter), incr, iter.vtype)),
                           lu))])] 
    body
  



(* the name of the C function we call to get ccgr ASTs
external parse : string -> file = "cil_main"
*)
(* 
  Pretty Printing
 *)

(* Some error reporting functions *)
let d_loc (_: unit) (loc: location) : doc =  
  dprintf "%s:%d" loc.file loc.line

let d_thisloc (_: unit) : doc = d_loc () !currentLoc

let error (fmt : ('a,unit,doc) format) : 'a = 
  let f d = 
    E.hadErrors := true; 
    ignore (eprintf "@!%t: Error: %a@!" 
              d_thisloc insert d);
    raise E.Error
  in
  Pretty.gprintf f fmt

let unimp (fmt : ('a,unit,doc) format) : 'a = 
  let f d = 
    E.hadErrors := true; 
    ignore (eprintf "@!%t: Unimplemented: %a@!" 
              d_thisloc insert d);
    raise E.Error
  in
  Pretty.gprintf f fmt

let bug (fmt : ('a,unit,doc) format) : 'a = 
  let f d = 
    E.hadErrors := true; 
    ignore (eprintf "@!%t: Bug: %a@!" 
              d_thisloc insert d);
    raise E.Error
  in
  Pretty.gprintf f fmt

let errorLoc (loc: location) (fmt : ('a,unit,doc) format) : 'a = 
  let f d = 
    E.hadErrors := true; 
    ignore (eprintf "@!%a: Error: %a@!" 
              d_loc loc insert d);
    raise E.Error
  in
  Pretty.gprintf f fmt

let warn (fmt : ('a,unit,doc) format) : 'a = 
  let f d =
    ignore (eprintf "@!%t: Warning: %a@!" 
              d_thisloc insert d);
    nil
  in
  Pretty.gprintf f fmt

let warnLoc (loc: location) (fmt : ('a,unit,doc) format) : 'a = 
  let f d =
    ignore (eprintf "@!%a: Warning: %a@!" 
              d_loc loc insert d);
    nil
  in
  Pretty.gprintf f fmt

let escape_char c = 
  let conv v = 
    String.make 1 
      (Char.chr (v + (if v < 10 then (Char.code '0') 
      else (Char.code 'a' - 10)))) 
  in
  match c with
    '\n' -> "\\n"
  | '\034' -> "\\\""   (* This is the doublequote in ASCII since otherwise it 
                          bothers the CAML fontification in emacs *)
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
      if !msvcMode then text "__int64" else text "long long"
  | IULongLong -> 
      if !msvcMode then text "unsigned __int64" 
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
  let suffix ik = 
    match ik with
      IUInt -> "U"
    | ILong -> "L"
    | IULong -> "UL"
    | ILongLong -> "LL"
    | IULongLong -> "ULL"
    | _ -> ""
  in
  match c with
    CInt32(_, _, Some s) -> text s (* Always print the text if there is one *)
  | CInt32(i, ik, None) -> 
      (* Watch out here for negative integers that we should be printing as 
       * large positive ones *)
      if i < Int32.zero 
          && (match ik with 
            IUInt | IULong | IULongLong -> true | _ -> false) then
        text ("0x" ^ Int32.format "%x" i ^ suffix ik)
      else
        text (Int32.to_string i ^ suffix ik)
  | CStr(s) -> dprintf "\"%s\"" (escape_string s)
  | CChr(c) -> dprintf "'%s'" (escape_char c)
  | CReal(_, _, Some s) -> text s
  | CReal(f, _, None) -> dprintf "%f" f

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
  | Question _ -> 80
                                        (* Bit operations. *)
  | BinOp((BOr|BXor|BAnd),_,_,_) -> bitwiseLevel (* 75 *)

                                        (* Comparisons *)
  | BinOp((Eq|Ne|Gt|Lt|Ge|Le|EqP|NeP|GtP|LtP|GeP|LeP),_,_,_) -> 70
                                        (* Additive. Shifts can have higher 
                                         * level but I want parentheses 
                                         * around them *)
  | BinOp((MinusA|MinusPP|MinusPI|PlusA|
           PlusPI|IndexPI|Shiftlt|Shiftrt),_,_,_)  
    -> additiveLevel (* 60 *)

                                        (* Multiplicative *)
  | BinOp((Div|Mod|Mult),_,_,_) -> 40

                                        (* Unary *)
  | CastE(_,_) -> 30
  | AddrOf(_) -> 30
  | StartOf(_) -> 30
  | UnOp((Neg|BNot|LNot),_,_) -> 30

                                        (* Lvals *)
  | Lval(Mem _ , _) -> 20                   
  | Lval(Var _, (Field _|Index _)) -> 20
  | SizeOf _ | SizeOfE _ -> 20

  | Lval(Var _, NoOffset) -> 0        (* Plain variables *)
  | Const _ -> 0                        (* Constants *)


(* types. Call with a function that when invoked will fill-in the declared 
 * name  *)

  (* When we print types for consumption by another compiler we must be 
   * careful to avoid printing multiple type definitions *)
let definedTypes : ((string * string), bool) H.t = H.create 17
let canPrintCompDef n = true (*
  try begin
    ignore (H.find definedTypes n); false
  end with Not_found -> begin
    H.add definedTypes n true;
    true
  end
*)

(* Separate out the MSVC storage-modifier name attributes *)
let separateStorageModifiers (al: attribute list) = 
  let isstoragemod (Attr(an, _): attribute) : bool =
    try 
      match H.find attributeHash an with
        AttrName issm -> issm
      | _ -> E.s (E.bug "separateStorageModifier: not a name attribute")
    with Not_found -> false
  in
  if not !msvcMode then
    [], al
  else
    let stom, rest = List.partition isstoragemod al in
    (* Put back the declspec. Put it without the leading __ since these will 
     * be added later *)
    let stom' = 
      List.map (fun (Attr(an, args)) -> Attr("declspec", 
                                             if args = [] then [AId(an)] else
                                             [ACons(an, args)])) stom in
    stom', rest
  



(* Print attributes in a custom way *)
let d_attrcustom : (attribute -> Pretty.doc option) ref = 
  ref (fun a -> None)

let setCustomPrintAttribute (pa: attribute -> doc option) : unit = 
  d_attrcustom := pa

let setCustomPrintAttributeScope custom f = 
  let ocustom = !d_attrcustom in
  let newPrint a = 
    match custom a with
      None -> ocustom a
    | x -> x
  in
  d_attrcustom := newPrint;
  fun x -> 
    let res = f x in
    d_attrcustom := ocustom;
    res

(* Make an statement that we'll use as an invalid statement during printing *)
let invalidStmt = {dummyStmt with sid = -2}

(* Print a declaration. Pass to this function a function that will print the 
 * declarator. It must be a function and not the document for the declarator 
 * because printing has a side-effect: we record whose structures we have 
 * printed the definition. Pass also an indication if the docName function 
 * returns just a name (this is used to avoid printing a lot of parentheses 
 * around names) *) 
type docNameWhat = 
    DNNothing                             (* docName is nil *)
  | DNString                              (* docName is just a variable name *)
  | DNStuff                             (* Anything else *)
let rec d_decl (docName: unit -> doc) (dnwhat: docNameWhat) () this = 
  (* Print the docName with some attributes, maybe with parentheses *)
  let parenthname () (a: attribute list) = 
    if a = [] && dnwhat <> DNStuff then
      docName ()
    else if dnwhat = DNNothing then
      (* Cannot print the attributes in this case *)
      dprintf "/*(%a)*/" d_attrlistpre a
    else begin
      dprintf "(%a%t)" d_attrlistpre a docName
    end
  in
  match this with 
    TVoid a -> dprintf "void%a %t" d_attrlistpost a docName
  | TInt (ikind,a) -> dprintf "%a%a %t" d_ikind ikind d_attrlistpost a docName
  | TBitfield(ikind,i,a) -> 
      dprintf "%a%a %t : %d" d_ikind ikind d_attrlistpost a docName i
  | TFloat(fkind, a) -> dprintf "%a%a %t" d_fkind fkind 
        d_attrlistpost a docName
  | TComp (comp, a) -> (* A reference to a struct *)
      let su = if comp.cstruct then "struct" else "union" in
      dprintf "%s %s %a%t" su comp.cname d_attrlistpre a docName

  | TEnum (enum, a) -> 
        dprintf "enum %s %a%t" enum.ename d_attrlistpre a docName

  | TPtr (bt, a)  -> 
      d_decl 
        (fun _ -> 
          dprintf "* %a%t" d_attrlistpre a docName )
        DNStuff
        () 
        bt

  | TArray (elemt, lo, a) -> 
      d_decl 
        (fun _ ->
          dprintf "%a[%t]" 
            parenthname a
            (fun _ -> 
              (match lo with None -> nil
              | Some e -> d_exp () e)))
        DNStuff
        ()
        elemt
  | TFun (restyp, args, isvararg, a) -> 
      d_decl 
        (fun _ -> 
          dprintf "%a(@[%a%t@])" 
            parenthname a
            (docList (chr ',' ++ break) (d_videcl ())) args
            (fun _ -> if isvararg then text ", ..." else nil))
        DNStuff
        ()
        restyp

  | TNamed (n, _, a) -> dprintf "%s%a %t" n d_attrlistpost a docName


(* Only a type (such as for a cast). There seems to be a problem with 
 * printing the top-level attribute (since it would come right before the 
 * missing name). So we strip it, but only if it is not printed in a custom 
 * way. This means that attributes such as const and volatile stay. *)        
and d_type () t = 
  let fixthem (ta: attribute list) = 
    List.filter 
      (fun a -> 
        match !d_attrcustom a with
          Some _ -> true
        | _ -> false)
(*
(Attr(an, _)) -> 
        match an with 
          "const" | "volatile" -> true | _ -> false)
*)
      ta
  in
  let fixattrs = function
      TVoid a -> TVoid (fixthem a)
    | TInt (ik, a) -> TInt (ik, fixthem a)
    | TFloat (fk, a) -> TFloat (fk, fixthem a)
    | TBitfield (ik, w, a) -> TBitfield (ik, w, fixthem a)
    | TNamed (n, t, a) -> TNamed (n, t, fixthem a)
    | TPtr (bt, a) -> TPtr (bt, fixthem a)
    | TArray (bt, lo, a) -> TArray (bt, lo, fixthem a)
    | TComp (comp, a) -> TComp (comp, fixthem a)
    | TEnum (enum, a) -> TEnum (enum, fixthem a)
    | TFun (rt, args, isva, a) -> TFun (rt, args, isva, a)
  in  
  d_decl (fun _ -> nil) DNNothing () (fixattrs t)


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
    Const(c) -> dprintf "%a" d_const c
  | Lval(l) -> dprintf "%a" d_lval l
  | UnOp(u,e1,_) -> 
      let d_unop () u =
        match u with
          Neg -> text "-"
        | BNot -> text "~"
        | LNot -> text "!"
      in
      dprintf "%a %a" d_unop u (d_expprec level) e1

  | BinOp(b,e1,e2,_) -> 
      dprintf "@[%a %a@?%a@]" 
        (d_expprec level) e1 d_binop b (d_expprec level) e2
  | Question (e1, e2, e3) -> 
      dprintf "%a ? %a : %a"
        (d_expprec level) e1 (d_expprec level) e2 (d_expprec level) e3
  | CastE(t,e) -> dprintf "(%a)%a" d_type t (d_expprec level) e
  | SizeOf (t) -> dprintf "sizeof(%a)" d_type t
  | SizeOfE (e) -> dprintf "sizeof(%a)" d_exp e
  | AddrOf(lv) -> 
      dprintf "& %a" (d_lvalprec addrOfLevel) lv

  | StartOf(lv) -> d_lval () lv

and d_init () = function
    SingleInit e -> d_exp () e
  | CompoundInit (t, initl) -> 
      (* We do not print the type of the Compound *)
      let dinit e = d_init () e in
      dprintf "{@[%a@]}"
        (docList (chr ',' ++ break) dinit) initl

and d_binop () b =
  match b with
    PlusA | PlusPI | IndexPI -> text "+"
  | MinusA | MinusPP | MinusPI -> text "-"
  | Mult -> text "*"
  | Div -> text "/"
  | Mod -> text "%"
  | Shiftlt -> text "<<"
  | Shiftrt -> text ">>"
  | Lt | LtP -> text "<"
  | Gt | GtP -> text ">"
  | Le | LeP -> text "<="
  | Ge | GeP -> text ">="
  | Eq | EqP -> text "=="
  | Ne | NeP -> text "!="
  | BAnd -> text "&"
  | BXor -> text "^"
  | BOr -> text "|"
        
(* attributes *)
and d_attr () (Attr(an, args): attribute) =
  (* Add underscores to the name *)
  let an' = if !msvcMode then "__" ^ an else "__" ^ an ^ "__" in
  if args = [] then 
    text an'
  else
    dprintf "%s(%a)" an'
      (docList (chr ',') (d_attrarg ())) args

and d_attrarg () = function
    AId s -> text s
  | AInt n -> num n
  | AStr s -> dprintf "\"%s\"" (escape_string s)
  | AVar vi -> text vi.vname
  | ACons(s,al) -> dprintf "%s(%a)" s
        (docList (chr ',') (d_attrarg ())) al
          
and d_attrlist pre () al = (* Whether it comes before or after stuff *)
  (* Take out the special attributes *)
  let rec loop remaining = function
      [] -> begin
        match remaining with
          [] -> nil
        | _ -> dprintf "__attribute__((%a)) "
              (docList (chr ',' ++ break) 
                 (fun a -> dprintf "%a" d_attr a)) remaining 
      end
    | x :: rest -> begin
        match !d_attrcustom x with
          Some xd -> dprintf "%a %a" insert xd insert (loop remaining rest)
        | None -> loop (x :: remaining) rest
    end
  in
  let res = loop [] al in
  if res = nil then
    res
  else
    if pre then res ++ text " " else text " " ++ res
    
and d_attrlistpre () al = d_attrlist true () al
and d_attrlistpost () al = d_attrlist false () al

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
    | Index (Const(CInt32(z,_,_)), NoOffset) when z = Int32.zero -> 
        dprintf "(*%t)" dobase
    | Index (e, o) ->
        d_offset (fun _ -> dprintf "%t[%a]" dobase d_exp e) o
  in
  match lv with
    Var vi, o -> d_offset (fun _ -> text vi.vname) o
  | Mem e, Field(fi, o) ->
      d_offset (fun _ ->
        dprintf "%a->%s" (d_expprec arrowLevel) e fi.fname) o
(*  | Mem e, NoOffset -> dprintf "(*%a)" (d_expprec derefStarLevel) e *)
  | Mem e, o ->
      d_offset (fun _ -> dprintf "(*%a)" (d_expprec derefStarLevel) e) o

and d_instr () i =
  match i with
  | Set(lv,e,l) -> begin
      (* Be nice to some special cases *)
      match e with
        BinOp((PlusA|PlusPI|IndexPI),Lval(lv'),Const(CInt32(one,_,_)),_)
          when lv == lv' && one = Int32.one ->
          dprintf "\n%s@!%a ++;" (printLine l) d_lval lv
      | BinOp((MinusA|MinusPI),Lval(lv'),
              Const(CInt32(one,_,_)), _) when lv == lv' && one = Int32.one ->
          dprintf "\n%s@!%a --;" (printLine l) d_lval lv
      | BinOp((PlusA|PlusPI|IndexPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
               Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
              Lval(lv'),e,_) when lv == lv' ->
          dprintf "\n%s@!%a %a= %a;" (printLine l) d_lval lv d_binop bop d_exp e
      | _ -> dprintf "\n%s@!%a = %a;" (printLine l) d_lval lv d_exp e
  end
  | Call(vio,e,args,l) ->
      dprintf "\n%s@!%t%t(@[%a@]);" (printLine l)
        (fun _ -> match vio with
          None -> nil |
          Some (vi, iscast) ->
            if iscast then
              dprintf "%s = (%a)" vi.vname d_type vi.vtype
            else
              dprintf "%s = " vi.vname)
        (fun _ -> match e with Lval(Var _, _) -> d_exp () e
        | _ -> dprintf "(%a)" d_exp e)
        (docList (chr ',' ++ break) (d_exp ())) args

  | Asm(tmpls, isvol, outs, ins, clobs, l) ->
      if !msvcMode then
        dprintf "\n%s@!__asm {@[%a@]};@!" (printLine l) (docList line text) tmpls
      else
        dprintf "\n%s@!__asm__ %s(@[%a%a%a%a@]);@!" (printLine l)
          (if isvol then "__volatile__" else "")
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


and d_stmt_next (next: stmt) () (s: stmt) =
  dprintf "%a%t"
    (* print the labels *)
    (docList line (fun l -> d_label () l)) s.labels
    (* print the statement itself. If the labels are non-empty and the 
     * statement is empty, print a semicolon  *)
    (fun _ ->
      if s.skind = Instr [] && s.labels <> [] then
        text ";"
      else
        d_stmtkind next () s.skind)

and d_stmt () (s: stmt) = (* A version that is easier to call *)
  d_stmt_next invalidStmt () s

and d_label () = function
    Label (s, _) -> dprintf "%s: " s
  | Case (e, _) -> dprintf "case %a: " d_exp e
  | Default _ -> text "default: "

and d_block () blk = 
  let rec dofirst () = function
      [] -> nil
    | [x] -> d_stmt_next invalidStmt () x
    | x :: rest -> dorest nil x rest
  and dorest acc prev = function
      [] -> acc ++ (d_stmt_next invalidStmt () prev)
    | x :: rest -> 
        dorest (acc ++ (d_stmt_next x () prev) ++ line)
                  x rest
  in
  dprintf "@[{ @[@!%a@]@!}@]" dofirst blk

and d_stmtkind (next: stmt) () = function
    Return(None, l) -> dprintf "\n%s@!return;" (printLine l)
  | Return(Some e, l) -> dprintf "\n%s@!return (%a);" (printLine l) d_exp e
  | Goto (sref, l) -> d_goto !sref
  | Break l -> dprintf "\n%s@!break;" (printLine l)
  | Continue l -> dprintf "\n%s@!continue;" (printLine l)
(*  | Instr [] -> text "/* empty block */" *)
  | Instr il ->
      dprintf "@[%a@]"
        (docList line (fun i -> d_instr () i)) il
  | If(be,t,[],l) ->
      dprintf "\n%s@!if@[ (%a)@!%a@]" (printLine l) d_exp be d_block t
  | If(be,t,[{skind=Goto(gref,_);labels=[]} as s],l)
      when !gref == next ->
      dprintf "\n%s@!if@[ (%a)@!%a@]" (printLine l) d_exp be d_block t
  | If(be,[],e,l) ->
      dprintf "\n%s@!if@[ (%a)@!%a@]" (printLine l) d_exp (UnOp(LNot,be,intType)) d_block e
  | If(be,[{skind=Goto(gref,_);labels=[]} as s],e,l)
      when !gref == next ->
      dprintf "\n%s@!if@[ (%a)@!%a@]" (printLine l) d_exp  (UnOp(LNot,be,intType))
          d_block e
  | If(be,t,e,l) ->
      dprintf "\n%s@!@[if@[ (%a)@!%a@]@!el@[se@!%a@]@]" (printLine l)
        d_exp be d_block t d_block e
  | Switch(e,b,_,l) ->
      dprintf "\n%s@!@[switch (%a)@!%a@]" (printLine l) d_exp e d_block b
(*
  | Loop(b, l) ->
      See if the first thing in the block is a "if e then skip else break"
      let rec findBreakExp = function
  | Loop({skind=If(e,[],[{skind=Goto (gref,_)} as brk],_)} :: rest, _)
    when !gref == next && brk.labels == [] ->
      dprintf "wh@[ile (%a)@!%a@]" d_exp e d_block rest
*)
  | Loop(b, l) ->
      (* Maybe the first thing is a conditional *)
      try
        let term, body =
          let rec skipEmpty = function
              [] -> []
            | {skind=Instr [];labels=[]} :: rest -> skipEmpty rest
            | x -> x
          in
          match skipEmpty b with
            {skind=If(e,tb,fb,_)} :: rest -> begin
              match skipEmpty tb, skipEmpty fb with
                [], {skind=Break _} :: _  -> e, rest
              | {skind=Break _} :: _, [] -> UnOp(LNot, e, intType), rest
              | _ -> raise Not_found
            end
          | _ -> raise Not_found
        in
        dprintf "\n%s@!wh@[ile (%a)@!%a@]" (printLine l) d_exp term d_block body
      with Not_found ->
        dprintf "\n%s@!wh@[ile (1)@!%a@]" (printLine l) d_block b

        

and d_goto (s: stmt) = 
  (* Grab one of the labels *)
  let rec pickLabel = function
      [] -> None
    | Label (l, _) :: _ -> Some l
    | _ :: rest -> pickLabel rest
  in
  match pickLabel s.labels with
    Some l -> dprintf "goto %s;" l
  | None -> 
      ignore (E.warn "Cannot find label for target of goto\n");
      text "goto __invalid_label;"

and d_fun_decl () f = 
  let stom, rest = separateStorageModifiers f.svar.vattr in
  dprintf "%s%a@!{ @[%a@!@!%a@]@!}" 
    (if f.sinline then "__inline " else "")
    d_videcl f.svar
    (* locals. *)
    (docList line (fun vi -> d_videcl () vi ++ text ";")) f.slocals
    (* the body *)
    d_block f.sbody

and d_videcl () vi = 
  let stom, rest = separateStorageModifiers vi.vattr in
  dprintf "%a%a%a %a"
    (* First the storage modifiers *)
    d_attrlistpre stom
    d_storage vi.vstorage
    (d_decl (fun _ -> dprintf "%s" vi.vname) DNString) vi.vtype
    d_attrlistpost rest
    
and d_fielddecl () fi = 
  dprintf "%a %a;"
    (d_decl 
       (fun _ -> 
         text (if fi.fname = "___missing_field_name" then "" else fi.fname))
       DNString) 
    fi.ftype
    d_attrlistpost fi.fattr
       

   (* Some plain pretty-printers. Unlike the above these expose all the 
    * details of the internal representation *)
let rec d_plainexp () = function
    Const(c) -> dprintf "Const(%a)" d_const c
  | Lval(lv) -> dprintf "Lval(@[%a@])" d_plainlval lv
  | CastE(t,e) -> dprintf "CastE(@[%a,@?%a@])" d_plaintype t d_plainexp e
  | StartOf lv -> dprintf "StartOf(%a)" d_plainlval lv
  | AddrOf (lv) -> dprintf "AddrOf(%a)" d_plainlval lv
  | e -> d_exp () e

and d_plaininit () = function
    SingleInit e -> dprintf "SI(%a)" d_exp e
  | CompoundInit (t, initl) -> 
      dprintf "CI(@[%a,@?%a@])" d_plaintype t
        (docList (chr ',' ++ break) (d_plaininit ())) initl

and d_plainlval () = function
  | Var vi, o -> dprintf "Var(@[%s,@?%a@])" vi.vname d_plainoffset o
  | Mem e, o -> dprintf "Mem(@[%a,@?%a@])" d_plainexp e d_plainoffset o

and d_plainoffset () = function
    NoOffset -> text "NoOffset"
  | Field(fi,o) -> 
      dprintf "Field(@[%s:%a,@?%a@])" 
        fi.fname d_plaintype fi.ftype d_plainoffset o
  | Index(e, o) -> dprintf "Index(@[%a,@?%a@])" d_plainexp e d_plainoffset o

and d_plaintype () (t: typ) = 
  let donecomps : (int, unit) H.t = H.create 13 in (* Keep track of structure 
                                                    * definitions to avoid 
                                                    * going into infinite 
                                                    * loop *)
  let rec scanType () = function
    TVoid a -> dprintf "TVoid(@[%a@])" d_attrlistpost a
  | TInt(ikind, a) -> dprintf "TInt(@[%a,@?%a@])" 
        d_ikind ikind d_attrlistpost a
  | TFloat(fkind, a) -> 
      dprintf "TFloat(@[%a,@?%a@])" d_fkind fkind d_attrlistpost a
  | TBitfield(ikind,i,a) -> 
      dprintf "TBitfield(@[%a,@?%d,@?%a@])" d_ikind ikind i d_attrlistpost a
  | TNamed (n, t, a) ->
      dprintf "TNamed(@[%s,@?%a,@?%a@])" n scanType t d_attrlistpost a
  | TPtr(t, a) -> dprintf "TPtr(@[%a,@?%a@])" scanType t d_attrlistpost a
  | TArray(t,l,a) -> 
      let dl = match l with 
        None -> text "None" | Some l -> dprintf "Some(@[%a@])" d_plainexp l in
      dprintf "TArray(@[%a,@?%a,@?%a@])" 
        scanType t insert dl d_attrlistpost a
  | TEnum(enum,a) -> dprintf "Enum(%s,@[%a@])" enum.ename d_attrlistpost a
  | TFun(tr,args,isva,a) -> 
      dprintf "TFun(@[%a,@?%a%s,@?%a@])"
        scanType tr 
        (docList (chr ',' ++ break) 
           (fun a -> dprintf "%s: %a" a.vname scanType a.vtype)) args
        (if isva then "..." else "") d_attrlistpost a
  | TComp (comp, a) -> 
      if H.mem donecomps comp.ckey then 
        dprintf "TCompLoop(%s %s, _, %a)" 
          (if comp.cstruct then "struct" else "union") comp.cname 
          d_attrlistpost comp.cattr
      else begin
        H.add donecomps comp.ckey (); (* Add it before we do the fields *)
        dprintf "TComp(@[%s %s,@?%a,@?%a,@?%a@])" 
          (if comp.cstruct then "struct" else "union") comp.cname
          (docList (chr ',' ++ break) 
             (fun f -> dprintf "%s : %a" f.fname d_plaintype f.ftype)) 
          comp.cfields
          d_attrlistpost comp.cattr
          d_attrlistpost a
      end
  in
  scanType () t

let _ = 
  let d_attrcustombase = function
    | Attr("const", []) -> Some (text "const")
    | Attr("aconst", []) when not !msvcMode -> 
        Some (text "__attribute__((__const__))")
    | Attr("volatile", []) -> Some (text "volatile")
    | Attr("restrict", []) -> Some (text "restrict")
    | Attr("cdecl", []) when !msvcMode -> Some (text "__cdecl")
    | Attr("stdcall", []) when !msvcMode -> Some (text "__stdcall")
    | Attr("declspec", args) when !msvcMode -> 
        Some (dprintf "__declspec(%a)"
                (docList (chr ',') (d_attrarg ())) args)
    | Attr("asm", args) -> 
        Some (dprintf "__asm__(%a)"
                (docList (chr ',') (d_attrarg ())) args)
    | _ -> None
  in
  setCustomPrintAttribute d_attrcustombase

(*** Define the visiting engine ****)
(* visit all the nodes in a Cil expression *)
let rec visitCilExpr (vis : cilVisitor) (e : exp) : unit =
begin
  (* visit the expression itself *)
  if (vis#vexpr e) then

  (* and visit its subexpressions *)
  let fExp e = visitCilExpr vis e in
  let fTyp t = visitCilType vis t in
  match e with
    Const _ -> ()
  | SizeOf t -> fTyp t
  | SizeOfE e -> fExp e
  | Lval lv -> (visitCilLval vis lv)
  | UnOp(_,e,t) -> fExp e; fTyp t
  | BinOp(_,e1,e2,t) -> fExp e1; fExp e2; fTyp t
  | Question (e1, e2, e3) -> fExp e1; fExp e2; fExp e3
  | CastE(t, e) -> fTyp t; fExp e
  | AddrOf (lv) -> (visitCilLval vis lv)
  | StartOf (lv) -> (visitCilLval vis lv)
end

and visitCilInit (vis: cilVisitor) (i: init) : unit = 
  (* visit the initializer itself *)
  if (vis#vinit i) then

  (* and visit its subexpressions *)
  let fExp e = visitCilExpr vis e in
  let fInit i = visitCilInit vis i in
  let fTyp t = visitCilType vis t in
  match i with
  | SingleInit e -> fExp e
  | CompoundInit (t, initl) ->
      fTyp t;
      List.iter fInit initl

  
and visitCilLval (vis: cilVisitor) (lv: lval) : unit =
begin
  if (vis#vlval lv) then

  match lv with
    Var v, off -> (
      (ignore (vis#vvrbl v));
      (visitCilOffset vis off)
    )
  | Mem e, off -> (
      (visitCilExpr vis e);
      (visitCilOffset vis off)
    )
end

and visitCilOffset (vis: cilVisitor) (off: offset) : unit =
begin
  if (vis#voffs off) then

  match off with
    Field (_, o) -> (visitCilOffset vis o)
  | Index (e, o) -> (visitCilExpr vis e); (visitCilOffset vis o)
  | NoOffset -> ()
end

and visitCilInstr (vis: cilVisitor) (i: instr) : unit =
begin
  if (vis#vinst i) then

  let fExp = visitCilExpr vis in
  let fLval = visitCilLval vis in

  match i with
  | Set(lv,e, _) -> fLval lv; fExp e
  | Call(None,f,args, _) -> fExp f; (List.iter fExp args)
  | Call((Some (v, _)),fn,args, _) -> (
      (ignore (vis#vvrbl v));
      (fExp fn);
      (List.iter fExp args)
    )
  | Asm(_,_,outs,ins,_,_) -> begin
      (List.iter (fun (_, lv) -> fLval lv) outs);
      (List.iter (fun (_, e) -> fExp e) ins)
    end
end


(* visit all nodes in a Cil statement tree in preorder *)
and visitCilStmt (vis: cilVisitor) (s: stmt) : unit =
  let fExp e = (visitCilExpr vis e) in
  let fLval lv = (visitCilLval vis lv) in
  let fOff o = (visitCilOffset vis o) in
  let fBlock b = visitCilBlock vis b in
  let fInst i = visitCilInstr vis i in

  let rec fStmt s = if (vis#vstmt s) then fStmt' s
  and fStmt' s = 
    match s.skind with
      Break _ | Continue _ | Goto _ | Return (None, _) -> ()
    | Return (Some e, _) -> fExp e
    | Loop (b, _) -> fBlock b
    | If(e, s1, s2, _) -> fExp e; fBlock s1; fBlock s2
    | Switch (e, b, _, _) -> fExp e; fBlock b
    | Instr il -> List.iter fInst il
  in
  (* Visit the labels *)
  List.iter (function Case (e, _) -> fExp e | _ -> ()) s.labels;
  fStmt s
    
 
and visitCilBlock (vis: cilVisitor) (b: block) : unit = 
  let fStmt s = (visitCilStmt vis s) in
  List.iter fStmt b


and visitCilType (vis : cilVisitor) (t : typ) : unit =
begin
  (*(trace "visitCilType" (dprintf "%a\n" d_type t));*)

  (* visit 't' itself *)
  if (vis#vtype t) then

  (* look for types referred to inside t's definition *)
  match t with
    TPtr(t, _) -> (visitCilType vis t)
  | TArray(t, None, _) -> (visitCilType vis t)
  | TArray(t, Some e, _) -> (
      (visitCilType vis t);
      (visitCilExpr vis e)
    )
(* Find a way to iterate over types
  | TComp(false, cinfo, _) -> (
      (* iterate over fields *)
      (List.iter
        (fun (finfo : fieldinfo) ->
          (visitCilType vis finfo.ftype))
        cinfo.cfields)
    )
*)
  | TFun(rettype, args, _, _) -> (
      (visitCilType vis rettype);

      (* iterate over formals *)
      (List.iter
        (fun (v : varinfo) ->
          (visitCilVarDecl vis v)      (* visit as a variable decl *)
        )
        args
      )
    )
  | TNamed(s, t, _) -> (
      (visitCilType vis t)
    )
  (* I choose not to recurse into TForward since my present *)
  (* purpose doesn't need it, and it could lead to inf loop *)
  | _ -> ()
end

(* for declarations, we visit the types inside; but for uses, *)
(* we just visit the varinfo node *)
and visitCilVarDecl (vis : cilVisitor) (v : varinfo) : unit =
begin
  (* visit the variable as a decl *)
  if (vis#vvdec v) then

  (* visit the type it's declared as *)
  (visitCilType vis v.vtype)
end

let visitCilFunction (vis : cilVisitor) (f : fundec) : unit =
begin
  if (vis#vfunc f) then (            (* preorder visit *)
    (visitCilVarDecl vis f.svar);      (* hit the function name *)
    (List.iter
      (fun (v : varinfo) ->
        (visitCilVarDecl vis v))       (* visit local declarations *)
      f.slocals);
    (visitCilBlock vis f.sbody);        (* visit the body *)
    (ignore (vis#vfuncPost f))         (* postorder visit *)
  )
end

let visitCilGlobal (vis: cilVisitor) (g: global) : unit =
begin
  if (vis#vglob g) then

  match g with
  | GFun (f, _) -> (visitCilFunction vis f)
  | GType(s, t, _) -> (
      (*(trace "visitTypedef" (dprintf "%s = %a\n" s d_type t));*)
      if (vis#vtdec s t) then (visitCilType vis t)
    )
  | GEnumTag (enum, _) ->
      (* For now behave like the old GType *)
      let t = TEnum (enum, []) in
      if (vis#vtdec "" t) then (visitCilType vis t)

  | GCompTag (comp, _) -> 
      (* For now behave like the old GType *)
      let t = TComp (comp, []) in
      if (vis#vtdec "" t) then (visitCilType vis t)
      
  | GDecl(v, _) -> (visitCilVarDecl vis v)
  | GVar (v, None, _) -> (visitCilVarDecl vis v)
  | GVar (v, Some i, _) -> (visitCilVarDecl vis v); (visitCilInit vis i)
  | _ -> ()
end

let visitCilFile (vis : cilVisitor) (f : file) : unit =
begin
  (trace "visitCilFile" (dprintf "%s\n" f.fileName));

  let fGlob g = (visitCilGlobal vis g) in

  (* primary list of globals *)
  (List.iter fGlob f.globals);

  (* the global initializer *)
  (match f.globinit with
    None -> ()
  | Some g -> (fGlob (GFun(g, locUnknown))))
end

(* sm: I didn't end up using this (because I needed more control *)
(* over the iteration process than the visitor provides), but I *)
(* leave it here anyway *)
let visitCilFileInReverse (vis : cilVisitor) (f : file) : unit =
begin
  (trace "visitCilFileInReverse" (dprintf "%s\n" f.fileName));

  let fGlob g = (visitCilGlobal vis g) in

  (* first the global initializer *)
  (match f.globinit with
    None -> ()
  | Some g -> (fGlob (GFun(g, locUnknown))));

  (* then the primary list of globals, reversed *)
  (List.iter fGlob (List.rev f.globals))
end



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
             vreferenced = false;    (* sm *)
           }  in
  fdec.slocals <- fdec.slocals @ [vi];
  vi

let makeTempVar fdec ?(name = "tmp") typ =
  let name = name ^ (string_of_int (1 + fdec.smaxid)) in
  makeLocalVar fdec name typ


   (* Make a global variable. Your responsibility to make sure that the name
    * is unique *)
let makeGlobalVar name typ =
  let vi = { vname = name;
             vid   = H.hash name;
             vglob = true;
             vtype = typ;
             vdecl = lu;
             vattr = [];
             vstorage = NoStorage;
             vaddrof = false;
             vreferenced = false;    (* sm *)
           }  in
  vi


   (* Make an empty function *)
let emptyFunction name = 
  { svar  = makeGlobalVar name (TFun(voidType, [], false,[]));
    smaxid = 0;
    slocals = [];
    sformals = [];
    sbody = [];
    sinline = false;
  } 


  (* Set the formals and make sure the function type shares them *)
let setFormals (f: fundec) (forms: varinfo list) = 
  f.sformals <- forms;
  match unrollType f.svar.vtype with
    TFun(rt, _, isva, fa) -> 
      f.svar.vtype <- TFun(rt, forms, isva, fa)
  | _ -> E.s (E.bug "Set formals. %s does not have function type\n"
                f.svar.vname)
      

    (* A dummy function declaration handy for initialization *)
let dummyFunDec = emptyFunction "@dummy"
let dummyFile = 
  { globals = [];
    fileName = "<dummy>";
    globinit = None;
    globinitcalled = false}


(* Take the name of a file and make a valid symbol name out of it. There are 
 * a few chanracters that are not valid in symbols *)
let makeValidSymbolName (s: string) = 
  let s = String.copy s in (* So that we can update in place *)
  let l = String.length s in
  for i = 0 to l - 1 do
    let c = String.get s i in
    let isinvalid = 
      match c with
        '-' | '.' -> true
      | _ -> false
    in
    if isinvalid then 
      String.set s i '_';
  done;
  s



let getGlobInit (fl: file) = 
  match fl.globinit with 
    Some f -> f
  | None -> begin
      let f = emptyFunction 
          (makeValidSymbolName ("__globinit_" ^ 
                                (Filename.chop_extension
                                   (Filename.basename fl.fileName))))
      in
      fl.globinit <- Some f;
      f
  end
  

(* Iterate over all globals, including the global initializer *)
let iterGlobals (fl: file)
                (doone: global -> unit) : unit =
  List.iter doone fl.globals;
  (match fl.globinit with
    None -> ()
  | Some g -> doone (GFun(g, locUnknown)))

(* Fold over all globals, including the global initializer *)
let foldGlobals (fl: file) 
                (doone: 'a -> global -> 'a) 
                (acc: 'a) : 'a = 
  let acc' = List.fold_left doone acc fl.globals in
  (match fl.globinit with
    None -> acc'
  | Some g -> doone acc' (GFun(g, locUnknown)))


(* Fold over all globals, including the global initializer *)
let mapGlobals (fl: file) 
               (doone: global -> global) : unit = 
  fl.globals <- List.map doone fl.globals;
  (match fl.globinit with
    None -> ()
  | Some g -> begin
      match doone (GFun(g, locUnknown)) with
        GFun(g', _) -> fl.globinit <- Some g'
      | _ -> E.s (E.bug "mapGlobals: globinit is not a function")
  end)

(* wes: I want to see this at the top level *)
let d_global () = function
    GFun (fundec, _) -> d_fun_decl () fundec ++ line
  | GType (str, typ, _) -> 
      if str = "" then
        dprintf "%a;@!" (d_decl (fun _ -> nil) DNNothing) typ
      else 
        dprintf "typedef %a;@!" (d_decl (fun _ -> text str) DNString) typ

  | GEnumTag (enum, _) -> 
      dprintf "enum@[ %s%a {%a@]@?};" enum.ename d_attrlistpost enum.eattr
        (docList line (fun (n,i) -> dprintf "%s = %a,@?" n d_exp i)) 
        enum.eitems

  | GCompTag (comp, _) -> (* This is a definition of a tag *)      
      let n = comp.cname in
      let su, su1, su2 = 
        if comp.cstruct then "struct", "str", "uct"
                        else "union",  "uni", "on"
      in
      dprintf "%s@[%s %s%a {@!%a@]@!};" su1 su2 n 
        d_attrlistpost comp.cattr
        (docList line (d_fielddecl ())) comp.cfields 
          
  | GVar (vi, io, _) -> 
      dprintf "%a %t;"
        d_videcl vi 
        (fun _ -> match io with None -> nil 
        | Some i -> dprintf " = %a" d_init i)
  | GDecl (vi, _) -> 
      dprintf "%a;" d_videcl vi 
  | GAsm (s, _) -> dprintf "__asm__(\"%s\");@!" (escape_string s)
  | GPragma (Attr(an, args), _) -> 
      let d = 
        if args = [] then 
          text an
        else
          dprintf "%s(%a)" an
            (docList (chr ',') (d_attrarg ())) args
      in
      dprintf "#pragma %a@!" insert d

  | GText s  -> text s

let printFile (out : out_channel) file = 
  printDepth := 99999;  (* We don't want ... in the output *)
  (* If we are in RELEASE mode then we do not print indentation *)
  noBreaks := true; noAligns := true;
  assert (noBreaks := false; noAligns := false; true);
  let print x = fprint out 80 x in
  print (text "/* Generated by safecc */\n\n");
  H.clear definedTypes;
  iterGlobals file (fun g -> print (d_global () g ++ line));
  H.clear definedTypes

    
let printFileWithCustom (out: out_channel) 
                        (custom: attribute -> doc option) 
                        (f: file) = 
  let oldCustom = !d_attrcustom in
  let newCustom a = 
    match custom a with
      None -> oldCustom a
    | x -> x
  in
  d_attrcustom := newCustom;
  printFile out f;
  d_attrcustom := oldCustom


(******************
 ******************
 ******************)



(******************** OPTIMIZATIONS *****)
let rec peepHole1 (* Process one statement and possibly replace it *)
                  (doone: instr -> instr list option)
                  (* Scan a block and recurse inside nested blocks *)
                  (b: block) : unit = 
  List.iter 
    (fun s -> 
      match s.skind with
        Instr il -> 
          let rec loop = function
              [] -> []
            | i :: rest -> begin
                match doone i with
                  None -> i :: loop rest
                | Some sl -> loop (sl @ rest)
            end
          in
          s.skind <- Instr (loop il)
      | If (e, tb, eb, _) -> 
          peepHole1 doone tb;
          peepHole1 doone eb
      | Switch (e, b, _, _) -> peepHole1 doone b
      | Loop (b, l) -> peepHole1 doone b
      | Return _ | Goto _ | Break _ | Continue _ -> ())
    b

let rec peepHole2  (* Process two statements and possibly replace them both *)
                   (dotwo: instr * instr -> instr list option)
                   (b: block) : unit = 
  List.iter 
    (fun s -> 
      match s.skind with
        Instr il -> 
          let rec loop = function
              [] -> []
            | [i] -> [i]
            | (i1 :: ((i2 :: rest) as rest2)) -> 
                begin
                  match dotwo (i1,i2) with
                    None -> i1 :: loop rest2
                  | Some sl -> loop (sl @ rest)
                end
          in
          s.skind <- Instr (loop il)
      | If (e, tb, eb, _) -> 
          peepHole2 dotwo tb;
          peepHole2 dotwo eb
      | Switch (e, b, _, _) -> peepHole2 dotwo b
      | Loop (b, l) -> peepHole2 dotwo b
      | Return _ | Goto _ | Break _ | Continue _ -> ())
    b



(**** Compute the type of an expression ****)
let rec typeOf (e: exp) : typ = 
  match e with
  | Const(CInt32 (_, ik, _)) -> TInt(ik, [])
  | Const(CChr _) -> charType
  | Const(CStr _) -> charPtrType 
  | Const(CReal (_, fk, _)) -> TFloat(fk, [])
  | Lval(lv) -> typeOfLval lv
  | SizeOf _ | SizeOfE _ -> uintType
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | Question (_, e2, _) -> typeOf e2
  | CastE (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLval lv, [])
  | StartOf (lv) -> begin
      match unrollType (typeOfLval lv) with
        TArray (t,_, _) -> TPtr(t, [])
      | TFun _ as t -> TPtr(t, [])
     | _ -> E.s (E.bug "typeOf: StartOf on a non-array or non-function")
  end
      
and typeOfInit (i: init) : typ = 
  match i with 
    SingleInit e -> typeOf e
  | CompoundInit (t, _) -> t

and typeOfLval = function
    Var vi, off -> typeOffset vi.vtype off
  | Mem addr, off -> begin
      match unrollType (typeOf addr) with
        TPtr (t, _) -> typeOffset t off
      | _ -> E.s (E.bug "typeOfLval: Mem on a non-pointer")
  end

and typeOffset basetyp = function
    NoOffset -> basetyp
  | Index (_, o) -> begin
      match unrollType basetyp with
        TArray (t, _, _) -> typeOffset t o
      | t -> E.s (E.bug "typeOffset: Index on a non-array: %a" d_plaintype t)
  end 
  | Field (fi, o) -> typeOffset fi.ftype o



let dExp: doc -> exp = 
  fun d -> Const(CStr(sprint 80 d))

let dInstr: doc -> location -> instr = 
  fun d l -> Asm([sprint 80 d], false, [], [], [], l)

let dGlobal: doc -> location -> global = 
  fun d l -> GAsm(sprint 80 d, l)

let rec addOffset toadd (off: offset) : offset =
  match off with
    NoOffset -> toadd
  | Field(fid', offset) -> Field(fid', addOffset toadd offset)
  | Index(e, offset) -> Index(e, addOffset toadd offset)

 (* Add an offset at the end of an lv *)      
let addOffsetLval toadd (b, off) : lval =
 b, addOffset toadd off



  (* Make a Mem, while optimizing StartOf. The type of the addr must be 
   * TPtr(t) and the type of the resulting expression is t *)
let mkMem (addr: exp) (off: offset) : exp =  
  let isarray = (* Maybe the addr is the start of an array *)
    match addr with 
      StartOf(lv) when 
      (match unrollType (typeOfLval lv) with TArray _ -> true | _ -> false)
        -> Some lv
    | _ -> None
  in
  let res = 
    match isarray, off with
      Some lv, Index _ -> (* index on an array *)
        Lval(addOffsetLval off lv)
    | Some lv, _ -> (* non-index on an array *)
        Lval(addOffsetLval (Index(zero, off)) lv)
    | None, Index(ei, resto) -> (* index on a non-array *)
        Lval(Mem (BinOp(IndexPI, addr, ei, typeOf addr)), resto) 
    | None, _ -> (* non-index on a non-array *)
        Lval(Mem addr, off)
  in
(*  ignore (E.log "memof : %a:%a\nresult = %a\n" 
            d_plainexp addr d_plainoffset off d_plainexp res); *)
  res
          


let mkAddrOf ((b, off) as lval) : exp = 
  match unrollType (typeOfLval lval) with
    TArray _ -> StartOf lval
  | TFun _ -> StartOf lval
  | _ -> begin
      (match lval with
        Var vi, off when vi.vstorage = Register -> vi.vstorage <- NoStorage
      | _ -> ());
      AddrOf(lval)
  end

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
    TPtr _ -> true
  | _ -> false

let isFunctionType t = 
  match unrollType t with
    TFun _ -> true
  | _ -> false


let rec typeAttrs = function
    TVoid a -> a
  | TInt (_, a) -> a
  | TFloat (_, a) -> a
  | TBitfield (_, _, a) -> a
  | TNamed (n, t, a) -> addAttributes a (typeAttrs t)
  | TPtr (_, a) -> a
  | TArray (_, _, a) -> a
  | TComp (comp, a) -> addAttributes comp.cattr a
  | TEnum (enum, a) -> addAttributes enum.eattr a
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
  | TComp (comp, _) -> TComp (comp, a)
  | TEnum (enum, _) -> TEnum (enum, a)
  | TFun (r, args, v, _) -> TFun(r,args,v,a)


let typeAddAttributes a0 t = 
  if a0 == [] then t else
  let add a = addAttributes a0 a in
  match t with 
    TVoid a -> TVoid (add a)
  | TInt (ik, a) -> TInt (ik, add a)
  | TFloat (fk, a) -> TFloat (fk, add a)
  | TBitfield (i, s, a) -> TBitfield (i, s, add a)
  | TEnum (enum, a) -> TEnum (enum, add a)
  | TPtr (t, a) -> TPtr (t, add a)
  | TArray (t, l, a) -> TArray (t, l, add a)
  | TFun (t, args, isva, a) -> TFun(t, args, isva, add a)
  | TComp (comp, a) -> TComp (comp, add a)
  | TNamed (n, t, a) -> TNamed (n, t, add a)

let typeRemoveAttributes (a0: attribute list) t = 
  let drop (al: attribute list) = dropAttributes a0 al in
  match t with 
    TVoid a -> TVoid (drop a)
  | TInt (ik, a) -> TInt (ik, drop a)
  | TFloat (fk, a) -> TFloat (fk, drop a)
  | TBitfield (i, s, a) -> TBitfield (i, s, drop a)
  | TEnum (enum, a) -> TEnum (enum, drop a)
  | TPtr (t, a) -> TPtr (t, drop a)
  | TArray (t, l, a) -> TArray (t, l, drop a)
  | TFun (t, args, isva, a) -> TFun(t, args, isva, drop a)
  | TComp (comp, a) -> TComp (comp, drop a)
  | TNamed (n, t, a) -> TNamed (n, t, drop a)

     (* Type signatures. Two types are identical iff they have identical 
      * signatures *)
type typsig = 
    TSArray of typsig * exp option * attribute list
  | TSPtr of typsig * attribute list
  | TSComp of bool * string * attribute list
  | TSFun of typsig * (typsig * attribute list) list * bool * attribute list
  | TSEnum of string * attribute list
  | TSBase of typ

(* Compute a type signature *)
let rec typeSigAttrs doattr t = 
  let typeSig = typeSigAttrs doattr in
  match t with 
  | (TInt _ | TFloat _ | TBitfield _ | TVoid _) -> TSBase t
  | TEnum (enum, a) -> TSEnum (enum.ename, doattr a)
  | TPtr (t, a) -> TSPtr (typeSig t, doattr a)
  | TArray (t,l,a) -> TSArray(typeSig t, l, doattr a)
  | TComp (comp, a) -> 
      TSComp (comp.cstruct, comp.cname, doattr (addAttributes comp.cattr a))
  | TFun(rt,args,isva,a) -> TSFun(typeSig rt, 
                                  List.map (fun vi -> (typeSig vi.vtype, 
                                                       doattr vi.vattr)) args,
                                  isva, doattr a)
  | TNamed(_, t, a) -> typeSigAddAttrs (doattr a) (typeSig t)
      
and typeSigAddAttrs a0 t = 
  if a0 == [] then t else
  match t with 
    TSBase t -> TSBase (typeAddAttributes a0 t)
  | TSPtr (ts, a) -> TSPtr (ts, addAttributes a0 a)
  | TSArray (ts, l, a) -> TSArray(ts, l, addAttributes a0 a)
  | TSComp (iss, n, a) -> TSComp (iss, n, addAttributes a0 a)
  | TSEnum (n, a) -> TSEnum (n, addAttributes a0 a)
  | TSFun(ts, tsargs, isva, a) -> TSFun(ts, tsargs, isva, addAttributes a0 a)


let typeSig t = typeSigAttrs (fun al -> al) t


let rec doCastT (e: exp) (oldt: typ) (newt: typ) = 
  (* Do not remove old casts because they are conversions !!! *)
  if typeSig oldt = typeSig newt then
    e
  else
    (* If the new type is a Bitfield then cast to the base type *)
    match newt with
      TBitfield (ik, _, a) -> doCastT e oldt (TInt(ik, a))
    | _ -> CastE(newt,e)

let doCast (e: exp) (newt: typ) = 
  doCastT e (typeOf e) newt

type existsAction = 
    ExistsTrue                          (* We have found it *)
  | ExistsFalse                         (* Stop processing this branch *)
  | ExistsMaybe                         (* This node is not what we are 
                                         * looking for but maybe its 
                                         * successors are *)
let existsType (f: typ -> existsAction) (t: typ) : bool = 
  let memo : (int, unit) H.t = H.create 17 in  (* Memo table *)
  let rec loop t = 
    match f t with 
      ExistsTrue -> true
    | ExistsFalse -> false
    | ExistsMaybe -> 
        (match t with 
          TNamed (_, t', _) -> loop t'
        | TComp (c, _) -> loopComp c
        | TArray (t', _, _) -> loop t'
        | TPtr (t', _) -> loop t'
        | TFun (rt, args, _, _) -> 
            (loop rt || List.exists (fun a -> loop a.vtype) args)
        | _ -> false)
  and loopComp c = 
    if H.mem memo c.ckey then 
      (* We are looping, the answer must be false *)
      false
    else begin
      H.add memo c.ckey ();
      List.exists (fun f -> loop f.ftype) c.cfields
    end
  in
  loop t
          
  

(*** Constant folding ***)
let rec constFold (e: exp) : exp = 
  match e with
    BinOp(bop, e1, e2, tres) -> constFoldBinOp bop e1 e2 tres
  | UnOp(Neg, e1, tres) -> begin
      match constFold e1 with
        Const(CInt32(i,_,_)) -> integer32 (Int32.neg i)
      | _ -> e
  end
  | _ -> e

and constFoldBinOp bop e1 e2 tres = 
  let e1' = constFold e1 in
  let e2' = constFold e2 in
  if isIntegralType tres then
    let newe = 
      let rec mkInt = function
          Const(CChr c) -> Const(CInt32(Int32.of_int (Char.code c), 
                                        IInt, None))
        | CastE(TInt _, e) -> mkInt e
        | e -> e
      in
      (* See if the result is unsigned *)
      let isunsigned = 
        match unrollType tres with
          TInt((IUInt | IUChar | IUShort | IULong | IULongLong), _) -> true
        | _ -> false
      in
      match bop, mkInt e1', mkInt e2' with
        PlusA, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer32 (Int32.add i1 i2)
      | PlusA, Const(CInt32(z,_,_)), e2'' when z = Int32.zero -> e2''
      | PlusA, e1'', Const(CInt32(z,_,_)) when z = Int32.zero -> e1''
      | PlusPI, e1'', Const(CInt32(z,_,_)) when z = Int32.zero -> e1''
      | IndexPI, e1'', Const(CInt32(z,_,_)) when z = Int32.zero -> e1''
      | MinusPI, e1'', Const(CInt32(z,_,_)) when z = Int32.zero -> e1''
      | MinusA, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer32 (Int32.sub i1 i2)
      | Mult, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer32 (Int32.mul i1 i2)
      | Div, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> begin
          try integer32 (Int32.div i1 i2)
          with Division_by_zero -> 
            E.s (unimp "Division by zero while constant folding")
      end
      | Mod, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> begin
          try integer32 (Int32.rem i1 i2)
          with Division_by_zero -> 
            E.s (unimp "Division by zero while constant folding")
      end
      | BAnd, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer32 (Int32.logand i1 i2)
      | BOr, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer32 (Int32.logor i1 i2)
      | BXor, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer32 (Int32.logxor i1 i2)
      | Shiftlt, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer32 (Int32.shift_left i1 (Int32.to_int i2))
      | Shiftrt, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          if isunsigned then 
            integer32 (Int32.shift_right_logical i1 (Int32.to_int i2))
          else
            integer32 (Int32.shift_right i1 (Int32.to_int i2))

      | Eq, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer (if i1 = i2 then 1 else 0)
      | Ne, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) -> 
          integer (if i1 <> i2 then 1 else 0)
      | Le, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) 
            when not isunsigned -> 
          integer (if i1 <= i2 then 1 else 0)
      | Ge, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) 
            when not isunsigned -> 
          integer (if i1 >= i2 then 1 else 0)
      | Lt, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) 
            when not isunsigned -> 
          integer (if i1 < i2 then 1 else 0)
      | Gt, Const(CInt32(i1,_,_)),Const(CInt32(i2,_,_)) 
            when not isunsigned -> 
          integer (if i1 > i2 then 1 else 0)
      | _ -> BinOp(bop, e1', e2', tres)
    in
    newe
  else
    BinOp(bop, e1', e2', tres)


(* Try to do an increment, with constant folding *)
let increm (e: exp) (i: int) =
  let et = typeOf e in
  let bop = if isPointerType et then PlusPI else PlusA in
  constFold (BinOp(bop, e, integer i, et))
      
  

(*** Make a initializer for zeroe-ing a data type ***)
let rec makeZeroInit (t: typ) : init = 
  match unrollType t with
    TInt (ik, _) -> SingleInit (Const(CInt32(Int32.zero, ik, None)))
  | TFloat(fk, _) -> SingleInit(Const(CReal(0.0, fk, None)))
  | (TEnum _ | TBitfield _) -> SingleInit zero
  | TComp (comp, _) as t' when comp.cstruct -> 
      CompoundInit (t', 
                    List.map (fun f -> makeZeroInit f.ftype) 
                      comp.cfields)
  | TComp (comp, _) as t' when not comp.cstruct -> 
      let fstfield = 
        match comp.cfields with
          f :: _ -> f
        | [] -> E.s (E.unimp "Cannot create init for empty union")
      in
      CompoundInit(t, [makeZeroInit fstfield.ftype])

  | TArray(bt, Some len, _) as t' -> 
      let n = 
        match constFold len with
          Const(CInt32(n, _, _)) -> Int32.to_int n
        | _ -> E.s (E.unimp "Cannot understand length of array")
      in
      let initbt = makeZeroInit bt in
      let rec loopElems acc i = 
        if i >= n then acc
        else loopElems (initbt :: acc) (i + 1) 
      in
      CompoundInit(t', loopElems [] 0)
  | TPtr _ as t -> SingleInit(CastE(t, zero))
  | _ -> E.s (E.unimp "makeZeroCompoundInit: %a" d_plaintype t)


(**** Fold over the list of initializers in a Compound ****)
let foldLeftCompound (doinit: offset -> init -> typ -> 'a -> 'a)
    (ct: typ) 
    (initl: init list)
    (acc: 'a) : 'a = 
  match unrollType ct with
    TArray(bt, _, _) -> 
      let rec foldArray  
          (nextidx: exp) 
          (initl: init list)
          (acc: 'a) : 'a  =
        let incrementIdx = function
            Const(CInt32(n, ik, _)) -> Const(CInt32(Int32.succ n, ik, None))
          | e -> BinOp(PlusA, e, one, intType)
        in
        match initl with
          [] -> acc
        | ie :: restinitl ->
            (* Now do the initializer expression *)
            let acc' = doinit (Index(nextidx, NoOffset)) ie bt acc in
            foldArray (incrementIdx nextidx) restinitl acc'
      in
      foldArray zero initl acc

  | TComp (comp, _) -> 
      if comp.cstruct then
        let rec foldFields 
            (allflds: fieldinfo list) 
            (nextflds: fieldinfo list) 
            (initl: init list)
            (acc: 'a) : 'a = 
          match initl with 
            [] -> acc   (* We are done *)
          | ie :: restinitl ->
              let nextfields, thisfield = 
                begin
                  match nextflds with
                    [] -> E.s (E.unimp "Too many initializers")
                  | x :: xs -> xs, x
                end
              in
              (* Now do the initializer expression *)
              let acc' = 
                doinit (Field(thisfield, NoOffset)) ie thisfield.ftype acc in
              foldFields allflds nextfields restinitl acc'
        in
        foldFields comp.cfields comp.cfields initl acc
      else
        (* UNION *)
        let oneinit, firstfield = 
          match initl, comp.cfields with
            [x], f :: _  -> x, f
          | _ -> E.s (E.bug "Compound for union should have only one init")
        in
        doinit (Field(firstfield, NoOffset)) oneinit firstfield.ftype acc
        

  | _ -> E.s (E.unimp "Type of Compound is not array or struct or union")



let rec isCompleteType t =
  match unrollType t with
  | TArray(t, None, _) -> false
  | TArray(t, Some z, _) when isZero z -> false
  | TComp (comp, _) -> (* Struct or union *)
      List.for_all (fun fi -> isCompleteType fi.ftype) comp.cfields
  | _ -> true


(* removeUnusedTemps has been moved to rmtmps.ml *)  


(*** Alpha conversion ***)
(* Create a new name based on a given name. The new name is formed from a 
 * prefix (obtained from the given name as the longest prefix that ends with 
 * a non-digit), followed by a '_' and then by a positive integer suffix. The 
 * first argument is a table mapping name prefixes with the largest suffix 
 * used so far for that prefix. The largest suffix is one when only the 
 * version without suffix has been used. *)
let rec newAlphaName (alphaTable: (string, int ref) H.t)
                     (lookupname: string) : string = 
  let prefix, sep, suffix = splitNameForAlpha lookupname in
  (* ignore (E.log "newAlphaName(%s). P=%s, S=%d\n" lookupname prefix suffix);
     *)
  let newname = 
    try
      let rc = H.find alphaTable prefix in
      let newsuffix, sep = 
        if suffix > !rc then suffix, sep else !rc + 1, "_" in
      rc := newsuffix;
      prefix ^ sep ^ (string_of_int newsuffix)
    with Not_found -> begin (* First variable with this prefix *)
      H.add alphaTable prefix (ref suffix);
      lookupname  (* Return the original name *)
    end
  in
  newname
  
and splitNameForAlpha (lookupname: string) : (string * string * int) = 
  (* Split the lookup name into a prefix, a separator (empty or _) and a 
   * suffix. The suffix is numberic and is separated by _ from the prefix  *)
  let l = String.length lookupname in
  let rec suffixStarts n = 
    if n = 0 then 0 else 
    let c = String.get lookupname (n - 1) in
    if c >= '0' && c <= '9' then suffixStarts (n - 1) else n
  in
  let sStart = suffixStarts l in
  (* Get the suffix *)
  let suffix = 
    if sStart >= l then -1
    else int_of_string (String.sub lookupname sStart (l - sStart))
  in
  (* Get the prefix, bug ignore a trailing _ *)
  let prefix, sep = 
    let pEnd, sep = (* prefix end *)
      if sStart < l && String.get lookupname (sStart - 1) = '_' then
        sStart - 2, "_"
      else
        sStart - 1, ""
    in
    if pEnd >= 0 then 
      String.sub lookupname 0 (pEnd + 1), sep
    else
      "", sep
  in
  prefix, sep, suffix

let docAlphaTable (alphaTable: (string, int ref) H.t) = 
  let acc : (string * int) list ref = ref [] in
  H.iter (fun k d -> acc := (k, !d) :: !acc) alphaTable;
  docList line (fun (k, d) -> dprintf "  %s -> %d" k d) () !acc


(**
 **
 ** MACHINE DEPENDENT PART
 **
 **)

     
type offsetAcc = 
    { oaFirstFree: int;   (* The first free bit *)
      oaLastFieldStart: int;   (* Where the previous field started *)
      oaLastFieldWidth: int;   (* The width of the previous field. Might not 
                                * be same as FirstFree - FieldStart because 
                                * of internal padding *)
      oaPrevBitPack: (int * ikind * int) option; (* If the previous fields 
                                                   * were packed bitfields, 
                                                   * the bit where packing 
                                                   * has started, the ikind 
                                                   * of the bitfield and the 
                                                   * width of the ikind *)
    } 

let rec offsetOfFieldAcc (fi: fieldinfo) 
                         (sofar: offsetAcc) : offsetAcc = 
  (* field type *)
  let ftype = unrollType fi.ftype in
  match ftype, sofar.oaPrevBitPack with (* Check for a bitfield that fits in 
                                         * the current pack after some other 
                                         * bitfields  *)
    TBitfield(ikthis, wdthis, _), Some (packstart, ikprev, wdpack)
      when ((not !msvcMode || ikthis = ikprev) && 
            packstart + wdpack >= sofar.oaFirstFree + wdthis) ->
              { oaFirstFree = sofar.oaFirstFree + wdthis;
                oaLastFieldStart = sofar.oaFirstFree; 
                oaLastFieldWidth = wdthis;
                oaPrevBitPack = sofar.oaPrevBitPack
              } 

  | _, Some (packstart, _, wdpack) -> (* Finish up the bitfield pack and 
                                       * restart *)
      offsetOfFieldAcc fi
        { oaFirstFree = packstart + wdpack;
          oaLastFieldStart = sofar.oaLastFieldStart;
          oaLastFieldWidth = sofar.oaLastFieldWidth;
          oaPrevBitPack = None }
  | _ -> 
      (* no active bitfield pack. Compute the internalPadding. Returns the 
       * alignment boundary for internal padding for the current field  *)
      let rec internalPaddingAlign = function 
          TInt((IChar|ISChar|IUChar), _) -> 1
        | TInt((IShort|IUShort), _) -> 2
        | TInt((IInt|IUInt), _) -> 4
        | TInt((ILong|IULong), _) -> 4
        | TInt((ILongLong|IULongLong), _) -> 4  (* !!! is this correct *)
        | TEnum _ -> 4 (* !!! Is this correct? *)
        | TBitfield(ik, _, a) -> 
            internalPaddingAlign (TInt(ik, a)) (* Is this correct ? *)
        | TFloat(FFloat, _) -> 4
        | TFloat((FDouble|FLongDouble), _) -> 8
        | TNamed (_, t, _) -> internalPaddingAlign t
        | TComp (comp, a) -> 4 (* Is this correct ? *)
        | TArray _ -> 4 (* Is this correct ? *)
        | TPtr _ -> 4
        | (TVoid _ | TFun _) -> E.s (E.bug "internalPaddingAlign")
      in
      let internPad = (internalPaddingAlign ftype) lsl 3 in
      let newStart = 
        (sofar.oaFirstFree + internPad - 1) land (lnot (internPad - 1)) in
      (* ignore (E.log "firstFree = %d, internPad = %d, newStart = %d\n"
                sofar.oaFirstFree internPad newStart); *)
      (* Now compute the width of this field *)
      let mkRes thiswd btpack = 
        { oaFirstFree = newStart + thiswd;
          oaLastFieldStart = sofar.oaFirstFree;
          oaLastFieldWidth = thiswd;
          oaPrevBitPack = btpack }
      in
      match unrollType ftype with
        TBitfield(ik, wd, a) -> 
          let wdpack = bitsSizeOf (TInt(ik, a)) in
          { oaFirstFree = newStart + wd;
            oaLastFieldStart = newStart;
            oaLastFieldWidth = wd;
            oaPrevBitPack = Some (newStart, ik, wdpack); }
      | _ ->
          let wd = bitsSizeOf ftype in
          (* ignore (E.log "non-bitfield (%a): wd=%d\n"
                    d_type ftype wd); *)
          { oaFirstFree = newStart + wd;
            oaLastFieldStart = newStart;
            oaLastFieldWidth = wd;
            oaPrevBitPack = None;
          } 

(* should we make a big noise when we cannot take the size of something? *)
and flagSizeOfErrors = ref true 
        
(* The size of a type, in bits. If struct or array then trailing padding is 
 * added *)
and bitsSizeOf t = 
  match t with 
    TInt((IChar|ISChar|IUChar), _) -> 8
  | TInt((IShort|IUShort), _) -> 16
  | TInt((IInt|IUInt), _) -> 32
  | TInt((ILong|IULong), _) -> 32
  | TInt((ILongLong|IULongLong), _) -> 64
  | TEnum _ -> 32 (* !!! is this correct ? *)
  | TBitfield(ik, wd, a) -> wd
  | TFloat(FFloat, _) -> 32
  | TFloat((FDouble|FLongDouble), _) -> 64
  | TNamed (_, t, _) -> bitsSizeOf t
  | TPtr _ -> 32
  | TComp (comp, _) when comp.cfields = [] -> 
      raise Not_found (*abstract type*)
  | TComp (comp, _) when comp.cstruct -> (* Struct *)
        (* Go and get the last offset *)
      let startAcc = 
        { oaFirstFree = 0;
          oaLastFieldStart = 0;
          oaLastFieldWidth = 0;
          oaPrevBitPack = None;
        } in
      let lastoff = 
        List.fold_left (fun acc fi -> offsetOfFieldAcc fi acc) 
          startAcc comp.cfields 
      in
      addTrailing lastoff.oaFirstFree
        
  | TComp (comp, _) -> (* when not comp.cstruct *)
        (* Get the maximum of all fields *)
      let startAcc = 
        { oaFirstFree = 0;
          oaLastFieldStart = 0;
          oaLastFieldWidth = 0;
          oaPrevBitPack = None;
        } in
      let max = 
        List.fold_left (fun acc fi -> 
          let lastoff = offsetOfFieldAcc fi startAcc in
          if lastoff.oaFirstFree > acc then
            lastoff.oaFirstFree else acc) 0 comp.cfields in
        (* Add trailing by simulating adding an extra field *)
      addTrailing max

  | TArray(t, Some (Const(CInt32(l,_,_))),_) -> 
      addTrailing ((bitsSizeOf t) * (Int32.to_int l))

  | TArray(t, None, _) -> raise Not_found
        
  | TArray _ -> 
      if (!flagSizeOfErrors) then 
        E.s (E.unimp "sizeOfInt for non-constant length array:@!%a" d_type t)
      else
        raise Not_found
	| TFun _ -> 32
  | TVoid _ -> 
      if (!flagSizeOfErrors) then
        E.s (E.bug "bitsSizeOf void")
      else
        raise Not_found


and addTrailing nrbits = 
    let roundto = 32 in
    (nrbits + roundto - 1) land (lnot (roundto - 1))

and sizeOf t = 
    match unrollType t with
      TBitfield _ -> E.s (E.bug "sizeOf(bitfield) not allowed")
    | t' -> begin
        try
          integer ((bitsSizeOf t') lsr 3)
        with Not_found -> SizeOf(t')
    end
            

 

let offsetOf (fi: fieldinfo) (startcomp: int) : int * int = 
  (* Construct a list of fields preceeding and including this one *)
  let prevflds = 
    let rec loop = function
        [] -> E.s (E.bug "Cannot find field %s\n" fi.fname)
      | fi' :: _ when fi' == fi -> [fi']
      | fi' :: rest -> fi' :: loop rest
    in
    loop fi.fcomp.cfields
  in
  let lastoff = 
    List.fold_left (fun acc fi' -> offsetOfFieldAcc fi' acc)
      { oaFirstFree = startcomp;
        oaLastFieldStart = 0;
        oaLastFieldWidth = 0;
        oaPrevBitPack = None } prevflds
  in
  (lastoff.oaLastFieldStart, lastoff.oaLastFieldWidth)
      
 
    
