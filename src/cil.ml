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

let printLn= ref true                 (* Whether to print line numbers *)
let printLnComment= ref false

type location = { 
    line: int;				(* -1 means "do not know" *)
    file: string; 
}

let locUnknown = { line = -1; file = ""; }
(* A reference to the current location *)
let currentLoc : location ref = ref locUnknown

let debugConstFold = false

let printShortTypes = ref false

(* Information about a variable. Use one of the makeLocalVar, makeTempVar or 
 * makeGlobalVar to create instances of this data structure. These structures a
 * re shared by all references to the variable. So, you can change the name
 * easily, for example *)
type varinfo = { 
    mutable vname: string;				
    mutable vtype: typ;                 (* The declared type *)
    mutable vattr: attribute list;
    mutable vstorage: storage;
    (* The other fields are not used in varinfo as they appear in the formal 
     * argument list in a TFun type *)


    mutable vglob: bool;	(* Is this a global variable? *)

    mutable vdecl: location;            (* where was this variable declared? *)

    mutable vid: int;	(* Unique integer indentifier. For globals this is a 
                         * hash of the name. Locals are numbered from 0 
                         * starting with the formal arguments. This field 
                         * will be set for you if you use one of the 
                         * makeLocalVar, makeTempVar or makeGlobalVar *)
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
                                         * the value of missingFieldName in 
                                         * which case it is not printed  *)
    mutable ftype: typ;
    mutable fbitfield: int option;      (* If a bitfield then ftype should be 
                                         * an integer type *)
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
    mutable creferenced: bool;          (* true if used *)
  }

(* Information about an enumeration. This is shared by all references to an
 * enumeration. Make sure you have a GEnumTag for each of of these.   *)
and enuminfo = {
    mutable ename: string;             (* the name. Always non-empty *)
    mutable eitems: (string * exp) list;(* items with names and values. This
                                         * list should be non-empty. The item
                                         * values must be compile-time
                                         * constants. *)
    mutable eattr: attribute list;     (* attributes *)
    mutable ereferenced: bool;         (* true if used *)
}

(* what is the type of an expression? Keep all attributes sorted. Use 
 * addAttribute and addAttributes to construct list of attributes *)
and typ =
    TVoid of attribute list
  | TInt of ikind * attribute list
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
  | TFun of typ * varinfo list option * bool * attribute list

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
  | ASizeOf of typ                      (* A way to talk about types *)
  | ASizeOfE of attrarg
  | AUnOp of unop * attrarg
  | ABinOp of binop * attrarg * attrarg

(* literal constants *)
and constant =
  | CInt64 of int64 * ikind * string option 
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

  | AlignOf    of typ                   (* Has UInt type *)
  | AlignOfE   of exp 

                                        (* Give the type of the result *)
  | UnOp       of unop * exp * typ

                                        (* Give the type of the result. The
                                         * arithemtic conversions are made
                                         * explicit for the arguments *)
  | BinOp      of binop * exp * exp * typ

  | CastE      of typ * exp            (* Use doCast to make casts *)

  | AddrOf     of lval                 (* Always use mkAddrOf to construct
                                         * one of these *)

  | StartOf    of lval                  (* There is no C correspondent for 
                                         * this. C has implicit coercions 
                                         * from an array to the address of 
                                         * the first element. StartOf is used 
                                         * in CIL to simplify type checking 
                                         * and is just an explicit form of 
                                         * the above mentioned implicit 
                                         * convertion. It is not printed. 
                                         * Given an lval of type TArray(T) 
                                         * produces an expression of type 
                                         * TPtr(T). *)

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
 * pointer expression. We distinguish the two cases so that we can tell 
 * quickly whether we are accessing some component of a variable directly or 
 * we are accessign a memory location through a pointer. *)

and lval =
    lbase * offset

(* The meaning of an lval is expressed as a function "[lval] = (a, T)" that
 * returns a memory address "a" and a type "T" of the object stored starting
 * at the address "a".  *)

(* The meaning of an lbase is expressed as a similar function. *)

(* The meaning of an offset is expressed as a function "[offset](a, T) = (a', 
 * T')" where (a, T) is the meaning of the base to be used with the offset. 
 * The result is another address and another base type  *)

(* With this notation we define
  
      [(lbase, offset)] = [offset] [lbase]   (where juxtaposition is just 
                                              function application)
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


(* The following equivalences hold *)
(* Mem(AddrOf(Mem a, aoff)), off   = Mem a, aoff + off                *)
(* Mem(AddrOf(Var v, aoff)), off   = Var v, aoff + off                *)
(* AddrOf (Mem a, NoOffset)        = a                                *)

(**** INSTRUCTIONS. May cause effects directly but may not have control flow.*)
and instr =
    Set        of lval * exp * location  (* An assignment. A cast is present 
                                          * if the exp has different type 
                                          * from lval *)
  | Call       of lval option * exp * exp list * location
 			 (* optional: result temporary variable. A cast might 
                          * be necessary if the declared result type of the 
                          * function is not the same as that of the 
                          * destination, the function value, argument list, 
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
  | Asm        of attribute list * (* Really only const and volatile can appear 
                               * here *)
                  string list *         (* templates (CR-separated) *)
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

  | Block of block                      (* Just a block of statements. Use it 
                                         * as a way to keep some attributes 
                                         * local *)


(* A block is a sequence of statements with the control falling through from 
 * one element to the next *)
and block = 
   { mutable battrs: attribute list;   (* Attributes for the block *)
     mutable bstmts: stmt list;
   } 

and label = 
    Label of string * location * bool   (* A real label.*)
		(* If the bool is "true", the label is from the input source program.
		 * If the bool is "false", the label was created by CIL or some
		 * other transformation *)
  | Case of exp * location              (* A case statement *)
  | Default of location                 (* A default statement *)


type fundec =
    { mutable svar:     varinfo;        (* Holds the name and type as a
                                         * variable, so we can refer to it
                                         * easily from the program *)
      mutable sformals: varinfo list;    (* These are the formals. These must 
                                         * be shared with the formals that 
                                         * appear in the type of the 
                                         * function. Use setFormals or 
                                         * makeFormalVar to set these formals 
                                         * and ensure that they are reflected 
                                         * in the function type. Do not make 
                                         * copies of these because the body 
                                         * refers to them.  *)
      mutable slocals: varinfo list;    (* locals, DOES NOT include the
                                         * sformals. Do not make copies of
                                         * these because the body refers to
                                         * them  *)
      mutable smaxid: int;              (* max local id. Starts at 0 *)
      mutable sbody: block;             (* the body *)
      mutable sinline: bool;            (* Whether the function is inline or 
                                         * not *)
			mutable smaxstmtid : int option;  (* max id of a (reachable)
																			   * statement in this function, if
			                                   * we have computed it.
																				 * range = 0 ... (smaxstmtid-1) *)
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

  | GCompTag of compinfo * location     (* Declares a struct/union tag with
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




let argsToList : varinfo list option -> varinfo list = function
    None -> []
  | Some al -> al



(* Different visiting actions. 'a will be instantiated with exp, instr, etc. *)
type 'a visitAction = 
    SkipChildren                        (* Do not visit the children. Return 
                                         * the node as it is *)
  | ChangeTo of 'a                      (* Replace the expression with the 
                                         * given one *)
  | DoChildren                          (* Continue with the children of this 
                                         * node. Rebuild the node on return 
                                         * if any of the children changes 
                                         * (use == test) *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a) (* First consider that the entire 
                                          * exp is replaced by the first 
                                          * paramenter. Then continue with 
                                          * the children. On return rebuild 
                                          * the node if any of the children 
                                          * has changed and then apply the 
                                          * function on the node *)


(* sm/gn: cil visitor interface for traversing Cil trees. *)
(* Use visitCilStmt and/or visitCilFile to use this. *)
(* Some of the nodes are changed in place if the children are changed. Use 
 * one of Change... actions if you want to copy the node *)
class type cilVisitor = object
  method vvrbl : varinfo -> varinfo visitAction  (* variable use *)
  method vvdec : varinfo -> varinfo visitAction  (* variable declaration *)
  method vexpr : exp -> exp visitAction          (* expression *)
  method vlval : lval -> lval visitAction        (* lval (base is 1st field) *)
  method voffs : offset -> offset visitAction    (* lval offset *)
  method vinst : instr -> instr list visitAction (* imperative instruction  *)
  method vstmt : stmt -> stmt visitAction        (* constrol-flow statement. 
                                                  * Changed in place. *)
  method vblock : block -> block visitAction     (* a block. Replaced in 
                                                  * place. *)
  method vfunc : fundec -> fundec visitAction    (* function definition. 
                                                  * Replaced in place. *)
  method vglob : global -> global list visitAction  (* global (vars, 
                                                     * types,etc.)  *)
  method vinit : init -> init visitAction        (* initializers for globals *)
  method vtype : typ -> typ visitAction          (* use of some type *)
  method vattr : attribute -> attribute list visitAction (* An attribute *)
end

(* the default visitor does nothing at each node, but does *)
(* not stop; hence they return true *)
class nopCilVisitor : cilVisitor = object
  method vvrbl (v:varinfo) = DoChildren (* variable *)
  method vvdec (v:varinfo) = DoChildren (* variable 
                                                               * declaration *)
  method vexpr (e:exp) = DoChildren   (* expression *) 
  method vlval (l:lval) = DoChildren  (* lval (base is 1st 
                                                         * field)  *)
  method voffs (o:offset) = DoChildren      (* lval offset *)
  method vinst (i:instr) = DoChildren       (* imperative instruction *)
  method vstmt (s:stmt) = DoChildren        (* constrol-flow statement *)
  method vblock (b: block) = DoChildren
  method vfunc (f:fundec) = DoChildren      (* function definition *)
  method vglob (g:global) = DoChildren      (* global (vars, types, etc.) *)
  method vinit (i:init) = DoChildren        (* global initializers *)
  method vtype (t:typ) = DoChildren         (* use of some type *)
  method vattr (a: attribute) = DoChildren
end

let lu = locUnknown


let get_instrLoc (inst : instr) =
  match inst with
      Set(_, _, loc) -> loc
    | Call(_, _, _, loc) -> loc
    | Asm(_, _, _, _, _, loc) -> loc

let get_globalLoc (g : global) =
  match g with
  | GFun(_,l) -> (l)
  | GType(_,_,l) -> (l)
  | GEnumTag(_,l) -> (l) 
  | GCompTag(_,l) -> (l) 
  | GDecl(_,l) -> (l) 
  | GVar(_,_,l) -> (l)
  | GAsm(_,l) -> (l)
  | GPragma(_,l) -> (l) 
  | GText(_) -> locUnknown

let rec get_stmtLoc (statement : stmtkind) =
  match statement with 
      Instr([]) -> lu
    | Instr(hd::tl) -> get_instrLoc(hd)
    | Return(_, loc) -> loc
    | Goto(_, loc) -> loc
    | Break(loc) -> loc
    | Continue(loc) -> loc
    | If(_, _, _, loc) -> loc
    | Switch (_, _, _, loc) -> loc
    | Loop (_, loc) -> loc
    | Block b -> if b.bstmts = [] then lu 
                 else get_stmtLoc ((List.hd b.bstmts).skind)




    (* A special location that we use to mark that a BinOp was created from 
     * an index *)
let luindex = { line = -1000; file = ""; }


let lastFileName = ref ""
let printLine (forcefile: bool) (l : location) : string =
  let str = ref "" in
  if !printLn && l.line > 0 then begin
    if !printLnComment then str := "//";
    str := !str ^ "#";
    if !msvcMode then str := !str ^ "line";
    str := !str ^ " " ^ string_of_int(l.line);
    if forcefile || l.file <> !lastFileName then begin
      lastFileName := l.file;
      str := !str ^ " \"" ^ l.file ^ "\""
    end
  end;
  currentLoc := l;
  !str
    

(* Represents an integer as for a given kind. Some truncation might be 
 * necessary *)
let truncateInteger64 (k: ikind) (i: int64) = 
  let nrBits, signed = 
    match k with 
    | IChar|ISChar -> 8, true
    | IUChar -> 8, false
    | IShort -> 16, true
    | IUShort -> 16, false
    | IInt | ILong -> 32, true
    | IUInt | IULong -> 32, false
    | ILongLong -> 64, true
    | IULongLong -> 64, false
  in
  if nrBits = 64 then 
    i
  else begin
    let i1 = Int64.shift_left i (64 - nrBits) in
    let i2 = 
      if signed then Int64.shift_right i1 (64 - nrBits) 
      else Int64.shift_right_logical i1 (64 - nrBits)
    in
    i2
  end

(* Construct an integer constant with possible truncation *)
let kinteger64 (k: ikind) (i: int64) : exp = 
  let i' = truncateInteger64 k i in
  if i' <> i then 
    ignore (E.warn "Truncating integer %s to %s\n" 
              (Int64.format "0x%x" i) (Int64.format "0x%x" i'));
  Const (CInt64(i', k,  None))

(* Construct an integer of a given kind. *)
let kinteger (k: ikind) (i: int) = kinteger64 k (Int64.of_int i)

(* Construct an integer. Use only for values that fit on 31 bits *)
let integer (i: int) = Const (CInt64(Int64.of_int i, IInt, None))
            
let zero      = integer 0
let one       = integer 1
let mone      = integer (-1)

let rec isInteger = function
  | Const(CInt64 (n,_,_)) -> Some n
  | Const(CChr c) -> Some (Int64.of_int (Char.code c))
  | CastE(_, e) -> isInteger e
  | _ -> None
        


let rec isZero (e: exp) : bool = isInteger e = Some Int64.zero

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

(* An integer type that fits pointers. We hardwire to unsigned long for now *)
let upointType = TInt(IULong, []) 


let mkStmt (sk: stmtkind) : stmt = 
  { skind = sk;
    labels = [];
    sid = -1; succs = []; preds = [] }

let mkBlock (slst: stmt list) : block = 
  { battrs = []; bstmts = slst; }

let mkEmptyStmt () = mkStmt (Instr [])
let mkStmtOneInstr (i: instr) = mkStmt (Instr [i])

let dummyInstr = (Asm([], ["dummy statement!!"], [], [], [], lu))
let dummyStmt =  mkStmt (Instr [dummyInstr])

let compactStmts (b: stmt list) : stmt list =  
      (* Try to compress statements. Scan the list of statements and remember 
       * the last instrunction statement encountered, along with a Clist of 
       * instructions in it. *)
  let rec compress (lastinstrstmt: stmt) (* Might be dummStmt *)
                   (lastinstrs: instr Clist.clist) 
                   (body: stmt list) =
    let finishLast (tail: stmt list) : stmt list = 
      if lastinstrstmt == dummyStmt then tail
      else begin
        lastinstrstmt.skind <- Instr (Clist.toList lastinstrs);
        lastinstrstmt :: tail
      end
    in
    match body with 
      [] -> finishLast []
    | ({skind=Instr il} as s) :: rest ->
        let ils = Clist.fromList il in
        if lastinstrstmt != dummyStmt && s.labels == [] then
          compress lastinstrstmt (Clist.append lastinstrs ils) rest
        else
          finishLast (compress s ils rest)

    | s :: rest -> 
        let res = s :: compress dummyStmt Clist.empty rest in
        finishLast res
  in
  compress dummyStmt Clist.empty b


(** Construct sorted lists of attributes ***)
let rec addAttribute (Attr(an, _) as a: attribute) (al: attribute list) = 
    let rec insertSorted = function
        [] -> [a]
      | ((Attr(an0, _) as a0) :: rest) as l -> 
          if an < an0 then a :: l
          else if an > an0 then a0 :: insertSorted rest
          else if a = a0 then l 
          else a0 :: insertSorted rest (* Make sure we see all attributes 
                                        * with this name *)
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
      "exception"; "model"; "mode"; "restrict"; 
      "aconst"; "__asm__" (* Gcc uses this to specifiy the name to be used in 
                           * assembly for a global  *)];

  (* Now come the MSVC declspec attributes *)
  List.iter (fun a -> H.add table a (AttrName true))
    [ "thread"; "naked"; "dllimport"; "dllexport"; "noreturn";
      "selectany"; "allocate"; "nothrow"; "novtable"; "property";
      "uuid" ];

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

 
let missingFieldName = "___missing_field_name"

(** Creates a a (potentially recursive) composite type. Make sure you add a 
  * GTag for it to the file! **)
let mkCompInfo
               (isstruct: bool) 
               (n: string)   (* empty for anonymous structures *)
               (* fspec is a function that when given a forward 
                * representation of the structure type constructs the type of 
                * the fields. The function can ignore this argument if not 
                * constructing a recursive type.  *)
               (mkfspec: typ -> (string * typ * 
                                 int option * attribute list) list) 
               (a: attribute list) : compinfo =
   (* make an new name for anonymous structs *)
   if n = "" then 
     E.s (E.bug "mkCompInfo: missing structure name\n");
(*
     newTypeName (if isstruct then "struct" else "union") else n in *)
   (* Make a new self cell and a forward reference *)
   let comp = 
     { cstruct = isstruct; cname = ""; ckey = 0; cfields = [];
       cattr = a; creferenced = false; } in
   compSetName comp n;  (* fix the name and the key *)
   let self = ref voidType in
   let tforward = TComp (comp, []) in
   let flds = 
       List.map (fun (fn, ft, fb, fa) -> 
          { fcomp = comp;
            ftype = ft;
            fname = fn;
            fbitfield = fb;
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
  [ mkStmt (Loop (mkBlock (mkStmt (If(guard, 
                                      mkBlock [ mkEmptyStmt () ], 
                                      mkBlock [ mkStmt (Break lu)], lu)) ::
                           body), lu)) ]



let mkFor (start: stmt list) (guard: exp) (next: stmt list) 
          (body: stmt list) : stmt list = 
  (start @ 
     (mkWhile guard (body @ next)))

    
let mkForIncr (iter : varinfo) (first: exp) (past: exp) (incr: exp) 
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
  text loc.file ++ chr ':' ++ num loc.line

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
    E.showContext ();
    raise E.Error
  in
  Pretty.gprintf f fmt

let errorLoc (loc: location) (fmt : ('a,unit,doc) format) : 'a = 
  let f d = 
    E.hadErrors := true; 
    ignore (eprintf "@!%a: Error: %a@!" 
              d_loc loc insert d);
    E.showContext ();
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

let warnContext (fmt : ('a,unit,doc) format) : 'a = 
  let f d =
    ignore (eprintf "@!%t: Warning: %a@!" 
              d_thisloc insert d);
    E.showContext ();
    nil
  in
  Pretty.gprintf f fmt

let warnLoc (loc: location) (fmt : ('a,unit,doc) format) : 'a = 
  let f d =
    ignore (eprintf "@!%a: Warning: %a@!" 
              d_loc loc insert d);
    E.showContext ();
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

(* sm: need this value below *)
let mostNeg32BitInt : int64 = (Int64.of_string "-0x80000000")

(* constant *)
let d_const () c = 
  let suffix ik = 
    match ik with
      IUInt -> "U"
    | ILong -> "L"
    | IULong -> "UL"
    | ILongLong -> if !msvcMode then "L" else "LL"
    | IULongLong -> if !msvcMode then "UL" else "ULL"
    | _ -> ""
  in
  match c with
    CInt64(_, _, Some s) -> text s (* Always print the text if there is one *)
  | CInt64(i, ik, None) -> 
      (* Watch out here for negative integers that we should be printing as 
       * large positive ones *)
      if i < Int64.zero 
          && (match ik with 
            IUInt | IULong | IULongLong | IUChar | IUShort -> true | _ -> false) then
        let high = Int64.shift_right i 32 in
        if ik <> IULongLong && ik <> ILongLong && high = Int64.of_int (-1) then
          (* Print only the low order 32 bits *)
          text ("0x" ^ 
                Int64.format "%x" (Int64.logand i (Int64.shift_right_logical high 32))
                ^ suffix ik)
        else
          text ("0x" ^ Int64.format "%x" i ^ suffix ik)
      else (
        if (i = mostNeg32BitInt) then
          (* sm: quirk here: if you print -2147483648 then this is two tokens *)
          (* in C, and the second one is too large to represent in a signed *)
          (* int.. so we do what's done in limits.h, and print (-2147483467-1); *)
          (* in gcc this avoids a warning, but it might avoid a real problem *)
          (* on another compiler or a 64-bit architecture *)
          text "(-0x7FFFFFFF-1)"
        else
          text (Int64.to_string i ^ suffix ik)
      )

  | CStr(s) -> text ("\"" ^ escape_string s ^ "\"")
  | CChr(c) -> text ("'" ^ escape_char c ^ "'")
  | CReal(_, _, Some s) -> text s
  | CReal(f, _, None) -> text (string_of_float f)

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
(*  | Question _ -> 80 *)
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
  | AlignOf _ | AlignOfE _ -> 20

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


let rec typeAttrs = function
    TVoid a -> a
  | TInt (_, a) -> a
  | TFloat (_, a) -> a
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
  | TSFun of typsig * typsig list * bool * attribute list
  | TSEnum of string * attribute list
  | TSBase of typ

(* Compute a type signature *)
let rec typeSigWithAttrs doattr t = 
  let typeSig = typeSigWithAttrs doattr in
  match t with 
  | TInt (ik, al) -> TSBase (TInt (ik, doattr al))
  | TFloat (fk, al) -> TSBase (TFloat (fk, doattr al))
  | TVoid al -> TSBase (TVoid (doattr al))
  | TEnum (enum, a) -> TSEnum (enum.ename, doattr a)
  | TPtr (t, a) -> TSPtr (typeSig t, doattr a)
  | TArray (t,l,a) -> TSArray(typeSig t, l, doattr a)
  | TComp (comp, a) -> 
      TSComp (comp.cstruct, comp.cname, doattr (addAttributes comp.cattr a))
  | TFun(rt,args,isva,a) -> TSFun(typeSig rt, 
                                  List.map (fun vi -> (typeSig vi.vtype)) 
                                    (argsToList args),
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


let typeSig t = typeSigWithAttrs (fun al -> al) t

(* Remove the attribute from the top-level of the type signature *)
let setTypeSigAttrs (a: attribute list) = function
    TSBase t -> TSBase (setTypeAttrs t a)
  | TSPtr (ts, _) -> TSPtr (ts, a)
  | TSArray (ts, l, _) -> TSArray(ts, l, a)
  | TSComp (iss, n, _) -> TSComp (iss, n, a)
  | TSEnum (n, _) -> TSEnum (n, a)
  | TSFun (ts, tsargs, isva, _) -> TSFun (ts, tsargs, isva, a)


let typeSigAttrs = function
    TSBase t -> typeAttrs t
  | TSPtr (ts, a) -> a
  | TSArray (ts, l, a) -> a
  | TSComp (iss, n, a) -> a
  | TSEnum (n, a) -> a
  | TSFun (ts, tsargs, isva, a) -> a
  

(**** Compute the type of an expression ****)
let rec typeOf (e: exp) : typ = 
  match e with
  | Const(CInt64 (_, ik, _)) -> TInt(ik, [])
  | Const(CChr _) -> charType
  | Const(CStr _) -> charPtrType 
  | Const(CReal (_, fk, _)) -> TFloat(fk, [])
  | Lval(lv) -> typeOfLval lv
  | SizeOf _ | SizeOfE _ -> uintType
  | AlignOf _ | AlignOfE _ -> uintType
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
(*  | Question (_, e2, _) -> typeOf e2 *)
  | CastE (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLval lv, [])
  | StartOf (lv) -> begin
      match unrollType (typeOfLval lv) with
        TArray (t,_, _) -> TPtr(t, [])
     | _ -> E.s (E.bug "typeOf: StartOf on a non-array")
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
      | t -> E.s (E.bug "typeOffset: Index on a non-array")
  end 
  | Field (fi, o) -> typeOffset fi.ftype o


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
      text "/*(" 
        ++ d_attrlist_pre () a
        ++ text ")*/"
    else begin
      text "(" 
        ++ d_attrlist_pre () a
        ++ docName ()
        ++ text ")"
    end
  in
  match this with 
    TVoid a ->
      text "void"
       ++ d_attrlist () a 
        ++ text " " 
        ++ docName ()

  | TInt (ikind,a) -> 
      d_ikind () ikind 
        ++ d_attrlist () a 
        ++ text " "
        ++ docName ()

  | TFloat(fkind, a) -> 
    d_fkind () fkind 
        ++ d_attrlist () a 
        ++ text " " 
        ++ docName ()

  | TComp (comp, a) -> (* A reference to a struct *)
      let su = if comp.cstruct then "struct" else "union" in
    text (su ^ " " ^ comp.cname ^ " ") 
        ++ d_attrlist_pre () a 
        ++ docName()

  | TEnum (enum, a) -> 
     text ("enum " ^ enum.ename ^ " ")
        ++ d_attrlist_pre () a 
        ++ docName ()

  | TPtr (bt, a)  -> 
      d_decl 
        (fun _ -> 
          text "* " ++ d_attrlist_pre () a ++ docName ())
        DNStuff
        () 
        bt

  | TArray (elemt, lo, a) -> 
      d_decl 
        (fun _ ->
            parenthname () a
            ++ text "[" 
            ++ (match lo with None -> nil | Some e -> d_exp () e)
            ++ text "]")
        DNStuff
        ()
        elemt

  | TFun (restyp, args, isvararg, a) -> 
      d_decl 
        (fun _ -> 
            parenthname () a
            ++ text "("
            ++ (align 
                  ++ (if args = Some [] && isvararg then 
                         text "..."
                      else
                         (if args = None then nil 
                          else if args = Some [] then text "void"
                          else (docList (chr ',' ++ break) (d_videcl ()) () 
                                 (argsToList args)))
                      ++ (if isvararg then break ++ text ", ..." else nil))
                  ++ unalign)
            ++ text ")")
        DNStuff
        ()
        restyp

  | TNamed (n, _, a) ->
        text n ++ d_attrlist () a ++ text " " ++ docName ()


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
      ta
  in
  let fixattrs = function
      TVoid a -> TVoid (fixthem a)
    | TInt (ik, a) -> TInt (ik, fixthem a)
    | TFloat (fk, a) -> TFloat (fk, fixthem a)

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
    text "(" ++ d_exp () e ++ text ")"
  else
    d_exp () e

and d_exp () e = 
  let level = getParenthLevel e in
  match e with
    Const(c) -> d_const () c
  | Lval(l) -> d_lval () l
  | UnOp(u,e1,_) -> 
      let d_unop () u =
        match u with
          Neg -> text "-"
        | BNot -> text "~"
        | LNot -> text "!"
      in
      (d_unop () u) ++ chr ' ' ++ (d_expprec level () e1)

  | BinOp(b,e1,e2,_) -> 
(*
      dprintf "@[%a %a@?%a@]" 
        (d_expprec level) e1 d_binop b (d_expprec level) e2
*)
      align 
        ++ (d_expprec level () e1)
        ++ chr ' ' 
        ++ (d_binop () b)
        ++ break 
        ++ (d_expprec level () e2)
        ++ unalign

(*  | Question (e1, e2, e3) ->
      (d_expprec level () e1)
        ++ text " ? "
        ++ (d_expprec level () e2)
        ++ text " : " 
        ++ (d_expprec level () e3)
*)
  | CastE(t,e) -> 
      text "(" 
        ++ d_type () t
        ++ text ")"
        ++ d_expprec level () e

  | SizeOf (t) -> 
      text "sizeof(" ++ d_type () t ++ chr ')'
  | SizeOfE (e) -> 
      text "sizeof(" ++ d_exp () e ++ chr ')'
  | AlignOf (t) -> 
      text "__alignof__(" ++ d_type () t ++ chr ')'
  | AlignOfE (e) -> 
      text "__alignof__(" ++ d_exp () e ++ chr ')'
  | AddrOf(lv) -> 
      text "& " ++ (d_lvalprec addrOfLevel () lv)

  | StartOf(lv) -> d_lval () lv

and d_init () = function
    SingleInit e -> d_exp () e
  | CompoundInit (t, initl) -> 
      (* We do not print the type of the Compound *)
(*
      let dinit e = d_init () e in
      dprintf "{@[%a@]}"
        (docList (chr ',' ++ break) dinit) initl
*)
      chr '{' ++ (align 
                    ++ ((docList (chr ',' ++ break) (d_init ())) () initl) 
                    ++ unalign)
        ++ chr '}'

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
    text (an' ^ "(") 
      ++(docList (chr ',') (d_attrarg ()) () args)
      ++ text ")"

and d_attrarg () = function
    AId s -> text s
  | AInt n -> num n
  | AStr s -> text ("\"" ^ escape_string s ^ "\"")
  | AVar vi -> text vi.vname
  | ACons(s,al) ->
      text (s ^ "(")
        ++ (docList (chr ',') (d_attrarg ()) () al)
        ++ text ")"
  | ASizeOfE a -> text "sizeof(" ++ d_attrarg () a ++ text ")"
  | ASizeOf t -> text "sizeof(" ++ d_type () t ++ text ")"
  | AUnOp(u,a1) -> 
      let d_unop () u =
        match u with
          Neg -> text "-"
        | BNot -> text "~"
        | LNot -> text "!"
      in
      (d_unop () u) ++ text " (" ++ (d_attrarg () a1) ++ text ")"

  | ABinOp(b,a1,a2) -> 
      align 
        ++ text "(" 
        ++ (d_attrarg () a1)
        ++ text ") "
        ++ (d_binop () b)
        ++ break 
        ++ text " (" ++ (d_attrarg () a2) ++ text ") "
        ++ unalign
      
          
and d_attrlistgen (block: bool) (pre: bool) () al = 
                    (* Whether it comes before or after stuff *)
  (* Take out the special attributes *)
  let rec loop remaining = function
      [] -> begin
        match remaining with
          [] -> nil
        | Attr(str,args) :: _->
            if (str = "dummydefn") then (
              text "/*dummydefn*/" (* don't print this because gcc complains *)
            )
            else (
              (if block then text " /* __block" else text "__") ++ 
                text "attribute__(" 
                ++ (if block then nil else text "(")
                ++ (docList (chr ',' ++ break) 
                      (fun a -> d_attr () a) () remaining)
                ++ text ")"
                ++ (if block then text "*/" else text ")")
            )
      end
    | x :: rest -> begin
        match !d_attrcustom x with
          Some xd -> xd ++ text "  " ++ loop remaining rest
        | None -> loop (x :: remaining) rest
    end
  in
  let res = loop [] al in
  if res = nil then
    res
  else
    if pre then res ++ text " " else text " " ++ res
    
and d_attrlist_pos pre () al = d_attrlistgen false pre () al
and d_attrlist () al = d_attrlist_pos false () al
and d_attrlist_pre () al = d_attrlist_pos true () al

(* lvalue *)
and d_lvalprec contextprec () lv = 
  if getParenthLevel (Lval(lv)) >= contextprec then
    text "(" ++ d_lval () lv ++ text ")"
  else
    d_lval () lv
  
and d_lval () lv = 
  let rec d_offset dobase = function
    | NoOffset -> dobase ()
    | Field (fi, o) -> 
        d_offset (fun _ -> dobase () ++ text "." ++ text fi.fname) o
(*    | Index (Const(CInt64(z,_,_)), NoOffset) when z = Int64.zero -> 
        text "(*" ++ dobase () ++ text ")"
*)
    | Index (e, o) ->
        d_offset (fun _ -> dobase () ++ text "[" ++ d_exp () e ++ text "]") o
  in
  match lv with
    Var vi, o -> d_offset (fun _ -> text vi.vname) o
  | Mem e, Field(fi, o) ->
      d_offset (fun _ ->
        (d_expprec arrowLevel () e) ++ text ("->" ^ fi.fname)) o
(*  | Mem e, NoOffset -> dprintf "(*%a)" (d_expprec derefStarLevel) e *)
  | Mem e, o ->
      d_offset (fun _ -> 
        text "(*" ++ d_expprec derefStarLevel () e ++ text ")") o

and d_instr () i =
  match i with
  | Set(lv,e,l) -> begin
      (* Be nice to some special cases *)
      match e with
        BinOp((PlusA|PlusPI|IndexPI),Lval(lv'),Const(CInt64(one,_,_)),_)
          when lv == lv' && one = Int64.one ->
          d_line l
           ++ d_lval () lv
           ++ text " ++;"

      | BinOp((MinusA|MinusPI),Lval(lv'),
              Const(CInt64(one,_,_)), _) when lv == lv' && one = Int64.one ->
         d_line l
          ++ d_lval () lv
          ++ text " --;"

      | BinOp((PlusA|PlusPI|IndexPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
               Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
              Lval(lv'),e,_) when lv == lv' ->
          d_line l
            ++ d_lval () lv
            ++ text " " ++ d_binop () bop
            ++ text "= "
            ++ d_exp () e
            ++ text ";"

      | _ ->
          d_line l
            ++ d_lval () lv
            ++ text " = "
            ++ d_exp () e
            ++ text ";"

  end
  | Call(dest,e,args,l) ->
       d_line l
         ++ (match dest with
               None -> nil
             | Some lv -> 
                 d_lval () lv ++ text " = " ++
                   (* Maybe we need to print a cast *)
                   (let destt = typeOfLval lv in
                   match unrollType (typeOf e) with
                     TFun (rt, _, _, _) when typeSig rt <> typeSig destt ->
                       text "(" ++ d_type () destt ++ text ")"
                   | _ -> nil))
        (* Now the function name *)
        ++ (match e with Lval(Var _, _) -> d_exp () e
                       | _ -> text "(" ++ d_exp () e ++ text ")")
        ++ text "(" ++ (align
                          (* Now the arguments *)
                          ++ (docList (chr ',' ++ break) (d_exp ()) () args)
                          ++ unalign)
        ++ text ");"

  | Asm(attrs, tmpls, outs, ins, clobs, l) ->
      if !msvcMode then
        d_line l
          ++ text "__asm {"
          ++ (align
                ++ (docList line text () tmpls)
                ++ unalign)
          ++ text "};"
      else
        d_line l
          ++ text ("__asm__ ") 
          ++ d_attrlist () attrs 
          ++ text " ("
          ++ (align
                ++ (docList line
                      (fun x -> text ("\"" ^ escape_string x ^ "\""))
                      () tmpls)
                ++
                (if outs = [] && ins = [] && clobs = [] then
                  nil
                else
                  (text ": "
                     ++ (docList (chr ',' ++ break)
                           (fun (c, lv) ->
                             text ("\"" ^ escape_string c ^ "\" (")
                               ++ d_lval () lv
                               ++ text ")") () outs)))
                ++
                (if ins = [] && clobs = [] then
                  nil
                else
                  (text ": "
                     ++ (docList (chr ',' ++ break)
                           (fun (c, e) ->
                             text ("\"" ^ escape_string c ^ "\" (")
                               ++ d_exp () e
                               ++ text ")") () ins)))
                ++
                (if clobs = [] then nil
                else
                  (text ": "
                     ++ (docList (chr ',' ++ break)
                           (fun c -> text ("\"" ^ escape_string c ^ "\""))
                           ()
                           clobs)))
                ++ unalign)
          ++ text ");"

and d_stmt_next (next: stmt) () (s: stmt) =
  (* print the labels *)
  ((docList line (fun l -> d_label () l)) () s.labels)
    (* print the statement itself. If the labels are non-empty and the
    * statement is empty, print a semicolon  *)
    ++ 
    (if s.skind = Instr [] && s.labels <> [] then
      text ";"
    else
      (if s.labels <> [] then line else nil) 
        ++ d_stmtkind next () s.skind)
    
and d_stmt () (s: stmt) = (* A version that is easier to call *)
  d_stmt_next invalidStmt () s

and d_label () = function
    Label (s, _, true) -> text (s ^ ": ")
  | Label (s, _, false) -> text (s ^ ": /* CIL Label */ ")
  | Case (e, _) -> text "case " ++ d_exp () e ++ text ": "
  | Default _ -> text "default: "

and d_block () (blk: block) =
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
(* Let the host of the block decide on the alignment. The d_block will pop 
 * the alignment as well *)
  text "{" 
    ++ (if blk.battrs <> [] then 
           d_attrlistgen true true () blk.battrs
        else nil)
    ++ line
    ++ (dofirst () blk.bstmts)
    ++ unalign ++ line ++ text "}"
(*
  dprintf "@[{ @[@!%a@]@!}@]" dofirst blk
*)

(* Make sure that you only call d_line on an empty line *)
and d_line l = 
  let ls = printLine false l in
  if ls <> "" then leftflush ++ text ls ++ line else nil
   
and d_stmtkind (next: stmt) () = function
    Return(None, l) ->
      d_line l
        ++ text "return;"
  | Return(Some e, l) ->
      d_line l
        ++ text "return ("
        ++ d_exp () e
        ++ text ");"

  | Goto (sref, l) -> d_goto !sref
  | Break l ->
      d_line l
        ++ text "break;"
  | Continue l -> 
      d_line l
        ++ text "continue;"

  | Instr il ->
      align
        ++ (docList line (fun i -> d_instr () i) () il)
        ++ unalign

  | If(be,t,{bstmts=[];battrs=[]},l) ->
      d_line l
        ++ text "if"
        ++ (align
              ++ text " ("
              ++ d_exp () be
              ++ text ") "
              ++ d_block () t)

  | If(be,t,{bstmts=[{skind=Goto(gref,_);labels=[]} as s];
             battrs=[]},l)
      when !gref == next ->
        d_line l
          ++ text "if"
          ++ (align
                ++ text " ("
                ++ d_exp () be
                ++ text ") "
                ++ d_block () t)

  | If(be,{bstmts=[];battrs=[]},e,l) ->
      d_line l
        ++ text "if"
        ++ (align
              ++ text " ("
              ++ d_exp () (UnOp(LNot,be,intType))
              ++ text ") "
              ++ d_block () e)

  | If(be,{bstmts=[{skind=Goto(gref,_);labels=[]} as s];
           battrs=[]},e,l)
      when !gref == next ->
      d_line l
        ++ text "if"
        ++ (align
              ++ text " ("
              ++ d_exp () (UnOp(LNot,be,intType))
              ++ text ") "
              ++ d_block () e)

  | If(be,t,e,l) ->
      d_line l
        ++ (align
              ++ text "if"
              ++ (align
                    ++ text " ("
                    ++ d_exp () be
                    ++ text ") "
                    ++ d_block () t)
              ++ text " el"
              ++ (align
                    ++ text "se "
                    ++ d_block () e)
              ++ unalign)

  | Switch(e,b,_,l) ->
      d_line l
        ++ (align
              ++ text "switch ("
              ++ d_exp () e
              ++ text ") "
              ++ d_block () b)
  | Loop(b, l) -> begin
      (* Maybe the first thing is a conditional. Turn it into a WHILE *)
      try
        let term, bodystmts =
          let rec skipEmpty = function
              [] -> []
            | {skind=Instr [];labels=[]} :: rest -> skipEmpty rest
            | x -> x
          in
          match skipEmpty b.bstmts with
            {skind=If(e,tb,fb,_)} :: rest -> begin
              match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
                [], {skind=Break _} :: _  -> e, rest
              | {skind=Break _} :: _, [] -> UnOp(LNot, e, intType), rest
              | _ -> raise Not_found
            end
          | _ -> raise Not_found
        in
        d_line l
          ++ text "wh"
          ++ (align
                ++ text "ile ("
                ++ d_exp () term
                ++ text ") "
                ++ d_block () {bstmts=bodystmts; battrs=b.battrs})

    with Not_found ->
      d_line l
        ++ text "wh"
        ++ (align
              ++ text "ile (1) "
              ++ d_block () b)
  end
  | Block b -> align ++ d_block () b
      

and d_goto (s: stmt) = 
  (* Grab one of the labels *)
  let rec pickLabel = function
      [] -> None
    | Label (l, _, _) :: _ -> Some l
    | _ :: rest -> pickLabel rest
  in
  match pickLabel s.labels with
    Some l -> text ("goto " ^ l ^ ";")
  | None -> 
      ignore (E.warn "Cannot find label for target of goto\n");
      text "goto __invalid_label;"

and d_fun_decl () f = begin
  text (if f.sinline then "__inline " else "")
    ++ d_videcl () f.svar
    ++ line
    ++ text "{ "
    ++ (align
          (* locals. *)
          ++ (docList line (fun vi -> d_videcl () vi ++ text ";") () f.slocals)
          ++ line ++ line
          (* the body *)
          ++ d_block () f.sbody)
    ++ line
    ++ text "}"
end

and d_videcl () vi = 
  let stom, rest = separateStorageModifiers vi.vattr in
    (* First the storage modifiers *)
    (d_attrlist_pre () stom)
    ++ d_storage () vi.vstorage
    ++ (d_decl (fun _ -> text vi.vname) DNString () vi.vtype)
    ++ text " "
    ++ d_attrlist () rest

and d_fielddecl () fi = 
  (d_decl 
     (fun _ -> 
       text (if fi.fname = missingFieldName then "" else fi.fname))
     DNString () fi.ftype)
    ++ text " "
    ++ (match fi.fbitfield with None -> nil 
                             | Some i -> text ": " ++ num i ++ text " ")
    ++ d_attrlist () fi.fattr
    ++ text ";"

   (* Some plain pretty-printers. Unlike the above these expose all the 
    * details of the internal representation *)
let rec d_plainexp () = function
    Const(c) -> 
      text "Const(" ++ d_const () c ++ text ")"
  | Lval(lv) -> 
      text "Lval(" 
        ++ (align
              ++ d_plainlval () lv
              ++ unalign)
        ++ text ")"
        
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
    TVoid a -> dprintf "TVoid(@[%a@])" d_attrlist a
  | TInt(ikind, a) -> dprintf "TInt(@[%a,@?%a@])" 
        d_ikind ikind d_attrlist a
  | TFloat(fkind, a) -> 
      dprintf "TFloat(@[%a,@?%a@])" d_fkind fkind d_attrlist a
  | TNamed (n, t, a) ->
      dprintf "TNamed(@[%s,@?%a,@?%a@])" n scanType t d_attrlist a
  | TPtr(t, a) -> dprintf "TPtr(@[%a,@?%a@])" scanType t d_attrlist a
  | TArray(t,l,a) -> 
      let dl = match l with 
        None -> text "None" | Some l -> dprintf "Some(@[%a@])" d_plainexp l in
      dprintf "TArray(@[%a,@?%a,@?%a@])" 
        scanType t insert dl d_attrlist a
  | TEnum(enum,a) -> dprintf "Enum(%s,@[%a@])" enum.ename d_attrlist a
  | TFun(tr,args,isva,a) -> 
      dprintf "TFun(@[%a,@?%a%s,@?%a@])"
        scanType tr 
        insert 
        (if args = None then text "None"
         else (docList (chr ',' ++ break) 
                 (fun a -> dprintf "%s: %a" a.vname scanType a.vtype)) 
                 () 
                 (argsToList args))
        (if isva then "..." else "") d_attrlist a
  | TComp (comp, a) -> 
      if H.mem donecomps comp.ckey then 
        dprintf "TCompLoop(%s %s, _, %a)" 
          (if comp.cstruct then "struct" else "union") comp.cname 
          d_attrlist comp.cattr
      else begin
        H.add donecomps comp.ckey (); (* Add it before we do the fields *)
        dprintf "TComp(@[%s %s,@?%a,@?%a,@?%a@])" 
          (if comp.cstruct then "struct" else "union") comp.cname
          (docList (chr ',' ++ break) 
             (fun f -> dprintf "%s : %a" f.fname scanType f.ftype)) 
          comp.cfields
          d_attrlist comp.cattr
          d_attrlist a
      end
  in
  scanType () t

let rec d_typsig () = function
    TSArray (ts, eo, al) -> 
      dprintf "TSArray(@[%a,@?%a,@?%a@])" 
        d_typsig ts 
        insert (match eo with None -> text "None" | Some e -> d_exp () e)
        d_attrlist_pre al
  | TSPtr (ts, al) -> 
      dprintf "TSPtr(@[%a,@?%a@])"
        d_typsig ts d_attrlist_pre al
  | TSComp (iss, name, al) -> 
      dprintf "TSComp(@[%s %s,@?%a@])"
        (if iss then "struct" else "union") name
        d_attrlist_pre al
  | TSFun (rt, args, isva, al) -> 
      dprintf "TSFun(@[%a,@?%a,%b,@?%a@])"
        d_typsig rt
        (docList (chr ',' ++ break) (d_typsig ())) args isva
        d_attrlist_pre al
  | TSEnum (n, al) -> 
      dprintf "TSEnum(@[%s,@?%a@])"
        n d_attrlist_pre al
  | TSBase t -> dprintf "TSBase(%a)" d_type t

let _ = 
  let d_attrcustombase = function
    | Attr("const", []) -> Some (text "const")
    | Attr("aconst", []) when not !msvcMode -> 
        Some (text "__attribute__((__const__))")
    | Attr("volatile", []) -> Some (text "volatile")
    | Attr("restrict", []) -> Some (text "__restrict")
    | Attr("missingproto", []) -> Some (text "/* missing proto */")
    | Attr("cdecl", []) when !msvcMode -> Some (text "__cdecl")
    | Attr("stdcall", []) when !msvcMode -> Some (text "__stdcall")
    | Attr("declspec", args) when !msvcMode -> 
        Some (text "__declspec(" 
                ++ docList (chr ',') (d_attrarg ()) () args
                ++ text ")")
    | Attr("asm", args) -> 
        Some (text "__asm__(" 
                ++ docList (chr ',') (d_attrarg ()) () args
                ++ text ")")
    (* we suppress printing mode(__si__) because it triggers an *)
    (* internal compiler error in all current gcc versions *)
    (* sm: I've now encountered a problem with mode(__hi__)... *)
    (* I don't know what's going on, but let's try disabling all "mode"..*)
    | Attr("mode", [AId tag]) -> Some ((text "/* mode(") ++ (text tag) ++ (text ") */"))
    | _ -> None
  in
  setCustomPrintAttribute d_attrcustombase




   (* Make a varinfo for use in argument part of a function type *)
let makeVarinfo name typ =
  (* Strip const from type *)
  { vname = name;
    vid   = 0;
    vglob = false;
    vtype = typeRemoveAttributes [Attr("const",[])] typ;
    vdecl = lu;
    vattr = [];
    vstorage = NoStorage;
    vaddrof = false;
    vreferenced = false;    (* sm *)
  } 

let makeLocal fdec name typ = (* a helper function *)
  fdec.smaxid <- 1 + fdec.smaxid;
  let vi = makeVarinfo name typ in
  vi.vid <- fdec.smaxid;
  vi
  
   (* Make a local variable and add it to a function *)
let makeLocalVar fdec ?(insert = true) name typ =
  let vi = makeLocal fdec name typ in
  if insert then fdec.slocals <- fdec.slocals @ [vi];
  vi


let makeTempVar fdec ?(name = "tmp") typ : varinfo =
  let name = name ^ (string_of_int (1 + fdec.smaxid)) in
  makeLocalVar fdec name typ

 
  (* Set the formals and make sure the function type shares them *)
let setFormals (f: fundec) (forms: varinfo list) = 
  f.sformals <- forms;
  match unrollType f.svar.vtype with
    TFun(rt, _, isva, fa) -> 
      f.svar.vtype <- TFun(rt, Some forms, isva, fa)
  | _ -> E.s (E.bug "Set formals. %s does not have function type\n"
                f.svar.vname)
    
   (* Set the types of arguments and results as given by the function type 
    * passed as the second argument *)
let setFunctionType (f: fundec) (t: typ) = 
  match unrollType t with
    TFun (rt, args, va, a) -> 
      (* Change the function type. For now use the sformals instead of args *)
      f.svar.vtype <- TFun (rt, Some f.sformals, va, a);
      (* Change the sformals and we know that indirectly we'll change the 
       * function type *)
      if List.length f.sformals <> List.length (argsToList args) then 
        E.s (E.bug "setFunctionType: wrong number of arguments");
      List.iter2 
        (fun a f -> 
          f.vtype <- a.vtype; f.vattr <- a.vattr; 
          if a.vname <> "" then
            f.vname <- a.vname) (argsToList args) f.sformals

  | _ -> E.s (E.bug "setFunctionType: not a function type")
      
          
  
  (* Make a formal variable for a function. Insert it in both the sformals 
   * and the type of the function. You can optionally specify where to insert 
   * this one. If where = "^" then it is inserted first. If where = "$" then 
   * it is inserted last. Otherwise where must be the name of a formal after 
   * which to insert this. By default it is inserted at the end. *)
let makeFormalVar fdec ?(where = "$") name typ : varinfo = 
  (* Search for the insertion place *)
  let thenewone = ref fdec.svar in (* Just a placeholder *)
  let makeit () : varinfo = 
    let vi = makeLocal fdec name typ in
    thenewone := vi;
    vi
  in
  let rec loopFormals = function
      [] -> 
        if where = "$" then [makeit ()]
        else E.s (E.error "makeFormalVar: cannot find insert-after formal %s"
                    where)
    | f :: rest when f.vname = where -> f :: makeit () :: rest
    | f :: rest -> f :: loopFormals rest
  in
  let newformals = 
    if where = "^" then makeit () :: fdec.sformals else 
    loopFormals fdec.sformals in
  setFormals fdec newformals;
  !thenewone

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
  { svar  = makeGlobalVar name (TFun(voidType, Some [], false,[]));
    smaxid = 0;
    slocals = [];
    sformals = [];
    sbody = mkBlock [];
    sinline = false;
		smaxstmtid = None;
  } 



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


(*** Define the visiting engine ****)
(* visit all the nodes in a Cil expression *)
let doVisit (vis: cilVisitor)
            (startvisit: 'a -> 'a visitAction) 
            (children: cilVisitor -> 'a -> 'a) 
            (node: 'a) : 'a = 
  let action = startvisit node in
  match action with
    SkipChildren -> node
  | ChangeTo node' -> node'
  | _ -> 
      let nodepre = match action with
        ChangeDoChildrenPost (node', _) -> node'
      | _ -> node
      in
      let nodepost = children vis nodepre in
      match action with
        ChangeDoChildrenPost (_, f) -> f nodepost
      | _ -> nodepost

let rec mapNoCopy (f: 'a -> 'a) = function
    [] -> []
  | (i :: resti) as li -> 
      let i' = f i in
      let resti' = mapNoCopy f resti in
      if i' != i || resti' != resti then i' :: resti' else li 

let rec mapNoCopyList (f: 'a -> 'a list) = function
    [] -> []
  | (i :: resti) as li -> 
      let il' = f i in
      let resti' = mapNoCopyList f resti in
      match il' with
        [i'] when i' == i && resti' == resti -> li
      | _ -> il' @ resti'

(* A visitor for lists *)
let doVisitList (vis: cilVisitor)
                (startvisit: 'a -> 'a list visitAction)
                (children: cilVisitor -> 'a -> 'a)
                (node: 'a) : 'a list = 
  let action = startvisit node in
  match action with
    SkipChildren -> [node]
  | ChangeTo nodes' -> nodes'
  | _ -> 
      let nodespre = match action with
        ChangeDoChildrenPost (nodespre, _) -> nodespre
      | _ -> [node]
      in
      let nodespost = mapNoCopy (children vis) nodespre in
      match action with
        ChangeDoChildrenPost (_, f) -> f nodespost
      | _ -> nodespost
  
let debugVisit = false

let rec visitCilExpr (vis: cilVisitor) (e: exp) : exp = 
  doVisit vis vis#vexpr childrenExp e
and childrenExp (vis: cilVisitor) (e: exp) : exp = 
  let vExp e = visitCilExpr vis e in
  let vTyp t = visitCilType vis t in
  let vLval lv = visitCilLval vis lv in
  match e with
    Const _ -> e
  | SizeOf t -> 
      let t'= vTyp t in 
      if t' != t then SizeOf t' else e
  | SizeOfE e1 -> 
      let e1' = vExp e1 in
      if e1' != e1 then SizeOfE e1' else e
  | AlignOf t -> 
      let t' = vTyp t in
      if t' != t then AlignOf t' else e
  | AlignOfE e1 -> 
      let e1' = vExp e in
      if e1' != e1 then AlignOfE e1' else e
  | Lval lv -> 
      let lv' = vLval lv in
      if lv' != lv then Lval lv' else e
  | UnOp (uo, e1, t) -> 
      let e1' = vExp e1 in let t' = vTyp t in
      if e1' != e1 || t' != t then UnOp(uo, e1', t') else e
  | BinOp (bo, e1, e2, t) -> 
      let e1' = vExp e1 in let e2' = vExp e2 in let t' = vTyp t in
      if e1' != e1 || e2' != e2 || t' != t then BinOp(bo, e1',e2',t') else e
(*  | Question (e1, e2, e3) -> 
      let e1' = vExp e1 in let e2' = vExp e2 in let e3' = vExp e3 in
      if e1' != e1 || e2' != e2 || e3' != e3 then Question(e1',e2',e3') else e
*)
  | CastE (t, e1) ->           
      let t' = vTyp t in let e1' = vExp e1 in
      if t' != t || e1' != e1 then CastE(t', e1') else e
  | AddrOf lv -> 
      let lv' = vLval lv in
      if lv' != lv then AddrOf lv' else e
  | StartOf lv -> 
      let lv' = vLval lv in
      if lv' != lv then StartOf lv' else e


and visitCilInit (vis: cilVisitor) (i: init) : init = 
  doVisit vis vis#vinit childrenInit i
and childrenInit (vis: cilVisitor) (i: init) : init = 
  let fExp e = visitCilExpr vis e in
  let fInit i = visitCilInit vis i in
  let fTyp t = visitCilType vis t in
  match i with
  | SingleInit e -> 
      let e' = fExp e in
      if e' != e then SingleInit e' else i
  | CompoundInit (t, initl) ->
      let t' = fTyp t in
      let initl' = mapNoCopy fInit initl in
      if t' != t || initl' != initl then CompoundInit (t', initl') else i

  
and visitCilLval (vis: cilVisitor) (lv: lval) : lval =
  doVisit vis vis#vlval childrenLval lv
and childrenLval (vis: cilVisitor) (lv: lval) : lval =  
  (* and visit its subexpressions *)
  let vExp e = visitCilExpr vis e in
  let vTyp t = visitCilType vis t in
  let vOff off = visitCilOffset vis off in
  match lv with
    Var v, off ->
      let v'   = doVisit vis vis#vvrbl (fun _ x -> x) v in
      let off' = vOff off in
      if v' != v || off' != off then Var v', off' else lv
  | Mem e, off -> 
      let e' = vExp e in
      let off' = vOff off in
      if e' != e || off' != off then Mem e', off' else lv

and visitCilOffset (vis: cilVisitor) (off: offset) : offset =
  doVisit vis vis#voffs childrenOffset off
and childrenOffset (vis: cilVisitor) (off: offset) : offset =
  let vOff off = visitCilOffset vis off in
  match off with
    Field (f, o) -> 
      let o' = vOff o in
      if o' != o then Field (f, o') else off
  | Index (e, o) -> 
      let e' = visitCilExpr vis e in
      let o' = visitCilOffset vis o in
      if e' != e || o' != o then Index (e', o') else off
  | NoOffset -> off

and visitCilInstr (vis: cilVisitor) (i: instr) : instr list =
  currentLoc := (get_instrLoc i);
  doVisitList vis vis#vinst childrenInstr i

and childrenInstr (vis: cilVisitor) (i: instr) : instr =
  let fExp = visitCilExpr vis in
  let fLval = visitCilLval vis in
  match i with
  | Set(lv,e,l) -> 
      let lv' = fLval lv in let e' = fExp e in
      if lv' != lv || e' != e then Set(lv',e',l) else i
  | Call(None,f,args,l) -> 
      let f' = fExp f in let args' = mapNoCopy fExp args in
      if f' != f || args' != args then Call(None,f',args',l) else i
  | Call(Some lv,fn,args,l) -> 
      let lv' = fLval lv in let fn' = fExp fn in 
      let args' = mapNoCopy fExp args in
      if lv' != lv || fn' != fn || args' != args 
      then Call(Some lv', fn', args', l) else i

  | Asm(sl,isvol,outs,ins,clobs,l) -> 
      let outs' = mapNoCopy (fun ((s,lv) as pair) -> 
                               let lv' = fLval lv in
                               if lv' != lv then (s,lv') else pair) outs in
      let ins'  = mapNoCopy (fun ((s,e) as pair) -> 
                               let e' = fExp e in
                               if e' != e then (s,e') else pair) ins in
      if outs' != outs || ins' != ins then
        Asm(sl,isvol,outs',ins',clobs,l) else i


(* visit all nodes in a Cil statement tree in preorder *)
and visitCilStmt (vis: cilVisitor) (s: stmt) : stmt =
  currentLoc := (get_stmtLoc s.skind) ;
  doVisit vis vis#vstmt childrenStmt s
and childrenStmt (vis: cilVisitor) (s: stmt) : stmt =
  let fExp e = (visitCilExpr vis e) in
  let fLval lv = (visitCilLval vis lv) in
  let fOff o = (visitCilOffset vis o) in
  let fBlock b = visitCilBlock vis b in
  let fInst i = visitCilInstr vis i in
  (* Just change the statement kind *)
  let skind' = 
    match s.skind with
      Break _ | Continue _ | Goto _ | Return (None, _) -> s.skind
    | Return (Some e, l) -> 
        let e' = fExp e in
        if e' != e then Return (Some e', l) else s.skind
    | Loop (b, l) -> 
        let b' = fBlock b in
        if b' != b then Loop (b', l) else s.skind
    | If(e, s1, s2, l) -> 
        let e' = fExp e in let s1'= fBlock s1 in let s2'= fBlock s2 in
        if e' != e || s1' != s1 || s2' != s2 then 
          If(e', s1', s2', l) else s.skind
    | Switch (e, b, stmts, l) -> 
        let e' = fExp e in let b' = fBlock b in
        (* Don't do stmts, but we better not change those *)
        if e' != e || b' != b then Switch (e', b', stmts, l) else s.skind
    | Instr il -> 
        let il' = mapNoCopyList fInst il in
        if il' != il then Instr il' else s.skind
    | Block b -> 
        let b' = fBlock b in 
        if b' != b then Block b' else s.skind
  in
  if skind' != s.skind then s.skind <- skind';
  (* Visit the labels *)
  let labels' = 
    let fLabel = function
        Case (e, l) as lb -> 
          let e' = fExp e in
          if e' != e then Case (e', l) else lb
        | lb -> lb
    in
    mapNoCopy fLabel s.labels
  in
  if labels' != s.labels then s.labels <- labels';
  s (* Always return the same statement *)
    
 
and visitCilBlock (vis: cilVisitor) (b: block) : block = 
  doVisit vis vis#vblock childrenBlock b
and childrenBlock (vis: cilVisitor) (b: block) : block = 
  let fStmt s = visitCilStmt vis s in
  let stmts' = mapNoCopy fStmt b.bstmts in
  if stmts' != b.bstmts then { battrs = b.battrs; bstmts = stmts'} else b


and visitCilType (vis : cilVisitor) (t : typ) : typ =
  doVisit vis vis#vtype childrenType t
and childrenType (vis : cilVisitor) (t : typ) : typ =
  (* look for types referred to inside t's definition *)
  let fTyp t  = visitCilType vis t in
  let fAttr a = visitCilAttributes vis a in
  match t with
    TPtr(t1, a) -> 
      let t1' = fTyp t1 in
      let a' = fAttr a in
      if t1' != t || a' != a then TPtr(t1', a') else t
  | TArray(t1, None, a) -> 
      let t1' = fTyp t1 in
      let a' = fAttr a in
      if t1' != t || a' != a  then TArray(t1', None, a') else t
  | TArray(t1, Some e, a) -> 
      let t1' = fTyp t1 in
      let e' = visitCilExpr vis e in
      let a' = fAttr a in
      if t1' != t || e' != e  || a' != a then TArray(t1', Some e', a') else t

      (* DON'T recurse automatically; user can call visitCompFields *)
  | TComp(cinfo, a) ->
      let a' = fAttr a in
      if a != a' then TComp(cinfo, a') else t

  | TFun(rettype, args, isva, a) -> 
      let rettype' = visitCilType vis rettype in
      (* iterate over formals, as variable declarations *)
      let argslist = argsToList args in
      let argslist' = mapNoCopy (visitCilVarDecl vis) argslist in
      let a' = fAttr a in
      if rettype' != rettype || argslist' != argslist || a' != a  then 
        let args' = if argslist' = argslist then args else Some argslist' in
        TFun(rettype', args', isva, a') else t

  | TNamed(s, t1, a) -> 
      let t1' = fTyp t1 in 
      let a' = fAttr a in
      if t1' != t1 || a' != a  then TNamed (s, t1', a') else t

  | _ -> t       (* other types don't contain types *)

(* for declarations, we visit the types inside; but for uses, *)
(* we just visit the varinfo node *)
and visitCilVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  doVisit vis vis#vvdec childrenVarDecl v 
and childrenVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  v.vtype <- visitCilType vis v.vtype;
  v.vattr <- visitCilAttributes vis v.vattr;  
  v

and visitCilAttributes (vis: cilVisitor) (al: attribute list) : attribute list=
   let al' = 
     mapNoCopyList (doVisitList vis vis#vattr childrenAttribute) al in
   if al' != al then 
     (* Must re-sort *)
     addAttributes al' []
   else
     al
and childrenAttribute (vis: cilVisitor) (a: attribute) : attribute = 
  let fTyp t  = visitCilType vis t in
  let rec doarg (aa: attrarg) = 
    match aa with 
      AId _ | AInt _ | AStr _ -> aa
    | AVar v -> 
        let v' = doVisit vis vis#vvrbl (fun _ x -> x) v in
        if v' != v then AVar v' else aa
    | ACons(n, args) -> 
        let args' = mapNoCopy doarg args in
        if args' != args then ACons(n, args') else aa
    | ASizeOf t -> 
        let t' = fTyp t in
        if t' != t then ASizeOf t' else aa
    | ASizeOfE e -> 
        let e' = doarg e in
        if e' != e then ASizeOfE e' else aa
    | AUnOp (uo, e1) -> 
        let e1' = doarg e1 in
        if e1' != e1 then AUnOp (uo, e1') else aa
    | ABinOp (bo, e1, e2) -> 
        let e1' = doarg e1 in
        let e2' = doarg e2 in
        if e1' != e1 || e2' != e2 then ABinOp (bo, e1', e2') else aa
  in
  match a with 
    Attr (n, args) -> 
      let args' = mapNoCopy doarg args in
      if args' != args then Attr(n, args') else a
      


let rec visitCilFunction (vis : cilVisitor) (f : fundec) : fundec =
  if debugVisit then ignore (E.log "Visiting function %s\n" f.svar.vname);
  doVisit vis vis#vfunc childrenFunction f

and childrenFunction (vis : cilVisitor) (f : fundec) : fundec =
  f.svar <- visitCilVarDecl vis f.svar; (* hit the function name *)
  (* visit local declarations *)
  f.slocals <- mapNoCopy (visitCilVarDecl vis) f.slocals;
  (* visit the formals *)
  let oldformals = f.sformals in
  let newformals = mapNoCopy (visitCilVarDecl vis) f.sformals in
  (* Restore the sharing if the formals have changed *)
  if oldformals != newformals then setFormals f newformals;
  f.sbody <- visitCilBlock vis f.sbody;        (* visit the body *)
  f

let rec visitCilGlobal (vis: cilVisitor) (g: global) : global list =
  (*(trace "visit" (dprintf "visitCilGlobal\n"));*)
  currentLoc := (get_globalLoc g) ;
  doVisitList vis vis#vglob childrenGlobal g
and childrenGlobal (vis: cilVisitor) (g: global) : global =
  match g with
  | GFun (f, l) -> 
      let f' = visitCilFunction vis f in
      if f' != f then GFun (f', l) else g
  | GType(s, t, l) ->
      let t' = visitCilType vis t in
      if t' != t then GType (s, t', l) else g
  | GEnumTag (enum, _) -> g
  | GCompTag (comp, _) ->
      (trace "visit" (dprintf "visiting global comp %s\n" comp.cname));
      (* Do the types of the fields *)
      List.iter (fun fi -> fi.ftype <- visitCilType vis fi.ftype) comp.cfields;
      g

  | GDecl(v, l) -> 
      let v' = visitCilVarDecl vis v in
      if v' != v then GDecl (v', l) else g
  | GVar (v, inito, l) -> 
      let v' = visitCilVarDecl vis v in
      let inito' = 
        match inito with
          None -> None 
        | Some i -> let i' = visitCilInit vis i in 
          if i' != i then Some i' else inito
      in
      if v' != v || inito' != inito then GVar (v', inito', l) else g
  | GPragma (a, l) -> begin
      match visitCilAttributes vis [a] with
        [a'] -> if a' != a then GPragma (a', l) else g
      | _ -> E.s (E.unimp "visitCilAttributes returns more than one attribute")
  end
  | _ -> g

let visitCilFile (vis : cilVisitor) (f : file) : file =
  let fGlob g = visitCilGlobal vis g in
  (* primary list of globals *)
  f.globals <- mapNoCopyList fGlob f.globals;
  (* the global initializer *)
  (match f.globinit with
    None -> ()
  | Some g -> f.globinit <- Some (visitCilFunction vis g));
  f




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

(* sm: utility *)
let startsWith (prefix: string) (s: string) : bool =
(
  let prefixLen = (String.length prefix) in
  (String.length s) >= prefixLen &&
  (String.sub s 0 prefixLen) = prefix
)

(* wes: I want to see this at the top level *)
let d_global () = function
  | GFun (fundec, l) ->
      (* If the function has attributes then print a prototype because GCC 
       * cannot accept function attributes in a definition  *)
      let oldattr = fundec.svar.vattr in
      let proto = 
        if oldattr <> [] then 
          (d_line l) ++ (d_videcl () fundec.svar) ++ chr ';' ++ line 
        else nil in
      (* Temporarily remove the function attributes *)
      fundec.svar.vattr <- [];
      let body = (d_line l) ++ (d_fun_decl () fundec) in
      fundec.svar.vattr <- oldattr;
      proto ++ body

  | GType (str, typ, l) ->
      d_line l ++
      if str = "" then
        ((d_decl (fun _ -> nil) DNNothing) () typ) ++ chr ';'
      else
        text "typedef "
          ++ ((d_decl (fun _ -> text str) DNString) () typ)
          ++ chr ';'

  | GEnumTag (enum, l) ->
     d_line l ++
     text "enum" ++ align ++ text (" " ^ enum.ename) ++
        d_attrlist () enum.eattr ++ text " {" ++ line
        ++ (docList line 
              (fun (n,i) -> 
                text (n ^ " = ") 
                  ++ d_exp () i
                  ++ text "," ++ break)
              () enum.eitems)
        ++ unalign ++ break ++ text "};"

  | GCompTag (comp, l) -> (* This is a definition of a tag *)
      let n = comp.cname in
      let su, su1, su2 =
        if comp.cstruct then "struct", "str", "uct"
                        else "union",  "uni", "on"
      in
      d_line l ++
      text su1 ++ (align ++ text su2 ++ chr ' ' ++ text n
                     ++ text " {" ++ line
                     ++ ((docList line (d_fielddecl ())) () comp.cfields)
                     ++ unalign)
        ++ line ++ text "}" ++
        (d_attrlist () comp.cattr) ++ text ";"

  | GVar (vi, io, l) ->
      d_line l ++
        (d_videcl () vi)
        ++ chr ' '
        ++ (match io with
              None -> nil
            | Some i -> text " = " ++ (d_init () i))
        ++ chr ';'

  | GDecl (vi, l) -> (
      (* sm: don't print boxmodels; avoids gcc warnings *)
      if (hasAttribute "boxmodel" vi.vattr) then
        (text ("// omitted boxmodel GDecl " ^ vi.vname ^ "\n"))
      (* sm: also don't print declarations for gcc builtins *)
      (* this doesn't do what I want, I don't know why *)
      else if (startsWith "__builtin_" vi.vname) then (
        (text ("// omitted gcc builtin " ^ vi.vname ^ "\n"))
      )
      else (
        d_line l ++
        (d_videcl () vi)
          ++ chr ';'
      )
    )
  | GAsm (s, l) ->
      d_line l ++
        text ("__asm__(\"" ^ escape_string s ^ "\");")

  | GPragma (Attr(an, args), l) ->
      (* sm: suppress printing pragmas that gcc does not understand *)
      (* assume anything starting with "box" is ours *)
      (* also don't print the 'combiner' pragma *)
      (* nor 'cilnoremove' *)
      let suppress = (startsWith "box" an) ||
                     (an = "combiner") ||
                     (an = "cilnoremove") in
      let d =
        if args = [] then
          text an
        else
          text (an ^ "(")
            ++ docList (chr ',') (d_attrarg ()) () args
            ++ text ")"
      in
      d_line l 
        ++ (if suppress then text "/* " else text "")
        ++ (text "#pragma ")
        ++ d
        ++ (if suppress then text " */" else text "")

  | GText s  -> text s

let printFile (out : out_channel) file =
  printDepth := 99999;  (* We don't want ... in the output *)
  (* If we are in RELEASE mode then we do not print indentation *)
  (* AB: These flags are no longer used by Pretty *)
(*
  noBreaks := true; noAligns := true;
  assert (noBreaks := false; noAligns := false; true);
*)    
  Pretty.fastMode := true;
  assert (Pretty.fastMode := false; true);
  if !E.verboseFlag then 
    ignore (E.log "printing file %s\n" file.fileName);
  let print x = fprint out 78 x in
  print (text ("/* Generated by safecc " ^ (Pretty.getAboutString()) ^ " */\n\n"));
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
                  (ss: stmt list) : unit = 
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
          peepHole1 doone tb.bstmts;
          peepHole1 doone eb.bstmts
      | Switch (e, b, _, _) -> peepHole1 doone b.bstmts
      | Loop (b, l) -> peepHole1 doone b.bstmts
      | Block b -> peepHole1 doone b.bstmts
      | Return _ | Goto _ | Break _ | Continue _ -> ())
    ss

let rec peepHole2  (* Process two statements and possibly replace them both *)
                   (dotwo: instr * instr -> instr list option)
                   (ss: stmt list) : unit = 
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
          peepHole2 dotwo tb.bstmts;
          peepHole2 dotwo eb.bstmts
      | Switch (e, b, _, _) -> peepHole2 dotwo b.bstmts
      | Loop (b, l) -> peepHole2 dotwo b.bstmts
      | Block b -> peepHole2 dotwo b.bstmts
      | Return _ | Goto _ | Break _ | Continue _ -> ())
    ss






let dExp: doc -> exp = 
  fun d -> Const(CStr(sprint 80 d))

let dInstr: doc -> location -> instr = 
  fun d l -> Asm([], [sprint 80 d], [], [], [], l)

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


  (* Make an AddrOf. Given an lval of type T will give back an expression of 
   * type ptr(T)  *)
let mkAddrOf ((b, off) as lval) : exp = 
  (* See if lval is a function or an array
  let isfun = 
    match  unrollType (typeOfLval lval) with
      TFun _ -> true
    | _ -> false
  in *)
  (* Never take the address of a register variable *)
  (match lval with
    Var vi, off when vi.vstorage = Register -> vi.vstorage <- NoStorage
  | _ -> ()); 
  match lval with
    Mem e, NoOffset -> e
  | b, Index(z, NoOffset) when isZero z -> StartOf (b, NoOffset)(* array *)
  | _ -> AddrOf lval


let mkAddrOrStartOf (lv: lval) : exp = 
  match unrollType (typeOfLval lv) with 
    TArray _ -> StartOf lv
  | _ -> mkAddrOf lv


  (* Make a Mem, while optimizing AddrOf. The type of the addr must be 
   * TPtr(t) and the type of the resulting lval is t. Note that in CIL the 
   * implicit conversion between a function and a pointer to a function does 
   * not apply. You must do the conversion yourself using AddrOf *)
let mkMem ~(addr: exp) ~(off: offset) : lval =  
  let res = 
    match addr, off with
      AddrOf lv, _ -> addOffsetLval off lv
    | StartOf lv, _ -> (* Must be an array *)
        addOffsetLval (Index(zero, off)) lv 
    | _, _ -> Mem addr, off
  in
(*  ignore (E.log "memof : %a:%a\nresult = %a\n" 
            d_plainexp addr d_plainoffset off d_plainexp res); *)
  res


let isIntegralType t = 
  match unrollType t with
    (TInt _ | TEnum _) -> true
  | _ -> false

let isArithmeticType t = 
  match unrollType t with
    (TInt _ | TEnum _ | TFloat _) -> true
  | _ -> false
    

let isPointerType t = 
  match unrollType t with
    TPtr _ -> true
  | _ -> false

let isFunctionType t = 
  match unrollType t with
    TFun _ -> true
  | _ -> false

let isArrayType t = 
  match unrollType t with
    TArray _ -> true
  | _ -> false


let rec isConstant = function
  | Const _ -> true
  | UnOp (_, e, _) -> isConstant e
  | BinOp (_, e1, e2, _) -> isConstant e1 && isConstant e2
  | Lval (Var vi, NoOffset) -> 
      (vi.vglob && isArrayType vi.vtype || isFunctionType vi.vtype)
  | Lval _ -> false
  | SizeOf _ | SizeOfE _ | AlignOf _ | AlignOfE _ -> true
  | CastE (_, e) -> isConstant e
  | AddrOf (Var vi, off) | StartOf (Var vi, off)
        -> vi.vglob && isConstantOff off
  | AddrOf (Mem e, off) | StartOf(Mem e, off) 
        -> isConstant e && isConstantOff off
(*  | Question (e1, e2, e3) -> 
      isConstant e1 && isConstant e2 && isConstant e3 *)

and isConstantOff = function
    NoOffset -> true
  | Field(fi, off) -> isConstantOff off
  | Index(e, off) -> isConstant e && isConstantOff off


let getCompField (cinfo:compinfo) (fieldName:string) : fieldinfo =
  (List.find (fun fi -> fi.fname = fieldName) cinfo.cfields)


let rec doCastT ~(e: exp) ~(oldt: typ) ~(newt: typ) = 
  (* Do not remove old casts because they are conversions !!! *)
  if typeSig oldt = typeSig newt then
    e
  else
    CastE(newt,e)

let doCast ~(e: exp) ~(newt: typ) = 
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
            (loop rt || List.exists (fun a -> loop a.vtype) (argsToList args))
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
          
(**
 **
 ** MACHINE DEPENDENT PART
 **
 **)
exception SizeOfError of typ

        
(* Get the minimum aligment in bytes for a given type *)
let rec alignOf_int = function
  | TInt((IChar|ISChar|IUChar), _) -> 1
  | TInt((IShort|IUShort), _) -> 2
  | TInt((IInt|IUInt), _) -> 4
  | TInt((ILong|IULong), _) -> 4
  | TInt((ILongLong|IULongLong), _) -> 8
  | TEnum _ -> 4 (* !!! is this correct ? *)
  | TFloat(FFloat, _) -> 4
  | TFloat((FDouble|FLongDouble), _) -> 8
  | TNamed (_, t, _) -> alignOf_int t
  | TArray (t, _, _) -> alignOf_int t
  | TPtr _ -> 4
        (* For composite types get the maximum alignment of any field inside *)
  | TComp (c, _) ->
      (* On GCC the zero-width fields do not contribute to the alignment. On 
       * MSVC only those zero-width that _do_ appear after other 
       * bitfields contribute to the alignment. So we drop those that 
       * do not occur after othe bitfields *)
      let rec dropZeros (afterbitfield: bool) = function
        | f :: rest when f.fbitfield = Some 0 && not afterbitfield -> 
            dropZeros afterbitfield rest
        | f :: rest -> f :: dropZeros (f.fbitfield <> None) rest
        | [] -> []
      in
      let fields = dropZeros false c.cfields in
      List.fold_left 
        (fun sofar f -> 
          (* Bitfields with zero width do not contribute to the alignment in 
           * GCC *)
          if not !msvcMode && f.fbitfield = Some 0 then sofar else
          max sofar (alignOf_int f.ftype)) 1 fields
        (* These are some error cases *)
  | (TFun _ | TVoid _) as t -> raise (SizeOfError t)
      
  
     
type offsetAcc = 
    { oaFirstFree: int;        (* The first free bit *)
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


(* GCC version *)
(* Does not use the sofar.oaPrevBitPack *)
let rec offsetOfFieldAcc_GCC (fi: fieldinfo) 
                             (sofar: offsetAcc) : offsetAcc = 
  (* field type *)
  let ftype = unrollType fi.ftype in
  let ftypeAlign = 8 * alignOf_int ftype in
  let ftypeBits = bitsSizeOf ftype in
(*
  if fi.fcomp.cname = "comp2898" then 
    ignore (E.log "offsetOfFieldAcc_GCC(%s of %s:%a%a,firstFree=%d,pack=%a)\n" 
              fi.fname fi.fcomp.cname 
              d_type ftype
              insert
              (match fi.fbitfield with
                None -> nil
              | Some wdthis -> dprintf ":%d" wdthis)
              sofar.oaFirstFree 
              insert
              (match sofar.oaPrevBitPack with 
                None -> text "None"
              | Some (packstart, _, wdpack) -> 
                  dprintf "Some(packstart=%d,wd=%d)"
                    packstart wdpack));
*)
  match ftype, fi.fbitfield with
    (* A width of 0 means that we must end the current packing. It seems that 
     * GCC pads only up to the alignment boundary for the type of this field. 
     * *)
  | _, Some 0 -> 
      let firstFree      = addTrailing sofar.oaFirstFree ftypeBits in
      { oaFirstFree      = firstFree;
        oaLastFieldStart = firstFree;
        oaLastFieldWidth = 0;
        oaPrevBitPack    = None }

    (* A bitfield cannot span the alignment unit of its type *)
  | _, Some wdthis 
      when (sofar.oaFirstFree + wdthis - 1) / ftypeAlign 
            <> sofar.oaFirstFree / ftypeAlign  -> 
        (* Pad and redo *)
        offsetOfFieldAcc_GCC fi
          { oaFirstFree      = addTrailing sofar.oaFirstFree ftypeAlign;
            oaLastFieldStart = sofar.oaLastFieldStart;
            oaLastFieldWidth = sofar.oaLastFieldWidth;
            oaPrevBitPack    = None }
        
   (* Try a simple method. Just put it down after we made certain that it 
    * does not span its alignment *)
  | _, Some wdthis -> 
      { oaFirstFree      = sofar.oaFirstFree + wdthis;
        oaLastFieldStart = sofar.oaFirstFree; 
        oaLastFieldWidth = wdthis;
        oaPrevBitPack    = None
      } 

     (* Non-bitfield *)
  | _, None -> 
      (* Align this field *)
      let newStart = addTrailing sofar.oaFirstFree ftypeAlign  in
      { oaFirstFree = newStart + ftypeBits;
        oaLastFieldStart = newStart;
        oaLastFieldWidth = ftypeBits;
        oaPrevBitPack = None;
      } 

(* MSVC version *)
and offsetOfFieldAcc_MSVC (fi: fieldinfo) 
                              (sofar: offsetAcc) : offsetAcc = 
  (* field type *)
  let ftype = unrollType fi.ftype in
  let ftypeAlign = 8 * alignOf_int ftype in
  let ftypeBits = bitsSizeOf ftype in
(*
  ignore (E.log "offsetOfFieldAcc_MSVC(%s of %s:%a%a,firstFree=%d, pack=%a)\n" 
            fi.fname fi.fcomp.cname 
            d_type ftype
            insert
            (match fi.fbitfield with
              None -> nil
            | Some wdthis -> dprintf ":%d" wdthis)
            sofar.oaFirstFree 
            insert
            (match sofar.oaPrevBitPack with 
              None -> text "None"
            | Some (prevpack, _, wdpack) -> dprintf "Some(prev=%d,wd=%d)"
                  prevpack wdpack));
*)
  match ftype, fi.fbitfield, sofar.oaPrevBitPack with
    (* Ignore zero-width bitfields that come after non-bitfields *)
  | TInt (ikthis, _), Some 0, None -> 
      let firstFree      = sofar.oaFirstFree in
      { oaFirstFree      = firstFree;
        oaLastFieldStart = firstFree;
        oaLastFieldWidth = 0;
        oaPrevBitPack    = None }

    (* If we are in a bitpack and we see a bitfield for a type with the 
     * different width than the pack, then we finish the pack and retry *)
  | _, Some _, Some (packstart, _, wdpack) when wdpack != ftypeBits ->
      let firstFree = 
        if sofar.oaFirstFree = packstart then packstart else
        packstart + wdpack
      in
      offsetOfFieldAcc_MSVC fi
        { oaFirstFree      = addTrailing firstFree ftypeAlign;
          oaLastFieldStart = sofar.oaLastFieldStart;
          oaLastFieldWidth = sofar.oaLastFieldWidth;
          oaPrevBitPack    = None }

    (* A width of 0 means that we must end the current packing. *)
  | TInt (ikthis, _), Some 0, Some (packstart, _, wdpack) -> 
      let firstFree = 
        if sofar.oaFirstFree = packstart then packstart else
        packstart + wdpack
      in
      let firstFree      = addTrailing firstFree ftypeAlign in
      { oaFirstFree      = firstFree;
        oaLastFieldStart = firstFree;
        oaLastFieldWidth = 0;
        oaPrevBitPack    = Some (firstFree, ikthis, ftypeBits) }

   (* Check for a bitfield that fits in the current pack after some other 
    * bitfields *)
  | TInt(ikthis, _), Some wdthis, Some (packstart, ikprev, wdpack)
      when  packstart + wdpack >= sofar.oaFirstFree + wdthis ->
              { oaFirstFree = sofar.oaFirstFree + wdthis;
                oaLastFieldStart = sofar.oaFirstFree; 
                oaLastFieldWidth = wdthis;
                oaPrevBitPack = sofar.oaPrevBitPack
              } 


  | _, _, Some (packstart, _, wdpack) -> (* Finish up the bitfield pack and 
                                          * restart. *)
      let firstFree = 
        if sofar.oaFirstFree = packstart then packstart else
        packstart + wdpack
      in
      offsetOfFieldAcc_MSVC fi
        { oaFirstFree      = addTrailing firstFree ftypeAlign;
          oaLastFieldStart = sofar.oaLastFieldStart;
          oaLastFieldWidth = sofar.oaLastFieldWidth;
          oaPrevBitPack    = None }

        (* No active bitfield pack. But we are seeing a bitfield. *)
  | TInt(ikthis, _), Some wdthis, None -> 
      let firstFree     = addTrailing sofar.oaFirstFree ftypeAlign in
      { oaFirstFree     = firstFree + wdthis;
        oaLastFieldStart = firstFree;
        oaLastFieldWidth = wdthis;
        oaPrevBitPack = Some (firstFree, ikthis, ftypeBits); }

     (* No active bitfield pack. Non-bitfield *)
  | _, None, None -> 
      (* Align this field *)
      let firstFree = addTrailing sofar.oaFirstFree ftypeAlign  in
      { oaFirstFree = firstFree + ftypeBits;
        oaLastFieldStart = firstFree;
        oaLastFieldWidth = ftypeBits;
        oaPrevBitPack = None;
      } 

  | _, Some _, None -> E.s (E.bug "offsetAcc")


and offsetOfFieldAcc (fi: fieldinfo) 
                     (sofar: offsetAcc) : offsetAcc = 
  if !msvcMode then offsetOfFieldAcc_MSVC fi sofar
  else offsetOfFieldAcc_GCC fi sofar

(* The size of a type, in bits. If struct or array then trailing padding is 
 * added *)
and bitsSizeOf t = 
  match t with 
    TInt _ | TFloat _ | TEnum _ | TPtr _ -> 8 * alignOf_int t
  | TNamed (_, t, _) -> bitsSizeOf t
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
      if !msvcMode && lastoff.oaFirstFree = 0 && comp.cfields <> [] then
          (* On MSVC if we have just a zero-width bitfields then the length 
           * is 32 and is not padded  *)
        32
      else
        addTrailing lastoff.oaFirstFree (8 * alignOf_int t)
        
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
      addTrailing max (8 * alignOf_int t)

  | TArray(t, Some len, _) -> begin
      match constFold true len with 
        Const(CInt64(l,_,_)) -> 
          addTrailing ((bitsSizeOf t) * (Int64.to_int l)) (8 * alignOf_int t)
      | _ -> raise (SizeOfError t)
  end

  | TArray (_, None, _) | TFun _ | TVoid _ -> raise (SizeOfError t)


and addTrailing nrbits roundto = 
    (nrbits + roundto - 1) land (lnot (roundto - 1))

and sizeOf t = 
  try
    integer ((bitsSizeOf t) lsr 3)
  with SizeOfError _ -> SizeOf(t)

 
and bitsOffset (baset: typ) (off: offset) : int * int = 
  let rec loopOff (baset: typ) (width: int) (start: int) = function
      NoOffset -> start, width
    | Index(e, off) -> begin
        let ei = 
          match isInteger e with
            Some i64 -> Int64.to_int i64
          | None -> raise (SizeOfError baset)
        in
        let bt = 
          match unrollType baset with
            TArray(bt, _, _) -> bt
          | _ -> E.s (E.bug "bitsOffset: Index on a non-array")
        in
        let bitsbt = bitsSizeOf bt in
        loopOff bt bitsbt (start + ei * bitsbt) off
    end
    | Field(f, off) when not f.fcomp.cstruct -> 
        (* All union fields start at offset 0 *)
        loopOff f.ftype (bitsSizeOf f.ftype) start off

    | Field(f, off) -> 
        (* Construct a list of fields preceeding and including this one *)
        let prevflds = 
          let rec loop = function
              [] -> E.s (E.bug "Cannot find field %s\n" f.fname)
            | fi' :: _ when fi' == f -> [fi']
            | fi' :: rest -> fi' :: loop rest
          in
          loop f.fcomp.cfields
        in
        let lastoff =
          List.fold_left (fun acc fi' -> offsetOfFieldAcc fi' acc)
            { oaFirstFree      = 0; (* Start at 0 because each struct is done 
                                     * separately *)
              oaLastFieldStart = 0;
              oaLastFieldWidth = 0;
              oaPrevBitPack    = None } prevflds
        in
        (* ignore (E.log "Field %s of %s: start=%d, lastFieldStart=%d\n"
                  f.fname f.fcomp.cname start lastoff.oaLastFieldStart); *)
        loopOff f.ftype lastoff.oaLastFieldWidth 
               (start + lastoff.oaLastFieldStart) off
  in
  loopOff baset (bitsSizeOf baset) 0 off
        



(*** Constant folding. If machdep is true then fold even sizeof operations ***)
and constFold (machdep: bool) (e: exp) : exp = 
  match e with
    BinOp(bop, e1, e2, tres) -> constFoldBinOp machdep bop e1 e2 tres
  | UnOp(Neg, e1, tres) -> begin
      try
        let tk = 
          match unrollType tres with
            TInt(ik, _) -> ik
          | TEnum _ -> IInt
          | _ -> raise Not_found (* probably a float *)
        in
        match constFold machdep e1 with
          Const(CInt64(i,ik,_)) -> kinteger64 tk (Int64.neg i)
        | _ -> e
      with Not_found -> e
  end
        (* Characters are integers *)
  | Const(CChr c) -> Const(CInt64(Int64.of_int (Char.code c), 
                                  IInt, None))
  | SizeOf t when machdep -> begin
      try
        let bs = bitsSizeOf t in
        kinteger IUInt (bs / 8)
      with SizeOfError _ -> e
  end
  | SizeOfE e when machdep -> constFold machdep (SizeOf (typeOf e))

  | AlignOf t when machdep -> kinteger IUInt (alignOf_int t)
  | AlignOfE e when machdep -> constFold machdep (AlignOf (typeOf e))

  | CastE (t, e) -> begin
      match constFold machdep e, unrollType t with 
        (* Might truncate silently *)
        Const(CInt64(i,k,_)), TInt(nk,_) -> 
          let i' = truncateInteger64 nk i in
          Const(CInt64(i', nk, None))
      | e', _ -> CastE (t, e')
  end

  | _ -> e

and constFoldBinOp (machdep: bool) bop e1 e2 tres = 
  let e1' = constFold machdep e1 in
  let e2' = constFold machdep e2 in
  if isIntegralType tres then begin
    let newe = 
      let rec mkInt = function
          Const(CChr c) -> Const(CInt64(Int64.of_int (Char.code c), 
                                        IInt, None))
        | CastE(TInt (ik, ta), e) -> begin
            match mkInt e with
              Const(CInt64(i, _, _)) -> 
                let i' = truncateInteger64 ik i in
                Const(CInt64(i', ik, None))

            | e' -> CastE(TInt(ik, ta), e')
        end
        | e -> e
      in
      let tk = 
        match unrollType tres with
          TInt(ik, _) -> ik
        | TEnum _ -> IInt
        | _ -> E.s (bug "constFoldBinOp")
      in
      (* See if the result is unsigned *)
      let isunsigned = function
          (IUInt | IUChar | IUShort | IULong | IULongLong) -> true
        | _ -> false
      in
      let ge (unsigned: bool) (i1: int64) (i2: int64) : bool = 
        if unsigned then 
          let l1 = Int64.shift_right_logical i1 1 in
          let l2 = Int64.shift_right_logical i2 1 in (* Both positive now *)
          (l1 > l2) || (l1 = l2 && 
                        Int64.logand i1 Int64.one >= Int64.logand i2 Int64.one)
        else i1 >= i2
      in
      (* Assume that the necessary promotions have been done *)
      match bop, mkInt e1', mkInt e2' with
      | PlusA, Const(CInt64(z,_,_)), e2'' when z = Int64.zero -> e2''
      | PlusA, e1'', Const(CInt64(z,_,_)) when z = Int64.zero -> e1''
      | PlusPI, e1'', Const(CInt64(z,_,_)) when z = Int64.zero -> e1''
      | IndexPI, e1'', Const(CInt64(z,_,_)) when z = Int64.zero -> e1''
      | MinusPI, e1'', Const(CInt64(z,_,_)) when z = Int64.zero -> e1''
      | PlusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> 
          kinteger64 tk (Int64.add i1 i2)
      | MinusA, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> 
          kinteger64 tk (Int64.sub i1 i2)
      | Mult, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> 
          kinteger64 tk (Int64.mul i1 i2)
      | Div, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> begin
          try kinteger64 tk (Int64.div i1 i2)
          with Division_by_zero -> BinOp(bop, e1', e2', tres)
      end
      | Mod, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> begin
          try kinteger64 tk (Int64.rem i1 i2)
          with Division_by_zero -> BinOp(bop, e1', e2', tres) 
      end
      | BAnd, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> 
          kinteger64 tk (Int64.logand i1 i2)
      | BOr, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> 
          kinteger64 tk (Int64.logor i1 i2)
      | BXor, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> 
          kinteger64 tk (Int64.logxor i1 i2)
      | Shiftlt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,IInt,_)) -> 
          kinteger64 tk (Int64.shift_left i1 (Int64.to_int i2))
      | Shiftrt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,IInt,_)) -> 
          if isunsigned ik1 then 
            kinteger64 tk (Int64.shift_right_logical i1 (Int64.to_int i2))
          else
            kinteger64 tk (Int64.shift_right i1 (Int64.to_int i2))

      | Eq, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> 
          integer (if i1 = i2 then 1 else 0)
      | Ne, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 -> 
          integer (if i1 <> i2 then 1 else 0)
      | Le, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          integer (if ge (isunsigned ik1) i2 i1 then 1 else 0)

      | Ge, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          integer (if ge (isunsigned ik1) i1 i2 then 1 else 0)

      | Lt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          integer (if i1 <> i2 && ge (isunsigned ik1) i2 i1 then 1 else 0)

      | Gt, Const(CInt64(i1,ik1,_)),Const(CInt64(i2,ik2,_)) when ik1 = ik2 ->
          integer (if i1 <> i2 && ge (isunsigned ik1) i1 i2 then 1 else 0)
      | _ -> BinOp(bop, e1', e2', tres)
    in
    if debugConstFold then 
      ignore (E.log "Folded %a to %a\n" d_exp (BinOp(bop, e1', e2', tres)) d_exp newe);
    newe
  end else
    BinOp(bop, e1', e2', tres)


(* Try to do an increment, with constant folding *)
let increm (e: exp) (i: int) =
  let et = typeOf e in
  let bop = if isPointerType et then PlusPI else PlusA in
  constFold false (BinOp(bop, e, integer i, et))
      
  

(*** Make a initializer for zeroe-ing a data type ***)
let rec makeZeroInit (t: typ) : init = 
  match unrollType t with
    TInt (ik, _) -> SingleInit (Const(CInt64(Int64.zero, ik, None)))
  | TFloat(fk, _) -> SingleInit(Const(CReal(0.0, fk, None)))
  | TEnum _ -> SingleInit zero
  | TComp (comp, _) as t' when comp.cstruct -> 
      let inits = 
        List.fold_right
          (fun f acc -> 
            if f.fname <> missingFieldName then 
              makeZeroInit f.ftype :: acc
            else
              acc)
          comp.cfields []
      in
      CompoundInit (t', inits)

  | TComp (comp, _) as t' when not comp.cstruct -> 
      let fstfield = 
        match comp.cfields with
          f :: _ -> f
        | [] -> E.s (E.unimp "Cannot create init for empty union")
      in
      CompoundInit(t, [makeZeroInit fstfield.ftype])

  | TArray(bt, Some len, _) as t' -> 
      let n = 
        match constFold true len with
          Const(CInt64(n, _, _)) -> Int64.to_int n
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
let foldLeftCompound 
    ~(doinit: offset -> init -> typ -> 'a -> 'a)
    ~(ct: typ) 
    ~(initl: init list)
    ~(acc: 'a) : 'a = 
  match unrollType ct with
    TArray(bt, _, _) -> 
      let rec foldArray  
          (nextidx: exp) 
          (initl: init list)
          (acc: 'a) : 'a  =
        let incrementIdx = function
            Const(CInt64(n, ik, _)) -> Const(CInt64(Int64.succ n, ik, None))
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
                doinit (Field(thisfield, NoOffset)) ie thisfield.ftype acc 
              in
              foldFields nextfields restinitl acc'
        in
        foldFields 
          (List.filter (fun f -> f.fname <> missingFieldName) comp.cfields) 
          initl acc
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



let debugAlpha = false
(*** Alpha conversion ***)
(* Create a new name based on a given name. The new name is formed from a 
 * prefix (obtained from the given name by stripping a suffix consisting of _ 
 * followed by only digits), followed by a '_' and then by a positive integer 
 * suffix. The first argument is a table mapping name prefixes with the 
b * largest suffix used so far for that prefix. The largest suffix is one when 
 * only the version without suffix has been used. *)
let rec newAlphaName (alphaTable: (string, int ref) H.t)
                     (lookupname: string) : string = 
  let prefix, sep, suffix = splitNameForAlpha lookupname in
  (* ignore (E.log "newAlphaName(%s). P=%s, S=%d\n" lookupname prefix suffix);
     *)
  if debugAlpha then
    ignore (E.log "Alpha conv: %s %s %d. " prefix sep suffix);
  let newname = 
    try
      let rc = H.find alphaTable prefix in
      if debugAlpha then
        ignore (E.log " Old suffix %d. " !rc);
      let newsuffix, sep = 
        if suffix > !rc then suffix, sep else !rc + 1, "_" in
      rc := newsuffix;
      prefix ^ sep ^ (string_of_int newsuffix)
    with Not_found -> begin (* First variable with this prefix *)
      H.add alphaTable prefix (ref suffix);
      if debugAlpha then ignore (E.log " First seen. ");
      lookupname  (* Return the original name *)
    end
  in
  if debugAlpha then
    ignore (E.log " Res=: %s\n" newname);
  newname
  
(* Strip the suffix. Return the prefix, the separator (empty or _) and a 
 * numeric suffix (-1 if the separator is empty or if _ is the last thing in 
 * the name) *)
and splitNameForAlpha (lookupname: string) : (string * string * int) = 
  (* Split the lookup name into a prefix, a separator (empty or _) and a 
   * suffix. The suffix is numberic and is separated by _ from the prefix  *)
  try
    let under_idx = String.rindex lookupname '_' in
    let l = String.length lookupname in
    (* Check that we have only digits following the underscore *)
    if under_idx = l - 1 then raise Not_found;
    (* If we have a 0 right after the _ and more characters after that then 
     * we consider that we do not have a suffix *)
    if String.get lookupname (under_idx + 1) = '0' &&
       under_idx < l - 2 then raise Not_found;
    let rec collectSuffix (acc: int) (i: int) = 
      if i = l then 
        (String.sub lookupname 0 under_idx, "_", acc)
      else
        let c = Char.code (String.get lookupname i) - Char.code '0' in
        if c >= 0 && c <= 9 then 
          collectSuffix (10 * acc + c) (i + 1)
        else
          raise Not_found
    in
    collectSuffix 0 (under_idx + 1)
  with Not_found -> (* No suffix in the name *)
    (lookupname, "", -1)


let docAlphaTable (alphaTable: (string, int ref) H.t) = 
  let acc : (string * int) list ref = ref [] in
  H.iter (fun k d -> acc := (k, !d) :: !acc) alphaTable;
  docList line (fun (k, d) -> dprintf "  %s -> %d" k d) () !acc



(* A visitor that makes a deep copy of a function body *)
class copyFunctionVisitor (newname: string) = object (self)
  inherit nopCilVisitor

      (* Keep here a maping from locals to their copies *)
  val map : (string, varinfo) H.t = H.create 113 
      (* Keep here a maping from statements to their copies *)
  val stmtmap : (int, stmt) H.t = H.create 113
  val sid = ref 0 (* Will have to assign ids to statements *)
      (* Keep here a list of statements to be patched *)
  val patches : stmt list ref = ref []

  val argid = ref 0

      (* This is the main function *)
  method vfunc (f: fundec) : fundec visitAction = 
    (* We need a map from the old locals/formals to the new ones *)
    H.clear map;
    argid := 0;
     (* Make a copy of the fundec. *)
    let f' = {f with svar = f.svar} in
    let patchfunction (f' : fundec) = 
      (* Change the name. Only this late to allow the visitor to copy the 
       * svar  *)
      f'.svar.vname <- newname;
      let findStmt (i: int) = 
        try H.find stmtmap i 
        with Not_found -> E.s (bug "Cannot find the copy of stmt#%d" i)
      in
      let patchstmt (s: stmt) = 
        match s.skind with
          Goto (sr, l) -> 
            (* Make a copy of the reference *)
            let sr' = ref (findStmt !sr.sid) in
            s.skind <- Goto (sr',l)
        | Switch (e, body, cases, l) -> 
            s.skind <- Switch (e, body, 
                               List.map (fun cs -> findStmt cs.sid) cases, l)
        | _ -> ()
      in
      List.iter patchstmt !patches;
      f'
    in
    patches := [];
    sid := 0;
    H.clear stmtmap;
    ChangeDoChildrenPost (f', patchfunction)
    
      (* We must create a new varinfo for each declaration. Memoize to 
       * maintain sharing *)
  method vvdec (v: varinfo) = 
    (* Some varinfo have empty names. Give them some name *)
    if v.vname = "" then begin
      v.vname <- "arg" ^ string_of_int !argid; incr argid
    end;
    try
      ChangeTo (H.find map v.vname)
    with Not_found -> begin
      let v' = {v with vname = v.vname} in
      H.add map v.vname v';
      ChangeDoChildrenPost (v', fun x -> x)
    end

      (* We must replace references to local variables *)
  method vvrbl (v: varinfo) = 
    if v.vglob then SkipChildren else 
    try
      ChangeTo (H.find map v.vname)
    with Not_found -> 
      E.s (bug "Cannot find the new copy of local variable %s" v.vname)


        (* Replace statements. *)
  method vstmt (s: stmt) : stmt visitAction = 
    s.sid <- !sid; incr sid;
    let s' = {s with sid = s.sid} in
    H.add stmtmap s.sid s'; (* Remember where we copied this *)
    (* if we have a Goto or a Switch remember them to fixup at end *)
    (match s'.skind with
      (Goto _ | Switch _) -> patches := s' :: !patches
    | _ -> ());
    (* Do the children *)
    ChangeDoChildrenPost (s', fun x -> x)

      (* Copy blocks since they are mutable *)
  method vblock (b: block) = 
    ChangeDoChildrenPost ({b with bstmts = b.bstmts}, fun x -> x)


  method vglob _ = E.s (bug "copyFunction should not be used on globals")
end

(* We need a function that copies a CIL function. *)
let copyFunction (f: fundec) (newname: string) : fundec = 
  visitCilFunction (new copyFunctionVisitor(newname)) f
  
