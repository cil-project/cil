(*
 * CIL: An intermediate language for analyzing C progams.
 *
 * George Necula
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

(**** The constraints specified in this file are checked in the module Check. 
    * Take a look there for a precise specification of the invariants that a 
    * Cil.file must satisfy *)

(* TODO
   - inner struct/union/enum/typedef tags
   - type of sizeof is hardwired to UInt
   - integerFits is hardwired to true
   - in cabs2cil we drop the volatile sometimes
*)

(* A few globals that control the interpretation of C source *)
val msvcMode : bool ref               (* Whether the pretty printer should 
                                       * print output for the MS VC 
                                       * compiler. Default is GCC *)
val charIsUnsigned : bool ref         (* Whether CHAR is unsigned. Default 
                                       * false *)
val ilongFitsUInt : bool ref         (* Whether a signed long can fit an 
                                       * unsigned integer. True only if a 
                                       * long uses more bits than an int  *)
val newCil : bool ref

val printLn: bool ref                   (* Whether to print line numbers *)
val printLnComment: bool ref            (* Whether to print line numbers in comments*)

type location = { 
    line: int;				(* -1 means "do not know" *)
    file: string; 
}

val locUnknown: location
(* A reference to the current location *)
val currentLoc: location ref

val d_loc: unit -> location -> Pretty.doc
val d_thisloc: unit -> Pretty.doc  (* the currentLoc *)

(* the following error message producing functions also print a location in 
 * the code. use Errormsg.bug and Errormsg.unimp if you do not want that *)
val bug: ('a,unit,Pretty.doc) format -> 'a
val unimp: ('a,unit,Pretty.doc) format -> 'a
val error: ('a,unit,Pretty.doc) format -> 'a
val errorLoc: location -> ('a,unit,Pretty.doc) format -> 'a  
val unimp: ('a,unit,Pretty.doc) format -> 'a  
val warn: ('a,unit,Pretty.doc) format -> 'a
val warnLoc: location -> ('a,unit,Pretty.doc) format -> 'a  

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
                         * makeFormal, makeLocalVar, makeTempVar or 
                         * makeGlobalVar  *)
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
    mutable fbitfield: int option;      (* If a bitfield then ftype should be 
                                         * an integer type *)
    mutable fattr: attribute list;
}


(* Information about a composite type (a struct or a union). Use mkCompInfo 
 * to create non-recursive or (potentially) recursive versions of this. Make 
 * sure you have a GCompTag for each one of these.  *)
and compinfo = {
    mutable cstruct: bool;              (* true if struct, false if union *)
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
    mutable creferenced: bool;          (* true if used; init to false *)
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
    mutable ereferenced: bool;         (* true if used; init to false *)
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
                  * represented on 64 bits. OCAML does not give Overflow 
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
                                         * This is not turned into
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

  | Question   of exp * exp * exp      (* e1 ? e2 : e3. Sometimes we cannot 
                                        * turn this into a conditional 
                                        * statement (e.g. in global 
                                        * initializers). This is only allowed 
                                        * inside constant initializers.!!! In 
                                        * all other places it must bne turned 
                                        * into IfThenElse  *)
  | CastE      of typ * exp            (* Use doCast to make casts *)

  | AddrOf     of lval                 (* Always use mkAddrOf to construct
                                        * one of these. Apply to an lv of 
                                        * type T yields an expression of type 
                                        * Ptr(T) *)

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
 			 (* optional: result is an lval. A cast might 
                          * be necessary if the declared result type of the 
                          * function is not the same as that of the 
                          * destination. If the function is declared then casts 
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
                                         * function. Use setFormals or 
                                         * makeFormalVar or setFunctionType 
                                         * to set these formals and ensure 
                                         * that they are reflected in the 
                                         * function type. Do not make copies 
                                         * of these because the body refers 
                                         * to them. *)
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
	(* global function decls, global variable decls *)


(* Construct an integer of a given kind. *)
val kinteger: ikind -> int -> exp
val kinteger64: ikind -> int64 -> exp

(* Construct an integer of the first kind that is big enough. Use only for 
 * positive integers *)
val integerKinds: ikind list -> int64 -> exp

(* Construct an integer of kind IInt. *)
val integer: int -> exp
val integer64: int64 -> exp


val hexinteger: int -> exp
             
val zero: exp
val one: exp
val mone: exp

(* Do constant folding *)    
val constFold: exp -> exp

(* And a special case for binary operations *)
val constFoldBinOp: binop -> exp -> exp -> typ -> exp

(* Increment an expression. Can be arithmetic or pointer type *) 
val increm: exp -> int -> exp

val voidType: typ
val intType: typ
val uintType: typ
val longType: typ
val ulongType: typ
val charType: typ
val charPtrType: typ
val charConstPtrType: typ
val voidPtrType: typ
val intPtrType: typ
val uintPtrType: typ
val doubleType: typ

(* An integer type that fits pointers. We hardwire to unsigned long for now *)
val upointType: typ

val isInteger: exp -> int64 option
val isZero: exp -> bool

val mkStmt: stmtkind -> stmt
val mkBlock: stmt list -> block

val mkStmtOneInstr: instr -> stmt

(* use this instead of List.@ because you get fewer basic blocks *)
val compactStmts: stmt list -> stmt list

val mkEmptyStmt: unit -> stmt
val dummyInstr: instr
val dummyStmt: stmt
  
(* Generate fresh names from a prefix 
val newTypeName: string -> string
*)

val isCompleteType: typ -> bool  (* Returns true if this is a complete type. 
                                  * This means that sizeof(t) makes sense. 
                                  * Incomplete types are not yet defined 
                                  * structures and empty arrays. *)


(* Get the full name of a comp *)
val compFullName: compinfo -> string

(* Set the name of a composite type. Also changes the key *)
val compSetName: compinfo -> string -> unit

 
(** Creates a a (potentially recursive) composite type **)
val mkCompInfo: bool ->       (* whether it is a struct or a union *)
               string ->     (* empty for anonymous structures *)
               (* mkfspec is a function that when given a forward 
                * representation of the structure type constructs the type of 
                * the fields. The function can ignore this argument if not 
                * constructing a recursive type.  *)
               (typ -> (string * typ * int option * attribute list) list) ->
               attribute list -> compinfo


(* Scans a type by applying the function on all elements. Care is taken to 
 * apply the function only once on each composite type, thus avoiding 
 * circularity. When the function returns ExistsTrue, the scan stops with true 
 * *)
type existsAction = 
    ExistsTrue                          (* We have found it *)
  | ExistsFalse                         (* Stop processing this branch *)
  | ExistsMaybe                         (* This node is not what we are 
                                         * looking for but maybe its 
                                         * successors are *)

val existsType: (typ -> existsAction) -> typ -> bool

val var: varinfo -> lval

  (* Make an AddrOf. Given an lval of type T will give back an expression of 
   * type ptr(T). It optimizes somewhat expressions like "& v" and "& v[0]"  *)
val mkAddrOf: lval -> exp               


  (* Make a Mem, while optimizing AddrOf. The type of the addr must be 
   * TPtr(t) and the type of the resulting lval is t. Note that in CIL the 
   * implicit conversion between an array and the pointer to the first 
   * element does not apply. You must do the conversion yourself using 
   * StartOf *)
val mkMem: addr:exp -> off:offset -> lval


val mkString: string -> exp


    (* Make a while loop. Can contain Break or Continue *)
val mkWhile: guard:exp -> body:stmt list -> stmt list

    (* Make a for loop for(i=start; i<past; i += incr) { ... }. The body 
     * can contain Break but not Continue !!!. Can be used with i a pointer 
     * or an integer. Start and done must have the same type but incr 
     * must be an integer *)
val mkForIncr:  iter:varinfo -> first:exp -> stopat:exp -> incr:exp 
                 -> body:stmt list -> stmt list

    (* Make a for loop for(start; guard; next) { ... }. The body can 
     * contain Break but not Continue !!! *) 
val mkFor: start:stmt list -> guard:exp -> next: stmt list -> 
                                       body: stmt list -> stmt list
 



(**** Utility functions ******)
val unrollType: typ -> typ   (* Might drop some attributes !! *)

(* 
  Pretty Printing
 *)

(* location *)
val d_loc: unit -> location -> Pretty.doc
val d_ikind: unit -> ikind -> Pretty.doc
val d_fkind: unit -> fkind -> Pretty.doc
val d_storage: unit -> storage -> Pretty.doc
val d_const: unit -> constant -> Pretty.doc


  (* When we print types for consumption by another compiler we must be 
   * careful to avoid printing multiple type definitions *)
val printShortTypes: bool ref (* Prints "struct n" instead of the fields *)
val d_type: unit -> typ -> Pretty.doc


(* exp *)

val d_exp: unit -> exp -> Pretty.doc
val d_init: unit -> init -> Pretty.doc
val d_binop: unit -> binop -> Pretty.doc



val d_attr: unit -> attribute -> Pretty.doc
val d_attrarg: unit -> attrarg -> Pretty.doc
val d_attrlist: bool -> unit -> attribute list -> Pretty.doc (* Whether it 
                                                              * comes before 
                                                              * or after 
                                                              * stuff  *) 
val d_stmt: unit -> stmt -> Pretty.doc
val d_block: unit -> block -> Pretty.doc
val d_lval: unit -> lval -> Pretty.doc
val d_instr: unit -> instr -> Pretty.doc
val d_fun_decl: unit -> fundec -> Pretty.doc
val d_videcl: unit -> varinfo -> Pretty.doc
val printFile: out_channel -> file -> unit

(* Use setCustomPrint to intercept all attributes as are printed. If your 
 * function returns Some d then d is used as the external form of the 
 * attribute. Otherwise the attribute is printed normally. *)
val setCustomPrintAttribute: 
    (attribute -> Pretty.doc option) -> unit

(* Like the above but _adds_ the given function to the begining of the chain 
 * of custom attribute printers but only for the duration of executing the 
 * function passed as the second argument on the third argument *)
val setCustomPrintAttributeScope: 
    (attribute -> Pretty.doc option) -> ('a -> 'b) -> 'a -> 'b


(* removeUnusedTemps moved to rmtmps.mli *)


   (* Some plain pretty-printers. Unlike the above these expose all the 
    * details of the internal representation *)
val d_plainexp: unit -> exp -> Pretty.doc
val d_plaininit: unit -> init -> Pretty.doc
val d_plainlval: unit -> lval -> Pretty.doc
val d_plainoffset: unit -> offset -> Pretty.doc
val d_plaintype: unit -> typ -> Pretty.doc
val d_global: unit -> global -> Pretty.doc

(******************
 ******************
 ******************)

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
  method vvrbl: varinfo -> varinfo visitAction  (* variable use.  *)
  method vvdec: varinfo -> varinfo visitAction  (* variable declaration. 
                                                 * Replaced in place. *)
  method vexpr: exp -> exp visitAction          (* expression *)
  method vlval: lval -> lval visitAction        (* lval (base is 1st field) *)
  method voffs: offset -> offset visitAction    (* lval offset *)
  method vinst: instr -> instr list visitAction (* imperative instruction. 
                                                 * Can produce multiple 
                                                 * instruction for each input 
                                                 * instruction. *)
  method vstmt: stmt -> stmt visitAction        (* constrol-flow statement. 
                                                 * Changed in place. *)
  method vblock: block -> block visitAction     (* a block. Replaced in 
                                                 * place. *)
  method vfunc: fundec -> fundec visitAction    (* function definition. 
                                                 * Replaced in place. *)
  method vglob: global -> global visitAction    (* global (vars, types,etc.)*)
  method vinit: init -> init visitAction        (* initializers for globals *)
  method vtype: typ -> typ visitAction          (* use of some type *)
end

class nopCilVisitor: cilVisitor
class copyFunctionVisitor: string -> cilVisitor

(* other cil constructs *)
val visitCilFile: cilVisitor -> file -> file
val visitCilExpr: cilVisitor -> exp -> exp
val visitCilLval: cilVisitor -> lval -> lval
val visitCilOffset: cilVisitor -> offset -> offset
val visitCilInstr: cilVisitor -> instr -> instr list
val visitCilType: cilVisitor -> typ -> typ
val visitCilVarDecl: cilVisitor -> varinfo -> varinfo
val visitCilFunction: cilVisitor -> fundec -> fundec
val visitCilGlobal: cilVisitor -> global -> global
val visitCilInit: cilVisitor -> init -> init
val visitCilStmt: cilVisitor -> stmt -> stmt
val visitCilBlock: cilVisitor -> block -> block

(* And some generic visitors. The above are built with these *)
val doVisit: vis: cilVisitor ->
             startvisit: ('a -> 'a visitAction) ->
             children: (cilVisitor -> 'a -> 'a) ->
             node: 'a -> 'a
val doVisitList: vis: cilVisitor ->
                 startvisit: ('a -> 'a list visitAction) ->
                 children: (cilVisitor -> 'a -> 'a) ->
                 node: 'a -> 'a list

   (* Make a varinfo (for use in a TFun). Use other functions to make locals 
    * and globals *)
val makeVarinfo: string -> typ -> varinfo

  (* Make a formal variable for a function. Insert it in both the sformals 
   * and the type of the function. You can optionally specify where to insert 
   * this one. If where = "^" then it is inserted first. If where = "$" then 
   * it is inserted last. Otherwise where must be the name of a formal after 
   * which to insert this. By default it is inserted at the end. *)
val makeFormalVar: fundec -> ?where:string -> string -> typ -> varinfo

   (* Make a local variable and add it to a function's slocals (only if 
    * insert = true) *)
val makeLocalVar: fundec -> ?insert:bool -> string -> typ -> varinfo

   (* Make a temporary variable and add it to a function's slocals *)
val makeTempVar: fundec -> ?name: string -> typ -> varinfo


   (* Make a global variable. Your responsibility to make sure that the name 
    * is unique *) 
val makeGlobalVar: string -> typ -> varinfo


   (* Make an empty function *)
val emptyFunction: string -> fundec

   (* Update the formals of a fundec and make sure that the function type 
    * shares them *)
val setFormals: fundec -> varinfo list -> unit

   (* Set the types of arguments and results as given by the function type 
    * passed as the second argument *)
val setFunctionType: fundec -> typ -> unit

    (* A dummy function declaration handy for initialization *)
val dummyFunDec: fundec
val dummyFile: file

val getGlobInit: file -> fundec  (* Get the global initializer and create one 
                                  * if it does not already exist *)


(* Iterate over all globals, including the global initializer *)
val iterGlobals: file -> (global -> unit) -> unit

(* Fold over all globals, including the global initializer *)
val foldGlobals: file -> ('a -> global -> 'a) -> 'a -> 'a

(* Map over all globals, including the global initializer and change things 
 * in place *)
val mapGlobals: file -> (global -> global) -> unit

(**** Compute the type of an expression ****)
val typeOf: exp -> typ
val typeOfLval: lval -> typ
val typeOffset: typ -> offset -> typ  (* Give the base type *)



(* Some expressions to be used in case of errors *)
val dExp: Pretty.doc -> exp 
val dInstr: Pretty.doc -> location -> instr
val dGlobal: Pretty.doc -> location -> global

 (* Add an offset at the end of an lv *)      
val addOffsetLval: offset -> lval -> lval 
val addOffset:     offset -> offset -> offset



val isIntegralType: typ -> bool
val isArithmeticType: typ -> bool
val isPointerType: typ -> bool
val isFunctionType: typ -> bool
val isArrayType: typ -> bool

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
val attributeHash: (string, attributeClass) Hashtbl.t
(* Partition the attributes into classes *)
val partitionAttributes:  default:attributeClass -> 
                         attribute list -> attribute list * (* AttrName *)
                                           attribute list * (* AttrFunType *)
                                           attribute list   (* AttrType *)

(** Construct sorted lists of attributes ***)
val addAttribute: attribute -> attribute list -> attribute list
val addAttributes: attribute list -> attribute list -> attribute list
val dropAttribute: attribute list -> attribute -> attribute list


(* Retains attributes AId or ACons with the named constructor *)
val filterAttributes: string -> attribute list -> attribute list

(* true if the named attribute appears in the attribute list *)
val hasAttribute: string -> attribute list -> bool

val typeAttrs: typ -> attribute list

val setTypeAttrs: typ -> attribute list -> typ (* Resets the attributes *)


val typeAddAttributes: attribute list -> typ -> typ
val typeRemoveAttributes: attribute list -> typ -> typ

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
val typeSig: typ -> typsig
(* Like typeSig but customize the incorporation of attributes *)
val typeSigWithAttrs: (attribute list -> attribute list) -> typ -> typsig

(* Replace the attributes of a signature (only at top level) *)
val setTypeSigAttrs: attribute list -> typsig -> typsig 
(* Get the top-level attributes of a signature *)
val typeSigAttrs: typsig -> attribute list

(* Construct a cast *)
val doCastT: e:exp -> oldt:typ -> newt:typ -> exp
val doCast: e:exp -> newt:typ -> exp (* Like doCastT but use typeOf to get 
                                      * oldt *)  

(*** Make a initializer for zeroe-ing a data type ***)
val makeZeroInit: typ -> init


(* Fold over the list of initializers in a Compound. doinit is called on 
 * every present initializer, even if it is of compound type. This is much 
 * like a a List.fold_left except we also pass the type of the initializer *)
val foldLeftCompound: 
    (doinit: offset -> init -> typ -> 'a -> 'a) ->
     ct: typ ->
    initl: init list ->
    acc: 'a -> 'a


(* ALPHA conversion *)
(* Create a new name based on a given name. The new name is formed from a 
 * prefix (obtained from the given name by stripping a suffix consisting of _ 
 * followed by only digits), followed by a '_' and then by a positive integer 
 * suffix. The first argument is a table mapping name prefixes with the 
 * largest suffix used so far for that prefix. The largest suffix is one when 
 * only the version without suffix has been used.  *)
val newAlphaName: alphaTable:(string, int ref) Hashtbl.t ->
                  lookupname:string -> string
(* Split the name in preparation for newAlphaName. The prefix returned is 
 * used to inded in the hashtable. The next result value is a separator 
 * (either empty or _)  *)
val splitNameForAlpha: lookupname:string -> string * string * int
val docAlphaTable: alphaTable:(string, int ref) Hashtbl.t -> Pretty.doc

(**
 ***
 ***   OPTIMIZERS 
 ***
 ***)

    (* Process all two adjacent statements and possibly replace them both. If 
     * some replacement happens then the new statements are themselves 
     * subject to optimization  *)
val peepHole2: (instr * instr -> instr list option) -> stmt list -> unit
val peepHole1: (instr -> instr list option) -> stmt list -> unit

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
val offsetOfFieldAcc: fi: fieldinfo ->
                      sofar: offsetAcc -> offsetAcc 
        
(* The size of a type, in bits. If struct or array then trailing padding is 
 * added *)
val flagSizeOfErrors: bool ref
val bitsSizeOf: typ -> int
val sizeOf: typ -> exp
            


val offsetOf: fi:fieldinfo -> startcomp: int -> int * int


(* sm: a little optimization of my own.. 
val rewriteExprs: file -> (exp -> exp) -> (lval -> lval) -> unit
val simplifyExprs: file -> unit
*)
