(*
 * CIL: An intermediate language for analyzing C programs.
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

(* The constraints specified in this file are checked in the module Check. 
 * Take a look there for a precise specification of the invariants that a 
 * Cil.file must satisfy *)



(** This module defines the abstract syntax of CIL. It also provides utility 
    functions for traversing the CIL data structures, and pretty-printing 
    them.*)


(** The Abstract Syntax of CIL *)


(** Describes a location in a source file *)
type location = { 
    line: int;		   (** The line number. -1 means "do not know" *)
    file: string;          (** The name of the source file*)
}


(** Information about a variable. These structures are shared by all 
    references to the variable. So, you can change the name easily, for 
    example. Use one of the makeLocalVar, makeTempVar or makeGlobalVar to 
    create instances of this data structure. *)
type varinfo = { 
    mutable vname: string;		(** The name of the variable *)
    mutable vtype: typ;                 (** The declared type *)
    mutable vattr: attribute list;      (** A list of attributes *)
    mutable vstorage: storage;          (** The storage-class*)
    (* The other fields are not used in varinfo as they appear in the formal 
     * argument list in a TFun type *)


    mutable vglob: bool;	        (** True if this is a global variable*)

    mutable vdecl: location;            (** Location of variable declaration *)

    mutable vid: int;  (** A unique integer identifier. For globals this is a 
                           hash of the name. Locals are numbered from 0 
                           starting with the formal arguments. This field 
                           will be set for you if you use one of the 
                           makeFormal, makeLocalVar, makeTempVar or 
                           makeGlobalVar  *)
    mutable vaddrof: bool;              (** True if the address of this
                                            variable is taken. To ensure
                                            that this is always set, always
                                            use mkAddrOf to construct an AddrOf
                                         *)

    mutable vreferenced: bool;          (** True if this variable is ever 
                                            referenced. This is computed by 
                                            removeUnusedVars. It is safe to 
                                            just initialize this to False *)
}

(** Storage-class information *)
and storage = 
    NoStorage |                         (** The default storage *)
    Static |                            
    Register |                          
    Extern                              

(** Information about a struct/union field *)
and fieldinfo = { 
    mutable fcomp: compinfo;            (** The compinfo of the host. Note 
                                            that this must be shared with the 
                                            host since there can be only one 
                                            compinfo for a given id *)
    mutable fname: string;              (** The name of the field. Might be 
                                            the value of missingFieldName 
                                            in which 
                                            case it is not printed *)
    mutable ftype: typ;                 (** The type *)
    mutable fbitfield: int option;      (** If a bitfield then ftype should be 
                                            an integer type *)
    mutable fattr: attribute list;      
}


(** Information about a composite type (a struct or a union). Use mkCompInfo 
    to create non-recursive or (potentially) recursive versions of this. Make 
    sure you have a GCompTag for each one of these.  *)
and compinfo = {
    mutable cstruct: bool;              (** True if struct, False if union *)
    mutable cname: string;              (** The name. Always non-empty. Use 
                                            compSetName to set the name and 
                                            the key. Use compFullName to get 
                                            the full name of a comp  *)
    mutable ckey: int;                  (** A unique integer. Use Hashtbl.hash 
                                            on the string returned by
                                            compFullName. All compinfo for a
                                            given key are shared.  *)
    mutable cfields: fieldinfo list;    (** Information about the fields *) 
    mutable cattr:   attribute list;    (** The attributes that are defined at
                                            the same time as the composite
                                            type *)
    mutable creferenced: bool;          (** True if used. Initially set to false *)
  }

(** Information about an enumeration. This is shared by all references to an
    enumeration. Make sure you have a GEnumTag for each of of these.   *)
and enuminfo = {
    mutable ename: string;              (** The name. Always non-empty *)
    mutable eitems: (string * exp) list;(** Items with names and values. This
                                            list should be non-empty. The item
                                            values must be compile-time
                                            constants. *)
    mutable eattr: attribute list;     (** Attributes *)
    mutable ereferenced: bool;         (** True if used. Initially set to false*)
}

(** The various types available. Every type is associated with a list of *
    attributes, which are always kept in sorted order. Use addAttribute and
    addAttributes to construct list of attributes. *)

and typ =
    TVoid of attribute list   (** Void type *)
  | TInt of ikind * attribute list (** An integer type *)
  | TFloat of fkind * attribute list (** A floating-point type *)

  | TEnum of enuminfo * attribute list
           (** A reference to an enumeration type. All such references must
               share the enuminfo. Make sure you have a GEnumTag for each one
               of these. The attributes refer to this use of the enumeration.
               The attributes of the enumeration itself are stored inside the 
               enumeration  *)

  | TPtr of typ * attribute list        (** Pointer type. The attributes refer 
                                            to the  *)

  | TArray of typ * exp option * attribute list (** Array type. It indicates 
                                                    the base type and the 
                                                    array length *)

  | TComp of compinfo * attribute list
               (** A reference to a struct or a union type. All references to 
                   the same struct or union must share the same compinfo. Make
                   sure you have a GCompTag for each compinfo that you use. 
                   The attributes given are those pertaining to this use of 
                   the type. The attributes that were given at the definition 
                   of the type are stored in the compinfo. Always make sure 
                   there is a GTag for each structure or union that you use. *)

  | TFun of typ * varinfo list * bool * attribute list
               (** Function type. Indicates the type of the result, arguments. 
                   Also indicates if it is a variable-argument function *)

  | TNamed of string * typ * attribute list (* From a typedef. The attributes 
                                             * are in addition to the 
                                             * attributes of the named type  *)


(** Various kinds of integers *)
and ikind = 
    IChar       (** char *)
  | ISChar      (** signed char *)
  | IUChar      (** unsigned char *)
  | IInt        (** int *)
  | IUInt       (** unsigned int *)
  | IShort      (** short *)
  | IUShort     (** unsigned short *)
  | ILong       (** long *)
  | IULong      (** unsigned long *)
  | ILongLong   (** long long (or _int64 on Microsoft Visual C) *)
  | IULongLong  (** unsigned long long (or unsigned_int64 on Microsoft 
                    Visual C) *)

(** Various kinds of floating-point numbers*)
and fkind = 
    FFloat      (** float *)
  | FDouble     (** double *)
  | FLongDouble (** long double *)

(** An attribute has a name and some optional arguments *)
and attribute = Attr of string * attrarg list

(** The type of information contained in an attributes *)
and attrarg = 
    AId of string  
  | AInt of int    
  | AStr of string 
  | AVar of varinfo 
  | ACons of string * attrarg list       (** Constructed attributes *)
  | ASizeOf of typ                       (** A way to talk about types *)
  | ASizeOfE of attrarg
  | AUnOp of unop * attrarg
  | ABinOp of binop * attrarg * attrarg


(** Literal constants *)
and constant =
  | CInt64 of int64 * ikind * string option 
                 (** Integer constant. Give the ikind (see ISO9899 6.1.3.2) and
                     the textual representation, if available. Use "integer" or
                     "kinteger" to create these. Watch out for integers that
                     cannot be represented on 64 bits. OCAML does not give
                     Overflow exceptions. *)
  | CStr of string (** String constant *)
  | CChr of char   (** Character constant *)
  | CReal of float * fkind * string option (** Floating point constant. Give
                                               the fkind (see ISO 6.4.4.2) and
                                               also the textual representation,
                                               if available *)

(** Unary operations *)
and unop =
    Neg                                 (** Unary minus *)
  | BNot                                (** Bitwise complement (~) *)
  | LNot                                (** Logical Not (!) *)

(** Binary operations *)
and binop =
    PlusA                               (** arithmetic + *)
  | PlusPI                              (** pointer + integer *)
  | IndexPI                             (** pointer[integer]. The difference 
                                           form PlusPI is that in this case 
                                           the integer is very likely 
                                           positive *)
  | MinusA                              (** arithemtic - *)
  | MinusPI                             (** pointer - integer *)
  | MinusPP                             (** pointer - pointer *)
  | Mult                                (** * *)
  | Div                                 (** / *)
  | Mod                                 (** % *)
  | Shiftlt                             (** shift left *)
  | Shiftrt                             (** shift right *)

  | Lt                                  (** <  (arithmetic comparison) *)
  | Gt                                  (** >  (arithmetic comparison) *)  
  | Le                                  (** <= (arithmetic comparison) *)
  | Ge                                  (** >  (arithmetic comparison) *)
  | Eq                                  (** == (arithmetic comparison) *)
  | Ne                                  (** != (arithmetic comparison) *)            

  | LtP                                 (** <  (pointer comparison) *)
  | GtP                                 (** >  (pointer comparison) *)
  | LeP                                 (** <= (pointer comparison) *)
  | GeP                                 (** >= (pointer comparison) *)
  | EqP                                 (** == (pointer comparison) *)
  | NeP                                 (** != (pointer comparison) *)

  | BAnd                                (** bitwise and *)
  | BXor                                (** exclusive-or *)
  | BOr                                 (** inclusive-or *)


(** Expressions (Side-effect free)*)
and exp =
    Const      of constant              (** Constant *)
  | Lval       of lval                  (** Lvalue *)
  | SizeOf of typ                       (** sizeof(<type>). Has UInt type
                                            (ISO 6.5.3.4). This is not
                                            turned into a constant because some
                                            transformations might want to 
                                            change types *)

  | SizeOfE    of exp                   (** sizeof(<expression>) *)
  | AlignOf    of typ                   (** Has UInt type *)
  | AlignOfE   of exp 

                                        
  | UnOp       of unop * exp * typ      (** Unary operation. Includes 
                                            the type of the result *)

  | BinOp      of binop * exp * exp * typ
                                        (** Binary operation. Includes the 
                                            type of the result. The arithemtic
                                            conversions are made  explicit
                                            for the arguments *)
                
  | Question   of exp * exp * exp      (** e1 ? e2 : e3. Sometimes we cannot 
                                           turn this into a conditional 
                                           statement (e.g. in global 
                                           initializers). This is only allowed 
                                           inside constant initializers. In 
                                           all other places it must be turned 
                                           into IfThenElse  *)
  | CastE      of typ * exp            (** Use doCast to make casts *)

  | AddrOf     of lval                 (** Always use mkAddrOf to construct
                                           one of these. Apply to an lvalue of 
                                           type T yields an expression of type 
                                           Ptr(T) *)

  | StartOf    of lval                  (** There is no C correspondent for 
                                            this. C has implicit coercions 
                                            from an array to the address of 
                                            the first element. StartOf is used 
                                            in CIL to simplify type checking 
                                            and is just an explicit form of 
                                            the above mentioned implicit 
                                            conversion. It is not printed. 
                                            Given an lval of type TArray(T) 
                                            produces an expression of type 
                                            TPtr(T). *)

(** Initializers for global variables *)
and init = 
  | SingleInit   of exp                 (** A single initializer, might be of 
                                            compound type *)
  | CompoundInit   of typ * init list
                                        (** Used only for initializers of 
                                            structures, unions and arrays. For 
                                            a structure we have a list of 
                                            initializers for a prefix of all 
                                            fields, for a union we have one 
                                            initializer for the first field, 
                                            and for an array we have some 
                                            prefix of the initializers *)


(* L-Values denote contents of memory addresses. A memory address is 
 * expressed as a base plus an offset. The base address can be the start 
 * address of storage for a local or global variable or, in general, any 
 * pointer expression. We distinguish the two cases so that we can tell 
 * quickly whether we are accessing some component of a variable directly or 
 * we are accessing a memory location through a pointer. *)

(** Lvalues. An lvalue consists of a base and an offset *)
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
  | Var        of varinfo               (** denotes the address & v, or if v 
                                            is an array then just v *)
    (* [Var v] = (&v, typeOf(v)) *)


  | Mem        of exp                   (** denotes an address expressed as an 
                                            expression. Use mkMem to make 
                                            these  *)
    (* [Mem e] = (e, T) if typeOf(e) = Ptr(T) *)

and offset = 
  | NoOffset                            (** l *)
    (* [NoOffset](a, T) = (a, T) *)

  | Field      of fieldinfo * offset    (** l.f + offset. l must be a struct 
                                            or an union and l.f is the element 
                                            type *)
    (* [Field(f, off)](a, struct {f : T, ...}) = [off](a + offsetof(f), T) *)

  | Index    of exp * offset           (** l[e] + offset. l must be an array 
                                           and l[e] has the element type *)
    (* [Index(e, off)](a, array(T)) = [off](a + e * sizeof(T), T) *)


(* The following equivalences hold *)
(* Mem(AddrOf(Mem a, aoff)), off   = Mem a, aoff + off                *)
(* Mem(AddrOf(Var v, aoff)), off   = Var v, aoff + off                *)
(* AddrOf (Mem a, NoOffset)        = a                                *)

(** Instructions. They may cause effects directly but may not have control
    flow.*)
and instr =
    Set        of lval * exp * location  (** An assignment. A cast is present 
                                             if the exp has different type 
                                             from lval *)
  | Call       of lval option * exp * exp list * location
 			 (** optional: result is an lval. A cast might be 
                             necessary if the declared result type of the 
                             function is not the same as that of the 
                             destination. If the function is declared then 
                             casts are inserted for those arguments that 
                             correspond to declared formals. (The actual 
                             number of arguments might be smaller or larger 
                             than the declared number of arguments. C allows 
                             this.) If the type of the result variable is not 
                             the same as the declared type of the function 
                             result then an implicit cast exists.  *)

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
        (** An inline assembly instruction. The arguments are (1) templates 
            (CR-separated), (2) whether it is volatile, (3) a list of 
            outputs, each of which is an lvalue with a constraint, (4) a list 
            of input expressions along with constraints, (5) clobbered 
            registers, and (5) location information *)

(** Statements. 
    The statement is the structural unit in the control flow graph. Use mkStmt 
    to make a statement and then fill in the fields. *)
and stmt = {
    mutable labels: label list;        (** Whether the statement starts with 
                                           some labels, case statements or 
                                           default statement *)
    mutable skind: stmtkind;           (** The kind of statement *)

    (* Now some additional control flow information. Initially this is not 
     * filled in. *)
    mutable sid: int;                  (** A number (>= 0) that is unique 
                                           in a function. *)
    mutable succs: stmt list;          (** The successor statements. They can 
                                           always be computed from the skind 
                                           and the context in which this 
                                           statement appears *)
    mutable preds: stmt list;          (** The inverse of the succs function*)
  } 

(* The various kinds of statements *)
and stmtkind = 
  | Instr  of instr list               (** A group of instructions that do not 
                                           contain control flow. Control
                                           implicitly falls through. *)
  | Return of exp option * location     (** The return statement. This is a 
                                            leaf in the CFG. *)

  | Goto of stmt ref * location         (** A goto statement. Appears from 
                                            actual goto's in the code. *)
  | Break of location                   (** A break to the end of the nearest 
                                             enclosing Loop or Switch *)
  | Continue of location                (** A continue to the start of the 
                                            nearest enclosing Loop *)
  | If of exp * block * block * location (** A conditional. 
                                             Two successors, the "then" and 
                                             the "else" branches. Both 
                                             branches  fall-through to the 
                                             successor of the If statement *)
  | Switch of exp * block * (stmt list) * location  
                                       (** A switch statement. The block 
                                           contains within all of the cases. 
                                           We also have direct pointers to the 
                                           statements that implement the 
                                           cases. Which cases they implement 
                                           you can get from the labels of the 
                                           statement *)

  | Loop of block * location            (** A "while(1)" loop *)

  | Block of block                      (** Just a block of statements. Use it 
                                            as a way to keep some attributes 
                                            local *)

(** A block is a sequence of statements with the control falling through from 
    one element to the next *)
and block = 
   { mutable battrs: attribute list;  (** Attributes for the block *)
     mutable bstmts: stmt list;       (** The statements comprising the block*)
   } 

(** Labels*)
and label = 
    Label of string * location          (** A real label *)
  | Case of exp * location              (** A case statement *)
  | Default of location                 (** A default statement *)


(** Function Declarations *)
type fundec =
    { mutable svar:     varinfo;        (** Holds the name and type as a
                                            variable, so we can refer to it
                                            easily from the program *)
      mutable sformals: varinfo list;   (** Formals. These must 
                                            be shared with the formals that 
                                            appear in the type of the 
                                            function. Use setFormals or 
                                            makeFormalVar or setFunctionType 
                                            to set these formals and ensure 
                                            that they are reflected in the 
                                            function type. Do not make copies 
                                            of these because the body refers 
                                            to them. *)
      mutable slocals: varinfo list;    (** Locals. Does not include the
                                            sformals. Do not make copies of
                                            these because the body refers to
                                            them  *)
      mutable smaxid: int;              (** Max local id. Starts at 0 *)
      mutable sbody: block;             (** Body *)
      mutable sinline: bool;            (** Whether the function is inline*)
      mutable smaxstmtid: int option;  (** max id of a (reachable) statement
                                            in this function, if we have 
                                            computed it. 
                                            range = 0 ... (smaxstmtid-1) *)
    }

(* Globals *)
type global =
    GFun of fundec * location           (** A function definition. Cannot have 
                                            storage Extern *)
  | GType of string * typ * location    (** A typedef. If the string is empty 
                                            then this is printed as a 
                                            type-only declaration, useful for 
                                            introducing structure tags *)
  | GEnumTag of enuminfo * location     (** Declares an enumeration tag with 
                                            some fields. There must be one of 
                                            these for each enumeration tag 
                                            that you use since this is the 
                                            only context in which the items 
                                            are printed. *)

  | GCompTag of compinfo * location     (** Declares a struct/union tag with 
                                            some fields. There must be one of 
                                            these for each struct/union tag 
                                            that you use since this is the 
                                            only context in which the fields 
                                            are printed. *)

  | GDecl of varinfo * location         (** A variable declaration. Might be a 
                                            prototype. There might be at most 
                                            one declaration and at most one 
                                            definition for a given variable. 
                                            If both forms appear then they 
                                            must share the same varinfo. A 
                                            prototype shares the varinfo with 
                                            the fundec of the definition. 
                                            Either has storage Extern or 
                                            there must be a definition (GVar 
                                            or GFun) in this file  *)
  | GVar  of varinfo * init option * location
                                        (** A variable definition. Might have 
                                            an initializer. There must be at 
                                            most one definition for a variable 
                                            in an entire program. Cannot have 
                                            storage Extern *)
  | GAsm of string * location           (** Global asm statement. These ones 
                                            can contain only a template *)
  | GPragma of attribute * location     (** Pragmas at top level. Use the same 
                                            syntax as attributes *)
  | GText of string                     (** Some text (printed verbatim) at 
                                            top level. E.g., this way you can 
                                            put comments in the output.  *)
    
(** Files *)
type file = 
    { mutable fileName: string;   (** The complete file name *)
      mutable globals: global list; (** List of globals *)
      mutable globinit: fundec option;  (** A global initializer. It 
                                            is not part of globals 
                                            and it is printed last. 
                                            Use getGlobInit to 
                                            create/get one.  *)
      mutable globinitcalled: bool;     (** Whether the global initialization 
                                            function is called in main *)
    } 


(***** TYPES *****)
(** Manipulating types *)

(** void *)
val voidType: typ

(** int *)
val intType: typ

(** unsigned int *)
val uintType: typ

(** long *)
val longType: typ

(** unsigned long *)
val ulongType: typ

(** char *)
val charType: typ

(** char * *)
val charPtrType: typ

(** char const * *)
val charConstPtrType: typ

(** void * *)
val voidPtrType: typ

(** int * *)
val intPtrType: typ

(** unsigned int * *)
val uintPtrType: typ

(** double *)
val doubleType: typ

(** An integer type that fits pointers. We hardwire to unsigned long for now *)
val upointType: typ


(** Creates a a (potentially recursive) composite type. The arguments are: 
    (1) a boolean indicating whether it is a struct or a union, (2) the name 
    (empty for anonymous structures), (3) a function that when given a
    forward representation of the structure type constructs the type of the
    fields recursive type, and (4) a list of attributes *)
val mkCompInfo: bool ->      (* whether it is a struct or a union *)
               string ->     (* empty for anonymous structures *)
               (typ -> (string * typ * int option * attribute list) list) ->
               (* a function that when given a forward 
                  representation of the structure type constructs the type of 
                  the fields. The function can ignore this argument if not 
                  constructing a recursive type.  *)
               attribute list -> compinfo


(* This is a constant used as the name of a missing field *)
val missingFieldName: string 

(** Get the full name of a comp *)
val compFullName: compinfo -> string

(** Set the name of a composite type. Also changes the key *)
val compSetName: compinfo -> string -> unit

(** Returns true if this is a complete type. 
   This means that sizeof(t) makes sense. 
   Incomplete types are not yet defined 
   structures and empty arrays. *)
val isCompleteType: typ -> bool  

(** Unroll a type (Some attributes may be dropped)*)
val unrollType: typ -> typ   (* Might drop some attributes !! *)

(** True if the argument is an integral type (i.e. integer or enum) *)
val isIntegralType: typ -> bool

(** True if the argument is an arithmetic type (i.e. integer, enum or 
    floating point *)
val isArithmeticType: typ -> bool

(**True if the argument is a pointer type *)
val isPointerType: typ -> bool

(** True if the argument is a function type *)
val isFunctionType: typ -> bool

(** True if the argument is an array type *)
val isArrayType: typ -> bool

(** return a named fieldinfo in compinfo, or raise Not_found *)
val getCompField: compinfo -> string -> fieldinfo


(***** Type signatures ****)

(** Type signatures. Two types are identical iff they have identical 
    signatures *)
type typsig = 
    TSArray of typsig * exp option * attribute list
  | TSPtr of typsig * attribute list
  | TSComp of bool * string * attribute list
  | TSFun of typsig * typsig list * bool * attribute list
  | TSEnum of string * attribute list
  | TSBase of typ

(** Print a type signature *)
val d_typsig: unit -> typsig -> Pretty.doc

(** Compute a type signature *)
val typeSig: typ -> typsig

(** Like typeSig but customize the incorporation of attributes *)
val typeSigWithAttrs: (attribute list -> attribute list) -> typ -> typsig

(** Replace the attributes of a signature (only at top level) *)
val setTypeSigAttrs: attribute list -> typsig -> typsig 

(** Get the top-level attributes of a signature *)
val typeSigAttrs: typsig -> attribute list

(** Compute the type of an expression *)
val typeOf: exp -> typ

(** Compute the type of an lvalue *)
val typeOfLval: lval -> typ

(** Compute the type of an offset from a bast type *)
val typeOffset: typ -> offset -> typ 



(*** LVALUES ***)

(** Make a varinfo (for use in a TFun). Use other functions to make locals 
    and globals *)
val makeVarinfo: string -> typ -> varinfo

(** Make a formal variable for a function. Insert it in both the sformals 
    and the type of the function. You can optionally specify where to insert 
    this one. If where = "^" then it is inserted first. If where = "$" then 
    it is inserted last. Otherwise where must be the name of a formal after 
    which to insert this. By default it is inserted at the end. *)
val makeFormalVar: fundec -> ?where:string -> string -> typ -> varinfo

(** Make a local variable and add it to a function's slocals (only if 
    insert = true) *)
val makeLocalVar: fundec -> ?insert:bool -> string -> typ -> varinfo

(** Make a temporary variable and add it to a function's slocals *)
val makeTempVar: fundec -> ?name: string -> typ -> varinfo


(** Make a global variable. Your responsibility to make sure that the name 
    is unique *) 
val makeGlobalVar: string -> typ -> varinfo


(** Add an offset at the end of an lvalue *)
val addOffsetLval: offset -> lval -> lval 

(** Add an offset to a given offset *)
val addOffset:     offset -> offset -> offset


(***** EXPRESSIONS *****)


(* Construct integer constants *)

(* 0 *)
val zero: exp

(* 1 *)
val one: exp

(* -1 *)
val mone: exp


(** Construct an integer of a given kind, using OCaml's int64 type. If needed 
  * it will truncate the integer to be within the representable range for the 
  * given kind. *)
val kinteger64: ikind -> int64 -> exp

(** Construct an integer of a given kind. Converts the integer to int64 and 
  * then uses kinteger64. This might truncate the value if you use a kind 
  * that cannot represent the given integer. This can only happen for one of 
  * the Char or Short kinds *)
val kinteger: ikind -> int -> exp

(** Construct an integer of kind IInt. You can use this always since the 
    OCaml integers are 31 bits and are guaranteed to fit in an IInt *)
val integer: int -> exp


(** True if the given expression is a (possibly cast'ed) 
    character or an integer constant *)
val isInteger: exp -> int64 option

(** True if the expression is a compile-time constant *)
val isConstant: exp -> bool

(** True if the given expression is a (possibly cast'ed) integer or character 
    constant with value zero *)
val isZero: exp -> bool

(** Do constant folding on an expression. If the first argument is true then 
    will also compute compiler-dependent expressions such as sizeof *)    
val constFold: bool -> exp -> exp

(** Do constant folding on a binary operation. The bulk of the work done by 
    [constFold] is done here. If the first argument is true then 
    will also compute compiler-dependent expressions such as sizeof *)
val constFoldBinOp: bool -> binop -> exp -> exp -> typ -> exp

(** Increment an expression. Can be arithmetic or pointer type *) 
val increm: exp -> int -> exp


(** Makes an lvalue out of a given variable *)
val var: varinfo -> lval

(** Make an AddrOf. Given an lvalue of type T will give back an expression of 
    type ptr(T). It optimizes somewhat expressions like "& v" and "& v[0]"  *)
val mkAddrOf: lval -> exp               


(** Like mkAddrOf except if the type of lval is an array then it uses 
    StartOf. This is the right operation for getting a pointer to the start 
    of the storage denoted by lval. *)
val mkAddrOrStartOf: lval -> exp

(** Make a Mem, while optimizing AddrOf. The type of the addr must be 
    TPtr(t) and the type of the resulting lval is t. Note that in CIL the 
    implicit conversion between an array and the pointer to the first 
    element does not apply. You must do the conversion yourself using 
    StartOf *)
val mkMem: addr:exp -> off:offset -> lval

(** Make an expression that is a string constant *)
val mkString: string -> exp

(** Construct a cast *)
val doCastT: e:exp -> oldt:typ -> newt:typ -> exp

(** Like doCastT but uses typeOf to get [oldt] *)  
val doCast: e:exp -> newt:typ -> exp 

(***** STATEMENTS ****)

(** Construct a statement, given its kind. Initialize the [sid] field to -1,
    and [labels], [succs] and [preds] to the empty list *)
val mkStmt: stmtkind -> stmt

(** Construct a block with no attributes, given a list of statements *)
val mkBlock: stmt list -> block

(** Construct a statement consisting of just one instruction *)
val mkStmtOneInstr: instr -> stmt

(** Try to compress statements so as to get maximal basic blocks *)
(* use this instead of List.@ because you get fewer basic blocks *)
val compactStmts: stmt list -> stmt list

(** Returns an empty statement (of kind [Instr]) *)
val mkEmptyStmt: unit -> stmt

(** A instr to serve as a placeholder *)
val dummyInstr: instr

(** A statement consisting of just [dummyInstr] *)
val dummyStmt: stmt
  
(***** INITIALIZERS ****)


(*** Make a initializer for zero-ing a data type ***)
val makeZeroInit: typ -> init


(** Fold over the list of initializers in a Compound. [doinit] is called on 
    every present initializer, even if it is of compound type. This is much 
    like [List.fold_left] except we also pass the type of the initializer *)
val foldLeftCompound: 
    (doinit: offset -> init -> typ -> 'a -> 'a) ->
    ct: typ ->
    initl: init list ->
    acc: 'a -> 'a


(**** GLOBALS ****)


(** Make an empty function *)
val emptyFunction: string -> fundec

(** Update the formals of a [fundec] and make sure that the function type 
    shares them *)
val setFormals: fundec -> varinfo list -> unit

(** Set the types of arguments and results as given by the function type 
    passed as the second argument *)
val setFunctionType: fundec -> typ -> unit

(** A dummy function declaration handy useful for initialization *)
val dummyFunDec: fundec

(** A dummy file *)
val dummyFile: file

(** Get the global initializer and create one if it does not already exist *)
val getGlobInit: file -> fundec  

(** Iterate over all globals, including the global initializer *)
val iterGlobals: file -> (global -> unit) -> unit

(** Fold over all globals, including the global initializer *)
val foldGlobals: file -> ('a -> global -> 'a) -> 'a -> 'a

(** Map over all globals, including the global initializer and change things 
    in place *)
val mapGlobals: file -> (global -> global) -> unit

(***** ATTRIBUTES *****)

(** Various classes of attributes *)
type attributeClass = 
    AttrName of bool 
        (** Attribute of a name. If argument is true and we are on MSVC then 
            the attribute is printed using __declspec as part of the storage 
            specifier  *)
  | AttrFunType of bool 
        (** Attribute of a function type. If argument is true and we are on 
            MSVC then the attribute is printed just before the function name *)
  | AttrType  (** Attribute of a type *)

(** This table contains the mapping of predefined attributes to classes. 
    Extend this table with more attributes as you need. This table is used to 
    determine how to associate attributes with names or types *)
val attributeHash: (string, attributeClass) Hashtbl.t

(** Partition the attributes into classes *)
val partitionAttributes:  default:attributeClass -> 
                         attribute list -> attribute list * (* AttrName *)
                                           attribute list * (* AttrFunType *)
                                           attribute list   (* AttrType *)

(** Add an attribute. Maintains the attributes in sorted order *)
val addAttribute: attribute -> attribute list -> attribute list

(** Add a list of attributes. Maintains the attributes in sorted order *)
val addAttributes: attribute list -> attribute list -> attribute list

(** Remove an attribute. Maintains the attributes in sorted order *)
val dropAttribute: attribute list -> attribute -> attribute list

(** Retains attributes AId or ACons with the named constructor *)
val filterAttributes: string -> attribute list -> attribute list

(** True if the named attribute appears in the attribute list *)
val hasAttribute: string -> attribute list -> bool

(** Returns all the attributes contained in a type. This requires a traversal 
    of the type structure, in case of composite, enumeration and named types *)
val typeAttrs: typ -> attribute list

val setTypeAttrs: typ -> attribute list -> typ (* Resets the attributes *)


val typeAddAttributes: attribute list -> typ -> typ
val typeRemoveAttributes: attribute list -> typ -> typ

             

(******************
 ******************  VISITOR
 ******************)

(** Different visiting actions. 'a will be instantiated with [exp], [instr],
    etc. *)
type 'a visitAction = 
    SkipChildren                        (** Do not visit the children. Return 
                                            the node as it is *)
  | ChangeTo of 'a                      (** Replace the expression with the 
                                            given one *)
  | DoChildren                          (** Continue with the children of this 
                                            node. Rebuild the node on return 
                                            if any of the children changes 
                                            (use == test) *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a) (** First consider that the entire 
                                           exp is replaced by the first 
                                           parameter. Then continue with 
                                           the children. On return rebuild 
                                           the node if any of the children 
                                           has changed and then apply the 
                                           function on the node *)



(* sm/gn: cil visitor interface for traversing Cil trees. *)
(* Use visitCilStmt and/or visitCilFile to use this. *)
(* Some of the nodes are changed in place if the children are changed. Use 
 * one of Change... actions if you want to copy the node *)

(** A visitor interface  for traversing CIL trees *)
class type cilVisitor = object
  method vvrbl: varinfo -> varinfo visitAction  (** Variable use.  *)
  method vvdec: varinfo -> varinfo visitAction  (** Variable declaration. 
                                                    Replaced in place. *)
  method vexpr: exp -> exp visitAction          (** Expression *)
  method vlval: lval -> lval visitAction        (** Lvalue (base is the first
                                                    field) *)
  method voffs: offset -> offset visitAction    (** Lvalue offset *)
  method vinst: instr -> instr list visitAction (** Imperative instruction. 
                                                    Can produce multiple 
                                                    instructions for each 
                                                    input instruction. *)
  method vstmt: stmt -> stmt visitAction        (** Constrol-flow statement. 
                                                    Changed in place. *)
  method vblock: block -> block visitAction     (** Block. Replaced in 
                                                    place. *)
  method vfunc: fundec -> fundec visitAction    (** Function definition. 
                                                    Replaced in place. *)
  method vglob: global -> global list visitAction (** Global (vars, types,
                                                      etc.)  *)
  method vinit: init -> init visitAction        (** Initializers for globals *)
  method vtype: typ -> typ visitAction          (** Use of some type *)
  method vattr: attribute -> attribute list visitAction (* Attribute *)
end

(** Default Visitor. Traverses the CIL tree without modifying anything *)
class nopCilVisitor: cilVisitor

(** A function that makes a deep copy of a function body. Pass a new name *)
val copyFunction: fundec -> string -> fundec

(* other cil constructs *)

(** Visit a file *)
val visitCilFile: cilVisitor -> file -> file

(* Visit an expression *)
val visitCilExpr: cilVisitor -> exp -> exp

(** Visit an lvalue *)
val visitCilLval: cilVisitor -> lval -> lval

(** Visit an lvalue offset *)
val visitCilOffset: cilVisitor -> offset -> offset

(** Visit an instruction *)
val visitCilInstr: cilVisitor -> instr -> instr list

(** Visit a type *)
val visitCilType: cilVisitor -> typ -> typ

(** Visit a variable declaration *)
val visitCilVarDecl: cilVisitor -> varinfo -> varinfo

(** Visit a function declaration *)
val visitCilFunction: cilVisitor -> fundec -> fundec

(** Visit a global *)
val visitCilGlobal: cilVisitor -> global -> global list

(** Visit an initializer *)
val visitCilInit: cilVisitor -> init -> init

(** Visit a statement *)
val visitCilStmt: cilVisitor -> stmt -> stmt

(** Visit a block *)
val visitCilBlock: cilVisitor -> block -> block

(** Visit a list of attributes *)
val visitCilAttributes: cilVisitor -> attribute list -> attribute list

(* And some generic visitors. The above are built with these *)

(** Visit all the nodes in an expression *)
val doVisit: vis: cilVisitor ->
             startvisit: ('a -> 'a visitAction) ->
             children: (cilVisitor -> 'a -> 'a) ->
             node: 'a -> 'a

(** Visit every item in a list *)
val doVisitList: vis: cilVisitor ->
                 startvisit: ('a -> 'a list visitAction) ->
                 children: (cilVisitor -> 'a -> 'a) ->
                 node: 'a -> 'a list


(** A datatype to be used in conjunction with [existsType] *)
type existsAction = 
    ExistsTrue                          (* We have found it *)
  | ExistsFalse                         (* Stop processing this branch *)
  | ExistsMaybe                         (* This node is not what we are 
                                         * looking for but maybe its 
                                         * successors are *)

(** Scans a type by applying the function on all elements. Care is taken to 
    apply the function only once on each composite type, thus avoiding 
    circularity. When the function returns ExistsTrue, the scan stops with
    true *)
val existsType: (typ -> existsAction) -> typ -> bool


(** Make a while loop. Can contain Break or Continue *)
val mkWhile: guard:exp -> body:stmt list -> stmt list

(** Make a for loop for(i=start; i<past; i += incr) \{ ... \}. The body 
    can contain Break but not Continue. Can be used with i a pointer 
    or an integer. Start and done must have the same type but incr 
    must be an integer *)
val mkForIncr:  iter:varinfo -> first:exp -> stopat:exp -> incr:exp 
                 -> body:stmt list -> stmt list

(** Make a for loop for(start; guard; next) \{ ... \}. The body can 
    contain Break but not Continue !!! *) 
val mkFor: start:stmt list -> guard:exp -> next: stmt list -> 
                                       body: stmt list -> stmt list
 



(**** Utility functions ******)

(**** PRETTY PRINTING ***) 

(* location *)
(** Pretty-print a location *)
val d_loc: unit -> location -> Pretty.doc

(** Pretty-print the current location *)
val d_thisloc: unit -> Pretty.doc

(** Pretty-print an integer *)
val d_ikind: unit -> ikind -> Pretty.doc

(** Pretty-print a floating-point value *)
val d_fkind: unit -> fkind -> Pretty.doc

(** Pretty-print storage-class information *)
val d_storage: unit -> storage -> Pretty.doc

(** Pretty-print a constant *)
val d_const: unit -> constant -> Pretty.doc

  (* When we print types for consumption by another compiler we must be 
   * careful to avoid printing multiple type definitions *)
val printShortTypes: bool ref (* Prints "struct n" instead of the fields *)

(** Pretty-print a type *)
val d_type: unit -> typ -> Pretty.doc


(* exp *)

(** Pretty-print an expression *)
val d_exp: unit -> exp -> Pretty.doc

(** Pretty-print an initializer *)
val d_init: unit -> init -> Pretty.doc

(** Pretty-print a binary operator *)
val d_binop: unit -> binop -> Pretty.doc

(** Pretty-print an attribute *)
val d_attr: unit -> attribute -> Pretty.doc

(** Pretty-print an argument of an attribute *)
val d_attrarg: unit -> attrarg -> Pretty.doc

(** Pretty-print a list of attributes *)
val d_attrlist: bool -> unit -> attribute list -> Pretty.doc (* Whether it 
                                                              * comes before 
                                                              * or after 
                                                              * stuff  *) 
(** Pretty-print a statement *)
val d_stmt: unit -> stmt -> Pretty.doc

(** Pretty-print a block *)
val d_block: unit -> block -> Pretty.doc

(** Pretty-print an lvalue *)
val d_lval: unit -> lval -> Pretty.doc

(** Pretty-print an instruction *)
val d_instr: unit -> instr -> Pretty.doc

(** Pretty-print a function declaration *)
val d_fun_decl: unit -> fundec -> Pretty.doc

(** Pretty-print a variable declaration *)
val d_videcl: unit -> varinfo -> Pretty.doc

(** Pretty-print an entire file *)
val printFile: out_channel -> file -> unit

(** Use this to intercept all attributes as are printed. If your 
    function returns Some d then d is used as the external form of the 
     attribute. Otherwise the attribute is printed normally. *)
val setCustomPrintAttribute: 
    (attribute -> Pretty.doc option) -> unit

(** Like [setCustomPrintAttribute] but adds the given function to the beginning
    of the chain of custom attribute printers but only for the duration of
    executing the function passed as the second argument on the third argument
    *)
val setCustomPrintAttributeScope: 
    (attribute -> Pretty.doc option) -> ('a -> 'b) -> 'a -> 'b


(* the following error message producing functions also print a location in 
 * the code. use Errormsg.bug and Errormsg.unimp if you do not want that *)

(** Like [Errormsg.bug] except that location information is also printed *)
val bug: ('a,unit,Pretty.doc) format -> 'a

(** Like [Errormsg.unimp] except that location information is also printed *)
val unimp: ('a,unit,Pretty.doc) format -> 'a

(** Like [Errormsg.error] except that location information is also printed *)
val error: ('a,unit,Pretty.doc) format -> 'a

(** Like [error] except that it explicitly takes a location argument, instead 
    of using the current location *)
val errorLoc: location -> ('a,unit,Pretty.doc) format -> 'a  

(** Like [Errormsg.warn] except that location information is also printed *)
val warn: ('a,unit,Pretty.doc) format -> 'a

(** Like [Errormsg.warn] except that location information and context 
    is also printed *)
val warnContext: ('a,unit,Pretty.doc) format -> 'a

(** Like [warn] except that it explicitly takes a location argument, instead 
    of using the current location *)
val warnLoc: location -> ('a,unit,Pretty.doc) format -> 'a  

   (* Some plain pretty-printers. Unlike the above these expose all the 
    * details of the internal representation *)

(** Pretty-print the internal representation of an expression *)
val d_plainexp: unit -> exp -> Pretty.doc

(** Pretty-print the internal representation of an integer *)
val d_plaininit: unit -> init -> Pretty.doc

(** Pretty-print the internal representation of an lvalue *)
val d_plainlval: unit -> lval -> Pretty.doc

(** Pretty-print the internal representation of an lvalue offset *)
val d_plainoffset: unit -> offset -> Pretty.doc

(** Pretty-print the internal representation of a type *)
val d_plaintype: unit -> typ -> Pretty.doc

(** Pretty-print the internal representation of a global *)
val d_global: unit -> global -> Pretty.doc


(* ALPHA conversion *)
(** Create a new name based on a given name. The new name is formed from a 
    prefix (obtained from the given name by stripping a suffix consisting of _ 
    followed by only digits), followed by a '_' and then by a positive integer 
    suffix. The first argument is a table mapping name prefixes with the 
    largest suffix used so far for that prefix. The largest suffix is one when 
    only the version without suffix has been used.  *)
val newAlphaName: alphaTable:(string, int ref) Hashtbl.t ->
                  lookupname:string -> string

(** Split the name in preparation for newAlphaName. The prefix returned is 
    used to index into the hashtable. The next result value is a separator 
    (either empty or _)  *)
val splitNameForAlpha: lookupname:string -> string * string * int
val docAlphaTable: alphaTable:(string, int ref) Hashtbl.t -> Pretty.doc

(**
 ***
 ***   Optimization Passes
 ***
 ***)

(** A peephole optimizer that processes two adjacent statements and possibly 
    replaces them both. If some replacement happens, then the new statements 
    are themselves subject to optimization *)
val peepHole2: (instr * instr -> instr list option) -> stmt list -> unit

(** Similar to [peepHole2] except that the optimization window consists of 
    one statement, not two *)
val peepHole1: (instr -> instr list option) -> stmt list -> unit

(**
 **
 ** MACHINE DEPENDENT PART
 **
 **)

     
type offsetAcc = 
    { oaFirstFree: int;   (** The first free bit *)
      oaLastFieldStart: int;   (** Where the previous field started *)
      oaLastFieldWidth: int;   (** The width of the previous field. Might not 
                                   be same as FirstFree - FieldStart because 
                                   of internal padding *)
      oaPrevBitPack: (int * ikind * int) option; (** If the previous fields 
                                                     were packed bitfields, 
                                                     the bit where packing 
                                                     has started, the ikind 
                                                     of the bitfield and the 
                                                     width of the ikind *)
    } 
val offsetOfFieldAcc: fi: fieldinfo ->
                      sofar: offsetAcc -> offsetAcc 
(** Raised when one of the bitsSizeOf functions cannot compute the size of a 
 * type. This can happen because the type contains array-length expressions 
 * that we don't know how to compute or because it is a type whose size is 
 * not defined (e.g. TVoid or TFun)  *)        
exception SizeOfError of typ

(** The size of a type, in bits. Trailing padding is added for structs and
    arrays *)
val bitsSizeOf: typ -> int

(** The size of a type, in bytes *)
val sizeOf: typ -> exp

(** The minimum alignment (in bytes) for a type *)
val alignOf_int: typ -> int

(** Converts an offset into a number of bits from the base address and a 
   width (also expressed in bits) *)
val bitsOffset: typ -> offset -> int * int

(** Whether the pretty printer should print output for the MS VC compiler.
   Default is GCC *)
val msvcMode: bool ref               

(** Whether CHAR is unsigned. Default false *)
val charIsUnsigned: bool ref

(** Whether a signed long can fit an  unsigned integer. True only if a long
    uses more bits than an int  *)
val ilongFitsUInt: bool ref

(** Whether to print line numbers *)
val printLn: bool ref

(** Whether to print line numbers in comments*)
val printLnComment: bool ref

(** Represents a location that cannot be determined *)
val locUnknown: location

(** A reference to the current location *)
val currentLoc: location ref

(** Generate an [exp] to be used in case of errors *)
val dExp: Pretty.doc -> exp 

(** Generate an [instr] to be used in case of errors *)
val dInstr: Pretty.doc -> location -> instr

(** Generate a [global] to be used in case of errors *)
val dGlobal: Pretty.doc -> location -> global

(** Like map but try not to make a copy of the list *)
val mapNoCopy: ('a -> 'a) -> 'a list -> 'a list

(** Like map but each call can return a list. Try not to make a copy of the 
    list *)
val mapNoCopyList: ('a -> 'a list) -> 'a list -> 'a list

(** sm: return true if the first is a prefix of the second string *)
val startsWith: string -> string -> bool


