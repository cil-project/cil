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

type location = { 
    line: int;				(* -1 means "do not know" *)
    col: int;
    file: string; 
}

val locUnknown : location

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
    vglob: bool;	(* Is this a global variable? *)

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
 * to create non-recursive or (potentially) recursive versions of this  *)
and compinfo = {
    cstruct: bool;                      (* true if struct *)
    mutable cname: string;              (* the name. Always non-empty. If it 
                                         * starts with @ then it is not 
                                         * printed. Use compSetName to set 
                                         * the name and the key. Use 
                                         * compFullName to get the full name 
                                         * of a comp *)
    mutable ckey: int;                  (* A unique integer. Use Hashtbl.hash 
                                         * on the string returned by 
                                         * compFullName. All compinfo for a 
                                         * given key are shared.  *)
    mutable cfields: fieldinfo list;
    mutable cattr:   attribute list;    (* The attributes that are defined at 
                                         * the same time as the composite 
                                         * type *)
  } 
    
(* what is the type of an expression? Keep all attributes sorted. Use 
 * addAttribute and addAttributes to construct list of attributes *)
and typ =
    TVoid of attribute list
  | TInt of ikind * attribute list
  | TBitfield of ikind * int * attribute list
  | TFloat of fkind * attribute list
           (* name, tags with values, attributes. The tag list should be 
            * non-empty  *)
  | TEnum of string * (string * int) list * attribute list

  | TPtr of typ * attribute list        (* Pointer type. The attributes refer 
                                         * to the  *)

              (* base type and length *)
  | TArray of typ * exp option * attribute list

               (* Structs and Unions: isstruct, name, fields, attributes, 
                * self cell.*)
  | TComp of compinfo
               (* The field list can be empty *)
               (* The name is never empty. mkCompInfo will create a unique 
                * name for anonymous types *)


               (* Composite types can be part of circular type structure. 
                * Thus a struct and a union can be referred in a TForward. 
                * But all compinfo for a given ckey are shared!. The 
                * attributes are in addition to the attributes contained in 
                * the compinfo *)
  | TForward of compinfo * attribute list

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
                                              * in global initializers). This 
                                              * is only allowed inside 
                                              * constant initializers.!!! In 
                                              * all other places it must bne 
                                              * turned into IfThenElse *)
  | CastE      of typ * exp * location  (* Use doCast to make casts *)

                                        (* Used only for initializers of 
                                         * structures and arrays.  *) 
  | Compound   of typ * (offset option * exp) list
  | AddrOf     of lval * location       (* Alpways use mkAddrOf to construct 
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
  | Call       of varinfo option * exp * exp list * location
			 (* result temporary variable, 
                            function value, argument list, location. Casts 
                          * are inserted for arguments *)

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
      mutable sformals: varinfo list;   (* These are the formals. There are 
                                         * other formals in the type of the 
                                         * svar, but these are referred from 
                                         * the body and these are printed in 
                                         * the argument list for function 
                                         * definitions. Do not make copies of 
                                         * these because the body refers to 
                                         * them.  *)
      mutable slocals: varinfo list;    (* locals, DOES NOT include the 
                                         * sformals. Do not make copies of 
                                         * these because the body refers to 
                                         * them  *)
      mutable smaxid: int;              (* max local id. Starts at 0 *)
      mutable sbody: stmt;              (* the body *)
    } 

type global = 
    GFun of fundec                      (* A function definition. Cannot have 
                                         * storage Extern *)
  | GType of string * typ               (* A typedef *)

  | GDecl of varinfo                    (* A variable declaration. Might be a 
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
  | GVar  of varinfo * exp option       (* A variable definition. Might have 
                                         * an initializer. There must be at 
                                         * most one definition for a variable 
                                         * in an entire program. Cannot have 
                                         * storage Extern *)
  | GAsm of string                      (* Global asm statement. These ones 
                                         * can contain only a template *)
  | GPragma of attribute                (* Pragmas at top level. Use the same 
                                         * syntax as attributes *)
  | GText of string                     (* Some text (printed verbatim) at 
                                         * top level. E.g., this way you can 
                                         * put comments in the output.  *)
    

type file = 
    { mutable fileName: string;   (* the complete file name *)
      mutable globals: global list;
    } 
	(* global function decls, global variable decls *)




(* Selects the proper integer kind *)
val integerKinds: int -> 
                  possiblekinds: ikind list -> s: string option -> constant
val integer: int -> exp
val kinteger: ikind -> int -> exp
val hexinteger: int -> exp
             
val zero: exp
val one: exp
val mone: exp

val voidType: typ
val intType: typ
val uintType: typ
val longType: typ
val ulongType: typ
val charType: typ
val charPtrType: typ
val voidPtrType: typ
val intPtrType: typ
val uintPtrType: typ
val doubleType: typ

(* Generate fresh names from a prefix *)
val newTypeName: string -> string

val isCompleteType: typ -> bool  (* Returns true if this is a complete type. 
                                  * This means that sizeof(t) makes sense. 
                                  * Incomplete types are not yet defined 
                                  * structures and empty arrays. *)

(** Construct sorted lists of attributes ***)
val addAttribute: attribute -> attribute list -> attribute list
val addAttributes: attribute list -> attribute list -> attribute list
val dropAttribute: attribute list -> attribute -> attribute list

(* Retains attributes AId or ACons with the named constructor *)
val filterAttributes: string -> attribute list -> attribute list

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
               (typ -> (string * typ * attribute list) list) ->
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
val mkSet: lval -> exp -> stmt
val mkAddrOf: lval -> exp               (* Works for both arrays (in which 
                                         * case it construct a StartOf) and 
                                         * for scalars. *)
val assign: varinfo -> exp -> stmt
val call: varinfo option -> exp -> exp list -> stmt

val mkString: string -> exp

    (* Make a sequence out of a list of statements *)
val mkSeq: stmt list -> stmt


    (* Make a while loop. Can contain Break or Continue *)
val mkWhile: guard:exp -> body:stmt list -> stmt

    (* Make a for loop for(i=start; i<past; i += incr) { ... }. The body 
     * should not contain Break or Continue !!!. Can be used with i a pointer 
     * or an arithemtic type. start and done must have the same type but incr 
     * must be an integer *)
val mkForIncr:  iter:varinfo -> first:exp -> past:exp -> incr:exp 
                -> body:stmt list -> stmt

    (* Make a for loop for(start; guard; next) { ... }. The body should not 
     * contain Break or Continue !!! *) 
val mkFor: start:stmt -> guard:exp -> next:stmt -> body: stmt list -> stmt
 



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
val noRedefinitions: bool ref
val printShortTypes: bool ref (* Prints "struct n" instead of the fields *)
val d_type: unit -> typ -> Pretty.doc


(* exp *)

val d_exp: unit -> exp -> Pretty.doc
val d_binop: unit -> binop -> Pretty.doc


(* Set this function to intercept attributes as are printed. *)
val d_attrcustom: (attribute -> Pretty.doc option) ref

val d_attr: unit -> attribute -> Pretty.doc
val d_attrlist: bool -> attribute list -> Pretty.doc (* Whether it comes 
                                                      * before or after stuff 
                                                      * *)
val d_lval: unit -> lval -> Pretty.doc
val d_instr: unit -> instr -> Pretty.doc
val d_stmt: unit -> stmt -> Pretty.doc
val d_fun_decl: unit -> fundec -> Pretty.doc
val printFile: out_channel -> file -> unit
val removeUnusedTemps: file -> unit        (* sm *)


   (* Some plain pretty-printers. Unlike the above these expose all the 
    * details of the internal representation *)
val d_plainexp: unit -> exp -> Pretty.doc
val d_plainlval: unit -> lval -> Pretty.doc
val d_plainoffset: unit -> offset -> Pretty.doc
val d_plaintype: unit -> typ -> Pretty.doc

(******************
 ******************
 ******************)



 (* Scan all the expressions in a statement *)
val iterExp: (exp -> unit) -> stmt -> unit



   (* Make a local variable and add it to a function *)
val makeLocalVar: fundec -> string -> typ -> varinfo
   (* Make a temporary variable *)
val makeTempVar: fundec -> ?name: string -> typ -> varinfo


   (* Make a global variable. Your responsibility to make sure that the name 
    * is unique *) 
val makeGlobalVar: string -> typ -> varinfo


   (* Make an empty function *)
val emptyFunction: string -> fundec

    (* A dummy function declaration handy for initialization *)
val dummyFunDec: fundec
val dummyFile: file



(**** Compute the type of an expression ****)
val typeOf: exp -> typ
val typeOfLval: lval -> typ
val typeOffset: typ -> offset -> typ  (* Give the base type *)



(* Some expressions to be used in case of errors *)
val dExp: Pretty.doc -> exp 
val dStmt: Pretty.doc -> stmt

 (* Add an offset at the end of an lv *)      
val addOffsetLval: offset -> lval -> lval 
val addOffset:     offset -> offset -> offset


  (* Make a Mem, while optimizing StartOf. The type of the addr must be 
   * TPtr(t) and the type of the resulting expression is t *)
val mkMem: addr:exp -> off:offset -> exp


val isIntegralType: typ -> bool
val isArithmeticType: typ -> bool
val isPointerType: typ -> bool

val typeAttrs: typ -> attribute list

val setTypeAttrs: typ -> attribute list -> typ (* Resets the attributes *)


val typeAddAttributes: attribute list -> typ -> typ

     (* Type signatures. Two types are identical iff they have identical 
      * signatures *)
type typsig = 
    TSArray of typsig * exp option * attribute list
  | TSPtr of typsig * attribute list
  | TSComp of bool * string * attribute list
  | TSFun of typsig * (typsig * attribute list) list * bool * attribute list
  | TSBase of typ

(* Compute a type signature *)
val typeSig: typ -> typsig

(* Construct a cast *)
val doCast: e:exp -> oldt:typ -> newt:typ -> exp
  

(*** Make a compound initializer for zeroe-ing a data type ***)
val makeZeroCompoundInit: typ -> exp


(* Fold over the list of initializers in a Compound. doexp is called on every 
 * present initializer, even if it is of compound type. *)
val foldLeftCompound: 
    (doexp: offset -> exp -> typ -> 'a -> 'a) ->
     ct: typ ->
    initl: (offset option * exp) list ->
    acc: 'a -> 'a


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
val bitsSizeOf: typ -> int
val sizeOf: typ -> exp
            

 
val offsetOf: fi:fieldinfo -> startcomp: int -> int * int 
      
 
