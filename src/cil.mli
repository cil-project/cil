(*
 *
 * Copyright (c) 2001 by
 *  George C. Necula	necula@cs.berkeley.edu
 *  Scott McPeak        smcpeak@cs.berkeley.edu
 *  Wes Weimer          weimer@cs.berkeley.edu
 *   
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software for research purposes only is hereby granted, 
 * provided that the following conditions are met: 
 * 1. XSRedistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 * 3. The name of the authors may not be used to endorse or promote products 
 * derived from  this software without specific prior written permission. 
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(*
 * CIL: An intermediate language for analyzing C programs.
 *
 * George Necula
 *
 *)

(** CIL API Documentation. An html version of this document can be found at 
 * http://manju.cs.berkeley.edu/cil. *)

(** This module defines the abstract syntax of CIL. It also provides utility 
 * functions for traversing the CIL data structures, and pretty-printing 
 * them. The parser for both the GCC and MSVC front-ends can be invoked as 
 * [Frontc.parse: string -> unit ->] {!Cil.file}. This function must be given 
 * the name of a preprocessed C file and will return the top-level data 
 * structure that describes a whole source file. By default the parsing and 
 * elaboration into CIL is done as for GCC source. If you want to use MSVC 
 * source you must set the {!Cil.msvcMode} to [true] and must also invoke the 
 * function [Frontc.setMSVCMode: unit -> unit]. *)


(** {b The Abstract Syntax of CIL} *)


(** The top-level representation of a CIL source file (and the result of the 
 * parsing and elaboration). Its main contents is the list of global 
 * declarations and definitions. You can iterate over the globals in a 
 * {!Cil.file} using the following iterators: {!Cil.mapGlobals}, 
 * {!Cil.iterGlobals} and {!Cil.foldGlobals}. You can also use the 
 * {!Cil.dummyFile} when you need a {!Cil.file} as a placeholder. For each 
 * global item CIL stores the source location where it appears (using the 
 * type {!Cil.location}) *)

type file = 
    { mutable fileName: string;   (** The complete file name *)
      mutable globals: global list; (** List of globals as they will appear 
                                        in the printed file *)
      mutable globinit: fundec option;  
      (** An optional global initializer function. This is a function where 
       * you can put stuff that must be executed before the program is 
       * started. This function, is conceptually at the end of the file, 
       * although it is not part of the globals list. Use {!Cil.getGlobInit} 
       * to create/get one. *)
      mutable globinitcalled: bool;     
      (** Whether the global initialization function is called in main. This 
          should always be false if there is no global initializer. When 
          you create a global initialization CIL will try to insert code in 
          main to call it. *)
    } 
(** Top-level representation of a C souce file *)

(** {b Globals}. The main type for representing global declarations and 
 * definitions. A list of these form a CIL file. The order of globals in the 
 * file is generally important. *)

(** A global declaration or definition *)
and global =
  | GType of string * typ * location    
    (** A typedef. All uses of type names (through the [TNamed] constructor) 
        must be preceeded in the file by a definition of the name. The string 
        is the defined name. If the string is empty then this global is 
        printed as a type-only declaration, useful for introducing 
        declarations of structure tags. In turn this is useful only when your 
        file refers to pointers to such undefined structures or unions. *)

  | GCompTag of compinfo * location     
    (** Defines a struct/union tag with some fields. There must be one of 
        these for each struct/union tag that you use (through the [TComp] 
        constructor) since this is the only context in which the fields are 
        printed. Consequently nested structure tag definitions must be 
        broken into individual definitions with the innermost structure 
        defined first. *)

  | GEnumTag of enuminfo * location
   (** Declares an enumeration tag with some fields. There must be one of 
      these for each enumeration tag that you use (through the [TEnum] 
      constructor) since this is the only context in which the items are 
      printed. *)

  | GDecl of varinfo * location
   (** A variable declaration (not a definition). If the variable has a 
       function type then this is a prototype. There can be at most one 
       declaration and at most one definition for a given variable. If both 
       forms appear then they must share the same varinfo structure. A 
       prototype shares the varinfo with the fundec of the definition. Either 
       has storage Extern or there must be a definition in this file *)

  | GVar  of varinfo * init option * location
     (** A variable definition. Can have an initializer. There can be at most 
         one definition for a variable in an entire program. Cannot have 
         storage Extern or function type.  *)

  | GFun of fundec * location           
     (** A function definition. *)

  | GAsm of string * location           (** Global asm statement. These ones 
                                            can contain only a template *)
  | GPragma of attribute * location     (** Pragmas at top level. Use the same 
                                            syntax as attributes *)
  | GText of string                     (** Some text (printed verbatim) at 
                                            top level. E.g., this way you can 
                                            put comments in the output.  *)

(** {b Types}. A C type is represented in CIL using the type {!Cil.typ}. 
 * Among types we differentiate the integral types (with different kinds 
 * denoting the sign and precision), floating point types, enumeration types, 
 * array and pointer types, and function types. Every type is associated with 
 * a list of attributes, which are always kept in sorted order. Use 
 * {!Cil.addAttribute} and {!Cil.addAttributes} to construct list of 
 * attributes. If you want to inspect a type, you should use 
 * {!Cil.unrollType} to see through the uses of named types. 
*)
(** CIL is configured at build-time with the sizes and alignments of the 
 * underlying compiler (GCC or MSVC). CIL contains functions that can compute 
 * the size of a type (in bits) {!Cil.bitsSizeOf}, the alignment of a type 
 * (in bytes) {!Cil.alignOf_int}, and can convert an offset into a start and 
 * width (both in bits) using the function {!Cil.bitsOffset}. At the moment 
 * these functions do not take into account the [packed] attributes and 
 * pragmas. *)

and typ =
    TVoid of attributes   (** Void type. Also predefined as {!Cil.voidType} *)
  | TInt of ikind * attributes 
     (** An integer type. The kind specifies the sign and width. Several 
      * useful variants are predefined as {!Cil.intType}, {!Cil.uintType}, 
      * {!Cil.longType}, {!Cil.charType}. *)


  | TFloat of fkind * attributes 
     (** A floating-point type. The kind specifies the precision. You can 
      * also use the predefined constant {!Cil.doubleType}. *)

  | TPtr of typ * attributes  
           (** Pointer type. Several useful variants are predefined as 
            * {!Cil.charPtrType}, {!Cil.charConstPtrType} (pointer to a 
            * constant character), {!Cil.voidPtrType}, 
            * {!Cil.intPtrType}  *)

  | TArray of typ * exp option * attributes
           (** Array type. It indicates the base type and the array length. *)

  | TFun of typ * varinfo list option * bool * attributes
          (** Function type. Indicates the type of the result, the formal 
           * arguments ([None] if no arguments were specified, as in a 
           * function whose definition or prototype we have not seen; [Some 
           * \[\]] means void). Use {!Cil.argsToList} to obtain a list of 
           * arguments. The boolean indicates if it is a variable-argument 
           * function. *)

  | TNamed of string * typ * attributes 
          (* The use of a named type. Each such type name must be preceeded 
           * in the file by a [GType] global. This is printed as just the 
           * type name. The actual referred type is not printed here and is 
           * carried only to simplify processing. To see through a sequence 
           * of named type references, use {!Cil.unrollType}. The attributes 
           * are in addition to those given when the type name was defined. *)

  | TComp of compinfo * attributes
(** The most delicate issue for C types is that recursion that is possible by 
 * using structures and pointers. To address this issue we have a more 
 * complex representation for structured types (struct and union). Each such 
 * type is represented using the {!Cil.compinfo} type. For each composite 
 * type the {!Cil.compinfo} structure must be declared at top level using 
 * [GCompTag] and all references to it must share the same copy of the 
 * structure. The attributes given are those pertaining to this use of the 
 * type and are in addition to the attributes that were given at the 
 * definition of the type and which are stored in the {!Cil.compinfo}. *)

  | TEnum of enuminfo * attributes
           (** A reference to an enumeration type. All such references must
               share the enuminfo among them and with a [GEnumTag] global that 
               preceeds all uses. The attributes refer to this use of the 
               enumeration and are in addition to the attributes of the 
               enumeration itself, which are stored inside the enuminfo  *)

  
  | TBuiltin_va_list of attributes
            (** This is the same as the gcc's type with the same name *)

(**
 There are a number of functions for querying the kind of a type. These are
 {!Cil.isIntegralType}, 
 {!Cil.isArithmeticType}, 
 {!Cil.isPointerType}, 
 {!Cil.isFunctionType}, 
 {!Cil.isArrayType}. 

 There are two easy ways to scan a type. First, you can use the
{!Cil.existsType} to return a boolean answer about a type. This function
is controled by a user-provided function that is queried for each type that is
used to construct the current type. The function can specify whether to
terminate the scan with a boolean result or to continue the scan for the
nested types. 

 The other method for scanning types is provided by the visitor interface (see
 {!Cil.cilVisitor}).

 If you want to compare types (or to use them as hash-values) then you should
use instead type signatures (represented as {!Cil.typsig}). These
contain the same information as types but canonicalized such that simple Ocaml
structural equality will tell whether two types are equal. Use
{!Cil.typeSig} to compute the signature of a type. If you want to ignore
certain type attributes then use {!Cil.typeSigWithAttrs}. 

*)


(** Various kinds of integers *)
and ikind = 
    IChar       (** [char] *)
  | ISChar      (** [signed char] *)
  | IUChar      (** [unsigned char] *)
  | IInt        (** [int] *)
  | IUInt       (** [unsigned int] *)
  | IShort      (** [short] *)
  | IUShort     (** [unsigned short] *)
  | ILong       (** [long] *)
  | IULong      (** [unsigned long] *)
  | ILongLong   (** [long long] (or [_int64] on Microsoft Visual C) *)
  | IULongLong  (** [unsigned long long] (or [unsigned _int64] on Microsoft 
                    Visual C) *)

(** Various kinds of floating-point numbers*)
and fkind = 
    FFloat      (** [float] *)
  | FDouble     (** [double] *)
  | FLongDouble (** [long double] *)

(** {b Attributes.} *)

and attribute = Attr of string * attrparam list
(** An attribute has a name and some optional parameters. The name should not 
 * start or end with underscore. When CIL parses attribute names it will 
 * strip leading and ending underscores (to ensure that the multitude of GCC 
 * attributes such as const, __const and __const__ all mean the same thing.) *)

(** Attributes are lists sorted by the attribute name. Use the functions 
 * {!Cil.addAttribute} and {!Cil.addAttributes} to insert attributes in an 
 * attribute list and maintain the sortedness. *)
and attributes = attribute list

(** The type of parameters of attributes *)
and attrparam = 
  | AInt of int                          (** An integer constant *)
  | AStr of string                       (** A string constant *)
  | ACons of string * attrparam list       (** Constructed attributes. These 
                                             are printed [foo(a1,a2,...,an)]. 
                                             The list of parameters can be 
                                             empty and in that case the 
                                             parentheses are not printed. *)
  | ASizeOf of typ                       (** A way to talk about types *)
  | ASizeOfE of attrparam
  | AUnOp of unop * attrparam
  | ABinOp of binop * attrparam * attrparam


(** {b Structures.} The {!Cil.compinfo} describes the definition of a 
 * structure or union type. Each such {!Cil.compinfo} must be defined at the 
 * top-level using the [GCompTag] constructor and must be shared by all 
 * references to this type (using either the [TComp] type constructor or from 
 * the definition of the fields. 

   If all you need is to scan the definition of each 
 * composite type once, you can do that by scanning all top-level [GCompTag]. 

 * Constructing a {!Cil.compinfo} can be tricky since it must contain fields 
 * that might refer to the host {!Cil.compinfo} and furthermore the type of 
 * the field might need to refer to the {!Cil.compinfo} for recursive types. 
 * Use the {!Cil.mkCompInfo} function to create a {!Cil.compinfo}. You can 
 * easily fetch the {!Cil.fieldinfo} for a given field in a structure with 
 * {!Cil.getCompField}. *)

(** The definition of a structure or union type. *)
and compinfo = {
    mutable cstruct: bool;              
   (** True if struct, False if union *)
    mutable cname: string;              
   (** The name. Always non-empty. Use {!Cil.compSetName} to set the name and 
    * the key simultaneously. Use {!Cil.compFullName} to get the full name of 
    * a comp (along with the struct or union) *)
    mutable ckey: int;                  
    (** A unique integer constructed from the name. Use {!Hashtbl.hash} on 
     * the string returned by {!Cil.compFullName}, or better yet use 
     * {!Cil.compSetName}. All compinfo for a given key are shared. *)
    mutable cfields: fieldinfo list;    
    (** Information about the fields *) 
    mutable cattr:   attributes;        
    (** The attributes that are defined at the same time as the composite 
     * type. These attributes can be supplemented individually at each 
     * reference to this [compinfo] using the [TComp] type constructor. *)
    mutable creferenced: bool;          
    (** True if used. Initially set to false. *)
  }

(** {b Structure fields.} The {!Cil.fieldinfo} structure is used to describe 
 * a structure or union field. Fields, just like variables, can have 
 * attributes asociated with the field itself or associated with the type of 
 * the field (stored along with the type of the field). *)

(** Information about a struct/union field *)
and fieldinfo = { 
    mutable fcomp: compinfo;            
     (** The host structure that contains this field. There can be only one 
      * [compinfo] that contains the field. *)
    mutable fname: string;              
    (** The name of the field. Might be the value of {!Cil.missingFieldName} 
     * in which case it must be a bitfield and is not printed and it does not 
     * participate in initialization *)
    mutable ftype: typ;     
    (** The type *)
    mutable fbitfield: int option;      
    (** If a bitfield then ftype should be an integer type and the width of 
     * the bitfield must be 0 or a positive integer smaller or equal to the 
     * width of the integer type. A field of width 0 is used in C to control 
     * the alignment of fields. *)
    mutable fattr: attributes;          
    (** The attributes for this field (not for its type) *)
}



(** {b Enumerations.} Information about an enumeration. This is shared by all 
 * references to an enumeration. Make sure you have a [GEnumTag] for each of 
 * of these. *)

(** Information about an enumeration *)
and enuminfo = {
    mutable ename: string;              
    (** The name. Always non-empty. *)
    mutable eitems: (string * exp) list;
    (** Items with names and values. This list should be non-empty. The item 
     * values must be compile-time constants. *)
    mutable eattr: attributes;         
    (** The attributes that are defined at the same time as the enumeration 
     * type. These attributes can be supplemented individually at each 
     * reference to this [enuminfo] using the [TEnum] type constructor. *)
    mutable ereferenced: bool;         
    (** True if used. Initially set to false*)
}

(** {b Variables.} 
 Each local or global variable is represented by a unique {!Cil.varinfo}
structure. A global {!Cil.varinfo} can be introduced with the [GDecl] or
[GVar] or [GFun] globals. A local varinfo can be introduced as part of a
function definition. 

 All references to a given global or local variable must refer to the same
copy of the [varinfo]. Each [varinfo] has a unique identifier that can be use
to index maps and hashtables. The identifier for globals is constructed by
hashing the name and the identifier for locals is an integer between 0 and the
number of locals in the function. 

 It is very important that you construct [varinfo] structures using only one
 of the following functions:
- {!Cil.makeGlobalVar} : to make a global variable
- {!Cil.makeTempVar} : to make a temporary local variable whose name
will be generated so that to avoid conflict with other locals. 
- {!Cil.makeLocalVar} : like {!Cil.makeTempVar} but you can specify the
exact name to be used. 
- {!Cil.makeFormalVar} : make a new formal argument. This is added
both to the formal arguments and to the locals of the host function. 


 A [varinfo] is also used in a function type to denote the list of formals. 

*)

(** Information about a variable. *)
and varinfo = { 
    mutable vname: string;		
    (** The name of the variable. Cannot be empty. It is primarily your 
     * responsibility to ensure the uniqueness of a variable name. For local 
     * variables {!Cil.makeTempVar} helps you ensure that the name is unique. 
     *)

    mutable vtype: typ;                 
    (** The declared type of the variable. *)

    mutable vattr: attributes;          
    (** A list of attributes associated with the variable.*)
    mutable vstorage: storage;          
    (** The storage-class *)

    mutable vglob: bool;	        
    (** True if this is a global variable*)

    mutable vdecl: location;            
    (** Location of variable declaration. Not yet implemented. *)

    mutable vid: int;  
    (** A unique integer identifier. For globals this is a hash of the name. 
     * Locals are numbered from 0 starting with the formal arguments. This 
     * field will be set for you if you use one of the {!Cil.makeFormalVar}, 
     * {!Cil.makeLocalVar}, {!Cil.makeTempVar} or {!Cil.makeGlobalVar}. *)

    mutable vaddrof: bool;              
    (** True if the address of this variable is taken. CIL will set these 
     * flags when it parses C, but you should make sure to set the flag 
     * whenever your transformation create [AddrOf] expression. *)

    mutable vreferenced: bool;          
    (** True if this variable is ever referenced. This is computed by 
     * [removeUnusedVars]. It is safe to just initialize this to False *)
}

(** Storage-class information *)
and storage = 
    NoStorage |                         (** The default storage. Nothing is 
                                         * printed  *)
    Static |                           
    Register |                          
    Extern                              


(** {b Expressions.} The CIL expression language contains only the side-effect free expressions of
C. They are represented as the type {!Cil.exp}. There are several
interesting aspects of CIL expressions: 

 Integer and floating point constants can carry their textual representation.
This way the integer 15 can be printed as 0xF if that is how it occured in the
source. 

 CIL uses 64 bits to represent the integer constants and also stores the width
of the integer type. Care must be taken to ensure that the constant is
representable with the given width. Use the functions {!Cil.kinteger},
{!Cil.kinteger64} and {!Cil.integer} to construct constant
expressions. CIL predefines the constants {!Cil.zero},
{!Cil.one} and {!Cil.mone} (for -1). 

 Use the functions {!Cil.isConstant} and {!Cil.isInteger} to test if
an expression is a constant and a constant integer respectively.

 CIL keeps the type of all unary and binary expressions. You can think of that
type qualifying the operator. Furthermore there are different operators for
arithmetic and comparisons on arithemtic types and on pointers. 

 Another unusual aspect of CIL is that the implicit convertion between an
expression of array type and one of pointer type is made explicit, using the
[StartOf] expression constructor (which is not printed). If you apply the
[AddrOf}]constructor to an lvalue of type [T] then you will be getting an
expression of type [TPtr(T)].

 You can find the type of an expression with {!Cil.typeOf}. 

 You can perform constant folding on expressions using the function
{!Cil.constFold}. 
*)

(** Expressions (Side-effect free)*)
and exp =
    Const      of constant              (** Constant *)
  | Lval       of lval                  (** Lvalue *)
  | SizeOf     of typ                   
    (** sizeof(<type>). Has [unsigned int] type (ISO 6.5.3.4). This is not 
     * turned into a constant because some transformations might want to 
     * change types *)

  | SizeOfE    of exp                   
    (** sizeof(<expression>) *)
  | AlignOf    of typ                   
    (** This corresponds to the GCC __alignof_. Has [unsigned int] type *)
  | AlignOfE   of exp 

                                        
  | UnOp       of unop * exp * typ     
    (** Unary operation. Includes the type of the result. *)

  | BinOp      of binop * exp * exp * typ
    (** Binary operation. Includes the type of the result. The arithemtic 
     * conversions are made explicit for the arguments. *)

  | CastE      of typ * exp            
    (** Use {!Cil.mkCast} to make casts.  *)

  | AddrOf     of lval                 
    (** Always use {!Cil.mkAddrOf} to construct one of these. Apply to an 
     * lvalue of type [T] yields an expression of type [TPtr(T)] *)

  | StartOf    of lval   
    (** Conversion from an array to a pointer to the beginning of the array. 
     * Given an lval of type [TArray(T)] produces an expression of type 
     * [TPtr(T)]. In C this operation is implicit, the [StartOf] operator is 
     * not printed. We have it in CIL because it makes the typing rules 
     * simpler. *)

(** {b Constants.} *)

(** Literal constants *)
and constant =
  | CInt64 of int64 * ikind * string option 
    (** Integer constant. Give the ikind (see ISO9899 6.1.3.2) and the 
     * textual representation, if available. (This allows us to print a 
     * constant as, for example, 0xF instead of 15.) Use {!Cil.integer} or 
     * {!Cil.kinteger} to create these. Watch out for integers that cannot be 
     * represented on 64 bits. OCAML does not give Overflow exceptions. *)
  | CStr of string 
    (** String constant. The escape characters inside the string have been 
     * alredy interpreted. *)
  | CChr of char   
    (** Character constant *)
  | CReal of float * fkind * string option 
     (** Floating point constant. Give the fkind (see ISO 6.4.4.2) and also 
      * the textual representation, if available. *)

(** Unary operators *)
and unop =
    Neg                                 (** Unary minus *)
  | BNot                                (** Bitwise complement (~) *)
  | LNot                                (** Logical Not (!) *)

(** Binary operations *)
and binop =
    PlusA                               (** arithmetic + *)
  | PlusPI                              (** pointer + integer *)
  | IndexPI                             (** pointer + integer but only when 
                                         * it arises from an expression 
                                         * [e\[i\]] when [e] is a pointer and 
                                         * not an array. This is semantically 
                                         * the same as PlusPI but CCured uses 
                                         * this as a hint that the integer is 
                                         * probably positive. *)
  | MinusA                              (** arithmetic - *)
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

(** {b Lvalues.} Lvalues are the sublanguage of expressions that can appear at the left of an assignment or as operand to the address-of operator. 
In C the sytax for lvalues is not always a good indication of the meaning 
of the lvalue. For example the C value
{v  
a[0][1][2]
 v}
 might involve 1, 2 or 3 memory reads when used in an expression context,
depending on the declared type of the variable [a]. If [a] has type [int
\[4\]\[4\]\[4\]] then we have one memory read from somewhere inside the area 
that stores the array [a]. On the other hand if [a] has type [int ***] then
the expression really means [* ( * ( * (a + 0) + 1) + 2)], in which case it is
clear that it involves three separate memory operations. 

An lvalue denotes the contents of a range of memory addresses. This range 
is denoted as a host object along with an offset within the object. The 
host object can be of two kinds: a local or global variable, or an object 
whose address is in a pointer expression. We distinguish the two cases so 
that we can tell quickly whether we are accessing some component of a 
variable directly or we are accessing a memory location through a pointer.
To make it easy to 
tell what an lvalue means CIL represents lvalues as a host object and an
offset (see {!Cil.lval}). The host object (represented as
{!Cil.lhost}) can be a local or global variable or can be the object
pointed-to by a pointer expression. The offset (represented as
{!Cil.offset}) is a sequence of field or array index designators.

 Both the typing rules and the meaning of an lvalue is very precisely
specified in CIL. 

 The following are a few useful function for operating on lvalues:
- {!Cil.mkMem} - makes an lvalue of [Mem] kind. Use this to ensure
that certain equivalent forms of lvalues are canonized. 
For example, [*&x = x]. 
- {!Cil.typeOfLval} - the type of an lvalue
- {!Cil.typeOffset} - the type of an offset, given the type of the
host. 
- {!Cil.addOffset} and {!Cil.addOffsetLval} - entend sequences
of offsets.

The following equivalences hold {v 
Mem(AddrOf(Mem a, aoff)), off   = Mem a, aoff + off 
Mem(AddrOf(Var v, aoff)), off   = Var v, aoff + off 
AddrOf (Mem a, NoOffset)        = a                 
 v}

*)
(** An lvalue *)
and lval =
    lhost * offset

(** The host part of an {!Cil.lval}. *)
and lhost = 
  | Var        of varinfo    
    (** The host is a variable. *)

  | Mem        of exp        
    (** The host is an object of type [T] when the expression has pointer 
     * [TPtr(T)]. *)


(** The offset part of an {!Cil.lval}. Each offset can be applied to certain 
  * kinds of lvalues and its effect is that it advances the starting address 
  * of the lvalue and changes the denoted type, essentially focussing to some 
  * smaller lvalue that is contained in the original one. *)
and offset = 
  | NoOffset          (** No offset. Can be applied to any lvalue and does 
                        * not change either the starting address or the type. 
                        * This is used when the lval consists of just a host 
                        * or as a terminator in a list of other kinds of 
                        * offsets. *)

  | Field      of fieldinfo * offset    
                      (** A field offset. Can be applied only to an lvalue 
                       * that denotes a structure or a union that contains 
                       * the mentioned field. This advances the offset to the 
                       * beginning of the mentioned field and changes the 
                       * type to the type of the mentioned field. *)

  | Index    of exp * offset
                     (** An array index offset. Can be applied only to an 
                       * lvalue that denotes an array. This advances the 
                       * starting address of the lval to the beginning of the 
                       * mentioned array element and changes the denoted type 
                       * to be the type of the array element *)


(** {b Initializers.} 
A special kind of expressions are those that can appear as initializers for
global variables (initialization of local variables is turned into
assignments). The initializers are represented as type {!Cil.init}. You
can create initializers with {!Cil.makeZeroInit} and you can conveniently
scan compound initializers them with {!Cil.foldLeftCompound} or with {!Cil.foldLeftCompoundAll}. 
*)
(** Initializers for global variables. *)
and init = 
  | SingleInit   of exp   (** A single initializer *)
  | CompoundInit   of typ * (offset * init) list
    (** Used only for initializers of structures, unions and arrays. The 
     * offsets are all of the form [Field(f, NoOffset)] or [Index(i, 
     * NoOffset)] and specify the field or the index being initialized. For 
     * structures and arrays all fields (indices) must have an initializer 
     * (except the unnamed bitfields), in the proper order. This is necessary 
     * since the offsets are not printed. For unions there must be exactly 
     * one initializer. If the initializer is not for the first field then a 
     * field designator is printed, so you better be on GCC since MSVC does 
     * not understand this. You can scan an initializer list with 
     * {!Cil.foldLeftCompound} or with {!Cil.foldLeftCompoundAll}. *)

(** {b Function definitions.} 
A function definition is always introduced with a [GFun] constructor at the
top level. All the information about the function is stored into a
{!Cil.fundec}. Some of the information (e.g. its name, type,
storage, attributes) is stored as a {!Cil.varinfo} that is a field of the
[fundec]. To refer to the function from the expression language you must use
the [varinfo]. 

 The function definition contains, in addition to the body, a list of all the
local variables and separately a list of the formals. Both kind of variables
can be refered to in the body of the function. The formals must also be shared
with the formals that appear in the function type. For that reason, to
manipulate formals you should use the provided functions
{!Cil.makeFormalVar} and {!Cil.setFormals}. 
*)
(** Function definitions. *)
and fundec =
    { mutable svar:     varinfo;        
         (** Holds the name and type as a variable, so we can refer to it 
          * easily from the program. All references to this function either 
          * in a function call or in a prototype must point to the same 
          * [varinfo]. *)
      mutable sformals: varinfo list;   
        (** Formals. These must be shared with the formals that appear in the 
         * type of the function. Use {!Cil.setFormals} or 
         * {!Cil.makeFormalVar} or {!Cil.setFunctionType} to set these 
         * formals and ensure that they are reflected in the function type. 
         * Do not make copies of these because the body refers to them. *)
      mutable slocals: varinfo list;    
        (** Locals. Does not include the sformals. Do not make copies of 
         * these because the body refers to them. *)
      mutable smaxid: int;           (** Max local id. Starts at 0 *)
      mutable sbody: block;          (** The function body. *)
      mutable sinline: bool;         (** Whether the function is inline *)
      mutable smaxstmtid: int option;  (** max id of a (reachable) statement 
                                        * in this function, if we have 
                                        * computed it. range = 0 ... 
                                        * (smaxstmtid-1) *)
    }


(** A block is a sequence of statements with the control falling through from 
    one element to the next *)
and block = 
   { mutable battrs: attributes;      (** Attributes for the block *)
     mutable bstmts: stmt list;       (** The statements comprising the block*)
   } 


(** {b Statements}. 
CIL statements are the strutural elements that make the CFG. They are 
represented using the type {!Cil.stmt}. Every
statement has a (possibly empty) list of labels. The
{!Cil.stmtkind} field of a statement indicates what kind of statement it 
is.

 Use {!Cil.mkStmt} to make a statement and the fill-in the fields. 

CIL also comes with support for control-flow graphs. The [sid] field in
[stmt] can be
used to give unique numbers to statements, and the [succs] and [preds]
fields can be used to maintain a list of successors and predeccors for every
statement. The CFG information is not computed by default. Instead you must 
explicitly use the function {!Cil.computeCFGInfo} to do it.

*)
(** Statements. *)
and stmt = {
    mutable labels: label list;        
    (** Whether the statement starts with some labels, case statements or 
     * default statements. *)

    mutable skind: stmtkind;           
    (** The kind of statement *)

    mutable sid: int;                  
    (** A number (>= 0) that is unique in a function. Filled in only after 
     * the CFG is computed. *)
    mutable succs: stmt list;          
    (** The successor statements. They can always be computed from the skind 
     * and the context in which this statement appears. Filled in only after 
     * the CFG is computed. *)
    mutable preds: stmt list;          
    (** The inverse of the succs function. *)
  } 

(** Labels *)
and label = 
    Label of string * location * bool   
          (** A real label. If the bool is "true", the label is from the 
           * input source program. If the bool is "false", the label was 
           * created by CIL or some other transformation *)
  | Case of exp * location              (** A case statement *)
  | Default of location                 (** A default statement *)



(** The various kinds of control-flow statements statements *)
and stmtkind = 
  | Instr  of instr list               
  (** A group of instructions that do not contain control flow. Control 
   * implicitly falls through. *)

  | Return of exp option * location     
   (** The return statement. This is a leaf in the CFG. *)

  | Goto of stmt ref * location         
   (** A goto statement. Appears from actual goto's in the code or from 
    * goto's that have been inserted during elaboration. The reference 
    * points to the statement that is the target of the Goto. This means that 
    * you have to update the reference whenever you replace the target 
    * statement. *)

  | Break of location                   
   (** A break to the end of the nearest enclosing Loop or Switch *)

  | Continue of location                
   (** A continue to the start of the nearest enclosing [Loop] *)
  | If of exp * block * block * location 
   (** A conditional. Two successors, the "then" and the "else" branches. 
    * Both branches fall-through to the successor of the If statement. *)

  | Switch of exp * block * (stmt list) * location  
   (** A switch statement. The statements that implement the cases can be 
    * reached through the provided list. For each such target you can find 
    * among its labels what cases it implements. The statements that 
    * implement the cases are somewhere within the provided [block]. *)

  | Loop of block * location            
    (** A [while(1)] loop. The termination test is implemented in the body of 
     * a loop using a [Break] statement *)

  | Block of block                      
    (** Just a block of statements. Use it as a way to keep some block 
     * attributes local *)
    
(** {b Instructions}. 
 An instruction {!Cil.instr} is a statement that has no local
(intraprocedural) control flow. It can be either an assignment,
function call, or an inline assembly instruction. *)

(** Instructions. *)
and instr =
    Set        of lval * exp * location  
   (** An assignment. The type of the expression is guaranteed to be the same 
    * with that of the lvalue *)
  | Call       of lval option * exp * exp list * location
   (** A function call with the (optional) result placed in an lval. It is 
    * possible that the returned type of the function is not identical to 
    * that of the lvalue. In that case a cast is printed. The type of the 
    * actual arguments are identical to those of the declared formals. The 
    * number of arguments is the same as that of the declared formals, except 
    * for vararg functions. This construct is also used to encode a call to 
    * "__builtin_va_arg". In this case the second argument (which should be a 
    * type T) is encoded SizeOf(T) *)

  | Asm        of attributes * (* Really only const and volatile can appear 
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
    (** There are for storing inline assembly. They follow the GCC 
      * specfication: 
{v 
  asm [volatile] ("...template..." "..template.."
                  : "c1" (o1), "c2" (o2), ..., "cN" (oN)
                  : "d1" (i1), "d2" (i2), ..., "dM" (iM)
                  : "r1", "r2", ..., "nL" );
 v}

where the parts are

  - [volatile] (optional): when present, the assembler instruction
    cannot be removed, moved, or otherwise optimized
  - template: a sequence of strings, with %0, %1, %2, etc. in the string to 
    refer to the input and output expressions. I think they're numbered
    consecutively, but the docs don't specify. Each string is printed on 
    a separate line. This is the only part that is present for MSVC inline
    assembly.
  - "ci" (oi): pairs of constraint-string and output-lval; the 
    constraint specifies that the register used must have some
    property, like being a floating-point register; the constraint
    string for outputs also has "=" to indicate it is written, or
    "+" to indicate it is both read and written; 'oi' is the
    name of a C lvalue (probably a variable name) to be used as
    the output destination
  - "dj" (ij): pairs of constraint and input expression; the constraint
    is similar to the "ci"s.  the 'ij' is an arbitrary C expression
    to be loaded into the corresponding register
  - "rk": registers to be regarded as "clobbered" by the instruction;
    "memory" may be specified for arbitrary memory effects

an example (from gcc manual):
{v 
  asm volatile ("movc3 %0,%1,%2"
                : /* no outputs */
                : "g" (from), "g" (to), "g" (count)
                : "r0", "r1", "r2", "r3", "r4", "r5");
 v}
*)

(** Describes a location in a source file *)
and location = { 
    line: int;		   (** The line number. -1 means "do not know" *)
    file: string;          (** The name of the source file*)
}


(** {b Values for manipulating globals} *)

(** Make an empty function *)
val emptyFunction: string -> fundec

(** Update the formals of a [fundec] and make sure that the function type 
    shares them *)
val setFormals: fundec -> varinfo list -> unit

(** Set the types of arguments and results as given by the function type 
    passed as the second argument *)
val setFunctionType: fundec -> typ -> unit

(** A dummy function declaration handy when you need one as a placeholder. It 
 * contains inside a dummy varinfo. *)
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

(** Compute the CFG information for all statements in a fundec and return a 
  * list of the statements *)
val computeCFGInfo: fundec -> stmt list


(** Create a deep copy of a function. There shoudl be no sharing between the 
 * copy and the riginal function *)
val copyFunction: fundec -> string -> fundec 

(** {b Values for manipulating initializers} *)


(** Make a initializer for zero-ing a data type ***)
val makeZeroInit: typ -> init


(** Fold over the list of initializers in a Compound. [doinit] is called on 
 * every present initializer, even if it is of compound type. In the case of 
 * arrays there might be missing zero-initializers at the end of the list. 
 * These are not scanned. This is much like [List.fold_left] except we also 
 * pass the type of the initializer *)
val foldLeftCompound: 
    doinit: (offset -> init -> typ -> 'a -> 'a) ->
    ct: typ ->
    initl: (offset * init) list ->
    acc: 'a -> 'a


(** Fold over the list of initializers in a Compound, like 
 * {!Cil.foldLeftCompound} but in the case of an array it scans even missing 
 * zero initializers at the end of the array *)
val foldLeftCompoundAll: 
    doinit: (offset -> init -> typ -> 'a -> 'a) ->
    ct: typ ->
    initl: (offset * init) list ->
    acc: 'a -> 'a



(** {b Values for manipulating types} *****)

(** void *)
val voidType: typ

(* is the given type "void"? *)
val isVoidType : typ -> bool

(* is the given type "void *"? *)
val isVoidPtrType : typ -> bool

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

(** wchar_t *)
val wcharType: typ

(** wchar_t * *)
val wcharPtrType: typ

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
    (always non-empty), (3) a function that when given a
    representation of the structure type constructs the type of the
    fields recursive type (the first argument is only useful when some fields 
   need to refer to the type of the structure itself), 
   and (4) a list of attributes to be associated with the composite type. *)
val mkCompInfo: bool ->      (* whether it is a struct or a union *)
               string ->     (* empty for anonymous structures *)
               (compinfo -> (string * typ * int option * attributes) list) ->
               (* a function that when given a forward 
                  representation of the structure type constructs the type of 
                  the fields. The function can ignore this argument if not 
                  constructing a recursive type.  *)
               attributes -> compinfo


(** This is a constant used as the name of an unnamed bitfield. These fields
    do not participate in initialization and their name si not printed. *)
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

(** Obtain the argument list ([] if None) *)
val argsToList: varinfo list option -> varinfo list

(** True if the argument is an array type *)
val isArrayType: typ -> bool

(** Return a named fieldinfo in compinfo, or raise Not_found *)
val getCompField: compinfo -> string -> fieldinfo


(** A datatype to be used in conjunction with [existsType] *)
type existsAction = 
    ExistsTrue                          (* We have found it *)
  | ExistsFalse                         (* Stop processing this branch *)
  | ExistsMaybe                         (* This node is not what we are 
                                         * looking for but maybe its 
                                         * successors are *)

(** Scans a type by applying the function on all elements. 
    When the function returns ExistsTrue, the scan stops with
    true. When the function returns ExistsFalse then the current branch is not
    scaned anymore. Care is taken to 
    apply the function only once on each composite type, thus avoiding 
    circularity. When the function returns ExistsMaybe then the types that 
    construct the current type are scanned (e.g. the base type for TPtr and 
    TArray, the type of fields for a TComp, etc). *)
val existsType: (typ -> existsAction) -> typ -> bool


(** {b Type signatures} ****)

(** Type signatures. Two types are identical iff they have identical 
 * signatures. These contain the same information as types but canonicalized. 
 * For example, two function types that are identical except for the name of 
 * the formal arguments are given the same signature. Also, [TNamed] 
 * constructors are unrolled. *)
type typsig = 
    TSArray of typsig * exp option * attributes
  | TSPtr of typsig * attributes
  | TSComp of bool * string * attributes
  | TSFun of typsig * typsig list * bool * attributes
  | TSEnum of string * attributes
  | TSBase of typ

(** Print a type signature *)
val d_typsig: unit -> typsig -> Pretty.doc

(** Compute a type signature *)
val typeSig: typ -> typsig

(** Like {!Cil.typeSig} but customize the incorporation of attributes *)
val typeSigWithAttrs: (attributes -> attributes) -> typ -> typsig

(** Replace the attributes of a signature (only at top level) *)
val setTypeSigAttrs: attributes -> typsig -> typsig 

(** Get the top-level attributes of a signature *)
val typeSigAttrs: typsig -> attributes

(*********************************************************)
(**  LVALUES *)

(** Make a varinfo (for use in a TFun). Use other functions to make locals 
    and globals *)
val makeVarinfo: string -> typ -> varinfo

(** Make a formal variable for a function. Insert it in both the sformals 
    and the type of the function. You can optionally specify where to insert 
    this one. If where = "^" then it is inserted first. If where = "$" then 
    it is inserted last. Otherwise where must be the name of a formal after 
    which to insert this. By default it is inserted at the end. *)
val makeFormalVar: fundec -> ?where:string -> string -> typ -> varinfo

(** Make a local variable and add it to a function's slocals (only if insert = 
    true, which is the default). Make sure you know what you are doing if you 
    set insert=false.  *)
val makeLocalVar: fundec -> ?insert:bool -> string -> typ -> varinfo

(** Make a temporary variable and add it to a function's slocals. The name of 
    the temporary variable will be generated based on the given name hint so 
    that to avoid conflicts with other locals.  *)
val makeTempVar: fundec -> ?name: string -> typ -> varinfo


(** Make a global variable. Your responsibility to make sure that the name 
    is unique *) 
val makeGlobalVar: string -> typ -> varinfo


(** Add an offset at the end of an lvalue. Make sure the type of the lvalue 
 * and the offset are compatible. *)
val addOffsetLval: offset -> lval -> lval 

(** [addOffset o1 o2] adds [o1] to the end of [o2]. *)
val addOffset:     offset -> offset -> offset

(** Compute the type of an lvalue *)
val typeOfLval: lval -> typ

(** Compute the type of an offset from a bast type *)
val typeOffset: typ -> offset -> typ 


(*******************************************************)
(** {b Values for manipulating expressions} *)


(* Construct integer constants *)

(** 0 *)
val zero: exp

(** 1 *)
val one: exp

(** -1 *)
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

(** Construct a cast when having the old type of the expression. *)
val mkCastT: e:exp -> oldt:typ -> newt:typ -> exp

(** Like {!Cil.mkCastT} but uses typeOf to get [oldt] *)  
val mkCast: e:exp -> newt:typ -> exp 

(** Compute the type of an expression *)
val typeOf: exp -> typ

(**********************************************)
(** {b Values for manipulating statments} *)

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
 


(**************************************************)
(** {b Values for manipulating attributes} *)

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

(** Partition the attributes into classes:name attributes, function type, 
    and type attributes *)
val partitionAttributes:  default:attributeClass -> 
                         attributes -> attribute list * (* AttrName *)
                                       attribute list * (* AttrFunType *)
                                           attribute list   (* AttrType *)

(** Add an attribute. Maintains the attributes in sorted order or the second 
    argument *)
val addAttribute: attribute -> attributes -> attributes

(** Add a list of attributes. Maintains the attributes in sorted order. The 
    second argument must be sorted, but not necessarily the first *)
val addAttributes: attribute list -> attributes -> attributes

(** Remove all attributes with the given name. Maintains the attributes in 
    sorted order.  *)
val dropAttribute: string -> attributes -> attributes

(** Retains attributes with the given name *)
val filterAttributes: string -> attributes -> attributes

(** True if the named attribute appears in the attribute list. The list of
    attributes must be sorted.  *)
val hasAttribute: string -> attributes -> bool

(** Returns all the attributes contained in a type. This requires a traversal 
    of the type structure, in case of composite, enumeration and named types *)
val typeAttrs: typ -> attribute list

val setTypeAttrs: typ -> attributes -> typ (* Resets the attributes *)


(** Add some attributes to a type *)
val typeAddAttributes: attribute list -> typ -> typ

(** Remove all attributes with the given names from a type. Note that this
    does not remove attributes from typedef and tag definitions, just from 
    their uses *)
val typeRemoveAttributes: string list -> typ -> typ

(** Use this to intercept all attributes when they are printed. If the 
 * functional argument returns [Some d] then [d] is used as the external form 
 * of the attribute. Otherwise the attribute is printed normally. *)
val setCustomPrintAttribute: 
    (attribute -> Pretty.doc option) -> unit

(** Like {!Cil.setCustomPrintAttribute} but adds the given function to the 
 * beginning of the chain of custom attribute printers but only for the 
 * duration of executing the function passed as the second argument when 
 * applied to the third argument *)
val setCustomPrintAttributeScope: 
    (attribute -> Pretty.doc option) -> ('a -> 'b) -> 'a -> 'b


             
(******************
 ******************  VISITOR
 ******************)
(** {b The visitor} *)

(** Different visiting actions. 'a will be instantiated with [exp], [instr],
    etc. *)
type 'a visitAction = 
    SkipChildren                        (** Do not visit the children. Return 
                                            the node as it is. *)
  | DoChildren                          (** Continue with the children of this 
                                            node. Rebuild the node on return 
                                            if any of the children changes 
                                            (use == test) *)
  | ChangeTo of 'a                      (** Replace the expression with the 
                                            given one *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a) (** First consider that the entire 
                                           exp is replaced by the first 
                                           parameter. Then continue with 
                                           the children. On return rebuild 
                                           the node if any of the children 
                                           has changed and then apply the 
                                           function on the node *)



(** A visitor interface for traversing CIL trees. Create instantiations of 
 * this type by specializing the class {!Cil.nopCilVisitor}. *)
class type cilVisitor = object
  method vvdec: varinfo -> varinfo visitAction  
    (** Invoked for each variable declaration. The subtrees to be traversed 
     * are those corresponding to the type and attributes of the variable. 
     * Note that variable declarations are all the [GVar], [GDecl], [GFun], 
     * all the [varinfo] in formals of function types, and the formals and 
     * locals for function definitions. This means that the list of formals 
     * in a function definition will be traversed twice, once as part of the 
     * function type and second as part of the formals in a function 
     * definition. *)

  method vvrbl: varinfo -> varinfo visitAction  
    (** Invoked on each variable use. Here only the [SkipChildren] and 
     * [ChangeTo] actions make sense since there are no subtrees. Note that 
     * the type and attributes of the variable are not traversed for a 
     * variable use *)

  method vexpr: exp -> exp visitAction          
    (** Invoked on each expression occurence. The subtrees are the 
     * subexpressions, the types (for a [Cast] or [SizeOf] expression) or the 
     * variable use. *)

  method vlval: lval -> lval visitAction        
    (** Invoked on each lvalue occurence *)

  method voffs: offset -> offset visitAction    
    (** Invoked on each offset occurrence *)

  method vinst: instr -> instr list visitAction 
    (** Invoked on each instruction occurrence. The [ChangeTo] action can 
     * replace this instruction with a list of instructions *)

  method vstmt: stmt -> stmt visitAction        
    (** Control-flow statement. *)

  method vblock: block -> block visitAction     (** Block. Replaced in 
                                                    place. *)
  method vfunc: fundec -> fundec visitAction    (** Function definition. 
                                                    Replaced in place. *)
  method vglob: global -> global list visitAction (** Global (vars, types,
                                                      etc.)  *)
  method vinit: init -> init visitAction        (** Initializers for globals *)
  method vtype: typ -> typ visitAction          (** Use of some type. Note 
                                                 * that for structure/union 
                                                 * and enumeration types the 
                                                 * definition of the 
                                                 * composite type is not 
                                                 * visited. Use [vglob] to 
                                                 * visit it.  *)
  method vattr: attribute -> attribute list visitAction 
    (** Attribute. Each attribute can be replaced by a list *)
end

(** Default Visitor. Traverses the CIL tree without modifying anything *)
class nopCilVisitor: cilVisitor

(* other cil constructs *)

(** Visit a file *)
val visitCilFile: cilVisitor -> file -> file

(** Visit a global *)
val visitCilGlobal: cilVisitor -> global -> global list

(** Visit a function definition *)
val visitCilFunction: cilVisitor -> fundec -> fundec

(* Visit an expression *)
val visitCilExpr: cilVisitor -> exp -> exp

(** Visit an lvalue *)
val visitCilLval: cilVisitor -> lval -> lval

(** Visit an lvalue offset *)
val visitCilOffset: cilVisitor -> offset -> offset

(** Visit an instruction *)
val visitCilInstr: cilVisitor -> instr -> instr list

(** Visit a statement *)
val visitCilStmt: cilVisitor -> stmt -> stmt

(** Visit a block *)
val visitCilBlock: cilVisitor -> block -> block

(** Visit a type *)
val visitCilType: cilVisitor -> typ -> typ

(** Visit a variable declaration *)
val visitCilVarDecl: cilVisitor -> varinfo -> varinfo

(** Visit an initializer *)
val visitCilInit: cilVisitor -> init -> init


(** Visit a list of attributes *)
val visitCilAttributes: cilVisitor -> attribute list -> attribute list

(* And some generic visitors. The above are built with these *)




(** {b Utility functions} *)

(** Whether the pretty printer should print output for the MS VC compiler.
   Default is GCC *)
val msvcMode: bool ref               

(** Whether to print line numbers *)
val printLn: bool ref

(** Whether to print line numbers in comments*)
val printLnComment: bool ref

(** {b Debugging support} *)

(** A reference to the current location. If you are careful to set this to 
 * the current location then you can use some built-in logging functions that 
 * will print the location. *)
val currentLoc: location ref

(** CIL has a faily easy to use mechanism for printing error messages. This 
 * mechanism is built on top of the pretty-printer mechanism (see 
 * {!Pretty.doc}) and the error-message modules (see {!Errormsg.error}). 

 Here is a typical example for printing a log message: {v 
ignore (Errormsg.log "Expression %a is not positive (at %s:%i)\n"
                        d_exp e loc.file loc.line)
 v}

 and here is an example of how you print a fatal error message that stop the 
* execution: {v 
Errormsg.s (Errormsg.bug "Why am I here?")
 v}

 Notice that you can use C format strings with some extension. The most 
useful extension is "%a" that means to consumer the next two argument from 
the argument list and to apply the first to [unit] and then to the second 
and to print the resulting {!Pretty.doc}. For each major type in CIL there is 
a corresponding function that pretty-prints an element of that type:
*)

(** Pretty-print a location *)
val d_loc: unit -> location -> Pretty.doc

(** Pretty-print the {!Cil.currentLoc} *)
val d_thisloc: unit -> Pretty.doc

(** Pretty-print an integer of a given kind *)
val d_ikind: unit -> ikind -> Pretty.doc

(** Pretty-print a floating-point value *)
val d_fkind: unit -> fkind -> Pretty.doc

(** Pretty-print storage-class information *)
val d_storage: unit -> storage -> Pretty.doc

(** Pretty-print a constant *)
val d_const: unit -> constant -> Pretty.doc

(** Pretty-print a type *)
val d_type: unit -> typ -> Pretty.doc

(** Pretty-print an expression *)
val d_exp: unit -> exp -> Pretty.doc

(** Pretty-print an initializer *)
val d_init: unit -> init -> Pretty.doc

(** Pretty-print a binary operator *)
val d_binop: unit -> binop -> Pretty.doc

(** Pretty-print an attribute *)
val d_attr: unit -> attribute -> Pretty.doc

(** Pretty-print an argument of an attribute *)
val d_attrparam: unit -> attrparam -> Pretty.doc

(** Pretty-print a list of attributes *)
val d_attrlist: unit -> attributes -> Pretty.doc 

(** Like d_attrlist but when attribute comes before the  
  * qualified entity. The only difference is in how spaces are printed *)
val d_attrlist_pre: unit -> attributes -> Pretty.doc  

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

(** Pretty-print an entire file. Here you give the channel where the printout
 * should be sent. *)
val printFile: out_channel -> file -> unit


(* the following error message producing functions also print a location in 
 * the code. use Errormsg.bug and Errormsg.unimp if you do not want that *)

(** Like {!Errormsg.bug} except that {!Cil.currentLoc} is also printed *)
val bug: ('a,unit,Pretty.doc) format -> 'a

(** Like {!Errormsg.unimp} except that {!Cil.currentLoc}is also printed *)
val unimp: ('a,unit,Pretty.doc) format -> 'a

(** Like {!Errormsg.error} except that {!Cil.currentLoc} is also printed *)
val error: ('a,unit,Pretty.doc) format -> 'a

(** Like {!Cil.error} except that it explicitly takes a location argument, 
 * instead of using the {!Cil.currentLoc} *)
val errorLoc: location -> ('a,unit,Pretty.doc) format -> 'a  

(** Like {!Errormsg.warn} except that {!Cil.currentLoc} is also printed *)
val warn: ('a,unit,Pretty.doc) format -> 'a

(** Like {!Errormsg.warn} except that {!Cil.currentLoc} and context 
    is also printed *)
val warnContext: ('a,unit,Pretty.doc) format -> 'a

(** Like {!Cil.warn} except that it explicitly takes a location argument, 
 * instead of using the {!Cil.currentLoc} *)
val warnLoc: location -> ('a,unit,Pretty.doc) format -> 'a  

(** Sometimes you do not want to see the syntactic sugar that the above 
 * pretty-printing functions add. In that case you can use the following 
 * pretty-printing functions. But note that the output of these functions is 
 * not valid C *)

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


(** ALPHA conversion *)
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
val docAlphaTable: unit -> (string, int ref) Hashtbl.t -> Pretty.doc

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

     
(** Raised when one of the bitsSizeOf functions cannot compute the size of a 
    type. This can happen because the type contains array-length expressions 
    that we don't know how to compute or because it is a type whose size is 
    not defined (e.g. TVoid or TFun)  *)        
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


(** Represents a location that cannot be determined *)
val locUnknown: location


(** Generate an {!Cil.exp} to be used in case of errors. *)
val dExp: Pretty.doc -> exp 

(** Generate an {!Cil.instr} to be used in case of errors. *)
val dInstr: Pretty.doc -> location -> instr

(** Generate a {!Cil.global} to be used in case of errors. *)
val dGlobal: Pretty.doc -> location -> global

(** Like map but try not to make a copy of the list *)
val mapNoCopy: ('a -> 'a) -> 'a list -> 'a list

(** Like map but each call can return a list. Try not to make a copy of the 
    list *)
val mapNoCopyList: ('a -> 'a list) -> 'a list -> 'a list

(** sm: return true if the first is a prefix of the second string *)
val startsWith: string -> string -> bool


