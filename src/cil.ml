(*

   Copyright (c) 2001-2003,
    George C. Necula    <necula@cs.berkeley.edu>
    Scott McPeak        <smcpeak@cs.berkeley.edu>
    Wes Weimer          <weimer@cs.berkeley.edu>
    Ben Liblit          <liblit@cs.berkeley.edu>
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. The names of the contributors may not be used to endorse or promote
   products derived from this software without specific prior written
   permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 *)

open Escape
open Pretty
open Cilint
(* open Trace      (\* sm: 'trace' function *\) *)
module E = Errormsg
module H = Hashtbl
module IH = Inthash

(*
   CIL: An intermediate language for analyzing C progams.

   Scott McPeak, George Necula, Wes Weimer

 *)

(* The module Cilversion is generated automatically by Makefile from
   information in configure.in *)
let cilVersion         = Cilversion.cilVersion

type cstd = C90 | C99 | C11
let cstd_of_string = function
| "c90" -> C90
| "c99" -> C99
| "c11" -> C11
| _ -> failwith "Not a valid c standard argument."
let cstd = ref C99
let gnu89inline = ref false
let c99Mode () = !cstd <> C90 (* True to handle ISO C 99 vs 90 changes.
      c99mode only affects parsing of decimal integer constants without suffix
          a) on machines where long and long long do not have the same size
             (e.g. 32 Bit machines, 64 Bit Windows, not 64 Bit MacOS or (most? all?) 64 Bit Linux):
             giving constants that are bigger than max long type long long in c99mode vs. unsigned long
             if c99mode is off.
          b) for constants bigger than long long producing a "Unimplemented: Cannot represent the integer"
             warning in C99 mode vs. unsigned long long if c99mode is off. *)

(* Set this to true to get old-style handling of gcc's extern inline C extension:
   old-style: the extern inline definition is used until the actual definition is
     seen (as long as optimization is enabled)
   new-style: the extern inline definition is used only if there is no actual
     definition (as long as optimization is enabled)
   Note that CIL assumes that optimization is always enabled ;-) *)
let oldstyleExternInline = ref false

let makeStaticGlobal = ref true

let useLogicalOperators = ref false

let useComputedGoto = ref false

let useCaseRange = ref false

let addReturnOnNoreturnFallthrough = ref false

module M = Machdep
(* Cil.initCil will set this to the current machine description.
   Makefile.cil generates the file src/machdep.ml,
   which contains the descriptions of gcc and msvc. *)
let envMachine : M.mach option ref = ref None


let lowerConstants: bool ref = ref true
    (** Do lower constants (default true) *)

let removeBranchingOnConstants: bool ref = ref true
    (** Remove branches of the form if(const) ... else ... (default true) *)

let insertImplicitCasts: bool ref = ref true
    (** Do insert implicit casts (default true) *)


let little_endian = ref true
let char_is_unsigned = ref false
let underscore_name = ref false

type lineDirectiveStyle =
  | LineComment                (** Before every element, print the line
                                  number in comments. This is ignored by
                                  processing tools (thus errors are reproted
                                  in the CIL output), but useful for
                                  visual inspection *)
  | LineCommentSparse          (** Like LineComment but only print a line
                                  directive for a new source line *)
  | LinePreprocessorInput      (** Use #line directives *)
  | LinePreprocessorOutput     (** Use # nnn directives (in gcc mode) *)

let lineDirectiveStyle = ref (Some LinePreprocessorInput)

let print_CIL_Input = ref false

let printCilAsIs = ref false

let lineLength = ref 80

let warnTruncate = ref true

(* sm: return the string 's' if we're printing output for gcc, suppres
   it if we're printing for CIL to parse back in.  the purpose is to
   hide things from gcc that it complains about, but still be able
   to do lossless transformations when CIL is the consumer *)
let forgcc (s: string) : string =
  if (!print_CIL_Input) then "" else s


let debugConstFold = false

(** The Abstract Syntax of CIL *)


(** The top-level representation of a CIL source file. Its main contents is
    the list of global declarations and definitions. *)
type file =
    { mutable fileName: string;   (** The complete file name *)
      mutable globals: global list; (** List of globals as they will appear
                                        in the printed file *)
      mutable globinit: fundec option;
      (** An optional global initializer function. This is a function where
         you can put stuff that must be executed before the program is
         started. This function, is conceptually at the end of the file,
         although it is not part of the globals list. Use {!getGlobInit}
         to create/get one. *)
      mutable globinitcalled: bool;
      (** Whether the global initialization function is called in main. This
          should always be false if there is no global initializer. When
          you create a global initialization CIL will try to insert code in
          main to call it. *)
    }

and comment = location * string

(** The main type for representing global declarations and definitions. A list
    of these form a CIL file. The order of globals in the file is generally
    important. *)
and global =
  | GType of typeinfo * location
    (** A typedef. All uses of type names (through the [TNamed] constructor)
        must be preceded in the file by a definition of the name. The string
        is the defined name and always not-empty. *)

  | GCompTag of compinfo * location
    (** Defines a struct/union tag with some fields. There must be one of
        these for each struct/union tag that you use (through the [TComp]
        constructor) since this is the only context in which the fields are
        printed. Consequently nested structure tag definitions must be
        broken into individual definitions with the innermost structure
        defined first. *)

  | GCompTagDecl of compinfo * location
    (** Declares a struct/union tag. Use as a forward declaration. This is
        printed without the fields.  *)

  | GEnumTag of enuminfo * location
   (** Declares an enumeration tag with some fields. There must be one of
      these for each enumeration tag that you use (through the [TEnum]
      constructor) since this is the only context in which the items are
      printed. *)

  | GEnumTagDecl of enuminfo * location
    (** Declares an enumeration tag. Use as a forward declaration. This is
        printed without the items.  *)

  | GVarDecl of varinfo * location
   (** A variable declaration (not a definition). If the variable has a
       function type then this is a prototype. There can be several
       declarations and at most one definition for a given variable in C, but
       in CIL there is also only at most one declaration per variable. If both
       forms appear then they must share the same varinfo structure. A
       prototype shares the varinfo with the fundec of the definition. Either
       has storage Extern or there must be a definition in this file *)

  | GVar  of varinfo * initinfo * location
     (** A variable definition. Can have an initializer. The initializer is
        updateable so that you can change it without requiring to recreate
        the list of globals. There can be at most one definition for a
        variable in an entire program. Cannot have storage Extern or function
        type. *)


  | GFun of fundec * location
     (** A function definition. *)

  | GAsm of string * location           (** Global asm statement. These ones
                                            can contain only a template *)
  | GPragma of attribute * location     (** Pragmas at top level. Use the same
                                            syntax as attributes *)
  | GText of string                     (** Some text (printed verbatim) at
                                            top level. E.g., this way you can
                                            put comments in the output.  *)


(** The various types available. Every type is associated with a list of
   attributes, which are always kept in sorted order. Use {!addAttribute}
   and {!addAttributes} to construct list of attributes. If you want to
   inspect a type, you should use {!unrollType} to see through the uses
   of named types. *)
and typ =
    TVoid of attributes   (** Void type *)
  | TInt of ikind * attributes (** An integer type. The kind specifies
                                       the sign and width. *)
  | TFloat of fkind * attributes (** A floating-point type. The kind
                                         specifies the precision. *)

  | TPtr of typ * attributes
           (** Pointer type. *)

  | TArray of typ * exp option * attributes
           (** Array type. It indicates the base type and the array length. *)

  | TFun of typ * (string * typ * attributes) list option * bool * attributes
          (** Function type. Indicates the type of the result, the name, type
             and name attributes of the formal arguments ([None] if no
             arguments were specified, as in a function whose definition or
             prototype we have not seen; [Some \[\]] means void). Use
             {!argsToList} to obtain a list of arguments. The boolean
             indicates if it is a variable-argument function. If this is the
             type of a varinfo for which we have a function declaration then
             the information for the formals must match that in the
             function's sformals. *)

  | TNamed of typeinfo * attributes
          (** The use of a named type. All uses of the same type name must
             share the typeinfo. Each such type name must be preceded
             in the file by a [GType] global. This is printed as just the
             type name. The actual referred type is not printed here and is
             carried only to simplify processing. To see through a sequence
             of named type references, use {!unrollType}. The attributes
             are in addition to those given when the type name was defined. *)

  | TComp of compinfo * attributes
          (** A reference to a struct or a union type. All references to the
             same struct or union must share the same compinfo among them and
             with a [GCompTag] global that precedes all uses (except maybe
             those that are pointers to the composite type). The attributes
             given are those pertaining to this use of the type and are in
             addition to the attributes that were given at the definition of
             the type and which are stored in the compinfo.  *)

  | TEnum of enuminfo * attributes
           (** A reference to an enumeration type. All such references must
               share the enuminfo among them and with a [GEnumTag] global that
               precedes all uses. The attributes refer to this use of the
               enumeration and are in addition to the attributes of the
               enumeration itself, which are stored inside the enuminfo  *)



  | TBuiltin_va_list of attributes
            (** This is the same as the gcc's type with the same name *)

(** Various kinds of integers *)
and ikind =
    IChar       (** [char] *)
  | ISChar      (** [signed char] *)
  | IUChar      (** [unsigned char] *)
  | IBool       (** [_Bool (C99)] *)
  | IInt        (** [int] *)
  | IUInt       (** [unsigned int] *)
  | IShort      (** [short] *)
  | IUShort     (** [unsigned short] *)
  | ILong       (** [long] *)
  | IULong      (** [unsigned long] *)
  | ILongLong   (** [long long] (or [_int64] on Microsoft Visual C) *)
  | IULongLong  (** [unsigned long long] (or [unsigned _int64] on Microsoft
                    Visual C) *)
  | IInt128     (** [__int128] *)
  | IUInt128    (** [unsigned __int128] *)

(** Various kinds of floating-point numbers*)
and fkind =
    FFloat              (** [float] *)
  | FDouble             (** [double] *)
  | FLongDouble         (** [long double] *)
  | FFloat128           (** [float128] *)
  | FComplexFloat       (** [float _Complex] *)
  | FComplexDouble      (** [double _Complex] *)
  | FComplexLongDouble  (** [long double _Complex]*)
  | FComplexFloat128    (** [_float128 _Complex]*)

(** An attribute has a name and some optional parameters *)
and attribute = Attr of string * attrparam list

(** Attributes are lists sorted by the attribute name *)
and attributes = attribute list

(** The type of parameters in attributes *)
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
  | ASizeOfS of typsig                   (** Replacement for ASizeOf in type
                                             signatures.  Only used for
                                             attributes inside typsigs.*)
  | AAlignOf of typ
  | AAlignOfE of attrparam
  | AAlignOfS of typsig
  | AUnOp of unop * attrparam
  | ABinOp of binop * attrparam * attrparam
  | ADot of attrparam * string           (** a.foo **)
  | AStar of attrparam                   (** * a *)
  | AAddrOf of attrparam                 (** & a **)
  | AIndex of attrparam * attrparam      (** a1[a2] *)
  | AQuestion of attrparam * attrparam * attrparam (** a1 ? a2 : a3 **)


(** Information about a composite type (a struct or a union). Use
    {!mkCompInfo}
    to create non-recursive or (potentially) recursive versions of this. Make
    sure you have a [GCompTag] for each one of these.  *)
and compinfo = {
    mutable cstruct: bool;              (** True if struct, False if union *)
    mutable cname: string;              (** The name. Always non-empty. Use
                                           {!compFullName} to get the
                                           full name of a comp (along with
                                           the struct or union) *)
    mutable ckey: int;                  (** A unique integer constructed from
                                           the name. Use {!Hashtbl.hash} on
                                           the string returned by
                                           {!compFullName}. All compinfo
                                           for a given key are shared. *)
    mutable cfields: fieldinfo list;    (** Information about the fields *)
    mutable cattr:   attributes;        (** The attributes that are defined at
                                            the same time as the composite
                                            type *)
    mutable cdefined: bool;             (** Whether this is a defined
                                           compinfo. *)
    mutable creferenced: bool;          (** True if used. Initially set to
                                           false *)
  }

(** Information about a struct/union field *)
and fieldinfo = {
    mutable fcomp: compinfo;            (** The compinfo of the host. Note
                                            that this must be shared with the
                                            host since there can be only one
                                            compinfo for a given id *)
    mutable fname: string;              (** The name of the field. Might be
                                           the value of
                                           {!missingFieldName} in which
                                           case it must be a bitfield and is
                                           not printed and it does not
                                           participate in initialization *)
    mutable ftype: typ;                 (** The type *)
    mutable fbitfield: int option;      (** If a bitfield then ftype should be
                                            an integer type *)
    mutable fattr: attributes;          (** The attributes for this field
                                            (not for its type) *)
    mutable floc: location;             (** The location where this field
                                            is defined *)
}



(** Information about an enumeration. This is shared by all references to an
    enumeration. Make sure you have a [GEnumTag] for each of of these.   *)
and enuminfo = {
    mutable ename: string;              (** The name. Always non-empty *)
    mutable eitems: (string * exp * location) list; (** Items with names
                                                      and values. This list
                                                      should be
                                                      non-empty. The item
                                                      values must be
                                                      compile-time
                                                      constants. *)
    mutable eattr: attributes;         (** Attributes *)
    mutable ereferenced: bool;         (** True if used. Initially set to false*)
    mutable ekind: ikind;
    (** The integer kind used to represent this enum. Per ANSI-C, this
        should always be IInt, but gcc allows other integer kinds *)
}

(** Information about a defined type *)
and typeinfo = {
    mutable tname: string;
    (** The name. Can be empty only in a [GType] when introducing a composite
       or enumeration tag. If empty cannot be referred to from the file *)
    mutable ttype: typ;
    (** The actual type. *)
    mutable treferenced: bool;
    (** True if used. Initially set to false*)
}


(** Information about a variable. These structures are shared by all
   references to the variable. So, you can change the name easily, for
   example. Use one of the {!makeLocalVar}, {!makeTempVar} or
   {!makeGlobalVar} to create instances of this data structure. *)
and varinfo = {
    mutable vname: string;		(** The name of the variable. Cannot
                                            be empty. *)
    mutable vtype: typ;                 (** The declared type of the
                                            variable. *)
    mutable vattr: attributes;          (** A list of attributes associated
                                            with the variable. *)
    mutable vstorage: storage;          (** The storage-class *)
    (* The other fields are not used in varinfo when they appear in the formal
       argument list in a [TFun] type *)


    mutable vglob: bool;	        (** True if this is a global variable*)

    mutable vinline: bool;        (** Whether this varinfo is for an inline function. *)

    mutable vdecl: location;            (** Location of variable declaration *)

    vinit: initinfo;
    (** Optional initializer.  Only used for static and global variables.
       Initializers for other types of local variables are turned into
       assignments. *)

    mutable vid: int;  (** A unique integer identifier.  *)
    mutable vaddrof: bool;              (** True if the address of this
                                            variable is taken. CIL will set
                                           these flags when it parses C, but
                                           you should make sure to set the
                                           flag whenever your transformation
                                           create [AddrOf] expression. *)

    mutable vreferenced: bool;          (** True if this variable is ever
                                            referenced. This is computed by
                                            [removeUnusedVars]. It is safe to
                                            just initialize this to False *)

    mutable vdescr: doc;                (** For most temporary variables, a
                                            description of what the var holds.
                                            (e.g. for temporaries used for
                                            function call results, this string
                                            is a representation of the function
                                            call.) *)

    mutable vdescrpure: bool;           (** Indicates whether the vdescr above
                                            is a pure expression or call.
                                            True for all CIL expressions and
                                            Lvals, but false for e.g. function
                                            calls.
                                            Printing a non-pure vdescr more
                                            than once may yield incorrect
                                            results. *)
    mutable vhasdeclinstruction: bool;  (** Indicates whether a VarDecl instruction
                                            was generated for this variable.
                                            Only applies to local variables.
                                            Currently, this is relevant for when to
                                            print the declaration. If this is
                                            true, it might be incorrect to print the
                                            declaration at the beginning of the
                                            function, rather than were the VarDecl
                                            instruction is. This was originally
                                            introduced to handle VLAs. *)
}

(** Storage-class information *)
and storage =
    NoStorage                         (** The default storage. Nothing is
                                           printed  *)
    | Static
    | Register
    | Extern


(** Expressions (Side-effect free)*)
and exp =
    Const      of constant              (** Constant *)
  | Lval       of lval                  (** Lvalue *)
  | SizeOf     of typ                   (** sizeof(<type>). Has [unsigned
                                           int] type (ISO 6.5.3.4). This is
                                           not turned into a constant because
                                           some transformations might want to
                                           change types *)
  | Real       of exp                   (** __real__(<expression>) *)
  | Imag       of exp                   (** __imag__(<expression>) *)
  | SizeOfE    of exp                   (** sizeof(<expression>) *)
  | SizeOfStr  of string
    (** sizeof(string_literal). We separate this case out because this is the
        only instance in which a string literal should not be treated as
        having type pointer to character. *)

  | AlignOf    of typ                   (** Has [unsigned int] type *)
  | AlignOfE   of exp


  | UnOp       of unop * exp * typ      (** Unary operation. Includes
                                            the type of the result *)

  | BinOp      of binop * exp * exp * typ
                                        (** Binary operation. Includes the
                                            type of the result. The arithmetic
                                            conversions are made  explicit
                                            for the arguments *)
  | Question   of exp * exp * exp * typ
                                        (** (a ? b : c) operation. Includes
                                            the type of the result *)
  | CastE      of typ * exp            (** Use {!mkCast} to make casts *)

  | AddrOf     of lval                 (** Always use {!mkAddrOf} to
                                          construct one of these. Apply to an
                                          lvalue of type [T] yields an
                                          expression of type [TPtr(T)] *)
  | AddrOfLabel of stmt ref

  | StartOf    of lval   (** There is no C correspondent for this. C has
                            implicit coercions from an array to the address
                            of the first element. [StartOf] is used in CIL to
                            simplify type checking and is just an explicit
                            form of the above mentioned implicit conversion.
                            It is not printed. Given an lval of type
                            [TArray(T)] produces an expression of type
                            [TPtr(T)]. *)

and wstring_type = | Wchar_t | Char16_t | Char32_t
and encoding = No_encoding | Utf8

(** Literal constants *)
and constant =
  | CInt of cilint * ikind * string option
                 (** Integer constant. Give the ikind (see ISO9899 6.1.3.2)
                    and the textual representation, if available. Use
                    {!integer} or {!kinteger} to create these. *)
  | CStr of string * encoding (** String constant (of pointer type) *)
  | CWStr of int64 list * wstring_type (** Wide string constant (of type "wchar_t *") *)
  | CChr of char (** Character constant.  This has type int, so use
                     charConstToInt to read the value in case
                     sign-extension is needed. *)
  | CReal of float * fkind * string option (** Floating point constant. Give
                                               the fkind (see ISO 6.4.4.2) and
                                               also the textual representation,
                                               if available *)
  | CEnum of exp * string * enuminfo
     (** An enumeration constant with the given value, name, from the given
        enuminfo. This is not used if {!lowerEnum} is false (default).
        Use {!Cillower.lowerEnumVisitor} to replace these with integer
        constants. *)

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
                                           it arises from an expression
                                           [e\[i\]] when [e] is a pointer and
                                           not an array. This is semantically
                                           the same as PlusPI but CCured uses
                                           this as a hint that the integer is
                                           probably positive. *)
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
  | BAnd                                (** bitwise and *)
  | BXor                                (** exclusive-or *)
  | BOr                                 (** inclusive-or *)

  | LAnd                                (** logical and *)
  | LOr                                 (** logical or *)




(** An lvalue denotes the contents of a range of memory addresses. This range
   is denoted as a host object along with an offset within the object. The
   host object can be of two kinds: a local or global variable, or an object
   whose address is in a pointer expression. We distinguish the two cases so
   that we can tell quickly whether we are accessing some component of a
   variable directly or we are accessing a memory location through a pointer.*)
and lval =
    lhost * offset

(** The host part of an {!lval}. *)
and lhost =
  | Var        of varinfo
    (** The host is a variable. *)

  | Mem        of exp
    (** The host is an object of type [T] when the expression has pointer
       [TPtr(T)]. *)


(** The offset part of an {!lval}. Each offset can be applied to certain
    kinds of lvalues and its effect is that it advances the starting address
    of the lvalue and changes the denoted type, essentially focussing to some
    smaller lvalue that is contained in the original one. *)
and offset =
  | NoOffset          (** No offset. Can be applied to any lvalue and does
                          not change either the starting address or the type.
                          This is used when the lval consists of just a host
                          or as a terminator in a list of other kinds of
                          offsets. *)

  | Field      of fieldinfo * offset
                      (** A field offset. Can be applied only to an lvalue
                         that denotes a structure or a union that contains
                         the mentioned field. This advances the offset to the
                         beginning of the mentioned field and changes the
                         type to the type of the mentioned field. *)

  | Index    of exp * offset
                     (** An array index offset. Can be applied only to an
                         lvalue that denotes an array. This advances the
                         starting address of the lval to the beginning of the
                         mentioned array element and changes the denoted type
                         to be the type of the array element *)



(* The following equivalences hold *)
(* Mem(AddrOf(Mem a, aoff)), off   = Mem a, aoff + off                *)
(* Mem(AddrOf(Var v, aoff)), off   = Var v, aoff + off                *)
(* AddrOf (Mem a, NoOffset)        = a                                *)

(** Initializers for global variables.  You can create an initializer with
   {!makeZeroInit}. *)
and init =
  | SingleInit   of exp   (** A single initializer *)
  | CompoundInit   of typ * (offset * init) list
            (** Used only for initializers of structures, unions and arrays.
               The offsets are all of the form [Field(f, NoOffset)] or
               [Index(i, NoOffset)] and specify the field or the index being
               initialized. For structures all fields
               must have an initializer (except the unnamed bitfields), in
               the proper order. This is necessary since the offsets are not
               printed. For arrays the list must contain a prefix of the
               initializers; the rest are 0-initialized.
               For unions there must be exactly one initializer. If
               the initializer is not for the first field then a field
               designator is printed. You can scan an initializer list with
               {!foldLeftCompound}. *)

(** We want to be able to update an initializer in a global variable, so we
   define it as a mutable field *)
and initinfo = {
    mutable init : init option;
  }


(** Function definitions. *)
and fundec =
    { mutable svar:     varinfo;
         (** Holds the name and type as a variable, so we can refer to it
            easily from the program. All references to this function either
            in a function call or in a prototype must point to the same
            varinfo. *)
      mutable sformals: varinfo list;
        (** Formals. These must be shared with the formals that appear in the
           type of the function. Use {!setFormals} or
           {!setFunctionType} to set these
           formals and ensure that they are reflected in the function type.
           Do not make copies of these because the body refers to them. *)
      mutable slocals: varinfo list;
        (** Locals. Does not include the sformals. Do not make copies of
           these because the body refers to them. *)
      mutable smaxid: int;           (** Max local id. Starts at 0. *)
      mutable sbody: block;          (** The function body. *)
      mutable smaxstmtid: int option;  (** max id of a (reachable) statement
                                          in this function, if we have
                                          computed it. range = 0 ...
                                          (smaxstmtid-1). This is computed by
                                          {!computeCFGInfo}. *)
      mutable sallstmts: stmt list;   (** After you call {!computeCFGInfo}
                                        this field is set to contain all
                                        statements in the function *)
    }


(** A block is a sequence of statements with the control falling through from
    one element to the next *)
and block =
   { mutable battrs: attributes;      (** Attributes for the block *)
     mutable bstmts: stmt list;       (** The statements comprising the block*)
   }


(** Statements.
    The statement is the structural unit in the control flow graph. Use mkStmt
    to make a statement and then fill in the fields. *)
and stmt = {
    mutable labels: label list;        (** Whether the statement starts with
                                           some labels, case statements or
                                           default statement *)
    mutable skind: stmtkind;           (** The kind of statement *)

    (* Now some additional control flow information. Initially this is not
       filled in. *)
    mutable sid: int;                  (** A number (>= 0) that is unique
                                           in a function. *)
    mutable succs: stmt list;          (** The successor statements. They can
                                           always be computed from the skind
                                           and the context in which this
                                           statement appears *)
    mutable preds: stmt list;          (** The inverse of the succs function*)
    mutable fallthrough: stmt option;  (** The fallthrough successor statement computed from the context of this statement in {!computeCFGInto}. Useful for the syntactic successor of Goto and Loop. *)
  }

(** Labels *)
and label =
    Label of string * location * bool
          (** A real label. If the bool is "true", the label is from the
             input source program. If the bool is "false", the label was
             created by CIL or some other transformation *)
  | Case of exp * location * location             (** A case statement. Second location is just for label. *)
  | CaseRange of exp * exp * location * location  (** A case statement corresponding to a
                                            range of values. Second location is just for label. *)
  | Default of location * location                (** A default statement. Second location is just for label. *)



(* The various kinds of statements *)
and stmtkind =
  | Instr  of instr list               (** A group of instructions that do not
                                           contain control flow. Control
                                           implicitly falls through. *)
  | Return of exp option * location     (** The return statement. This is a
                                            leaf in the CFG. *)

  | Goto of stmt ref * location         (** A goto statement. Appears from
                                            actual goto's in the code. *)

  | ComputedGoto of exp * location

  | Break of location                   (** A break to the end of the nearest
                                             enclosing Loop or Switch *)
  | Continue of location                (** A continue to the start of the
                                            nearest enclosing [Loop] *)
  | If of exp * block * block * location * location (** A conditional.
                                             Two successors, the "then" and
                                             the "else" branches. Both
                                             branches  fall-through to the
                                             successor of the If statement.
                                             Second location is just for expression. *)
  | Switch of exp * block * (stmt list) * location * location
                                       (** A switch statement. The block
                                           contains within all of the cases.
                                           We also have direct pointers to the
                                           statements that implement the
                                           cases. Which cases they implement
                                           you can get from the labels of the
                                           statement.
                                           Second location is just for expression. *)

  | Loop of block * location * location * (stmt option) * (stmt option)
                                           (** A [while(1)] loop. The
                                              termination test is implemented
                                              in the body of a loop using a
                                              [Break] statement. If
                                              prepareCFG has been called, the
                                              first stmt option will point to
                                              the stmt containing the
                                              continue label for this loop
                                              and the second will point to
                                              the stmt containing the break
                                              label for this loop.
                                              Second location is just for expression. *)

  | Block of block                      (** Just a block of statements. Use it
                                            as a way to keep some attributes
                                            local *)

(** Instructions. They may cause effects directly but may not have control
    flow.*)
and instr =
    Set        of lval * exp * location * location  (** An assignment. A cast is present
                                             if the exp has different type
                                             from lval.
                                             Second location is just for expression when inside condition. *)
  | VarDecl    of varinfo * location     (** "Instruction" in the location where a varinfo was declared.
                                             All varinfos for which such a VarDecl instruction exists have
                                             vhasdeclinstruction set to true.
                                             The motivation for the addition of this instruction was to
                                             support VLAs for which declarations can not be pulled up like
                                             CIL used to do. *)
  | Call       of lval option * exp * exp list * location * location
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
                             result then an implicit cast exists.
                             Second location is just for expression when inside condition. *)

                         (* See the GCC specification for the meaning of ASM.
                            If the source is MS VC then only the templates
                            are used *)
                         (* sm: I've added a notes.txt file which contains more
                            information on interpreting Asm instructions *)
  | Asm        of attributes * (* Really only const and volatile can appear
                                 here *)
                  string list *         (* templates (CR-separated) *)
                  (string option * string * lval) list *
                                          (* outputs must be lvals with
                                             optional names and constraints.
                                             I would like these
                                             to be actually variables, but I
                                             run into some trouble with ASMs
                                             in the Linux sources  *)
                  (string option * string * exp) list *
                                        (* inputs with optional names and constraints *)
                  string list *         (* register clobbers *)
                  string list *         (* GoToLabels *)
                  location
        (** An inline assembly instruction. The arguments are (1) a list of
            attributes (only const and volatile can appear here and only for
            GCC), (2) templates (CR-separated), (3) a list of
            outputs, each of which is an lvalue with a constraint, (4) a list
            of input expressions along with constraints, (5) clobbered
            registers, and (5) location information *)



(** Describes a location in a source file *)
and location = {
    line: int;		   (** The line number. -1 means "do not know" *)
    file: string;          (** The name of the source file*)
    byte: int;             (** The byte position in the source file *)
    column: int;           (** The column number *)
    endLine: int;          (** End line number. Negative means unknown. *)
    endByte: int;          (** End byte position. Negative means unknown. *)
    endColumn: int;        (** End column number. Negative means unknown. *)
    synthetic: bool;       (** Synthetic location, doesn't necessarily precisely correspond to a location in original source code, e.g. due to CIL transformations.
                               @see <https://github.com/goblint/cil/pull/98> for some examples. *)
}

(* Type signatures. Two types are identical iff they have identical
   signatures *)
and typsig =
    TSArray of typsig * cilint option * attribute list
  | TSPtr of typsig * attribute list
  | TSComp of bool * string * attribute list
  | TSFun of typsig * typsig list option * bool * attribute list
  | TSEnum of string * attribute list
  | TSBase of typ

let locUnknown = { line = -1;
		   file = "";
		   byte = -1;
       column = -1;
       endLine = -1;
       endByte = -1;
       endColumn = -1;
       synthetic = true;}

(* A reference to the current location *)
let currentLoc : location ref = ref locUnknown
(* A reference to the current expression location *)
let currentExpLoc : location ref = ref locUnknown

(* A reference to the current global being visited *)
let currentGlobal: global ref = ref (GText "dummy")


let compareLoc (a: location) (b: location) : int =
  let namecmp = compare a.file b.file in
  if namecmp != 0
  then namecmp
  else
    let linecmp = a.line - b.line in
    if linecmp != 0
    then linecmp
    else
      let columncmp = a.column - b.column in
      if columncmp != 0
      then columncmp
      else
        let bytecmp = a.byte - b.byte in
        if bytecmp != 0
        then bytecmp
        else
          let endLinecmp = a.endLine - b.endLine in
          if endLinecmp != 0
          then endLinecmp
          else
            let endColumncmp = a.endColumn - b.endColumn in
            if endColumncmp != 0
            then endColumncmp
            else a.endByte - b.endByte

let argsToList : (string * typ * attributes) list option
                  -> (string * typ * attributes) list
    = function
    None -> []
  | Some al -> al


(* A hack to allow forward reference of d_exp *)
let pd_exp : (unit -> exp -> doc) ref =
  ref (fun _ -> E.s (E.bug "pd_exp not initialized"))
let pd_type : (unit -> typ -> doc) ref =
  ref (fun _ -> E.s (E.bug "pd_type not initialized"))
let pd_attr : (unit -> attribute -> doc) ref =
  ref (fun _ -> E.s (E.bug "pd_attr not initialized"))

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



(* sm/gn: cil visitor interface for traversing Cil trees. *)
(* Use visitCilStmt and/or visitCilFile to use this. *)
(* Some of the nodes are changed in place if the children are changed. Use
   one of Change... actions if you want to copy the node *)

(** A visitor interface for traversing CIL trees. Create instantiations of
   this type by specializing the class {!nopCilVisitor}. *)
class type cilVisitor = object

  method vvdec: varinfo -> varinfo visitAction
    (** Invoked for each variable declaration. The subtrees to be traversed
       are those corresponding to the type and attributes of the variable.
       Note that variable declarations are all the [GVar], [GVarDecl], [GFun],
       all the [varinfo] in formals of function types, and the formals and
       locals for function definitions. This means that the list of formals
       in a function definition will be traversed twice, once as part of the
       function type and second as part of the formals in a function
       definition. *)

  method vvrbl: varinfo -> varinfo visitAction
    (** Invoked on each variable use. Here only the [SkipChildren] and
       [ChangeTo] actions make sense since there are no subtrees. Note that
       the type and attributes of the variable are not traversed for a
       variable use *)

  method vexpr: exp -> exp visitAction
    (** Invoked on each expression occurrence. The subtrees are the
       subexpressions, the types (for a [Cast] or [SizeOf] expression) or the
       variable use. *)

  method vlval: lval -> lval visitAction
    (** Invoked on each lvalue occurrence *)

  method voffs: offset -> offset visitAction
    (** Invoked on each offset occurrence that is *not* as part
        of an initializer list specification, i.e. in an lval or
        recursively inside an offset. *)

  method vinitoffs: offset -> offset visitAction
    (** Invoked on each offset appearing in the list of a
        CompoundInit initializer.  *)

  method vinst: instr -> instr list visitAction
    (** Invoked on each instruction occurrence. The [ChangeTo] action can
       replace this instruction with a list of instructions *)

  method vstmt: stmt -> stmt visitAction
    (** Control-flow statement. *)

  method vblock: block -> block visitAction     (** Block. Replaced in
                                                    place. *)

  method vfunc: fundec -> fundec visitAction    (** Function definition.
                                                    Replaced in place. *)

  method vglob: global -> global list visitAction (** Global (vars, types,
                                                      etc.)  *)

  method vinit: varinfo -> offset -> init -> init visitAction
                                                (** Initializers for globals,
                                                   pass the global where this
                                                   occurs, and the offset *)

  method vtype: typ -> typ visitAction          (** Use of some type. Note
                                                   that for structure/union
                                                   and enumeration types the
                                                   definition of the
                                                   composite type is not
                                                   visited. Use [vglob] to
                                                   visit it.  *)

  method vattr: attribute -> attribute list visitAction
    (** Attribute. Each attribute can be replaced by a list *)

  method vattrparam: attrparam -> attrparam visitAction
    (** Attribute parameters. *)

    (** Add here instructions while visiting to queue them to
       precede the current statement or instruction being processed *)
  method queueInstr: instr list -> unit

    (** Gets the queue of instructions and resets the queue *)
  method unqueueInstr: unit -> instr list

end

(* the default visitor does nothing at each node, but does *)
(* not stop; hence they return true *)
class nopCilVisitor : cilVisitor = object
  method vvrbl (v:varinfo) = DoChildren (* variable *)
  method vvdec (v:varinfo) = DoChildren (* variable
                                                                 declaration *)
  method vexpr (e:exp) = DoChildren   (* expression *)
  method vlval (l:lval) = DoChildren  (* lval (base is 1st
                                                           field)  *)
  method voffs (o:offset) = DoChildren      (* lval or recursive offset *)
  method vinitoffs (o:offset) = DoChildren  (* initializer offset *)
  method vinst (i:instr) = DoChildren       (* imperative instruction *)
  method vstmt (s:stmt) = DoChildren        (* control-flow statement *)
  method vblock (b: block) = DoChildren
  method vfunc (f:fundec) = DoChildren      (* function definition *)
  method vglob (g:global) = DoChildren      (* global (vars, types, etc.) *)
  method vinit (forg: varinfo) (off: offset) (i:init) = DoChildren  (* global initializers *)
  method vtype (t:typ) = DoChildren         (* use of some type *)
  method vattr (a: attribute) = DoChildren
  method vattrparam (a: attrparam) = DoChildren

  val mutable instrQueue = []

  method queueInstr (il: instr list) =
    List.iter (fun i -> instrQueue <- i :: instrQueue) il

  method unqueueInstr () =
    let res = List.rev instrQueue in
    instrQueue <- [];
    res

end

let assertEmptyQueue vis =
  if vis#unqueueInstr () <> [] then
    (* Either a visitor inserted an instruction somewhere that it shouldn't
       have (i.e. at the top level rather than inside of a statement), or
       there's a bug in the visitor engine. *)
    E.s (E.bug "Visitor's instruction queue is not empty.\n  You should only use queueInstr inside a function body!");
  ()


let lu = locUnknown

(* sm: utility *)
let startsWith (prefix: string) (s: string) : bool =
(
  let prefixLen = (String.length prefix) in
  (String.length s) >= prefixLen &&
  (String.sub s 0 prefixLen) = prefix
)

let endsWith (suffix: string) (s: string) : bool =
  let suffixLen = String.length suffix in
  let sLen = String.length s in
  sLen >= suffixLen &&
  (String.sub s (sLen - suffixLen) suffixLen) = suffix

let stripUnderscores (s: string) : string =
  if (startsWith "__" s) && (endsWith "__" s) then
    String.sub s 2 ((String.length s) - 4)
  else
    s

let get_instrLoc (inst : instr) =
  match inst with
      Set(_, _, loc, _) -> loc
    | Call(_, _, _, loc, _) -> loc
    | Asm(_, _, _, _, _, _, loc) -> loc
    | VarDecl(_,loc) -> loc
let get_globalLoc (g : global) =
  match g with
  | GFun(_,l) -> (l)
  | GType(_,l) -> (l)
  | GEnumTag(_,l) -> (l)
  | GEnumTagDecl(_,l) -> (l)
  | GCompTag(_,l) -> (l)
  | GCompTagDecl(_,l) -> (l)
  | GVarDecl(_,l) -> (l)
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
    | ComputedGoto(_, loc) -> loc
    | Break(loc) -> loc
    | Continue(loc) -> loc
    | If(_, _, _, loc, _) -> loc
    | Switch (_, _, _, loc, _) -> loc
    | Loop (_, loc, _, _, _) -> loc
    | Block b -> if b.bstmts == [] then lu
                 else get_stmtLoc ((List.hd b.bstmts).skind)


(* The next variable identifier to use. Counts up *)
let nextGlobalVID = ref 1

(* The next compinfo identifier to use. Counts up. *)
let nextCompinfoKey = ref 1

(* Some error reporting functions *)
let d_loc (_: unit) (loc: location) : doc =
  text loc.file ++ chr ':' ++ num loc.line

let d_thisloc (_: unit) : doc = d_loc () !currentLoc

let error (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    E.hadErrors := true;
    ignore (eprintf "%t: Error: %a@!"
              d_thisloc insert d);
    nil
  in
  Pretty.gprintf f fmt

let unimp (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    E.hadErrors := true;
    ignore (eprintf "%t: Unimplemented: %a@!"
              d_thisloc insert d);
    nil
  in
  Pretty.gprintf f fmt

let bug (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    E.hadErrors := true;
    ignore (eprintf "%t: Bug: %a@!"
              d_thisloc insert d);
    E.showContext ();
    nil
  in
  Pretty.gprintf f fmt

let errorLoc (loc: location) (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    E.hadErrors := true;
    ignore (eprintf "%a: Error: %a@!"
              d_loc loc insert d);
    E.showContext ();
    nil
  in
  Pretty.gprintf f fmt

let warn (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    ignore (eprintf "%t: Warning: %a@!"
              d_thisloc insert d);
    nil
  in
  Pretty.gprintf f fmt


let warnOpt (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    if !E.warnFlag then
      ignore (eprintf "%t: Warning: %a@!"
                d_thisloc insert d);
    nil
  in
  Pretty.gprintf f fmt

let warnContext (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    ignore (eprintf "%t: Warning: %a@!"
              d_thisloc insert d);
    E.showContext ();
    nil
  in
  Pretty.gprintf f fmt

let warnContextOpt (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    if !E.warnFlag then
      ignore (eprintf "%t: Warning: %a@!"
                d_thisloc insert d);
    E.showContext ();
    nil
  in
  Pretty.gprintf f fmt

let warnLoc (loc: location) (fmt : ('a,unit,doc) format) : 'a =
  let f d =
    ignore (eprintf "%a: Warning: %a@!"
              d_loc loc insert d);
    E.showContext ();
    nil
  in
  Pretty.gprintf f fmt

let zero      = Const(CInt(zero_cilint, IInt, None))

(** Given the character c in a (CChr c), sign-extend it to 32 bits.
  (This is the official way of interpreting character constants, according to
  ISO C 6.4.4.4.10, which says that character constants are chars cast to ints)
  Returns CInt(sign-extended c, IInt, None) *)
let charConstToInt (c: char) : constant =
  let c' = Char.code c in
  let value =
    if c' < 128
    then cilint_of_int c'
    else cilint_of_int (c' - 256)
  in
  CInt(value, IInt, None)


(** Convert a 64-bit int to an OCaml int, or raise an exception if that
    can't be done. *)
let i64_to_int (i: int64) : int =
  let i': int = Int64.to_int i in (* i.e. i' = i mod 2^31 *)
  if i = Int64.of_int i' then i'
  else E.s (E.unimp "%a: Int constant too large: %Ld\n" d_loc !currentLoc i)

let cilint_to_int (i: cilint) : int =
  try int_of_cilint i
  with _ -> E.s (E.unimp "%a: Int constant too large: %s\n"
		   d_loc !currentLoc (string_of_cilint i))

let voidType = TVoid([])
let intType = TInt(IInt,[])
let uintType = TInt(IUInt,[])
let longType = TInt(ILong,[])
let ulongType = TInt(IULong,[])
let charType = TInt(IChar, [])
let boolType = TInt(IBool, [])

let charPtrType = TPtr(charType,[])
let charConstPtrType = TPtr(TInt(IChar, [Attr("const", []); Attr("pconst", [])]),[])
let stringLiteralType = charPtrType

let voidPtrType = TPtr(voidType, [])
let intPtrType = TPtr(intType, [])
let uintPtrType = TPtr(uintType, [])
let boolPtrType = TPtr(boolType, [])

let doubleType = TFloat(FDouble, [])


(* An integer type that fits pointers. Initialized by initCIL *)
let upointType = ref voidType

(* An integer type that fits a pointer difference. Initialized by initCIL *)
let ptrdiffType = ref voidType

(* Integer types that fit wchar_t, char16_t, and char32_t. Initialized by initCIL *)
let wcharKind = ref IChar
let wcharType = ref voidType
let char16Kind = ref IChar
let char16Type = ref voidType
let char32Kind = ref IChar
let char32Type = ref voidType


(* An integer type that is the type of sizeof. Initialized by initCIL *)
let typeOfSizeOf = ref voidType
let kindOfSizeOf = ref IUInt

let initCIL_called = ref false

(** Returns true if and only if the given integer type is signed. *)
let isSigned = function
  | IBool
  | IUChar
  | IUShort
  | IUInt
  | IULong
  | IULongLong
  | IUInt128 ->
      false
  | ISChar
  | IShort
  | IInt
  | ILong
  | ILongLong
  | IInt128 ->
      true
  | IChar ->
      not !M.theMachine.M.char_is_unsigned

let mkStmt (sk: stmtkind) : stmt =
  { skind = sk;
    labels = [];
    sid = -1; succs = []; preds = []; fallthrough = None }

let mkBlock (slst: stmt list) : block =
  { battrs = []; bstmts = slst; }

let mkEmptyStmt () = mkStmt (Instr [])
let mkStmtOneInstr (i: instr) = mkStmt (Instr [i])

let dummyInstr = (Asm([], ["dummy statement!!"], [], [], [], [], lu))
let dummyStmt =  mkStmt (Instr [dummyInstr])

let compactStmts (b: stmt list) : stmt list =
      (* Try to compress statements. Scan the list of statements and remember
         the last instrunction statement encountered, along with a Clist of
         instructions in it. *)
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
    | ({skind=Instr il; _} as s) :: rest ->
        let ils = Clist.fromList il in
        if lastinstrstmt != dummyStmt && s.labels == [] then
          compress lastinstrstmt (Clist.append lastinstrs ils) rest
        else
          finishLast (compress s ils rest)

    | {skind=Block b;labels = []; _} :: rest when b.battrs = [] ->
        compress lastinstrstmt lastinstrs (b.bstmts@rest)
    | s :: rest ->
        let res = s :: compress dummyStmt Clist.empty rest in
        finishLast res
  in
  compress dummyStmt Clist.empty b


(** Construct sorted lists of attributes ***)
let rec addAttribute (Attr(an, _) as a: attribute) (al: attributes) =
  let rec insertSorted = function
      [] -> [a]
    | ((Attr(an0, _) as a0) :: rest) as l ->
        if an < an0 then a :: l
        else if Util.equals a a0 then l (* Do not add if already in there *)
        else a0 :: insertSorted rest (* Make sure we see all attributes with
                                        this name *)
  in
  insertSorted al

(** The second attribute list is sorted *)
and addAttributes al0 (al: attributes) : attributes =
    if al0 == [] then al else
    List.fold_left (fun acc a -> addAttribute a acc) al al0

and dropAttribute (an: string) (al: attributes) =
  List.filter (fun (Attr(an', _)) -> an <> an') al

and dropAttributes (anl: string list) (al: attributes) =
  List.fold_left (fun acc an -> dropAttribute an acc) al anl

and filterAttributes (s: string) (al: attribute list) : attribute list =
  List.filter (fun (Attr(an, _)) -> an = s) al

(* sm: *)
let hasAttribute s al =
  (filterAttributes s al <> [])


type attributeClass =
    AttrName  (* Attribute of a name. *)
  | AttrFunType  (* Attribute of a function type. *)
  | AttrType  (* Attribute of a type *)

(* This table contains the mapping of predefined attributes to classes.
   Extend this table with more attributes as you need. This table is used to
   determine how to associate attributes with names or type during cabs2cil
   conversion *)
let attributeHash: (string, attributeClass) H.t =
  let table = H.create 13 in
  List.iter (fun a -> H.add table a AttrName)
    [ "section"; "constructor"; "destructor"; "unused"; "used"; "weak";
      "no_instrument_function"; "alias"; "no_check_memory_usage";
      "exception"; "model"; (* "restrict"; *)
      "aconst"; "__asm__" (* Gcc uses this to specify the name to be used in
                             assembly for a global  *)];

  (* MSVC declspec attributes that are also supported by GCC *)
  List.iter (fun a -> H.add table a AttrName)
    [ "thread"; "naked"; "dllimport"; "dllexport";
      "selectany"; "nothrow"; "property";  "noreturn"; "align" ];

  List.iter (fun a -> H.add table a AttrFunType)
    [ "format"; "regparm"; "longcall";
      "noinline"; "always_inline"; "gnu_inline"; "leaf";
      "artificial"; "warn_unused_result"; "nonnull";
    ];

  List.iter (fun a -> H.add table a AttrFunType)
    [ "stdcall";"cdecl"; "fastcall" ];

  List.iter (fun a -> H.add table a AttrType)
    [ "const"; "volatile"; "restrict"; "mode" ];
  table


(* Partition the attributes into classes *)
let partitionAttributes
    ~(default:attributeClass)
    (attrs:  attribute list) :
    attribute list * attribute list * attribute list =
  let rec loop (n,f,t) = function
      [] -> n, f, t
    | (Attr(an, _) as a) :: rest ->
        match (try H.find attributeHash an with Not_found -> default) with
          AttrName -> loop (addAttribute a n, f, t) rest
        | AttrFunType ->
            loop (n, addAttribute a f, t) rest
        | AttrType -> loop (n, f, addAttribute a t) rest
  in
  loop ([], [], []) attrs


(* Get the full name of a comp *)
let compFullName comp =
  (if comp.cstruct then "struct " else "union ") ^ comp.cname


let missingFieldName = "___missing_field_name"

(** Creates a a (potentially recursive) composite type. Make sure you add a
    GTag for it to the file! **)
let mkCompInfo
      (isstruct: bool)
      (n: string)
      (* fspec is a function that when given a forward
         representation of the structure type constructs the type of
         the fields. The function can ignore this argument if not
         constructing a recursive type.  *)
       (mkfspec: compinfo -> (string * typ * int option * attribute list *
                             location) list)
       (a: attribute list) : compinfo =

  (* make a new name for anonymous structs *)
   if n = "" then
     E.s (E.bug "mkCompInfo: missing structure name\n");
   (* Make a new self cell and a forward reference *)
   let comp =
     { cstruct = isstruct; cname = ""; ckey = 0; cfields = [];
       cattr = a; creferenced = false;
       (* Make this compinfo undefined by default *)
       cdefined = false; }
   in
   comp.cname <- n;
   comp.ckey <- !nextCompinfoKey;
   incr nextCompinfoKey;
   let flds =
       Util.list_map (fun (fn, ft, fb, fa, fl) ->
          { fcomp = comp;
            ftype = ft;
            fname = fn;
            fbitfield = fb;
            fattr = fa;
            floc = fl}) (mkfspec comp) in
   comp.cfields <- flds;
   if flds <> [] then comp.cdefined <- true;
   comp

(** Make a copy of a compinfo, changing the name and the key *)
let copyCompInfo (ci: compinfo) (n: string) : compinfo =
  let ci' = {ci with cname = n;
                     ckey = !nextCompinfoKey; } in
  incr nextCompinfoKey;
  (* Copy the fields and set the new pointers to parents *)
  ci'.cfields <- Util.list_map (fun f -> {f with fcomp = ci'}) ci'.cfields;
  ci'

(**** Utility functions ******)

let rec typeAttrs = function
    TVoid a -> a
  | TInt (_, a) -> a
  | TFloat (_, a) -> a
  | TNamed (t, a) -> addAttributes a (typeAttrs t.ttype)
  | TPtr (_, a) -> a
  | TArray (_, _, a) -> a
  | TComp (comp, a) -> addAttributes comp.cattr a
  | TEnum (enum, a) -> addAttributes enum.eattr a
  | TFun (_, _, _, a) -> a
  | TBuiltin_va_list a -> a

(** [typeAttrs], which doesn't add inner attributes. *)
let typeAttrsOuter = function
  | TVoid a -> a
  | TInt (_, a) -> a
  | TFloat (_, a) -> a
  | TNamed (_, a) -> a
  | TPtr (_, a) -> a
  | TArray (_, _, a) -> a
  | TComp (_, a) -> a
  | TEnum (_, a) -> a
  | TFun (_, _, _, a) -> a
  | TBuiltin_va_list a -> a

let setTypeAttrs t a =
  match t with
    TVoid _ -> TVoid a
  | TInt (i, _) -> TInt (i, a)
  | TFloat (f, _) -> TFloat (f, a)
  | TNamed (t, _) -> TNamed(t, a)
  | TPtr (t', _) -> TPtr(t', a)
  | TArray (t', l, _) -> TArray(t', l, a)
  | TComp (comp, _) -> TComp (comp, a)
  | TEnum (enum, _) -> TEnum (enum, a)
  | TFun (r, args, v, _) -> TFun(r,args,v,a)
  | TBuiltin_va_list _ -> TBuiltin_va_list a


let typeAddAttributes a0 t =
begin
  match a0 with
  | [] ->
      (* no attributes, keep same type *)
      t
  | _ ->
      (* anything else: add a0 to existing attributes *)
      let add (a: attributes) = addAttributes a0 a in
      match t with
        TVoid a -> TVoid (add a)
      | TInt (ik, a) -> TInt (ik, add a)
      | TFloat (fk, a) -> TFloat (fk, add a)
      | TEnum (enum, a) -> TEnum (enum, add a)
      | TPtr (t, a) -> TPtr (t, add a)
      | TArray (t, l, a) -> TArray (t, l, add a)
      | TFun (t, args, isva, a) -> TFun(t, args, isva, add a)
      | TComp (comp, a) -> TComp (comp, add a)
      | TNamed (t, a) -> TNamed (t, add a)
      | TBuiltin_va_list a -> TBuiltin_va_list (add a)
end

let typeRemoveAttributes (anl: string list) t =
  let drop (al: attributes) = dropAttributes anl al in
  match t with
    TVoid a -> TVoid (drop a)
  | TInt (ik, a) -> TInt (ik, drop a)
  | TFloat (fk, a) -> TFloat (fk, drop a)
  | TEnum (enum, a) -> TEnum (enum, drop a)
  | TPtr (t, a) -> TPtr (t, drop a)
  | TArray (t, l, a) -> TArray (t, l, drop a)
  | TFun (t, args, isva, a) -> TFun(t, args, isva, drop a)
  | TComp (comp, a) -> TComp (comp, drop a)
  | TNamed (t, a) -> TNamed (t, drop a)
  | TBuiltin_va_list a -> TBuiltin_va_list (drop a)

(** Partition attributes into type qualifiers and non type qualifiers. *)
let partitionQualifierAttributes al =
  List.partition (function
      | Attr (("const" | "volatile" | "restrict"), []) -> true
      | _ -> false
    ) al

(** Remove top-level type qualifiers from type. *)
let removeOuterQualifierAttributes t =
  let a = typeAttrsOuter t in
  let (_, a') = partitionQualifierAttributes a in
  setTypeAttrs t a'

let unrollType (t: typ) : typ =
  let rec withAttrs (al: attributes) (t: typ) : typ =
    match t with
      TNamed (r, a') -> withAttrs (addAttributes al a') r.ttype
    | x -> typeAddAttributes al x
  in
  withAttrs [] t

let rec unrollTypeDeep (t: typ) : typ =
  let rec withAttrs (al: attributes) (t: typ) : typ =
    match t with
      TNamed (r, a') -> withAttrs (addAttributes al a') r.ttype
    | TPtr(t, a') -> TPtr(unrollTypeDeep t, addAttributes al a')
    | TArray(t, l, a') -> TArray(unrollTypeDeep t, l, addAttributes al a')
    | TFun(rt, args, isva, a') ->
        TFun (unrollTypeDeep rt,
              (match args with
                None -> None
              | Some argl ->
                  Some (Util.list_map (fun (an,at,aa) ->
                  (an, unrollTypeDeep at, aa)) argl)),
              isva,
              addAttributes al a')
    | x -> typeAddAttributes al x
  in
  withAttrs [] t

let isVoidType t =
  match unrollType t with
    TVoid _ -> true
  | _ -> false
let isVoidPtrType t =
  match unrollType t with
    TPtr(tau,_) when isVoidType tau -> true
  | _ -> false

(* get the typ of __real__(e) or __imag__(e) for e of typ t*)
let typeOfRealAndImagComponents t =
  match unrollType t with
  | TInt _ -> t
  | TFloat (fkind, attrs) ->
    let newfkind = function
      | FFloat -> FFloat      (* [float] *)
      | FDouble -> FDouble     (* [double] *)
      | FLongDouble -> FLongDouble (* [long double] *)
      | FFloat128 -> FFloat128
      | FComplexFloat -> FFloat
      | FComplexDouble -> FDouble
      | FComplexLongDouble -> FLongDouble
      | FComplexFloat128 -> FFloat128
    in
    TFloat (newfkind fkind, attrs)
  | _ -> E.s (E.bug "unexpected non-numerical type for argument to __real__/__imag__ ")

(** for an fkind, return the corresponding complex fkind *)
let getComplexFkind = function
  | FFloat -> FComplexFloat
  | FDouble -> FComplexDouble
  | FLongDouble -> FComplexLongDouble
  | FFloat128 -> FComplexFloat128
  | FComplexFloat -> FComplexFloat
  | FComplexDouble -> FComplexDouble
  | FComplexLongDouble -> FComplexLongDouble
  | FComplexFloat128 -> FComplexFloat128

let var vi : lval = (Var vi, NoOffset)
(* let assign vi e = Instrs(Set (var vi, e), lu) *)

let mkString s = Const(CStr (s, No_encoding))


let mkWhile ~(guard:exp) ~(body: stmt list) : stmt list =
  (* Do it like this so that the pretty printer recognizes it *)
  [ mkStmt (Loop (mkBlock (mkStmt (If(guard,
                                      mkBlock [ mkEmptyStmt () ],
                                      mkBlock [ mkStmt (Break lu)], lu, lu)) ::
                           body), lu, lu, None, None)) ]



let mkFor ~(start: stmt list) ~(guard: exp) ~(next: stmt list)
          ~(body: stmt list) : stmt list =
  (start @
     (mkWhile ~guard:guard ~body:(body @ next)))


let mkForIncr ~(iter : varinfo) ~(first: exp) ~stopat:(past: exp) ~(incr: exp)
    ~(body: stmt list) : stmt list =
      (* See what kind of operator we need *)
  let compop, nextop =
    match unrollType iter.vtype with
      TPtr _ -> Lt, PlusPI
    | _ -> Lt, PlusA
  in
  mkFor
    ~start:[ mkStmt (Instr [(Set (var iter, first, lu, lu))]) ]
    ~guard:(BinOp(compop, Lval(var iter), past, intType))
    ~next:[ mkStmt (Instr [(Set (var iter,
                           (BinOp(nextop, Lval(var iter), incr, iter.vtype)),
                           lu, lu))])]
    ~body:body


let rec stripCasts (e: exp) =
  match e with CastE(_, e') -> stripCasts e' | _ -> e



(* the name of the C function we call to get ccgr ASTs
external parse : string -> file = "cil_main"
*)
(*
  Pretty Printing
 *)

let d_ikind () = function
    IChar -> text "char"
  | ISChar -> text "signed char"
  | IUChar -> text "unsigned char"
  | IBool -> text "_Bool"
  | IInt -> text "int"
  | IUInt -> text "unsigned int"
  | IShort -> text "short"
  | IUShort -> text "unsigned short"
  | ILong -> text "long"
  | IULong -> text "unsigned long"
  | ILongLong -> text "long long"
  | IULongLong -> text "unsigned long long"
  | IInt128 -> text "__int128"
  | IUInt128 -> text "unsigned __int128"

let d_fkind () = function
    FFloat -> text "float"
  | FDouble -> text "double"
  | FLongDouble -> text "long double"
  | FFloat128 -> text "_Float128"
  | FComplexFloat -> text "_Complex float"
  | FComplexDouble -> text "_Complex double"
  | FComplexLongDouble -> text "_Complex long double"
  | FComplexFloat128 -> text "_Complex _Float128"

let d_storage () = function
    NoStorage -> nil
  | Static -> text "static "
  | Extern -> text "extern "
  | Register -> text "register "

(* sm: need this value below *)
let mostNeg32BitInt : cilint = cilint_of_string "-0x80000000"
let mostNeg64BitInt : cilint = cilint_of_string "-0x8000000000000000"

let bytesSizeOfInt (ik: ikind): int =
  match ik with
  | IChar | ISChar | IUChar -> 1
  | IBool -> !M.theMachine.M.sizeof_bool
  | IInt | IUInt -> !M.theMachine.M.sizeof_int
  | IShort | IUShort -> !M.theMachine.M.sizeof_short
  | ILong | IULong -> !M.theMachine.M.sizeof_long
  | ILongLong | IULongLong -> !M.theMachine.M.sizeof_longlong
  | IInt128 | IUInt128 -> 16

(* constant *)
let d_const () c =
  match c with
    CInt(_, _, Some s) -> text s (* Always print the text if there is one *)
  | CInt(i, ik, None) ->
      (* We must make sure to capture the type of the constant. For some
         constants this is done with a suffix, for others with a cast prefix.*)
      let suffix : string =
        match ik with
          IUInt -> "U"
        | ILong -> "L"
        | IULong -> "UL"
        | ILongLong -> "LL"
        | IULongLong -> "ULL"
        (* if long long is 128 bit we can use its suffix, otherwise unsupported by GCC, see https://github.com/goblint/cil/issues/41#issuecomment-893291878 *)
        | IInt128  when !M.theMachine.M.sizeof_longlong = 16 -> "LL"
        | IUInt128 when !M.theMachine.M.sizeof_longlong = 16 -> "ULL"
        | _ -> ""
          (* TODO warn/fail? *)
          (* E.s (E.bug "unknown/unsupported suffix") *)
      in
      let prefix : string =
        if suffix <> "" then ""
        else if ik = IInt then ""
        else "(" ^ (sprint ~width:!lineLength (d_ikind () ik)) ^ ")"
      in
      (* Watch out here for negative integers that we should be printing as
         large positive ones *)
      if compare_cilint i zero_cilint < 0 && (not (isSigned ik)) then
        if bytesSizeOfInt ik <> 8 then
          (* I am convinced that we shall never store smaller than 64-bits
             integers in negative form. -- Gabriel *)
          E.s (E.bug "unexpected negative unsigned integer (please report this bug)")
        else
          text (prefix ^ "0x" ^ Z.format "%x" i ^ suffix)
      else (
        if (compare_cilint i mostNeg32BitInt = 0) then
          (* sm: quirk here: if you print -2147483648 then this is two tokens *)
          (* in C, and the second one is too large to represent in a signed *)
          (* int.. so we do what's done in limits.h, and print (-2147483467-1); *)
          (* in gcc this avoids a warning, but it might avoid a real problem *)
          (* on another compiler or a 64-bit architecture *)
          text (prefix ^ "(-0x7FFFFFFF-1)")
        else if (compare_cilint i mostNeg64BitInt = 0) then
          (* The same is true of the largest 64-bit negative. *)
          text (prefix ^ "(-0x7FFFFFFFFFFFFFFF-1)")
        else
          text (prefix ^ (string_of_cilint i ^ suffix))
      )

  | CStr(s, enc) -> let prefix = match enc with No_encoding -> "" | Utf8 -> "u8" in text (prefix ^ "\"" ^ escape_string s ^ "\"")
  | CWStr(s, st) ->
      (* text ("L\"" ^ escape_string s ^ "\"")  *)
      let prefix = match st with Wchar_t -> "L" | Char16_t -> "u" | Char32_t -> "U" in
      (List.fold_left (fun acc elt ->
        acc ++
        if (elt >= Int64.zero &&
            elt <= (Int64.of_int 255)) then
          text (escape_char (Char.chr (Int64.to_int elt)))
        else
          ( text (Printf.sprintf "\\x%LX\"" elt) ++ break ++
            (text "\""))
      ) (text (prefix ^ "\"")) s ) ++ text "\""
      (* we cannot print L"\xabcd" "feedme" as L"\xabcdfeedme" --
         the former has 7 wide characters and the later has 3. *)

  | CChr(c) -> text ("'" ^ escape_char c ^ "'")
  | CReal(_, _, Some s) -> text s
  | CReal(f, fsize, None) ->
      text (string_of_float f) ++
      (match fsize with
         FFloat -> chr 'f'
       | FDouble -> nil
       | FLongDouble -> chr 'L'
       | FFloat128 -> text "F128"
       | FComplexFloat -> text "iF"
       | FComplexDouble -> chr 'i'
       | FComplexLongDouble -> text "iL"
       | FComplexFloat128 -> text "iF128")
  | CEnum(_, s, ei) -> text s


(* Parentheses/precedence level. An expression "a op b" is printed
   parenthesized if its parentheses level is >= that that of its context.
   Identifiers have the lowest level and weakly binding operators (e.g. |)
   have the largest level. The correctness criterion is that a smaller level
   MUST correspond to a stronger precedence! *)
let derefStarLevel = 20
let indexLevel = 20
let arrowLevel = 20
let addrOfLevel = 30
let additiveLevel = 60
let comparativeLevel = 70
let bitwiseLevel = 75
let questionLevel = 100
let getParenthLevel (e: exp) =
  match e with
  | Question _ -> questionLevel
  | BinOp((LAnd | LOr), _,_,_) -> 80
                                        (* Bit operations. *)
  | BinOp((BOr|BXor|BAnd),_,_,_) -> bitwiseLevel (* 75 *)

                                        (* Comparisons *)
  | BinOp((Eq|Ne|Gt|Lt|Ge|Le),_,_,_) ->
      comparativeLevel (* 70 *)
                                        (* Additive. Shifts can have higher
                                           level than + or - but I want
                                           parentheses around them *)
  | BinOp((MinusA|MinusPP|MinusPI|PlusA|
           PlusPI|IndexPI|Shiftlt|Shiftrt),_,_,_)
    -> additiveLevel (* 60 *)

                                        (* Multiplicative *)
  | BinOp((Div|Mod|Mult),_,_,_) -> 40

                                        (* Unary *)
  | Real _ -> 30
  | Imag _ -> 30
  | CastE(_,_) -> 30
  | AddrOf(_) -> 30
  | AddrOfLabel(_) -> 30
  | StartOf(_) -> 30
  | UnOp((Neg|BNot|LNot),_,_) -> 30

                                        (* Lvals *)
  | Lval(Mem _ , _) -> derefStarLevel (* 20 *)
  | Lval(Var _, (Field _|Index _)) -> indexLevel (* 20 *)
  | SizeOf _ | SizeOfE _ | SizeOfStr _ -> 20
  | AlignOf _ | AlignOfE _ -> 20

  | Lval(Var _, NoOffset) -> 0        (* Plain variables *)
  | Const _ -> 0                        (* Constants *)


let getParenthLevelAttrParam (a: attrparam) =
  (* Create an expression of the same shape, and use {!getParenthLevel} *)
  match a with
    AInt _ | AStr _ | ACons _ -> 0
  | ASizeOf _ | ASizeOfE _ | ASizeOfS _ -> 20
  | AAlignOf _ | AAlignOfE _ | AAlignOfS _ -> 20
  | AUnOp (uo, _) -> getParenthLevel (UnOp(uo, zero, intType))
  | ABinOp (bo, _, _) -> getParenthLevel (BinOp(bo, zero, zero, intType))
  | AAddrOf _ -> 30
  | ADot _ | AIndex _ | AStar _ -> 20
  | AQuestion _ -> questionLevel


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

let isScalarType t =
  isArithmeticType t || isPointerType t

let isFunctionType t =
  match unrollType t with
    TFun _ -> true
  | _ -> false

(**** Compute the type of an expression ****)
let rec typeOf (e: exp) : typ =
  match e with
  | Const(CInt (_, ik, _)) -> TInt(ik, [])

    (* Character constants have type int.  ISO/IEC 9899:1999 (E),
       section 6.4.4.4 [Character constants], paragraph 10, if you
       don't believe me. *)
  | Const(CChr _) -> intType

    (* The type of a string is a pointer to characters ! The only case when
       you would want it to be an array is as an argument to sizeof, but we
       have SizeOfStr for that *)
  | Const(CStr (_, _)) -> stringLiteralType

  | Const(CWStr (s,st)) -> TPtr((match st with Wchar_t -> !wcharType | Char16_t -> !char16Type | Char32_t -> !char32Type), [])

  | Const(CReal (_, fk, _)) -> TFloat(fk, [])

  | Const(CEnum(tag, _, ei)) -> typeOf tag
  | Real e -> typeOfRealAndImagComponents @@ typeOf e
  | Imag e -> typeOfRealAndImagComponents @@ typeOf e
  | Lval(lv) -> typeOfLval lv
  | SizeOf _ | SizeOfE _ | SizeOfStr _ -> !typeOfSizeOf
  | AlignOf _ | AlignOfE _ -> !typeOfSizeOf
  | UnOp (_, _, t)
  | BinOp (_, _, _, t)
  | Question (_, _, _, t)
  | CastE (t, _) -> t
  | AddrOf (lv) -> TPtr(typeOfLval lv, [])
  | AddrOfLabel (lv) -> voidPtrType
  | StartOf (lv) -> begin
      match unrollType (typeOfLval lv) with
        TArray (t,_, a) -> TPtr(t, a)
     | _ -> E.s (E.bug "typeOf: StartOf on a non-array")
  end

and typeOfLval = function
    Var vi, off -> typeOffset vi.vtype off
  | Mem addr, off -> begin
      match unrollType (typeOf addr) with
        TPtr (t, _) -> typeOffset t off
      | _ -> E.s (bug "typeOfLval: Mem on a non-pointer (%a)" !pd_exp addr)
  end

and typeOffset basetyp =
  let blendAttributes baseAttrs =
    let (_, _, contageous) =
      partitionAttributes ~default:AttrName baseAttrs in
    typeAddAttributes contageous
  in
  function
    NoOffset -> basetyp
  | Index (_, o) -> begin
      match unrollType basetyp with
        TArray (t, _, baseAttrs) ->
	  let elementType = typeOffset t o in
	  blendAttributes baseAttrs elementType
      | t -> E.s (E.bug "typeOffset: Index on a non-array")
  end
  | Field (fi, o) ->
      match unrollType basetyp with
        TComp (_, baseAttrs) ->
	  let fieldType = typeOffset fi.ftype o in
	  blendAttributes baseAttrs fieldType
      | _ -> E.s (bug "typeOffset: Field on a non-compound")


(**
 **
 ** MACHINE DEPENDENT PART
 **
 **)
exception SizeOfError of string * typ


let unsignedVersionOf (ik:ikind): ikind =
  match ik with
  | ISChar | IChar -> IUChar
  | IShort -> IUShort
  | IInt -> IUInt
  | ILong -> IULong
  | ILongLong -> IULongLong
  | IInt128 -> IUInt128
  | _ -> ik

let signedVersionOf (ik:ikind): ikind =
  match ik with
  | IUChar | IChar -> ISChar
  | IUShort -> IShort
  | IUInt -> IInt
  | IULong -> ILong
  | IULongLong -> ILongLong
  | IUInt128 -> IInt128
  | _ -> ik

(* Return the integer conversion rank of an integer kind *)
let intRank (ik:ikind) : int =
  match ik with
  | IBool -> 0
  | IChar | ISChar | IUChar -> 1
  | IShort | IUShort -> 2
  | IInt | IUInt -> 3
  | ILong | IULong -> 4
  | ILongLong | IULongLong -> 5
  | IInt128 | IUInt128 -> 6

(* Return the common integer kind of the two integer arguments, as
   defined in ISO C 6.3.1.8 ("Usual arithmetic conversions") *)
let commonIntKind (ik1:ikind) (ik2:ikind) : ikind =
  let r1 = intRank ik1 in
  let r2 = intRank ik2 in
  if (isSigned ik1) = (isSigned ik2) then begin
    (* Both signed or both unsigned. *)
    if r1 > r2 then ik1 else ik2
  end
  else begin
    let signedKind, unsignedKind, signedRank, unsignedRank =
      if isSigned ik1 then ik1, ik2, r1, r2 else ik2, ik1, r2, r1
    in
    (* The rules for signed + unsigned get hairy.
       (unsigned short + long) is converted to signed long,
       but (unsigned int + long) is converted to unsigned long.*)
    if unsignedRank >= signedRank then unsignedKind
    else if (bytesSizeOfInt signedKind) > (bytesSizeOfInt unsignedKind) then
      signedKind
    else
      unsignedVersionOf signedKind
  end

let intKindForSize (s:int) (unsigned:bool) : ikind =
  if unsigned then
    (* Test the most common sizes first *)
    if s = 1 then IUChar
    else if s = !M.theMachine.M.sizeof_int then IUInt
    else if s = !M.theMachine.M.sizeof_long then IULong
    else if s = !M.theMachine.M.sizeof_short then IUShort
    else if s = !M.theMachine.M.sizeof_longlong then IULongLong
    else if s = 16 then IUInt128
    else raise Not_found
  else
    (* Test the most common sizes first *)
    if s = 1 then ISChar
    else if s = !M.theMachine.M.sizeof_int then IInt
    else if s = !M.theMachine.M.sizeof_long then ILong
    else if s = !M.theMachine.M.sizeof_short then IShort
    else if s = !M.theMachine.M.sizeof_longlong then ILongLong
    else if s = 16 then IInt128
    else raise Not_found

let floatKindForSize (s:int) =
  if s = !M.theMachine.M.sizeof_double then FDouble
  else if s = !M.theMachine.M.sizeof_float then FFloat
  else if s = !M.theMachine.M.sizeof_longdouble then FLongDouble
  else if s = !M.theMachine.M.sizeof_float128 then FFloat128
  else raise Not_found

(* Represents an integer as for a given kind.  Returns a flag saying
   whether any "interesting" bits were lost during truncation. By
   "interesting", we mean that the lost bits were not all-0 or all-1. *)
let truncateCilint (k: ikind) (i: cilint) : cilint * truncation =
  (* TODO: What is this "truncation"? The standard defines no such notion
     and the _Bool reference is about conversions (casts). *)
  (* Truncations to _Bool are special: they behave like "!= 0"
     ISO C99 6.3.1.2 *)
  if k = IBool then
    if is_zero_cilint i then
      zero_cilint, NoTruncation
    else
      one_cilint, NoTruncation
  else
    let nrBits = 8 * (bytesSizeOfInt k) in
    if isSigned k then
      truncate_signed_cilint i nrBits
    else
      truncate_unsigned_cilint i nrBits

let mkCilintIk (ik:ikind) (i:cilint) : cilint =
  fst (truncateCilint ik i)

let mkCilint (ik:ikind) (i:int64) : cilint =
 mkCilintIk ik (cilint_of_int64 i)


(* Construct an integer constant with possible truncation *)
let kintegerCilintString (k: ikind) (i: cilint) (s:string option): exp =
  let i', truncated = truncateCilint k i in
  if truncated = BitTruncation && !warnTruncate then
    ignore (warnOpt "Truncating integer %s to %s"
              (string_of_cilint i) (string_of_cilint i'));
  Const (CInt(i', k, s))

let kintegerCilint (k: ikind) (i: cilint) : exp =
  kintegerCilintString k i None

(* Construct an integer constant with possible truncation *)
let kinteger64 (k: ikind) (i: int64) : exp =
  kintegerCilint k (cilint_of_int64 i)

(* Construct an integer of a given kind. *)
let kinteger (k: ikind) (i: int) =
  kintegerCilint k (cilint_of_int i)

(** Construct an integer of kind IInt. On targets where C's 'int' is 16-bits,
    the integer may get truncated. *)
let integer (i: int) = kinteger IInt i

let one       = integer 1
let mone      = integer (-1)

(* True if the integer fits within the kind's range *)
let fitsInInt (k: ikind) (i: cilint) : bool =
  if k = IBool then (
    (* truncateCilint is weirdly defined for IBool, like it always fits *)
    is_zero_cilint i || compare_cilint i one_cilint = 0 (* only 0 and 1 fit *)
  )
  else (
    let _, truncated = truncateCilint k i in
    truncated = NoTruncation
  )

(* Return the smallest kind that will hold the integer's value.  The
   kind will be unsigned if the 2nd argument is true, signed
   otherwise.  Note that if the value doesn't fit in any of the
   available types, you will get ILongLong (2nd argument false) or
   IULongLong (2nd argument true). *)
let intKindForValue (i: cilint) (unsigned: bool) =
  if unsigned then
    if fitsInInt IBool i then IBool
    else if fitsInInt IUChar i then IUChar
    else if fitsInInt IUShort i then IUShort
    else if fitsInInt IUInt i then IUInt
    else if fitsInInt IULong i then IULong
    else if fitsInInt IUInt128 i then IUInt128
    else IULongLong (* warn, IUInt128? *)
  else
    if fitsInInt ISChar i then ISChar
    else if fitsInInt IShort i then IShort
    else if fitsInInt IInt i then IInt
    else if fitsInInt ILong i then ILong
    else if fitsInInt IInt128 i then IInt128
    else ILongLong (* warn, IInt128? *)

(** If the given expression is an integer constant or a CastE'd
    integer constant, return that constant's value as an ikind, int64 pair.
    Otherwise return None. *)
let rec getInteger (e:exp) : cilint option =
  match e with
  | Const(CInt (n, ik, _)) -> Some (mkCilintIk ik n)
  | Const(CChr c) -> getInteger (Const (charConstToInt c))
  | Const(CEnum(v, _, _)) -> getInteger v
  | CastE(t, e) -> begin
      (* Handle any truncation due to cast. We optimistically ignore
	 loss-of-precision due to floating-point casts. *)
      let mkInt ik n = Some (fst (truncateCilint ik n)) in
      match unrollType t, getInteger e with
      | TInt (ik, _), Some n -> mkInt ik n
      (* "integer constant expressions" may not cast to ptr *)
      | TEnum (ei, _), Some n -> mkInt ei.ekind n
      | TFloat _, v -> v
      | _, _ -> None
    end
  | _ -> None

(** Return the (wrapped) constant i if it fits into ik without any signed overflow,
    otherwise return fallback  *)
let const_if_not_overflow fallback ik i =
  if not (isSigned ik) then
    kintegerCilint ik i
  else
    let i', trunc = truncateCilint ik i in
    if trunc = NoTruncation then
      kintegerCilint ik i
    else
      fallback

let isZero (e: exp) : bool =
  match getInteger e with
  | Some n -> is_zero_cilint n
  | _ -> false

type offsetAcc =
    { oaFirstFree: int;        (* The first free bit *)
      oaLastFieldStart: int;   (* Where the previous field started *)
      oaLastFieldWidth: int;   (* The width of the previous field. Might not
                                  be same as FirstFree - FieldStart because
                                  of internal padding *)
      oaPrevBitPack: (int * ikind * int) option; (* If the previous fields
                                                     were packed bitfields,
                                                     the bit where packing
                                                     has started, the ikind
                                                     of the bitfield and the
                                                     width of the ikind *)
    }

(* Hack to prevent infinite recursion in alignments *)
let ignoreAlignmentAttrs = ref false

(* Get the minimum alignment in bytes for a given type *)
let rec alignOf_int t =
  let alignOfType () =
    match t with
    | TInt((IChar|ISChar|IUChar), _) -> 1
    | TInt(IBool, _) -> !M.theMachine.M.alignof_bool
    | TInt((IShort|IUShort), _) -> !M.theMachine.M.alignof_short
    | TInt((IInt|IUInt), _) -> !M.theMachine.M.alignof_int
    | TInt((ILong|IULong), _) -> !M.theMachine.M.alignof_long
    | TInt((ILongLong|IULongLong), _) -> !M.theMachine.M.alignof_longlong
    | TInt((IInt128|IUInt128), _) -> 16 (* not generated since not all architectures support 128bit ints and the value should be the same for those that do *)
    | TEnum(ei, _) -> alignOf_int (TInt(ei.ekind, []))
    | TFloat(FFloat, _) -> !M.theMachine.M.alignof_float
    | TFloat(FDouble, _) -> !M.theMachine.M.alignof_double
    | TFloat(FLongDouble, _) -> !M.theMachine.M.alignof_longdouble
    | TFloat(FFloat128, _) -> !M.theMachine.M.alignof_float128
    | TFloat(FComplexFloat, _) -> !M.theMachine.M.alignof_floatcomplex
    | TFloat(FComplexDouble, _) -> !M.theMachine.M.alignof_doublecomplex
    | TFloat(FComplexLongDouble, _) -> !M.theMachine.M.alignof_longdoublecomplex
    | TFloat(FComplexFloat128, _) -> !M.theMachine.M.alignof_float128complex
    | TNamed (t, _) -> alignOf_int t.ttype
    | TArray (t, _, _) -> alignOf_int t
    | TPtr _ | TBuiltin_va_list _ -> !M.theMachine.M.alignof_ptr

    (* For composite types get the maximum alignment of any field inside *)
    | TComp (c, _) ->
        (* On GCC the zero-width fields do not contribute to the alignment. *)
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
                GCC *)
             if f.fbitfield = Some 0 then sofar else
               max sofar (alignOfField f)) 1 fields
          (* These are some error cases *)
    | TFun _ -> !M.theMachine.M.alignof_fun
    | TVoid _ as t -> raise (SizeOfError ("void", t))
  in
  match filterAttributes "aligned" (typeAttrs t) with
    [] ->
      (* no __aligned__ attribute, so get the default alignment *)
      alignOfType ()
  | _ when !ignoreAlignmentAttrs ->
      ignore (warn "ignoring recursive align attributes on %a"
                (!pd_type) t);
      alignOfType ()
  | (Attr(_, [a]) as at)::rest -> begin
      if rest <> [] then
        ignore (warn "ignoring duplicate align attributes on %a"
                  (!pd_type) t);
      match intOfAttrparam a with
        Some n -> n
      | None ->
          ignore (warn "alignment attribute \"%a\" not understood on %a"
                    (!pd_attr) at (!pd_type) t);
          alignOfType ()
    end
   | Attr(_, [])::rest ->
       (* aligned with no arg means a power of two at least as large as
          any alignment on the system.*)
       if rest <> [] then
         ignore(warn "ignoring duplicate align attributes on %a"
                  (!pd_type) t);
       !M.theMachine.M.alignof_aligned
  | at::_ ->
      ignore (warn "alignment attribute \"%a\" not understood on %a"
                (!pd_attr) at (!pd_type) t);
      alignOfType ()

(* alignment of a possibly-packed struct field. *)
and alignOfField (fi: fieldinfo) =
  let fieldIsPacked = hasAttribute "packed" fi.fattr
                      || hasAttribute "packed" fi.fcomp.cattr in
  if fieldIsPacked then 1
  else alignOf_int fi.ftype

and intOfAttrparam (a:attrparam) : int option =
  let rec doit a : int =
    match a with
      AInt(n) -> n
    | ABinOp(Shiftlt, a1, a2) -> (doit a1) lsl (doit a2)
    | ABinOp(Div, a1, a2) -> (doit a1) / (doit a2)
    | ASizeOf(t) ->
        let bs = bitsSizeOf t in
        bs / 8
    | AAlignOf(t) ->
        alignOf_int t
    | _ -> raise (SizeOfError ("", voidType))
  in
  (* Use ignoreAlignmentAttrs here to prevent stack overflow if a buggy
     program does something like
             struct s {...} __attribute__((aligned(sizeof(struct s))))
     This is too conservative, but it's often enough.
  *)
  assert (not !ignoreAlignmentAttrs);
  ignoreAlignmentAttrs := true;
  try
    let n = doit a in
    ignoreAlignmentAttrs := false;
    Some n
  with SizeOfError _ -> (* Can't compile *)
    ignoreAlignmentAttrs := false;
    None


(* GCC version *)
(* Does not use the sofar.oaPrevBitPack *)
and offsetOfFieldAcc_GCC
                         (fi: fieldinfo)
                         (sofar: offsetAcc) : offsetAcc =
  (* field type *)
  let ftype = unrollType fi.ftype in
  let ftypeAlign = 8 * alignOfField fi in
  let ftypeBits = bitsSizeOf ftype in
  match ftype, fi.fbitfield with
    (* A width of 0 means that we must end the current packing. It seems that
       GCC pads only up to the alignment boundary for the type of this field.
       *)
  | _, Some 0 ->
      let firstFree      = addTrailing sofar.oaFirstFree ftypeAlign in
      { oaFirstFree      = firstFree;
        oaLastFieldStart = firstFree;
        oaLastFieldWidth = 0;
        oaPrevBitPack    = None }

    (* A bitfield cannot span more alignment boundaries of its type than the
       type itself *)
  | _, Some wdthis
      when (sofar.oaFirstFree + wdthis + ftypeAlign - 1) / ftypeAlign
            - sofar.oaFirstFree / ftypeAlign > ftypeBits / ftypeAlign ->
          let start = addTrailing sofar.oaFirstFree ftypeAlign in
          { oaFirstFree      = start + wdthis;
            oaLastFieldStart = start;
            oaLastFieldWidth = wdthis;
            oaPrevBitPack    = None }

   (* Try a simple method. Just put the field down *)
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

and offsetOfFieldAcc ~(fi: fieldinfo)
                     ~(sofar: offsetAcc) : offsetAcc =
  offsetOfFieldAcc_GCC fi sofar

(* The size of a type, in bits. If a struct or array, then trailing padding is
   added *)
and bitsSizeOf t =
  if not !initCIL_called then
    E.s (E.error "You did not call Cil.initCIL before using the CIL library");
  match t with
  | TInt (ik,_) -> 8 * (bytesSizeOfInt ik)
  | TFloat(FDouble, _) -> 8 * !M.theMachine.M.sizeof_double
  | TFloat(FLongDouble, _) -> 8 * !M.theMachine.M.sizeof_longdouble
  | TFloat(FFloat128, _) -> 8 * !M.theMachine.M.sizeof_float128
  | TFloat(FFloat, _) -> 8 * !M.theMachine.M.sizeof_float
  | TFloat(FComplexDouble, _) ->  8 * !M.theMachine.M.sizeof_doublecomplex
  | TFloat(FComplexLongDouble, _) -> 8 * !M.theMachine.M.sizeof_longdoublecomplex
  | TFloat(FComplexFloat128, _) -> 8 * !M.theMachine.M.sizeof_float128complex
  | TFloat(FComplexFloat, _) -> 8 * !M.theMachine.M.sizeof_floatcomplex
  | TEnum (ei, _) -> bitsSizeOf (TInt(ei.ekind, []))
  | TPtr _ -> 8 * !M.theMachine.M.sizeof_ptr
  | TBuiltin_va_list _ -> 8 * !M.theMachine.M.sizeof_ptr
  | TNamed (t, _) -> bitsSizeOf t.ttype
  | TComp (comp, _) when comp.cfields == [] -> begin
      if not comp.cdefined then
        raise (SizeOfError ("abstract type", t)) (*abstract type*)
      else
        0
  end

  | TComp (comp, _) when comp.cstruct -> (* Struct *)
        (* Go and get the last offset *)
      let startAcc =
        { oaFirstFree = 0;
          oaLastFieldStart = 0;
          oaLastFieldWidth = 0;
          oaPrevBitPack = None;
        } in
      let lastoff =
        List.fold_left (fun acc fi -> offsetOfFieldAcc ~fi ~sofar:acc)
          startAcc comp.cfields
      in
      (* Drop e.g. the align attribute from t.  For this purpose,
          consider only the attributes on comp itself.*)
      let structAlign = 8 * alignOf_int
                          (TComp (comp, [])) in
      addTrailing lastoff.oaFirstFree structAlign

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
          let lastoff = offsetOfFieldAcc ~fi ~sofar:startAcc in
          if lastoff.oaFirstFree > acc then
            lastoff.oaFirstFree else acc) 0 comp.cfields in
        (* Add trailing by simulating adding an extra field *)
      addTrailing max (8 * alignOf_int t)

  | TArray(bt, Some len, _) -> begin
      match constFold true len with
        Const(CInt(l,lk,_)) ->
	  let sz = mul_cilint (mkCilintIk lk l) (cilint_of_int (bitsSizeOf bt)) in
          (* Check for overflow.
             There are other places in these cil.ml that overflow can occur,
             but this multiplication is the most likely to be a problem. *)
          if not (is_int_cilint sz) then
            raise (SizeOfError ("Array is so long that its size can't be "
                                  ^"represented with an OCaml int.", t))
          else
            addTrailing (int_of_cilint sz) (8 * alignOf_int t)
      | _ -> raise (SizeOfError ("array non-constant length", t))
  end


  | TVoid _ -> 8 * !M.theMachine.M.sizeof_void
  | TFun _ -> (* On GCC the size of a function is defined *)
      8 * !M.theMachine.M.sizeof_fun

  | TArray (_, None, _) -> (* it seems that on GCC the size of such an
                              array is 0 *)
      0


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
          match getInteger e with
            Some i -> cilint_to_int i
          | None -> raise (SizeOfError ("index not constant", baset))
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
        (* Construct a list of fields preceding and including this one *)
        let prevflds =
          let rec loop = function
              [] -> E.s (E.bug "bitsOffset: Cannot find field %s in %s\n"
                           f.fname f.fcomp.cname)
            | fi' :: _ when fi' == f -> [fi']
            | fi' :: rest -> fi' :: loop rest
          in
          loop f.fcomp.cfields
        in
        let lastoff =
          List.fold_left (fun acc fi' -> offsetOfFieldAcc ~fi:fi' ~sofar:acc)
            { oaFirstFree      = 0; (* Start at 0 because each struct is done
                                       separately *)
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




(** Do constant folding on an expression. If the first argument is true then
    will also compute compiler-dependent expressions such as sizeof.
    See also {!constFoldVisitor}, which will run constFold on all
    expressions in a given AST node.*)
and constFold (machdep: bool) (e: exp) : exp =
  match e with
    BinOp(bop, e1, e2, tres) -> constFoldBinOp machdep bop e1 e2 tres
  | UnOp(unop, e1, tres) -> begin
      try
        let tk =
          match unrollType tres with
          | TInt(ik, _) -> ik
          | TEnum (ei, _) -> ei.ekind
          | _ -> raise Not_found (* probably a float *)
        in
        match constFold machdep e1 with
        |  Const(CInt(i,ik,s)) -> begin
	         let ic = mkCilintIk ik i in
            match unop with
              Neg -> const_if_not_overflow (UnOp(Neg,Const(CInt(i,ik,s)),tres)) tk (neg_cilint ic)
            | BNot -> const_if_not_overflow (UnOp(BNot,Const(CInt(i,ik,s)),tres)) tk (lognot_cilint ic)
            | LNot -> if is_zero_cilint ic then one else zero
            end
        | e1c -> UnOp(unop, e1c, tres)
      with Not_found -> e
  end
        (* Characters are integers *)
  | Const(CChr c) -> Const(charConstToInt c)
  | Const(CEnum (v, _, _)) -> constFold machdep v
  | SizeOf t when machdep -> begin
      try
        let bs = bitsSizeOf t in
        kinteger !kindOfSizeOf (bs / 8)
      with SizeOfError _ -> e
  end
  | SizeOfE e when machdep -> constFold machdep (SizeOf (typeOf e))
  | SizeOfStr s when machdep -> kinteger !kindOfSizeOf (1 + String.length s)
  | AlignOf t when machdep -> kinteger !kindOfSizeOf (alignOf_int t)
  | AlignOfE e when machdep -> begin
      (* The alignment of an expression is not always the alignment of its
         type. I know that for strings this is not true *)
      match e with
        Const (CStr _) ->
          kinteger !kindOfSizeOf !M.theMachine.M.alignof_str
            (* For an array, it is the alignment of the array ! *)
      | _ -> constFold machdep (AlignOf (typeOf e))
  end

  | CastE(it,
          AddrOf (Mem (CastE(TPtr(bt, _), z)), off))
    when machdep && isZero z -> begin
      try
        let start, width = bitsOffset bt off in
        if start mod 8 <> 0 then
          E.s (error "Using offset of bitfield");
        constFold machdep (CastE(it, (kinteger !kindOfSizeOf (start / 8))))
      with SizeOfError _ -> e
  end


  | CastE (t, e) -> begin
      match constFold machdep e, unrollType t with
        (* Might truncate silently *)
      | Const(CInt(i,k,_)), TInt(nk,a)
          (* It's okay to drop a cast to const.
             If the cast has any other attributes, leave the cast alone. *)
          when (dropAttributes ["const"; "pconst"] a) = [] ->
          let i', _ = truncateCilint nk (mkCilintIk k i) in
          Const(CInt(i', nk, None))
      | e', _ -> CastE (t, e')
  end
  | Lval lv -> Lval (constFoldLval machdep lv)
  | AddrOf lv -> AddrOf (constFoldLval machdep lv)
  | StartOf lv -> StartOf (constFoldLval machdep lv)
  | _ -> e

and constFoldLval machdep (host,offset) =
  let newhost =
    match host with
    | Mem e -> Mem (constFold machdep e)
    | Var _ -> host
  in
  let rec constFoldOffset machdep = function
    | NoOffset -> NoOffset
    | Field (fi,offset) -> Field (fi, constFoldOffset machdep offset)
    | Index (exp,offset) -> Index (constFold machdep exp,
                                   constFoldOffset machdep offset)
  in
  (newhost, constFoldOffset machdep offset)

and constFoldBinOp (machdep: bool) bop e1 e2 tres =
  let e1' = constFold machdep e1 in
  let e2' = constFold machdep e2 in
  if isIntegralType tres then begin
    let newe =
      let tk =
        match unrollType tres with
          TInt(ik, _) -> ik
        | TEnum (ei, _) -> ei.ekind
        | _ -> E.s (bug "constFoldBinOp")
      in
      let collapse0 () = kinteger tk 0 in
      let collapse e = e (*mkCast e tres*) in
      let shiftInBounds i2 =
         (* We only try to fold shifts if the second arg is positive and
            less than the size of the type of the first argument.
            Otherwise, the semantics are processor-dependent, so let the
            compiler sort it out. *)
        if machdep then
          try
            compare_cilint i2 zero_cilint >= 0 && compare_cilint i2 (cilint_of_int (bitsSizeOf (typeOf e1'))) < 0
          with SizeOfError _ -> false
        else false
      in
      let no_ov = const_if_not_overflow (BinOp(bop, e1', e2', tres)) tk in
      (* Assume that the necessary promotions have been done *)
      match bop, getInteger e1', getInteger e2' with
      | PlusA, Some i1, Some i2 -> no_ov (add_cilint i1 i2)
      | PlusA, Some z, _ when is_zero_cilint z -> collapse e2'
      | PlusA, _, Some z when is_zero_cilint z -> collapse e1'
      | MinusA, Some i1, Some i2 -> no_ov (sub_cilint i1 i2)
      | MinusA, _, Some z when is_zero_cilint z -> collapse e1'
      | Mult, Some i1, Some i2 -> no_ov (mul_cilint i1 i2)
      | Mult, Some z, _ when is_zero_cilint z -> collapse0 ()
      | Mult, _, Some z when is_zero_cilint z -> collapse0 ()
      | Mult, Some o, _ when compare_cilint o one_cilint = 0 -> collapse e2'
      | Mult, _, Some o when compare_cilint o one_cilint = 0 -> collapse e1'
      | Div, Some i1, Some i2 -> begin
          try no_ov (div0_cilint i1 i2)
          with Division_by_zero -> BinOp(bop, e1', e2', tres)
	      end
      | Div, _, Some o when compare_cilint o one_cilint = 0 -> collapse e1'
      | Mod, Some i1, Some i2 -> begin
          try no_ov (rem_cilint i1 i2)
          with Division_by_zero -> BinOp(bop, e1', e2', tres)
	      end
      | Mod, _, Some o when compare_cilint o one_cilint = 0 -> collapse0 ()

      | BAnd, Some i1, Some i2 -> kintegerCilint tk (logand_cilint i1 i2)
      | BAnd, Some z, _ when is_zero_cilint z -> collapse0 ()
      | BAnd, _, Some z when is_zero_cilint z -> collapse0 ()
      | BOr, Some i1, Some i2 -> kintegerCilint tk (logor_cilint i1 i2)
      | BOr, Some z, _ when is_zero_cilint z -> collapse e2'
      | BOr, _, Some z when is_zero_cilint z -> collapse e1'
      | BXor, Some i1, Some i2 -> kintegerCilint tk (logxor_cilint i1 i2)
      | BXor, Some z, _ when is_zero_cilint z -> collapse e2'
      | BXor, _, Some z when is_zero_cilint z -> collapse e1'

      (* C99 6.5.7 (4) *)
      | Shiftlt, Some i1, Some i2 when shiftInBounds i2 && not @@ isSigned tk ->
          kintegerCilint tk (shift_left_cilint i1 (int_of_cilint i2))
      | Shiftlt, Some i1, Some i2 when compare_cilint i1 zero_cilint >= 0 && shiftInBounds i2 ->
          (* i1 has signed type and is non-negative *)
          const_if_not_overflow (BinOp(bop, e1', e2', tres)) tk (shift_left_cilint i1 (int_of_cilint i2))
      | Shiftlt, Some z, _ when is_zero_cilint z -> collapse0 ()
      | Shiftlt, _, Some z when is_zero_cilint z && not @@ isSigned tk-> collapse e1'
      | Shiftrt, Some i1, Some i2 when shiftInBounds i2 ->
          kintegerCilint tk (shift_right_cilint i1 (int_of_cilint i2))
      | Shiftrt, Some z, _ when is_zero_cilint z -> collapse0 ()
      | Shiftrt, _, Some z when is_zero_cilint z -> collapse e1'

      | Eq, Some i1, Some i2 -> if compare_cilint i1 i2 = 0 then one else zero
      | Ne, Some i1, Some i2 -> if compare_cilint i1 i2 <> 0 then one else zero
      | Le, Some i1, Some i2 -> if compare_cilint i1 i2 <= 0 then one else zero
      | Ge, Some i1, Some i2 -> if compare_cilint i1 i2 >= 0 then one else zero
      | Lt, Some i1, Some i2 -> if compare_cilint i1 i2 < 0 then one else zero
      | Gt, Some i1, Some i2 -> if compare_cilint i1 i2 > 0 then one else zero

      | LAnd, Some i1, _  when !removeBranchingOnConstants -> if is_zero_cilint i1 then collapse0 () else collapse e2'
      | LAnd, _, Some i2 when !removeBranchingOnConstants -> if is_zero_cilint i2 then collapse0 () else collapse e1'
      | LOr, Some i1, _ when !removeBranchingOnConstants -> if is_zero_cilint i1 then collapse e2' else one
      | LOr, _, Some i2 when !removeBranchingOnConstants -> if is_zero_cilint i2 then collapse e1' else one

      | _ -> BinOp(bop, e1', e2', tres)
    in
    if debugConstFold then
      ignore (E.log "Folded %a to %a\n"
                (!pd_exp) (BinOp(bop, e1', e2', tres)) (!pd_exp) newe);
    newe
  end else
    BinOp(bop, e1', e2', tres)

let isArrayType t =
  match unrollType t with
    TArray _ -> true
  | _ -> false

(** 6.3.2.3 subsection 3
    An integer constant expr with value 0, or such an expr cast to void *, is called a null pointer constant. *)
let isNullPtrConstant e =
  let rec isNullPtrConstant = function
    | CastE (TPtr (TVoid [], []), e) -> isNullPtrConstant e (* no qualifiers allowed on void or ptr *)
    | e -> isZero e
  in
  isNullPtrConstant (constFold true e)

let rec isConstant = function
  | Const _ -> true
  | UnOp (_, e, _) -> isConstant e
  | BinOp (_, e1, e2, _) -> isConstant e1 && isConstant e2
  | Question (e1, e2, e3, _) -> isConstant e1 && isConstant e2 && isConstant e3
  | Lval (Var vi, NoOffset) ->
      (vi.vglob && isArrayType vi.vtype || isFunctionType vi.vtype)
  | Lval _ -> false
  | Real e -> isConstant e
  | Imag e -> isConstant e
  | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ -> true
  | CastE (_, e) -> isConstant e
  | AddrOf (Var vi, off) | StartOf (Var vi, off)
        -> vi.vglob && isConstantOffset off
  | AddrOf (Mem e, off) | StartOf(Mem e, off)
        -> isConstant e && isConstantOffset off
  | AddrOfLabel _ -> true
and isConstantOffset = function
    NoOffset -> true
  | Field(fi, off) -> isConstantOffset off
  | Index(e, off) -> isConstant e && isConstantOffset off

let parseInt (str: string) : exp =
  let hasSuffix str suff =
    let l = String.length str in
    let lsuff = String.length suff in
    l >= lsuff && suff = String.uppercase_ascii (String.sub str (l - lsuff) lsuff)
  in
  let l = String.length str in
  (* See if it is octal or hex *)
  let octalhex = (l >= 1 && String.get str 0 = '0') in
  (* The length of the suffix and a list of possible kinds. See ISO
    6.4.4.1 *)
  let hasSuffix = hasSuffix str in
  let suffixlen, kinds = (* 128bit constants are only supported if long long is also 128bit, so we can parse those as long long *)
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
      0, if octalhex then [IInt; IUInt; ILong; IULong; ILongLong; IULongLong]
      else if not (c99Mode ()) then [ IInt; ILong; IULong; ILongLong; IULongLong]
      else [IInt; ILong; ILongLong]
      (* c99mode only affects parsing of decimal integer constants without suffix
          a) on machines where long and long long do not have the same size
             (e.g. 32 Bit machines, 64 Bit Windows, not 64 Bit MacOS or (most? all?) 64 Bit Linux:
             giving constants that are bigger than max long type long long in c99mode vs. unsigned long
             if c99mode is off.
          b) for constants bigger than long long producing a "Unimplemented: Cannot represent the integer"
             warning in C99 mode vs. unsigned long long if c99mode is off. *)
  in
  let start, base =
    if octalhex then
      if l >= 2 && (let c = String.get str 1 in c = 'x' || c = 'X') then
        2, 16
      else
        1, 8
    else
     0, 10
  in
  let t = String.sub str start (String.length str - start - suffixlen) in
  (* Normal Z.of_string does not work here as 0 is not recognized as the prefix for octal here *)
  let i = Z.of_string_base base t in
  try
    (* Construct an integer of the first kinds that fits. i must be POSITIVE  *)
    let ik = List.find (fun ik -> fitsInInt ik i) kinds in
    kintegerCilintString ik i (Some str)
  with Not_found ->
    E.s (E.unimp "Cannot represent the integer %s\n" (string_of_cilint i))


let d_unop () u =
  match u with
    Neg -> text "-"
  | BNot -> text "~"
  | LNot -> text "!"

let d_binop () b =
  match b with
    PlusA | PlusPI | IndexPI -> text "+"
  | MinusA | MinusPP | MinusPI -> text "-"
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
  | LAnd -> text "&&"
  | LOr -> text "||"

let invalidStmt = mkStmt (Instr [])

(** Construct a hash with the builtins *)
let builtinFunctions : (string, typ * typ list * bool) H.t =
  H.create 49

(* Initialize the builtin functions after the machine has been initialized. *)
let initGccBuiltins () : unit =
  if not !initCIL_called then
    E.s (bug "Call initCIL before initGccBuiltins");
  if H.length builtinFunctions <> 0 then
    E.s (bug "builtins already initialized.");
  let h = builtinFunctions in
  (* See if we have builtin_va_list *)
  let hasbva = !M.theMachine.M.__builtin_va_list in
  let ulongLongType = TInt(IULongLong, []) in
  let floatType = TFloat(FFloat, []) in
  let longDoubleType = TFloat (FLongDouble, []) in
  let voidConstPtrType = TPtr(TVoid [Attr ("const", []); Attr ("pconst", [])], []) in
  let sizeType = !typeOfSizeOf in
  let v4sfType = TFloat (FFloat,[Attr("__vector_size__", [AInt 16])]) in

  H.add h "__builtin___fprintf_chk" (intType, [ voidPtrType; intType; charConstPtrType ], true) (* first argument is really FILE*, not void*, but we don't want to build in the definition for FILE *);
  H.add h "__builtin___memcpy_chk" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType; sizeType ], false);
  H.add h "__builtin___memmove_chk" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType; sizeType ], false);
  H.add h "__builtin___mempcpy_chk" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType; sizeType ], false);
  H.add h "__builtin___memset_chk" (voidPtrType, [ voidPtrType; intType; sizeType; sizeType ], false);
  H.add h "__builtin___printf_chk" (intType, [ intType; charConstPtrType ], true);
  H.add h "__builtin___snprintf_chk" (intType, [ charPtrType; sizeType; intType; sizeType; charConstPtrType ], true);
  H.add h "__builtin___sprintf_chk" (intType, [ charPtrType; intType; sizeType; charConstPtrType ], true);
  H.add h "__builtin___stpcpy_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  H.add h "__builtin___strcat_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  H.add h "__builtin___strcpy_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  H.add h "__builtin___strncat_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType; sizeType ], false);
  H.add h "__builtin___strncpy_chk" (charPtrType, [ charPtrType; charConstPtrType; sizeType; sizeType ], false);
  H.add h "__builtin___vfprintf_chk" (intType, [ voidPtrType; intType; charConstPtrType; TBuiltin_va_list [] ], false) (* first argument is really FILE*, not void*, but we don't want to build in the definition for FILE *);
  H.add h "__builtin___vprintf_chk" (intType, [ intType; charConstPtrType; TBuiltin_va_list [] ], false);
  H.add h "__builtin___vsnprintf_chk" (intType, [ charPtrType; sizeType; intType; sizeType; charConstPtrType; TBuiltin_va_list [] ], false);
  H.add h "__builtin___vsprintf_chk" (intType, [ charPtrType; intType; sizeType; charConstPtrType; TBuiltin_va_list [] ], false);

  H.add h "__builtin_acos" (doubleType, [ doubleType ], false);
  H.add h "__builtin_acosf" (floatType, [ floatType ], false);
  H.add h "__builtin_acosl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_alloca" (voidPtrType, [ sizeType ], false);

  H.add h "__builtin_asin" (doubleType, [ doubleType ], false);
  H.add h "__builtin_asinf" (floatType, [ floatType ], false);
  H.add h "__builtin_asinl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_atan" (doubleType, [ doubleType ], false);
  H.add h "__builtin_atanf" (floatType, [ floatType ], false);
  H.add h "__builtin_atanl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_atan2" (doubleType, [ doubleType; doubleType ], false);
  H.add h "__builtin_atan2f" (floatType, [ floatType; floatType ], false);
  H.add h "__builtin_atan2l" (longDoubleType, [ longDoubleType;
                                                longDoubleType ], false);

  let addSwap sizeInBits =
    try
      assert (sizeInBits mod 8 = 0);
      let sizeInBytes = sizeInBits / 8 in
      let sizedIntType = TInt (intKindForSize sizeInBytes false, []) in
      let name = Printf.sprintf "__builtin_bswap%d" sizeInBits in
      H.add h name (sizedIntType, [ sizedIntType ], false)
    with Not_found ->
      ()
  in
  addSwap 16;
  addSwap 32;
  addSwap 64;

  H.add h "__builtin_ceil" (doubleType, [ doubleType ], false);
  H.add h "__builtin_ceilf" (floatType, [ floatType ], false);
  H.add h "__builtin_ceill" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_cos" (doubleType, [ doubleType ], false);
  H.add h "__builtin_cosf" (floatType, [ floatType ], false);
  H.add h "__builtin_cosl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_cosh" (doubleType, [ doubleType ], false);
  H.add h "__builtin_coshf" (floatType, [ floatType ], false);
  H.add h "__builtin_coshl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_clz" (intType, [ uintType ], false);
  H.add h "__builtin_clzl" (intType, [ ulongType ], false);
  H.add h "__builtin_clzll" (intType, [ ulongLongType ], false);
  H.add h "__builtin_constant_p" (intType, [ intType ], false);
  H.add h "__builtin_ctz" (intType, [ uintType ], false);
  H.add h "__builtin_ctzl" (intType, [ ulongType ], false);
  H.add h "__builtin_ctzll" (intType, [ ulongLongType ], false);

  H.add h "__builtin_exp" (doubleType, [ doubleType ], false);
  H.add h "__builtin_expf" (floatType, [ floatType ], false);
  H.add h "__builtin_expl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_expect" (longType, [ longType; longType ], false);

  H.add h "__builtin_trap" (voidType, [], false);
  H.add h "__builtin_unreachable" (voidType, [], false);

  H.add h "__builtin_fabs" (doubleType, [ doubleType ], false);
  H.add h "__builtin_fabsf" (floatType, [ floatType ], false);
  H.add h "__builtin_fabsl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_ffs" (intType, [ uintType ], false);
  H.add h "__builtin_ffsl" (intType, [ ulongType ], false);
  H.add h "__builtin_ffsll" (intType, [ ulongLongType ], false);
  H.add h "__builtin_frame_address" (voidPtrType, [ uintType ], false);

  H.add h "__builtin_floor" (doubleType, [ doubleType ], false);
  H.add h "__builtin_floorf" (floatType, [ floatType ], false);
  H.add h "__builtin_floorl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_huge_val" (doubleType, [], false);
  H.add h "__builtin_huge_valf" (floatType, [], false);
  H.add h "__builtin_huge_vall" (longDoubleType, [], false);
  H.add h "__builtin_inf" (doubleType, [], false);
  H.add h "__builtin_inff" (floatType, [], false);
  H.add h "__builtin_infl" (longDoubleType, [], false);
  H.add h "__builtin_memcpy" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType ], false);
  H.add h "__builtin_memchr" (voidPtrType, [voidConstPtrType; intType; ulongType], false);
  H.add h "__builtin_mempcpy" (voidPtrType, [ voidPtrType; voidConstPtrType; sizeType ], false);
  H.add h "__builtin_memset" (voidPtrType,
                              [ voidPtrType; intType; intType ], false);
  H.add h "__builtin_bcopy" (voidType, [ voidConstPtrType; voidPtrType; sizeType ], false);
  H.add h "__builtin_bzero" (voidType,
                              [ voidPtrType; sizeType ], false);

  H.add h "__builtin_fmod" (doubleType, [ doubleType ], false);
  H.add h "__builtin_fmodf" (floatType, [ floatType ], false);
  H.add h "__builtin_fmodl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_frexp" (doubleType, [ doubleType; intPtrType ], false);
  H.add h "__builtin_frexpf" (floatType, [ floatType; intPtrType  ], false);
  H.add h "__builtin_frexpl" (longDoubleType, [ longDoubleType;
                                                intPtrType  ], false);

  H.add h "__builtin_ldexp" (doubleType, [ doubleType; intType ], false);
  H.add h "__builtin_ldexpf" (floatType, [ floatType; intType  ], false);
  H.add h "__builtin_ldexpl" (longDoubleType, [ longDoubleType;
                                                intType  ], false);

  H.add h "__builtin_log" (doubleType, [ doubleType ], false);
  H.add h "__builtin_logf" (floatType, [ floatType ], false);
  H.add h "__builtin_logl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_log10" (doubleType, [ doubleType ], false);
  H.add h "__builtin_log10f" (floatType, [ floatType ], false);
  H.add h "__builtin_log10l" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_modff" (floatType, [ floatType;
                                          TPtr(floatType,[]) ], false);
  H.add h "__builtin_modfl" (longDoubleType, [ longDoubleType;
                                               TPtr(longDoubleType, []) ],
                             false);

  H.add h "__builtin_nan" (doubleType, [ charConstPtrType ], false);
  H.add h "__builtin_nanf" (floatType, [ charConstPtrType ], false);
  H.add h "__builtin_nanl" (longDoubleType, [ charConstPtrType ], false);
  H.add h "__builtin_nans" (doubleType, [ charConstPtrType ], false);
  H.add h "__builtin_nansf" (floatType, [ charConstPtrType ], false);
  H.add h "__builtin_nansl" (longDoubleType, [ charConstPtrType ], false);
  H.add h "__builtin_next_arg" ((if hasbva then TBuiltin_va_list [] else voidPtrType), [], false) (* When we parse builtin_next_arg we drop the argument *);
  H.add h "__builtin_object_size" (sizeType, [ voidPtrType; intType ], false);

  H.add h "__builtin_parity" (intType, [ uintType ], false);
  H.add h "__builtin_parityl" (intType, [ ulongType ], false);
  H.add h "__builtin_parityll" (intType, [ ulongLongType ], false);

  H.add h "__builtin_popcount" (intType, [ uintType ], false);
  H.add h "__builtin_popcountl" (intType, [ ulongType ], false);
  H.add h "__builtin_popcountll" (intType, [ ulongLongType ], false);

  H.add h "__builtin_powi" (doubleType, [ doubleType; intType ], false);
  H.add h "__builtin_powif" (floatType, [ floatType; intType ], false);
  H.add h "__builtin_powil" (longDoubleType, [ longDoubleType; intType ], false);
  H.add h "__builtin_prefetch" (voidType, [ voidConstPtrType ], true);
  H.add h "__builtin_return" (voidType, [ voidConstPtrType ], false);
  H.add h "__builtin_return_address" (voidPtrType, [ uintType ], false);
  H.add h "__builtin_extract_return_addr" (voidPtrType, [ voidPtrType ], false);
  H.add h "__builtin_frob_return_address" (voidPtrType, [ voidPtrType ], false);

  H.add h "__builtin_sin" (doubleType, [ doubleType ], false);
  H.add h "__builtin_sinf" (floatType, [ floatType ], false);
  H.add h "__builtin_sinl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_sinh" (doubleType, [ doubleType ], false);
  H.add h "__builtin_sinhf" (floatType, [ floatType ], false);
  H.add h "__builtin_sinhl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_sqrt" (doubleType, [ doubleType ], false);
  H.add h "__builtin_sqrtf" (floatType, [ floatType ], false);
  H.add h "__builtin_sqrtl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_stpcpy" (charPtrType, [ charPtrType; charConstPtrType ], false);
  H.add h "__builtin_strcat" (charPtrType, [charPtrType; charConstPtrType], false);
  H.add h "__builtin_strchr" (charPtrType, [ charPtrType; intType ], false);
  H.add h "__builtin_strcmp" (intType, [ charConstPtrType; charConstPtrType ], false);
  H.add h "__builtin_strcpy" (charPtrType, [ charPtrType; charConstPtrType ], false);
  H.add h "__builtin_strlen" (sizeType, [ charConstPtrType ], false);
  H.add h "__builtin_strcspn" (sizeType, [ charConstPtrType; charConstPtrType ], false);
  H.add h "__builtin_strncat" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  H.add h "__builtin_strncmp" (intType, [ charConstPtrType; charConstPtrType; sizeType ], false);
  H.add h "__builtin_strncpy" (charPtrType, [ charPtrType; charConstPtrType; sizeType ], false);
  H.add h "__builtin_strspn" (sizeType, [ charConstPtrType; charConstPtrType ], false);
  H.add h "__builtin_strpbrk" (charPtrType, [ charConstPtrType; charConstPtrType ], false);
  (* When we parse builtin_types_compatible_p, we change its interface *)
  H.add h "__builtin_types_compatible_p"
                            (intType, [ !typeOfSizeOf;(* Sizeof the type *)
                                        !typeOfSizeOf (* Sizeof the type *) ],
                               false);
  H.add h "__builtin_tan" (doubleType, [ doubleType ], false);
  H.add h "__builtin_tanf" (floatType, [ floatType ], false);
  H.add h "__builtin_tanl" (longDoubleType, [ longDoubleType ], false);

  H.add h "__builtin_tanh" (doubleType, [ doubleType ], false);
  H.add h "__builtin_tanhf" (floatType, [ floatType ], false);
  H.add h "__builtin_tanhl" (longDoubleType, [ longDoubleType ], false);

  (* MMX Builtins *)
  H.add h "__builtin_ia32_addps" (v4sfType, [v4sfType; v4sfType], false);
  H.add h "__builtin_ia32_subps" (v4sfType, [v4sfType; v4sfType], false);
  H.add h "__builtin_ia32_mulps" (v4sfType, [v4sfType; v4sfType], false);
  H.add h "__builtin_ia32_unpckhps" (v4sfType, [v4sfType; v4sfType], false);
  H.add h "__builtin_ia32_unpcklps" (v4sfType, [v4sfType; v4sfType], false);
  H.add h "__builtin_ia32_maxps" (v4sfType, [v4sfType; v4sfType], false);

  (* tgmath in newer versions of GCC *)
  H.add h "__builtin_tgmath" (TVoid[Attr("overloaded",[])], [ ], true);

  (* Atomic Builtins
     These builtins have an overloaded return type, hence the "magic" void type
     with __overloaded__ attribute, used to infer return type from parameters in
     cabs2cil.ml.
     For the same reason, we do not specify the type of the parameters. *)
  H.add h "__sync_fetch_and_add" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_fetch_and_sub" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_fetch_and_or" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_fetch_and_and" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_fetch_and_xor" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_fetch_and_nand" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_add_and_fetch" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_sub_and_fetch" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_or_and_fetch" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_and_and_fetch" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_xor_and_fetch" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_nand_and_fetch" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_bool_compare_and_swap" (TInt (IBool, []), [ ], true);
  H.add h "__sync_val_compare_and_swap" (TVoid[Attr("overloaded",[])], [ ],
    true);
  H.add h "__sync_synchronize" (voidType, [ ], true);
  H.add h "__sync_lock_test_and_set" (TVoid[Attr("overloaded",[])], [ ], true);
  H.add h "__sync_lock_release" (voidType, [ ], true);

  (* __atomic builtins for various bit widths

     Most __atomic functions are offered for various bit widths, using
     a different suffix for each concrete bit width:  "_1", "_2", and
     so on up to "_16".  Each of these functions also exists in a form
     with no bit width specified, and occasionally with a bit width
     suffix of "_n".

     Note that these __atomic functions are not really va_arg, but we
     set the va_arg flag nonetheless because it prevents CIL from
     trying to check the type of parameters against the prototype.
   *)

  let addAtomicForWidths baseName ?n ~none ~num () =
    (* ?n gives the return type to be used with the "_n" suffix, if any *)
    (* ~none gives the return type to be used with no suffix *)
    (* ~num gives the return type to be used with the "_1" through "_16" suffixes *)
    let addWithSuffix suffix returnType =
      let identifier = "__atomic_" ^ baseName ^ suffix in
      H.add h identifier (returnType, [], true)
    in
    List.iter begin
	fun bitWidth ->
	let suffix = "_" ^ (string_of_int bitWidth) in
	addWithSuffix suffix num
      end [1; 2; 4; 8; 16];
    addWithSuffix "" none;
    match n with
    | None -> ()
    | Some typ -> addWithSuffix "_n" typ
  in

  let anyType = TVoid [Attr("overloaded", [])] in

  (* binary operations combined with a fetch of either the old or new value *)
  List.iter begin
      fun operation ->
      addAtomicForWidths ("fetch_" ^ operation) ~none:anyType ~num:anyType ();
      addAtomicForWidths (operation ^ "_fetch") ~none:anyType ~num:anyType ()
    end ["add"; "and"; "nand"; "or"; "sub"; "xor"];

  (* other atomic operations provided at various bit widths *)
  addAtomicForWidths "compare_exchange" ~none:boolType ~n:boolType ~num:boolType ();
  addAtomicForWidths "exchange" ~none:voidType ~n:anyType ~num:anyType ();
  addAtomicForWidths "load" ~none:voidType ~n:anyType ~num:anyType ();
  addAtomicForWidths "store" ~none:voidType ~n:voidType ~num:voidType ();

  (* Some atomic builtins actually have a decent, C-compatible type *)
  H.add h "__atomic_test_and_set" (boolType, [voidPtrType; intType], false);
  H.add h "__atomic_clear" (voidType, [boolPtrType; intType], false);
  H.add h "__atomic_thread_fence" (voidType, [intType], false);
  H.add h "__atomic_signal_fence" (voidType, [intType], false);
  H.add h "__atomic_always_lock_free" (boolType, [sizeType; voidPtrType], false);
  H.add h "__atomic_is_lock_free" (boolType, [sizeType; voidPtrType], false);
  H.add h "__atomic_feraiseexcept" (voidType, [intType], false);

  if hasbva then begin
    H.add h "__builtin_va_end" (voidType, [ TBuiltin_va_list [] ], false);
    H.add h "__builtin_varargs_start"
      (voidType, [ TBuiltin_va_list [] ], false);
    (* When we parse builtin_{va,stdarg}_start, we drop the second argument *)
    H.add h "__builtin_va_start" (voidType, [ TBuiltin_va_list [] ], false);
    H.add h "__builtin_stdarg_start" (voidType, [ TBuiltin_va_list []; ],
                                      false);
    (* When we parse builtin_va_arg we change its interface *)
    H.add h "__builtin_va_arg" (voidType, [ TBuiltin_va_list [];
                                            !typeOfSizeOf;(* Sizeof the type *)
                                            voidPtrType; (* Ptr to res *) ],
                               false);
    H.add h "__builtin_va_copy" (voidType, [ TBuiltin_va_list [];
					     TBuiltin_va_list [] ],
                                false);
  end;

  H.add h "__builtin_apply_args" (voidPtrType, [ ], false);
  let fnPtr = TPtr(TFun (voidType, None, false, []), []) in
  H.add h "__builtin_apply" (voidPtrType, [fnPtr; voidPtrType; sizeType], false);
  H.add h "__builtin_va_arg_pack" (intType, [ ], false);
  H.add h "__builtin_va_arg_pack_len" (intType, [ ], false);
  ()


(** This is used as the location of the prototypes of builtin functions. *)
let builtinLoc: location = { line = 1;
                             file = "<compiler builtins>";
                             byte = 0;
                             column = 0;
                             endLine = -1;
                             endByte = -1;
                             endColumn = -1;
                             synthetic = true;}



let pTypeSig : (typ -> typsig) ref =
  ref (fun _ -> E.s (E.bug "pTypeSig not initialized"))


(** A printer interface for CIL trees. Create instantiations of
   this type by specializing the class {!defaultCilPrinter}. *)
class type cilPrinter = object

  method setCurrentFormals : varinfo list -> unit

  method setPrintInstrTerminator : string -> unit
  method getPrintInstrTerminator : unit -> string

  method pVDecl: unit -> varinfo -> doc
    (** Invoked for each variable declaration. Note that variable
       declarations are all the [GVar], [GVarDecl], [GFun], all the [varinfo]
       in formals of function types, and the formals and locals for function
       definitions. *)

  method pVar: varinfo -> doc
    (** Invoked on each variable use. *)

  method pLval: unit -> lval -> doc
    (** Invoked on each lvalue occurrence *)

  method pOffset: doc -> offset -> doc
    (** Invoked on each offset occurrence. The second argument is the base. *)

  method pInstr: unit -> instr -> doc
    (** Invoked on each instruction occurrence. *)

  method pStmt: unit -> stmt -> doc
    (** Control-flow statement. This is used by
       {!printGlobal} and by {!dumpGlobal}. *)

  method dStmt: out_channel -> int -> stmt -> unit
    (** Dump a control-flow statement to a file with a given indentation. This is used by
       {!dumpGlobal}. *)

  method dBlock: out_channel -> int -> block -> unit
    (** Dump a control-flow block to a file with a given indentation. This is
       used by {!dumpGlobal}. *)

  method pBlock: unit -> block -> Pretty.doc
    (** Print a block. *)

  method pGlobal: unit -> global -> doc
    (** Global (vars, types, etc.). This can be slow and is used only by
       {!printGlobal} but by {!dumpGlobal} for everything else except
       [GVar] and [GFun]. *)

  method dGlobal: out_channel -> global -> unit
    (** Dump a global to a file. This is used by {!dumpGlobal}. *)

  method pFieldDecl: unit -> fieldinfo -> doc
    (** A field declaration *)

  method pType: doc option -> unit -> typ -> doc
  (* Use of some type in some declaration. The first argument is used to print
     the declared element, or is None if we are just printing a type with no
     name being declared. Note that for structure/union and enumeration types
     the definition of the composite type is not visited. Use [vglob] to
     visit it.  *)

  method pAttr: attribute -> doc * bool
    (** Attribute. Also return an indication whether this attribute must be
        printed inside the __attribute__ list or not. *)

  method pAttrParam: unit -> attrparam -> doc
    (** Attribute parameter *)

  method pAttrs: unit -> attributes -> doc
    (** Attribute lists *)

  method pLabel: unit -> label -> doc
    (** Label *)

  method pLineDirective: ?forcefile:bool -> location -> Pretty.doc
    (** Print a line-number. This is assumed to come always on an empty line.
       If the forcefile argument is present and is true then the file name
       will be printed always. Otherwise the file name is printed only if it
       is different from the last time time this function is called. The last
       file name is stored in a private field inside the cilPrinter object. *)

  method pStmtKind : stmt -> unit -> stmtkind -> Pretty.doc
    (** Print a statement kind. The code to be printed is given in the
       {!stmtkind} argument.  The initial {!stmt} argument
       records the statement which follows the one being printed;
       {!defaultCilPrinterClass} uses this information to prettify
       statement printing in certain special cases. *)

  method pExp: unit -> exp -> doc
    (** Print expressions *)

  method pInit: unit -> init -> doc
    (** Print initializers. This can be slow and is used by
       {!printGlobal} but not by {!dumpGlobal}. *)

  method dInit: out_channel -> int -> init -> unit
    (** Dump a global to a file with a given indentation. This is used by
       {!dumpGlobal}. *)
end


class defaultCilPrinterClass : cilPrinter = object (self)
  val mutable currentFormals : varinfo list = []
  method private getLastNamedArgument (s:string) : exp =
    match List.rev currentFormals with
      f :: _ -> Lval (var f)
    | [] ->
        E.s (bug "Cannot find the last named argument when printing call to %s\n" s)

  method private setCurrentFormals (fms : varinfo list) =
    currentFormals <- fms

  (*** VARIABLES ***)
  (* variable use *)
  method pVar (v:varinfo) = text v.vname

  (* variable declaration *)
  method pVDecl () (v:varinfo) =
    (* First the storage modifiers *)
    text (if v.vinline then "__inline " else "")
      ++ d_storage () v.vstorage
      ++ (self#pType (Some (text v.vname)) () v.vtype)
      ++ text " "
      ++ self#pAttrs () v.vattr

  (*** L-VALUES ***)
  method pLval () (lv:lval) =  (* lval (base is 1st field)  *)
    match lv with
      Var vi, o -> self#pOffset (self#pVar vi) o
    | Mem e, Field(fi, o) ->
        self#pOffset
          ((self#pExpPrec arrowLevel () e) ++ text ("->" ^ fi.fname)) o
    | Mem e, NoOffset ->
        text "*" ++ self#pExpPrec derefStarLevel () e
    | Mem e, o ->
        self#pOffset
          (text "(*" ++ self#pExpPrec derefStarLevel () e ++ text ")") o

  (** Offsets **)
  method pOffset (base: doc) = function
    | NoOffset -> base
    | Field (fi, o) ->
        self#pOffset (base ++ text "." ++ text fi.fname) o
    | Index (e, o) ->
        self#pOffset (base ++ text "[" ++ self#pExp () e ++ text "]") o

  method private pLvalPrec (contextprec: int) () lv =
    if getParenthLevel (Lval(lv)) >= contextprec then
      text "(" ++ self#pLval () lv ++ text ")"
    else
      self#pLval () lv

  (*** EXPRESSIONS ***)
  method pExp () (e: exp) : doc =
    let level = getParenthLevel e in
    match e with
      Const(c) -> d_const () c
    | Lval(l) -> self#pLval () l
    | UnOp(u,e1,_) ->
        (d_unop () u) ++ chr ' ' ++ (self#pExpPrec level () e1)

    | BinOp(b,e1,e2,_) ->
        align
          ++ (self#pExpPrec level () e1)
          ++ chr ' '
          ++ (d_binop () b)
          ++ chr ' '
          ++ (self#pExpPrec level () e2)
          ++ unalign

    | Question(e1,e2,e3,_) ->
        (self#pExpPrec level () e1)
          ++ text " ? "
          ++ (self#pExpPrec level () e2)
          ++ text " : "
          ++ (self#pExpPrec level () e3)

    | CastE(t,e) ->
        text "("
          ++ self#pType None () t
          ++ text ")"
          ++ self#pExpPrec level () e

    | SizeOf (t) ->
        text "sizeof(" ++ self#pType None () t ++ chr ')'
    | SizeOfE (Lval (Var fv, NoOffset)) when fv.vname = "__builtin_va_arg_pack" && (not !printCilAsIs) ->
        text "__builtin_va_arg_pack()"
    | SizeOfE (e) ->
        text "sizeof(" ++ self#pExp () e ++ chr ')'
    | Imag e ->
        text "__imag__(" ++ self#pExp () e ++ chr ')'
    | Real e ->
        text "__real__(" ++ self#pExp () e ++ chr ')'
    | SizeOfStr s ->
        text "sizeof(" ++ d_const () (CStr (s, No_encoding)) ++ chr ')'

    | AlignOf (t) ->
        text "__alignof__(" ++ self#pType None () t ++ chr ')'
    | AlignOfE (e) ->
        text "__alignof__(" ++ self#pExp () e ++ chr ')'
    | AddrOf(lv) ->
        text "& " ++ (self#pLvalPrec addrOfLevel () lv)
    | AddrOfLabel(sref) -> begin
        (* Grab one of the labels *)
        let rec pickLabel = function
            [] -> None
          | Label (l, _, _) :: _ -> Some l
          | _ :: rest -> pickLabel rest
        in
        match pickLabel !sref.labels with
          Some lbl -> text ("&& " ^ lbl)
        | None ->
            ignore (error "Cannot find label for target of address of label");
            text "&& __invalid_label"
    end

    | StartOf(lv) -> self#pLval () lv

  (* Print an expression, given the precedence of the context in which it
     appears. *)
  method private pExpPrec (contextprec: int) () (e: exp) =
    let thisLevel = getParenthLevel e in
    let needParens =
      if thisLevel >= contextprec then
	true
      else if contextprec == bitwiseLevel then
        (* quiet down some GCC warnings *)
	thisLevel == additiveLevel || thisLevel == comparativeLevel
      else
	false
    in
    if needParens then
      chr '(' ++ self#pExp () e ++ chr ')'
    else
      self#pExp () e

  method pInit () = function
      SingleInit e -> self#pExp () e
    | CompoundInit (t, initl) ->
      (* We do not print the type of the Compound *)
(*
      let dinit e = d_init () e in
      dprintf "{@[%a@]}"
        (docList ~sep:(chr ',' ++ break) dinit) initl
*)
        let printDesignator =
          (* Print only for union when we do not initialize the first field *)
          match unrollType t, initl with
            TComp(ci, _), [(Field(f, NoOffset), _)] ->
              if not (ci.cstruct) && ci.cfields != [] &&
                (List.hd ci.cfields) != f then
                true
              else
                false
          | _ -> false
        in
        let d_oneInit = function
            Field(f, NoOffset), i ->
              (if printDesignator then
                text ("." ^ f.fname ^ " = ")
              else nil) ++ self#pInit () i
          | Index(e, NoOffset), i ->
              (if printDesignator then
                text "[" ++ self#pExp () e ++ text "] = " else nil) ++
                self#pInit () i
          | _ -> E.s (unimp "Trying to print malformed initializer")
        in
        chr '{' ++ (align
                      ++ ((docList ~sep:(chr ',' ++ break) d_oneInit) () initl)
                      ++ unalign)
          ++ chr '}'
(*
    | ArrayInit (_, _, il) ->
        chr '{' ++ (align
                      ++ ((docList (chr ',' ++ break) (self#pInit ())) () il)
                      ++ unalign)
          ++ chr '}'
*)
  (* dump initializers to a file. *)
  method dInit (out: out_channel) (ind: int) (i: init) =
    (* Dump an array *)
    let dumpArray (bt: typ) (il: 'a list) (getelem: 'a -> init) =
      let onALine = (* How many elements on a line *)
        match unrollType bt with TComp _ | TArray _ -> 1 | _ -> 4
      in
      let rec outputElements (isfirst: bool) (room_on_line: int) = function
          [] -> output_string out "}"
        | (i: 'a) :: rest ->
            if not isfirst then output_string out ", ";
            let new_room_on_line =
              if room_on_line == 0 then begin
                output_string out "\n"; output_string out (String.make ind ' ');
                onALine - 1
              end else
                room_on_line - 1
            in
            self#dInit out (ind + 2) (getelem i);
            outputElements false new_room_on_line rest
      in
      output_string out "{ ";
      outputElements true onALine il
    in
    match i with
      SingleInit e ->
        fprint out ~width:!lineLength (indent ind (self#pExp () e))
    | CompoundInit (t, initl) -> begin
        match unrollType t with
          TArray(bt, _, _) ->
            dumpArray bt initl (fun (_, i) -> i)
        | _ ->
            (* Now a structure or a union *)
            fprint out ~width:!lineLength (indent ind (self#pInit () i))
    end
(*
    | ArrayInit (bt, len, initl) -> begin
        (* If the base type does not contain structs then use the pInit
        match unrollType bt with
          TComp _ | TArray _ ->
            dumpArray bt initl (fun x -> x)
        | _ -> *)
            fprint out !lineLength (indent ind (self#pInit () i))
    end
*)

  (** What terminator to print after an instruction. sometimes we want to
     print sequences of instructions separated by comma *)
  val mutable printInstrTerminator = ";"

  method private setPrintInstrTerminator (term : string) =
    printInstrTerminator <- term

  method private getPrintInstrTerminator () = printInstrTerminator

  (*** INSTRUCTIONS ****)
  method pInstr () (i:instr) =       (* imperative instruction *)
    match i with
    | Set(lv,e,l,el) -> begin
        (* Be nice to some special cases *)
        match e with
          BinOp((PlusA|PlusPI|IndexPI),Lval(lv'),Const(CInt(one,_,_)),_)
            when Util.equals lv lv' && compare_cilint one one_cilint = 0 && not !printCilAsIs ->
              self#pLineDirective l
                ++ self#pLvalPrec indexLevel () lv
                ++ text (" ++" ^ printInstrTerminator)

        | BinOp((MinusA|MinusPI),Lval(lv'),
                Const(CInt(one,_,_)), _)
            when Util.equals lv lv' && compare_cilint one one_cilint = 0 && not !printCilAsIs ->
                  self#pLineDirective l
                    ++ self#pLvalPrec indexLevel () lv
                    ++ text (" --" ^ printInstrTerminator)

        | BinOp((PlusA|PlusPI|IndexPI),Lval(lv'),Const(CInt(mone,_,_)),_)
            when Util.equals lv lv' && compare_cilint mone mone_cilint = 0
                && not !printCilAsIs ->
              self#pLineDirective l
                ++ self#pLvalPrec indexLevel () lv
                ++ text (" --" ^ printInstrTerminator)

        | BinOp((PlusA|PlusPI|IndexPI|MinusA|MinusPP|MinusPI|BAnd|BOr|BXor|
          Mult|Div|Mod|Shiftlt|Shiftrt) as bop,
                Lval(lv'),e,_) when Util.equals lv lv'
                && not !printCilAsIs ->
                  self#pLineDirective l
                    ++ self#pLval () lv
                    ++ text " " ++ d_binop () bop
                    ++ text "= "
                    ++ self#pExp () e
                    ++ text printInstrTerminator

        | _ ->
            self#pLineDirective l
              ++ self#pLval () lv
              ++ text " = "
              ++ self#pExp () e
              ++ text printInstrTerminator

    end
    | VarDecl(v, l) ->
        self#pLineDirective l
        ++ self#pVDecl () v
        ++ (match v.vinit.init with
            | None -> text ";"
            | Some i -> text " = " ++
                self#pInit () i ++ text ";")
      (* In cabs2cil we have turned the call to builtin_va_arg into a
         three-argument call: the last argument is the address of the
         destination *)
    | Call(None, Lval(Var vi, NoOffset), [dest; SizeOf t; adest], l, el)
        when vi.vname = "__builtin_va_arg" && not !printCilAsIs ->
          let destlv = match stripCasts adest with
            AddrOf destlv -> destlv
              (* If this fails, it's likely that an extension interfered
                 with the AddrOf *)
          | _ -> E.s (E.bug
                        "%a: Encountered unexpected call to %s with dest %a\n"
                        d_loc l vi.vname self#pExp adest)
          in
          self#pLineDirective l
	    ++ self#pLval () destlv ++ text " = "

            (* Now the function name *)
            ++ text "__builtin_va_arg"
            ++ text "(" ++ (align
                              (* Now the arguments *)
                              ++ self#pExp () dest
                              ++ chr ',' ++ break
                              ++ self#pType None () t
                              ++ unalign)
            ++ text (")" ^ printInstrTerminator)

      (* In cabs2cil we have dropped the last argument in the call to
         __builtin_va_start and __builtin_stdarg_start. *)
    | Call(None, Lval(Var vi, NoOffset), [marker], l, el)
        when ((vi.vname = "__builtin_stdarg_start" ||
               vi.vname = "__builtin_va_start") && not !printCilAsIs) ->
        if currentFormals <> [] then begin
          let last = self#getLastNamedArgument vi.vname in
          self#pInstr () (Call(None,Lval(Var vi,NoOffset),[marker; last],l,el))
        end
        else begin
          (* We can't print this call because someone called pInstr outside
             of a pFunDecl, so we don't know what the formals of the current
             function are.  Just put in a placeholder for now; this isn't
             valid C. *)
          self#pLineDirective l
          ++ dprintf
            "%s(%a, /* last named argument of the function calling %s */)"
            vi.vname self#pExp marker vi.vname
          ++ text printInstrTerminator
        end
      (* In cabs2cil we have dropped the last argument in the call to
         __builtin_next_arg. *)
    | Call(res, Lval(Var vi, NoOffset), [ ], l, el)
        when vi.vname = "__builtin_next_arg" && not !printCilAsIs -> begin
          let last = self#getLastNamedArgument vi.vname in
          self#pInstr () (Call(res,Lval(Var vi,NoOffset),[last],l, el))
        end

      (* In cparser we have turned the call to
         __builtin_types_compatible_p(t1, t2) into
         __builtin_types_compatible_p(sizeof t1, sizeof t2), so that we can
         represent the types as expressions.
         Remove the sizeofs when printing. *)
    | Call(dest, Lval(Var vi, NoOffset), [SizeOf t1; SizeOf t2], l, el)
        when vi.vname = "__builtin_types_compatible_p" && not !printCilAsIs ->
        self#pLineDirective l
          (* Print the destination *)
        ++ (match dest with
              None -> nil
            | Some lv -> self#pLval () lv ++ text " = ")
          (* Now the call itself *)
        ++ dprintf "%s(%a, %a)" vi.vname
             (self#pType None) t1  (self#pType None) t2
        ++ text printInstrTerminator
    | Call(_, Lval(Var vi, NoOffset), _, l, el)
        when vi.vname = "__builtin_types_compatible_p" && not !printCilAsIs ->
        E.s (bug "__builtin_types_compatible_p: cabs2cil should have added sizeof to the arguments.")

    | Call(dest,e,args,l,el) ->
        let rec patchTypeNotVLA t =
          match t with
          | TPtr(t, args) -> TPtr(patchTypeNotVLA t, args)
          | TArray(t, None, args) -> TArray(patchTypeNotVLA t, None, args)
          | TArray(t, Some exp, args) when isConstant exp -> TArray(patchTypeNotVLA t, Some exp, args)
          | TArray(t, Some exp, args) -> TArray(patchTypeNotVLA t, None, args)
          | _ -> t
        in
        let patchArgNotUseVLACast exp =
          match exp with
          | CastE(t, e) -> CastE(patchTypeNotVLA t, e)
          | e -> e
        in
        self#pLineDirective l
          ++ (match dest with
            None -> nil
          | Some lv ->
              self#pLval () lv ++ text " = " ++
                (* Maybe we need to print a cast *)
                (let destt = typeOfLval lv in
                match unrollType (typeOf e) with
                  TFun (rt, _, _, _)
                      when not (Util.equals (!pTypeSig rt)
                                            (!pTypeSig destt)) ->
                    text "(" ++ self#pType None () destt ++ text ")"
                | _ -> nil))
          (* Now the function name *)
          ++ (let ed = self#pExp () e in
              match e with
                Lval(Var _, _) -> ed
              | _ -> text "(" ++ ed ++ text ")")
          ++ text "(" ++
          (align
             (* Now the arguments *)
             ++ (docList ~sep:(chr ',' ++ break)
                   (fun x -> self#pExp () (patchArgNotUseVLACast x)) () args) (* here we would need to remove casts to array types that are not ok *)
             ++ unalign)
        ++ text (")" ^ printInstrTerminator)

    | Asm(attrs, tmpls, outs, ins, clobs, gotos, l) ->
        self#pLineDirective l
          ++ text ("__asm__ ")
          ++ self#pAttrs () attrs
          ++ text " ("
          ++ (align
                ++ (docList ~sep:line
                      (fun x -> text ("\"" ^ escape_string x ^ "\""))
                      () tmpls)
                ++
                (if outs = [] && ins = [] && clobs = [] then
                  chr ':'
              else
                (text ": "
                    ++ (docList ~sep:(chr ',' ++ break)
                          (fun (idopt, c, lv) ->
                          text(match idopt with
                                None -> ""
                              | Some id -> "[" ^ id ^ "] "
                          ) ++
                            text ("\"" ^ escape_string c ^ "\" (")
                              ++ self#pLval () lv
                              ++ text ")") () outs)))
              ++
                (if ins = [] && clobs = [] then
                  nil
                else
                  (text ": "
                      ++ (docList ~sep:(chr ',' ++ break)
                            (fun (idopt, c, e) ->
                              text(match idopt with
                                    None -> ""
                                  | Some id -> "[" ^ id ^ "] "
                              ) ++
                              text ("\"" ^ escape_string c ^ "\" (")
                                ++ self#pExp () e
                                ++ text ")") () ins)))
                ++
                (if clobs = [] then nil
                else
                  (text ": "
                      ++ (docList ~sep:(chr ',' ++ break)
                            (fun c -> text ("\"" ^ escape_string c ^ "\""))
                            ()
                            clobs)))
                ++
                (if gotos = [] then nil
                else (
                  text ": " ++ (docList ~sep:(chr ',' ++ break) (fun c -> text (escape_string c)) () gotos)
                ))
                ++ unalign)
          ++ text (")" ^ printInstrTerminator)


  (**** STATEMENTS ****)
  method pStmt () (s:stmt) =        (* control-flow statement *)
    self#pStmtNext invalidStmt () s

  method dStmt (out: out_channel) (ind: int) (s:stmt) : unit =
    fprint out ~width:!lineLength (indent ind (self#pStmt () s))

  method dBlock (out: out_channel) (ind: int) (b:block) : unit =
    fprint out ~width:!lineLength (indent ind (align ++ self#pBlock () b))

  method private pStmtNext (next: stmt) () (s: stmt) =
    (* print the labels *)
    let labels = ((docList ~sep:line (fun l -> self#pLabel () l)) () s.labels) in
    if s.skind = Instr [] && s.labels <> [] then
      (* If the labels are non-empty and the statement is empty, print a semicolon  *)
      labels ++ text ";"
    else
      let pre =
        if s.labels <> [] then
          (match s.skind with
          | Instr (VarDecl(_)::_)-> text ";" (* first instruction is VarDecl, insert semicolon *)
          | _ -> nil)
          ++ line
        else
          nil (* no labels, no new line needed *)
      in
      labels ++ pre ++ self#pStmtKind next () s.skind

  method private pLabel () = function
      Label (s, _, true) -> text (s ^ ": ")
    | Label (s, _, false) -> text (s ^ ": /* CIL Label */ ")
    | Case (e, _, _) -> text "case " ++ self#pExp () e ++ text ": "
    | CaseRange (e1, e2, _, _) -> text "case " ++ self#pExp () e1 ++ text " ... "
        ++ self#pExp () e2 ++ text ": "
    | Default _ -> text "default: "

  (* The pBlock will put the unalign itself *)
  method pBlock () (blk: block) =
    let rec dofirst () = function
        [] -> nil
      | [x] -> self#pStmtNext invalidStmt () x
      | x :: rest -> dorest nil x rest
    and dorest acc prev = function
        [] -> acc ++ (self#pStmtNext invalidStmt () prev)
      | x :: rest ->
          dorest (acc ++ (self#pStmtNext x () prev) ++ line)
            x rest
    in
    (* Let the host of the block decide on the alignment. The d_block will
       pop the alignment as well  *)
    text "{"
      ++
      (if blk.battrs <> [] then
        self#pAttrsGen true blk.battrs
      else nil)
      ++ line
      ++ (dofirst () blk.bstmts)
      ++ unalign ++ line ++ text "}"


  (* Store here the name of the last file printed in a line number. This is
     private to the object *)
  val mutable lastFileName = ""
  val mutable lastLineNumber = -1

  (* Make sure that you only call self#pLineDirective on an empty line *)
  method pLineDirective ?(forcefile=false) l =
    currentLoc := l;
    match !lineDirectiveStyle with
    | None -> nil
    | Some _ when l.line <= 0 -> nil

      (* Do not print lineComment if the same line as above *)
    | Some LineCommentSparse when l.line = lastLineNumber -> nil

    | Some style  ->
	let directive =
	  match style with
	  | LineComment | LineCommentSparse -> text "//#line "
	  | LinePreprocessorOutput -> chr '#'
	  | LinePreprocessorInput -> text "#line"
	in
        lastLineNumber <- l.line;
	let filename =
          if forcefile || l.file <> lastFileName then
	    begin
	      lastFileName <- l.file;
	      text " \"" ++ text l.file ++ text "\""
            end
	  else
	    nil
	in
	leftflush ++ directive ++ chr ' ' ++ num l.line ++ filename ++ line

  method private pIfConditionThen loc condition thenBlock =
      self#pLineDirective loc
      ++ text "if"
      ++ (align
          ++ text " ("
          ++ self#pExp () condition
          ++ text ") "
          ++ self#pBlock () thenBlock)

  method private pStmtKind (next: stmt) () = function
      Return(None, l) ->
        self#pLineDirective l
          ++ text "return;"

    | Return(Some e, l) ->
        self#pLineDirective l
          ++ text "return ("
          ++ self#pExp () e
          ++ text ");"

    | Goto (sref, l) -> begin
        (* Grab one of the labels *)
        let rec pickLabel = function
            [] -> None
          | Label (l, _, _) :: _ -> Some l
          | _ :: rest -> pickLabel rest
        in
        match pickLabel !sref.labels with
          Some lbl -> self#pLineDirective l ++ text ("goto " ^ lbl ^ ";")
        | None ->
            ignore (error "Cannot find label for target of goto");
            text "goto __invalid_label;"
    end

    | ComputedGoto(e, l) ->
        self#pLineDirective l
          ++ text "goto *("
          ++ self#pExp () e
          ++ text ");"

    | Break l ->
        self#pLineDirective l
          ++ text "break;"

    | Continue l ->
        self#pLineDirective l
          ++ text "continue;"

    | Instr il ->
        align
          ++ (docList ~sep:line (fun i -> self#pInstr () i) () il)
          ++ unalign

    | If(be,t,{bstmts=[];battrs=[]},l,el) when not !printCilAsIs ->
        self#pIfConditionThen l be t

    | If(be,t,{bstmts=[{skind=Goto(gref,_);labels=[]; _}];
                battrs=[]},l,el)
     when !gref == next && not !printCilAsIs ->
        self#pIfConditionThen l be t

    | If(be,{bstmts=[];battrs=[]},e,l,el) when not !printCilAsIs ->
          self#pIfConditionThen l (UnOp(LNot,be,intType)) e

    | If(be,{bstmts=[{skind=Goto(gref,_);labels=[]; _}];
           battrs=[]},e,l,el)
      when !gref == next && not !printCilAsIs ->
        self#pIfConditionThen l (UnOp(LNot,be,intType)) e

    | If(be,t,e,l,el) ->
        self#pIfConditionThen l be t
          ++ (match e with
                { bstmts=[{skind=If _; _} as elsif]; battrs=[] } ->
                    text " else"
                    ++ line (* Don't indent else-ifs *)
                    ++ self#pStmtNext next () elsif
              | _ ->
                    text " "   (* sm: indent next code 2 spaces (was 4) *)
                    ++ align
                    ++ text "else "
                    ++ self#pBlock () e)

    | Switch(e,b,_,l,el) ->
        self#pLineDirective l
          ++ (align
                ++ text "switch ("
                ++ self#pExp () e
                ++ text ") "
                ++ self#pBlock () b)
    | Loop(b, l, el, _, _) -> begin
        (* Maybe the first thing is a conditional. Turn it into a WHILE *)
        try
          let term, bodystmts =
            let rec skipEmpty = function
                [] -> []
              | {skind=Instr [];labels=[]; _} :: rest -> skipEmpty rest
              | x -> x
            in
            (* Bill McCloskey: Do not remove the If if it has labels *)
            match skipEmpty b.bstmts with
              {skind=If(e,tb,fb,_,_); labels=[]; _} :: rest
                                              when not !printCilAsIs -> begin
                match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
                  [], {skind=Break _; labels=[]; _} :: _  -> e, rest
                | {skind=Break _; labels=[]; _} :: _, []
                                     -> UnOp(LNot, e, intType), rest
                | _ -> raise Not_found
              end
            | _ -> raise Not_found
          in
          self#pLineDirective l
            ++ text "wh"
            ++ (align
                  ++ text "ile ("
                  ++ self#pExp () term
                  ++ text ") "
                  ++ self#pBlock () {bstmts=bodystmts; battrs=b.battrs})

        with Not_found ->
          self#pLineDirective l
            ++ text "wh"
            ++ (align
                  ++ text "ile (1) "
                  ++ self#pBlock () b)
    end
    | Block b -> align ++ self#pBlock () b


  (*** GLOBALS ***)
  method pGlobal () (g:global) : doc =       (* global (vars, types, etc.) *)
    match g with
    | GFun (fundec, l) ->
        (* If the function has attributes then print a prototype because
          GCC cannot accept function attributes in a definition *)
        let oldattr = fundec.svar.vattr in
        (* Always print the file name before function declarations *)
        let proto =
          if oldattr <> [] then
            (self#pLineDirective l) ++ (self#pVDecl () fundec.svar)
              ++ chr ';' ++ line
          else nil in
        (* Temporarily remove the function attributes *)
        fundec.svar.vattr <- [];
        let body = (self#pLineDirective ~forcefile:true l)
                      ++ (self#pFunDecl () fundec) in
        fundec.svar.vattr <- oldattr;
        proto ++ body ++ line

    | GType (typ, l) ->
        self#pLineDirective ~forcefile:true l ++
          text "typedef "
          ++ (self#pType (Some (text typ.tname)) () typ.ttype)
          ++ text ";\n"

    | GEnumTag (enum, l) ->
        self#pLineDirective l ++
          text "enum" ++ align ++ text (" " ^ enum.ename) ++
          text " {" ++ line
          ++ (docList ~sep:(chr ',' ++ line)
                (fun (n,i, loc) ->
                  text (n ^ " = ")
                    ++ self#pExp () i)
                () enum.eitems)
          ++ unalign ++ line ++ text "} "
          ++ self#pAttrs () enum.eattr ++ text";\n"

    | GEnumTagDecl (enum, l) -> (* This is a declaration of a tag *)
        self#pLineDirective l ++
          text "enum " ++ text enum.ename ++ chr ' '
          ++ self#pAttrs () enum.eattr ++ text ";\n"

    | GCompTag (comp, l) -> (* This is a definition of a tag *)
        let n = comp.cname in
        let su, su1, su2 =
          if comp.cstruct then "struct", "str", "uct"
          else "union",  "uni", "on"
        in
        self#pLineDirective ~forcefile:true l ++
          text su1 ++ (align ++ text su2 ++ chr ' '
                         ++ text n
                         ++ text " {" ++ line
                         ++ ((docList ~sep:line (self#pFieldDecl ())) ()
                               comp.cfields)
                         ++ unalign)
          ++ line ++ text "}" ++
          (self#pAttrs () comp.cattr) ++ text ";\n"

    | GCompTagDecl (comp, l) -> (* This is a declaration of a tag *)
        let su = if comp.cstruct then "struct " else "union " in
        self#pLineDirective l
          ++ text su
          ++ text comp.cname ++ chr ' '
          ++ self#pAttrs () comp.cattr ++ text ";\n"

    | GVar (vi, io, l) ->
        self#pLineDirective ~forcefile:true l ++
          self#pVDecl () vi
          ++ chr ' '
          ++ (match io.init with
            None -> nil
          | Some i -> text " = " ++
                (let islong =
                  match i with
                    CompoundInit (_, il) when List.length il >= 8 -> true
                  | _ -> false
                in
                if islong then
                  line ++ self#pLineDirective l ++ text "  "
                else nil) ++
                (self#pInit () i))
          ++ text ";\n"

    (* print global variable 'extern' declarations, and function prototypes *)
    | GVarDecl (vi, l) ->
        if not !printCilAsIs && H.mem builtinFunctions vi.vname then begin
          (* Compiler builtins need no prototypes. Just print them in
             comments. *)
          text "/* compiler builtin: \n   " ++
            (self#pVDecl () vi)
            ++ text ";  */\n"

        end else
          self#pLineDirective l ++
            (self#pVDecl () vi)
            ++ text ";\n"

    | GAsm (s, l) ->
        self#pLineDirective l ++
          text ("__asm__(\"" ^ escape_string s ^ "\");\n")

    | GPragma (Attr(an, args), l) ->
        (* sm: suppress printing pragmas that gcc does not understand *)
        (* assume anything starting with "ccured" is ours *)
        (* also don't print the 'combiner' pragma *)
        (* nor 'cilnoremove' *)
        let suppress =
          not !print_CIL_Input &&
          ((startsWith "box" an) ||
           (startsWith "ccured" an) ||
           (an = "merger") ||
           (an = "cilnoremove")) in
        let d =
	  match an, args with
	  | _, [] ->
              text an
	  | "weak", [ACons (symbol, [])] ->
	      text "weak " ++ text symbol
	  | _ ->
            text (an ^ "(")
              ++ docList ~sep:(chr ',') (self#pAttrParam ()) () args
              ++ text ")"
        in
        self#pLineDirective l
          ++ (if suppress then text "/* " else text "")
          ++ (text "#pragma ")
          ++ d
          ++ (if suppress then text " */\n" else text "\n")

    | GText s  ->
        if s <> "//" then
          text s ++ text "\n"
        else
          nil


   method dGlobal (out: out_channel) (g: global) : unit =
     (* For all except functions and variable with initializers, use the
        pGlobal *)
     match g with
       GFun (fdec, l) ->
         (* If the function has attributes then print a prototype because
            GCC cannot accept function attributes in a definition *)
         let oldattr = fdec.svar.vattr in
         let proto =
           if oldattr <> [] then
             (self#pLineDirective l) ++ (self#pVDecl () fdec.svar)
               ++ chr ';' ++ line
           else nil in
         fprint out ~width:!lineLength
           (proto ++ (self#pLineDirective ~forcefile:true l));
         (* Temporarily remove the function attributes *)
         fdec.svar.vattr <- [];
         fprint out ~width:!lineLength (self#pFunDecl () fdec);
         fdec.svar.vattr <- oldattr;
         output_string out "\n"

     | GVar (vi, {init = Some i}, l) -> begin
         fprint out ~width:!lineLength
           (self#pLineDirective ~forcefile:true l ++
              self#pVDecl () vi
              ++ text " = "
              ++ (let islong =
                match i with
                  CompoundInit (_, il) when List.length il >= 8 -> true
                | _ -> false
              in
              if islong then
                line ++ self#pLineDirective l ++ text "  "
              else nil));
         self#dInit out 3 i;
         output_string out ";\n"
     end

     | g -> fprint out ~width:!lineLength (self#pGlobal () g)

   method pFieldDecl () fi =
     (self#pType
        (Some (text (if fi.fname = missingFieldName then "" else fi.fname)))
        ()
        fi.ftype)
       ++ text " "
       ++ (match fi.fbitfield with None -> nil
       | Some i -> text ": " ++ num i ++ text " ")
       ++ self#pAttrs () fi.fattr
       ++ text ";"

  method private pFunDecl () f =
      self#pVDecl () f.svar
      ++  line
      ++ text "{ "
      ++ (align
            (* locals. *)
            ++ line
            ++ (docList ~sep:line
                (fun vi -> match vi.vinit.init with
                | None -> self#pVDecl () vi ++ text ";"
                | Some i -> self#pVDecl () vi ++ text " = " ++
                    self#pInit () i ++ text ";")
                () (List.filter (fun v -> not v.vhasdeclinstruction) f.slocals))
            ++ line ++ line
            (* the body *)
            ++ ((* remember the declaration *) currentFormals <- f.sformals;
                let body = self#pBlock () f.sbody in
                currentFormals <- [];
                body))
      ++ line
      ++ text "}"

  (***** PRINTING DECLARATIONS and TYPES ****)

  method pType (nameOpt: doc option) (* Whether we are declaring a name or
                                        we are just printing a type *)
               () (t:typ) =       (* use of some type *)
    let name = match nameOpt with None -> nil | Some d -> d in
    let printAttributes (a: attributes) =
      let pa = self#pAttrs () a in
      match nameOpt with
      | None when not !print_CIL_Input ->
          (* Cannot print the attributes in this case because gcc does not
             like them here, except if we are printing for CIL. *)
          if pa = nil then nil else
          text "/*" ++ pa ++ text "*/"
      | _ -> pa
    in
    match t with
      TVoid a ->
        text "void"
          ++ self#pAttrs () a
          ++ text " "
          ++ name

    | TInt (ikind,a) ->
        d_ikind () ikind
          ++ self#pAttrs () a
          ++ text " "
          ++ name

    | TFloat(fkind, a) ->
        d_fkind () fkind
          ++ self#pAttrs () a
          ++ text " "
          ++ name

    | TComp (comp, a) -> (* A reference to a struct *)
        let su = if comp.cstruct then "struct" else "union" in
        text (su ^ " " ^ comp.cname ^ " ")
          ++ self#pAttrs () a
          ++ name

    | TEnum (enum, a) ->
        text ("enum " ^ enum.ename ^ " ")
          ++ self#pAttrs () a
          ++ name
    | TPtr (bt, a)  ->
        (* Parenthesize the ( * attr name) if a pointer to a function or an
           array. *)
        let (paren: doc option), (bt': typ) =
          match bt with
          | TFun _ | TArray _ -> Some (text "("), bt
          | _ -> None, bt
        in
        let name' = text "*" ++ printAttributes a ++ name in
        let name'' = (* Put the parenthesis *)
          match paren with
            Some p -> p ++ name' ++ text ")"
          | _ -> name'
        in
        self#pType
          (Some name'')
          ()
          bt'

    | TArray (elemt, lo, a) ->
        (* ignore the const attribute for arrays *)
        let a' = dropAttributes [ "pconst" ] a in
        let name' =
          if a' == [] then name else
          if nameOpt == None then printAttributes a' else
          text "(" ++ printAttributes a' ++ name ++ text ")"
        in
        self#pType
          (Some (name'
                   ++ text "["
                   ++ (match lo with None -> nil | Some e -> self#pExp () e)
                   ++ text "]"))
          ()
          elemt

    | TFun (restyp, args, isvararg, a) ->
        let name' =
          if a == [] then name else
          if nameOpt == None then printAttributes a else
          text "(" ++ printAttributes a ++ name ++ text ")"
        in
        self#pType
          (Some
             (name'
                ++ text "("
                ++ (align
                      ++
                      (if args = Some [] && isvararg then
                        text "..."
                      else
                        (if args = None then nil
                        else if args = Some [] then text "void"
                        else
                          let pArg (aname, atype, aattr) =
                            (self#pType (Some (text aname)) () atype)
                              ++ text " "
                              ++ self#pAttrs () aattr
                          in
                          (docList ~sep:(chr ',' ++ break) pArg) ()
                            (argsToList args))
                          ++ (if isvararg then break ++ text ", ..." else nil))
                      ++ unalign)
                ++ text ")"))
          ()
          restyp

  | TNamed (t, a) ->
      text t.tname ++ self#pAttrs () a ++ text " " ++ name

  | TBuiltin_va_list a ->
      text "__builtin_va_list"
       ++ self#pAttrs () a
        ++ text " "
        ++ name


  (**** PRINTING ATTRIBUTES *********)
  method pAttrs () (a: attributes) =
    self#pAttrsGen false a


  (* Print one attribute. Return also an indication whether this attribute
     should be printed inside the __attribute__ list *)
  method pAttr (Attr(an, args): attribute) : doc * bool =
    (* Recognize and take care of some known cases *)
    match an, args with
      "atomic", [] -> text "_Atomic", false
    | "const", [] -> nil, false (* don't print const directly, because of split local declarations *)
    | "pconst", [] -> text "const", false (* pconst means print const *)
          (* Put the aconst inside the attribute list *)
    | "complex", [] when (c99Mode ()) -> text "_Complex", false
    | "complex", [] -> text "__complex__", false
    | "aconst", [] -> text "__const__", true
    | "thread", [] -> text "__thread", false
    | "volatile", [] -> text "volatile", false
    | "restrict", [] -> text "__restrict", false
    | "missingproto", [] -> text "/* missing proto */", false
    | "asm", args ->
        text "__asm__("
          ++ docList (self#pAttrParam ()) () args
          ++ text ")", false
    (* we suppress printing mode(__si__) because it triggers an *)
    (* internal compiler error in all current gcc versions *)
    (* sm: I've now encountered a problem with mode(__hi__)... *)
    (* I don't know what's going on, but let's try disabling all "mode"..*)
    | "mode", [ACons(tag,[])] ->
        text "/* mode(" ++ text tag ++ text ") */", false

    (* sm: also suppress "format" because we seem to print it in *)
    (* a way gcc does not like *)
    | "format", _ -> text "/* format attribute */", false

    (* sm: here's another one I don't want to see gcc warnings about.. *)
    | "mayPointToStack", _ when not !print_CIL_Input
    (* [matth: may be inside another comment.]
      -> text "/*mayPointToStack*/", false
    *)
      -> text "", false
    | "arraylen", [a] ->
        (* text "/*[" ++ self#pAttrParam () a ++ text "]*/" *) nil, false


    | _ -> (* This is the default case *)
        (* Add underscores to the name *)
        let an' =  "__" ^ an ^ "__" in
        if args = [] then
          text an', true
        else
          text (an' ^ "(")
            ++ (docList (self#pAttrParam ()) () args)
            ++ text ")",
          true

  method private pAttrPrec (contextprec: int) () (a: attrparam) =
    let thisLevel = getParenthLevelAttrParam a in
    let needParens =
      if thisLevel >= contextprec then
	true
      else if contextprec == bitwiseLevel then
        (* quiet down some GCC warnings *)
	thisLevel == additiveLevel || thisLevel == comparativeLevel
      else
	false
    in
    if needParens then
      chr '(' ++ self#pAttrParam () a ++ chr ')'
    else
      self#pAttrParam () a


  method pAttrParam () a =
    let level = getParenthLevelAttrParam a in
    match a with
    | AInt n -> num n
    | AStr s -> text ("\"" ^ escape_string s ^ "\"")
    | ACons(s, []) -> text s
    | ACons(s,al) ->
        text (s ^ "(")
          ++ (docList (self#pAttrParam ()) () al)
          ++ text ")"
    | ASizeOfE a -> text "sizeof(" ++ self#pAttrParam () a ++ text ")"
    | ASizeOf t -> text "sizeof(" ++ self#pType None () t ++ text ")"
    | ASizeOfS ts -> text "sizeof(<typsig>)"
    | AAlignOfE a -> text "__alignof__(" ++ self#pAttrParam () a ++ text ")"
    | AAlignOf t -> text "__alignof__(" ++ self#pType None () t ++ text ")"
    | AAlignOfS ts -> text "__alignof__(<typsig>)"
    | AUnOp(u,a1) ->
        (d_unop () u) ++ chr ' ' ++ (self#pAttrPrec level () a1)

    | ABinOp(b,a1,a2) ->
        align
          ++ text "("
          ++ (self#pAttrPrec level () a1)
          ++ text ") "
          ++ (d_binop () b)
          ++ break
          ++ text " (" ++ (self#pAttrPrec level () a2) ++ text ") "
          ++ unalign
    | ADot (ap, s) -> (self#pAttrParam () ap) ++ text ("." ^ s)
    | AStar a1 ->
        text "(*" ++ (self#pAttrPrec derefStarLevel () a1) ++ text ")"
    | AAddrOf a1 -> text "& " ++ (self#pAttrPrec addrOfLevel () a1)
    | AIndex (a1, a2) -> self#pAttrParam () a1 ++ text "[" ++
                         self#pAttrParam () a2 ++ text "]"
    | AQuestion (a1, a2, a3) ->
          self#pAttrParam () a1 ++ text " ? " ++
          self#pAttrParam () a2 ++ text " : " ++
          self#pAttrParam () a3


  (* A general way of printing lists of attributes *)
  method private pAttrsGen (block: bool) (a: attributes) =
    (* Scan all the attributes and separate those that must be printed inside
       the __attribute__ list *)
    let rec loop (in__attr__: doc list) = function
        [] -> begin
          match in__attr__ with
            [] -> nil
          | _ :: _->
              (* sm: added 'forgcc' calls to not comment things out
                 if CIL is the consumer; this is to address a case
                 Daniel ran into where blockattribute(nobox) was being
                 dropped by the merger
               *)
              (if block then
                text (" " ^ (forgcc "/*") ^ " __blockattribute__(")
               else
                 text "__attribute__((")

                ++ (docList ~sep:(chr ',' ++ break)
                      (fun a -> a)) () in__attr__
                ++ text ")"
                ++ (if block then text (forgcc "*/") else text ")")
        end
      | x :: rest ->
          let dx, ina = self#pAttr x in
          if ina then
            loop (dx :: in__attr__) rest
          else if dx = nil then
            loop in__attr__ rest
          else
            dx ++ text " " ++ loop in__attr__ rest
    in
    let res = loop [] a in
    if res = nil then
      res
    else
      text " " ++ res ++ text " "

end (* class defaultCilPrinterClass *)

let defaultCilPrinter = new defaultCilPrinterClass

(* Top-level printing functions *)
let printType (pp: cilPrinter) () (t: typ) : doc =
  pp#pType None () t

let printExp (pp: cilPrinter) () (e: exp) : doc =
  pp#pExp () e

let printLval (pp: cilPrinter) () (lv: lval) : doc =
  pp#pLval () lv

let printGlobal (pp: cilPrinter) () (g: global) : doc =
  pp#pGlobal () g

let dumpGlobal (pp: cilPrinter) (out: out_channel) (g: global) : unit =
  pp#dGlobal out g

let printAttr (pp: cilPrinter) () (a: attribute) : doc =
  let ad, _ = pp#pAttr a in ad

let printAttrs (pp: cilPrinter) () (a: attributes) : doc =
  pp#pAttrs () a

let printInstr (pp: cilPrinter) () (i: instr) : doc =
  pp#pInstr () i

let printStmt (pp: cilPrinter) () (s: stmt) : doc =
  pp#pStmt () s

let printBlock (pp: cilPrinter) () (b: block) : doc =
  (* We must add the alignment ourselves, because pBlock will pop it *)
  align ++ pp#pBlock () b

let dumpStmt (pp: cilPrinter) (out: out_channel) (ind: int) (s: stmt) : unit =
  pp#dStmt out ind s

let dumpBlock (pp: cilPrinter) (out: out_channel) (ind: int) (b: block) : unit =
  pp#dBlock out ind b

let printInit (pp: cilPrinter) () (i: init) : doc =
  pp#pInit () i

let dumpInit (pp: cilPrinter) (out: out_channel) (ind: int) (i: init) : unit =
  pp#dInit out ind i

(* Now define some short cuts *)
let d_exp () e = printExp defaultCilPrinter () e
let _ = pd_exp := d_exp
let d_lval () lv = printLval defaultCilPrinter () lv
let d_offset base () off = defaultCilPrinter#pOffset base off
let d_init () i = printInit defaultCilPrinter () i
let d_type () t = printType defaultCilPrinter () t
let _ = pd_type := d_type
let d_global () g = printGlobal defaultCilPrinter () g
let d_attrlist () a = printAttrs defaultCilPrinter () a
let d_attr () a = printAttr defaultCilPrinter () a
let _ = pd_attr := d_attr
let d_attrparam () e = defaultCilPrinter#pAttrParam () e
let d_label () l = defaultCilPrinter#pLabel () l
let d_stmt () s = printStmt defaultCilPrinter () s
let d_block () b = printBlock defaultCilPrinter () b
let d_instr () i = printInstr defaultCilPrinter () i

let d_shortglobal () = function
    GPragma (Attr(an, _), _) -> dprintf "#pragma %s" an
  | GType (ti, _) -> dprintf "typedef %s" ti.tname
  | GVarDecl (vi, _) -> dprintf "declaration of %s" vi.vname
  | GVar (vi, _, _) -> dprintf "definition of %s" vi.vname
  | GCompTag(ci,_) -> dprintf "definition of %s" (compFullName ci)
  | GCompTagDecl(ci,_) -> dprintf "declaration of %s" (compFullName ci)
  | GEnumTag(ei,_) -> dprintf "definition of enum %s" ei.ename
  | GEnumTagDecl(ei,_) -> dprintf "declaration of enum %s" ei.ename
  | GFun(fd, _) -> dprintf "definition of %s" fd.svar.vname
  | GText _ -> text "GText"
  | GAsm _ -> text "GAsm"


(* sm: given an ordinary CIL object printer, yield one which
   behaves the same, except it never prints #line directives
   (this is useful for debugging printfs) *)
let dn_obj (func: unit -> 'a -> doc) : (unit -> 'a -> doc) =
begin
  (* construct the closure to return *)
  let theFunc () (obj:'a) : doc =
  begin
    let prevStyle = !lineDirectiveStyle in
    lineDirectiveStyle := None;
    let ret = (func () obj) in    (* call underlying printer *)
    lineDirectiveStyle := prevStyle;
    ret
  end in
  theFunc
end

(* now define shortcuts for the non-location-printing versions,
   with the naming prefix "dn_" *)
let dn_exp       = (dn_obj d_exp)
let dn_lval      = (dn_obj d_lval)
(* dn_offset is missing because it has a different interface *)
let dn_init      = (dn_obj d_init)
let dn_type      = (dn_obj d_type)
let dn_global    = (dn_obj d_global)
let dn_attrlist  = (dn_obj d_attrlist)
let dn_attr      = (dn_obj d_attr)
let dn_attrparam = (dn_obj d_attrparam)
let dn_stmt      = (dn_obj d_stmt)
let dn_instr     = (dn_obj d_instr)


(* Now define a cilPlainPrinter *)
class plainCilPrinterClass =
  (* We keep track of the composite types that we have done to avoid
     recursion *)
  let donecomps : (int, unit) H.t = H.create 13 in
  object (self)

  inherit defaultCilPrinterClass

  (*** PLAIN TYPES ***)
  method! pType (dn: doc option) () (t: typ) =
    match dn with
      None -> self#pOnlyType () t
    | Some d -> d ++ text " : " ++ self#pOnlyType () t

  method private pOnlyType () = function
     TVoid a -> dprintf "TVoid(@[%a@])" self#pAttrs a
   | TInt(ikind, a) -> dprintf "TInt(@[%a,@?%a@])"
         d_ikind ikind self#pAttrs a
   | TFloat(fkind, a) ->
       dprintf "TFloat(@[%a,@?%a@])" d_fkind fkind self#pAttrs a
   | TNamed (t, a) ->
       dprintf "TNamed(@[%s,@?%a,@?%a@])"
         t.tname self#pOnlyType t.ttype self#pAttrs a
   | TPtr(t, a) -> dprintf "TPtr(@[%a,@?%a@])" self#pOnlyType t self#pAttrs a
   | TArray(t,l,a) ->
       let dl = match l with
         None -> text "None" | Some l -> dprintf "Some(@[%a@])" self#pExp l in
       dprintf "TArray(@[%a,@?%a,@?%a@])"
         self#pOnlyType t insert dl self#pAttrs a
   | TEnum(enum,a) -> dprintf "Enum(%s,@[%a@])" enum.ename self#pAttrs a
   | TFun(tr,args,isva,a) ->
       dprintf "TFun(@[%a,@?%a%s,@?%a@])"
         self#pOnlyType tr
         insert
         (if args = None then text "None"
         else (docList ~sep:(chr ',' ++ break)
                 (fun (an,at,aa) ->
                   dprintf "%s: %a" an self#pOnlyType at))
             ()
             (argsToList args))
         (if isva then "..." else "") self#pAttrs a
   | TComp (comp, a) ->
       if H.mem donecomps comp.ckey then
         dprintf "TCompLoop(%s %s, _, %a)"
           (if comp.cstruct then "struct" else "union") comp.cname
           self#pAttrs comp.cattr
       else begin
         H.add donecomps comp.ckey (); (* Add it before we do the fields *)
         let doc = dprintf "TComp(@[%s %s,@?%a,@?%a,@?%a@])"
           (if comp.cstruct then "struct" else "union") comp.cname
           (docList ~sep:(chr ',' ++ break)
              (fun f -> dprintf "%s : %a" f.fname self#pOnlyType f.ftype))
           comp.cfields
           self#pAttrs comp.cattr
           self#pAttrs a
         in
         H.remove donecomps comp.ckey; (* Remove it after we do the fields, so printer doesn't have global state *)
         doc
       end
   | TBuiltin_va_list a ->
       dprintf "TBuiltin_va_list(%a)" self#pAttrs a


  (* Some plain pretty-printers. Unlike the above these expose all the
     details of the internal representation *)
  method! pExp () = function
    Const(c) ->
      let d_plainconst () c =
        match c with
          CInt(i, ik, so) ->
            let fmt = if isSigned ik then "%d" else "%x" in
            dprintf "Int(%s,%a,%s)"
              (Z.format fmt i)
              d_ikind ik
              (match so with Some s -> s | _ -> "None")
        | CStr(s, enc) ->
            let enc_string = match enc with No_encoding -> "_" | Utf8 -> "UTF8" in
            text ("CStr(\"" ^ escape_string s ^ "\"," ^ enc_string ^ ")")
        | CWStr(s,_) ->
            dprintf "CWStr(%a)" d_const c
        | CChr(c) -> text ("CChr('" ^ escape_char c ^ "')")
        | CReal(f, fk, so) ->
            dprintf "CReal(%f, %a, %s)"
              f
              d_fkind fk
              (match so with Some s -> s | _ -> "None")
        | CEnum(_, s, _) -> text s
      in
      text "Const(" ++ d_plainconst () c ++ text ")"


  | Lval(lv) ->
      text "Lval("
        ++ (align
              ++ self#pLval () lv
              ++ unalign)
        ++ text ")"

  | CastE(t,e) -> dprintf "CastE(@[%a,@?%a@])" self#pOnlyType t self#pExp e

  | UnOp(u,e1,_) ->
      dprintf "UnOp(@[%a,@?%a@])"
        d_unop u self#pExp e1

  | BinOp(b,e1,e2,_) ->
      let d_plainbinop () b =
        match b with
          PlusA -> text "PlusA"
        | PlusPI -> text "PlusPI"
        | IndexPI -> text "IndexPI"
        | MinusA -> text "MinusA"
        | MinusPP -> text "MinusPP"
        | MinusPI -> text "MinusPI"
        | _ -> d_binop () b
      in
      dprintf "%a(@[%a,@?%a@])" d_plainbinop b
        self#pExp e1 self#pExp e2

  | Question(e1,e2,e3,_) ->
      dprintf "Question(@[%a,@?%a,@?%a@])"
        self#pExp e1 self#pExp e2 self#pExp e3

  | SizeOf (t) ->
      text "sizeof(" ++ self#pType None () t ++ chr ')'
  | SizeOfE (e) ->
      text "sizeofE(" ++ self#pExp () e ++ chr ')'
  | SizeOfStr (s) ->
      text "sizeofStr(" ++ d_const () (CStr (s, No_encoding)) ++ chr ')'
  | AlignOf (t) ->
      text "__alignof__(" ++ self#pType None () t ++ chr ')'
  | AlignOfE (e) ->
      text "__alignof__(" ++ self#pExp () e ++ chr ')'
  | Imag e ->
      text "__imag__(" ++ self#pExp () e ++ chr ')'
  | Real e ->
      text "__real__(" ++ self#pExp () e ++ chr ')'
  | StartOf lv -> dprintf "StartOf(%a)" self#pLval lv
  | AddrOf (lv) -> dprintf "AddrOf(%a)" self#pLval lv
  | AddrOfLabel (sref) -> dprintf "AddrOfLabel(%a)" self#pStmt !sref



  method private d_plainoffset () = function
      NoOffset -> text "NoOffset"
    | Field(fi,o) ->
        dprintf "Field(@[%s:%a,@?%a@])"
          fi.fname self#pOnlyType fi.ftype self#d_plainoffset o
     | Index(e, o) ->
         dprintf "Index(@[%a,@?%a@])" self#pExp e self#d_plainoffset o

  method! pInit () = function
      SingleInit e -> dprintf "SI(%a)" d_exp e
    | CompoundInit (t, initl) ->
        let d_plainoneinit (o, i) =
          self#d_plainoffset () o ++ text " = " ++ self#pInit () i
        in
        dprintf "CI(@[%a,@?%a@])" self#pOnlyType t
          (docList ~sep:(chr ',' ++ break) d_plainoneinit) initl
(*
    | ArrayInit (t, len, initl) ->
        let idx = ref (- 1) in
        let d_plainoneinit i =
          incr idx;
          text "[" ++ num !idx ++ text "] = " ++ self#pInit () i
        in
        dprintf "AI(@[%a,%d,@?%a@])" self#pOnlyType t len
          (docList ~sep:(chr ',' ++ break) d_plainoneinit) initl
*)
  method! pLval () (lv: lval) =
    match lv with
    | Var vi, o -> dprintf "Var(@[%s,@?%a@])" vi.vname self#d_plainoffset o
    | Mem e, o -> dprintf "Mem(@[%a,@?%a@])" self#pExp e self#d_plainoffset o


end
let plainCilPrinter = new plainCilPrinterClass

(* And now some shortcuts *)
let d_plainexp () e = plainCilPrinter#pExp () e
let d_plaintype () t = plainCilPrinter#pType None () t
let d_plaininit () i = plainCilPrinter#pInit () i
let d_plainlval () l = plainCilPrinter#pLval () l

class type descriptiveCilPrinter = object
  inherit cilPrinter

  method startTemps: unit -> unit
  method stopTemps: unit -> unit
  method pTemps: unit -> Pretty.doc
end

class descriptiveCilPrinterClass (enable: bool) : descriptiveCilPrinter =
object (self)
  (** Like defaultCilPrinterClass, but instead of temporary variable
      names it prints the description that was provided when the temp was
      created.  This is usually better for messages that are printed for end
      users, although you may want the temporary names for debugging.

      The boolean here enables descriptive printing.  Usually use true
      here, but you can set enable to false to make this class behave
      like defaultCilPrinterClass. This allows subclasses to turn the
      feature off. *)
  inherit defaultCilPrinterClass as super

  val mutable temps: (varinfo * string * doc) list = []
  val mutable useTemps: bool = false

  method startTemps () : unit =
    temps <- [];
    useTemps <- true

  method stopTemps () : unit =
    temps <- [];
    useTemps <- false

  method pTemps () : doc =
    if temps = [] then
      nil
    else
      text "\nWhere:\n  " ++
      docList ~sep:(text "\n  ")
              (fun (_, s, d) -> dprintf "%s = %a" s insert d) ()
              (List.rev temps)

  method private pVarDescriptive (vi: varinfo) : doc =
    if vi.vdescr <> nil then begin
      if vi.vdescrpure || not useTemps then
        vi.vdescr
      else begin
        try
          let _, name, _ = List.find (fun (vi', _, _) -> vi == vi') temps in
          text name
        with Not_found ->
          let name = "tmp" ^ string_of_int (List.length temps) in
          temps <- (vi, name, vi.vdescr) :: temps;
          text name
      end
    end else
      super#pVar vi

  (* Only substitute temp vars that appear in expressions.
     (Other occurrences of lvalues are the left-hand sides of assignments,
      but we shouldn't substitute there since "foo(a,b) = foo(a,b)"
      would make no sense to the user.)  *)
  method! pExp () (e:exp) : doc =
    if enable then
      match e with
        Lval (Var vi, o)
      | StartOf (Var vi, o) ->
          self#pOffset (self#pVarDescriptive vi) o
      | AddrOf (Var vi, o) ->
          (* No parens needed, since offsets have higher precedence than & *)
          text "& " ++ self#pOffset (self#pVarDescriptive vi) o
      | _ -> super#pExp () e
    else
      super#pExp () e
end

let descriptiveCilPrinter: descriptiveCilPrinter =
  ((new descriptiveCilPrinterClass true) :> descriptiveCilPrinter)

let dd_exp = descriptiveCilPrinter#pExp
let dd_lval = descriptiveCilPrinter#pLval

(* zra: this allows pretty printers not in cil.ml to
   be exposed to cilmain.ml *)
let printerForMaincil = ref defaultCilPrinter

let rec d_typsig () = function
    TSArray (ts, eo, al) ->
      dprintf "TSArray(@[%a,@?%a,@?%a@])"
        d_typsig ts
        insert (text (match eo with None -> "None"
                       | Some e -> "Some " ^ string_of_cilint e))
        d_attrlist al
  | TSPtr (ts, al) ->
      dprintf "TSPtr(@[%a,@?%a@])"
        d_typsig ts d_attrlist al
  | TSComp (iss, name, al) ->
      dprintf "TSComp(@[%s %s,@?%a@])"
        (if iss then "struct" else "union") name
        d_attrlist al
  | TSFun (rt, args, isva, al) ->
      dprintf "TSFun(@[%a,@?%a,%B,@?%a@])"
        d_typsig rt
        insert
        (match args with
        | None -> text "None"
        | Some args ->
            docList ~sep:(chr ',' ++ break) (d_typsig ()) () args)
        isva
        d_attrlist al
  | TSEnum (n, al) ->
      dprintf "TSEnum(@[%s,@?%a@])"
        n d_attrlist al
  | TSBase t -> dprintf "TSBase(%a)" d_type t


let newVID () =
  let t = !nextGlobalVID in
  incr nextGlobalVID;
  t

   (* Make a varinfo. Used mostly as a helper function below  *)
let makeVarinfo global name ?init typ =
  (* Strip const from type for locals *)
  let vi =
    { vname = name;
      vid   = newVID ();
      vglob = global;
      vtype = if global then typ else typeRemoveAttributes ["pconst"] typ;
      vdecl = lu;
      vinit = {init=init};
      vinline = false;
      vattr = [];
      vstorage = NoStorage;
      vaddrof = false;
      vreferenced = false;
      vdescr = nil;
      vdescrpure = true;
      vhasdeclinstruction = false;
    } in
  vi

let copyVarinfo (vi: varinfo) (newname: string) : varinfo =
  let vi' = {vi with vname = newname; vid = newVID () } in
  vi'

let makeLocal fdec name typ init = (* a helper function *)
  fdec.smaxid <- 1 + fdec.smaxid;
  let vi = makeVarinfo false name ?init:init typ in
  vi

   (* Make a local variable and add it to a function *)
let makeLocalVar fdec ?(insert = true) name ?init typ =
  let vi = makeLocal fdec name typ init in
  if insert then fdec.slocals <- fdec.slocals @ [vi];
  vi

let makeTempVar fdec ?(insert = true) ?(name = "__cil_tmp")
                ?(descr = nil) ?(descrpure = true) typ : varinfo =
  let rec findUniqueName () : string=
    let n = name ^ (string_of_int (1 + fdec.smaxid)) in
    (* Is this check a performance problem?  We could bring the old
       unchecked makeTempVar back as a separate function that assumes
       the prefix name does not occur in the original program. *)
    if (List.exists (fun vi -> vi.vname = n) fdec.slocals)
      || (List.exists (fun vi -> vi.vname = n) fdec.sformals) then begin
        fdec.smaxid <- 1 + fdec.smaxid;
        findUniqueName ()
      end else
        n
  in
  let name = findUniqueName () in
  let vi = makeLocalVar fdec ~insert name typ in
  vi.vdescr <- descr;
  vi.vdescrpure <- descrpure;
  vi


(* Set the formals and re-create the function name based on the information*)
let setFormals (f: fundec) (forms: varinfo list) =
  f.sformals <- forms; (* Set the formals *)
  match unrollType f.svar.vtype with
    TFun(rt, _, isva, fa) ->
      f.svar.vtype <-
         TFun(rt,
              Some (Util.list_map (fun a -> (a.vname, a.vtype, a.vattr)) forms),
              isva, fa)
  | _ -> E.s (E.bug "Set formals. %s does not have function type\n"
                f.svar.vname)

   (* Set the types of arguments and results as given by the function type
      passed as the second argument *)
let setFunctionType (f: fundec) (t: typ) =
  match unrollType t with
    TFun (rt, Some args, va, a) ->
      if List.length f.sformals <> List.length args then
        E.s (E.bug "setFunctionType: number of arguments differs from the number of formals");
      (* Change the function type. *)
      f.svar.vtype <- t;
      (* Change the sformals and we know that indirectly we'll change the
         function type *)
      List.iter2
        (fun (an,at,aa) f ->
          f.vtype <- at; f.vattr <- aa)
        args f.sformals

  | _ -> E.s (E.bug "setFunctionType: not a function type")


   (* Set the types of arguments and results as given by the function type
      passed as the second argument *)
let setFunctionTypeMakeFormals (f: fundec) (t: typ) =
  match unrollType t with
    TFun (rt, Some args, va, a) ->
      if f.sformals <> [] then
        E.s (E.warn "setFunctionTypMakeFormals called on function %s with some formals already"
               f.svar.vname);
      (* Change the function type. *)
      f.svar.vtype <- t;
      f.sformals <- [];

      f.sformals <- Util.list_map (fun (n,t,a) -> makeLocal f n t None) args;

      setFunctionType f t

  | _ -> E.s (E.bug "setFunctionTypeMakeFormals: not a function type: %a"
             d_type t)


let setMaxId (f: fundec) =
  f.smaxid <- List.length f.sformals + List.length f.slocals


  (* Make a formal variable for a function. Insert it in both the sformals
     and the type of the function. You can optionally specify where to insert
     this one. If where = "^" then it is inserted first. If where = "$" then
     it is inserted last. Otherwise where must be the name of a formal after
     which to insert this. By default it is inserted at the end. *)
let makeFormalVar fdec ?(where = "$") name typ : varinfo =
  (* Search for the insertion place *)
  let thenewone = ref fdec.svar in (* Just a placeholder *)
  let makeit () : varinfo =
    let vi = makeLocal fdec name typ None in
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
      is unique *)
let makeGlobalVar name typ =
  let vi = makeVarinfo true name typ in
  vi


   (* Make an empty function *)
let emptyFunction name =
  { svar  = makeGlobalVar name (TFun(voidType, Some [], false,[]));
    smaxid = 0;
    slocals = [];
    sformals = [];
    sbody = mkBlock [];
    smaxstmtid = None;
    sallstmts = [];
  }


    (* A dummy function declaration handy for initialization *)
let dummyFunDec = emptyFunction "@dummy"
let dummyFile =
  { globals = [];
    fileName = "<dummy>";
    globinit = None;
    globinitcalled = false;}

(***** Load and store files as unmarshalled Ocaml binary data. ****)
type savedFile =
    { savedFile: file;
      savedNextVID: int;
      savedNextCompinfoKey: int}

let saveBinaryFileChannel (cil_file : file) (outchan : out_channel) =
  let save = {savedFile = cil_file;
              savedNextVID = !nextGlobalVID;
              savedNextCompinfoKey = !nextCompinfoKey} in
  Marshal.to_channel outchan save []

let saveBinaryFile (cil_file : file) (filename : string) =
  let outchan = open_out_bin filename in
  saveBinaryFileChannel cil_file outchan;
  close_out outchan

(** Read a {!file} in binary form from the filesystem. The first
   argument is the name of a file previously created by
   {!saveBinaryFile}. Because this also reads some global state,
   this should be called before any other CIL code is parsed or generated. *)
let loadBinaryFile (filename : string) : file =
  let inchan = open_in_bin filename in
  let loaded : savedFile = (Marshal.from_channel inchan : savedFile) in
  close_in inchan ;
  (* nextGlobalVID = 11 because CIL initialises many dummy variables *)
  if !nextGlobalVID != 11 || !nextCompinfoKey != 1 then begin
    (* In this case, we should change all of the varinfo and compinfo
       keys in loaded.savedFile to prevent conflicts.  But since that hasn't
       been implemented yet, just print a warning.  If you do implement this,
       please send it to the CIL maintainers. *)
    ignore (E.warn "You are possibly loading a binary file after another file has been loaded.  This isn't currently supported, so varinfo and compinfo id numbers may conflict.")
  end;
  nextGlobalVID := max loaded.savedNextVID !nextGlobalVID;
  nextCompinfoKey := max loaded.savedNextCompinfoKey !nextCompinfoKey;
  loaded.savedFile


(* Take the name of a file and make a valid symbol name out of it. There are
   a few characters that are not valid in symbols *)
let makeValidSymbolName (s: string) =
  let b = Bytes.copy (Bytes.of_string s) in (* So that we can update in place *)
  let l = String.length s in
  for i = 0 to l - 1 do
    let c = String.get s i in
    let isinvalid =
      match c with
        '-' | '.' -> true
      | _ -> false
    in
    if isinvalid then
      Bytes.set b i '_';
  done;
  Bytes.to_string b

let rec addOffset (toadd: offset) (off: offset) : offset =
  match off with
    NoOffset -> toadd
  | Field(fid', offset) -> Field(fid', addOffset toadd offset)
  | Index(e, offset) -> Index(e, addOffset toadd offset)

 (* Add an offset at the end of an lv *)
let addOffsetLval toadd (b, off) : lval =
 b, addOffset toadd off

let rec removeOffset (off: offset) : offset * offset =
  match off with
    NoOffset -> NoOffset, NoOffset
  | Field(f, NoOffset) -> NoOffset, off
  | Index(i, NoOffset) -> NoOffset, off
  | Field(f, restoff) ->
      let off', last = removeOffset restoff in
      Field(f, off'), last
  | Index(i, restoff) ->
      let off', last = removeOffset restoff in
      Index(i, off'), last

let removeOffsetLval ((b, off): lval) : lval * offset =
  let off', last = removeOffset off in
  (b, off'), last


(*** Define the visiting engine ****)
(* visit all the nodes in a Cil expression *)
let doVisit (vis: cilVisitor)
            (action: 'a visitAction)
            (children: cilVisitor -> 'a -> 'a)
            (node: 'a) : 'a =
  match action with
    SkipChildren -> node
  | ChangeTo node' -> node'
  | DoChildren -> children vis node
  | ChangeDoChildrenPost(node', f) -> f (children vis node')

(* mapNoCopy is like map but avoid copying the list if the function does not
   change the elements. *)
let mapNoCopy (f: 'a -> 'a) l =
  let rec aux acc changed = function
    [] -> if changed then List.rev acc else l
  | i :: resti ->
      let i' = f i in
      aux (i' :: acc) (changed || i != i') resti
  in aux [] false l

let mapNoCopyList (f: 'a -> 'a list) l =
  let rec aux acc changed = function
    [] -> if changed then List.rev acc else l
  | i :: resti ->
      let il' = f i in
      let has_changed =
        match il' with
          [i'] when i' == i -> false
        | _ -> true in
      aux (List.rev_append il' acc) (changed || has_changed) resti
  in aux [] false l

(* A visitor for lists *)
let doVisitList  (vis: cilVisitor)
                 (action: 'a list visitAction)
                 (children: cilVisitor -> 'a -> 'a)
                 (node: 'a) : 'a list =
  match action with
    SkipChildren -> [node]
  | ChangeTo nodes' -> nodes'
  | DoChildren -> [children vis node]
  | ChangeDoChildrenPost(nodes', f) ->
      f (mapNoCopy (fun n -> children vis n) nodes')

let debugVisit = false

let rec visitCilExpr (vis: cilVisitor) (e: exp) : exp =
  doVisit vis (vis#vexpr e) childrenExp e
and childrenExp (vis: cilVisitor) (e: exp) : exp =
  let vExp e = visitCilExpr vis e in
  let vTyp t = visitCilType vis t in
  let vLval lv = visitCilLval vis lv in
  match e with
  | Const (CEnum(v, s, ei)) ->
      let v' = vExp v in
      if v' != v then Const (CEnum(v', s, ei)) else e

  | Const _ -> e
  | SizeOf t ->
      let t'= vTyp t in
      if t' != t then SizeOf t' else e
  | SizeOfE e1 ->
      let e1' = vExp e1 in
      if e1' != e1 then SizeOfE e1' else e
  | SizeOfStr s -> e
  | Real e1 ->
    let e1' = vExp e1 in
    if e1' != e1 then Real e1' else e
  | Imag e1 ->
    let e1' = vExp e1 in
    if e1' != e1 then Imag e1' else e
  | AlignOf t ->
      let t' = vTyp t in
      if t' != t then AlignOf t' else e
  | AlignOfE e1 ->
      let e1' = vExp e1 in
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
  | Question (e1, e2, e3, t) ->
      let e1' = vExp e1 in let e2' = vExp e2 in let e3' = vExp e3 in let t' = vTyp t in
      if e1' != e1 || e2' != e2 || e3' != e3 || t' != t then Question(e1',e2',e3',t') else e
  | CastE (t, e1) ->
      let t' = vTyp t in let e1' = vExp e1 in
      if t' != t || e1' != e1 then CastE(t', e1') else e
  | AddrOf lv ->
      let lv' = vLval lv in
      if lv' != lv then AddrOf lv' else e
  | AddrOfLabel _ -> e
  | StartOf lv ->
      let lv' = vLval lv in
      if lv' != lv then StartOf lv' else e

and visitCilInit (vis: cilVisitor) (forglob: varinfo)
                 (atoff: offset) (i: init) : init =
  let childrenInit (vis: cilVisitor) (i: init) : init =
    let fExp e = visitCilExpr vis e in
    let fTyp t = visitCilType vis t in
    match i with
    | SingleInit e ->
        let e' = fExp e in
        if e' != e then SingleInit e' else i
    | CompoundInit (t, initl) ->
        let t' = fTyp t in
        (* Collect the new initializer list, in reverse. We prefer two
           traversals to ensure tail-recursion. *)
        let newinitl : (offset * init) list ref = ref [] in
        (* Keep track whether the list has changed *)
        let hasChanged = ref false in
        let doOneInit ((o, i) as oi) =
          let o' = visitCilInitOffset vis o in    (* use initializer version *)
          let i' = visitCilInit vis forglob (addOffset o' atoff) i in
          let newio =
            if o' != o || i' != i then
              begin hasChanged := true; (o', i') end else oi
          in
          newinitl := newio :: !newinitl
        in
        List.iter doOneInit initl;
        let initl' = if !hasChanged then List.rev !newinitl else initl in
        if t' != t || initl' != initl then CompoundInit (t', initl') else i
  in
  doVisit vis (vis#vinit forglob atoff i) childrenInit i

and visitCilLval (vis: cilVisitor) (lv: lval) : lval =
  doVisit vis (vis#vlval lv) childrenLval lv
and childrenLval (vis: cilVisitor) (lv: lval) : lval =
  (* and visit its subexpressions *)
  let vExp e = visitCilExpr vis e in
  let vOff off = visitCilOffset vis off in
  match lv with
    Var v, off ->
      let v'   = doVisit vis (vis#vvrbl v) (fun _ x -> x) v in
      let off' = vOff off in
      if v' != v || off' != off then Var v', off' else lv
  | Mem e, off ->
      let e' = vExp e in
      let off' = vOff off in
      if e' != e || off' != off then Mem e', off' else lv

and visitCilOffset (vis: cilVisitor) (off: offset) : offset =
  doVisit vis (vis#voffs off) childrenOffset off
and childrenOffset (vis: cilVisitor) (off: offset) : offset =
  let vOff off = visitCilOffset vis off in
  match off with
    Field (f, o) ->
      let o' = vOff o in
      if o' != o then Field (f, o') else off
  | Index (e, o) ->
      let e' = visitCilExpr vis e in
      let o' = vOff o in
      if e' != e || o' != o then Index (e', o') else off
  | NoOffset -> off

(* sm: for offsets in initializers, the 'startvisit' will be the
   vinitoffs method, but we can re-use the childrenOffset from
   above since recursive offsets are visited by voffs.  (this point
   is moot according to cil.mli which claims the offsets in
   initializers will never recursively contain offsets)
 *)
and visitCilInitOffset (vis: cilVisitor) (off: offset) : offset =
  doVisit vis (vis#vinitoffs off) childrenOffset off

and visitCilInstr (vis: cilVisitor) (i: instr) : instr list =
  let oldloc = !currentLoc in
  currentLoc := (get_instrLoc i);
  assertEmptyQueue vis;
  let res = doVisitList vis (vis#vinst i) childrenInstr i in
  currentLoc := oldloc;
  (* See if we have accumulated some instructions *)
  vis#unqueueInstr () @ res

and childrenInstr (vis: cilVisitor) (i: instr) : instr =
  let fExp e = visitCilExpr vis e in
  let fLval lv = visitCilLval vis lv in
  match i with
  | VarDecl(v,l) -> i
  | Set(lv,e,l,el) ->
      let lv' = fLval lv in let e' = fExp e in
      if lv' != lv || e' != e then Set(lv',e',l,el) else i
  | Call(None,f,args,l,el) ->
      let f' = fExp f in let args' = mapNoCopy fExp args in
      if f' != f || args' != args then Call(None,f',args',l,el) else i
  | Call(Some lv,fn,args,l,el) ->
      let lv' = fLval lv in let fn' = fExp fn in
      let args' = mapNoCopy fExp args in
      if lv' != lv || fn' != fn || args' != args
      then Call(Some lv', fn', args', l, el) else i

  | Asm(sl,isvol,outs,ins,clobs,gotos,l) ->
      let outs' = mapNoCopy (fun ((id,s,lv) as pair) ->
                               let lv' = fLval lv in
                               if lv' != lv then (id,s,lv') else pair) outs in
      let ins'  = mapNoCopy (fun ((id,s,e) as pair) ->
                               let e' = fExp e in
                               if e' != e then (id,s,e') else pair) ins in
      if outs' != outs || ins' != ins then
        Asm(sl,isvol,outs',ins',clobs,gotos,l) else i


(* visit all nodes in a Cil statement tree in preorder *)
and visitCilStmt (vis: cilVisitor) (s: stmt) : stmt =
  let oldloc = !currentLoc in
  currentLoc := (get_stmtLoc s.skind) ;
  assertEmptyQueue vis;
  let toPrepend : instr list ref = ref [] in (* childrenStmt may add to this *)
  let res = doVisit vis (vis#vstmt s) (childrenStmt toPrepend) s in
  (* Now see if we have saved some instructions *)
  toPrepend := !toPrepend @ vis#unqueueInstr ();
  (match !toPrepend with
    [] -> () (* Return the same statement *)
  | _ ->
      (* Make our statement contain the instructions to prepend *)
      res.skind <- Block { battrs = []; bstmts = [ mkStmt (Instr !toPrepend);
                                                   mkStmt res.skind ] });
  currentLoc := oldloc;
  res

and childrenStmt (toPrepend: instr list ref) : cilVisitor -> stmt -> stmt =
  (* this is a hack to avoid currying and reduce GC pressure *)
  () ; fun vis s ->
  let fExp e = (visitCilExpr vis e) in
  let fBlock b = visitCilBlock vis b in
  let fInst i = visitCilInstr vis i in
  (* Just change the statement kind *)
  let skind' =
    match s.skind with
      Break _ | Continue _ | Goto _ | Return (None, _) -> s.skind
    | ComputedGoto (e, l) ->
         let e' = fExp e in
         if e' != e then ComputedGoto (e', l) else s.skind
    | Return (Some e, l) ->
        let e' = fExp e in
        if e' != e then Return (Some e', l) else s.skind
    | Loop (b, l, el, s1, s2) ->
        let b' = fBlock b in
        if b' != b then Loop (b', l, el, s1, s2) else s.skind
    | If(e, s1, s2, l, el) ->
        let e' = fExp e in
        (*if e queued any instructions, pop them here and remember them so that
          they are inserted before the If stmt, not in the then block. *)
        toPrepend := vis#unqueueInstr ();
        let s1'= fBlock s1 in let s2'= fBlock s2 in
        (* the stmts in the blocks should have cleaned up after themselves.*)
        assertEmptyQueue vis;
        if e' != e || s1' != s1 || s2' != s2 then
          If(e', s1', s2', l, el) else s.skind
    | Switch (e, b, stmts, l, el) ->
        let e' = fExp e in
        toPrepend := vis#unqueueInstr (); (* insert these before the switch *)
        let b' = fBlock b in
        (* the stmts in b should have cleaned up after themselves.*)
        assertEmptyQueue vis;
        (* Don't do stmts, but we better not change those *)
        if e' != e || b' != b then Switch (e', b', stmts, l, el) else s.skind
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
        Case (e, l, el) as lb ->
          let e' = fExp e in
          if e' != e then Case (e', l, el) else lb
        | CaseRange (e1, e2, l, el) as lb ->
          let e1' = fExp e1 in
          let e2' = fExp e2 in
          if e1' != e1 || e2' != e2 then CaseRange (e1', e2', l, el) else lb
        | lb -> lb
    in
    mapNoCopy fLabel s.labels
  in
  if labels' != s.labels then s.labels <- labels';
  s



and visitCilBlock (vis: cilVisitor) (b: block) : block =
  doVisit vis (vis#vblock b) childrenBlock b
and childrenBlock (vis: cilVisitor) (b: block) : block =
  let fStmt s = visitCilStmt vis s in
  let stmts' = mapNoCopy fStmt b.bstmts in
  if stmts' != b.bstmts then { battrs = b.battrs; bstmts = stmts'} else b


and visitCilType (vis : cilVisitor) (t : typ) : typ =
  doVisit vis (vis#vtype t) childrenType t
and childrenType (vis : cilVisitor) (t : typ) : typ =
  (* look for types referred to inside t's definition *)
  let fTyp t  = visitCilType vis t in
  let fAttr a = visitCilAttributes vis a in
  match t with
    TPtr(t1, a) ->
      let t1' = fTyp t1 in
      let a' = fAttr a in
      if t1' != t1 || a' != a then TPtr(t1', a') else t
  | TArray(t1, None, a) ->
      let t1' = fTyp t1 in
      let a' = fAttr a in
      if t1' != t1 || a' != a  then TArray(t1', None, a') else t
  | TArray(t1, Some e, a) ->
      let t1' = fTyp t1 in
      let e' = visitCilExpr vis e in
      let a' = fAttr a in
      if t1' != t1 || e' != e  || a' != a then TArray(t1', Some e', a') else t

      (* DON'T recurse into the compinfo, this is done in visitCilGlobal.
	 User can iterate over cinfo.cfields manually, if desired.*)
  | TComp(cinfo, a) ->
      let a' = fAttr a in
      if a != a' then TComp(cinfo, a') else t

  | TFun(rettype, args, isva, a) ->
      let rettype' = fTyp rettype in
      (* iterate over formals, as variable declarations *)
      let argslist = argsToList args in
      let visitArg ((an,at,aa) as arg) =
        let at' = fTyp at in
        let aa' = fAttr aa in
        if at' != at || aa' != aa then (an,at',aa') else arg
      in
      let argslist' = mapNoCopy visitArg argslist in
      let a' = fAttr a in
      if rettype' != rettype || argslist' != argslist || a' != a  then
        let args' = if argslist' == argslist then args else Some argslist' in
        TFun(rettype', args', isva, a') else t

  | TNamed(t1, a) -> (* Do not go into the type. Will do it at the time of
                        GType *)
      let a' = fAttr a in
      if a' != a  then TNamed (t1, a') else t

  | _ ->  (* other types (TVoid, TInt, TFloat, TEnum, and TBuiltin_va_list)
             don't contain nested types, but they do have attributes. *)
      let a = typeAttrs t in
      let a' = fAttr a in
      if a' != a  then setTypeAttrs t a' else t


(* for declarations, we visit the types inside; but for uses, *)
(* we just visit the varinfo node *)
and visitCilVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  doVisit vis (vis#vvdec v) childrenVarDecl v
and childrenVarDecl (vis : cilVisitor) (v : varinfo) : varinfo =
  v.vtype <- visitCilType vis v.vtype;
  v.vattr <- visitCilAttributes vis v.vattr;
  (match v.vinit.init with
    None -> ()
  | Some i -> let i' = visitCilInit vis v NoOffset i in
    if i' != i then v.vinit.init <- Some i');
  v

and visitCilAttributes (vis: cilVisitor) (al: attribute list) : attribute list=
   let al' =
     mapNoCopyList (fun x -> doVisitList vis (vis#vattr x) childrenAttribute x) al in
   if al' != al then
     (* Must re-sort *)
     addAttributes al' []
   else
     al
and childrenAttribute (vis: cilVisitor) (a: attribute) : attribute =
  let fAttrP a = visitCilAttrParams vis a in
  match a with
    Attr (n, args) ->
      let args' = mapNoCopy fAttrP args in
      if args' != args then Attr(n, args') else a


and visitCilAttrParams (vis: cilVisitor) (a: attrparam) : attrparam =
   doVisit vis (vis#vattrparam a) childrenAttrparam a
and childrenAttrparam (vis: cilVisitor) (aa: attrparam) : attrparam =
  let fTyp t  = visitCilType vis t in
  let fAttrP a = visitCilAttrParams vis a in
  match aa with
      AInt _ | AStr _ -> aa
    | ACons(n, args) ->
        let args' = mapNoCopy fAttrP args in
        if args' != args then ACons(n, args') else aa
    | ASizeOf t ->
        let t' = fTyp t in
        if t' != t then ASizeOf t' else aa
    | ASizeOfE e ->
        let e' = fAttrP e in
        if e' != e then ASizeOfE e' else aa
    | AAlignOf t ->
        let t' = fTyp t in
        if t' != t then AAlignOf t' else aa
    | AAlignOfE e ->
        let e' = fAttrP e in
        if e' != e then AAlignOfE e' else aa
    | ASizeOfS _ | AAlignOfS _ ->
        ignore (warn "Visitor inside of a type signature.");
        aa
    | AUnOp (uo, e1) ->
        let e1' = fAttrP e1 in
        if e1' != e1 then AUnOp (uo, e1') else aa
    | ABinOp (bo, e1, e2) ->
        let e1' = fAttrP e1 in
        let e2' = fAttrP e2 in
        if e1' != e1 || e2' != e2 then ABinOp (bo, e1', e2') else aa
    | ADot (ap, s) ->
        let ap' = fAttrP ap in
        if ap' != ap then ADot (ap', s) else aa
    | AStar ap ->
        let ap' = fAttrP ap in
        if ap' != ap then AStar ap' else aa
    | AAddrOf ap ->
        let ap' = fAttrP ap in
        if ap' != ap then AAddrOf ap' else aa
    | AIndex (e1, e2) ->
        let e1' = fAttrP e1 in
        let e2' = fAttrP e2 in
        if e1' != e1 || e2' != e2 then AIndex (e1', e2') else aa
    | AQuestion (e1, e2, e3) ->
        let e1' = fAttrP e1 in
        let e2' = fAttrP e2 in
        let e3' = fAttrP e3 in
        if e1' != e1 || e2' != e2 || e3' != e3
        then AQuestion (e1', e2', e3') else aa


let rec visitCilFunction (vis : cilVisitor) (f : fundec) : fundec =
  if debugVisit then ignore (E.log "Visiting function %s\n" f.svar.vname);
  assertEmptyQueue vis;
  let f = doVisit vis (vis#vfunc f) childrenFunction f in

  let toPrepend = vis#unqueueInstr () in
  if toPrepend <> [] then
    f.sbody.bstmts <- mkStmt (Instr toPrepend) :: f.sbody.bstmts;
  f

and childrenFunction (vis : cilVisitor) (f : fundec) : fundec =
  let visitVarDecl vd = visitCilVarDecl vis vd in
  f.svar <- visitCilVarDecl vis f.svar; (* hit the function name *)
  (* visit local declarations *)
  f.slocals <- mapNoCopy visitVarDecl f.slocals;
  (* visit the formals *)
  let newformals = mapNoCopy visitVarDecl f.sformals in
  (* Make sure the type reflects the formals *)
  setFormals f newformals;
  (* Remember any new instructions that were generated while visiting
     variable declarations. *)
  let toPrepend = vis#unqueueInstr () in

  f.sbody <- visitCilBlock vis f.sbody;        (* visit the body *)
  if toPrepend <> [] then
    f.sbody.bstmts <- mkStmt (Instr toPrepend) :: f.sbody.bstmts;
  f

let rec visitCilGlobal (vis: cilVisitor) (g: global) : global list =
  (*(trace "visit" (dprintf "visitCilGlobal\n"));*)
  let oldloc = !currentLoc in
  currentLoc := (get_globalLoc g) ;
  currentGlobal := g;
  let res = doVisitList vis (vis#vglob g) childrenGlobal g in
  currentLoc := oldloc;
  res
and childrenGlobal (vis: cilVisitor) (g: global) : global =
  match g with
  | GFun (f, l) ->
      let f' = visitCilFunction vis f in
      if f' != f then GFun (f', l) else g
  | GType(t, l) ->
      t.ttype <- visitCilType vis t.ttype;
      g

  | GEnumTagDecl _ | GCompTagDecl _ -> g (* Nothing to visit *)
  | GEnumTag (enum, _) ->
      (* (trace "visit" (dprintf "visiting global enum %s\n" enum.ename)); *)
      (* Do the values and attributes of the enumerated items *)
      let itemVisit (name, exp, loc) = (name, visitCilExpr vis exp, loc) in
      enum.eitems <- mapNoCopy itemVisit enum.eitems;
      enum.eattr <- visitCilAttributes vis enum.eattr;
      g

  | GCompTag (comp, _) ->
      (* (trace "visit" (dprintf "visiting global comp %s\n" comp.cname)); *)
      (* Do the types and attributes of the fields *)
      let fieldVisit = fun fi ->
        fi.ftype <- visitCilType vis fi.ftype;
        fi.fattr <- visitCilAttributes vis fi.fattr
      in
      List.iter fieldVisit comp.cfields;
      comp.cattr <- visitCilAttributes vis comp.cattr;
      g

  | GVarDecl(v, l) ->
      let v' = visitCilVarDecl vis v in
      if v' != v then GVarDecl (v', l) else g
  | GVar (v, inito, l) ->
      let v' = visitCilVarDecl vis v in
      if v' != v then GVar (v', inito, l) else g

  | GPragma (a, l) -> begin
      match visitCilAttributes vis [a] with
        [a'] -> if a' != a then GPragma (a', l) else g
      | _ -> E.s (E.unimp "visitCilAttributes returns more than one attribute")
  end
  | _ -> g


(** A visitor that does constant folding. If "machdep" is true then we do
   machine dependent simplification (e.g., sizeof) *)
class constFoldVisitorClass (machdep: bool) : cilVisitor = object
  inherit nopCilVisitor

  method! vinst i =
    match i with
      (* Skip two functions to which we add Sizeof to the type arguments.
         See the comments for these above. *)
      Call(_,(Lval (Var vi,NoOffset)),_,_,_)
        when ((vi.vname = "__builtin_va_arg")
              || (vi.vname = "__builtin_types_compatible_p")) ->
          SkipChildren
    | _ -> DoChildren
  method! vexpr (e: exp) =
    (* Do it bottom up *)
    ChangeDoChildrenPost (e, constFold machdep)

end
let constFoldVisitor (machdep: bool) = new constFoldVisitorClass machdep

(* Iterate over all globals, including the global initializer *)
let iterGlobals (fl: file)
                (doone: global -> unit) : unit =
  let doone' g =
      currentLoc := get_globalLoc g;
      doone g
  in
  List.iter doone' fl.globals;
  (match fl.globinit with
    None -> ()
  | Some g -> doone' (GFun(g, locUnknown)))

(* Fold over all globals, including the global initializer *)
let foldGlobals (fl: file)
                (doone: 'a -> global -> 'a)
                (acc: 'a) : 'a =
  let doone' acc g =
      currentLoc := get_globalLoc g;
      doone acc g
  in
  let acc' = List.fold_left doone' acc fl.globals in
  (match fl.globinit with
    None -> acc'
  | Some g -> doone' acc' (GFun(g, locUnknown)))

(** Find a function or function prototype with the given name in the file.
    If it does not exist, create a prototype with the given type, and return
    the new varinfo.  This is useful when you need to call a libc function
    whose prototype may or may not already exist in the file.

    Because the new prototype is added to the start of the file, you shouldn't
    refer to any struct or union types in the function type.*)
let findOrCreateFunc (f:file) (name:string) (t:typ) : varinfo =
  let rec search glist =
    match glist with
	GVarDecl(vi,_) :: rest | GFun ({svar = vi; _},_) :: rest when vi.vname = name ->
          if not (isFunctionType vi.vtype) then
            E.s (error ("findOrCreateFunc: can't create %s because another "
                        ^^"global exists with that name.") name);
          vi
      | _ :: rest -> search rest (* tail recursive *)
      | [] -> (*not found, so create one *)
          let t' = unrollTypeDeep t in
	  let new_decl = makeGlobalVar name t' in
	  f.globals <- GVarDecl(new_decl, locUnknown) :: f.globals;
	  new_decl
  in
  search f.globals



(* A visitor for the whole file that does not change the globals *)
let visitCilFileSameGlobals (vis : cilVisitor) (f : file) : unit =
  let fGlob g = visitCilGlobal vis g in
  iterGlobals f (fun g ->
    match fGlob g with
      [g'] when g' == g || Util.equals g' g -> () (* Try to do the pointer check first *)
    | gl ->
        ignore (E.log "You used visitCilFilSameGlobals but the global got changed:\n %a\nchanged to %a\n" d_global g (docList ~sep:line (d_global ())) gl);
        ())

(* Be careful with visiting the whole file because it might be huge. *)
let visitCilFile (vis : cilVisitor) (f : file) : unit =
  let fGlob g = visitCilGlobal vis g in
  (* Scan the globals. Make sure this is tail recursive. *)
  let rec loop (acc: global list) = function
      [] -> f.globals <- List.rev acc
    | g :: restg ->
        loop ((List.rev (fGlob g)) @ acc) restg
  in
  loop [] f.globals;
  (* the global initializer *)
  (match f.globinit with
    None -> ()
  | Some g -> f.globinit <- Some (visitCilFunction vis g))



(** Create or fetch the global initializer. Tries to put a call to the
   function with the main_name into it *)
let getGlobInit ?(main_name="main") (fl: file) =
  match fl.globinit with
    Some f -> f
  | None -> begin
      (* Sadly, we cannot use the Filename library because it does not like
         function names with multiple . in them *)
      let f =
        let len = String.length fl.fileName in
        (* Find the last path separator and record the first . that we see,
          going backwards *)
        let lastDot = ref len in
        let rec findLastPathSep i =
          if i < 0 then -1 else
          let c = String.get fl.fileName i in
          if c = '/' || c = '\\' then i
          else begin
            if c = '.' && !lastDot = len then
              lastDot := i;
            findLastPathSep (i - 1)
          end
        in
        let lastPathSep = findLastPathSep (len - 1) in
        let basenoext =
          String.sub fl.fileName (lastPathSep + 1) (!lastDot - lastPathSep - 1)
        in
        emptyFunction
          (makeValidSymbolName ("__globinit_" ^ basenoext))
      in
      fl.globinit <- Some f;
      (* Now try to add a call to the global initialized at the beginning of
         main *)
      let inserted = ref false in
      List.iter
        (function
            GFun(m, lm) when m.svar.vname = main_name ->
              (* Prepend a prototype to the global initializer *)
              fl.globals <- GVarDecl (f.svar, lm) :: fl.globals;
              m.sbody.bstmts <-
                 compactStmts (mkStmt (Instr [Call(None,
                                                   Lval(var f.svar),
                                                   [], locUnknown, locUnknown)])
                               :: m.sbody.bstmts);
              inserted := true;
              if !E.verboseFlag then
                ignore (E.log "Inserted the globinit\n");
              fl.globinitcalled <- true;
          | _ -> ())
        fl.globals;

      if not !inserted then
        ignore (E.warn "Cannot find %s to add global initializer %s"
                  main_name f.svar.vname);

      f
  end



(* Fold over all globals, including the global initializer *)
let mapGlobals (fl: file)
               (doone: global -> global) : unit =
  fl.globals <- Util.list_map doone fl.globals;
  (match fl.globinit with
    None -> ()
  | Some g -> begin
      match doone (GFun(g, locUnknown)) with
        GFun(g', _) -> fl.globinit <- Some g'
      | _ -> E.s (E.bug "mapGlobals: globinit is not a function")
  end)



let dumpFile (pp: cilPrinter) (out : out_channel) (outfile: string) file =
  printDepth := 99999;  (* We don't want ... in the output *)

  Pretty.fastMode := true;

  if !E.verboseFlag then
    ignore (E.log "printing file %s\n" outfile);
  let print x = fprint out ~width:78 x in
  print (text ("/* Generated by Goblint-CIL v. " ^ cilVersion ^ " */\n" ^
               (* sm: I want to easily tell whether the generated output
                  is with print_CIL_Input or not *)
               "/* print_CIL_Input is " ^ (if !print_CIL_Input then "true" else "false") ^ " */\n\n"));
  iterGlobals file (fun g -> dumpGlobal pp out g);

  (* sm: we have to flush the output channel; if we don't then under *)
  (* some circumstances (I haven't figure out exactly when, but it happens *)
  (* more often with big inputs), we get a truncated output file *)
  flush out



(******************
 ******************
 ******************)

(* Convert an expression into an attribute, if possible. Otherwise raise
   NotAnAttrParam *)
exception NotAnAttrParam of exp
let rec expToAttrParam (e: exp) : attrparam =
  match e with
    Const(CInt(i,k,_)) ->
      let i' = mkCilintIk k i in
      if not (is_int_cilint i') then
        raise (NotAnAttrParam e);
      AInt (int_of_cilint i')
  | Lval (Var v, NoOffset) -> ACons(v.vname, [])
  | SizeOf t -> ASizeOf t
  | SizeOfE e' -> ASizeOfE (expToAttrParam e')

  | UnOp(uo, e', _)  -> AUnOp (uo, expToAttrParam e')
  | BinOp(bo, e1',e2', _)  -> ABinOp (bo, expToAttrParam e1',
                                      expToAttrParam e2')
  | _ -> raise (NotAnAttrParam e)

(******************** OPTIMIZATIONS *****)
let rec peepHole1 (* Process one instruction and possibly replace it *)
                  (doone: instr -> instr list option)
                  (* Scan a block and recurse inside nested blocks *)
                  (ss: stmt list) : unit =
  let rec doInstrList (il: instr list) : instr list =
    match il with
      [] -> []
    | i :: rest -> begin
        match doone i with
          None -> i :: doInstrList rest
        | Some sl -> doInstrList (sl @ rest)
    end
  in

  List.iter
    (fun s ->
      match s.skind with
        Instr il -> s.skind <- Instr (doInstrList il)
      | If (e, tb, eb, _, _) ->
          peepHole1 doone tb.bstmts;
          peepHole1 doone eb.bstmts
      | Switch (e, b, _, _, _) -> peepHole1 doone b.bstmts
      | Loop (b, l, el, _, _) -> peepHole1 doone b.bstmts
      | Block b -> peepHole1 doone b.bstmts
      | Return _ | Goto _ | ComputedGoto _ | Break _ | Continue _ -> ())
    ss

let rec peepHole2  (* Process two instructions and possibly replace them both *)
                   (dotwo: instr * instr -> instr list option)
                   (ss: stmt list) : unit =
  let rec doInstrList (il: instr list) : instr list =
    match il with
      [] -> []
    | [i] -> [i]
    | (i1 :: ((i2 :: rest) as rest2)) ->
        begin
          match dotwo (i1,i2) with
            None -> i1 :: doInstrList rest2
          | Some sl -> doInstrList (sl @ rest)
        end
  in
  List.iter
    (fun s ->
      match s.skind with
        Instr il -> s.skind <- Instr (doInstrList il)
      | If (e, tb, eb, _, _) ->
          peepHole2 dotwo tb.bstmts;
          peepHole2 dotwo eb.bstmts
      | Switch (e, b, _, _, _) -> peepHole2 dotwo b.bstmts
      | Loop (b, l, el, _, _) -> peepHole2 dotwo b.bstmts
      | Block b -> peepHole2 dotwo b.bstmts

      | Return _ | Goto _ | ComputedGoto _ | Break _ | Continue _ -> ())
    ss




(*** Type signatures ***)

(* Helper class for typeSig: replace any types in attributes with typsigs *)
class typeSigVisitor(typeSigConverter: typ->typsig) = object
  inherit nopCilVisitor
  method! vattrparam ap =
    match ap with
      | ASizeOf t -> ChangeTo (ASizeOfS (typeSigConverter t))
      | AAlignOf t -> ChangeTo (AAlignOfS (typeSigConverter t))
      | _ -> DoChildren
end

let typeSigAddAttrs a0 t =
  if a0 == [] then t else
  match t with
    TSBase t -> TSBase (typeAddAttributes a0 t)
  | TSPtr (ts, a) -> TSPtr (ts, addAttributes a0 a)
  | TSArray (ts, l, a) -> TSArray(ts, l, addAttributes a0 a)
  | TSComp (iss, n, a) -> TSComp (iss, n, addAttributes a0 a)
  | TSEnum (n, a) -> TSEnum (n, addAttributes a0 a)
  | TSFun(ts, tsargs, isva, a) -> TSFun(ts, tsargs, isva, addAttributes a0 a)

(* Compute a type signature.
    Use ~ignoreSign:true to convert all signed integer types to unsigned,
    so that signed and unsigned will compare the same. *)
let rec typeSigWithAttrs ?(ignoreSign=false) doattr t =
  let typeSig = typeSigWithAttrs ~ignoreSign doattr in
  let attrVisitor = new typeSigVisitor typeSig in
  let doattr al = visitCilAttributes attrVisitor (doattr al) in
  match t with
  | TInt (ik, al) ->
      let ik' =
        if ignoreSign then unsignedVersionOf ik  else ik
      in
      TSBase (TInt (ik', doattr al))
  | TFloat (fk, al) -> TSBase (TFloat (fk, doattr al))
  | TVoid al -> TSBase (TVoid (doattr al))
  | TEnum (enum, a) -> TSEnum (enum.ename, doattr a)
  | TPtr (t, a) -> TSPtr (typeSig t, doattr a)
  | TArray (t,l,a) -> (* We do not want fancy expressions in array lengths.
                         So constant fold the lengths *)
      let l' =
        match l with
          Some l -> begin
            match constFold true l with
              Const(CInt(i, _, _)) -> Some i
            | e -> None (* Returning None for length in a typesig if the length is not a constant (VLA) *)
          end
        | None -> None
      in
      TSArray(typeSig t, l', doattr a)

  | TComp (comp, a) ->
      TSComp (comp.cstruct, comp.cname, doattr (addAttributes comp.cattr a))
  | TFun(rt,args,isva,a) ->
      TSFun(typeSig rt, (Util.list_map_opt (fun (_, atype, _) -> (typeSig atype)) args), isva, doattr a)
  | TNamed(t, a) -> typeSigAddAttrs (doattr a) (typeSig t.ttype)
  | TBuiltin_va_list al -> TSBase (TBuiltin_va_list (doattr al))

let typeSig t =
  typeSigWithAttrs (fun al -> al) t

let _ = pTypeSig := typeSig

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



let dExp: doc -> exp =
  fun d -> Const(CStr(sprint ~width:!lineLength d, No_encoding))

let dInstr: doc -> location -> instr =
  fun d l -> Asm([], [sprint ~width:!lineLength d], [], [], [], [], l)

let dGlobal: doc -> location -> global =
  fun d l -> GAsm(sprint ~width:!lineLength d, l)

  (* Make an AddrOf. Given an lval of type T will give back an expression of
     type ptr(T)  *)
let mkAddrOf ((b, off) as lval) : exp =
  (* Never take the address of a register variable *)
  (match lval with
    Var vi, off when vi.vstorage = Register -> vi.vstorage <- NoStorage
  | _ -> ());
  match lval with
    Mem e, NoOffset -> e
  (* Don't do this:
    | b, Index(z, NoOffset) when isZero z -> StartOf (b, NoOffset)
    &a[0] is not the same as a, e.g. within typeof and sizeof.
    Code must be able to handle the results without this anyway... *)
  | _ -> AddrOf lval


let mkAddrOrStartOf (lv: lval) : exp =
  match unrollType (typeOfLval lv) with
    TArray _ -> StartOf lv
  | _ -> mkAddrOf lv


  (* Make a Mem, while optimizing AddrOf. The type of the addr must be
     TPtr(t) and the type of the resulting lval is t. Note that in CIL the
     implicit conversion between a function and a pointer to a function does
     not apply. You must do the conversion yourself using AddrOf *)
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



let splitFunctionType (ftype: typ)
    : typ * (string * typ * attributes) list option * bool * attributes =
  match unrollType ftype with
    TFun (rt, args, isva, a) -> rt, args, isva, a
  | _ -> E.s (bug "splitFunctionType invoked on a non function type %a"
                d_type ftype)

let splitFunctionTypeVI (fvi: varinfo)
    : typ * (string * typ * attributes) list option * bool * attributes =
  match unrollType fvi.vtype with
    TFun (rt, args, isva, a) -> rt, args, isva, a
  | _ -> E.s (bug "Function %s invoked on a non function type" fvi.vname)


let getCompField (cinfo:compinfo) (fieldName:string) : fieldinfo =
  (List.find (fun fi -> fi.fname = fieldName) cinfo.cfields)


let mkCastT ~(e: exp) ~(oldt: typ) ~(newt: typ) =
  (* Do not remove old casts because they are conversions !!! *)
  if Util.equals (typeSig oldt) (typeSig newt) then begin
    e
  end else begin
    (* Watch out for constants *)
    match newt, e with
      (* Casts to _Bool are special: they behave like "!= 0" ISO C99 6.3.1.2 *)
      TInt(IBool, []), Const(CInt(i, _, _)) ->
        let v = if compare i zero_cilint = 0 then zero_cilint else one_cilint in
        Const (CInt(v, IBool,  None))
    | TInt(newik, []), Const(CInt(i, _, _)) -> kintegerCilint newik i
    | _ -> CastE(newt,e)
  end

let mkCast ~(e: exp) ~(newt: typ) =
  mkCastT ~e:e ~oldt:(typeOf e) ~newt:newt

type existsAction =
    ExistsTrue                          (* We have found it *)
  | ExistsFalse                         (* Stop processing this branch *)
  | ExistsMaybe                         (* This node is not what we are
                                           looking for but maybe its
                                           successors are *)
let existsType (f: typ -> existsAction) (t: typ) : bool =
  let memo : (int, unit) H.t = H.create 17 in  (* Memo table *)
  let rec loop t =
    match f t with
      ExistsTrue -> true
    | ExistsFalse -> false
    | ExistsMaybe ->
        (match t with
          TNamed (t', _) -> loop t'.ttype
        | TComp (c, _) -> loopComp c
        | TArray (t', _, _) -> loop t'
        | TPtr (t', _) -> loop t'
        | TFun (rt, args, _, _) ->
            (loop rt || List.exists (fun (_, at, _) -> loop at)
              (argsToList args))
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


(* Try to do an increment, with constant folding *)
let increm (e: exp) (i: int) =
  let et = typeOf e in
  let bop = if isPointerType et then PlusPI else PlusA in
  constFold false (BinOp(bop, e, integer i, et))

exception LenOfArray
let lenOfArray (eo: exp option) : int =
  match eo with
    None -> raise LenOfArray
  | Some e -> begin
      match constFold true e with
      | Const(CInt(ni, _, _)) when compare_cilint ni zero_cilint >= 0 ->
          cilint_to_int ni
      | e -> raise LenOfArray
  end


(*** Make an initializer for zeroe-ing a data type ***)
let rec makeZeroInit (t: typ) : init =
  match unrollType t with
    TInt (ik, _) -> SingleInit (Const(CInt(zero_cilint, ik, None)))
  | TFloat(fk, _) -> SingleInit(Const(CReal(0.0, fk, None)))
  | TEnum (e, _) -> SingleInit (kinteger e.ekind 0)
  | TComp (comp, _) as t' when comp.cstruct ->
      let inits =
        List.fold_right
          (fun f acc ->
            if f.fname <> missingFieldName then
              (Field(f, NoOffset), makeZeroInit f.ftype) :: acc
            else
              acc)
          comp.cfields []
      in
      CompoundInit (t', inits)

  | TComp (comp, _) when not comp.cstruct ->
      let fstfield, rest =
        match comp.cfields with
          f :: rest -> f, rest
        | [] -> E.s (unimp "Cannot create init for empty union")
      in
      let fieldToInit =
        (* gcc initializes the whole union to zero.  So choose the largest
           field, and set that to zero.  Choose the first field if possible. *)
        let fieldSize f = try bitsSizeOf f.ftype with SizeOfError _ -> 0 in
        let widestField, widestFieldWidth =
          List.fold_left (fun acc thisField ->
                            let widestField, widestFieldWidth = acc in
                            let thisSize = fieldSize thisField in
                            if thisSize > widestFieldWidth then
                              thisField, thisSize
                            else
                              acc)
            (fstfield, fieldSize fstfield)
            rest
        in
        widestField
      in
      CompoundInit(t, [(Field(fieldToInit, NoOffset),
                        makeZeroInit fieldToInit.ftype)])

  | TArray(bt, Some len, _) as t' ->
      let n =
        match constFold true len with
          Const(CInt(n, _, _)) -> cilint_to_int n
        | _ -> E.s (E.unimp "Cannot understand length of array")
      in
      let initbt = makeZeroInit bt in
      let rec loopElems acc i =
        if i < 0 then acc
        else loopElems ((Index(integer i, NoOffset), initbt) :: acc) (i - 1)
      in
      CompoundInit(t', loopElems [] (n - 1))

  | TArray (bt, None, at) as t' ->
      (* Unsized array, allow it and fill it in later
         (see cabs2cil.ml, collectInitializer) *)
      CompoundInit (t', [])

  | TPtr _ as t ->
      SingleInit(if !insertImplicitCasts then mkCast ~e:zero ~newt:t else zero)
  | x -> E.s (unimp "Cannot initialize type: %a" d_type x)


(** Fold over the list of initializers in a Compound (not also the nested
   ones). [doinit] is called on every present initializer, even if it is of
   compound type. The parameters of [doinit] are: the offset in the compound
   (this is [Field(f,NoOffset)] or [Index(i,NoOffset)]), the initializer
   value, expected type of the initializer value, accumulator. In the case of
   arrays there might be missing zero-initializers at the end of the list.
   These are scanned only if [implicit] is true. This is much like
   [List.fold_left] except we also pass the type of the initializer. *)
let foldLeftCompound
    ~(implicit: bool)
    ~(doinit: offset -> init -> typ -> 'a -> 'a)
    ~(ct: typ)
    ~(initl: (offset * init) list)
    ~(acc: 'a) =
  match unrollType ct with
    TArray(bt, leno, _) -> begin
      (* Scan the existing initializer *)
      let part =
        List.fold_left (fun acc (o, i) -> doinit o i bt acc) acc initl in
      (* See how many more we have to do *)
      match leno with
        Some lene when implicit -> begin
          match constFold true lene with
            Const(CInt(i, _, _)) ->
              let len_array = cilint_to_int i in
              let len_init = List.length initl in
              if len_array > len_init then
                let zi = makeZeroInit bt in
                let rec loop acc i =
                  if i >= len_array then acc
                  else
                    loop (doinit (Index(integer i, NoOffset)) zi bt acc)
                         (i + 1)
                in
                loop part (len_init + 1)
              else
                part
          | _ -> E.s (unimp "foldLeftCompound: array with initializer and non-constant length\n")
        end

      | _ when not implicit -> part

      | _ -> E.s (unimp "foldLeftCompound: TArray with initializer and no length")
    end

  | TComp (comp, _) ->
      let getTypeOffset = function
          Field(f, NoOffset) -> f.ftype
        | _ -> E.s (bug "foldLeftCompound: malformed initializer")
      in
      List.fold_left
        (fun acc (o, i) -> doinit o i (getTypeOffset o) acc) acc initl

  | _ -> E.s (E.unimp "Type of Compound is not array or struct or union")




let rec isCompleteType t =
  match unrollType t with
  | TArray(t, None, _) -> false
  | TArray(t, Some z, _) when isZero z -> false
  | TComp (comp, _) -> (* Struct or union *)
      List.for_all (fun fi -> isCompleteType fi.ftype) comp.cfields
  | _ -> true


module A = Alpha


(** Uniquefy the variable names *)
let uniqueVarNames (f: file) : unit =
  (* Setup the alpha conversion table for globals *)
  let gAlphaTable: (string,
                    location A.alphaTableData ref) H.t = H.create 113 in
  (* Keep also track of the global names that we have used. Map them to the
     variable ID. We do this only to check that we do not have two globals
     with the same name. *)
  let globalNames: (string, int) H.t = H.create 113 in
  (* Scan the file and add the global names to the table *)
  iterGlobals f
    (function
        GVarDecl(vi, l)
      | GVar(vi, _, l)
      | GFun({svar = vi; _}, l) ->
          (* See if we have used this name already for something else *)
          (try
            let oldid = H.find globalNames vi.vname in
            if oldid <> vi.vid then
              ignore (warn "The name %s is used for two distinct globals"
                        vi.vname);
            (* Here if we have used this name already. Go ahead *)
            ()
          with Not_found -> begin
            (* Here if this is the first time we define a name *)
            H.add globalNames vi.vname vi.vid;
            (* And register it *)
            A.registerAlphaName ~alphaTable:gAlphaTable ~undolist:None ~lookupname:vi.vname ~data:!currentLoc;
            ()
          end)
      | _ -> ());

  (* Now we must scan the function bodies and rename the locals *)
  iterGlobals f
    (function
        GFun(fdec, l) -> begin
          currentLoc := l;
          (* Setup an undo list to be able to revert the changes to the
             global alpha table *)
          let undolist = ref [] in
          (* Process one local variable *)
          let processLocal (v: varinfo) =
            let newname, oldloc =
              A.newAlphaName ~alphaTable:gAlphaTable ~undolist:(Some undolist) ~lookupname:v.vname
               ~data:!currentLoc
            in
            if false && newname <> v.vname then (* Disable this warning *)
              ignore (warn "uniqueVarNames: Changing the name of local %s in %s to %s (due to duplicate at %a)"
                        v.vname fdec.svar.vname newname d_loc oldloc);
            v.vname <- newname
          in
          (* Do the formals first *)
          List.iter processLocal fdec.sformals;
          (* Fix the type again *)
          setFormals fdec fdec.sformals;
          (* And now the locals *)
          List.iter processLocal fdec.slocals;
          (* Undo the changes to the global table *)
          A.undoAlphaChanges ~alphaTable:gAlphaTable ~undolist:!undolist;
          ()
        end
      | _ -> ());
  ()


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
  method! vfunc (f: fundec) : fundec visitAction =
    (* We need a map from the old locals/formals to the new ones *)
    H.clear map;
    argid := 0;
     (* Make a copy of the fundec. *)
    let f' = {f with svar = f.svar} in
    let patchfunction (f' : fundec) =
      (* Change the name. Only this late to allow the visitor to copy the
         svar  *)
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
        | Switch (e, body, cases, l, el) ->
            s.skind <- Switch (e, body,
                               Util.list_map (fun cs -> findStmt cs.sid) cases, l, el)
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
         maintain sharing *)
  method! vvdec (v: varinfo) =
    (* Some varinfo have empty names. Give them some name *)
    if v.vname = "" then begin
      v.vname <- "arg" ^ string_of_int !argid; incr argid
    end;
    try
      ChangeTo (H.find map v.vname)
    with Not_found -> begin
      let v' = {v with vid = newVID () } in
      H.add map v.vname v';
      ChangeDoChildrenPost (v', fun x -> x)
    end

      (* We must replace references to local variables *)
  method! vvrbl (v: varinfo) =
    if v.vglob then SkipChildren else
    try
      ChangeTo (H.find map v.vname)
    with Not_found ->
      E.s (bug "Cannot find the new copy of local variable %s" v.vname)


        (* Replace statements. *)
  method! vstmt (s: stmt) : stmt visitAction =
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
  method! vblock (b: block) =
    ChangeDoChildrenPost ({b with bstmts = b.bstmts}, fun x -> x)


  method! vglob _ = E.s (bug "copyFunction should not be used on globals")
end

(* We need a function that copies a CIL function. *)
let copyFunction (f: fundec) (newname: string) : fundec =
  visitCilFunction (new copyFunctionVisitor(newname)) f

(********* Compute the CFG ********)
let sid_counter = ref 0

let new_sid () =
  let id = !sid_counter in
  incr sid_counter;
  id

let statements : stmt list ref = ref []
(* Clear all info about the CFG in statements *)
class clear : cilVisitor = object
  inherit nopCilVisitor
  method! vstmt s = begin
    s.sid <- !sid_counter ;
    incr sid_counter ;
    statements := s :: !statements;
    s.succs <- [] ;
    s.preds <- [] ;
    DoChildren
  end
  method! vexpr _ = SkipChildren
  method! vtype _ = SkipChildren
  method! vinst _ = SkipChildren
end

let link source dest = begin
  if not (List.mem dest source.succs) then
    source.succs <- dest :: source.succs ;
  if not (List.mem source dest.preds) then
    dest.preds <- source :: dest.preds
end
let trylink source dest_option = match dest_option with
  None -> ()
| Some(dest) -> link source dest


(** Compute the successors and predecessors of a block, given a fallthrough *)
let rec succpred_block b fallthrough rlabels =
  let rec handle sl = match sl with
    [] -> ()
  | [a] -> succpred_stmt a fallthrough rlabels
  | hd :: ((next :: _) as tl) ->
      succpred_stmt hd (Some next) rlabels;
      handle tl
  in handle b.bstmts


and succpred_stmt s fallthrough rlabels =
  s.fallthrough <- fallthrough;
  match s.skind with
    Instr _ -> trylink s fallthrough
  | Return _ -> ()
  | Goto(dest,l) -> link s !dest
  | ComputedGoto(e,l) ->  List.iter (link s) rlabels
  | Break _
  | Continue _
  | Switch _ ->
    failwith "computeCFGInfo: cannot be called on functions with break, continue or switch statements. Use prepareCFG first to remove them."

  | If(e1,b1,b2,l,el) ->
      (match b1.bstmts with
        [] -> trylink s fallthrough
      | hd :: tl -> (link s hd ; succpred_block b1 fallthrough rlabels )) ;
      (match b2.bstmts with
        [] -> trylink s fallthrough
      | hd :: tl -> (link s hd ; succpred_block b2 fallthrough rlabels ))

  | Loop(b,l,el,_,_) ->
      begin match b.bstmts with
        [] -> failwith "computeCFGInfo: empty loop"
      | hd :: tl ->
          link s hd ;
          succpred_block b (Some(hd)) rlabels
      end

  | Block(b) -> begin match b.bstmts with
                  [] -> trylink s fallthrough
                | hd :: tl -> link s hd ;
                    succpred_block b fallthrough rlabels
                end


let caseRangeFold (l: label list) =
  let rec fold acc = function
  | ((Case _ | Default _ | Label _) as x) :: xs -> fold (x :: acc) xs
  | CaseRange(el, eh, loc, eloc) :: xs ->
      let il, ih, ik =
        match constFold true el, constFold true eh with
          Const(CInt(il, ilk, _)), Const(CInt(ih, ihk, _)) ->
            mkCilintIk ilk il, mkCilintIk ihk ih, commonIntKind ilk ihk
        | _ -> E.s (error "Cannot understand the constants in case range (%a and %a)" d_exp el d_exp eh)
      in
      if compare_cilint il ih > 0 then
        E.s (error "Empty case range");
      let rec mkAll (i: cilint) acc =
        if compare_cilint i ih > 0 then acc
        else mkAll (add_cilint i one_cilint) (Case(kintegerCilint ik i, loc, eloc) :: acc)
      in
      fold (mkAll il acc) xs
   | [] -> List.rev acc
   in fold [] l

(* [weimer] Sun May  5 12:25:24 PDT 2002
   This code was pulled from ext/switch.ml because it looks like we really
   want it to be part of CIL.

   Here is the magic handling to
    (1) replace switch statements with if/goto
    (2) remove "break"
    (3) remove "default"
    (4) remove "continue"
 *)

(* This alphaTable is used to prevent collision of label names when
   transforming switch statements and loops. It uses a *unit*
   alphaTableData ref because there isn't any information we need to
   carry around. *)
let labelAlphaTable : (string, unit A.alphaTableData ref) H.t =
  H.create 11

(* Compute a fresh label name, call populateLabelAlphaTable with the appropriate fd before *)
let freshLabel (base:string) =
  fst (A.newAlphaName ~alphaTable:labelAlphaTable ~undolist:None  ~lookupname:base ~data:())

let rec xform_switch_stmt s break_dest cont_dest = begin
  let suffix e = match getInteger e with
  | Some value ->
      if compare_cilint value zero_cilint < 0 then
        "neg_" ^ string_of_cilint (neg_cilint value)
        else
          string_of_cilint value
  | None -> "exp"
  in
  s.labels <- Util.list_map (fun lab -> match lab with
    Label _ -> lab
  | Case(e,l,el) ->
      let str = Printf.sprintf "case_%s" (suffix e) in
      Label(freshLabel str,l,false)
  | CaseRange(e1,e2,l,el) ->
      let str = Printf.sprintf "caserange_%s_%s" (suffix e1) (suffix e2) in
      Label(freshLabel str,l,false)
  | Default(l,el) -> Label(freshLabel "switch_default",l,false)
  ) s.labels ;
  match s.skind with
  | Instr _ | Return _ | Goto _ | ComputedGoto _ -> ()
  | Break(l) -> begin try
                  s.skind <- Goto(break_dest (),l)
                with e ->
                  ignore (error "prepareCFG: break: %a@!" d_stmt s) ;
                  raise e
                end
  | Continue(l) -> begin try
                  s.skind <- Goto(cont_dest (),l)
                with e ->
                  ignore (error "prepareCFG: continue: %a@!" d_stmt s) ;
                  raise e
                end
  | If(e,b1,b2,l,el) -> xform_switch_block b1 break_dest cont_dest ;
                        xform_switch_block b2 break_dest cont_dest
  | Switch(e,b,sl,l,el) ->
      (* change
         switch (se) {
           case 0: s0 ;
           case 1: s1 ; break;
           ...
         }

         into:

         if (se == 0) goto label_0;
         if (se == 1) goto label_1;
         ...
         goto label_default; // If there is a [Default]
         goto label_break; // If there is no [Default]
         label_0: s0;
         label_1: s1; goto label_break;
         ...
         label_break: ; // break_stmt

         The default case, if present, must be used only if *all*
         non-default cases fail [ISO/IEC 9899:1999, �6.8.4.2, �5]. As
         a result, we test all cases first, and hit 'default' only if
         no case matches. However, we do not reorder the switch's
         body, so fall-through still works as expected.

       *)

      let break_stmt = mkStmt (Instr []) in
      break_stmt.labels <- [Label(freshLabel "switch_break",l,false)] ;

      (* To be changed into goto default if there if a [Default] *)
      let goto_break = mkStmt (Goto (ref break_stmt, l)) in

      (* Return a list of [If] statements, equivalent to the cases of [stmt].
         Use a single [If] and || operators if useLogicalOperators is true.
         If [stmt] is a [Default], update goto label_break into goto
         label_default.
       *)
      let xform_choice stmt =
        let cases = List.filter (function Label _ -> false | _ -> true ) stmt.labels in
        try (* is this the default case? *)
          match List.find (function Default _ -> true | _ -> false) cases with
          | Default (dl, del) ->
              (* We found a [Default], update the fallthrough goto *)
              goto_break.skind <- Goto(ref stmt, dl);
              []
          | _ -> E.s (bug "Unexpected pattern-matching failure")
        with
        Not_found -> (* this is a list of specific cases *)
          match cases with
          | ((Case (_, cl, cel) | CaseRange (_, _, cl, cel)) as lab) :: lab_tl ->
            (* assume that integer promotion and type conversion of cases is
               performed by cabs2cil. *)
            let comp_case_range e1 e2 =
                  BinOp(Ge, e, e1, intType), BinOp(Le, e, e2, intType) in
            let make_comp lab = begin match lab with
              | Case (exp, _, _) -> BinOp(Eq, e, exp, intType)
              | CaseRange (e1, e2, _, _) when !useLogicalOperators ->
                  let c1, c2 = comp_case_range e1 e2 in
                  BinOp(LAnd, c1, c2, intType)
              | _ -> E.s (bug "Unexpected pattern-matching failure")
            end in
            let make_or_from_cases () =
              List.fold_left
                  (fun pred label -> BinOp(LOr, pred, make_comp label, intType))
                  (make_comp lab) lab_tl
            in
            let make_if_stmt pred cl cel =
              let then_block = mkBlock [ mkStmt (Goto(ref stmt,cl)) ] in
              let else_block = mkBlock [] in
              mkStmt(If(pred,then_block,else_block,cl,cel)) in
            let make_double_if_stmt (pred1, pred2) cl cel =
              let then_block = mkBlock [ make_if_stmt pred2 cl cel ] in
              let else_block = mkBlock [] in
              mkStmt(If(pred1,then_block,else_block,cl,cel)) in
            if !useLogicalOperators then
              [make_if_stmt (make_or_from_cases ()) cl cel]
            else
              List.map (function
                | Case _ as lab -> make_if_stmt (make_comp lab) cl cel
                | CaseRange (e1, e2, _, _) -> make_double_if_stmt (comp_case_range e1 e2) cl cel
                | _ -> E.s (bug "Unexpected pattern-matching failure"))
                cases
          | Default _ :: _ | Label _ :: _ ->
              E.s (bug "Unexpected pattern-matching failure")
          | [] -> E.s (bug "Block missing 'case' and 'default' in switch statement")
      in
      b.bstmts <-
        (List.flatten (List.map xform_choice sl)) @
        [goto_break] @
        b.bstmts @
        [break_stmt];
      s.skind <- Block b;
      xform_switch_block b (fun () -> ref break_stmt) cont_dest
  | Loop(b,l,el,_,_) ->
          let break_stmt = mkStmt (Instr []) in
          break_stmt.labels <- [Label(freshLabel "while_break",l,false)] ;
          let cont_stmt = mkStmt (Instr []) in
          cont_stmt.labels <- [Label(freshLabel "while_continue",l,false)] ;
          b.bstmts <- cont_stmt :: b.bstmts ;
          let this_stmt = mkStmt
            (Loop(b,l,el,Some(cont_stmt),Some(break_stmt))) in
          let break_dest () = ref break_stmt in
          let cont_dest () = ref cont_stmt in
          xform_switch_block b break_dest cont_dest ;
          break_stmt.succs <- s.succs ;
          let new_block = mkBlock [ this_stmt ; break_stmt ] in
          s.skind <- Block new_block
  | Block(b) -> xform_switch_block b break_dest cont_dest


end and xform_switch_block b break_dest cont_dest =
  try
    let rec link_succs sl = match sl with
    | [] -> ()
    | hd :: tl -> (if hd.succs = [] then hd.succs <- tl) ; link_succs tl
    in
    link_succs b.bstmts ;
    List.iter (fun stmt ->
      xform_switch_stmt stmt break_dest cont_dest) b.bstmts ;
  with e ->
    List.iter (fun stmt -> ignore
      (warn "prepareCFG: %a@!" d_stmt stmt)) b.bstmts ;
    raise e

(* Enter all the labels in a function into an alpha renaming table to
   prevent duplicate labels when transforming loops and switch
   statements. *)
class registerLabelsVisitor : cilVisitor = object
  inherit nopCilVisitor
  method! vstmt { labels = labels; _ } = begin
    List.iter
      (function
           Label (name,_,_) -> A.registerAlphaName ~alphaTable:labelAlphaTable ~undolist:None ~lookupname:name ~data:()
         | _ -> ())
      labels;
    DoChildren
  end
  method! vexpr _ = SkipChildren
  method! vtype _ = SkipChildren
  method! vinst _ = SkipChildren
end

(* Find all labels-as-value in a function to use them as successors of computed
   gotos. Duplicated in src/ext/cfg.ml. *)
class addrOfLabelFinder slr = object(self)
    inherit nopCilVisitor

    method! vexpr e = match e with
    | AddrOfLabel sref ->
        slr := !sref :: (!slr);
        SkipChildren
    | _ -> DoChildren

end

let findAddrOfLabelStmts (b : block) : stmt list =
    let slr = ref [] in
    let vis = new addrOfLabelFinder slr in
    ignore(visitCilBlock vis b);
    !slr

(* Clears the labelAlphaTable and populates it with all label names appearing in fd *)
let populateLabelAlphaTable (fd: fundec): unit =
  H.clear labelAlphaTable;
  ignore (visitCilFunction (new registerLabelsVisitor) fd)

(* prepare a function for computeCFGInfo by removing break, continue,
   default and switch statements/labels and replacing them with Ifs and
   Gotos. *)
let prepareCFG (fd : fundec) : unit =
  (* Labels are local to a function, so start with a clean slate by
     clearing labelAlphaTable. Then register all labels. *)
  populateLabelAlphaTable fd;
  xform_switch_block fd.sbody
      (fun () -> failwith "prepareCFG: break with no enclosing loop")
      (fun () -> failwith "prepareCFG: continue with no enclosing loop")

(* make the cfg and return a list of statements *)
let computeCFGInfo (f : fundec) (global_numbering : bool) : unit =
  if not global_numbering then
    sid_counter := 0 ;
  statements := [];
  let clear_it = new clear in
  ignore (visitCilBlock clear_it f.sbody) ;
  f.smaxstmtid <- Some (!sid_counter) ;
  let rlabels = findAddrOfLabelStmts f.sbody in
  succpred_block f.sbody None rlabels;
  let res = List.rev !statements in
  statements := [];
  f.sallstmts <- res;
  ()

let initCIL () =
  if not !initCIL_called then begin
    (* Set the machine *)
    begin
      match !envMachine with
        Some machine -> M.theMachine := machine
      | None -> M.theMachine := M.gcc
    end;
    (* Find the right ikind given the size *)
    let findIkindSz (unsigned: bool) (sz: int) : ikind =
      try
	intKindForSize sz unsigned
      with Not_found ->
        E.s(E.unimp "initCIL: cannot find the right ikind for size %d\n" sz)
    in
    (* Find the right ikind given the name *)
    let findIkindName (name: string) : ikind =
      (* Test the most common sizes first *)
      if name = "int" then IInt
      else if name = "unsigned int" then IUInt
      else if name = "long" then ILong
      else if name = "unsigned long" then IULong
      else if name = "long long" then ILongLong
      else if name = "unsigned long long" then IULongLong
      else if name = "short" then IShort
      else if name = "unsigned short" then IUShort
      else if name = "char" then IChar
      else if name = "unsigned char" then IUChar
      else if name = "__int128" then IInt128
      else if name = "unsigned __int128" then IUInt128
      else E.s(E.unimp "initCIL: cannot find the right ikind for type %s\n" name)
    in
    upointType := TInt(findIkindSz true !M.theMachine.M.sizeof_ptr, []);
    ptrdiffType := TInt(findIkindSz false !M.theMachine.M.sizeof_ptr, []);
    kindOfSizeOf := findIkindName !M.theMachine.M.size_t;
    typeOfSizeOf := TInt(!kindOfSizeOf, []);
    wcharKind := findIkindName !M.theMachine.M.wchar_t;
    wcharType := TInt(!wcharKind, []);
    char_is_unsigned := !M.theMachine.M.char_is_unsigned;
    little_endian := !M.theMachine.M.little_endian;
    underscore_name := !M.theMachine.M.underscore_name;
(*     nextGlobalVID := 1; *)
(*     nextCompinfoKey := 1; *)

    initCIL_called := true;
    initGccBuiltins ()
  end


(* We want to bring all type declarations before the data declarations. This
   is needed for code of the following form:

   int f(); // Prototype without arguments
   typedef int FOO;
   int f(FOO x) { ... }

   In CIL the prototype also lists the type of the argument as being FOO,
   which is undefined.

   There is one catch with this scheme. If the type contains an array whose
   length refers to variables then those variables must be declared before
   the type *)

let pullTypesForward = true


    (* Scan a type and collect the variables that are referred *)
class getVarsInGlobalClass (pacc: varinfo list ref) = object
  inherit nopCilVisitor
  method! vvrbl (vi: varinfo) =
    pacc := vi :: !pacc;
    SkipChildren

  method! vglob = function
      GType _ | GCompTag _ -> DoChildren
    | _ -> SkipChildren

end

let getVarsInGlobal (g : global) : varinfo list =
  let pacc : varinfo list ref = ref [] in
  let v : cilVisitor = new getVarsInGlobalClass pacc in
  ignore (visitCilGlobal v g);
  !pacc

let pushGlobal (g: global)
               ~(types:global list ref)
               ~(variables: global list ref) =
  if not pullTypesForward then
    variables := g :: !variables
  else
    begin
      (* Collect a list of variables that are referred from the type. Return
         Some if the global should go with the types and None if it should go
         to the variables. *)
      let varsintype : (varinfo list * location) option =
        match g with
          GType (_, l) | GCompTag (_, l) -> Some (getVarsInGlobal g, l)
        | GEnumTag (_, l) | GPragma (Attr("pack", _), l)
        | GCompTagDecl (_, l) | GEnumTagDecl (_, l) -> Some ([], l)
          (* Move the warning pragmas early
        | GPragma(Attr(s, _), l) when hasPrefix "warning" s -> Some ([], l)
          *)
        | _ -> None (* Does not go with the types *)
      in
      match varsintype with
      None -> variables := g :: !variables
    | Some (vl, loc) ->
        types :=
           (* insert declarations for referred variables ('vl'), before
              the type definition 'g' itself *)
           g :: (List.fold_left (fun acc v -> GVarDecl(v, loc) :: acc)
                                !types vl)
  end


type formatArg =
    Fe of exp
  | Feo of exp option  (** For array lengths *)
  | Fu of unop
  | Fb of binop
  | Fk of ikind
  | FE of exp list (** For arguments in a function call *)
  | Ff of (string * typ * attributes) (** For a formal argument *)
  | FF of (string * typ * attributes) list (* For formal argument lists *)
  | Fva of bool (** For the ellipsis in a function type *)
  | Fv of varinfo
  | Fl of lval
  | Flo of lval option (** For the result of a function call *)
  | Fo of offset
  | Fc of compinfo
  | Fi of instr
  | FI of instr list
  | Ft of typ
  | Fd of int
  | Fg of string
  | Fs of stmt
  | FS of stmt list
  | FA of attributes

  | Fp of attrparam
  | FP of attrparam list

  | FX of string

let d_formatarg () = function
    Fe e -> dprintf "Fe(%a)" d_exp e
  | Feo None -> dprintf "Feo(None)"
  | Feo (Some e) -> dprintf "Feo(%a)" d_exp e
  | FE _ -> dprintf "FE()"
  | Fk ik -> dprintf "Fk()"
  | Fva b -> dprintf "Fva(%b)" b
  | Ff (an, _, _) -> dprintf "Ff(%s)" an
  | FF _ -> dprintf "FF(...)"
  | FA _ -> dprintf "FA(...)"
  | Fu uo -> dprintf "Fu()"
  | Fb bo -> dprintf "Fb()"
  | Fv v -> dprintf "Fv(%s)" v.vname
  | Fl l -> dprintf "Fl(%a)" d_lval l
  | Flo None -> dprintf "Flo(None)"
  | Flo (Some l) -> dprintf "Flo(%a)" d_lval l
  | Fo o -> dprintf "Fo"
  | Fc ci -> dprintf "Fc(%s)" ci.cname
  | Fi i -> dprintf "Fi(...)"
  | FI i -> dprintf "FI(...)"
  | Ft t -> dprintf "Ft(%a)" d_type t
  | Fd n -> dprintf "Fd(%d)" n
  | Fg s -> dprintf "Fg(%s)" s
  | Fp _ -> dprintf "Fp(...)"
  | FP n -> dprintf "FP(...)"
  | Fs _ -> dprintf "FS"
  | FS _ -> dprintf "FS"

  | FX _ -> dprintf "FX()"

(* ------------------------------------------------------------------------- *)
(*                            DEPRECATED FUNCTIONS                           *)
(*                        These will eventually go away                      *)
(* ------------------------------------------------------------------------- *)

(** Deprecated.  For compatibility with older programs, these are
  aliases for {!builtinFunctions} *)
let gccBuiltins = builtinFunctions
