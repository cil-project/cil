(* cabs -- abstract syntax for FrontC                                 
**
** Project:	frontc
** File:	cabs.ml
** Version:	2.1
** Date:	4.7.99
** Author:	Hugues Cassé
**
** 1.0	2.19.99	Hugues Cassé	First version.
** 2.0	3.22.99	Hugues Cassé	Generalization of typed names.
** 2.1	4.7.99	Hugues Cassé	GNU Statement embedded in expressions managed.
*)

let version = "Cabs 2.1 4.7.99 Hugues Cassé"

(*
** Types
*)

type cabsloc = {
 lineno : int;
 filename: string;

}                                                                     

type typeSpecifier = (* Merge all specifiers into one type *)
    Tvoid                             (* Type specifier ISO 6.7.2 *)
  | Tchar
  | Tshort
  | Tint
  | Tlong
  | Tint64
  | Tfloat
  | Tdouble
  | Tsigned
  | Tunsigned
  | Tnamed of string
  | Tstruct of string * name_group list option  (* None if an old type *)
  | Tunion of string * name_group list option   (* None if an old type *)
  | Tenum of string * enum_item list option    (* None if an old type *)
  | Ttypeof of expression                      (* GCC __typeof__ *)

and storage =
    NO_STORAGE | AUTO | STATIC | EXTERN | REGISTER


(* Type specifier elements. These appear at the start of a declaration *)
and spec_elem =
    SpecTypedef
  | SpecInline
  | SpecAttr of attribute
  | SpecStorage of storage
  | SpecType of typeSpecifier

(* Declarator type. They modify the base type given in the specifier. Keep
 * them in the order as they are printed (this means that the top level
 * constructor for ARRAY and PTR is the inner-level in the meaning of the
 * declared type) *)
and decl_type =
 | JUSTBASE                               (* Prints the declared name *)
 | PARENTYPE of attribute list * decl_type * attribute list
                                          (* Prints "(attrs1 decl attrs2)".
                                           * attrs2 are attributes of the
                                           * declared identifier and it is as
                                           * if they appeared at the very end
                                           * of the declarator. attrs1 can
                                           * contain attributes for the
                                           * identifier or attributes for the
                                           * enclosing type.  *)
 | BITFIELD of expression                 (* Prints "name : exp" *)
 | ARRAY of decl_type * expression        (* Prints "decl [ exp ]". decl is
                                           * never a PTR. *)
 | PTR of attribute list * decl_type      (* Prints "* attrs decl" *)
 | PROTO of decl_type * single_name list * bool
                                          (* Prints "decl (args[, ...])".
                                           * decl is never a PTR. *)

(* The base type and the storage are common to all names. Each name might
 * contain type or storage modifiers *)
and name_group = spec_elem list * name list

and init_name_group = spec_elem list * init_name list

(* The decl_type is in the order in which they are printed. Only the name of
 * the declared identifier is pulled out. The attributes are those that are
 * printed after the declarator *)
and name = string * decl_type * attribute list

(* A name with an initializer *)
and init_name = name * init_expression

(* Single names are for declarations that cannot come in groups, like
 * function parameters and functions *)
and single_name = spec_elem list * name


and enum_item = string * expression

(*
** Declaration definition
*)
and definition =
   FUNDEF of single_name * body * cabsloc
 | DECDEF of init_name_group * cabsloc
 | TYPEDEF of name_group * cabsloc
 | ONLYTYPEDEF of spec_elem list * cabsloc
 | GLOBASM of string * cabsloc
 | PRAGMA of expression * cabsloc

and file = definition list


(*
** statements
*)
and bodyelem =                          (* ISO 6.8.2 allows declarations to
                                           be intermixed with statements *)
   BDEF of definition
 | BSTM of statement

and body = bodyelem list

and statement =
   NOP of cabsloc
 | COMPUTATION of expression * cabsloc
 | BLOCK of body * cabsloc
 | SEQUENCE of statement * statement * cabsloc
 | IF of expression * statement * statement * cabsloc
 | WHILE of expression * statement * cabsloc
 | DOWHILE of expression * statement * cabsloc
 | FOR of expression * expression * expression * statement * cabsloc
 | BREAK of cabsloc
 | CONTINUE of cabsloc
 | RETURN of expression * cabsloc
 | SWITCH of expression * statement * cabsloc
 | CASE of expression * statement * cabsloc
 | DEFAULT of statement * cabsloc
 | LABEL of string * statement * cabsloc
 | GOTO of string * cabsloc
 (* template, whether volatile, list of constraints and expressions for 
  * outputs and for inputs. The final list contains the clobbered registers  *)
 | ASM of string list * bool * (string * expression) list 
       * (string * expression) list * string list * cabsloc
       
(*
** Expressions
*)
and binary_operator =
    ADD | SUB | MUL | DIV | MOD
  | AND | OR
  | BAND | BOR | XOR | SHL | SHR
  | EQ | NE | LT | GT | LE | GE
  | ASSIGN
  | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN | SHL_ASSIGN | SHR_ASSIGN

and unary_operator =
    MINUS | PLUS | NOT | BNOT | MEMOF | ADDROF
  | PREINCR | PREDECR | POSINCR | POSDECR

and expression =
    NOTHING
  | UNARY of unary_operator * expression
  | BINARY of binary_operator * expression * expression
  | QUESTION of expression * expression * expression
  | CAST of (spec_elem list * decl_type) * expression
  | CALL of expression * expression list
  | COMMA of expression list
  | CONSTANT of constant
  | VARIABLE of string
  | EXPR_SIZEOF of expression
  | TYPE_SIZEOF of spec_elem list * decl_type
  | INDEX of expression * expression
  | MEMBEROF of expression * string
  | MEMBEROFPTR of expression * string
  | GNU_BODY of body

and constant =
  | CONST_INT of string
  | CONST_FLOAT of string
  | CONST_CHAR of string
  | CONST_STRING of string

and init_expression =
  | NO_INIT
  | SINGLE_INIT of expression
  | COMPOUND_INIT of (initwhat * init_expression) list

and initwhat =
    NEXT_INIT
  | INFIELD_INIT of string * initwhat
  | ATINDEX_INIT of expression * initwhat


                                        (* Each attribute has a name and some
                                         * optional arguments *)
and attribute = string * expression list
                                              

(*********** HELPER FUNCTIONS **********)

let missingFieldDecl = ("___missing_field_name", JUSTBASE, [])

let rec isStatic = function
    [] -> false
  | (SpecStorage STATIC) :: _ -> true
  | _ :: rest -> isStatic rest

let rec isExtern = function
    [] -> false
  | (SpecStorage EXTERN) :: _ -> true
  | _ :: rest -> isExtern rest

let rec isInline = function
    [] -> false
  | SpecInline :: _ -> true
  | _ :: rest -> isInline rest

let rec isTypedef = function
    [] -> false
  | SpecTypedef :: _ -> true
  | _ :: rest -> isTypedef rest


let get_statementloc (s : statement) : cabsloc =
begin
  match s with
  | NOP(loc) -> loc
  | COMPUTATION(_,loc) -> loc
  | BLOCK(_,loc) -> loc
  | SEQUENCE(_,_,loc) -> loc
  | IF(_,_,_,loc) -> loc
  | WHILE(_,_,loc) -> loc
  | DOWHILE(_,_,loc) -> loc
  | FOR(_,_,_,_,loc) -> loc
  | BREAK(loc) -> loc
  | CONTINUE(loc) -> loc
  | RETURN(_,loc) -> loc
  | SWITCH(_,_,loc) -> loc
  | CASE(_,_,loc) -> loc
  | DEFAULT(_,loc) -> loc
  | LABEL(_,_,loc) -> loc
  | GOTO(_,loc) -> loc
  | ASM(_,_,_,_,_,loc) -> loc
end
