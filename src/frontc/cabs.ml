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
type size = NO_SIZE | CHAR | SHORT | LONG | LONG_LONG
and sign = NO_SIGN | SIGNED | UNSIGNED
and storage =
    NO_STORAGE | AUTO | STATIC | EXTERN | REGISTER

and base_type =
   NO_TYPE
 | VOID
 | INT of size * sign
 | BITFIELD of base_type * expression
 | FLOAT of bool					(* is long ? *)
 | DOUBLE of bool					(* is long ? *)
 | PTR of base_type					(* is const ? *)
 | ARRAY of base_type * expression

 | STRUCTDEF of string * name_group list
 | STRUCT of string                     (* A reference to a STRUCT but with 
                                         * no field definitions *)
 | UNIONDEF of string * name_group list
 | UNION of string

 | ENUMDEF of string * enum_item list
 | ENUM    of string


 | PROTO of proto
 | NAMED_TYPE of string
 | ATTRTYPE of base_type * attribute list(* Type with attributes *)
 | TYPEOF of expression                 (* GCC __typeof__ *)

and name_group = base_type * storage * name list

and name = string * base_type * attribute list * init_expression

and single_name = base_type * storage * name

and enum_item = string * expression

and proto =
    base_type * single_name list * bool (* isvar arg*) * bool (* inline *)



(*
** Declaration definition
*)
and definition = 
   FUNDEF of single_name * body
 | DECDEF of name_group
 | TYPEDEF of name_group
 | ONLYTYPEDEF of name_group
 | GLOBASM of string
 | PRAGMA of attribute

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
   NOP of Cil.location
 | COMPUTATION of expression * Cil.location
 | BLOCK of body * Cil.location
 | SEQUENCE of statement * statement * Cil.location
 | IF of expression * statement * statement * Cil.location
 | WHILE of expression * statement * Cil.location
 | DOWHILE of expression * statement * Cil.location
 | FOR of expression * expression * expression * statement * Cil.location
 | BREAK of Cil.location
 | CONTINUE of Cil.location
 | RETURN of expression * Cil.location
 | SWITCH of expression * statement * Cil.location
 | CASE of expression * statement * Cil.location
 | DEFAULT of statement * Cil.location
 | LABEL of string * statement * Cil.location
 | GOTO of string * Cil.location
 (* template, whether volatile, list of constraints and expressions for 
  * outputs and for inputs. The final list contains the clobbered registers  *)
 | ASM of string list * bool * (string * expression) list 
       * (string * expression) list * string list * Cil.location
       
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
  | CAST of base_type * expression
  | CALL of expression * expression list
  | COMMA of expression list
  | CONSTANT of constant
  | VARIABLE of string
  | EXPR_SIZEOF of expression
  | TYPE_SIZEOF of base_type
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
   

let get_statementloc (s : statement) : Cil.location =
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
