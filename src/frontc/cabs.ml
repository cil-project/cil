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
    (* bool = whether it is inline *)
    NO_STORAGE | AUTO | STATIC of bool | EXTERN of bool | REGISTER | INLINE 

and base_type =
   NO_TYPE
 | VOID
 | INT of size * sign
 | BITFIELD of base_type * expression  
 | FLOAT of bool					(* is long ? *)
 | DOUBLE of bool					(* is long ? *)
 | PTR of base_type					(* is const ? *)
 | ARRAY of base_type * expression
 | STRUCT of string * name_group list
 | UNION of string * name_group list
 | PROTO of proto
 | OLD_PROTO of old_proto	
 | NAMED_TYPE of string
 | ENUM of string * enum_item list
 | CONST of base_type
 | VOLATILE of base_type
 | TYPEOF of expression                 (* GCC __typeof__ *)

and name = string * base_type * attributes * expression

and name_group = base_type * storage * name list

and single_name = base_type * storage * name

and enum_item = string * expression

and proto = base_type * single_name list * bool

and old_proto = base_type * string list * bool
 

(*
** Declaration definition
*)
and definition = 
   FUNDEF of single_name * body
 | OLDFUNDEF of single_name * name_group list * body
 | DECDEF of name_group
 | TYPEDEF of name_group
 | ONLYTYPEDEF of name_group
 | GLOBASM of string

and file = definition list				


(*
** statements
*)
and body = definition list * statement
and statement =
   NOP
 | COMPUTATION of expression
 | BLOCK of body
 | SEQUENCE of statement * statement
 | IF of expression * statement * statement
 | WHILE of expression * statement
 | DOWHILE of expression * statement
 | FOR of expression * expression * expression * statement
 | BREAK
 | CONTINUE
 | RETURN of expression
 | SWITCH of expression * statement
 | CASE of expression * statement
 | DEFAULT of statement
 | LABEL of string * statement
 | GOTO of string
 (* template, whether volatile, list of constraints and expressions for 
  * outputs and for inputs. The final list contains the clobbered registers  *)
 | ASM of string list * bool * (string * expression) list 
       * (string * expression) list * string list
       

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
  | CONST_COMPOUND of expression list

(*** Implementation Relatives ***)
and attributes = attribute list
                                        (* Each attribute has a name and some 
                                         * optional arguments *)
and attribute = string * expression list

