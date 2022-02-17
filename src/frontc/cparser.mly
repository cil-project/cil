/*(*
 *
 * Copyright (c) 2001-2003,
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 *  Ben Liblit          <liblit@cs.berkeley.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 **)
(**
** 1.0	3.22.99	Hugues Cass�	First version.
** 2.0  George Necula 12/12/00: Practically complete rewrite.
*)
*/
%{
open Cabs
open Cabshelper
module E = Errormsg

let parse_error msg : unit =       (* sm: c++-mode highlight hack: -> ' <- *)
  E.parse_error msg

let print = print_string

(* unit -> string option *)
(*
let getComments () =
  match !comments with
    [] -> None
  | _ ->
      let r = Some(String.concat "\n" (List.rev !comments)) in
      comments := [];
      r
*)

(* cabsloc -> cabsloc *)
(*
let handleLoc l =
  l.clcomment <- getComments();
  l
*)

(*
** Expression building
*)
let smooth_expression lst =
  match lst with
    [] -> NOTHING
  | [expr] -> expr
  | _ -> COMMA (lst)


let currentFunctionName = ref "<outside any function>"

let announceFunctionName ((n, decl, _, _):name) =
  !Lexerhack.add_identifier n;
  (* Start a context that includes the parameter names and the whole body.
   * Will pop when we finish parsing the function body *)
  !Lexerhack.push_context ();
  (* Go through all the parameter names and mark them as identifiers *)
  let rec findProto = function
      PROTO (d, args, _) when isJUSTBASE d ->
        List.iter (fun (_, (an, _, _, _)) -> !Lexerhack.add_identifier an) args

    | PROTO (d, _, _) -> findProto d
    | PARENTYPE (_, d, _) -> findProto d
    | PTR (_, d) -> findProto d
    | ARRAY (d, _, _) -> findProto d
    | _ -> parse_error "Cannot find the prototype in a function definition";
           raise Parsing.Parse_error

  and isJUSTBASE = function
      JUSTBASE -> true
    | PARENTYPE (_, d, _) -> isJUSTBASE d
    | _ -> false
  in
  findProto decl;
  currentFunctionName := n



let applyPointer (ptspecs: attribute list list) (dt: decl_type)
       : decl_type =
  (* Outer specification first *)
  let rec loop = function
      [] -> dt
    | attrs :: rest -> PTR(attrs, loop rest)
  in
  loop ptspecs

let doDeclaration (loc: cabsloc) (specs: spec_elem list) (nl: init_name list) : definition =
  if isTypedef specs then begin
    (* Tell the lexer about the new type names *)
    List.iter (fun ((n, _, _, _), _) -> !Lexerhack.add_type n) nl;
    TYPEDEF ((specs, List.map (fun (n, _) -> n) nl), loc)
  end else
    if nl = [] then
      ONLYTYPEDEF (specs, loc)
    else begin
      (* Tell the lexer about the new variable names *)
      List.iter (fun ((n, _, _, _), _) -> !Lexerhack.add_identifier n) nl;
      DECDEF ((specs, nl), loc)
    end


let doFunctionDef (loc: cabsloc)
                  (lend: cabsloc)
                  (specs: spec_elem list)
                  (n: name)
                  (b: block) : definition =
  let fname = (specs, n) in
  FUNDEF (fname, b, loc, lend)


let doOldParDecl (names: string list)
                 ((pardefs: name_group list), (isva: bool))
    : single_name list * bool =
  let findOneName n =
    (* Search in pardefs for the definition for this parameter *)
    let rec loopGroups = function
        [] -> ([SpecType Tint], (n, JUSTBASE, [], cabslu))
      | (specs, names) :: restgroups ->
          let rec loopNames = function
              [] -> loopGroups restgroups
            | ((n',_, _, _) as sn) :: _ when n' = n -> (specs, sn)
            | _ :: restnames -> loopNames restnames
          in
          loopNames names
    in
    loopGroups pardefs
  in
  let args = List.map findOneName names in
  (args, isva)

let checkConnective (s : string) : unit =
begin
  (* checking this means I could possibly have more connectives, with *)
  (* different meaning *)
  if (s <> "to") then (
    parse_error "transformer connective must be 'to'";
    raise Parsing.Parse_error
  )
  else ()
end

let int64_to_char value =
  if (compare value (Int64.of_int 255) > 0) || (compare value Int64.zero < 0) then
    begin
      let msg = Printf.sprintf "cparser:intlist_to_string: character 0x%Lx too big" value in
      parse_error msg;
      raise Parsing.Parse_error
    end
  else
    Char.chr (Int64.to_int value)

(* takes a not-nul-terminated list, and converts it to a string. *)
let rec intlist_to_string (str: int64 list):string =
  match str with
    [] -> ""  (* add nul-termination *)
  | value::rest ->
      let this_char = int64_to_char value in
      (String.make 1 this_char) ^ (intlist_to_string rest)

let fst3 (result, _, _) = result
let snd3 (_, result, _) = result
let trd3 (_, _, result) = result


(*
   transform:  __builtin_offsetof(type, member)
   into     :  (size_t) (&(type * ) 0)->member
 *)

let transformOffsetOf (speclist, dtype) member =
  let rec addPointer = function
    | JUSTBASE ->
	PTR([], JUSTBASE)
    | PARENTYPE (attrs1, dtype, attrs2) ->
	PARENTYPE (attrs1, addPointer dtype, attrs2)
    | ARRAY (dtype, attrs, expr) ->
	ARRAY (addPointer dtype, attrs, expr)
    | PTR (attrs, dtype) ->
	PTR (attrs, addPointer dtype)
    | PROTO (dtype, names, variadic) ->
	PROTO (addPointer dtype, names, variadic)
  in
  let nullType = (speclist, addPointer dtype) in
  let nullExpr = CONSTANT (CONST_INT "0") in
  let castExpr = CAST (nullType, SINGLE_INIT nullExpr) in

  let rec replaceBase = function
    | VARIABLE field ->
	MEMBEROFPTR (castExpr, field)
    | MEMBEROF (base, field) ->
	MEMBEROF (replaceBase base, field)
    | INDEX (base, index) ->
	INDEX (replaceBase base, index)
    | _ ->
	parse_error "malformed offset expression in __builtin_offsetof";
        raise Parsing.Parse_error
  in
  let memberExpr = replaceBase member in
  let addrExpr = UNARY (ADDROF, memberExpr) in
  let sizeofType = [SpecType Tsizet], JUSTBASE in
  let resultExpr = CAST (sizeofType, SINGLE_INIT addrExpr) in
  resultExpr

  let queue_to_int64_list queue =
    List.rev (Queue.fold (fun l e -> List.rev_append e l) [] queue)

  let queue_to_string queue =
    let buffer = Buffer.create (Queue.length queue) in
    Queue.iter
      (List.iter
        (fun value ->
          let char = int64_to_char value in
          Buffer.add_char buffer char))
      queue;
    Buffer.contents buffer

%}

%token <string * Cabs.cabsloc> IDENT
%token <string * Cabs.cabsloc> QUALIFIER
%token <int64 list * Cabs.cabsloc> CST_CHAR
%token <int64 list * Cabs.cabsloc> CST_WCHAR CST_CHAR16 CST_CHAR32
%token <string * Cabs.cabsloc> CST_INT
%token <string * Cabs.cabsloc> CST_FLOAT
%token <string * Cabs.cabsloc> CST_COMPLEX
%token <string * Cabs.cabsloc> NAMED_TYPE

/* Each character is its own list element, and the terminating nul is not
   included in this list. */
%token <int64 list * Cabs.cabsloc> CST_STRING
%token <int64 list * Cabs.cabsloc> CST_WSTRING CST_STRING16 CST_STRING32 CST_U8STRING

%token EOF
%token<Cabs.cabsloc> CHAR INT BOOL DOUBLE FLOAT VOID INT64 INT32
%token<Cabs.cabsloc> INT128 FLOAT128 COMPLEX /* C99 */
%token<Cabs.cabsloc> FLOAT32 FLOAT64 /* FloatN */
%token<Cabs.cabsloc> FLOAT32X FLOAT64X /* FloatNx */
%token<Cabs.cabsloc> GENERIC NORETURN /* C11 */
%token<Cabs.cabsloc> AUTOTYPE /* GCC */
%token<Cabs.cabsloc> ENUM STRUCT TYPEDEF UNION
%token<Cabs.cabsloc> SIGNED UNSIGNED LONG SHORT
%token<Cabs.cabsloc> VOLATILE EXTERN STATIC CONST ATOMIC RESTRICT AUTO REGISTER
%token<Cabs.cabsloc> THREAD

%token<Cabs.cabsloc> SIZEOF ALIGNOF

%token EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
%token AND_EQ PIPE_EQ CIRC_EQ INF_INF_EQ SUP_SUP_EQ
%token ARROW DOT

%token EQ_EQ EXCLAM_EQ INF SUP INF_EQ SUP_EQ
%token<Cabs.cabsloc> PLUS MINUS STAR
%token SLASH PERCENT
%token<Cabs.cabsloc> TILDE AND
%token PIPE CIRC
%token<Cabs.cabsloc> EXCLAM AND_AND
%token PIPE_PIPE
%token INF_INF SUP_SUP
%token<Cabs.cabsloc> PLUS_PLUS MINUS_MINUS

%token<Cabs.cabsloc> RPAREN
%token<Cabs.cabsloc> LPAREN RBRACE
%token<Cabs.cabsloc> LBRACE
%token LBRACKET RBRACKET
%token<Cabs.cabsloc> COLON
%token<Cabs.cabsloc> SEMICOLON
%token COMMA ELLIPSIS QUEST

%token<Cabs.cabsloc> BREAK CONTINUE GOTO RETURN
%token<Cabs.cabsloc> SWITCH CASE DEFAULT
%token<Cabs.cabsloc> WHILE DO FOR
%token<Cabs.cabsloc> IF
%token ELSE

%token<Cabs.cabsloc> ATTRIBUTE INLINE STATIC_ASSERT ASM TYPEOF REAL IMAG FUNCTION__ PRETTY_FUNCTION__ CLASSIFYTYPE
%token LABEL__
%token<Cabs.cabsloc> BUILTIN_VA_ARG ATTRIBUTE_USED
%token BUILTIN_VA_LIST
%token BLOCKATTRIBUTE
%token<Cabs.cabsloc> BUILTIN_TYPES_COMPAT BUILTIN_OFFSETOF
%token<Cabs.cabsloc> DECLSPEC
%token<string * Cabs.cabsloc> PRAGMA_LINE
%token<Cabs.cabsloc> PRAGMA
%token PRAGMA_EOL

/* sm: cabs tree transformation specification keywords */
%token<Cabs.cabsloc> AT_TRANSFORM AT_TRANSFORMEXPR AT_SPECIFIER AT_EXPR
%token AT_NAME

/* operator precedence */
%nonassoc 	IF
%nonassoc 	ELSE


%left	COMMA
%right	EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
                AND_EQ PIPE_EQ CIRC_EQ INF_INF_EQ SUP_SUP_EQ
%right	QUEST COLON
%left	PIPE_PIPE
%left	AND_AND
%left	PIPE
%left 	CIRC
%left	AND
%left	EQ_EQ EXCLAM_EQ
%left	INF SUP INF_EQ SUP_EQ
%left	INF_INF SUP_SUP
%left	PLUS MINUS
%left	STAR SLASH PERCENT CONST RESTRICT ATOMIC VOLATILE COMPLEX
%right	EXCLAM TILDE PLUS_PLUS MINUS_MINUS CAST RPAREN ADDROF SIZEOF ALIGNOF IMAG REAL CLASSIFYTYPE
%left 	LBRACKET
%left	DOT ARROW LPAREN LBRACE
%right  NAMED_TYPE     /* We'll use this to handle redefinitions of
                        * NAMED_TYPE as variables */
%left   IDENT

/* Non-terminals informations */
%start interpret file
%type <Cabs.definition list> file interpret globals

%type <Cabs.definition> global


%type <Cabs.attribute list> attributes attributes_with_asm asmattr
%type <Cabs.statement> statement
%type <Cabs.constant * cabsloc> constant
%type <int64 list Queue.t * Cabs.wchar_type * cabsloc> string_constant
%type <Cabs.expression * cabsloc> expression
%type <Cabs.expression> opt_expression
%type <Cabs.init_expression> init_expression
%type <Cabs.expression list * cabsloc> comma_expression
%type <Cabs.expression list * cabsloc> paren_comma_expression
%type <Cabs.expression list> arguments
%type <Cabs.expression list> bracket_comma_expression
/* %type <int64 list Queue.t * cabsloc> string_list */
/* %type <int64 list * cabsloc> wstring_list */

%type <Cabs.initwhat * Cabs.init_expression> initializer
%type <(Cabs.initwhat * Cabs.init_expression) list> initializer_list
%type <Cabs.initwhat> init_designators init_designators_opt

%type <spec_elem list * cabsloc> decl_spec_list
%type <typeSpecifier * cabsloc> type_spec
%type <Cabs.field_group list> struct_decl_list


%type <Cabs.name> old_proto_decl
%type <Cabs.single_name> parameter_decl
%type <Cabs.enum_item> enumerator
%type <Cabs.enum_item list> enum_list
%type <Cabs.definition> declaration function_def
%type <cabsloc * spec_elem list * name> function_def_start
%type <Cabs.spec_elem list * Cabs.decl_type> type_name
%type <Cabs.block * cabsloc * cabsloc> block
%type <Cabs.statement list> block_element_list
%type <string list> local_labels local_label_names
%type <string list> old_parameter_list_ne

%type <Cabs.init_name> init_declarator
%type <Cabs.init_name list> init_declarator_list
%type <Cabs.name> declarator
%type <Cabs.name * expression option> field_decl
%type <(Cabs.name * expression option) list> field_decl_list
%type <string * Cabs.decl_type> direct_decl
%type <Cabs.decl_type> abs_direct_decl abs_direct_decl_opt
%type <Cabs.decl_type * Cabs.attribute list> abstract_decl

 /* (* Each element is a "* <type_quals_opt>". *) */
%type <attribute list list * cabsloc> pointer pointer_opt
%type <Cabs.cabsloc> location
%type <Cabs.spec_elem * cabsloc> cvspec
%%

interpret:
  file EOF				{$1}
;
file: globals				{$1}
;
globals:
  /* empty */                           { [] }
| global globals                        { $1 :: $2 }
| SEMICOLON globals                     { $2 }
;

location:
   /* empty */                	{ currentLoc () }  %prec IDENT


/*** Global Definition ***/
global:
| declaration                           { $1 }
| function_def                          { $1 }
/*(* Some C header files ar shared with the C++ compiler and have linkage
   * specification *)*/
| EXTERN string_constant declaration { let q,t,l = $2 in LINKAGE (queue_to_string q, (*handleLoc*) l, [ $3 ]) }
| EXTERN string_constant LBRACE globals RBRACE
                                        { let q,t,l = $2 in LINKAGE (queue_to_string q, (*handleLoc*) l, $4) }
| ASM LPAREN string_constant RPAREN SEMICOLON
                                        { let q,t,l = $3 in GLOBASM (queue_to_string q, (*handleLoc*) $1) }
| pragma                                { $1 }
/* (* Old-style function prototype. This should be somewhere else, like in
    * "declaration". For now we keep it at global scope only because in local
    * scope it looks too much like a function call  *) */
| IDENT LPAREN old_parameter_list_ne RPAREN old_pardef_list SEMICOLON
                           { (* Convert pardecl to new style *)
                             let pardecl, isva = doOldParDecl $3 $5 in
                             (* Make the function declarator *)
                             doDeclaration ((*handleLoc*) (snd $1)) []
                               [((fst $1, PROTO(JUSTBASE, pardecl,isva), [], cabslu),
                                 NO_INIT)]
                            }
/* (* Old style function prototype, but without any arguments *) */
| IDENT LPAREN RPAREN  SEMICOLON
                           { (* Make the function declarator *)
                             doDeclaration ((*handleLoc*)(snd $1)) []
                               [((fst $1, PROTO(JUSTBASE,[],false), [], cabslu),
                                 NO_INIT)]
                            }
/* transformer for a toplevel construct */
| AT_TRANSFORM LBRACE global RBRACE  IDENT/*to*/  LBRACE globals RBRACE {
    checkConnective(fst $5);
    TRANSFORMER($3, $7, $1)
  }
/* transformer for an expression */
| AT_TRANSFORMEXPR LBRACE expression RBRACE  IDENT/*to*/  LBRACE expression RBRACE {
    checkConnective(fst $5);
    EXPRTRANSFORMER(fst $3, fst $7, $1)
  }
| location error SEMICOLON { PRAGMA (VARIABLE "parse_error", $1) }
;

id_or_typename:
    IDENT				{fst $1}
|   NAMED_TYPE				{fst $1}
|   AT_NAME LPAREN IDENT RPAREN         { "@name(" ^ fst $3 ^ ")" }     /* pattern variable name */
;

maybecomma:
   /* empty */                          { () }
|  COMMA                                { () }
;

/* *** Expressions *** */

primary_expression:                     /*(* 6.5.1. *)*/
|		IDENT
		        {VARIABLE (fst $1), snd $1}
|        	constant
		        {CONSTANT (fst $1), snd $1}
|		paren_comma_expression
		        {PAREN (smooth_expression (fst $1)), snd $1}
|		LPAREN block RPAREN
		        { GNU_BODY (fst3 $2), $1 }

     /*(* Next is Scott's transformer *)*/
|               AT_EXPR LPAREN IDENT RPAREN         /* expression pattern variable */
                         { EXPR_PATTERN(fst $3), $1 }
|   GENERIC LPAREN assignment_expression COMMA generic_assoc_list RPAREN {GENERIC ((fst $3), $5), $1}
;

/* (specifier, expression) list */
generic_assoc_list:
| generic_association {[$1]}
| generic_assoc_list COMMA generic_association {$3 :: $1}

/* specifier, expression */
generic_association:
| type_name COLON assignment_expression {($1, fst $3)}
| DEFAULT COLON assignment_expression {([SpecType Tdefault], JUSTBASE), fst $3}

postfix_expression:                     /*(* 6.5.2 *)*/
|               primary_expression
                        { $1 }
|		postfix_expression bracket_comma_expression
			{INDEX (fst $1, smooth_expression $2), snd $1}
|		postfix_expression LPAREN arguments RPAREN
			{CALL (fst $1, $3), snd $1}
|               BUILTIN_VA_ARG LPAREN expression COMMA type_name RPAREN
                        { let b, d = $5 in
                          CALL (VARIABLE "__builtin_va_arg",
                                [fst $3; TYPE_SIZEOF (b, d)]), $1 }
|               BUILTIN_TYPES_COMPAT LPAREN type_name COMMA type_name RPAREN
                        { let b1,d1 = $3 in
                          let b2,d2 = $5 in
                          CALL (VARIABLE "__builtin_types_compatible_p",
                                [TYPE_SIZEOF(b1,d1); TYPE_SIZEOF(b2,d2)]), $1 }
|               BUILTIN_OFFSETOF LPAREN type_name COMMA offsetof_member_designator RPAREN
                        { transformOffsetOf $3 $5, $1 }
|		postfix_expression DOT id_or_typename
		        {MEMBEROF (fst $1, $3), snd $1}
|		postfix_expression ARROW id_or_typename
		        {MEMBEROFPTR (fst $1, $3), snd $1}
|		postfix_expression PLUS_PLUS
		        {UNARY (POSINCR, fst $1), snd $1}
|		postfix_expression MINUS_MINUS
		        {UNARY (POSDECR, fst $1), snd $1}
/* (* We handle GCC constructor expressions *) */
|		LPAREN type_name RPAREN LBRACE initializer_list_opt RBRACE
		        { CAST($2, COMPOUND_INIT $5), $1 }
;

offsetof_member_designator:	/* GCC extension for __builtin_offsetof */
|		id_or_typename
		        { VARIABLE ($1) }
|		offsetof_member_designator DOT IDENT
			{ MEMBEROF ($1, fst $3) }
|		offsetof_member_designator bracket_comma_expression
			{ INDEX ($1, smooth_expression $2) }
;

unary_expression:   /*(* 6.5.3 *)*/
|               postfix_expression
                        { $1 }
|		PLUS_PLUS unary_expression
		        {UNARY (PREINCR, fst $2), $1}
|		MINUS_MINUS unary_expression
		        {UNARY (PREDECR, fst $2), $1}
|		SIZEOF unary_expression
		        {EXPR_SIZEOF (fst $2), $1}
|	 	SIZEOF LPAREN type_name RPAREN
		        {let b, d = $3 in TYPE_SIZEOF (b, d), $1}
|	 	REAL cast_expression
		        {REAL (fst $2), $1}
|	 	IMAG cast_expression
		        {IMAG (fst $2), $1}
|   CLASSIFYTYPE cast_expression
            {CLASSIFYTYPE (fst $2), $1}
|		ALIGNOF unary_expression
		        {EXPR_ALIGNOF (fst $2), $1}
|	 	ALIGNOF LPAREN type_name RPAREN
		        {let b, d = $3 in TYPE_ALIGNOF (b, d), $1}
|		PLUS cast_expression
		        {UNARY (PLUS, fst $2), $1}
|		MINUS cast_expression
		        {UNARY (MINUS, fst $2), $1}
|		STAR cast_expression
		        {UNARY (MEMOF, fst $2), $1}
|		AND cast_expression
		        {UNARY (ADDROF, fst $2), $1}
|		EXCLAM cast_expression
		        {UNARY (NOT, fst $2), $1}
|		TILDE cast_expression
		        {UNARY (BNOT, fst $2), $1}
|               AND_AND IDENT  { LABELADDR (fst $2), $1 }
;

cast_expression:   /*(* 6.5.4 *)*/
|              unary_expression
                         { $1 }
|		LPAREN type_name RPAREN cast_expression
		         { CAST($2, SINGLE_INIT (fst $4)), $1 }
;

multiplicative_expression:  /*(* 6.5.5 *)*/
|               cast_expression
                         { $1 }
|		multiplicative_expression STAR cast_expression
			{BINARY(MUL, fst $1, fst $3), snd $1}
|		multiplicative_expression SLASH cast_expression
			{BINARY(DIV, fst $1, fst $3), snd $1}
|		multiplicative_expression PERCENT cast_expression
			{BINARY(MOD, fst $1, fst $3), snd $1}
;

additive_expression:  /*(* 6.5.6 *)*/
|               multiplicative_expression
                        { $1 }
|		additive_expression PLUS multiplicative_expression
			{BINARY(ADD, fst $1, fst $3), snd $1}
|		additive_expression MINUS multiplicative_expression
			{BINARY(SUB, fst $1, fst $3), snd $1}
;

shift_expression:      /*(* 6.5.7 *)*/
|               additive_expression
                         { $1 }
|		shift_expression  INF_INF additive_expression
			{BINARY(SHL, fst $1, fst $3), snd $1}
|		shift_expression  SUP_SUP additive_expression
			{BINARY(SHR, fst $1, fst $3), snd $1}
;


relational_expression:   /*(* 6.5.8 *)*/
|               shift_expression
                        { $1 }
|		relational_expression INF shift_expression
			{BINARY(LT, fst $1, fst $3), snd $1}
|		relational_expression SUP shift_expression
			{BINARY(GT, fst $1, fst $3), snd $1}
|		relational_expression INF_EQ shift_expression
			{BINARY(LE, fst $1, fst $3), snd $1}
|		relational_expression SUP_EQ shift_expression
			{BINARY(GE, fst $1, fst $3), snd $1}
;

equality_expression:   /*(* 6.5.9 *)*/
|              relational_expression
                        { $1 }
|		equality_expression EQ_EQ relational_expression
			{BINARY(EQ, fst $1, fst $3), snd $1}
|		equality_expression EXCLAM_EQ relational_expression
			{BINARY(NE, fst $1, fst $3), snd $1}
;


bitwise_and_expression:   /*(* 6.5.10 *)*/
|               equality_expression
                       { $1 }
|		bitwise_and_expression AND equality_expression
			{BINARY(BAND, fst $1, fst $3), snd $1}
;

bitwise_xor_expression:   /*(* 6.5.11 *)*/
|               bitwise_and_expression
                       { $1 }
|		bitwise_xor_expression CIRC bitwise_and_expression
			{BINARY(XOR, fst $1, fst $3), snd $1}
;

bitwise_or_expression:   /*(* 6.5.12 *)*/
|               bitwise_xor_expression
                        { $1 }
|		bitwise_or_expression PIPE bitwise_xor_expression
			{BINARY(BOR, fst $1, fst $3), snd $1}
;

logical_and_expression:   /*(* 6.5.13 *)*/
|               bitwise_or_expression
                        { $1 }
|		logical_and_expression AND_AND bitwise_or_expression
			{BINARY(AND, fst $1, fst $3), snd $1}
;

logical_or_expression:   /*(* 6.5.14 *)*/
|               logical_and_expression
                        { $1 }
|		logical_or_expression PIPE_PIPE logical_and_expression
			{BINARY(OR, fst $1, fst $3), snd $1}
;

conditional_expression:    /*(* 6.5.15 *)*/
|               logical_or_expression
                         { $1 }
|		logical_or_expression QUEST opt_expression COLON conditional_expression
			{QUESTION (fst $1, $3, fst $5), snd $1}
;

/*(* The C spec says that left-hand sides of assignment expressions are unary
 * expressions. GCC allows cast expressions in there ! *)*/

assignment_expression:     /*(* 6.5.16 *)*/
|               conditional_expression
                         { $1 }
|		cast_expression EQ assignment_expression
			{BINARY(ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression PLUS_EQ assignment_expression
			{BINARY(ADD_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression MINUS_EQ assignment_expression
			{BINARY(SUB_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression STAR_EQ assignment_expression
			{BINARY(MUL_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression SLASH_EQ assignment_expression
			{BINARY(DIV_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression PERCENT_EQ assignment_expression
			{BINARY(MOD_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression AND_EQ assignment_expression
			{BINARY(BAND_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression PIPE_EQ assignment_expression
			{BINARY(BOR_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression CIRC_EQ assignment_expression
			{BINARY(XOR_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression INF_INF_EQ assignment_expression
			{BINARY(SHL_ASSIGN, fst $1, fst $3), snd $1}
|		cast_expression SUP_SUP_EQ assignment_expression
			{BINARY(SHR_ASSIGN, fst $1, fst $3), snd $1}
;

expression:           /*(* 6.5.17 *)*/
                assignment_expression
                        { $1 }
;


constant:
    CST_INT				{CONST_INT (fst $1), snd $1}
|   CST_FLOAT				{CONST_FLOAT (fst $1), snd $1}
|   CST_COMPLEX     {CONST_COMPLEX (fst $1), snd $1}
|   CST_CHAR {CONST_CHAR (fst $1), snd $1}
|   CST_WCHAR				{CONST_WCHAR (fst $1, WCHAR_T), snd $1}
|   CST_CHAR16      {CONST_WCHAR (fst $1, CHAR16_T), snd $1}
|   CST_CHAR32      {CONST_WCHAR (fst $1, CHAR32_T), snd $1}
|   string_constant     {
        let queue, typ, location = $1 in
        match typ with
        | CHAR -> CONST_STRING (queue_to_string queue, NO_ENCODING), location
        | CHAR_UTF8 -> CONST_STRING (queue_to_string queue, UTF8), location
        | _ -> CONST_WSTRING (queue_to_int64_list queue, typ), location
    }
;

one_string_constant:
/* Don't concat multiple strings.  For asm templates. */
    CST_STRING                          {intlist_to_string (fst $1) }
;

string_constant:
    one_string                          {
      let queue = Queue.create () in
      let str, typ, loc = $1 in
      Queue.add str queue;
      queue, typ, loc
    }
|   CST_WSTRING {
      let queue = Queue.create () in
      Queue.add (fst $1) queue;
      queue, WCHAR_T, snd $1
    }
|   CST_STRING16 {
      let queue = Queue.create () in
      Queue.add (fst $1) queue;
      queue, CHAR16_T, snd $1
    }
|   CST_STRING32 {
      let queue = Queue.create () in
      Queue.add (fst $1) queue;
      queue, CHAR32_T, snd $1
    }
|   string_constant one_string              {
      let queue, typ, loc = $1 in
      let str, typ2, _ = $2 in
      Queue.add str queue;
      if typ2 = CHAR_UTF8 && typ <> CHAR && typ <> CHAR_UTF8 then (
        parse_error "Incompatible string literals";
        raise Parsing.Parse_error)
      else
        let typ3 = if typ2 = CHAR_UTF8 then CHAR_UTF8 else typ in
        queue, typ3, loc
    }
|   string_constant CST_WSTRING {
      let queue, typ, loc = $1 in
      Queue.add (fst $2) queue;
      if typ <> CHAR && typ <> WCHAR_T then (
        parse_error "Incompatible string literals";
        raise Parsing.Parse_error)
      else
        queue, WCHAR_T, loc
    }
|   string_constant CST_STRING16 {
      let queue, typ, loc = $1 in
      Queue.add (fst $2) queue;
      if typ <> CHAR && typ <> CHAR16_T then (
        parse_error "Incompatible string literals";
        raise Parsing.Parse_error)
      else
        queue, CHAR16_T, loc
    }
|   string_constant CST_STRING32 {
      let queue, typ, loc = $1 in
      Queue.add (fst $2) queue;
      if typ <> CHAR && typ <> CHAR32_T then (
        parse_error "Incompatible string literals";
        raise Parsing.Parse_error)
      else
        queue, CHAR32_T, loc
    }
;

one_string:
    CST_STRING				{fst $1, CHAR, snd $1}
|   CST_U8STRING      {fst $1, CHAR_UTF8, snd $1}
|   FUNCTION__                          {(Cabshelper.explodeStringToInts
					    !currentFunctionName), CHAR, $1}
|   PRETTY_FUNCTION__                   {(Cabshelper.explodeStringToInts
					    !currentFunctionName), CHAR, $1}

init_expression:
     expression         { SINGLE_INIT (fst $1) }
|    LBRACE initializer_list_opt RBRACE
			{ COMPOUND_INIT $2}

initializer_list:    /* ISO 6.7.8. Allow a trailing COMMA */
    initializer                             { [$1] }
|   initializer COMMA initializer_list_opt  { $1 :: $3 }
;
initializer_list_opt:
    /* empty */                             { [] }
|   initializer_list                        { $1 }
;
initializer:
    init_designators eq_opt init_expression { ($1, $3) }
|   gcc_init_designators init_expression { ($1, $2) }
|                       init_expression { (NEXT_INIT, $1) }
;
eq_opt:
   EQ                        { () }
   /*(* GCC allows missing = *)*/
|  /*(* empty *)*/               { () }
;
init_designators:
    DOT id_or_typename init_designators_opt      { INFIELD_INIT($2, $3) }
|   LBRACKET  expression RBRACKET init_designators_opt
                                        { ATINDEX_INIT(fst $2, $4) }
|   LBRACKET  expression ELLIPSIS expression RBRACKET
                                        { ATINDEXRANGE_INIT(fst $2, fst $4) }
;
init_designators_opt:
   /* empty */                          { NEXT_INIT }
|  init_designators                     { $1 }
;

gcc_init_designators:  /*(* GCC supports these strange things *)*/
   id_or_typename COLON                 { INFIELD_INIT($1, NEXT_INIT) }
;

arguments:
                /* empty */         { [] }
|               comma_expression    { fst $1 }
;

opt_expression:
	        /* empty */
	        	{NOTHING}
|	        comma_expression
	        	{smooth_expression (fst $1)}
;

comma_expression:
	        expression                        {[fst $1], snd $1}
|               expression COMMA comma_expression { fst $1 :: fst $3, snd $1 }
|               error COMMA comma_expression      { $3 }
;

comma_expression_opt:
                /* empty */         { NOTHING }
|               comma_expression    { smooth_expression (fst $1) }
;

paren_comma_expression:
  LPAREN comma_expression RPAREN                   { $2 }
| LPAREN error RPAREN                              { [], $1 }
;

bracket_comma_expression:
  LBRACKET comma_expression RBRACKET                   { fst $2 }
| LBRACKET error RBRACKET                              { [] }
;


/*** statements ***/
block: /* ISO 6.8.2 */
    block_begin local_labels block_attrs block_element_list RBRACE
                                         {!Lexerhack.pop_context();
                                          { blabels = $2;
                                            battrs = $3;
                                            bstmts = $4 },
					    $1, $5
                                         }
|   error location RBRACE                { { blabels = [];
                                             battrs  = [];
                                             bstmts  = [] },
					     $2, $3
                                         }
;
block_begin:
    LBRACE      		         {!Lexerhack.push_context (); $1}
;

block_attrs:
   /* empty */                                              { [] }
|  BLOCKATTRIBUTE paren_attr_list_ne
                                        { [("__blockattribute__", $2)] }
;

/* statements and declarations in a block, in any order (for C99 support) */
block_element_list:
    /* empty */                          { [] }
|   declaration block_element_list       { DEFINITION($1) :: $2 }
|   statement block_element_list         { $1 :: $2 }
/*(* GCC accepts a label at the end of a block *)*/
|   IDENT COLON	                         { [ LABEL (fst $1, NOP (snd $1),
                                                    snd $1)] }
|   pragma block_element_list            { $2 }
;

local_labels:
   /* empty */                                       { [] }
|  LABEL__ local_label_names SEMICOLON local_labels  { $2 @ $4 }
;
local_label_names:
   IDENT                                 { [ fst $1 ] }
|  IDENT COMMA local_label_names         { fst $1 :: $3 }
;

statement:
|  attribute_nocv_list SEMICOLON {NOP ((*handleLoc*) $2) }
|  statement_no_null { $1 }

statement_no_null:
    comma_expression SEMICOLON
	        	{COMPUTATION (smooth_expression (fst $1), joinLoc (snd $1) $2)}
|   block               {BLOCK (fst3 $1, joinLoc (snd3 $1) (trd3 $1))}
|   IF paren_comma_expression location statement location                   %prec IF
                	{IF (smooth_expression (fst $2), $4, NOP $1, joinLoc $1 $5, joinLoc (snd $2) $3)}
|   IF paren_comma_expression location statement location ELSE statement location
	                {IF (smooth_expression (fst $2), $4, $7, joinLoc $1 $8, joinLoc (snd $2) $3)}
|   SWITCH paren_comma_expression location statement location
                        {SWITCH (smooth_expression (fst $2), $4, joinLoc $1 $5, joinLoc (snd $2) $3)}
|   WHILE paren_comma_expression location statement location
	        	{WHILE (smooth_expression (fst $2), $4, joinLoc $1 $5, joinLoc (snd $2) $3)}
|   DO statement WHILE paren_comma_expression SEMICOLON
	        	         {DOWHILE (smooth_expression (fst $4), $2, joinLoc $1 $5, joinLoc (snd $4) $5)}
|   FOR LPAREN for_clause opt_expression
	        SEMICOLON opt_expression RPAREN statement location
	                         {FOR ($3, $4, $6, $8, joinLoc $1 $9, joinLoc $2 $7)}
|   IDENT COLON attribute_nocv_list location statement_no_null
		                 {(* The only attribute that should appear here
                                     is "unused". For now, we drop this on the
                                     floor, since unused labels are usually
                                     removed anyways by Rmtmps. *)
                                  LABEL (fst $1, $5, joinLoc (snd $1) $4)}
|   IDENT COLON attribute_nocv_list location SEMICOLON
		                 {(* The only attribute that should appear here
                                     is "unused". For now, we drop this on the
                                     floor, since unused labels are usually
                                     removed anyways by Rmtmps. *)
                                  LABEL (fst $1, NOP ($5), joinLoc (snd $1) $4)}
|   CASE expression COLON statement location
	                         {CASE (fst $2, $4, joinLoc $1 $5, joinLoc $1 $3)}
|   CASE expression ELLIPSIS expression COLON statement location
	                         {CASERANGE (fst $2, fst $4, $6, joinLoc $1 $7, joinLoc $1 $5)}
|   DEFAULT COLON statement location
	                         {DEFAULT ($3, joinLoc $1 $4, joinLoc $1 $2)}
|   RETURN SEMICOLON		 {RETURN (NOTHING, joinLoc $1 $2)}
|   RETURN comma_expression SEMICOLON
	                         {RETURN (smooth_expression (fst $2), joinLoc $1 $3)}
|   BREAK SEMICOLON     {BREAK (joinLoc $1 $2)}
|   CONTINUE SEMICOLON	 {CONTINUE (joinLoc $1 $2)}
|   GOTO IDENT SEMICOLON
		                 {GOTO (fst $2, joinLoc $1 $3)}
|   GOTO STAR comma_expression SEMICOLON
                                 { COMPGOTO (smooth_expression (fst $3), joinLoc $1 $4) }
|   ASM asmattr LPAREN asmtemplate asmoutputs RPAREN SEMICOLON
                        { ASM ($2, $4, $5, joinLoc $1 $7) }
|   error location   SEMICOLON   { (NOP $2)}
;


for_clause:
    opt_expression SEMICOLON     { FC_EXP $1 }
|   declaration                  { FC_DECL $1 }
;

declaration:                                /* ISO 6.7.*/
    decl_spec_list init_declarator_list SEMICOLON
                                       { doDeclaration (joinLoc (snd $1) $3) (fst $1) $2 }
|   decl_spec_list_no_attr_only SEMICOLON
                                       { doDeclaration (joinLoc (snd $1) $2) (fst $1) [] }
|   static_assert_declaration          { let (e, m, loc) = $1 in STATIC_ASSERT (e, m, loc) }
;

static_assert_declaration:

|   STATIC_ASSERT LPAREN expression RPAREN /* C23 */
      {
        (fst $3, "", $1)
      }
|   STATIC_ASSERT LPAREN expression COMMA string_constant RPAREN
      {
        let q,t,l = $5 in
        (fst $3, queue_to_string q, $1)
      }
;

;
init_declarator_list:                       /* ISO 6.7 */
    init_declarator                              { [$1] }
|   init_declarator COMMA init_declarator_attr_list   { $1 :: $3 }
;

init_declarator_attr_list:
  init_declarator_attr { [ $1 ] }
| init_declarator_attr COMMA init_declarator_attr_list { $1 :: $3 }
;

init_declarator_attr:
  attribute_nocv_list init_declarator {
    let ((name, decl, attrs, loc), init) = $2 in
    ((name, PARENTYPE ($1,decl,[]), attrs, loc), init)
  }
;

;
init_declarator:                             /* ISO 6.7 */
    declarator                          { ($1, NO_INIT) }
|   declarator EQ init_expression
                                        { ($1, $3) }
;

decl_spec_list_common:                  /* ISO 6.7 */
                                        /* ISO 6.7.1 */
|   TYPEDEF decl_spec_list_opt          { SpecTypedef :: $2, $1  }
|   EXTERN decl_spec_list_opt           { SpecStorage EXTERN :: $2, $1 }
|   STATIC  decl_spec_list_opt          { SpecStorage STATIC :: $2, $1 }
|   AUTO   decl_spec_list_opt           { SpecStorage AUTO :: $2, $1 }
|   REGISTER decl_spec_list_opt         { SpecStorage REGISTER :: $2, $1}
                                        /* ISO 6.7.2 */
|   type_spec decl_spec_list_opt_no_named { SpecType (fst $1) :: $2, snd $1 }
|   ATOMIC LPAREN decl_spec_list RPAREN decl_spec_list_opt_no_named { (fst $3) @ SpecCV(CV_ATOMIC) :: $5, $1 }
                                        /* ISO 6.7.4 */
|   INLINE decl_spec_list_opt           { SpecInline :: $2, $1 }
|   NORETURN decl_spec_list_opt         { SpecNoreturn  :: $2, $1 }

|   cvspec decl_spec_list_opt           { (fst $1) :: $2, snd $1 }
/* specifier pattern variable (must be last in spec list) */
|   AT_SPECIFIER LPAREN IDENT RPAREN    { [ SpecPattern(fst $3) ], $1 }

decl_spec_list_no_attr_only:
|   decl_spec_list_common               { $1 }
|   attribute_nocv decl_spec_list       { SpecAttr (fst $1) :: (fst $2), snd $1 }
;

decl_spec_list:
|   decl_spec_list_common               { $1 }
|   attribute_nocv decl_spec_list_opt   { SpecAttr (fst $1) :: $2, snd $1 }
;
/* (* In most cases if we see a NAMED_TYPE we must shift it. Thus we declare
    * NAMED_TYPE to have right associativity  *) */
decl_spec_list_opt:
    /* empty */                         { [] } %prec NAMED_TYPE
|   decl_spec_list                      { fst $1 }
;
/* (* We add this separate rule to handle the special case when an appearance
    * of NAMED_TYPE should not be considered as part of the specifiers but as
    * part of the declarator. IDENT has higher precedence than NAMED_TYPE  *)
 */
decl_spec_list_opt_no_named:
    /* empty */                         { [] } %prec IDENT
|   decl_spec_list                      { fst $1 }
;
type_spec:   /* ISO 6.7.2 */
    VOID            { Tvoid, $1}
|   CHAR            { Tchar, $1 }
|   BOOL            { Tbool, $1 }
|   SHORT           { Tshort, $1 }
|   INT             { Tint, $1 }
|   LONG            { Tlong, $1 }
|   INT64           { Tint64, $1 }
|   INT128          { Tint128, $1 }
|   FLOAT           { Tfloat, $1 }
|   FLOAT32         { Tfloat32, $1 }
|   FLOAT64         { Tfloat64, $1 }
|   FLOAT128        { Tfloat128, $1 }
|   FLOAT32X        { Tfloat32x, $1 }
|   FLOAT64X        { Tfloat64x, $1 }
|   DOUBLE          { Tdouble, $1 }
|   AUTOTYPE        { Tauto, $1 }
/* |   COMPLEX FLOAT   { Tfloat, $2 } */
/* |   COMPLEX FLOAT128{ Tfloat128, $2 } */
/* |   COMPLEX DOUBLE  { Tdouble, $2 } */
|   SIGNED          { Tsigned, $1 }
|   UNSIGNED        { Tunsigned, $1 }
|   STRUCT                 id_or_typename
                                                   { Tstruct ($2, None,    []), $1 }
|   STRUCT just_attributes id_or_typename
                                                   { Tstruct ($3, None,    $2), $1 }
|   STRUCT                 id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tstruct ($2, Some $4, []), $1 }
|   STRUCT                                LBRACE struct_decl_list RBRACE
                                                   { Tstruct ("", Some $3, []), $1 }
|   STRUCT just_attributes id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tstruct ($3, Some $5, $2), $1 }
|   STRUCT just_attributes                LBRACE struct_decl_list RBRACE
                                                   { Tstruct ("", Some $4, $2), $1 }
|   UNION                  id_or_typename
                                                   { Tunion  ($2, None,    []), $1 }
|   UNION                  id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tunion  ($2, Some $4, []), $1 }
|   UNION                                 LBRACE struct_decl_list RBRACE
                                                   { Tunion  ("", Some $3, []), $1 }
|   UNION  just_attributes id_or_typename LBRACE struct_decl_list RBRACE
                                                   { Tunion  ($3, Some $5, $2), $1 }
|   UNION  just_attributes                LBRACE struct_decl_list RBRACE
                                                   { Tunion  ("", Some $4, $2), $1 }
|   ENUM                   id_or_typename
                                                   { Tenum   ($2, None,    []), $1 }
|   ENUM                   id_or_typename LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ($2, Some $4, []), $1 }
|   ENUM                                  LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ("", Some $3, []), $1 }
|   ENUM   just_attributes id_or_typename LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ($3, Some $5, $2), $1 }
|   ENUM   just_attributes                LBRACE enum_list maybecomma RBRACE
                                                   { Tenum   ("", Some $4, $2), $1 }
|   NAMED_TYPE      { Tnamed (fst $1), snd $1 }
|   TYPEOF LPAREN comma_expression RPAREN     { TtypeofE (smooth_expression (fst $3)), $1 }
|   TYPEOF LPAREN type_name RPAREN      { let s, d = $3 in
                                          TtypeofT (s, d), $1 }
;
struct_decl_list: /* (* ISO 6.7.2. Except that we allow empty structs. We
                      * also allow missing field names. *)
                   */
   /* empty */                           { [] }
|  decl_spec_list                 SEMICOLON struct_decl_list
                                         { (fst $1,
                                            [(missingFieldDecl, None)]) :: $3 }
/*(* GCC allows extra semicolons *)*/
|                                 SEMICOLON struct_decl_list
                                         { $2 }
|  decl_spec_list field_decl_list SEMICOLON struct_decl_list
                                          { (fst $1, $2)
                                            :: $4 }
/*(* MSVC allows pragmas in strange places *)*/
|  pragma struct_decl_list                { $2 }

|  error                          SEMICOLON struct_decl_list
                                          { $3 }
/*(* C11 allows static_assert-declaration *)*/
|  static_assert_declaration             {
       []
   }

|  static_assert_declaration      SEMICOLON struct_decl_list  {
       $3
   }

;
field_decl_list: /* (* ISO 6.7.2 *) */
    field_decl                           { [$1] }
|   field_decl COMMA field_decl_list     { $1 :: $3 }
;
field_decl: /* (* ISO 6.7.2. Except that we allow unnamed fields. *) */
|   declarator                      { ($1, None) }
|   declarator COLON expression attributes
                                    { let (n,decl,al,loc) = $1 in
                                      let al' = al @ $4 in
                                     ((n,decl,al',loc), Some (fst $3)) }
|              COLON expression     { (missingFieldDecl, Some (fst $2)) }
;

enum_list: /* (* ISO 6.7.2.2 *) */
    enumerator				{[$1]}
|   enum_list COMMA enumerator	        {$1 @ [$3]}
|   enum_list COMMA error               { $1 }
;
enumerator:
    IDENT			{(fst $1, NOTHING, snd $1)}
|   IDENT EQ expression		{(fst $1, fst $3, snd $1)}
;


declarator:  /* (* ISO 6.7.5. Plus Microsoft declarators.*) */
   pointer_opt direct_decl location attributes_with_asm
                               { let (n, decl) = $2 in
                                (n, applyPointer (fst $1) decl, $4, joinLoc (snd $1) $3) }
;


direct_decl: /* (* ISO 6.7.5 *) */
                                   /* (* We want to be able to redefine named
                                    * types as variable names *) */
|   id_or_typename                 { ($1, JUSTBASE) }

|   LPAREN attributes declarator RPAREN
                                   { let (n,decl,al,loc) = $3 in
                                     (n, PARENTYPE($2,decl,al)) }

|   direct_decl LBRACKET attributes comma_expression_opt RBRACKET
                                   { let (n, decl) = $1 in
                                     (n, ARRAY(decl, $3, $4)) }
|   direct_decl LBRACKET attributes error RBRACKET
                                   { let (n, decl) = $1 in
                                     (n, ARRAY(decl, $3, NOTHING)) }
|   direct_decl parameter_list_startscope rest_par_list RPAREN
                                   { let (n, decl) = $1 in
                                     let (params, isva) = $3 in
                                     !Lexerhack.pop_context ();
                                     (n, PROTO(decl, params, isva))
                                   }
;
parameter_list_startscope:
    LPAREN                         { !Lexerhack.push_context () }
;
rest_par_list:
|   /* empty */                    { ([], false) }
|   parameter_decl rest_par_list1  { let (params, isva) = $2 in
                                     ($1 :: params, isva)
                                   }
;
rest_par_list1:
    /* empty */                         { ([], false) }
|   COMMA ELLIPSIS                      { ([], true) }
|   COMMA parameter_decl rest_par_list1 { let (params, isva) = $3 in
                                          ($2 :: params, isva)
                                        }
;


parameter_decl: /* (* ISO 6.7.5 *) */
   decl_spec_list declarator              { (fst $1, $2) }
|  decl_spec_list abstract_decl           { let d, a = $2 in
                                            (fst $1, ("", d, a, cabslu)) }
|  decl_spec_list                         { (fst $1, ("", JUSTBASE, [], cabslu)) }
|  LPAREN parameter_decl RPAREN           { $2 }
;

/* (* Old style prototypes. Like a declarator *) */
old_proto_decl:
  pointer_opt direct_old_proto_decl   { let (n, decl, a) = $2 in
					  (n, applyPointer (fst $1) decl,
                                           a, snd $1)
                                      }

;

direct_old_proto_decl:
  direct_decl LPAREN old_parameter_list_ne RPAREN old_pardef_list
                                   { let par_decl, isva = doOldParDecl $3 $5 in
                                     let n, decl = $1 in
                                     (n, PROTO(decl, par_decl, isva), [])
                                   }
| direct_decl LPAREN                       RPAREN
                                   { let n, decl = $1 in
                                     (n, PROTO(decl, [], false), [])
                                   }

/* (* appears sometimesm but generates a shift-reduce conflict. *)
| LPAREN STAR direct_decl LPAREN old_parameter_list_ne RPAREN RPAREN LPAREN RPAREN old_pardef_list
                                   { let par_decl, isva
                                             = doOldParDecl $5 $10 in
                                     let n, decl = $3 in
                                     (n, PROTO(decl, par_decl, isva), [])
                                   }
*/
;

old_parameter_list_ne:
|  IDENT                                       { [fst $1] }
|  IDENT COMMA old_parameter_list_ne           { let rest = $3 in
                                                 (fst $1 :: rest) }
;

old_pardef_list:
   /* empty */                            { ([], false) }
|  decl_spec_list old_pardef SEMICOLON ELLIPSIS
                                          { ([(fst $1, $2)], true) }
|  decl_spec_list old_pardef SEMICOLON old_pardef_list
                                          { let rest, isva = $4 in
                                            ((fst $1, $2) :: rest, isva)
                                          }
;

old_pardef:
   declarator                             { [$1] }
|  declarator COMMA old_pardef            { $1 :: $3 }
|  error                                  { [] }
;


pointer: /* (* ISO 6.7.5 *) */
   STAR attributes pointer_opt  { $2 :: fst $3, $1 }
;
pointer_opt:
   /**/                          { let l = currentLoc () in
                                   ([], l) }
|  pointer                       { $1 }
;

type_name: /* (* ISO 6.7.6 *) */
  decl_spec_list abstract_decl { let d, a = $2 in
                                 if a <> [] then begin
                                   parse_error "attributes in type name";
                                   raise Parsing.Parse_error
                                 end;
                                 (fst $1, d)
                               }
| decl_spec_list               { (fst $1, JUSTBASE) }
;
abstract_decl: /* (* ISO 6.7.6. *) */
  pointer_opt abs_direct_decl attributes  { applyPointer (fst $1) $2, $3 }
| pointer                                 { applyPointer (fst $1) JUSTBASE, [] }
;

abs_direct_decl: /* (* ISO 6.7.6. We do not support optional declarator for
                     * functions. Plus Microsoft attributes. See the
                     * discussion for declarator. *) */
|   LPAREN attributes abstract_decl RPAREN
                                   { let d, a = $3 in
                                     PARENTYPE ($2, d, a)
                                   }

|   LPAREN error RPAREN
                                   { JUSTBASE }

|   abs_direct_decl_opt LBRACKET comma_expression_opt RBRACKET
                                   { ARRAY($1, [], $3) }
/*(* The next should be abs_direct_decl_opt but we get conflicts *)*/
|   abs_direct_decl  parameter_list_startscope rest_par_list RPAREN
                                   { let (params, isva) = $3 in
                                     !Lexerhack.pop_context ();
                                     PROTO ($1, params, isva)
                                   }
;
abs_direct_decl_opt:
    abs_direct_decl                 { $1 }
|   /* empty */                     { JUSTBASE }
;
function_def:  /* (* ISO 6.9.1 *) */
  function_def_start block
          { let (loc, specs, decl) = $1 in
            currentFunctionName := "<__FUNCTION__ used outside any functions>";
            !Lexerhack.pop_context (); (* The context pushed by
                                    * announceFunctionName *)
            doFunctionDef ((*handleLoc*) loc) (trd3 $2) specs decl (fst3 $2)
          }


function_def_start:  /* (* ISO 6.9.1 *) */
  decl_spec_list declarator
                            { announceFunctionName $2;
                              (snd $1, fst $1, $2)
                            }

/* (* Old-style function prototype *) */
| decl_spec_list old_proto_decl
                            { announceFunctionName $2;
                              (snd $1, fst $1, $2)
                            }
/* (* New-style function that does not have a return type *) */
| IDENT parameter_list_startscope rest_par_list RPAREN
                           { let (params, isva) = $3 in
                             let fdec =
                               (fst $1, PROTO(JUSTBASE, params, isva), [], snd $1) in
                             announceFunctionName fdec;
                             (* Default is int type *)
                             let defSpec = [SpecType Tint] in
                             (snd $1, defSpec, fdec)
                           }

/* (* No return type and old-style parameter list *) */
| IDENT LPAREN old_parameter_list_ne RPAREN old_pardef_list
                           { (* Convert pardecl to new style *)
                             let pardecl, isva = doOldParDecl $3 $5 in
                             (* Make the function declarator *)
                             let fdec = (fst $1,
                                         PROTO(JUSTBASE, pardecl,isva),
                                         [], snd $1) in
                             announceFunctionName fdec;
                             (* Default is int type *)
                             let defSpec = [SpecType Tint] in
                             (snd $1, defSpec, fdec)
                            }
/* (* No return type and no parameters *) */
| IDENT LPAREN                      RPAREN
                           { (* Make the function declarator *)
                             let fdec = (fst $1,
                                         PROTO(JUSTBASE, [], false),
                                         [], snd $1) in
                             announceFunctionName fdec;
                             (* Default is int type *)
                             let defSpec = [SpecType Tint] in
                             (snd $1, defSpec, fdec)
                            }
;

/* const/volatile as type specifier elements */
cvspec:
    CONST                               { SpecCV(CV_CONST), $1 }
|   VOLATILE                            { SpecCV(CV_VOLATILE), $1 }
|   RESTRICT                            { SpecCV(CV_RESTRICT), $1 }
|   COMPLEX                             { SpecCV(CV_COMPLEX), $1 }
|   ATOMIC                              { SpecCV(CV_ATOMIC), $1 }
;

/*** GCC attributes ***/
attributes:
    /* empty */				{ []}
|   attribute attributes	        { fst $1 :: $2 }
;

/* (* In some contexts we can have an inline assembly to specify the name to
    * be used for a global. We treat this as a name attribute *) */
attributes_with_asm:
    /* empty */                         { [] }
|   attribute attributes_with_asm       { fst $1 :: $2 }
|   ASM LPAREN string_constant RPAREN attributes
                                        { let q,t,l = $3 in ("__asm__",
					   [CONSTANT(CONST_STRING (queue_to_string q, NO_ENCODING))]) :: $5 }
;

/* things like __attribute__, but no const/volatile */
attribute_nocv:
    ATTRIBUTE LPAREN paren_attr_list RPAREN
                                        { ("__attribute__", $3), $1 }
/*(*
|   ATTRIBUTE_USED                      { ("__attribute__",
                                             [ VARIABLE "used" ]), $1 }
*)*/
|   DECLSPEC paren_attr_list_ne         { ("__declspec", $2), $1 }
                                        /* ISO 6.7.3 */
|   THREAD                              { ("__thread",[]), $1 }
|   QUALIFIER                     {("__attribute__",[VARIABLE(fst $1)]),snd $1}
;

attribute_nocv_list:
    /* empty */				{ []}
|   attribute_nocv attribute_nocv_list  { fst $1 :: $2 }
;

/* __attribute__ plus const/volatile */
attribute:
    attribute_nocv                      { $1 }
|   CONST                               { ("const", []), $1 }
|   RESTRICT                            { ("restrict",[]), $1 }
|   VOLATILE                            { ("volatile",[]), $1 }
|   STATIC                              { ("static",[]), $1 }
;

/* (* sm: I need something that just includes __attribute__ and nothing more,
 *  to support them appearing between the 'struct' keyword and the type name.
 * Actually, a declspec can appear there as well (on MSVC) *)  */
just_attribute:
    ATTRIBUTE LPAREN paren_attr_list RPAREN
                                        { ("__attribute__", $3) }
|   DECLSPEC paren_attr_list_ne         { ("__declspec", $2) }
;

/* this can't be empty, b/c I folded that possibility into the calling
 * productions to avoid some S/R conflicts */
just_attributes:
    just_attribute                      { [$1] }
|   just_attribute just_attributes      { $1 :: $2 }
;

/** (* PRAGMAS and ATTRIBUTES *) ***/
pragma:
| PRAGMA attr PRAGMA_EOL		{ PRAGMA ($2, $1) }
| PRAGMA attr SEMICOLON PRAGMA_EOL	{ PRAGMA ($2, $1) }
| PRAGMA_LINE                           { PRAGMA (VARIABLE (fst $1),
                                                  snd $1) }
;

/* (* We want to allow certain strange things that occur in pragmas, so we
    * cannot use directly the language of expressions *) */
primary_attr:
    IDENT				                        { VARIABLE (fst $1) }
    /* (* This is just so code such as __attribute(_NoReturn) is not rejected, which may arise when combining GCC noreturn attribute and including C11 stdnoreturn.h *) */
|   NORETURN                            { VARIABLE ("__noreturn__") }
    /*(* The NAMED_TYPE here creates conflicts with IDENT *)*/
|   NAMED_TYPE				{ VARIABLE (fst $1) }
|   LPAREN attr RPAREN                  { $2 }
|   IDENT IDENT                          { CALL(VARIABLE (fst $1), [VARIABLE (fst $2)]) }
|   CST_INT                              { CONSTANT(CONST_INT (fst $1)) }
|   string_constant                      { let q,t,l = $1 in CONSTANT(CONST_STRING (queue_to_string q, NO_ENCODING)) }
                                           /*(* Const when it appears in
                                            * attribute lists, is translated
                                            * to aconst *)*/
|   CONST                                { VARIABLE "aconst" }

|   IDENT COLON CST_INT                  { VARIABLE (fst $1 ^ ":" ^ fst $3) }

/*(* The following rule conflicts with the ? : attributes. We give it a very
   * low priority *)*/
|   CST_INT COLON CST_INT                { VARIABLE (fst $1 ^ ":" ^ fst $3) }

|   DEFAULT COLON CST_INT                { VARIABLE ("default:" ^ fst $3) }

                                            /*(** GCC allows this as an
                                             * attribute for functions,
                                             * synonim for noreturn **)*/
|   VOLATILE                             { VARIABLE ("__noreturn__") }
;

postfix_attr:
    primary_attr                         { $1 }
                                         /* (* use a VARIABLE "" so that the
                                             * parentheses are printed *) */
|   IDENT LPAREN  RPAREN             { CALL(VARIABLE (fst $1), [VARIABLE ""]) }
|   IDENT paren_attr_list_ne         { CALL(VARIABLE (fst $1), $2) }

|   postfix_attr ARROW id_or_typename    {MEMBEROFPTR ($1, $3)}
|   postfix_attr DOT id_or_typename      {MEMBEROF ($1, $3)}
|   postfix_attr LBRACKET attr RBRACKET  {INDEX ($1, $3) }
;

/*(* Since in attributes we use both IDENT and NAMED_TYPE as indentifiers,
 * that leads to conflicts for SIZEOF and ALIGNOF. In those cases we require
 * that their arguments be expressions, not attributes *)*/
unary_attr:
    postfix_attr                         { $1 }
|   SIZEOF unary_expression              { EXPR_SIZEOF (fst $2) }
|   REAL unary_expression                { REAL (fst $2) }
|   IMAG unary_expression                { IMAG (fst $2) }
|   CLASSIFYTYPE unary_expression        { CLASSIFYTYPE (fst $2) }
|   SIZEOF LPAREN type_name RPAREN
		                         {let b, d = $3 in TYPE_SIZEOF (b, d)}

|   ALIGNOF unary_expression             {EXPR_ALIGNOF (fst $2) }
|   ALIGNOF LPAREN type_name RPAREN      {let b, d = $3 in TYPE_ALIGNOF (b, d)}
|   PLUS cast_attr                      {UNARY (PLUS, $2)}
|   MINUS cast_attr                     {UNARY (MINUS, $2)}
|   STAR cast_attr		        {UNARY (MEMOF, $2)}
|   AND cast_attr
	                                {UNARY (ADDROF, $2)}
|   EXCLAM cast_attr    	        {UNARY (NOT, $2)}
|   TILDE cast_attr                     {UNARY (BNOT, $2)}
;

cast_attr:
    unary_attr                           { $1 }
;

multiplicative_attr:
    cast_attr                           { $1 }
|   multiplicative_attr STAR cast_attr  {BINARY(MUL ,$1 , $3)}
|   multiplicative_attr SLASH cast_attr	  {BINARY(DIV ,$1 , $3)}
|   multiplicative_attr PERCENT cast_attr {BINARY(MOD ,$1 , $3)}
;


additive_attr:
    multiplicative_attr                 { $1 }
|   additive_attr PLUS multiplicative_attr  {BINARY(ADD ,$1 , $3)}
|   additive_attr MINUS multiplicative_attr {BINARY(SUB ,$1 , $3)}
;

shift_attr:
    additive_attr                       { $1 }
|   shift_attr INF_INF additive_attr	{BINARY(SHL ,$1 , $3)}
|   shift_attr SUP_SUP additive_attr	{BINARY(SHR ,$1 , $3)}
;

relational_attr:
    shift_attr                          { $1 }
|   relational_attr INF shift_attr	{BINARY(LT ,$1 , $3)}
|   relational_attr SUP shift_attr	{BINARY(GT ,$1 , $3)}
|   relational_attr INF_EQ shift_attr	{BINARY(LE ,$1 , $3)}
|   relational_attr SUP_EQ shift_attr	{BINARY(GE ,$1 , $3)}
;

equality_attr:
    relational_attr                     { $1 }
|   equality_attr EQ_EQ relational_attr	    {BINARY(EQ ,$1 , $3)}
|   equality_attr EXCLAM_EQ relational_attr {BINARY(NE ,$1 , $3)}
;


bitwise_and_attr:
    equality_attr                       { $1 }
|   bitwise_and_attr AND equality_attr	{BINARY(BAND ,$1 , $3)}
;

bitwise_xor_attr:
    bitwise_and_attr                       { $1 }
|   bitwise_xor_attr CIRC bitwise_and_attr {BINARY(XOR ,$1 , $3)}
;

bitwise_or_attr:
    bitwise_xor_attr                      { $1 }
|   bitwise_or_attr PIPE bitwise_xor_attr {BINARY(BOR ,$1 , $3)}
;

logical_and_attr:
    bitwise_or_attr                             { $1 }
|   logical_and_attr AND_AND bitwise_or_attr	{BINARY(AND ,$1 , $3)}
;

logical_or_attr:
    logical_and_attr                           { $1 }
|   logical_or_attr PIPE_PIPE logical_and_attr {BINARY(OR ,$1 , $3)}
;

conditional_attr:
    logical_or_attr                        { $1 }
/* This is in conflict for now */
|   logical_or_attr QUEST conditional_attr COLON conditional_attr
                                          { QUESTION($1, $3, $5) }


attr: conditional_attr                    { $1 }
;

attr_list_ne:
|  attr                                  { [$1] }
|  attr COMMA attr_list_ne               { $1 :: $3 }
|  error COMMA attr_list_ne              { $3 }
;
attr_list:
  /* empty */                            { [] }
| attr_list_ne                           { $1 }
;
paren_attr_list_ne:
   LPAREN attr_list_ne RPAREN            { $2 }
|  LPAREN error RPAREN                   { [] }
;
paren_attr_list:
   LPAREN attr_list RPAREN               { $2 }
|  LPAREN error RPAREN                   { [] }
;
/*** GCC ASM instructions ***/
asmattr:
     /* empty */                        { [] }
|    VOLATILE  asmattr                  { ("volatile", []) :: $2 }
|    CONST asmattr                      { ("const", []) :: $2 }
;
asmtemplate:
    one_string_constant                          { [$1] }
|   one_string_constant asmtemplate              { $1 :: $2 }
;
asmoutputs:
  /* empty */           { None }
| COLON asmoperands asminputs
                        { let (ins, clobs) = $3 in
                          Some {aoutputs = $2; ainputs = ins; aclobbers = clobs} }
;
asmoperands:
     /* empty */                        { [] }
|    asmoperandsne                      { List.rev $1 }
;
asmoperandsne:
     asmoperand                         { [$1] }
|    asmoperandsne COMMA asmoperand     { $3 :: $1 }
;

asmoperand:
     asmopname string_constant LPAREN expression RPAREN    { let q,t,l = $2 in ($1, queue_to_string q, fst $4) }
|    asmopname string_constant LPAREN error RPAREN         { let q,t,l = $2 in ($1, queue_to_string q, NOTHING ) }
;

asminputs:
  /* empty */                { ([], []) }
| COLON asmoperands asmclobber
                        { ($2, $3) }
;
asmopname:
    /* empty */                         { None }
|   LBRACKET IDENT RBRACKET             { Some (fst $2) }
;

asmclobber:
    /* empty */                         { [] }
| COLON asmcloberlst                    { $2 }
;
asmcloberlst:
    /* empty */                         { [] }
| asmcloberlst_ne                       { $1 }
;
asmcloberlst_ne:
   one_string_constant                           { [$1] }
|  one_string_constant COMMA asmcloberlst_ne     { $1 :: $3 }
;

%%
