/*(* NOTE: This parser is based on a parser written by Hugues Casse. Since
   * then I have changed it in numerous ways to the point where it probably
   * does not resemble Hugues's original one at all  *)*/
%{ 
open Cabs
module E = Errormsg

let parse_error msg : 'a =           (* sm: c++-mode highlight hack: -> ' <- *)
  Errormsg.hadErrors := true;
  Cxxlexer.display_error
    msg
    (Parsing.symbol_start ()) (Parsing.symbol_end ())

let print = print_string


let currentLoc () = { lineno = !Cxxlexer.currentLine; 
                      filename = !Cxxlexer.currentFile;}


(*
** Expression building
*)
let smooth_expression lst =
  match lst with
    [] -> NOTHING
  | [expr] -> expr
  | _ -> COMMA (lst)


let currentFunctionName = ref "<outside any function>"
    
let announceFunctionName _ =
  let n = "<announce function name>" in
  let decl = JUSTBASE in
  Cxxlexer.add_identifier n;
  (* Start a context that includes the parameter names and the whole body. 
   * Will pop when we finish parsing the function body *)
  Cxxlexer.push_context ();
  (* Go through all the parameter names and mark them as identifiers *)
  let rec findProto = function
      PROTO (d, args, _, _) when isJUSTBASE d -> 
        List.iter (fun (_, (an, _, _)) -> Cxxlexer.add_identifier an) args

    | PROTO (d, _, _, _) -> findProto d
    | PARENTYPE (_, d, _) -> findProto d
    | PTR (_, d) -> findProto d
    | ARRAY (d, _) -> findProto d
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

let dummyName = 
  ("<this is a dummy name>", JUSTBASE, [])

let doDeclaration (loc: cabsloc) (specs: spec_elem list) 
    (nl: init_name list) : definition = 
  (* In C++ we must also take the names of structs and unions and make them 
   * type names *)
  let rec findStructNames = function
      [] -> ()
    | SpecType (Tstruct (n, _)) :: rest -> Cxxlexer.add_type n
    | SpecType (Tunion (n, _)) :: rest -> Cxxlexer.add_type n
    | _ :: rest -> findStructNames rest
  in
  if !Cprint.cxxMode then findStructNames specs;
  if isTypedef specs then begin
    (* Tell the lexer about the new type names *)
    List.iter (fun ((n, _, _), _) -> Cxxlexer.add_type n) nl;
    TYPEDEF ((specs, List.map (fun (n, _) -> n) nl), loc)
  end else
    if nl = [] then
      ONLYTYPEDEF (specs, loc)
    else begin
      (* Tell the lexer about the new variable names *)
      List.iter (fun ((n, _, _), _) -> Cxxlexer.add_identifier n) nl;
      DECDEF ((specs, nl), loc)  
    end


let doFunctionDef (loc: cabsloc)
                  (specs: spec_elem list) 
                  (b: block) : definition = 
  let fname = (specs, dummyName) in
  FUNDEF (fname, b, loc)


let doOldParDecl (names: string list)
                 ((pardefs: name_group list), (isva: bool)) 
    : single_name list * bool =
  let findOneName n =
    (* Search in pardefs for the definition for this parameter *)
    let rec loopGroups = function
        [] -> ([SpecType Tint], (n, JUSTBASE, []))
      | (specs, names) :: restgroups ->
          let rec loopNames = function
              [] -> loopGroups restgroups
            | ((n',_, _) as sn) :: _ when n' = n -> (specs, sn)
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


%}

%token <string> IDENT TILDE_IDENT
%token <string> CST_CHAR
%token <string> CST_INT
%token <string> CST_FLOAT
%token <string> CST_STRING
%token <string> CST_WSTRING
%token <string> NAMED_TYPE
%token <string> NAMED_CLASS
%token <string> NAMED_NAMESPACE
%token <string> NAMED_ENUM

%token EOF
%token CHAR INT DOUBLE FLOAT VOID INT64 INT32
%token ENUM STRUCT TYPEDEF UNION
%token SIGNED UNSIGNED LONG SHORT
%token VOLATILE EXTERN STATIC CONST RESTRICT AUTO REGISTER

%token SIZEOF ALIGNOF

%token EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
%token AND_EQ PIPE_EQ CIRC_EQ INF_INF_EQ SUP_SUP_EQ
%token ARROW DOT

%token EQ_EQ EXCLAM_EQ INF SUP INF_EQ SUP_EQ
%token PLUS MINUS STAR SLASH PERCENT
%token TILDE AND PIPE CIRC
%token EXCLAM AND_AND PIPE_PIPE
%token INF_INF SUP_SUP
%token PLUS_PLUS MINUS_MINUS

%token RPAREN LPAREN RBRACE LBRACE LBRACKET RBRACKET
%token COLON SEMICOLON COMMA ELLIPSIS QUEST

%token BREAK CONTINUE GOTO RETURN
%token SWITCH CASE DEFAULT
%token WHILE DO FOR
%token IF ELSE

%token ATTRIBUTE INLINE ASM TYPEOF FUNCTION__ PRETTY_FUNCTION__ LABEL__
%token BUILTIN_VA_ARG BUILTIN_VA_LIST
%token BLOCKATTRIBUTE
%token DECLSPEC
%token <string> MSASM MSATTR
%token PRAGMA END_PRAGMA
%token DD_NOTHROW IDE_INF INDE_NOINF

/* sm: cabs tree transformation specification keywords */
%token AT_TRANSFORM AT_TRANSFORMEXPR AT_SPECIFIER AT_EXPR AT_NAME


/* C++ keywords */
%token BOOL CATCH CLASS CONST_CLASS DELETE DYNAMIC_CAST EXPLICIT
%token EXPORT FALSE FRIEND MUTABLE NAMESPACE NEW OPERATOR PRIVATE PROTECTED
%token PUBLIC REINTERPRET_CAST STATIC_CAST TEMPLATE THIS THROW TRUE TRY
%token TYPEID TYPENAME USING VIRTUAL 
%token COLON_COLON

%token DOT_STAR ARROW_STAR

/* Conflict resolution tokens */
%token CR_a0 CR_a1 CR_a2 CR_a3 CR_a4 CR_a5 CR_a6 CR_a7 CR_a8 CR_a9  
%token CR_b0 CR_b1 CR_b2 CR_b3 CR_b4 CR_b5 CR_b6 CR_b7 CR_b8 CR_b9
%token CR_c0 CR_c1 CR_c2 CR_c3 CR_c4 CR_c5 CR_c6 CR_c7 CR_c8 CR_c9  


/* Non-terminals informations */
%start file

%type <Cabs.definition list> file

/* (* ELSE is right associative to solve the dangling ELSE ambiguity *)*/
%right ELSE

/* (* We have the operator new[] ambiguity. We solve by shifting the LBRACKET 
    * *)*/
%left NEW DELETE
%left LBRACKET

/* (* Sometimes we have attributes after a function declarator. We don't know 
    * if they belong to the function or to the declared type. Shift them 
    * anyway. *) */
%left DD_NOTHROW
/* (* The attribute specifiers are right associative to ensure the we keep 
    * shifting them *) */
%right VOLATILE CONST RESTRICT ATTRIBUTE DECLSPEC MSATTR

/* (* :: has very high precedence so that we shift it always *) */
/* %nonassoc IDENT NAMED_TYPE
   %nonassoc COLON_COLON
*/
%right COLON_COLON
%right IDENT TILDE_IDENT TEMPLATE

%nonassoc IDE_NOINF
%right INF

%%


location:
   /* empty */                	{ currentLoc () }  %prec IDENT

/* (* ****************** *) */
literal:
    CST_INT				{CONST_INT $1}
|   CST_FLOAT				{CONST_FLOAT $1}
|   CST_CHAR				{CONST_CHAR $1}
|   string_list				{CONST_STRING $1}
|   wstring_list			{CONST_WSTRING $1}
|   TRUE                                {CONST_BOOL true}
|   FALSE                               {CONST_BOOL false}
;    

string_list:
    one_string                          { $1 }
|   string_list one_string              { $1 ^ $2 }
;
wstring_list:
    CST_WSTRING                         { $1 }
|   wstring_list one_string             { $1 ^ $2 }
|   wstring_list CST_WSTRING            { $1 ^ $2 }
/* Only the first string in the list needs an L, so L"a" "b" is the same
 * as L"ab" or L"a" L"b". */

one_string: 
    CST_STRING				{$1}
|   FUNCTION__                          {!currentFunctionName}
|   PRETTY_FUNCTION__                   {!currentFunctionName}
;    



virtual_opt: 
  /* empty */      { () }  
| VIRTUAL  { () }
;


semicolon_opt: 
  /* empty */      { () }  
| SEMICOLON  { () }
;

identifier_opt: 
  /* empty */      { () }  
| IDENT  { () }
;




/* (* 1.3  Basic concepts                              [gram.basic] *) */
file:
| declaration_seq_opt EOF  { [] }

/* (* 1.4  Expressions                                     [gram.expr] *) */

primary_expression:
  literal                                    { () }
| THIS                                       { () }
| LPAREN expression RPAREN                   { () }
| id_expression                              { () }
;

/* (* A sequence of IDENT and TEMPLATE IDENT and TILDE_IDENT separated by 
    * COLON_COLON and optionally preceeded by COLON_COLON. Always ended by 
    * IDENT or TEMPLATE ID or  TILDE_IDENT. *)  
    *) */
nn_spec_id: 
|  COLON_COLON                        { "::" }
|  COLON_COLON nn_spec_id_no_col      { "::" ^ $2 }
|              nn_spec_id_no_col      { $1 }
;
nn_spec_id_no_col: 
|  IDENT  nn_spec_id_col              { $1 ^ $2 }
|  TILDE_IDENT nn_spec_id_col         { $1 ^ $2 }
|  TEMPLATE nn_spec_id_no_col         { "template" ^ $2}
;
nn_spec_id_col:
   /* empty */                        { "" }  %prec COLON_COLON
|  COLON_COLON                        { "::" }
|  COLON_COLON nn_spec_id_no_col      { "::" ^ $2}
;

id_expression:
  nn_spec_id                                             { () } %prec IDE_NOINF
| nn_spec_id INF template_argument_list SUP              { () } %prec IDE_INF
|                        operator_function_id            { () }
| nn_spec_id operator_function_id            { () }
|                        conversion_function_id          { () }
| nn_spec_id conversion_function_id          { () }
;



postfix_expression:
  primary_expression                                   { () }
| postfix_expression LBRACKET expression RBRACKET      { () }
| postfix_expression LPAREN expression RPAREN           { () }
| simple_type_specifier LPAREN expression_opt RPAREN    { () }
| TYPENAME nn_spec_id LPAREN expression_list_opt RPAREN    { () }
| TYPENAME nn_spec_id INF template_argument_list SUP 
                        LPAREN expression_list_opt RPAREN    { () }
| postfix_expression DOT          id_expression    { () }
| postfix_expression ARROW        id_expression    { () }
| postfix_expression PLUS_PLUS    { () }
| postfix_expression MINUS_MINUS    { () }
| DYNAMIC_CAST INF type_id SUP LPAREN expression RPAREN    { () }
| STATIC_CAST INF type_id SUP LPAREN expression RPAREN    { () }
/* (* missing *) */
| TYPEID LPAREN expression RPAREN    { () }
;


expression_list:
  assignment_expression    { () }
| expression_list COMMA assignment_expression    { () }
;

expression_list_opt: 
  /* empty */      { () }  
| expression_list  { () }
;


unary_expression:
  postfix_expression    { () }
| PLUS_PLUS cast_expression    { () }
| MINUS_MINUS cast_expression    { () }
| STAR cast_expression    { () }
| AND cast_expression    { () }
| PLUS cast_expression    { () }
| MINUS cast_expression    { () }
| EXCLAM cast_expression    { () }
| TILDE cast_expression    { () }
| SIZEOF unary_expression    { () }
| SIZEOF LPAREN type_id RPAREN    { () }
| new_expression    { () }
| delete_expression    { () }
/* (* GCC extensions *) */
| ALIGNOF unary_expression   { () }
| ALIGNOF LPAREN type_id RPAREN { () }
| LPAREN compound_statement RPAREN    { () }
| BUILTIN_VA_ARG LPAREN assignment_expression COMMA type_id RPAREN { () }
/* (* We handle GCC constructor expressions *) */
| LPAREN type_id RPAREN LBRACE initializer_list_opt RBRACE
		                { () }
/* (* GCC's address of labels *)  */
|  AND_AND IDENT                { () }
;

new_expression:
              NEW new_placement new_type_id new_initializer_opt    { () }
| COLON_COLON NEW new_placement new_type_id new_initializer_opt    { () }
|             NEW new_placement LPAREN type_id RPAREN new_initializer_opt    { () }
| COLON_COLON NEW new_placement LPAREN type_id RPAREN new_initializer_opt    { () }
;

new_placement:
  LPAREN expression_list RPAREN    { () }
;

new_initializer_opt:
  LPAREN expression_list RPAREN    { () }
| /* empty */   { () }
;

new_type_id:
  declaration    { () }
;

delete_expression:
               DELETE cast_expression    { () }
|  COLON_COLON DELETE cast_expression    { () }
|              DELETE LBRACKET RBRACKET cast_expression    { () }
|  COLON_COLON DELETE LBRACKET RBRACKET cast_expression    { () }
;


cast_expression:
  unary_expression    { () }
| LPAREN type_id RPAREN cast_expression    { () }
;

pm_expression:
  cast_expression    { () }
| pm_expression DOT_STAR cast_expression    { () }
| pm_expression ARROW_STAR cast_expression    { () }
;


multiplicative_expression:
  pm_expression    { () }
| multiplicative_expression STAR pm_expression    { () }
| multiplicative_expression SLASH pm_expression    { () }
| multiplicative_expression PERCENT pm_expression    { () }
;

additive_expression:
| multiplicative_expression    { () }
| additive_expression PLUS multiplicative_expression    { () }
| additive_expression MINUS multiplicative_expression    { () }
;

shift_expression:
| additive_expression                             { () }
| shift_expression INF_INF additive_expression    { () }
| shift_expression SUP_SUP additive_expression    { () }
;

relational_expression:
| shift_expression    { () }
| relational_expression INF shift_expression    { () }
| relational_expression SUP shift_expression    { () }
| relational_expression INF_EQ shift_expression    { () }
| relational_expression SUP_EQ shift_expression    { () }
;

equality_expression:
| relational_expression    { () }
| equality_expression EQ_EQ relational_expression    { () }
| equality_expression EXCLAM_EQ relational_expression    { () }
;

and_expression:
| equality_expression    { () }
| and_expression AND equality_expression    { () }
;

exclusive_or_expression:
| and_expression    { () }
| exclusive_or_expression CIRC and_expression    { () }
;


inclusive_or_expression:
| exclusive_or_expression    { () }
| inclusive_or_expression PIPE exclusive_or_expression    { () }
;


logical_and_expression:
  inclusive_or_expression    { () }
| logical_and_expression AND_AND inclusive_or_expression    { () }
;

logical_or_expression:
| logical_and_expression    { () }
| logical_or_expression PIPE_PIPE logical_and_expression    { () }
;

conditional_expression:
| logical_or_expression    { () }
| logical_or_expression QUEST expression COLON assignment_expression    { () }
| logical_or_expression QUEST            COLON assignment_expression    { () }
;

assignment_expression:
  conditional_expression    { () }
| logical_or_expression EQ assignment_expression    { () }
| logical_or_expression STAR_EQ assignment_expression    { () }
| logical_or_expression SLASH_EQ assignment_expression    { () }
| logical_or_expression PERCENT_EQ assignment_expression    { () }
| logical_or_expression PLUS_EQ assignment_expression    { () }
| logical_or_expression MINUS_EQ assignment_expression    { () }
| logical_or_expression SUP_SUP_EQ assignment_expression    { () }
| logical_or_expression INF_INF_EQ assignment_expression    { () }
| logical_or_expression AND_EQ assignment_expression    { () }
| logical_or_expression CIRC_EQ assignment_expression    { () }
| logical_or_expression PIPE_EQ assignment_expression    { () }
| throw_expression    { () }
;


expression:
| assignment_expression    { () }
| expression COMMA assignment_expression    { () }
;

expression_opt: 
  /* empty */      { () }  
| expression  { () }
;

constant_expression:
    conditional_expression    { () }
;

constant_expression_opt: 
  /* empty */      { () }  
| constant_expression  { () }
;

/* (* 1.5  Statements                      [gram.stmt.stmt]  *) */
statement:
| labeled_statement  { () }
| expression_statement  { () }
| compound_statement  { () }
| selection_statement  { () }
| iteration_statement  { () }
| jump_statement  { () }
| declaration_statement  { () } 
| try_block  { () }
| location ASM asmattr LPAREN asmtemplate asmoutputs RPAREN SEMICOLON
                               { () } 
| location MSASM               { () } 
| location error   SEMICOLON   { () }

labeled_statement:
| location IDENT COLON statement  { () }
| location CASE constant_expression COLON statement  { () }
| location CASE constant_expression ELLIPSIS constant_expression COLON
	                         { () } 
| location DEFAULT COLON statement  { () }
;

expression_statement:
   location expression_opt SEMICOLON  { () }
;

compound_statement:
| location LBRACE local_labels block_attrs statement_seq_opt RBRACE  { () }
;

/* (* GCC extensions *) */
block_attrs:
   /* empty */                                              { [] }
|  BLOCKATTRIBUTE paren_attr_list_ne
                                        { [("__blockattribute__", $2)] }
;

local_labels: 
   /* empty */                                       { [] }
|  LABEL__ local_label_names SEMICOLON local_labels  { $2 @ $4 }
;
local_label_names: 
   IDENT                                 { [ $1 ] }
|  IDENT COMMA local_label_names         { $1 :: $3 }
;

statement_seq:
| statement  { () }
| statement_seq statement  { () }
;

statement_seq_opt: 
  /* empty */      { () }  
| statement_seq  { () }
;

selection_statement:
| location IF LPAREN condition RPAREN statement  { () }  %prec ELSE
| location IF LPAREN condition RPAREN statement ELSE statement  { () }
| location SWITCH LPAREN condition RPAREN statement  { () }
;

condition:
| expression  { () }
| type_specifier_seq declarator EQ assignment_expression  { () }
;

condition_opt: 
  /* empty */      { () }  
| condition  { () }
;

iteration_statement:
| location WHILE LPAREN condition RPAREN statement  { () }
| location DO statement WHILE LPAREN expression RPAREN SEMICOLON  { () }
| location FOR LPAREN for_init_statement condition_opt SEMICOLON 
                         expression_opt RPAREN statement  { () }
;

for_init_statement:
| expression_statement  { () }
| simple_declaration  { () }
;

jump_statement:
| location BREAK SEMICOLON  { () }
| location CONTINUE SEMICOLON  { () }
| location RETURN expression_opt SEMICOLON  { () }
| location GOTO IDENT SEMICOLON  { () }
| location GOTO STAR expression SEMICOLON 
                                 { () } 
;

  
declaration_statement:
  block_declaration  { () }
;

/* (*  1.6  Declarations                                [gram.dcl.dcl]  *) */
declaration_seq:
| declaration  { () }
| declaration_seq declaration  { () }
;
declaration_seq_opt:
  /* empty */                   { () }
| declaration_seq                { () }
;

declaration:
| block_declaration  { () }
| function_definition  { () }
| template_declaration  { () }
| explicit_instantiation  { () }
| explicit_specialization  { () }
| linkage_specification  { () }
| namespace_definition  { () }
| location PRAGMA      END_PRAGMA { () }
| location PRAGMA attr END_PRAGMA { () }
;

block_declaration:
| simple_declaration  { () }
/*(* This ASM definition is a statement *)*/
| CR_a3 asm_definition  { () }
| namespace_alias_definition  { () }
| using_declaration  { () }
| using_directive  { () }
;

simple_declaration:
| location decl_specifier_seq_opt init_declarator_list_opt SEMICOLON  { () }
;

decl_specifier:
| storage_class_specifier  { () }
| type_specifier  { () }
| function_specifier  { () }
| FRIEND  { () }
| TYPEDEF  { () }

decl_specifier_seq:
| decl_specifier_seq_opt decl_specifier  { () }
;
  
decl_specifier_seq_opt: 
  /* empty       { () }  */
| decl_specifier_seq  CR_c1 { () }
;

storage_class_specifier:
| AUTO  { () }
| REGISTER  { () }
| STATIC  { () }
| EXTERN  { () }
| MUTABLE  { () }
;
 
function_specifier:
| INLINE  { () }
| VIRTUAL  { () }
| EXPLICIT  { () }
;

 
type_specifier:
| simple_type_specifier  { () }
| class_specifier  { () }
| enum_specifier  { () }
| elaborated_type_specifier  { () }
/* (* | cv_qualifier  { () }  Attributes take care of this *) */


simple_type_specifier:
| nn_spec_id CR_c2 { () }
| CHAR  { () }
/* (* missing wchar_t *) */  
| BOOL  { () }
| SHORT  { () }
| INT  { () }
| LONG  { () }
| SIGNED  { () }
| UNSIGNED  { () }
| FLOAT  { () }
| DOUBLE  { () }
| VOID  { () }
| INT64 { () }
;

elaborated_type_specifier:
| class_key nn_spec_id  { () }
| ENUM nn_spec_id { () }
| TYPENAME nn_spec_id  { () }
| TYPENAME nn_spec_id INF template_argument_list SUP  { () }
| TYPEOF LPAREN expression RPAREN     { () } 
| TYPEOF LPAREN type_id RPAREN        { () } 
;


enum_specifier:
| ENUM identifier_opt LBRACE enumerator_list_opt RBRACE  { () }
;

enumerator_list:
| enumerator_definition  { () }
| enumerator_list COMMA enumerator_definition  { () }
;

enumerator_list_opt: 
  /* empty */      { () }  
| enumerator_list  { () }
;

enumerator_definition:
| IDENT                         { () }
| IDENT EQ constant_expression  { () }
;


namespace_definition:
| location NAMESPACE IDENT LBRACE namespace_body RBRACE  { () }
| location NAMESPACE       LBRACE namespace_body RBRACE  { () }
;

namespace_body:
| declaration_seq_opt  { () }
;

  

namespace_alias_definition:
| location NAMESPACE IDENT EQ nn_spec_id SEMICOLON  { () }
;

  
using_declaration:
| location USING              nn_spec_id SEMICOLON  { () }
| location USING              nn_spec_id 
                     INF template_argument_list SUP SEMICOLON  { () }
| location USING TYPENAME     nn_spec_id SEMICOLON  { () }
| location USING TYPENAME     nn_spec_id 
                     INF template_argument_list SUP SEMICOLON  { () }
;

using_directive:
| location USING NAMESPACE nn_spec_id SEMICOLON  { () }
;

asm_definition:
  location ASM LPAREN string_list RPAREN SEMICOLON  { () }
;

linkage_specification:
| location EXTERN one_string LBRACE declaration_seq_opt RBRACE  { () }
| location EXTERN one_string declaration  { () }
;

/* (* 1.7  Declarators                                [gram.dcl.decl]  *) */
init_declarator_list:
| init_declarator  { () }
| init_declarator_list COMMA init_declarator  { () }
;

init_declarator_list_opt: 
  /* empty */      { () }  
| init_declarator_list  { () }
;
  
init_declarator:
| declarator initializer_opt  { () }
;
declarator:
| ptr_operators_opt direct_declarator attributes_with_asm CR_c3 { () }
;

direct_declarator:
| declarator_id  { () }
| direct_declarator LPAREN parameter_declaration_clause RPAREN 
                                { () } %prec DD_NOTHROW
| direct_declarator 
     LPAREN parameter_declaration_clause RPAREN 
           attributes THROW LPAREN type_id_list_opt RPAREN  { () }
| direct_declarator LBRACKET constant_expression_opt RBRACKET  { () }
| LPAREN attributes declarator RPAREN  { () }
;

ptr_operators: 
| STAR attributes ptr_operators_opt          { () }
| AND attributes ptr_operators_opt           { () }
| nn_spec_id STAR attributes ptr_operators_opt  { () }
;
ptr_operators_opt:
   CR_c4 /* empty */                 { () }
|  ptr_operators                     { $1 }
;
  

declarator_id:
|     id_expression   { () }
;
 
type_id:
| type_specifier_seq abstract_declarator_opt  { () }
;
type_specifier_seq:
| type_specifier type_specifier_seq_opt  CR_c5 { () }
;

type_specifier_seq_opt: 
  /* empty */         { () }  
| type_specifier_seq  { () }
;

abstract_declarator:
| ptr_operators                                           { () }
| ptr_operators_opt direct_abstract_declarator attributes { () }
;

abstract_declarator_opt: 
  /* empty */      { () }  
| abstract_declarator  { () }
;

direct_abstract_declarator:
| direct_abstract_declarator_opt 
           LPAREN parameter_declaration_clause RPAREN  { () } %prec DD_NOTHROW
| direct_abstract_declarator_opt 
           LPAREN parameter_declaration_clause RPAREN 
               attributes 
               THROW LPAREN type_id_list_opt RPAREN  { () }
| direct_abstract_declarator_opt LBRACKET constant_expression_opt RBRACKET  
               { () }
| LPAREN attributes abstract_declarator RPAREN  { () }
;

direct_abstract_declarator_opt: 
  CR_c6 /* empty */      { () }  
| direct_abstract_declarator { () }
;
  
parameter_declaration_clause:
| parameter_declaration_list_opt               { () }
| parameter_declaration_list_opt ELLIPSIS  { () }
| parameter_declaration_list COMMA ELLIPSIS  { () }
;

parameter_declaration_list: 
| parameter_declaration  { () }
| parameter_declaration_list COMMA parameter_declaration  { () }
;

parameter_declaration_list_opt: 
  /* empty */      { () }  
| parameter_declaration_list { () }
;

parameter_declaration:
| decl_specifier_seq declarator  { () }
| decl_specifier_seq declarator EQ assignment_expression  { () }
| decl_specifier_seq abstract_declarator_opt  { () }
| decl_specifier_seq abstract_declarator_opt EQ assignment_expression  { () }
;

          
function_definition:
| location decl_specifier_seq_opt declarator 
                   ctor_initializer_opt function_body  { () }
| location decl_specifier_seq_opt declarator 
                   function_try_block  { () }
;


function_body:
| compound_statement  { () }
;
 
initializer_a:
| EQ initializer_clause             { () }
| LPAREN expression_list RPAREN     { () } 
;

initializer_opt: 
  /* empty */      { () }  
| initializer_a      { () }
;

initializer_clause:
| assignment_expression  { () }
| LBRACE initializer_list RBRACE  { () }
| LBRACE RBRACE  { () }
;

/* (* GCC and ISO C initializers with designators *) */
initializer_list:    /* ISO 6.7.8. Allow a trailing COMMA */
    d_initializer                             { [$1] }
|   d_initializer COMMA initializer_list_opt  { $1 :: $3 }
;

initializer_list_opt:
    /* empty */                             { [] }
|   initializer_list                        { $1 }
;
d_initializer: 
|                           initializer_clause { (NEXT_INIT, $1) }
|   init_designators eq_opt initializer_clause { ($1, $3) }
|   gcc_init_designators    initializer_clause { ($1, $2) }
;
eq_opt: 
   EQ                        { () }
   /*(* GCC allows missing = *)*/
|  /*(* empty *)*/               { () }
;
init_designators: 
    DOT     IDENT  init_designators_opt      { INFIELD_INIT($2, $3) }
|   LBRACKET  expression RBRACKET init_designators_opt
                                        { ATINDEX_INIT(NOTHING (* $2 *), $4) }
|   LBRACKET  expression ELLIPSIS expression RBRACKET
                                        { ATINDEXRANGE_INIT(NOTHING, 
                                                            NOTHING) }
;         
init_designators_opt:
   /* empty */                          { NEXT_INIT }
|  init_designators                     { $1 }
;

gcc_init_designators:  /*(* GCC supports these strange things *)*/
   IDENT COLON                 { INFIELD_INIT($1, NEXT_INIT) }
;


/* (* 1.8  Classes                                   [gram.class]  *) */

class_specifier:
| class_head LBRACE member_specification_opt RBRACE  { () }
;

class_head:
| class_key            base_clause_opt  { () }
| class_key nn_spec_id base_clause_opt  { () }

;
class_key:
  CLASS  { () }
| STRUCT  { () }
| UNION  { () }
;

member_specification:
| member_declaration member_specification_opt  { () }
| access_specifier COLON member_specification_opt  { () }
;

member_specification_opt: 
  /* empty */      { () }  
| member_specification { () }
;

member_declaration:
| decl_specifier_seq_opt member_declarator_list_opt SEMICOLON   { () }
| function_definition semicolon_opt { () }
/* | qualified_id SEMICOLON  { () } */
| using_declaration  { () }
| template_declaration  { () }
;

 
member_declarator_list:
| member_declarator { () }
| member_declarator_list COMMA member_declarator { () }
;

member_declarator_list_opt: 
  /* empty */      { () }  
| member_declarator_list { () }
;

member_declarator:
/* (* The pure specifier is treated as a constant_initializer *) */
| declarator constant_initializer_opt { () }
| IDENT COLON constant_expression { () }
|       COLON constant_expression { () }
;

constant_initializer:
| EQ constant_expression { () }
;


constant_initializer_opt: 
  /* empty */      { () }  
| constant_initializer  { () }
;

/* (* 1.9  Derived classes                   [gram.class.derived]  *) */

base_clause:
| COLON base_specifier_list  { () }
;

base_clause_opt: 
  /* empty */      { () }  
| base_clause { () }
;

base_specifier_list:
| base_specifier  { () }
| base_specifier_list COMMA base_specifier  { () }
;


base_specifier:
| nn_spec_id { () }
| VIRTUAL access_specifier_opt nn_spec_id  { () }
| access_specifier virtual_opt nn_spec_id  { () }
;


access_specifier:
  PRIVATE  { () }
| PROTECTED  { () }
| PUBLIC  { () }
;

access_specifier_opt: 
  /* empty */      { () }  
| access_specifier { () }
;

/* (* 1.10  Special member functions                  [gram.special] *) */

conversion_function_id:
| OPERATOR conversion_type_id  { () }
;

conversion_type_id:
    type_specifier_seq conversion_declarator  { () }
;
conversion_declarator:
   ptr_operators_opt             { () }
;

ctor_initializer:
| COLON mem_initializer_list  { () }
;

ctor_initializer_opt: 
  /* empty */      { () }  
|  ctor_initializer { () }
;

mem_initializer_list:
| mem_initializer  { () }
| mem_initializer COMMA mem_initializer_list  { () }
;

mem_initializer:
| mem_initializer_id LPAREN expression_list_opt RPAREN  { () }
;

mem_initializer_id:
| nn_spec_id { () }
;

/* (* 1.11  Overloading                            [gram.over] *) */
operator_function_id:
| OPERATOR NEW                    { () }  
| OPERATOR NEW LBRACKET RBRACKET  { () }
| OPERATOR DELETE  { () }
| OPERATOR DELETE LBRACKET RBRACKET  { () }
| OPERATOR PLUS  { () }
| OPERATOR MINUS  { () }
| OPERATOR STAR  { () }
| OPERATOR SLASH  { () }
| OPERATOR PERCENT  { () }
| OPERATOR CIRC  { () }
| OPERATOR AND  { () }
| OPERATOR PIPE  { () }
| OPERATOR TILDE  { () }
| OPERATOR EXCLAM  { () }
| OPERATOR EQ  { () }
| OPERATOR INF  { () }
| OPERATOR SUP  { () }
| OPERATOR PLUS_EQ  { () }
| OPERATOR MINUS_EQ  { () }
| OPERATOR STAR_EQ  { () }
| OPERATOR SLASH_EQ  { () }
| OPERATOR PERCENT_EQ  { () }
| OPERATOR CIRC_EQ  { () }
| OPERATOR AND_EQ  { () }
| OPERATOR PIPE_EQ  { () }
| OPERATOR INF_INF  { () }
| OPERATOR SUP_SUP  { () }
| OPERATOR INF_INF_EQ  { () }
| OPERATOR SUP_SUP_EQ  { () }
| OPERATOR EQ_EQ  { () }
| OPERATOR EXCLAM_EQ  { () }
| OPERATOR INF_EQ  { () }
| OPERATOR SUP_EQ  { () }
| OPERATOR AND_AND  { () }
| OPERATOR PIPE_PIPE  { () }
| OPERATOR PLUS_PLUS  { () }
| OPERATOR MINUS_MINUS  { () }
| OPERATOR COMMA  { () }
| OPERATOR ARROW_STAR  { () }
| OPERATOR ARROW  { () }
| OPERATOR LPAREN RPAREN  { () }
| OPERATOR LBRACKET RBRACKET  { () }


/* (* 1.12  Templates                                        [gram.temp] *) */
template_declaration:
| location        TEMPLATE 
                    INF template_parameter_list SUP declaration  { () }
| location EXPORT TEMPLATE 
                    INF template_parameter_list SUP declaration  { () }
;

template_parameter_list:
| template_parameter  { () }
| template_parameter_list COMMA template_parameter  { () }
;


template_parameter:
| type_parameter  { () }
| parameter_declaration  CR_c7 { () }
;

type_parameter:
| CLASS identifier_opt  { () }
| CLASS identifier_opt EQ type_id  { () }
| TYPENAME identifier_opt  { () }
| TYPENAME identifier_opt EQ type_id  { () }
| TEMPLATE INF template_parameter_list SUP CLASS identifier_opt  { () }
| TEMPLATE INF template_parameter_list SUP 
              CLASS identifier_opt EQ id_expression  { () }
;

       
template_argument_list:
| template_argument   { () }
| template_argument_list COMMA template_argument  { () }
;


template_argument:
| assignment_expression CR_c6 { () }
| type_id  { () } 
;

explicit_instantiation:
| location TEMPLATE declaration  { () }
;

explicit_specialization:
| location TEMPLATE INF SUP declaration  { () }

;

/* (* 1.13  Exception handling                     [gram.except]  *) */
try_block:
  location TRY compound_statement handler_seq  { () }
;
function_try_block:
  location TRY  ctor_initializer_opt function_body handler_seq  { () }
;

handler_seq:
| handler handler_seq_opt  { () }
;

handler_seq_opt: 
  /* empty */      { () }  
| handler_seq  { () }
;

handler:
  location CATCH LPAREN exception_declaration RPAREN compound_statement  { () }
;

exception_declaration:
| type_specifier_seq declarator  { () }
| type_specifier_seq abstract_declarator  { () }
| type_specifier_seq  { () }
| ELLIPSIS  { () }
;
 
throw_expression:
| THROW                            { () }
| THROW assignment_expression      { () }
;


type_id_list:
| type_id  { () }
| type_id_list COMMA  type_id  { () }
;

type_id_list_opt: 
  /* empty */      { () }  
| type_id_list  { () }
;


/*** GCC attributes ***/
attributes:
    /* empty */				{ []}	%prec VOLATILE
|   attribute attributes	        { $1 :: $2 }
;

/* (* In some contexts we can have an inline assembly to specify the name to 
    * be used for a global. We treat this as a name attribute *) */
attributes_with_asm:
    /* empty */                         { [] }
|   attribute attributes_with_asm       { $1 :: $2 }
|   ASM LPAREN string_list RPAREN attributes        
                                        { ("__asm__", 
                                           [CONSTANT(CONST_STRING $3)]) :: $5 }
;
 
attribute:
    ATTRIBUTE LPAREN paren_attr_list_ne RPAREN	
                                        { ("__attribute__", $3) }
|   DECLSPEC paren_attr_list_ne         { ("__declspec", $2) }
|   MSATTR                              { ($1, []) }
                                        /* ISO 6.7.3 */
|   CONST                               { ("const", []) }
|   RESTRICT                            { ("restrict",[]) }
|   VOLATILE                            { ("volatile",[]) }
;

/** (* PRAGMAS and ATTRIBUTES *) ***/
/* (* We want to allow certain strange things that occur in pragmas, so we 
    * cannot use directly the language of expressions *) */ 
attr: 
  logical_or_attr                            { $1 }
;
logical_or_attr: 
  logical_or_attr PIPE_PIPE logical_and_attr  {BINARY(OR ,$1 , $3)}
| logical_and_attr                            { $1 }
;
logical_and_attr:
  inclusive_or_attr                           { $1 }
| logical_and_attr AND_AND inclusive_or_attr  {BINARY(AND ,$1 , $3)}
;
inclusive_or_attr:
| exclusive_or_attr                           { $1 }
| inclusive_or_attr PIPE exclusive_or_attr    {BINARY(BOR ,$1 , $3)}
;
exclusive_or_attr:
| and_attr                                     { $1 }
| exclusive_or_attr CIRC and_attr              {BINARY(XOR ,$1 , $3)}
;
and_attr:
| equality_attr                                { $1 }
| and_attr AND equality_attr                   { BINARY(BAND ,$1 , $3) }
;
equality_attr:
| relational_attr                              { $1 }
| equality_attr EQ_EQ relational_attr          { BINARY(EQ ,$1 , $3) }
| equality_attr EXCLAM_EQ relational_attr      { BINARY(NE ,$1 , $3) }
;
relational_attr:
| shift_attr                                   { $1 }
| relational_attr INF shift_attr               { BINARY(LT ,$1 , $3) }
| relational_attr SUP shift_attr               { BINARY(GT ,$1 , $3) }
| relational_attr INF_EQ shift_attr            { BINARY(LE ,$1 , $3) }
| relational_attr SUP_EQ shift_attr            { BINARY(GE ,$1 , $3) }
;
shift_attr:
| additive_attr                                { $1 }
| shift_attr INF_INF additive_attr             { BINARY(SHL ,$1 , $3) }
| shift_attr SUP_SUP additive_attr             { BINARY(SHR ,$1 , $3) }
;
additive_attr:
| multiplicative_attr                       { $1 }
| additive_attr PLUS multiplicative_attr    { BINARY(ADD ,$1 , $3) }
| additive_attr MINUS multiplicative_attr   { BINARY(SUB ,$1 , $3) }
;
multiplicative_attr:
  unary_attr                                { $1 }
| multiplicative_attr STAR unary_attr       { BINARY(MUL ,$1 , $3) }
| multiplicative_attr SLASH unary_attr      { BINARY(DIV ,$1 , $3) }
| multiplicative_attr PERCENT unary_attr    { BINARY(MOD ,$1 , $3) }
;
unary_attr:
  postfix_attr                              { $1 }
| STAR unary_attr                           { UNARY (MEMOF, $2) }
| AND unary_attr                            { UNARY (ADDROF, $2) }
| PLUS unary_attr                           { UNARY (PLUS, $2) }
| MINUS unary_attr                          { UNARY (MINUS, $2) }
| EXCLAM unary_attr                         { UNARY (NOT, $2) }
| TILDE unary_attr                          { UNARY (BNOT, $2) }
| SIZEOF unary_attr                         { EXPR_SIZEOF $2 }
| SIZEOF LPAREN type_id RPAREN              { let b, d = [], JUSTBASE (*$3*) in 
                                              TYPE_SIZEOF (b, d) }
/* (* GCC extensions *) */
| ALIGNOF unary_attr                        { EXPR_ALIGNOF $2 }
| ALIGNOF LPAREN type_id RPAREN             {let b, d = [],JUSTBASE(*$3*)in 
                                             TYPE_ALIGNOF (b, d) }
;

postfix_attr:
| IDENT                                { VARIABLE $1 }
| IDENT COLON CST_INT                  { VARIABLE ($1 ^ ":" ^ $3) }
| DEFAULT COLON CST_INT                { VARIABLE ("default:" ^ $3) }
                                         /* (* use a VARIABLE "" so that the 
                                             * parentheses are printed *) */
| IDENT LPAREN  RPAREN                 { CALL(VARIABLE $1, [VARIABLE ""]) }
| IDENT paren_attr_list_ne             { CALL(VARIABLE $1, $2) }
| CST_INT                              { CONSTANT(CONST_INT $1) }
| string_list                          { CONSTANT(CONST_STRING $1) }
                                           /*(* Const when it appears in 
                                            * attribute lists, is translated 
                                            * to aconst *)*/
| CONST                                { VARIABLE "aconst" }
| postfix_attr DOT            IDENT    { MEMBEROF ($1, $3) }
| postfix_attr ARROW          IDENT    { MEMBEROFPTR ($1, $3) }
| LPAREN attr RPAREN                   { $2 } 
;

attr_list_ne:
|  attr                                  { [$1] }
|  attr COMMA attr_list_ne               { $1 :: $3 }
|  error COMMA attr_list_ne              { $3 }
;
paren_attr_list_ne: 
   LPAREN attr_list_ne RPAREN            { $2 }
|  LPAREN error RPAREN                   { [] }
;
/*** GCC ASM instructions ***/
asmattr:
     /* empty */                        { [] }
|    VOLATILE  asmattr                  { ("volatile", []) :: $2 }
|    CONST asmattr                      { ("const", []) :: $2 } 
;
asmtemplate: 
    CST_STRING                          { [$1] }
|   CST_STRING asmtemplate              { $1 :: $2 }
;
asmoutputs: 
  /* empty */           { ([], [], []) }
| COLON asmoperands asminputs
                        { let (ins, clobs) = $3 in
                          ($2, ins, clobs) }
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
     string_list LPAREN expression RPAREN    { ($1, NOTHING(*$3*)) }
|    string_list LPAREN error RPAREN         { ($1, NOTHING ) } 
; 
asminputs: 
  /* empty */                { ([], []) }
| COLON asmoperands asmclobber
                        { ($2, $3) }
;
asmclobber:
    /* empty */                         { [] }
| COLON asmcloberlst_ne                 { $2 }
;
asmcloberlst_ne:
   CST_STRING                           { [$1] }
|  CST_STRING COMMA asmcloberlst_ne     { $1 :: $3 }
;

%%




  

