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

%token <string> IDENT
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
%token PRAGMA

/* sm: cabs tree transformation specification keywords */
%token AT_TRANSFORM AT_TRANSFORMEXPR AT_SPECIFIER AT_EXPR AT_NAME


/* C++ keywords */
%token BOOL CATCH CLASS CONST_CLASS DELETE DYNAMIC_CAST EXPLICIT
%token EXPORT FALSE FRIEND MUTABLE NAMESPACE NEW OPERATOR PRIVATE PROTECTED
%token PUBLIC REINTERPRET_CAST STATIC_CAST TEMPLATE THIS THROW TRUE TRY
%token TYPEID TYPENAME USING VIRTUAL 
%token COLON_COLON

%token TILDE_IDENT DOT_STAR ARROW_STAR

/* Conflict resolution tokens */
%token CR_a0 CR_a1 CR_a2 CR_a3 CR_a4 CR_a5 CR_a6 CR_a7 CR_a8 CR_a9  
%token CR_b0 CR_b1 CR_b2 CR_b3 CR_b4 CR_b5 CR_b6 CR_b7 CR_b8 CR_b9
%token CR_c0 CR_c1 CR_c2 CR_c3 CR_c4 CR_c5 CR_c6 CR_c7 CR_c8 CR_c9  


/* Non-terminals informations */
%start file

%type <Cabs.definition list> file


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



ellipsis_opt: 
  /* empty */  { () }
| ELLIPSIS   { () }
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

id_expression:
  unqualified_id                                { () }
| qualified_id                                   { () }
;


unqualified_id:
  IDENT                                   { () }
| operator_function_id                    { () }
| conversion_function_id                  { () }
| TILDE_IDENT                             { () } 
;

qualified_id:
  nn_spec_unqualified_id    { () }
| COLON_COLON IDENT                                   { () }
| COLON_COLON operator_function_id                                   { () }
| COLON_COLON template_id                                   { () }
;


nested_name_specifier:
  class_or_namespace_name COLON_COLON nested_name_specifier_opt                                        { () }
| class_or_namespace_name COLON_COLON TEMPLATE nested_name_specifier                                   { () }
;

nested_name_specifier_opt:
  /* empty */                                   { () }
| nested_name_specifier                                   { () }
;


class_or_namespace_name:
  NAMED_TYPE  { () }
;


postfix_expression:
  primary_expression                                   { () }
| postfix_expression LBRACKET expression RBRACKET      { () }
| postfix_expression LPAREN expression RPAREN           { () }
| simple_type_specifier LPAREN expression_opt RPAREN    { () }
| TYPENAME nn_spec_IDENT  
                        LPAREN expression_list_opt RPAREN    { () }
| TYPENAME nn_spec_template_opt_template_id 
                        LPAREN expression_list_opt RPAREN    { () }
| postfix_expression DOT          id_expression    { () }
| postfix_expression DOT TEMPLATE id_expression    { () }
| postfix_expression ARROW        id_expression    { () }
| postfix_expression ARROW TEMPLATE id_expression    { () }
| postfix_expression DOT pseudo_destructor_name    { () }
| postfix_expression ARROW pseudo_destructor_name    { () }
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

pseudo_destructor_name:
  nn_spec_opt_type_name COLON_COLON TILDE named_type    { () }
| nn_spec TEMPLATE template_id COLON_COLON TILDE named_type    { () }
| nn_spec_opt TILDE named_type    { () }
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
| location IF LPAREN condition RPAREN statement  { () }
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
| location PRAGMA attr                  { () }
;

block_declaration:
| simple_declaration  { () }
| asm_definition  { () }
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
| decl_specifier_seq  { () }
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
| nn_spec_opt_type_name  { () }
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
| class_key nn_spec_opt_IDENT  { () }
| ENUM nn_spec_opt_IDENT { () }
| TYPENAME nn_spec_IDENT  { () }
| TYPENAME nn_spec_IDENT  
                       INF template_argument_list SUP  { () }
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
| location NAMESPACE IDENT EQ nn_spec_opt_IDENT SEMICOLON  { () }
;

  
using_declaration:
| location USING              nn_spec_opt_unqualified_id SEMICOLON  { () }
| location USING TYPENAME     nn_spec_unqualified_id SEMICOLON  { () }
;

using_directive:
| location USING NAMESPACE nn_spec_opt_IDENT SEMICOLON  { () }
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
| direct_declarator  attributes_with_asm { () }
| ptr_operator declarator  { () }
;

direct_declarator:
| declarator_id  { () }
| direct_declarator LPAREN parameter_declaration_clause 
           RPAREN                       { () }
| direct_declarator LPAREN parameter_declaration_clause 
           RPAREN /*(* cv_qualifier_seq_opt *)*/ 
           attributes THROW LPAREN type_id_list_opt RPAREN  { () }
| direct_declarator LBRACKET constant_expression_opt RBRACKET  { () }
| LPAREN attributes declarator RPAREN  { () }
;

  
ptr_operator:
| STAR cv_qualifier_seq_opt  { () }
| AND  { () }
| nn_spec STAR cv_qualifier_seq_opt  { () }
;

cv_qualifier_seq_opt: 
  attributes      { () }   
;

declarator_id:
|             id_expression  { () }  /* (* just for test *) */
/* (* The following is covered by id_expression *) */
| CR_a2 nn_spec_opt_type_name  { () }
;
 
type_id:
| type_specifier_seq abstract_declarator_opt  { () }
;
type_specifier_seq:
    type_specifier type_specifier_seq_opt  { () }
;

type_specifier_seq_opt: 
  /* empty */      { () }  
| type_specifier_seq  { () }
;

abstract_declarator:
| ptr_operator abstract_declarator_opt  { () }
| direct_abstract_declarator attributes { () }
;

abstract_declarator_opt: 
  /* empty */      { () }  
| abstract_declarator  { () }
;

direct_abstract_declarator:
| direct_abstract_declarator_opt 
           LPAREN parameter_declaration_clause RPAREN  { () }
| direct_abstract_declarator_opt 
           LPAREN parameter_declaration_clause RPAREN 
               attributes /*(*cv_qualifier_seq_opt *)*/
               THROW LPAREN type_id_list_opt RPAREN  { () }
| direct_abstract_declarator_opt LBRACKET constant_expression_opt RBRACKET  
               { () }
| LPAREN attributes abstract_declarator RPAREN  { () }
;

direct_abstract_declarator_opt: 
  /* empty */      { () }  
| direct_abstract_declarator { () }
;
  
parameter_declaration_clause:
| parameter_declaration_list_opt ellipsis_opt  { () }
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
class_name:
| named_type  { () }
| template_id  { () }
;


class_specifier:
| class_head LBRACE member_specification_opt RBRACE  { () }
;

class_head:
| class_key identifier_opt base_clause_opt  { () }
| class_key nn_spec_IDENT base_clause_opt  { () }

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
| qualified_id SEMICOLON  { () }
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
| identifier_opt COLON constant_expression { () }
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
| nn_spec_opt_class_name { () }
| VIRTUAL access_specifier_opt nn_spec_opt_class_name  { () }
| access_specifier virtual_opt nn_spec_opt_class_name  { () }
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
    type_specifier_seq conversion_declarator_opt  { () }
;
conversion_declarator:
    ptr_operator conversion_declarator_opt  { () }
;

conversion_declarator_opt: 
  /* empty */           { () }  
| conversion_declarator { () }
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
| nn_spec_opt_class_name { () }
/* (* The IDENT case is covered by the previous case *) */
| CR_a0 IDENT  { () }
;

/* (* 1.11  Overloading                            [gram.over] *) */
operator_function_id:
| OPERATOR NEW  { () }
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
| OPERATOR ARROW STAR  { () }
| OPERATOR ARROW  { () }
| OPERATOR LPAREN RPAREN  { () }
| OPERATOR LBRACKET RBRACKET  { () }


/* (* 1.12  Templates                                        [gram.temp] *) */
template_declaration:
| location        TEMPLATE INF template_parameter_list 
                                   SUP declaration  { () }
| location EXPORT TEMPLATE INF template_parameter_list 
                                   SUP declaration  { () }
;

template_parameter_list:
| template_parameter  { () }
| template_parameter_list COMMA template_parameter  { () }
;


template_parameter:
| type_parameter  { () }
| parameter_declaration  { () }
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

template_id:
| named_type INF template_argument_list SUP  { () }
;
       
template_argument_list:
| template_argument  { () }
| template_argument_list COMMA template_argument  { () }
;


template_argument:
| assignment_expression  { () }
| type_id  { () }
/* (* The IDENT case is covered by the assignment_expression *) */
| CR_a1 IDENT  { () }
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


nn_spec_opt_type_name: 
|                                   named_type   { () }
|             nested_name_specifier named_type   { () }
| COLON_COLON nested_name_specifier named_type   { () }
;

named_type: 
  IDENT /* NAMED_TYPE  */     { () }
;

nn_spec_opt_class_name: 
|                                   class_name   { () }
|             nested_name_specifier class_name   { () }
| COLON_COLON nested_name_specifier class_name   { () }

nn_spec_unqualified_id:
              nested_name_specifier unqualified_id   { () }
| COLON_COLON nested_name_specifier unqualified_id   { () }
;

nn_spec_opt_unqualified_id:
                                    unqualified_id   { () }
|             nested_name_specifier unqualified_id   { () }
| COLON_COLON nested_name_specifier unqualified_id   { () }
;

nn_spec_IDENT:
              nested_name_specifier IDENT   { () }
| COLON_COLON nested_name_specifier IDENT   { () }
;

nn_spec_opt_IDENT:
|                                   IDENT   { () }
|             nested_name_specifier IDENT   { () }
| COLON_COLON nested_name_specifier IDENT   { () }
;


nn_spec_template_opt_template_id:
  nn_spec template_id          { () }
| nn_spec TEMPLATE template_id { () }
;

nn_spec:
|             nested_name_specifier   { () }
| COLON_COLON nested_name_specifier   { () }
;

nn_spec_opt:
|                                      { () }
|             nested_name_specifier    { () }
| COLON_COLON nested_name_specifier    { () }
;

/*** GCC attributes ***/
attributes:
    /* empty */				{ []}	
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




  
