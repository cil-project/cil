/*(* NOTE: This parser is based on a parser written by Hugues Casse. Since
   * then I have changed it in numerous ways to the point where it probably
   * does not resemble Hugues's original one at all  *)*/
%{
open Cabs
let version = "Cparser V3.0b 10.9.99 Hugues Cassé"

let parse_error msg : 'a =           (* sm: c++-mode highlight hack: -> ' <- *)
  Errormsg.hadErrors := true;
  Clexer.display_error
    msg
    (Parsing.symbol_start ()) (Parsing.symbol_end ())

let print = print_string


let currentLoc () = { lineno = !Clexer.currentLine; 
                      filename = !Clexer.currentFile;}


(*
** Expression building
*)
let smooth_expression lst =
  match lst with
    [] -> NOTHING
  | [expr] -> expr
  | _ -> COMMA (lst)


let currentFunctionName = ref "<outside any function>"
    
let announceFunctionName ((n, decl, _):name) =
  Clexer.add_identifier n;
  (* Start a context that includes the parameter names and the whole body. 
   * Will pop when we finish parsing the function body *)
  Clexer.push_context ();
  (* Go through all the parameter names and mark them as identifiers *)
  let rec findProto = function
      PROTO (d, args, _) when isJUSTBASE d -> 
        List.iter (fun (_, (an, _, _)) -> Clexer.add_identifier an) args

    | PROTO (d, _, _) -> findProto d
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

let doDeclaration (loc: cabsloc) (specs: spec_elem list) (nl: init_name list) : definition = 
  if isTypedef specs then begin
    (* Tell the lexer about the new type names *)
    List.iter (fun ((n, _, _), _) -> Clexer.add_type n) nl;
    TYPEDEF ((specs, List.map (fun (n, _) -> n) nl), loc)
  end else
    if nl = [] then
      ONLYTYPEDEF (specs, loc)
    else begin
      (* Tell the lexer about the new variable names *)
      List.iter (fun ((n, _, _), _) -> Clexer.add_identifier n) nl;
      DECDEF ((specs, nl), loc)  
    end


let doFunctionDef (loc: cabsloc)
                  (specs: spec_elem list) 
                  (n: name) 
                  (b: block) : definition = 
  let fname = (specs, n) in
  FUNDEF (fname, b, loc)


let doOldParDecl (names: string list)
                 ((pardefs: name_group list), (isva: bool)) : single_name list * bool =
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
  (List.map findOneName names, isva)

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
%token <string> NAMED_TYPE

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
%token BLOCKATTRIBUTE
%token DECLSPEC
%token <string> MSASM MSATTR
%token PRAGMA

/* sm: cabs tree transformation specification keywords */
%token AT_TRANSFORM AT_TRANSFORMEXPR AT_SPECIFIER AT_EXPR AT_NAME

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
%left	STAR SLASH PERCENT CONST RESTRICT VOLATILE
%right	EXCLAM TILDE PLUS_PLUS MINUS_MINUS CAST RPAREN ADDROF SIZEOF ALIGNOF
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
%type <Cabs.constant> constant
%type <Cabs.expression> expression opt_expression
%type <Cabs.init_expression> init_expression
%type <Cabs.expression list> comma_expression paren_comma_expression arguments
%type <Cabs.expression list> bracket_comma_expression
%type <string> string_list

%type <Cabs.initwhat * Cabs.init_expression> initializer
%type <(Cabs.initwhat * Cabs.init_expression) list> initializer_list
%type <Cabs.initwhat> init_designators init_designators_opt

%type <spec_elem list> decl_spec_list
%type <typeSpecifier> type_spec
%type <Cabs.field_group list> struct_decl_list


%type <Cabs.name> old_proto_decl
%type <Cabs.single_name> parameter_decl
%type <Cabs.enum_item> enumerator
%type <Cabs.enum_item list> enum_list
%type <Cabs.definition> declaration function_def
%type <cabsloc * spec_elem list * name> function_def_start
%type <Cabs.spec_elem list * Cabs.decl_type> type_name
%type <Cabs.block> block
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
%type <attribute list list> pointer pointer_opt /* Each element is a "* <type_quals_opt>" */
%type <Cabs.cabsloc> location
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
  declaration                           { $1 }
| function_def                          { $1 }
| location ASM LPAREN string_list RPAREN SEMICOLON
                                        { GLOBASM ($4, $1) }
| location PRAGMA attr                  { PRAGMA ($3, $1) }
/* (* Old-style function prototype. This should be somewhere else, like in
    * "declaration". For now we keep it at global scope only because in local
    * scope it looks too much like a function call  *) */
| location  IDENT LPAREN old_parameter_list_ne RPAREN old_pardef_list SEMICOLON
                           { (* Convert pardecl to new style *)
                             let pardecl, isva = doOldParDecl $4 $6 in
                             (* Make the function declarator *)
                             doDeclaration $1 []
                               [(($2, PROTO(JUSTBASE, pardecl,isva), []),
                                 NO_INIT)]
                            }
/* (* Old style function prototype, but without any arguments *) */
| location  IDENT LPAREN RPAREN  SEMICOLON
                           { (* Make the function declarator *)
                             doDeclaration $1 []
                               [(($2, PROTO(JUSTBASE,[],false), []),
                                 NO_INIT)]
                            }
/* transformer for a toplevel construct */
| location AT_TRANSFORM LBRACE global RBRACE  IDENT/*to*/  LBRACE globals RBRACE {
    checkConnective($6);
    TRANSFORMER($4, $8, $1)
  }
/* transformer for an expression */
| location AT_TRANSFORMEXPR LBRACE expression RBRACE  IDENT/*to*/  LBRACE expression RBRACE {
    checkConnective($6);
    EXPRTRANSFORMER($4, $8, $1)
  }
| location error SEMICOLON { PRAGMA (VARIABLE "parse_error", $1) }
;

id_or_typename:
    IDENT				{$1}
|   NAMED_TYPE				{$1}
|   AT_NAME LPAREN IDENT RPAREN         { "@name(" ^ $3 ^ ")" }     /* pattern variable name */
;

maybecomma:
   /* empty */                          { () }
|  COMMA                                { () }
;

/* *** Expressions *** */


expression:
        	constant
		        {CONSTANT $1}
|		IDENT
		        {VARIABLE $1}
|		SIZEOF expression
		        {EXPR_SIZEOF $2}
|	 	SIZEOF LPAREN type_name RPAREN
		        {let b, d = $3 in TYPE_SIZEOF (b, d)}
|		ALIGNOF expression
		        {EXPR_ALIGNOF $2}
|	 	ALIGNOF LPAREN type_name RPAREN
		        {let b, d = $3 in TYPE_ALIGNOF (b, d)}
|		PLUS expression
		        {UNARY (PLUS, $2)}
|		MINUS expression
		        {UNARY (MINUS, $2)}
|		STAR expression
		        {UNARY (MEMOF, $2)}
|		AND expression				%prec ADDROF
		        {UNARY (ADDROF, $2)}
|		EXCLAM expression
		        {UNARY (NOT, $2)}
|		TILDE expression
		        {UNARY (BNOT, $2)}
|		PLUS_PLUS expression                    %prec CAST
		        {UNARY (PREINCR, $2)}
|		expression PLUS_PLUS
		        {UNARY (POSINCR, $1)}
|		MINUS_MINUS expression                  %prec CAST
		        {UNARY (PREDECR, $2)}
|		expression MINUS_MINUS
		        {UNARY (POSDECR, $1)}
|		expression ARROW id_or_typename
		        {MEMBEROFPTR ($1, $3)}
|		expression DOT id_or_typename
		        {MEMBEROF ($1, $3)}
|		LPAREN block RPAREN
		        { GNU_BODY $2 }
|		paren_comma_expression
		        {(smooth_expression $1)}
|		expression LPAREN arguments RPAREN
			{CALL ($1, $3)}
|		expression bracket_comma_expression
			{INDEX ($1, smooth_expression $2)}
|		expression QUEST opt_expression COLON expression
			{QUESTION ($1, $3, $5)}
|		expression PLUS expression
			{BINARY(ADD ,$1 , $3)}
|		expression MINUS expression
			{BINARY(SUB ,$1 , $3)}
|		expression STAR expression
			{BINARY(MUL ,$1 , $3)}
|		expression SLASH expression
			{BINARY(DIV ,$1 , $3)}
|		expression PERCENT expression
			{BINARY(MOD ,$1 , $3)}
|		expression AND_AND expression
			{BINARY(AND ,$1 , $3)}
|		expression PIPE_PIPE expression
			{BINARY(OR ,$1 , $3)}
|		expression AND expression
			{BINARY(BAND ,$1 , $3)}
|		expression PIPE expression
			{BINARY(BOR ,$1 , $3)}
|		expression CIRC expression
			{BINARY(XOR ,$1 , $3)}
|		expression EQ_EQ expression
			{BINARY(EQ ,$1 , $3)}
|		expression EXCLAM_EQ expression
			{BINARY(NE ,$1 , $3)}
|		expression INF expression
			{BINARY(LT ,$1 , $3)}
|		expression SUP expression
			{BINARY(GT ,$1 , $3)}
|		expression INF_EQ expression
			{BINARY(LE ,$1 , $3)}
|		expression SUP_EQ expression
			{BINARY(GE ,$1 , $3)}
|		expression  INF_INF expression
			{BINARY(SHL ,$1 , $3)}
|		expression  SUP_SUP expression
			{BINARY(SHR ,$1 , $3)}
|		expression EQ expression
			{BINARY(ASSIGN ,$1 , $3)}
|		expression PLUS_EQ expression
			{BINARY(ADD_ASSIGN ,$1 , $3)}
|		expression MINUS_EQ expression
			{BINARY(SUB_ASSIGN ,$1 , $3)}
|		expression STAR_EQ expression
			{BINARY(MUL_ASSIGN ,$1 , $3)}
|		expression SLASH_EQ expression
			{BINARY(DIV_ASSIGN ,$1 , $3)}
|		expression PERCENT_EQ expression
			{BINARY(MOD_ASSIGN ,$1 , $3)}
|		expression AND_EQ expression
			{BINARY(BAND_ASSIGN ,$1 , $3)}
|		expression PIPE_EQ expression
			{BINARY(BOR_ASSIGN ,$1 , $3)}
|		expression CIRC_EQ expression
			{BINARY(XOR_ASSIGN ,$1 , $3)}
|		expression INF_INF_EQ expression	
			{BINARY(SHL_ASSIGN ,$1 , $3)}
|		expression SUP_SUP_EQ expression
			{BINARY(SHR_ASSIGN ,$1 , $3)}
|		LPAREN type_name RPAREN expression
		         { CAST($2, SINGLE_INIT $4) }
/* (* We handle GCC constructor expressions *) */
|		LPAREN type_name RPAREN LBRACE initializer_list_opt RBRACE
		         { CAST($2, COMPOUND_INIT $5) }
/* (* GCC's address of labels *)  */
|               AND_AND IDENT  { LABELADDR $2 }
|               AT_EXPR LPAREN IDENT RPAREN         /* expression pattern variable */
                         { EXPR_PATTERN($3) }
;

constant:
    CST_INT				{CONST_INT $1}
|   CST_FLOAT				{CONST_FLOAT $1}
|   CST_CHAR				{CONST_CHAR $1}
|   string_list				{CONST_STRING $1}
;
string_list:
    one_string                          { $1 }
|   string_list one_string              { $1 ^ $2 }
;
one_string: 
    CST_STRING				{$1}
|   FUNCTION__                          {!currentFunctionName}
|   PRETTY_FUNCTION__                   {!currentFunctionName}
;    

init_expression:
     expression         { SINGLE_INIT $1 }
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
                                        { ATINDEX_INIT($2, $4) }
|   LBRACKET  expression ELLIPSIS expression RBRACKET
                                        { ATINDEXRANGE_INIT($2, $4) }
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
|               comma_expression    { $1 }
;

opt_expression:
	        /* empty */
	        	{NOTHING}
|	        comma_expression
	        	{smooth_expression $1}
;
comma_expression:
	        expression                        {[$1]}
|               expression COMMA comma_expression { $1 :: $3 }
|               error COMMA comma_expression      { $3 }
;

paren_comma_expression:
  LPAREN comma_expression RPAREN                   { $2 }
| LPAREN error RPAREN                              { [] }
;

bracket_comma_expression:
  LBRACKET comma_expression RBRACKET                   { $2 }
| LBRACKET error RBRACKET                              { [] }
;


/*** statements ***/
block: /* ISO 6.8.2 */
    block_begin local_labels block_attrs declaration_list statement_list RBRACE   
                                         {Clexer.pop_context(); 
                                          { blabels = $2;
                                            battrs = $3;
                                            bdefs = $4;
                                            bstmts = $5; }
                                         } 
|   error RBRACE                         { { blabels = [];
                                             battrs  = [];
                                             bdefs   = [];
                                             bstmts  = [] }
                                         } 
;
block_begin:
    LBRACE      		         {Clexer.push_context ()}
;

block_attrs:
   /* empty */                                              { [] }
|  BLOCKATTRIBUTE paren_attr_list_ne
                                        { [("__blockattribute__", $2)] }
;

declaration_list: 
    /* empty */                          { [] }
|   declaration declaration_list         { $1 :: $2 }
;
statement_list:
|   /* empty */                          { [] }
|   statement statement_list             { $1 :: $2 }
/*(* GCC accepts a label at the end of a block *)*/
|   location IDENT COLON                 { [ LABEL ($2, NOP $1, $1)] }
;


local_labels: 
   /* empty */                           { [] }
|  LABEL__ local_label_names SEMICOLON   { $2 }
;
local_label_names: 
   IDENT                                 { [ $1 ] }
|  IDENT COMMA local_label_names         { $1 :: $3 }
;



statement:
    location SEMICOLON		{NOP $1 }
|   location comma_expression SEMICOLON
	        	{COMPUTATION (smooth_expression $2, $1)}
|   location block               {BLOCK ($2, $1)}
|   location IF paren_comma_expression statement                    %prec IF
                	{IF (smooth_expression $3, $4, NOP $1, $1)}
|   location IF paren_comma_expression statement ELSE statement
	                {IF (smooth_expression $3, $4, $6, $1)}
|   location SWITCH paren_comma_expression statement
                        {SWITCH (smooth_expression $3, $4, $1)}
|   location WHILE paren_comma_expression statement
	        	{WHILE (smooth_expression $3, $4, $1)}
|   location DO statement WHILE paren_comma_expression SEMICOLON
	        	         {DOWHILE (smooth_expression $5, $3, $1)}
|   location FOR LPAREN for_clause opt_expression
	        SEMICOLON opt_expression RPAREN statement
	                         {FOR ($4, $5, $7, $9, $1)}
|   location IDENT COLON statement
		                 {LABEL ($2, $4, $1)}
|   location CASE expression COLON
	                         {CASE ($3, NOP $1, $1)}
|   location CASE expression ELLIPSIS expression COLON
	                         {CASERANGE ($3, $5, NOP $1, $1)}
|   location DEFAULT COLON
	                         {DEFAULT (NOP $1, $1)}
|   location RETURN SEMICOLON    {RETURN (NOTHING, $1)}
|   location RETURN comma_expression SEMICOLON
	                         {RETURN (smooth_expression $3, $1)}
|   location BREAK SEMICOLON     {BREAK $1}
|   location CONTINUE SEMICOLON	 {CONTINUE $1}
|   location GOTO IDENT SEMICOLON
		                 {GOTO ($3, $1)}
|   location GOTO STAR comma_expression SEMICOLON 
                                 { COMPGOTO (smooth_expression $4, $1) }
|   location ASM asmattr LPAREN asmtemplate asmoutputs RPAREN SEMICOLON
                        { let (outs,ins,clobs) = $6 in
                          ASM ($3, $5, outs, ins, clobs, $1) }
|   location MSASM               { ASM ([], [$2], [], [], [], $1)}
|   location error   SEMICOLON   { (NOP $1)}
;


for_clause: 
    opt_expression SEMICOLON     { FC_EXP $1 }
|   declaration                  { FC_DECL $1 }
;

declaration:                                /* ISO 6.7.*/
    location decl_spec_list init_declarator_list SEMICOLON
                                       { doDeclaration $1 $2 $3 }
|   location decl_spec_list SEMICOLON  { doDeclaration $1 $2 [] }
;
init_declarator_list:                       /* ISO 6.7 */
    init_declarator                              { [$1] }
|   init_declarator COMMA init_declarator_list   { $1 :: $3 }

;
init_declarator:                             /* ISO 6.7 */
    declarator                          { ($1, NO_INIT) }
|   declarator EQ init_expression
                                        { ($1, $3) }
;

decl_spec_list:                         /* ISO 6.7 */
                                        /* ISO 6.7.1 */
|   TYPEDEF decl_spec_list_opt          { SpecTypedef :: $2  }    
|   EXTERN decl_spec_list_opt           { SpecStorage EXTERN :: $2 }
|   STATIC  decl_spec_list_opt          { SpecStorage STATIC :: $2 }
|   AUTO   decl_spec_list_opt           { SpecStorage AUTO :: $2 }
|   REGISTER decl_spec_list_opt         { SpecStorage REGISTER :: $2}
                                        /* ISO 6.7.2 */
|   type_spec decl_spec_list_opt_no_named { SpecType $1 :: $2 }
                                        /* ISO 6.7.4 */
|   INLINE decl_spec_list_opt           { SpecInline :: $2 }
|   attribute decl_spec_list_opt        { SpecAttr $1 :: $2 }  
/* specifier pattern variable (must be last in spec list) */
|   AT_SPECIFIER LPAREN IDENT RPAREN    { [ SpecPattern($3) ] }
;
/* (* In most cases if we see a NAMED_TYPE we must shift it. Thus we declare 
    * NAMED_TYPE to have right associativity  *) */
decl_spec_list_opt: 
    /* empty */                         { [] } %prec NAMED_TYPE
|   decl_spec_list                      { $1 }
;
/* (* We add this separate rule to handle the special case when an appearance 
    * of NAMED_TYPE should not be considered as part of the specifiers but as 
    * part of the declarator. IDENT has higher precedence than NAMED_TYPE  *)
 */
decl_spec_list_opt_no_named: 
    /* empty */                         { [] } %prec IDENT
|   decl_spec_list                      { $1 }
;
type_spec:   /* ISO 6.7.2 */
    VOID            { Tvoid}
|   CHAR            { Tchar }
|   SHORT           { Tshort }
|   INT             { Tint }
|   LONG            { Tlong }
|   INT64           { Tint64 }
|   FLOAT           { Tfloat }
|   DOUBLE          { Tdouble }
|   SIGNED          { Tsigned }
|   UNSIGNED        { Tunsigned }
|   STRUCT id_or_typename 
                    { Tstruct ($2, None) }
|   STRUCT id_or_typename LBRACE struct_decl_list RBRACE
                    { Tstruct ($2, Some $4) }
|   STRUCT           LBRACE struct_decl_list RBRACE
                    { Tstruct ("", Some $3) }
|   UNION id_or_typename 
                    { Tunion ($2, None) }
|   UNION id_or_typename LBRACE struct_decl_list RBRACE
                    { Tunion ($2, Some $4) }
|   UNION          LBRACE struct_decl_list RBRACE
                    { Tunion ("", Some $3) }
|   ENUM id_or_typename   { Tenum ($2, None) }
|   ENUM id_or_typename LBRACE enum_list maybecomma RBRACE
                    { Tenum ($2, Some $4) }
|   ENUM          LBRACE enum_list maybecomma RBRACE
                    { Tenum ("", Some $3) }
|   NAMED_TYPE      { Tnamed $1 }
|   TYPEOF LPAREN expression RPAREN     { TtypeofE $3 } 
|   TYPEOF LPAREN type_name RPAREN      { let s, d = $3 in
                                          TtypeofT (s, d) } 
;
struct_decl_list: /* (* ISO 6.7.2. Except that we allow empty structs. We 
                      * also allow missing field names. *)
                   */
   /* empty */                           { [] }
|  decl_spec_list                 SEMICOLON struct_decl_list
                                         { ($1, 
                                            [(missingFieldDecl, None)]) :: $3 }
|  decl_spec_list field_decl_list SEMICOLON struct_decl_list
                                          { ($1, $2) 
                                            :: $4 }
|  error                          SEMICOLON struct_decl_list
                                          { $3 } 
;
field_decl_list: /* (* ISO 6.7.2 *) */
    field_decl                           { [$1] }
|   field_decl COMMA field_decl_list     { $1 :: $3 }
;
field_decl: /* (* ISO 6.7.2. Except that we allow unnamed fields. *) */
|   declarator                           { ($1, None) }
|   declarator COLON expression          { ($1, Some $3) }    
|              COLON expression          { (missingFieldDecl, Some $2) }
;

enum_list: /* (* ISO 6.7.2.2 *) */
    enumerator				{[$1]}
|   enum_list COMMA enumerator	        {$1 @ [$3]}
|   enum_list COMMA error               { $1 } 
;
enumerator:	
    IDENT				{($1, NOTHING)}
|   IDENT EQ expression			{($1, $3)}
;


declarator:  /* (* ISO 6.7.5. Plus Microsoft declarators.*) */
   pointer_opt direct_decl attributes_with_asm
                                         { let (n, decl) = $2 in
                                           (n, applyPointer $1 decl, $3) }
;

direct_decl: /* (* ISO 6.7.5 *) */
                                   /* (* We want to be able to redefine named
                                    * types as variable names *) */
|   id_or_typename                 { ($1, JUSTBASE) }

|   LPAREN attributes declarator RPAREN
                                   { let (n,decl,al) = $3 in
                                     (n, PARENTYPE($2,decl,al)) }

|   direct_decl bracket_comma_expression
                                   { let (n, decl) = $1 in
                                     (n, ARRAY(decl, smooth_expression $2)) }
|   direct_decl LBRACKET RBRACKET  { let (n, decl) = $1 in
                                     (n, ARRAY(decl, NOTHING)) }
|   direct_decl parameter_list_startscope rest_par_list RPAREN
                                   { let (n, decl) = $1 in
                                     let (params, isva) = $3 in
                                     Clexer.pop_context ();
                                     (n, PROTO(decl, params, isva))
                                   }
;
parameter_list_startscope: 
    LPAREN                         { Clexer.push_context () }
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
   decl_spec_list declarator              { ($1, $2) }
|  decl_spec_list abstract_decl           { let d, a = $2 in
                                            ($1, ("", d, a)) }
|  decl_spec_list                         { ($1, ("", JUSTBASE, [])) }
|  LPAREN parameter_decl RPAREN           { $2 } 
;

/* (* Old style prototypes. Like a declarator *) */
old_proto_decl:
  pointer_opt direct_old_proto_decl       { let (n, decl, a) = $2 in
                                            (n, applyPointer $1 decl, a) }
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
;

old_parameter_list_ne:
|  IDENT                                       { [$1] }
|  IDENT COMMA old_parameter_list_ne           { let rest = $3 in
                                                 ($1 :: rest) }
;

old_pardef_list: 
   /* empty */                            { ([], false) }
|  decl_spec_list old_pardef SEMICOLON ELLIPSIS
                                          { ([($1, $2)], true) }  
|  decl_spec_list old_pardef SEMICOLON old_pardef_list  
                                          { let rest, isva = $4 in
                                            (($1, $2) :: rest, isva) 
                                          }
;

old_pardef: 
   declarator                             { [$1] }
|  declarator COMMA old_pardef            { $1 :: $3 }
|  error                                  { [] }
;


pointer: /* (* ISO 6.7.5 *) */ 
   STAR attributes pointer_opt  { $2 :: $3 }
;
pointer_opt:
                                     { [] }
|  pointer                           { $1 }
;

type_name: /* (* ISO 6.7.6 *) */
  decl_spec_list abstract_decl { let d, a = $2 in
                                 if a <> [] then begin
                                   parse_error "attributes in type name";
                                   raise Parsing.Parse_error
                                 end;
                                 ($1, d) 
                               }
| decl_spec_list               { ($1, JUSTBASE) }
;
abstract_decl: /* (* ISO 6.7.6. *) */
  pointer_opt abs_direct_decl attributes  { applyPointer $1 $2, $3 }
| pointer                                 { applyPointer $1 JUSTBASE, [] }
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
            
|   abs_direct_decl_opt bracket_comma_expression
                                   { ARRAY($1, smooth_expression $2) }
|   abs_direct_decl_opt LBRACKET RBRACKET  { ARRAY($1, NOTHING) }
|   abs_direct_decl parameter_list_startscope rest_par_list RPAREN
                                   { let (params, isva) = $3 in
                                     Clexer.pop_context ();
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
            Clexer.pop_context (); (* The context pushed by 
                                    * announceFunctionName *)
            doFunctionDef loc specs decl $2
          } 


function_def_start:  /* (* ISO 6.9.1 *) */
  location decl_spec_list declarator   
                            { announceFunctionName $3;
                              ($1, $2, $3)
                            } 

/* (* Old-style function prototype *) */
| location decl_spec_list old_proto_decl 
                            { announceFunctionName $3;
                              ($1, $2, $3) 
                            } 
/* (* New-style function that does not have a return type *) */
| location        IDENT parameter_list_startscope rest_par_list RPAREN 
                           { let (params, isva) = $4 in
                             let fdec = 
                               ($2, PROTO(JUSTBASE, params, isva), []) in
                             announceFunctionName fdec;
                             (* Default is int type *)
                             let defSpec = [SpecType Tint] in
                             ($1, defSpec, fdec) 
                           }

/* (* No return type and old-style parameter list *) */
| location        IDENT LPAREN old_parameter_list_ne RPAREN old_pardef_list
                           { (* Convert pardecl to new style *)
                             let pardecl, isva = doOldParDecl $4 $6 in
                             (* Make the function declarator *)
                             let fdec = ($2, 
                                         PROTO(JUSTBASE, pardecl,isva), []) in
                             announceFunctionName fdec;
                             (* Default is int type *)
                             let defSpec = [SpecType Tint] in
                             ($1, defSpec, fdec) 
                            }
/* (* No return type and no parameters *) */
| location        IDENT LPAREN                      RPAREN
                           { (* Make the function declarator *)
                             let fdec = ($2, 
                                         PROTO(JUSTBASE, [], false), []) in
                             announceFunctionName fdec;
                             (* Default is int type *)
                             let defSpec = [SpecType Tint] in
                             ($1, defSpec, fdec) 
                            }
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
|   id_or_typename                       { VARIABLE $1 }
|   IDENT COLON CST_INT                  { VARIABLE ($1 ^ ":" ^ $3) }
|   DEFAULT COLON CST_INT                { VARIABLE ("default:" ^ $3) }
                                         /* (* use a VARIABLE "" so that the 
                                             * parentheses are printed *) */
|   IDENT LPAREN  RPAREN                 { CALL(VARIABLE $1, [VARIABLE ""]) }
|   IDENT paren_attr_list_ne             { CALL(VARIABLE $1, $2) }
|   CST_INT                              { CONSTANT(CONST_INT $1) }
|   string_list                          { CONSTANT(CONST_STRING $1) }
                                           /*(* Const when it appears in 
                                            * attribute lists, is translated 
                                            * to aconst *)*/
|   CONST                                { VARIABLE "aconst" }
|   SIZEOF expression                     {EXPR_SIZEOF $2}
|   SIZEOF LPAREN type_name RPAREN
		                         {let b, d = $3 in TYPE_SIZEOF (b, d)}
|   ALIGNOF expression                   {EXPR_ALIGNOF $2}
|   ALIGNOF LPAREN type_name RPAREN      {let b, d = $3 in TYPE_ALIGNOF (b, d)}
|   PLUS expression    	                 {UNARY (PLUS, $2)}
|   MINUS expression 		        {UNARY (MINUS, $2)}
|   STAR expression		        {UNARY (MEMOF, $2)}
|   AND expression				                 %prec ADDROF
	                                {UNARY (ADDROF, $2)}
|   EXCLAM expression		        {UNARY (NOT, $2)}
|   TILDE expression		        {UNARY (BNOT, $2)}
|   attr PLUS attr                      {BINARY(ADD ,$1 , $3)} 
|   attr MINUS attr                     {BINARY(SUB ,$1 , $3)}
|   attr STAR expression                {BINARY(MUL ,$1 , $3)}
|   attr SLASH attr			{BINARY(DIV ,$1 , $3)}
|   attr PERCENT attr			{BINARY(MOD ,$1 , $3)}
|   attr AND_AND attr			{BINARY(AND ,$1 , $3)}
|   attr PIPE_PIPE attr			{BINARY(OR ,$1 , $3)}
|   attr AND attr			{BINARY(BAND ,$1 , $3)}
|   attr PIPE attr			{BINARY(BOR ,$1 , $3)}
|   attr CIRC attr			{BINARY(XOR ,$1 , $3)}
|   attr EQ_EQ attr			{BINARY(EQ ,$1 , $3)}
|   attr EXCLAM_EQ attr			{BINARY(NE ,$1 , $3)}
|   attr INF attr			{BINARY(LT ,$1 , $3)}
|   attr SUP attr			{BINARY(GT ,$1 , $3)}
|   attr INF_EQ attr			{BINARY(LE ,$1 , $3)}
|   attr SUP_EQ attr			{BINARY(GE ,$1 , $3)}
|   attr INF_INF attr			{BINARY(SHL ,$1 , $3)}
|   attr SUP_SUP attr			{BINARY(SHR ,$1 , $3)}
|   attr ARROW id_or_typename           {MEMBEROFPTR ($1, $3)} 
|   attr DOT id_or_typename             {MEMBEROF ($1, $3)}  
|   LPAREN attr RPAREN                  { $2 } 
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
     string_list LPAREN expression RPAREN    { ($1, $3) }
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



