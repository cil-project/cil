/*(* NOTE: This parser is based on a parser written by Hugues Casse. Since
   * then I have changed it in numerous ways to the point where it probably
   * does not resemble Hugues's original one at all  *)*/
%{
open Cabs
module E = Errormsg

let version = "Cparser V3.0b 10.9.99 Hugues Cassé"

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
%right  NAMED_CLASS    /* So that it shifts always */
%left   IDENT
%right  COLON_COLON /* (* Give this a high precedence and right-associativity 
                        * to ensure that it is shifted always as part of a 
                        * nested_namespace *) */

/* Non-terminals informations */
%start interpret file
%type <Cabs.definition list> file interpret globals

%type <Cabs.definition> global


%type <Cabs.attribute list> asmattr
%type <Cabs.statement> statement
%type <Cabs.constant> constant
%type <Cabs.expression> expression opt_expression
%type <Cabs.init_expression> init_expression
%type <Cabs.expression list> comma_expression paren_comma_expression arguments
%type <Cabs.expression list> bracket_comma_expression
%type <string> string_list wstring_list

%type <Cabs.initwhat * Cabs.init_expression> initializer
%type <(Cabs.initwhat * Cabs.init_expression) list> initializer_list
%type <Cabs.initwhat> init_designators init_designators_opt


%type <Cabs.enum_item> enumerator
%type <Cabs.enum_item list> enum_list
%type <Cabs.definition> declaration function_def
%type <cabsloc * spec_elem list> function_def_start
%type <Cabs.spec_elem list * Cabs.decl_type> type_name
%type <Cabs.block> block
%type <string list> local_labels local_label_names

%type <Cabs.cabsloc> location


%type <(Cabs.spec_elem list * Cabs.decl_type) list> type_id_list

%type <Cabs.spec_elem list list> decldef decl_list
%type <Cabs.spec_elem list> decl_rest
%type <Cabs.spec_elem> decl_spec decl_spec_start

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
| location decldef SEMICOLON            { PRAGMA(VARIABLE "decl", $1) }
| function_def                          { $1 } 
/*
| location ASM LPAREN string_list RPAREN SEMICOLON
                                        { GLOBASM ($4, $1) }
*/
| location PRAGMA attr                  { PRAGMA ($3, $1) }
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
| location EXTERN CST_STRING LBRACE globals RBRACE  
                           { LINKAGE ($3, $5, $1) }
| location EXTERN CST_STRING global 
                           { LINKAGE ($3, [$4], $1) }
| location error SEMICOLON { PRAGMA (VARIABLE "parse_error", $1) }
;

id_or_typename:
    IDENT				{$1}
|   NAMED_TYPE				{$1}
|   NAMED_CLASS                         {$1}
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
|               BUILTIN_VA_ARG LPAREN expression COMMA type_name RPAREN
                        { let b, d = $5 in
                          CALL (VARIABLE "__builtin_va_arg", 
                                [$3; TYPE_SIZEOF (b, d)]) }
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
|   wstring_list			{CONST_WSTRING $1}
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
                                         {Cxxlexer.pop_context(); 
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
    LBRACE      		         {Cxxlexer.push_context ()}
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
   /* empty */                                       { [] }
|  LABEL__ local_label_names SEMICOLON local_labels  { $2 @ $4 }
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
  location decldef SEMICOLON     { PRAGMA(VARIABLE("declaration"), $1) } 
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



function_def:  /* (* ISO 6.9.1 *) */
  function_def_start block   
          { let (loc, specs) = $1 in
            currentFunctionName := "<__FUNCTION__ used outside any functions>";
            Cxxlexer.pop_context (); (* The context pushed by 
                                      * announceFunctionName *)
            doFunctionDef loc specs $2
          } 


function_def_start:  /* (* ISO 6.9.1 *) */
  location decldef   
                            { match $2 with 
                                 [x] -> announceFunctionName x;
                                        ($1, x)
                               | _ -> E.s (E.bug "COMMA not allowed in function definition") 
                            } 
;

/*** GCC attributes ***/
 
attribute:
    ATTRIBUTE LPAREN paren_attr_list_ne RPAREN	
                                        { ("__attribute__", $3) }
|   DECLSPEC paren_attr_list_ne         { ("__declspec", $2) }
|   MSATTR                              { ($1, []) }
                                        /* ISO 6.7.3 */
|   CONST                               { ("const", []) }
|   RESTRICT                            { ("restrict",[]) }
|   VOLATILE                            { ("volatile",[]) }
/* (* In some contexts we can have an inline assembly to specify the name to 
    * be used for a global. We treat this as a name attribute *) */
|   ASM LPAREN string_list RPAREN        
                                        { ("__asm__", 
                                           [CONSTANT(CONST_STRING $3)]) }
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


/* (********************** C++ stuff **************************) */  

class_spec: 
  class_head  base_clause_opt LBRACE member_spec_opt RBRACE
                                         { () }
;

class_head: 
    class_key ns_id_or_typename          { Cxxlexer.add_class $2 }
;

class_key: 
  CLASS                                  { () }
| STRUCT                                 { () }
| UNION                                  { () }
;

base_clause_opt:
  /* nothing */                           { [] } %prec COMMA
| COLON base_specifier_list               { $2 }
;
base_specifier_list:
  ns_classname                            { [ $1 ] }
| ns_classname COMMA base_specifier_list  { $1 :: $3 }
;

member_spec_opt:
  /* nothing */                                    { [] }
| location decldef SEMICOLON member_spec_opt       { () :: $4 }
| function_def     member_spec_opt                 { () :: $2 }
| location access_specifier COLON member_spec_opt  { $2 :: $4 }
;

access_specifier:
  PRIVATE                                    { () }
| PROTECTED                                  { () }
| PUBLIC                                     { () }
;

ns_id_or_typename: 
  ns_id                                      { $1 }
| nested_name_spec NAMED_TYPE                { $1 ^ $2 }
| nested_name_spec NAMED_ENUM                { $1 ^ $2 }
| ns_classname                               { $1 }
;

ns_classname: 
  NAMED_CLASS                                { $1 }
| NAMED_CLASS COLON_COLON ns_classname       { $1 ^ "::" ^ $3 }
| NAMED_NAMESPACE COLON_COLON ns_classname   { $1 ^ "::" ^ $3 }
;

ns_id: 
  nested_name_spec IDENT                     { $1 ^ $2 }
;

ns_typename: 
| nested_name_spec NAMED_TYPE                { $1 ^ $2 }
| nested_name_spec NAMED_ENUM                { $1 ^ $2 }
| ns_classname                               { $1 }
;


nested_name_spec:
| COLON_COLON nested_name_spec_nocol   { "::" ^ $2 } 
|             nested_name_spec_nocol   {        $1 }
;
/* (* There is a conflict here when we expect the possibly qualified name of a 
    * class. We see a NAMED_CLASS and we do not know if it is part of the 
    * namespace or not. Do not use this before a NAMED_CLASS *) */
nested_name_spec_nocol:
    /* empty */                                         { "" }
|   NAMED_CLASS     COLON_COLON nested_name_spec_nocol  { $1 ^ "::" ^ $3 }
|   NAMED_NAMESPACE COLON_COLON nested_name_spec_nocol  { $1 ^ "::" ^ $3 }
;

/* (* Parse declarations as word-soup and let the downstream sort it out. 
    * Declarations always start with a specifier or a type name. Then follows 
    * a comma-separated list of declarators, each being a list of specifiers. 
    * The first in the list contains some specifiers that apply to all 
    * declarators. *)*/
decldef: decl_spec_start decl_list           
                           { match $2 with 
                               h :: rest -> ($1 :: h) :: rest
                             | _ -> E.s (E.bug "decldef") }
;
decl_spec_start: 
|  TYPEDEF       { SpecTypedef }
|  AUTO          { SpecStorage AUTO }                            
|  REGISTER      { SpecStorage REGISTER }
|  STATIC        { SpecStorage STATIC }
|  EXTERN        { SpecStorage EXTERN }
|  MUTABLE       { SpecStorage MUTABLE }
|  INLINE        { SpecFunspec INLINE }
|  VIRTUAL       { SpecFunspec VIRTUAL }
|  EXPLICIT      { SpecFunspec EXPLICIT }
|  attribute     { SpecAttr $1 }

|  VOID          { SpecType Tvoid }
|  CHAR          { SpecType Tchar }
|  SHORT         { SpecType Tshort }
|  INT           { SpecType Tint }
|  LONG          { SpecType Tlong }
|  INT64         { SpecType Tint64 }
|  FLOAT         { SpecType Tfloat }
|  DOUBLE        { SpecType Tdouble }
|  SIGNED        { SpecType Tsigned }
|  UNSIGNED      { SpecType Tunsigned }
/* (* There is a conflict here because we don't know whether the LBRACE that 
    * follows is part of the definition of the ENUM or CLASS or is the end of 
    * the declaration part and starts the body of a function. So give these 
    * productions a very low precedence so that the LBRACE is shifted. *) */
|  ENUM ns_id_or_typename               { SpecType 
                                            (Tenum ($2, None))}  %prec COMMA
|  class_head                           { SpecType Tvoid }  %prec COMMA

|  ns_typename                          { SpecType (Tnamed $1) }
|  TYPEOF LPAREN expression RPAREN      { SpecType (TtypeofE $3) } 
|  TYPEOF LPAREN type_name RPAREN       { let s, d = $3 in
                                          SpecType (TtypeofT (s, d)) }

|  ENUM ns_id_or_typename LBRACE enum_list maybecomma RBRACE
                                        { SpecType (Tenum ($2, Some $4)) }
|  ENUM                   LBRACE enum_list maybecomma RBRACE
                                        { SpecType (Tenum ("", Some $3)) }
|  class_spec                           { SpecType Tvoid }
;

decl_spec: 
  decl_spec_start                         { $1 }
| ns_id                                   { SpecName $1 }
| STAR                                    { SpecPtr }
;
 
/* (* Comma-separated list of declarators. Each declarator is a list of 
    * specifiers.  *) */
decl_list: 
  decl_rest                                 { [ $1 ] }
| decl_rest COMMA decl_list                 { $1 :: $3 }
| decl_rest COMMA ELLIPSIS                  { $1 :: [[SpecEllipsis]] }
; 
decl_rest: 
| decl_spec decl_rest                       { $1 :: $2 }
| LPAREN RPAREN decl_rest                   { SpecParen [] :: $3 }
| parameter_list_start_scope decldef parameter_list_end_scope decl_rest
                                            { SpecParen [] :: $4 }
| bracket_comma_expression decl_rest        { SpecArray (smooth_expression $1)
                                                :: $2 }
| LBRACKET RBRACKET decl_rest               { SpecArray NOTHING :: $3 }
| EQ init_expression                        { [SpecInit $2] }
| THROW LPAREN type_id_list RPAREN          { [SpecExc $3] }
| /* done */                                { [] }
;

parameter_list_start_scope:
  LPAREN                                    { Cxxlexer.push_context () }
;
parameter_list_end_scope:
  RPAREN                                    { Cxxlexer.pop_context () }
;
type_id_list: 
   /* nothing */                         { [] }
| type_name COMMA type_id_list           { $1 :: $3 }
;

type_name: 
    decldef                              { match $1 with
                                              [x] -> (x, JUSTBASE)
                                           | _ -> E.s (E.bug "COMMA declarations as a type name") }
;

%%




