/* NOTE: This parser is based on a parser written by Hugues Casse. Since then 
   I have changed it in numerous ways to the point where it probably does not
   resemble Hugues's original one at all */
%{
open Cabs
let version = "Cparser V3.0b 10.9.99 Hugues Cassé"

let parse_error msg : 'a =
  Errormsg.hadErrors := true;
  Clexer.display_error
    msg
    (Parsing.symbol_start ()) (Parsing.symbol_end ())

let print = print_string


(*
let getCurLn () : int = !Clexer.currentLine
         curLine                            

let getCurFn () : string =
        (Clexer.file_name !Clexer.current_handle)
*)
let currentLoc () = { lineno = !Clexer.currentLine; 
                      filename = !Clexer.currentFile;}


(*
** Expression building
*)
let smooth_expression lst =
  match lst with
    [] -> NOTHING
  | [expr] -> expr
  | _ -> COMMA (List.rev lst)

let list_expression expr =
  match expr with
    COMMA lst -> lst
  | NOTHING -> []
  | _ -> [expr]



let currentFunctionName = ref "<outside any function>"
    
let announceFunctionName ((n, _, _):name) =
  Clexer.add_identifier n;
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
                  (b: body) : definition = 
  let fname = (specs, n) in
  FUNDEF (fname, b, loc)


let doOldParDecl (names: string list)
                 (pardefs: name_group list) : single_name list =
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
  List.map findOneName names


let bodyOfBlock ((labels: string list), (body: body)) : body = 
  if labels <> [] then
    parse_error "Local labels declared outside of a statement expression";
  body

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
/* weimer: gcc "__extension__" keyword */
%token EXTENSION
%token DECLSPEC
%token <string> MSASM MSATTR
%token PRAGMA

/* operator precedence */
%nonassoc 	IF
%nonassoc 	ELSE


%nonassoc EXTENSION
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


%type <Cabs.attribute list> attributes attributes_with_asm
%type <Cabs.statement> statement
%type <Cabs.constant> constant
%type <Cabs.expression> expression opt_expression
%type <Cabs.init_expression> init_expression
%type <Cabs.expression list> comma_expression
%type <string> string_list

%type <Cabs.initwhat * Cabs.init_expression> initializer
%type <(Cabs.initwhat * Cabs.init_expression) list> initializer_list
%type <Cabs.initwhat> init_designators init_designators_opt

%type <spec_elem list> decl_spec_list
%type <typeSpecifier> type_spec
%type <Cabs.name_group list> struct_decl_list


%type <Cabs.name> old_proto_decl
%type <Cabs.single_name list> parameter_list
%type <Cabs.single_name> parameter_decl
%type <Cabs.enum_item> enumerator
%type <Cabs.enum_item list> enum_list
%type <Cabs.definition> declaration function_def
%type <cabsloc * spec_elem list * name> function_def_start
%type <Cabs.spec_elem list * Cabs.decl_type> type_name
%type <Cabs.body> block_item_list
%type <string list * Cabs.body> block
%type <string list> local_labels local_label_names
%type <string list> old_parameter_list

%type <Cabs.init_name> init_declarator
%type <Cabs.init_name list> init_declarator_list
%type <Cabs.name> declarator field_decl
%type <Cabs.name list> field_decl_list
%type <string * Cabs.decl_type> direct_decl
%type <Cabs.decl_type> abs_direct_decl abs_direct_decl_opt
%type <Cabs.decl_type * Cabs.attribute list> abstract_decl
%type <attribute list list> pointer pointer_opt /* Each element is a "* <type_quals_opt>" */
%type <Cabs.cabsloc> location
%%

interpret:
  file EOF				{$1}
;
file:
  /* empty */				{[]}
| globals				{List.rev $1}
;
globals:
  global				{[$1]}
| globals global			{$2::$1}
;

location:
   /* empty */                	{ currentLoc () }  %prec IDENT


/*** Global Definition ***/
global:
  declaration  { $1 }
| function_def          { $1 }
| location ASM LPAREN string_list RPAREN SEMICOLON
                        { GLOBASM ($4, $1) }
| location PRAGMA attr  { PRAGMA ($3, $1) }
| location error SEMICOLON { PRAGMA (CONSTANT(CONST_STRING "error"), $1) }
;
typename:
    IDENT				{$1}
|   NAMED_TYPE				{$1}
;
maybecomma:
   /* empty */                          { () }
|  COMMA                                { () }
;

/*** Expressions ****/


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
|		PLUS_PLUS expression %prec CAST
		        {UNARY (PREINCR, $2)}
|		expression PLUS_PLUS
		        {UNARY (POSINCR, $1)}
|		MINUS_MINUS expression %prec CAST
		        {UNARY (PREDECR, $2)}
|		expression MINUS_MINUS
		        {UNARY (POSDECR, $1)}
|		expression ARROW IDENT
		        {MEMBEROFPTR ($1, $3)}
|		expression ARROW NAMED_TYPE
		        {MEMBEROFPTR ($1, $3)}
|		expression DOT IDENT
		        {MEMBEROF ($1, $3)}
|		expression DOT NAMED_TYPE
		        {MEMBEROF ($1, $3)}
|		LPAREN block RPAREN
		        { let labels, body = $2 in GNU_BODY (labels, body) }
|		LPAREN comma_expression RPAREN
		        {(smooth_expression $2)}
|		expression LPAREN opt_expression RPAREN
			{CALL ($1, list_expression $3)}
|		expression LBRACKET comma_expression RBRACKET
			{INDEX ($1, smooth_expression $3)}
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
|               EXTENSION expression  { $2 } 
|		LPAREN type_name RPAREN expression
		         { CAST($2, SINGLE_INIT $4) }
/* (* We handle GCC constructor expressions *) */
|		LPAREN type_name RPAREN LBRACE initializer_list RBRACE
		         { CAST($2, COMPOUND_INIT $5) }
/* (* GCC's address of labels *)  */
|               AND_AND IDENT  { LABELADDR $2 }
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
|    LBRACE initializer_list RBRACE
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
    init_designators EQ init_expression { ($1, $3) }
|                       init_expression { (NEXT_INIT, $1) }
;
init_designators: 
    DOT IDENT init_designators_opt      { INFIELD_INIT($2, $3) }
|   LBRACKET  expression RBRACKET init_designators_opt
                                        { ATINDEX_INIT($2, $4) }
;         
init_designators_opt:
   /* empty */                          { NEXT_INIT }
|  init_designators                     { $1 }
;



opt_expression:
	        /* empty */
	        	{NOTHING}
|	        comma_expression
	        	{smooth_expression $1}
;
comma_expression:
	        expression
	        	{[$1]}
|	        comma_expression COMMA expression
	        	{$3::$1}
;


/*** statements ***/
block: /* ISO 6.8.2 */
    block_begin local_labels block_item_list RBRACE   
                                         {Clexer.pop_context(); ($2, $3) }
;
block_begin:
    LBRACE      		         {Clexer.push_context ()}
;

block_item_list:
    /* empty */                          { [] }
|   declaration block_item_list          {BDEF $1 :: $2 }
|   statement block_item_list            {BSTM $1 :: $2 }
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
|   location block               {BLOCK (bodyOfBlock $2, $1)}
|   location IF LPAREN comma_expression RPAREN statement %prec IF
                	{IF (smooth_expression $4, $6, NOP $1, $1)}
|   location IF LPAREN comma_expression RPAREN statement ELSE statement
	                {IF (smooth_expression $4, $6, $8, $1)}
|   location SWITCH LPAREN comma_expression RPAREN statement
                        {SWITCH (smooth_expression $4, $6, $1)}
|   location WHILE LPAREN comma_expression RPAREN statement
	        	{WHILE (smooth_expression $4, $6, $1)}
|   location DO statement WHILE LPAREN comma_expression RPAREN SEMICOLON
	        	         {DOWHILE (smooth_expression $6, $3, $1)}
|   location FOR LPAREN opt_expression SEMICOLON opt_expression
	        SEMICOLON opt_expression RPAREN statement
	                         {FOR ($4, $6, $8, $10, $1)}
|   location IDENT COLON statement
		                 {LABEL ($2, $4, $1)}
|   location CASE expression COLON
	                         {CASE ($3, NOP $1, $1)}
|   location CASE expression ELLIPSIS expression COLON
	                         {CASERANGE ($3, $5, NOP $1, $1)}
|   location DEFAULT COLON
	                         {DEFAULT (NOP $1, $1)}
|   location RETURN SEMICOLON    {RETURN (NOTHING, $1)}
|   location RETURN expression SEMICOLON
	                         {RETURN ($3, $1)}
|   location BREAK SEMICOLON     {BREAK $1}
|   location CONTINUE SEMICOLON	 {CONTINUE $1}
|   location GOTO IDENT SEMICOLON
		                 {GOTO ($3, $1)}
|   location GOTO STAR expression SEMICOLON 
                                 { COMPGOTO ($4, $1) }
|   location ASM maybevol LPAREN asmtemplate asmoutputs RPAREN SEMICOLON
                        { let (outs,ins,clobs) = $6 in
                          ASM ($5, $3, outs, ins, clobs, $1) }
|   location MSASM               { ASM ([$2], false, [], [], [], $1)}
|   location error   SEMICOLON   { (NOP $1)}
;



/*******************************************************/
/* This is an attempt to clean up the handling of types*/
/*******************************************************/

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
|   EXTENSION decl_spec_list            { $2 }
;
/* In most cases if we see a NAMED_TYPE we must shift it. Thus we declare 
 * NAMED_TYPE to have right associativity */
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
|   STRUCT typename 
                    { Tstruct ($2, None) }
|   STRUCT typename LBRACE struct_decl_list RBRACE
                    { Tstruct ($2, Some $4) }
|   STRUCT           LBRACE struct_decl_list RBRACE
                    { Tstruct ("", Some $3) }
|   UNION typename 
                    { Tunion ($2, None) }
|   UNION typename LBRACE struct_decl_list RBRACE
                    { Tunion ($2, Some $4) }
|   UNION          LBRACE struct_decl_list RBRACE
                    { Tunion ("", Some $3) }
|   ENUM typename   { Tenum ($2, None) }
|   ENUM typename LBRACE enum_list maybecomma RBRACE
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
                                         { ($1, [missingFieldDecl]) :: $3 }
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
|   declarator                            { $1 }
|   declarator COLON expression            
             { (match $1 with
                  (n, JUSTBASE, a) -> ( n, BITFIELD $3, a)
                | (n, d, _) -> Cprint.print_decl n d;
                            parse_error "bitfield not on an integer type"; 
                            raise Parsing.Parse_error) 
             } 
|              COLON expression   { let (n, _, a) = missingFieldDecl in
                                    (n, BITFIELD $2, a)
                                  } 
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
    IDENT                          { ($1, JUSTBASE) }

                                   /* (* We want to be able to redefine named 
                                    * types as variable names *) */
|   NAMED_TYPE                     { ($1, JUSTBASE) }

|   LPAREN attributes declarator RPAREN       
                                   { let (n,decl,al) = $3 in
                                     (n, PARENTYPE($2,decl,al)) } 

|   direct_decl LBRACKET comma_expression RBRACKET
                                   { let (n, decl) = $1 in
                                     (n, ARRAY(decl, smooth_expression $3)) }
|   direct_decl LBRACKET RBRACKET  { let (n, decl) = $1 in
                                     (n, ARRAY(decl, NOTHING)) }
|   direct_decl LPAREN parameter_list RPAREN 
                                   { let (n, decl) = $1 in
                                     (n, PROTO(decl, $3, false)) }
|   direct_decl LPAREN parameter_list_ne COMMA ELLIPSIS RPAREN
                                   { let (n, decl) = $1 in
                                     (n, PROTO(decl, $3, true)) }
;
parameter_list_ne: /* (* ISO 6.7.5 *) */
|   parameter_decl                        { [$1] }
|   parameter_list_ne COMMA parameter_decl   { $1 @ [$3] }
;
parameter_list: 
|   /* empty */                           { [] }
|  parameter_list_ne                      { $1 }
;
parameter_decl: /* (* ISO 6.7.5 *) */
   decl_spec_list declarator              { ($1, $2) }
|  decl_spec_list abstract_decl           { let d, a = $2 in
                                            ($1, ("", d, a)) }
|  decl_spec_list                         { ($1, ("", JUSTBASE, [])) }
;

/* (* Old style prototypes. Like a declarator *) */
old_proto_decl:
  pointer_opt direct_old_proto_decl       { let (n, decl, a) = $2 in
                                            (n, applyPointer $1 decl, a) }
;
direct_old_proto_decl:
  direct_decl LPAREN old_parameter_list RPAREN old_pardef_list
                                   { let par_decl = doOldParDecl $3 $5 in
                                     let n, decl = $1 in
                                     (n, PROTO(decl, par_decl, false), [])
                                   }
;

old_parameter_list: 
|  IDENT                                       { [$1] }
|  error                                       { [] }
|  old_parameter_list COMMA IDENT              { $1 @ [$3]} 
;

old_pardef_list_ne: 
|  decl_spec_list old_pardef SEMICOLON old_pardef_list   
                                     {($1, $2) :: $4 }
;

old_pardef_list: 
   /* empty */                            { [] }
|  old_pardef_list_ne                     { $1 }
;

old_pardef: 
   declarator                             { [$1] }
|  error                                  { [] }
|  declarator COMMA old_pardef            { $1 :: $3 }
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

abs_direct_decl: /* ISO 6.7.6. We do not support optional declarator for 
                  * functions. Plus Microsoft attributes. See the discussion 
                  * for declarator.  */
|   LPAREN attributes abstract_decl RPAREN
                                   { let d, a = $3 in
                                     PARENTYPE ($2, d, a)
                                   }
            
|   abs_direct_decl_opt LBRACKET comma_expression RBRACKET
                                   { ARRAY($1, smooth_expression $3) }
|   abs_direct_decl_opt LBRACKET RBRACKET  { ARRAY($1, NOTHING) }
|   abs_direct_decl LPAREN parameter_list RPAREN 
                                   { PROTO($1, $3, false) }
|   abs_direct_decl LPAREN parameter_list ELLIPSIS RPAREN
                                   { PROTO($1, $3, true) }
;
abs_direct_decl_opt:
    abs_direct_decl                 { $1 }
|   /* empty */                     { JUSTBASE }
;
function_def:  /* (* ISO 6.9.1 *) */
  function_def_start block    
          { let (loc, specs, decl) = $1 in
            currentFunctionName := "<__FUNCTION__ used outside any functions>";
            doFunctionDef loc specs decl (bodyOfBlock $2)
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
| location        IDENT LPAREN parameter_list RPAREN 
                           { let fdec = ($2, PROTO(JUSTBASE, $4, false), []) in
                             announceFunctionName fdec;
                             (* Default is int type *)
                             let defSpec = [SpecType Tint] in
                             ($1, defSpec, fdec) 
                           }
/* (* No return type and old-style parameter list *) */
| location        IDENT LPAREN old_parameter_list RPAREN old_pardef_list
                           { (* Convert pardecl to new style *)
                             let pardecl = doOldParDecl $4 $6 in
                             (* Make the function declarator *)
                             let fdec = ($2, 
                                         PROTO(JUSTBASE, pardecl,false), []) in
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
    ATTRIBUTE LPAREN LPAREN attr_list_ne RPAREN RPAREN	
                                        { ("__attribute__", $4) }
|   DECLSPEC LPAREN attr_list_ne RPAREN    { ("__declspec", $3) }
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
    IDENT                                { VARIABLE $1 }
|   IDENT COLON CST_INT                  { VARIABLE ($1 ^ ":" ^ $3) }
|   DEFAULT COLON CST_INT                { VARIABLE ("default:" ^ $3) }
|   NAMED_TYPE                           { VARIABLE $1 }
                                         /* (* use a VARIABLE "" so that the 
                                             * parentheses are printed *) */
|   IDENT LPAREN  RPAREN                 { CALL(VARIABLE $1, [VARIABLE ""]) }
|   IDENT LPAREN attr_list_ne RPAREN     { CALL(VARIABLE $1, $3) }
|   CST_INT                              { CONSTANT(CONST_INT $1) }
|   string_list                          { CONSTANT(CONST_STRING $1) }
                                           /*(* Const when it appears in 
                                            * attribute lists, is translated 
                                            * to aconst *)*/
|   CONST                                { VARIABLE "aconst" }
;
attr_list_ne:
|  attr                                  { [$1] }
|  attr COMMA attr_list_ne               { $1 :: $3 }
;

/*** GCC ASM instructions ***/
maybevol:
     /* empty */                        { false }
|    VOLATILE                           { true }
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



