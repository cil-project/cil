/* NOTE: in the symbol table, local definition must replace type definition
**		in order to correctly parse local variable in functions body.
**		This is the only way to correctly handle this kind of exception,
**		that is,
**
**		typedef ... ID;
**		int f(int *p) {int ID; return (ID) * *p;}
**		If ID isn't overload, last expression is parsed as a type cast,
**		if it isn't, this a multiplication.
**
** IMPLEMENT:
**		(1) Old-parameter passing style with an exception: 
**                  the first old-style
**		parameter name can't be a type name.
**		(2) GNU __attribute__ modifier, 
**                  GNU ({ }) statement in expression form.
**
** HISTORY
**	1.0	2.19.99	Hugues Cassé	First version.
**	2.0	3.22.99	Hugues Cassé	Large simplification 
**                                      about declarations.
**					"register" parameters added, 
**                                      function pointers,
**					GCC attributes, typedef full supported.
**	2.1	4.23.99	Hugues Cassé	GNU Statement embedded statements 
**                                      managed.
**		a	&x == y was analyzed as ADDROF(EQ(x, y)) corrected
**                      into the right form EQ(ADDROF(x), y)
**		b	typedef struct ID ... ID; is now accepted.
**		c	{v1, v2, v3, } now accepted.
**		d	Spaced string components now accepted. 
**                      Example: "Hel" "lo !".
**	3.0	6.1.99	Hugues Cassé	Solve fully the problem of 
**                                      local/field/parameter with the same 
**                                      identifier to a typedef.
**	a		  const and volatile accepted for basic types
**			  for fields and only-types.
**	b	10.9.99	Hugues Cassé	Correct priorities of type algebra:
**			   ()() > * > []. Add typalg.c for testing it.	
**
**      George Necula. 12/12/00: extended the syntax to process GNU C
**      George Necula. 1/4/00: completely rewrote the parsing of types
*/
%{
open Cabs
let version = "Cparser V3.0b 10.9.99 Hugues Cassé"

let parse_error msg : 'a =
  Clexer.display_error 
    ("Syntax error (" ^ msg ^")") 
    (Parsing.symbol_start ()) (Parsing.symbol_end ());
  raise Parsing.Parse_error

let print = print_string


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
        


let __functionString = (String.make 1 (Char.chr 0)) ^ "__FUNCTION__" 


(* An attempt to clean up the declarations and types *)
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
(* A declaration specifier *)

type specifier = 
    { styp: base_type;
      ssto: storage;
      stypedef: bool;
      sinline: bool;
      sattr: attribute list;            (* Will put qualifiers in here also *)
    } 

let emptySpec = 
  { styp = NO_TYPE;
    ssto = NO_STORAGE;
    stypedef = false;
    sinline = false;
    sattr = [];
  } 

let emptyName = ("", NO_TYPE, [], NOTHING)
let missingFieldDecl = ("___missing_field_name", NO_TYPE, [], NOTHING) 

(* Keep attributes sorted *)
let addAttribute ((n, args) as q) a = 
  let insertBefore (n', _) = 
    if n = n' then None else Some (n < n')
  in
  let rec insertSorted = function
      [] -> [q]
    | (q' :: rest) as current -> begin
        match insertBefore q' with
          None -> current
        | Some true -> q :: current
        | Some false -> q' :: insertSorted rest
    end
  in
  if n = "cdecl" then a else
  insertSorted a

let addAttributes a ats = 
  List.fold_left (fun acc a -> addAttribute a acc) ats a

(* Apply an attribute to a specifier. Keep them sorted *)
let applyAttribute a spec =
  {spec with sattr = addAttribute a spec.sattr}

let applyAttributes a spec =
  {spec with sattr = addAttributes a spec.sattr}

let addAttributeName a ((n,bt,ats,i) : name) : name = 
  (n, bt, addAttribute a ats, i)
  
let addAttributesName a ((n,bt,ats,i) : name) : name = 
  (n, bt, addAttributes a ats, i)
  
let addAttributesTypeOfName a ((n,bt,ats,i) : name) : name = 
  if a = [] then (n,bt,ats,i)
  else match bt with 
    ATTRTYPE (bt', a') -> (n, ATTRTYPE (bt', addAttributes a a'), ats, i)
  | _ -> (n, ATTRTYPE (bt, addAttributes a []), ats, i)
  
(* Make it inline *)
let applyInline spec = {spec with sinline = true}

(* Apply a storage *)
let applyStorage s spec = 
  match spec.ssto with
    NO_STORAGE -> {spec with ssto = s}
  | _ -> parse_error "Multiple storage specifiers"
  
(* Apply typedef *)
let applyTypedef spec = 
  match spec.ssto, spec.stypedef with
    NO_STORAGE, false -> {spec with stypedef = true}
  | _, _ -> parse_error "Typedef along with storage specifier"

let applyInitializer i' ((n,bt,a,i) as name) = 
  match i with 
    NOTHING -> (n, bt, a, i')
  | _ -> parse_error "Multiple initializers"

(* Apply a type specifier. We start with NO_TYPE. ISO 6.7.2 *)
let applyTypeSpec ts spec = 
  let typ = 
    match spec.styp, ts with
      NO_TYPE, Tvoid -> VOID
    | NO_TYPE, Tchar -> INT(CHAR, NO_SIGN)
    | INT(NO_SIZE, sign), Tchar -> INT(CHAR, sign)
    | NO_TYPE, Tint  -> INT(NO_SIZE, NO_SIGN)
    | INT(size, sign), Tint -> spec.styp
    | INT(size, NO_SIGN), Tsigned -> INT(size, SIGNED)
    | INT(size, NO_SIGN), Tunsigned -> INT(size, UNSIGNED)
    | NO_TYPE, Tsigned   -> INT(NO_SIZE, SIGNED)
    | NO_TYPE, Tunsigned -> INT(NO_SIZE, UNSIGNED)
    | NO_TYPE, Tshort -> INT(SHORT, NO_SIGN)
    | INT(NO_SIZE, sign), Tshort -> INT(SHORT, sign)
    | NO_TYPE, Tlong -> INT(LONG, NO_SIGN)
    | NO_TYPE, Tint64 -> INT(LONG_LONG, NO_SIGN)
    | INT(NO_SIZE, sign), Tlong -> INT(LONG, sign)
    | INT(NO_SIZE, sign), Tint64 -> INT(LONG_LONG, sign)
    | INT(LONG, sign), Tlong -> INT(LONG_LONG, sign)
    | DOUBLE false, Tlong -> DOUBLE true
    | NO_TYPE, Tfloat -> FLOAT false
    | NO_TYPE, Tdouble -> DOUBLE false
    | NO_TYPE, Tenum (n, elo) -> begin
        match elo with
          None -> ENUM n
        | Some el -> ENUMDEF (n, el)
    end
    | NO_TYPE, Tstruct (n, fso) -> begin
        match fso with
          None -> STRUCT n
        | Some fs -> STRUCTDEF (n, fs)
     end
    | NO_TYPE, Tunion (n, fso) -> begin
        match fso with
          None -> UNION n
        | Some fs -> UNIONDEF (n, fs)
     end
    | NO_TYPE, Tnamed n -> NAMED_TYPE n         
    | _, _ -> parse_error "Bad combination of type specifiers"
  in
  {spec with styp = typ}

let makeAttrType bt = function
    [] -> bt
  | a -> ATTRTYPE (bt, a)

(* Replace the NO_TYPE in wrapt with the inner *)
let rec injectType inner = function
    NO_TYPE -> inner
  | ATTRTYPE (bt, a) -> ATTRTYPE (injectType inner bt, a)
  | ARRAY (bt, l) -> ARRAY (injectType inner bt, l)
  | PTR bt -> PTR (injectType inner bt)
  | PROTO (bt, args, va, inl) -> PROTO (injectType inner bt, args, va, inl)
  | BITFIELD (bt, e) -> BITFIELD (injectType inner bt, e)
  | _ -> parse_error "Unexpectted wrap type in injectType"
  
let injectTypeName (inner: base_type) ((n,bt,a,i) : name) : name = 
  (n, injectType inner bt, a, i)

let structId = ref 0
let anonStructName n = 
  incr structId;
  "__anon" ^ n ^ (string_of_int (!structId))


let applyPointer (ptspecs: specifier list) ((n,bt,a,i) : name) : name = 
  (* Inner specification first *)
  let rec loop acc = function
      [] -> acc
    | s :: rest -> begin
        if s.styp <> NO_TYPE || s.ssto <> NO_STORAGE ||
           s.stypedef || s.sinline then 
          parse_error "Only qualifiers allowed in pointer";
        loop (makeAttrType (PTR acc) s.sattr) rest
    end
  in
  (n, injectType (loop NO_TYPE ptspecs) bt, a, i)

let typeOfSingleName (_, _, (_, bt, _, _)) = bt

let isTypeAttr (n, _) = 
  match n with 
    "const" | "volatile" | "cdecl" | "stdcall" | "restrict" -> true
  | _ -> false

let nameOfIdent (n: string) (al: attribute list) : name = 
  (n, 
   (if al = [] then NO_TYPE else ATTRTYPE (NO_TYPE, al)), 
   [], NOTHING)
  
let makeNameGroup (spec: specifier) (nl : name list) : name_group = 
  (* Move some GCC attributes from the specifier to names *)
  let typeattr, nameattr = List.partition isTypeAttr spec.sattr in
  let innert = makeAttrType spec.styp typeattr in
  let names = 
    List.map (fun (n,bt,a,i) -> (n, injectType innert bt, 
                                    addAttributes nameattr a, i)) nl in
  (spec.styp, spec.ssto, names)

let makeSingleName (spec: specifier) (n : name) : single_name =
  let (bt, sto, ns) = makeNameGroup spec [n] in
  match ns with 
    [n'] -> (bt, sto, n')
  | _ -> parse_error "makeSingleName: impossible"


let doDeclaration (spec: specifier) (nl: name list) : definition = 
  (* Like a makeNameGroup but also treats the typedef and inline case *)
  let (_, _, names) as ng = makeNameGroup spec nl in
  if spec.stypedef then begin
    (* Tell the lexer about the new type names *)
    List.iter (fun (n, _, _, _) -> Clexer.add_type n) names;
    TYPEDEF ng
  end else
    if nl = [] then
      ONLYTYPEDEF ng
    else
      DECDEF ng

let doFunctionDecl (n: name) (pardecl: single_name list) (va: bool) : name = 
  injectTypeName (PROTO(NO_TYPE, pardecl, va, false)) n

let doFunctionDef (spec: specifier) (n: name) 
                  (b: body) : definition = 
  (match n with 
    (_, _, _, NOTHING) -> ()
  | _ -> parse_error "Initializer in function definition");
  let n' = (* Associate the inline attribute with the function itself *)
    if spec.sinline then
      addAttributeName ("inline", []) n
    else n
  in
  let innert = makeAttrType spec.styp spec.sattr in
  let fname = (spec.styp, spec.ssto, injectTypeName innert n') in
  FUNDEF (fname, b)


let doOldParDecl (names: string list) 
                 (pardefs: name_group list) : single_name list = 
  let findOneName n = 
    (* Search in pardefs for the definition for this parameter *)
    let rec loopGroups = function
        [] -> (* parse_error ("Cannot find definition of parameter " ^
                              n ^ " in old style prototype\n")*)
               (INT(NO_SIZE,NO_SIGN), NO_STORAGE, 
                (n, INT(NO_SIZE,NO_SIGN), [], NOTHING))
      | (bt, st, names) :: restgroups -> 
          let rec loopNames = function
              [] -> loopGroups restgroups
            | ((n',_,_,_) as sn) :: _ when n' = n -> (bt, st, sn)
            | _ :: restnames -> loopNames restnames
          in
          loopNames names
    in
    loopGroups pardefs
  in
  List.map findOneName names

%}

%token <string> IDENT
%token <string> CST_CHAR
%token <string> CST_INT
%token <string> CST_FLOAT
%token <string> CST_STRING
%token <string> NAMED_TYPE

%token EOF
%token CHAR INT DOUBLE FLOAT VOID INT64
%token ENUM STRUCT TYPEDEF UNION
%token SIGNED UNSIGNED LONG SHORT
%token VOLATILE EXTERN STATIC CONST RESTRICT AUTO REGISTER

%token SIZEOF

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

%token ATTRIBUTE INLINE ASM TYPEOF FUNCTION__ 
/* weimer: gcc "__extension__" keyword */
%token EXTENSION
%token STDCALL CDECL
%token <string> MSASM
%token PRAGMA

/* operator precedence */
%nonassoc 	IF
%nonassoc 	ELSE


%left   EXTENSION
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
%right	EXCLAM TILDE PLUS_PLUS MINUS_MINUS CAST RPAREN ADDROF
%left 	LBRACKET
%left	DOT ARROW LPAREN LBRACE SIZEOF
%right  NAMED_TYPE     /* We'll use this to handle redefinitions of 
                        * NAMED_TYPE as variables */
%left   IDENT

/* Non-terminals informations */
%start interpret file
%type <Cabs.definition list> file interpret globals

%type <Cabs.definition> global


%type <Cabs.attribute list> gcc_attributes
%type <Cabs.statement> statement
%type <Cabs.constant> constant
%type <Cabs.expression> expression opt_expression 
%type <Cabs.expression> init_expression
%type <Cabs.expression list> comma_expression
%type <string> string_list

%type <Cabs.init * Cabs.expression> initializer
%type <(Cabs.init * Cabs.expression) list> initializer_list
%type <Cabs.init> init_designators init_designators_opt

%type <specifier> decl_spec_list
%type <typeSpecifier> type_spec
%type <Cabs.name_group list> struct_decl_list 
%type <Cabs.name list> field_decl_list init_declarator_list
%type <Cabs.name> declarator init_declarator direct_decl old_proto_decl
%type <Cabs.name> abs_direct_decl abstract_decl 
%type <specifier list> pointer  /* Each element is a "* <type_quals_opt>" */
%type <Cabs.single_name list> parameter_list
%type <Cabs.single_name> parameter_decl
%type <Cabs.enum_item> enumerator
%type <Cabs.enum_item list> enum_list
%type <Cabs.definition> declaration function_def
%type <Cabs.base_type> type_name
%type <Cabs.body> block block_item_list
%type <string list> old_parameter_list
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


/*** Global Definition ***/
global:
  declaration           { $1 }
| function_def          { $1 }
| ASM LPAREN CST_STRING RPAREN SEMICOLON
                        { GLOBASM $3 }
| PRAGMA attr           { PRAGMA $2 }
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


init_expression:
     expression                                 { $1 }
|    LBRACE initializer_list RBRACE
			{CONSTANT (CONST_COMPOUND $2)}

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
|                       init_expression { (NO_INIT, $1) }
;
init_designators: 
    DOT IDENT init_designators_opt      { FIELD_INIT($2, $3) }
|   LBRACKET  init_expression RBRACKET init_designators_opt
                                        { INDEX_INIT($2, $4) }
;
init_designators_opt:
   /* empty */                          { NO_INIT }
|  init_designators                     { $1 }
;



opt_expression:
		/* empty */
			{NOTHING}
|		comma_expression
			{smooth_expression $1}
;
comma_expression:
		expression
			{[$1]}
|		comma_expression COMMA expression
			{$3::$1}
;
expression:
		constant
			{CONSTANT $1}		
|		IDENT
			{VARIABLE $1}
|		SIZEOF expression
			{EXPR_SIZEOF $2}
|	 	SIZEOF LPAREN type_name RPAREN  
			{TYPE_SIZEOF $3}
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
			{GNU_BODY $2}
|		LPAREN comma_expression RPAREN
			{(smooth_expression $2)}
|		LPAREN type_name RPAREN expression %prec CAST 
			{CAST ($2, $4)}
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
;
constant:
    CST_INT				{CONST_INT $1}
|   CST_FLOAT				{CONST_FLOAT $1}
|   CST_CHAR				{CONST_CHAR $1}
|   string_list				{CONST_STRING $1}
;
string_list:
    CST_STRING				{$1}
|   string_list CST_STRING		{$1 ^ $2}
|   string_list FUNCTION__              {$1 ^ __functionString}
;


/*** statements ***/
block: /* ISO 6.8.2 */
    block_begin block_item_list RBRACE   {Clexer.pop_context(); $2}
;
block_begin:
    LBRACE 				 {Clexer.push_context ()}
;
block_item_list:
    /* empty */                          { [] }
|   declaration block_item_list          { BDEF $1 :: $2  }
|   statement block_item_list            { BSTM $1 :: $2 }
;

statement:
    SEMICOLON		{NOP}  
|   comma_expression SEMICOLON
			{COMPUTATION (smooth_expression $1)}			
|   block		{BLOCK $1}					
|   IF LPAREN comma_expression RPAREN statement %prec IF
			{IF (smooth_expression $3, $5, NOP)}
|   IF LPAREN comma_expression RPAREN statement ELSE statement
			{IF (smooth_expression $3, $5, $7)}
|   SWITCH LPAREN comma_expression RPAREN statement
                        {SWITCH (smooth_expression $3, $5)}
|   WHILE LPAREN comma_expression RPAREN statement
			{WHILE (smooth_expression $3, $5)}
|   DO statement WHILE LPAREN comma_expression RPAREN SEMICOLON
			{DOWHILE (smooth_expression $5, $2)}
|   FOR LPAREN opt_expression SEMICOLON opt_expression
		SEMICOLON opt_expression RPAREN statement
			{FOR ($3, $5, $7, $9)}
|   IDENT COLON statement
			{LABEL ($1, $3)}			
|   CASE expression COLON
			{CASE ($2, NOP)}
|   DEFAULT COLON
			{DEFAULT NOP}		
|   RETURN SEMICOLON	{RETURN NOTHING}		     
|   RETURN expression SEMICOLON
			{RETURN $2}		
|   BREAK SEMICOLON	{BREAK}						
|   CONTINUE SEMICOLON	{CONTINUE}				
|   GOTO IDENT SEMICOLON
			{GOTO $2}				
|   gnuasm  SEMICOLON   { $1 } 
|   MSASM               { ASM ([$1], false, [], [], []) }
;



/*******************************************************/
/* This is an attempt to clean up the handling of types*/
/*******************************************************/

declaration:                                /* ISO 6.7. GCC attributes apply 
                                             * to all declarands on a line. 
                                             * We add them to specifiers and 
                                             * we'll move them to names later 
                                             */
    decl_spec_list init_declarator_list gcc_attributes SEMICOLON 
                                       { doDeclaration 
                                           (applyAttributes $3 $1) $2 }
|   decl_spec_list SEMICOLON { doDeclaration $1 [] }
;
init_declarator_list:                       /* ISO 6.7 */
    init_declarator                              { [$1] }
|   init_declarator COMMA init_declarator_list   { $1 :: $3 }
 
;
init_declarator:                             /* ISO 6.7 */
    declarator                          { $1 }
|   declarator EQ init_expression { applyInitializer $3 $1 }
;

decl_spec_list:                         /* ISO 6.7 */
                                        /* ISO 6.7.1 */
|   TYPEDEF decl_spec_list_opt          { applyTypedef $2 }
/* weimer: gcc allows "__extension__ typedef unsigned long yada ..." */
|   EXTERN decl_spec_list_opt           { applyStorage (EXTERN false) $2 }
|   STATIC  decl_spec_list_opt          { applyStorage (STATIC false) $2 }
|   AUTO   decl_spec_list_opt           { applyStorage AUTO $2 }
|   REGISTER decl_spec_list_opt         { applyStorage REGISTER $2} 
                                        /* ISO 6.7.2 */
|   type_spec decl_spec_list_opt_no_named { applyTypeSpec $1 $2 }
                                        /* ISO 6.7.3 */
|   CONST decl_spec_list_opt            { applyAttribute ("const",[]) $2 }
|   RESTRICT decl_spec_list_opt         { $2 }
|   VOLATILE decl_spec_list_opt         { applyAttribute ("volatile",[]) $2 }
                                        /* ISO 6.7.4 */
|   INLINE decl_spec_list_opt           { applyInline $2 }
|   attribute decl_spec_list_opt        { applyAttributes $1 $2 }
|   EXTENSION decl_spec_list            { $2 }
;
/* In most cases if we see a NAMED_TYPE we must shift it. Thus we declare 
 * NAMED_TYPE to have right associativity */
decl_spec_list_opt: 
    /* empty */                         { emptySpec } %prec NAMED_TYPE
|   decl_spec_list                      { $1 }
;
/* (* We add this separate rule to handle the special case when an appearance 
    * of NAMED_TYPE should not be considered as part of the specifiers but as 
    * part of the declarator. IDENT has higher precedence than NAMED_TYPE  *)
 */
decl_spec_list_opt_no_named: 
    /* empty */                         { emptySpec } %prec IDENT
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
                    { Tstruct (anonStructName "struct", Some $3) }
|   UNION typename 
                    { Tunion ($2, None) }
|   UNION typename LBRACE struct_decl_list RBRACE
                    { Tunion ($2, Some $4) }
|   UNION          LBRACE struct_decl_list RBRACE
                    { Tunion (anonStructName "union", Some $3) }
|   ENUM typename   { Tenum ($2, None) }
|   ENUM typename LBRACE enum_list maybecomma RBRACE
                    { Tenum ($2, Some $4) }
|   ENUM          LBRACE enum_list maybecomma RBRACE
                    { Tenum (anonStructName "enum", Some $3) }
|   NAMED_TYPE      { Tnamed $1 }
;
struct_decl_list: /* (* ISO 6.7.2. Except that we allow empty structs. We 
                      * also allow missing field names. GCC attributes apply 
                      * to all declarands. Put them in the specifier and 
                      * makeNameGroup will move them to the names  *)
                   */
   /* empty */                           { [] }
|  decl_spec_list                 SEMICOLON struct_decl_list
                                         { (makeNameGroup $1 
                                               [missingFieldDecl]) :: $3 }
|  decl_spec_list field_decl_list gcc_attributes SEMICOLON struct_decl_list
                                          { (makeNameGroup 
                                               (applyAttributes $3 $1) $2) 
                                            :: $5 }
;
field_decl_list: /* (* ISO 6.7.2 *) */
    field_decl                           { [$1] }
|   field_decl COMMA field_decl_list     { $1 :: $3 }
;
field_decl: /* (* ISO 6.7.2. Except that we allow unnamed fields. *) */
|   declarator                             { $1 } 
|   declarator COLON expression            {  (match $1 with
                                               (n, t, [], NOTHING) -> 
                                                ( n, BITFIELD (t, $3), 
                                                     [], NOTHING)
                                               | _ -> parse_error "bitfield") 
                                           } 
|              COLON expression            {  (match missingFieldDecl with
                                               (n, t, [], NOTHING) -> 
                                                ( n, BITFIELD (t, $2), 
                                                     [], NOTHING)
                                               | _ -> parse_error "bitfield") 
                                           }
;

enum_list: /* (* ISO 6.7.2.2 *) */
    enumerator				{[$1]}
|   enum_list COMMA enumerator	        {$1 @ [$3]}
;
enumerator:	
    IDENT				{($1, NOTHING)}
|   IDENT EQ expression			{($1, $3)}
;


declarator:  /* (* ISO 6.7.5. Plus Microsoft declarators. The specification 
                says that they are specifiers but in practice they appear to 
                be part of the declarator. Right now we allow them only right 
               before an indentifier (e.g. int __cdecl foo()) or in pointers 
               to functions (e.g. int (__cdecl *foo)(). But they should 
               always be part of the function type attributes not of the 
               declared name's attributes *) */
            direct_decl            { $1 }
|   pointer direct_decl            { applyPointer $1 $2 }
;

direct_decl: /* (* ISO 6.7.5 *) */
    msqual_list_opt IDENT          { nameOfIdent $2 $1 } 
                                   /* (* We want to be able to redefine named 
                                    * types as variable names *) */
|   msqual_list_opt NAMED_TYPE     {Clexer.add_identifier $2;
                                     ($2, 
                                      (if $1 = [] then NO_TYPE
                                       else ATTRTYPE (NO_TYPE, $1)), 
                                      [], NOTHING) }
|   LPAREN declarator RPAREN       { $2 }
|   LPAREN msqual_list pointer direct_decl RPAREN       
                                   { addAttributesTypeOfName $2
                                            (applyPointer $3 $4) } 
|   direct_decl LBRACKET comma_expression RBRACKET
                                   { injectTypeName (ARRAY(NO_TYPE, 
                                                       smooth_expression $3))
                                                    $1 }
|   direct_decl LBRACKET RBRACKET  { injectTypeName (ARRAY(NO_TYPE, 
                                                       NOTHING)) $1}
|   direct_decl LBRACKET STAR RBRACKET { injectTypeName (ARRAY(NO_TYPE, 
                                                       NOTHING)) $1}
|   direct_decl LPAREN parameter_list RPAREN 
                                   { doFunctionDecl $1 $3 false } 
|   direct_decl LPAREN parameter_list_ne COMMA ELLIPSIS RPAREN
                                   { doFunctionDecl $1 $3 true } 
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
   decl_spec_list declarator              { makeSingleName $1 $2 }
|  decl_spec_list abstract_decl           { makeSingleName $1 $2 }
|  decl_spec_list                         { makeSingleName $1 emptyName }
;

/* (* Old style prototypes. Like a declarator *) */
old_proto_decl:
|         direct_old_proto_decl           { $1 } 
| pointer direct_old_proto_decl           { applyPointer $1 $2 }
;
direct_old_proto_decl:
  direct_decl LPAREN old_parameter_list RPAREN old_pardef_list
                                   { let par_decl = doOldParDecl $3 $5 in
                                     doFunctionDecl $1 par_decl false  
                                   }
;

old_parameter_list: 
|  IDENT                                       { [$1] }
|  old_parameter_list COMMA IDENT              { $1 @ [$3]} 
;

old_pardef_list_ne: 
|  decl_spec_list old_pardef SEMICOLON old_pardef_list   
                                     {(makeNameGroup $1 $2) :: $4 }
;

old_pardef_list: 
   /* empty */                            { [] }
|  old_pardef_list_ne                     { $1 }
;

old_pardef: 
   declarator                             { [$1] }
|  declarator COMMA old_pardef            { $1 :: $3 }
;


pointer: /* (* ISO 6.7.5 *) */ 
   STAR decl_spec_list_opt_no_named          { [$2] }
|  STAR decl_spec_list_opt_no_named pointer  { $2 :: $3 }
;

type_name: /* (* ISO 6.7.6 *) */
  decl_spec_list abstract_decl { typeOfSingleName (makeSingleName $1 $2) }
| decl_spec_list               { typeOfSingleName 
                                           (makeSingleName $1 emptyName) }
| TYPEOF expression            {TYPEOF $2} 
;
abstract_decl: /* (* ISO 6.7.6. *) */
  pointer abs_direct_decl_opt        { applyPointer $1 $2 }
|         abs_direct_decl            { $1 }
;

abs_direct_decl: /* ISO 6.7.6. We do not support optional declarator for 
                  * functions. Plus Microsoft attributes. See the discussion 
                  * for declarator.  */
|   LPAREN abstract_decl RPAREN    { $2 }
|   LPAREN msqual_list pointer abs_direct_decl_opt RPAREN
                                   { addAttributesTypeOfName $2
                                        (applyPointer $3 $4) }
            
|   abs_direct_decl_opt LBRACKET comma_expression RBRACKET
                                   { injectTypeName (ARRAY(NO_TYPE, 
                                                       smooth_expression $3))
                                                    $1 }
|   abs_direct_decl_opt LBRACKET RBRACKET  { injectTypeName (ARRAY(NO_TYPE, 
                                                         NOTHING)) $1}
|   abs_direct_decl_opt LBRACKET STAR RBRACKET { injectTypeName 
                                                  (ARRAY(NO_TYPE, 
                                                         NOTHING)) $1}
|   abs_direct_decl LPAREN parameter_list RPAREN 
                                   { doFunctionDecl $1 $3 false } 
|   abs_direct_decl LPAREN parameter_list ELLIPSIS RPAREN
                                   { doFunctionDecl $1 $3 true }
;
abs_direct_decl_opt:
    abs_direct_decl                 { $1 }
|   /* empty */                     { emptyName }
;
function_def:  /* (* ISO 6.9.1 *) */
  decl_spec_list declarator block { doFunctionDef $1 $2 $3 }
/* (* Old-style function prototype *) */
| decl_spec_list old_proto_decl block  { doFunctionDef $1 $2 $3 } 
/* (* New-style function that does not have a return type *) */
|          IDENT LPAREN parameter_list RPAREN block
                           { let name = nameOfIdent $1 [] in
                             let fdec = doFunctionDecl name $3 false in
                             (* Default is int type *)
                             let defSpec = applyTypeSpec Tint emptySpec in
                             doFunctionDef defSpec fdec $5 }
/* (* No return type and old-style parameter list *) */
|          IDENT LPAREN old_parameter_list RPAREN old_pardef_list block
                           { let name = nameOfIdent $1 [] in
                             (* Convert pardecl to new style *)
                             let pardecl = doOldParDecl $3 $5 in
                             (* Make the function declarator *)
                             let fdec = doFunctionDecl name pardecl false in
                             (* Default is int type *)
                             let defSpec = applyTypeSpec Tint emptySpec in
                             doFunctionDef defSpec fdec $6 }
;


/* Microsoft specific qualifiers */
msqual:
    CDECL                           { "cdecl", [] }
|   STDCALL                         { "stdcall", [] }
;
msqual_list:
   msqual                           { [$1] }
|  msqual msqual_list               { $1 :: $2 }
;
msqual_list_opt:
   /* empty */                      { [] }
|  msqual_list                      { $1 }
;
/*** GCC attributes ***/
gcc_attributes:
    /* empty */						{[]}	
|   gcc_neattributes		{ $1 }
;

gcc_neattributes:
    attribute				{ $1}
|   gcc_neattributes attribute		{$1 @ $2}
;

attribute:
    ATTRIBUTE LPAREN LPAREN attrs RPAREN RPAREN	{ $4 }
;
attrs:
    attr				{[$1]}
|   attr COMMA attrs			{$1::$3}
;
attr:
    IDENT				{ ($1,[]) }
|   IDENT COLON CST_INT                 { ($1 ^ ":" ^ $3, []) }
|   IDENT LPAREN args RPAREN		{ ($1, $3) }
;

args:
    expression                           { [$1] }
|   IDENT COLON CST_INT                  { [VARIABLE ($1 ^ ":" ^ $3)] }
|   DEFAULT COLON CST_INT                { [VARIABLE ("default:" ^ $3)] }
|   expression COMMA args                { $1 :: $3 }
;

/*** GCC ASM instructions ***/
gnuasm: 
   ASM maybevol LPAREN asmtemplate asmoutputs RPAREN
                        { let (outs,ins,clobs) = $5 in
                          ASM ($4, $2, outs, ins, clobs) }
;
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
asminputs: 
  /* empty */                { ([], []) }
| COLON asmoperands asmclobber
                        { ($2, $3) }
;
asmclobber:
    /* empty */                         { [] }
| COLON asmtemplate                     { $2 }
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
     CST_STRING LPAREN expression RPAREN    { ($1, $3) }
; 

%%



