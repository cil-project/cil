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
*/
%{
open Cabs
let version = "Cparser V3.0b 10.9.99 Hugues Cassé"

let parse_error msg =
  Clexer.display_error 
    "Syntax error" (Parsing.symbol_start ()) (Parsing.symbol_end ())

exception BadType
let badType tp where = 
  Clexer.display_error 
    ("Syntax error: unexpected type" ^ where)
    (Parsing.symbol_start ()) (Parsing.symbol_end ());
  raise BadType


(*
** Type analysis
*)
type modifier =
    BASE_SIZE of size
  | BASE_SIGN of sign
  | BASE_STORAGE of storage
  | BASE_VOLATILE
  | BASE_CONST

exception BadModifier
let badModifier (modi : modifier) where = 
  Clexer.display_error 
    ("Syntax error: unexpected modifier" ^ where)
    (Parsing.symbol_start ()) (Parsing.symbol_end ());
  raise BadModifier
      
let apply_mod (typ, sto) modi =
  let rec mod_root typ =
    match (typ, modi) with
      (NO_TYPE, BASE_SIGN sign) -> INT (NO_SIZE, sign)
    | (NO_TYPE, BASE_SIZE size) -> INT (size, NO_SIGN)
    | (INT (NO_SIZE, sign), BASE_SIZE size) -> INT (size, sign)
    | (INT (LONG, sign), BASE_SIZE LONG) -> INT (LONG_LONG, sign)
    | (INT (size, NO_SIGN), BASE_SIGN sign) -> INT (size, sign)
    | (BITFIELD (NO_SIGN, exp), BASE_SIGN sign) -> BITFIELD (sign, exp)
    | (FLOAT false, BASE_SIZE LONG) -> FLOAT true
    | (DOUBLE false, BASE_SIZE LONG) -> DOUBLE true
    | (PTR typ, _) -> PTR (mod_root typ)
    | (CONST typ, _) -> CONST (mod_root typ)
    | (VOLATILE typ, _) -> VOLATILE (mod_root typ)
    | _ -> badModifier modi "1"
  in
  let check_access typ =
    match typ with
      PROTO _ | OLD_PROTO _ | VOLATILE _ -> false
    | _ -> true in
  match modi with
    BASE_SIGN _ -> (mod_root typ, sto)
  | BASE_SIZE _ -> (mod_root typ, sto)
  | BASE_CONST ->
      if (check_access typ) then (CONST typ, sto)
      else badModifier modi "2"
  | BASE_VOLATILE ->
      if (check_access typ) then (VOLATILE typ, sto)
      else badModifier modi "3"
  | BASE_STORAGE INLINE -> begin
      match sto with
        NO_STORAGE -> (typ, INLINE)
      | STATIC _ -> (typ, STATIC true)
      | EXTERN _ -> (typ, EXTERN true)
      | _ -> badModifier modi "4"
  end
  | BASE_STORAGE (EXTERN false) -> begin
      match sto with
        NO_STORAGE -> (typ, EXTERN false)
      | INLINE -> (typ, EXTERN true)
      |  _ -> badModifier modi "5"
  end
  | BASE_STORAGE sto' ->
      if sto = NO_STORAGE then (typ, sto')
      else badModifier modi "6"
                    
let apply_mods mods fty =
  List.fold_left apply_mod fty mods
      
let set_type tst tin =
  let rec set typ =
    match typ with
      NO_TYPE -> tst
    | PTR typ -> PTR (set typ)
    | ARRAY (typ, dim) -> ARRAY (set typ, dim)
    | PROTO (typ, pars, ell) -> PROTO (set typ, pars, ell)
    | OLD_PROTO (typ, pars, ell) -> OLD_PROTO (set typ, pars, ell)
    | CONST typ -> CONST (set typ)
    | VOLATILE typ -> VOLATILE (set typ)
    | BITFIELD (NO_SIGN, exp) ->
	(match tst with
	  INT (_, sign) -> BITFIELD (sign, exp)
	| _ -> badType tst "1")
    | _ -> badType typ "2" in
  set tin
    

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
        

(*** Named Building ***)
let set_name (typ : base_type) (id, typ', attr, exp) =
  (id, set_type typ typ', attr, exp)
	
let set_name_group (typ, sto) (lst : name list) : name_group =
  (typ, sto, List.map (set_name typ) lst)
	
let set_single (typ, sto) name : single_name =
	(typ, sto, set_name typ name)

let set_data (id, typ, attr, _) ini = (id, typ, attr, ini)

let apply_qual ((t1, q1) : base_type * modifier list)
    ((t2, q2) : base_type * modifier list)
    : base_type * modifier list =
  ((if t1 = NO_TYPE then t2 else
  if t2 = NO_TYPE then t1 else  raise BadModifier),
   List.append q1 q2)
%}

%token <string> IDENT
%token <string> CST_CHAR
%token <string> CST_INT
%token <string> CST_FLOAT
%token <string> CST_STRING
%token <string> NAMED_TYPE

%token EOF
%token CHAR INT DOUBLE FLOAT VOID
%token ENUM STRUCT TYPEDEF UNION
%token SIGNED UNSIGNED LONG SHORT
%token VOLATILE EXTERN STATIC CONST AUTO REGISTER

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

%token ATTRIBUTE INLINE ASM TYPEOF

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
%left	STAR SLASH PERCENT CONST VOLATILE
%right	EXCLAM TILDE PLUS_PLUS MINUS_MINUS CAST RPAREN ADDROF
%left 	LBRACKET
%left	DOT ARROW LPAREN LBRACE SIZEOF

/* Non-terminals informations */
%start interpret file
%type <Cabs.definition list> file interpret globals

%type <Cabs.definition> global
%type <Cabs.base_type * Cabs.storage> global_type
%type <Cabs.base_type * modifier list> global_qual
%type <modifier list> global_mod_list_opt global_mod_list
%type <modifier> global_mod
%type <Cabs.name list> global_defs
%type <Cabs.name> global_def
%type <string * Cabs.base_type> global_dec

%type <Cabs.definition> local
%type <Cabs.base_type * Cabs.storage> local_type
%type <Cabs.base_type * modifier list> local_qual
%type <modifier list> local_mod_list_opt local_mod_list
%type <modifier> local_mod
%type <Cabs.name list> local_defs
%type <Cabs.name> local_def
%type <string * Cabs.base_type> local_dec

%type <Cabs.attributes> gcc_attributes
%type <Cabs.definition list * Cabs.statement> body
%type <Cabs.statement> statement opt_stats stats
%type <Cabs.constant> constant
%type <Cabs.expression> expression init_expression opt_expression
%type <Cabs.expression list> comma_expression init_comma_expression
%type <Cabs.single_name list * bool> parameters
%type <string> string_list


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
  global_type global_defs SEMICOLON
			{DECDEF (set_name_group $1 (List.rev $2))}
| global_type global_proto body
                        {FUNDEF (set_single $1 $2, $3)}
| global_type old_proto old_pardefs body
			{OLDFUNDEF (set_single $1 $2, List.rev $3, $4)}
| global_type SEMICOLON
			{ONLYTYPEDEF (set_name_group $1 [])}
| TYPEDEF typedef_type typedef_defs SEMICOLON
			{let _ = 
                          List.iter (fun (id, _, _, _) -> Clexer.add_type id) 
                            $3 in
			TYPEDEF (set_name_group $2 $3)}
| ASM LPAREN CST_STRING RPAREN
                        { GLOBASM $3 }
;
global_type:
    global_mod_list_opt gcc_attributes global_qual
			{apply_mods (snd $3) 
                            (apply_mods $1 ((fst $3), NO_STORAGE))}
|   global_mod_list_opt comp_type global_mod_list_opt
			{apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
|   global_mod_list_opt NAMED_TYPE global_mod_list_opt
			{apply_mods $3 
                            (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
|   global_mod_list_opt
			{apply_mods $1 (NO_TYPE, NO_STORAGE)}
;
global_mod_list_opt:
    /* empty */				{[]}
|   global_mod_list		        {List.rev $1}
;
global_mod_list:
    global_mod				{[$1]}
|   global_mod_list global_mod		{$2::$1}
;
global_mod:
    STATIC				{BASE_STORAGE (STATIC false)}
|   CONST				{BASE_CONST}
|   VOLATILE				{BASE_VOLATILE}
|   EXTERN				{BASE_STORAGE (EXTERN false)}
|   INLINE                              {BASE_STORAGE INLINE}
;
global_qual:
    qual_type					{$1}
|   global_qual qual_type			{apply_qual $1 $2}
|   global_qual global_mod			{(fst $1, $2::(snd $1))}
;
global_defs:
    global_def				{[$1]}
|   global_defs COMMA global_def	{$3::$1}
;
global_def:
    global_dec gcc_attributes
			{(fst $1, snd $1, $2, NOTHING)}
|   global_dec gcc_attributes EQ init_expression
			{(fst $1, snd $1, $2, $4)}
;
global_dec:
    IDENT 		{($1, NO_TYPE)}	
|   LPAREN global_dec RPAREN
			{$2}
|   STAR global_dec
			{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STAR CONST global_dec
			{(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE global_dec
			{(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
|   global_dec LBRACKET comma_expression RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, smooth_expression $3)) 
                            (snd $1))}
|   global_dec LBRACKET RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
|   global_dec LPAREN parameters RPAREN			
			{(fst $1, PROTO (snd $1, fst $3, snd $3))}
|   LPAREN global_dec RPAREN LPAREN parameters RPAREN
			{(fst $2, 
                          set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
|   global_dec LPAREN old_parameters RPAREN
			{(fst $1, OLD_PROTO (snd $1, fst $3, snd $3))}
|   LPAREN global_dec RPAREN LPAREN old_parameters RPAREN
			{(fst $2, 
                          set_type (OLD_PROTO (NO_TYPE, fst $5, snd $5)) 
                            (snd $2))}
;
global_proto:
    global_dec gcc_attributes
			{match (snd $1) with
				PROTO _ -> (fst $1, snd $1, $2, NOTHING)
				| x -> badType x "3"}
;
old_proto:
    global_dec gcc_attributes
			{match (snd $1) with
				OLD_PROTO _ -> (fst $1, snd $1, $2, NOTHING)
				| x -> badType x "4"}
;


/*** Old Parameter Style ***/
old_parameters:
    old_pardecs				{(List.rev $1, false)}
|   old_pardecs ELLIPSIS		{(List.rev $1, true)}
;
old_pardecs:
    IDENT				{[$1]}
|   old_pardecs COMMA IDENT		{$3::$1}
|   old_pardecs COMMA NAMED_TYPE	{$3::$1}
;

old_pardefs:
    old_pardef				{[$1]}
|   old_pardefs old_pardef		{$2::$1}
;
old_pardef:
    old_type old_defs SEMICOLON         {set_name_group $1 (List.rev $2)}
;
old_type:
    old_mods_opt NAMED_TYPE old_mods_opt
			{apply_mods $3 (apply_mods $1 
                                          (NAMED_TYPE $2, NO_STORAGE))}
|   old_mods_opt comp_type old_mods_opt
			{apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
|  old_mods_opt old_qual
			{apply_mods (snd $2) 
                            (apply_mods $1 ((fst $2), NO_STORAGE))}
;
old_mods_opt:
    /* empty */				{[]}
|   CONST				{[BASE_CONST]}
|   REGISTER				{[BASE_STORAGE REGISTER]}
;
old_qual:
    qual_type				{$1}
|   old_qual qual_type			{apply_qual $1 $2}
|   old_qual CONST			{(fst $1, BASE_CONST::(snd $1))}
|   old_qual REGISTER			{(fst $1, 
                                          (BASE_STORAGE REGISTER)::(snd $1))}
;
old_defs:
    old_def				{[$1]}
|   old_defs COMMA old_def		{$3::$1}
;
old_def:
    old_dec                             {(fst $1, snd $1, [], NOTHING)}
;
old_dec:
    IDENT			{($1, NO_TYPE)}
|   STAR old_dec		{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STAR CONST old_dec		{(fst $3, 
                                  set_type (CONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE old_dec	{(fst $3, 
                                  set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
|   old_dec LBRACKET comma_expression RBRACKET
			        {(fst $1, 
                                  set_type (ARRAY (NO_TYPE, 
                                                   smooth_expression $3)) 
                                    (snd $1))}
|   old_dec LBRACKET RBRACKET	{(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) 
                                    (snd $1))}
|  old_dec LPAREN parameters RPAREN				
        			{(fst $1, PROTO (snd $1, fst $3, snd $3))}
|  LPAREN old_dec RPAREN LPAREN parameters RPAREN
                                {(fst $2, set_type (PROTO (NO_TYPE, 
                                                           fst $5, snd $5)) 
                                    (snd $2))}
|  LPAREN old_dec RPAREN	{$2}
;


/*** Local Definition ***/
local:
    local_type local_defs SEMICOLON
			{DECDEF (set_name_group $1 (List.rev $2))}
;
local_type:
    local_mod_list_opt local_qual
                        {apply_mods (snd $2) 
                            (apply_mods $1 ((fst $2), NO_STORAGE))}
|   local_mod_list_opt comp_type local_mod_list_opt
			{apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
|   local_mod_list_opt NAMED_TYPE local_mod_list_opt
			{apply_mods $3 
                            (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
;
local_mod_list_opt:						
    /* empty */				{[]}
|   local_mod_list			{List.rev $1}
;
local_mod_list:
    local_mod				{[$1]}
|   local_mod_list local_mod		{$2::$1}
;
local_mod:
    STATIC				{BASE_STORAGE (STATIC false)}
|   AUTO				{BASE_STORAGE AUTO}
|   CONST				{BASE_CONST}
|   VOLATILE				{BASE_VOLATILE}
|   REGISTER				{BASE_STORAGE REGISTER}
|   EXTERN				{BASE_STORAGE (EXTERN false)}
;
local_qual:
    qual_type				{$1}
|   local_qual qual_type		{apply_qual $1 $2}
|   local_qual local_mod		{(fst $1, $2::(snd $1))}
;
local_defs:
    local_def				{[$1]}
|   local_defs COMMA local_def		{$3::$1}
;
local_def:
    local_dec gcc_attributes
			{(fst $1, snd $1, $2, NOTHING)}
|   local_dec gcc_attributes EQ init_expression
			{(fst $1, snd $1, $2, $4)}
;
local_dec:
    IDENT		{($1, NO_TYPE)}
|   NAMED_TYPE		{Clexer.add_identifier $1;($1, NO_TYPE)}
|   STAR local_dec	{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STAR CONST local_dec
			{(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE local_dec
			{(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
|   local_dec LBRACKET comma_expression RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, smooth_expression $3)) 
                            (snd $1))}
|   local_dec LBRACKET RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
|   local_dec LPAREN parameters RPAREN
			{(fst $1, PROTO (snd $1, fst $3, snd $3))}
|   LPAREN local_dec RPAREN LPAREN parameters RPAREN
			{(fst $2, 
                          set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
|   LPAREN local_dec RPAREN
			{$2}
;


/*** Typedef Definition ***/
typedef_type:
    typedef_sub		{apply_mods (snd $1) ((fst $1), NO_STORAGE)}
|   CONST typedef_sub	{apply_mods (BASE_CONST::(snd $2)) 
                            ((fst $2), NO_STORAGE)}
;
typedef_sub:
    NAMED_TYPE			{(NAMED_TYPE $1, [])}
|   NAMED_TYPE CONST		{(NAMED_TYPE $1, [BASE_CONST])}
|   comp_type			{($1, [])}		
|   comp_type CONST		{($1, [BASE_CONST])}
|   typedef_qual		{$1}
;

typedef_qual:
   qual_type			{$1}
|  typedef_qual qual_type	{apply_qual $1 $2}
|  typedef_qual CONST		{(fst $1, BASE_CONST::(snd $1))}
;
typedef_defs:
    typedef_def				{[$1]}
|   typedef_defs COMMA typedef_def	{$3::$1}
;
typedef_def:
    typedef_dec			{(fst $1, snd $1, [], NOTHING)}
;
typedef_dec:
    IDENT		{($1, NO_TYPE)}
|   STAR typedef_dec	{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STAR CONST typedef_dec   
                        {(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE typedef_dec
			{(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
|   typedef_dec LBRACKET comma_expression RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, smooth_expression $3)) 
                            (snd $1))}
|   typedef_dec LBRACKET RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
|   typedef_dec LPAREN parameters RPAREN
			{(fst $1, PROTO (snd $1, fst $3, snd $3))}
|   LPAREN typedef_dec RPAREN LPAREN parameters RPAREN
			{(fst $2, 
                          set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
|   LPAREN typedef_dec RPAREN	{$2}
;


/*** Field Definition ***/
field_list:
    /* empty */                                 { [] }
|   field_list field                            { $2 :: $1 }
;

field:
    field_type field_defs SEMICOLON    {set_name_group $1 (List.rev $2)}
;
field_type:
    field_mod_list_opt field_qual 
      {apply_mods (snd $2) (apply_mods $1 ((fst $2), NO_STORAGE))}
|  field_mod_list_opt comp_type field_mod_list_opt
      {apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
|  field_mod_list_opt	NAMED_TYPE field_mod_list_opt
      {apply_mods $3 (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
;
field_mod_list_opt:
	/* empty */			   {[]}
|	field_mod_list			   {List.rev $1}
;
field_mod_list:
	field_mod				{[$1]}
|	field_mod_list field_mod		{$2::$1}
;
field_mod:
	CONST			       {BASE_CONST}
|	VOLATILE		       {BASE_VOLATILE}
;
field_qual:	
	qual_type			{$1}
|	field_qual qual_type		{apply_qual $1 $2}
|	field_qual field_mod		{(fst $1, $2::(snd $1))}
;
field_defs:
    field_defs COMMA field_def	                {$3::$1}	
|   field_def			                {[$1]}
;
field_def:
    field_dec gcc_attributes	{(fst $1, snd $1, $2, NOTHING)}
;
field_dec:
    IDENT			{($1, NO_TYPE)}
|   NAMED_TYPE			{($1, NO_TYPE)}
|   STAR field_dec		{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STAR CONST field_dec	{(fst $3, 
                                  set_type (CONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE field_dec	{(fst $3, 
                                  set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
|   field_dec LBRACKET comma_expression RBRACKET
                                {(fst $1, 
                                  set_type (ARRAY (NO_TYPE, 
                                                   smooth_expression $3)) 
                                    (snd $1))}
|   field_dec LBRACKET RBRACKET	{(fst $1, 
                                  set_type (ARRAY (NO_TYPE, NOTHING)) 
                                    (snd $1))}
|   field_dec LPAREN parameters RPAREN
                                {(fst $1, PROTO (snd $1, fst $3, snd $3))}
|   LPAREN field_dec RPAREN LPAREN parameters RPAREN
	      		        {(fst $2, 
                                  set_type (PROTO (NO_TYPE, fst $5, snd $5)) 
                                    (snd $2))}
|  LPAREN field_dec RPAREN	{$2}
|  IDENT COLON expression	{($1, BITFIELD (NO_SIGN, $3))}
|  COLON expression             {("___missing_field_name", 
                                  BITFIELD (NO_SIGN, $2))}
;


/*** Parameter Definition ***/
parameters:
    /* empty */				{([], false)}
|   param_list				{(List.rev $1, false)}
|   param_list COMMA ELLIPSIS		{(List.rev $1, true)}
;
param_list:
    param_list COMMA param 	        {$3::$1}
|   param				{[$1]}
;
param:
    param_type param_def		{set_single $1 $2}
;
param_type:
    param_mods_opt NAMED_TYPE param_mods_opt
			{apply_mods $3 
                            (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
|   param_mods_opt comp_type param_mods_opt
			{apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
|   param_mods_opt param_qual
			{apply_mods (snd $2) 
                            (apply_mods $1 ((fst $2), NO_STORAGE))}
;
param_mods_opt:
    /* empty */				{[]}
|   param_mods				{List.rev $1}
;
param_mods:
    param_mod				{[$1]}
|   param_mods param_mod		{$2::$1}
;
param_mod:
    CONST				{BASE_CONST}
|   REGISTER				{BASE_STORAGE REGISTER}
|   VOLATILE				{BASE_VOLATILE}
;
param_qual:
    qual_type				{$1}
|   param_qual qual_type		{apply_qual $1 $2}
|   param_qual CONST			{(fst $1, BASE_CONST::(snd $1))}
|   param_qual REGISTER			{(fst $1, 
                                          (BASE_STORAGE REGISTER)::(snd $1))}
|   param_qual VOLATILE			{(fst $1, BASE_VOLATILE::(snd $1))}
;
param_def:
    param_dec gcc_attributes		{(fst $1, snd $1, $2, NOTHING)}
;
param_dec:
    /* empty */		{("", NO_TYPE)}
|   IDENT		{($1, NO_TYPE)}
|   NAMED_TYPE		{($1, NO_TYPE)}
|   STAR param_dec	{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STAR CONST param_dec
			{(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))} 
|   STAR VOLATILE param_dec
			{(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
|   param_dec LBRACKET comma_expression RBRACKET
			{(fst $1, set_type (ARRAY (NO_TYPE, 
                                                   smooth_expression $3)) 
                            (snd $1))}
|   param_dec LBRACKET RBRACKET
			{(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) 
                            (snd $1))}
|   LPAREN param_dec RPAREN LPAREN parameters RPAREN
			{(fst $2, set_type (PROTO (NO_TYPE, fst $5, snd $5)) 
                            (snd $2))}
|   LPAREN param_dec RPAREN	{$2}
;
		

/*** Only-type Definition ***/
only_type:
    only_type_type only_dec		{set_type (fst $1) $2}
|   TYPEOF expression                   {TYPEOF $2} 
;
only_type_type:
   only_mod_list_opt only_qual
		{apply_mods (snd $2) (apply_mods $1 ((fst $2), NO_STORAGE))}
|  only_mod_list_opt comp_type only_mod_list_opt
		{apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
|  only_mod_list_opt NAMED_TYPE only_mod_list_opt
		{apply_mods $3 (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
;
only_mod_list_opt:
    /* empty */				{[]}
|   only_mod_list			{List.rev $1}
;
only_qual:	
    qual_type				{$1}
|   only_qual qual_type			{apply_qual $1 $2}
|   only_qual only_mod			{(fst $1, $2::(snd $1))}
;
only_mod_list:
    only_mod				{[$1]}
|   only_mod_list only_mod		{$2::$1}
;
only_mod:
    CONST				{BASE_CONST}
|   VOLATILE				{BASE_VOLATILE}
;
only_dec:
    /* empty */		{NO_TYPE}
|   STAR only_dec	{set_type (PTR NO_TYPE) $2}
|   STAR CONST only_dec	{set_type (CONST (PTR NO_TYPE)) $3}
|   STAR VOLATILE only_dec
			{set_type (VOLATILE (PTR NO_TYPE)) $3}
|   only_dec LBRACKET comma_expression RBRACKET
			{set_type (ARRAY (NO_TYPE, smooth_expression $3)) $1}
|   only_dec LBRACKET RBRACKET
			{set_type (ARRAY (NO_TYPE, NOTHING)) $1}
|   LPAREN only_dec RPAREN LPAREN parameters RPAREN
			{set_type (PROTO (NO_TYPE, fst $5, snd $5)) $2}
|   LPAREN only_dec RPAREN
			{$2}
;


/*** Base type ***/
qual_type:
    VOID				{(VOID, [])}
|   CHAR				{(NO_TYPE, [BASE_SIZE CHAR])}
|   INT					{(INT (NO_SIZE, NO_SIGN), [])}
|   FLOAT				{(FLOAT false, [])}
|   DOUBLE				{(DOUBLE false, [])}
|   LONG				{(NO_TYPE, [BASE_SIZE LONG])}
|   SHORT				{(NO_TYPE, [BASE_SIZE SHORT])}
|   SIGNED				{(NO_TYPE, [BASE_SIGN SIGNED])}
|   UNSIGNED				{(NO_TYPE, [BASE_SIGN UNSIGNED])}
;
comp_type:
    STRUCT type_name	{STRUCT ($2, [])}				
|   STRUCT LBRACE field_list RBRACE
			{STRUCT ("", List.rev $3)}
|   STRUCT type_name LBRACE field_list RBRACE
			{STRUCT ($2, List.rev $4)}
|   UNION type_name	{UNION ($2, [])} 
|   UNION LBRACE field_list RBRACE			
			{UNION ("", List.rev $3)}
|   UNION type_name LBRACE field_list RBRACE
			{UNION ($2, List.rev $4)}
|   ENUM type_name
			{ENUM ($2, [])}					
|   ENUM LBRACE enum_list maybecomma RBRACE
			{ENUM ("", List.rev $3)}		
|   ENUM type_name LBRACE enum_list maybecomma RBRACE
			{ENUM ($2, List.rev $4)}
;
type_name:
    IDENT				{$1}
|   NAMED_TYPE				{$1}
;
enum_list:	
    enum_name				{[$1]}
|   enum_list COMMA enum_name	        {$3::$1}
;
maybecomma:
   /* empty */                          { () }
|  COMMA                                { () }
enum_name:	
    IDENT				{($1, NOTHING)}
|   IDENT EQ expression			{($1, $3)}
;


/*** Expressions ****/

/* Separate initializer expresion because they have the special 
 * structure initializer */
init_expression:
     LBRACE init_comma_expression RBRACE
			{CONSTANT (CONST_COMPOUND (List.rev $2))}
/*
|    LPAREN init_expression RPAREN
                        {$2}
|    LPAREN only_type RPAREN init_expression
                        {CAST ($2, $4)}
*/
|    expression		{$1}
;
init_comma_expression:
    init_expression	                          {[$1]}
|   init_comma_expression COMMA init_expression	  {$3::$1}
|   init_comma_expression COMMA			  {$1}
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
|	 	SIZEOF LPAREN only_type RPAREN
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
|		LPAREN body RPAREN
			{GNU_BODY $2}
|		LPAREN comma_expression RPAREN
			{(smooth_expression $2)}
|		LPAREN only_type RPAREN expression %prec CAST
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
;


/*** statements ***/
body_begin:
    LBRACE 				{Clexer.push_context ()}
;
body_middle:
    opt_locals opt_stats		{($1, $2)}
;
body:
    body_begin body_middle RBRACE 	{Clexer.pop_context(); $2}
;
opt_locals:
    /* empty */				{[]}
|   locals				{List.rev $1}
;
locals:
    local				{[$1]}
|   locals local			{$2::$1}
;
opt_stats:
    /* empty */				{NOP}
|   stats				{$1}
;
stats:
    statement				{$1}
|   stats statement			{SEQUENCE($1, $2)}
;
statement:
    SEMICOLON		{NOP}  
|   comma_expression SEMICOLON
			{COMPUTATION (smooth_expression $1)}			
|   body		{BLOCK $1}					
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
;


/*** GCC attributes ***/
gcc_attributes:
    /* empty */						{[]}	
|   attributes						{ $1 }
;

attributes:
    attribute				{ $1}
|   attributes attribute		{$1 @ $2}
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
|   IDENT LPAREN args RPAREN		{ ($1, $3) }
;

args:
    expression                           { [$1] }
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



