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

let parse_error msg : 'a =
  Clexer.display_error 
    ("Syntax error (" ^ msg ^")") 
    (Parsing.symbol_start ()) (Parsing.symbol_end ());
  raise Parsing.Parse_error

exception BadType
let badType tp where = parse_error ("unexpected type " ^ where)

let print = print_string

(*
** Type analysis
*)
type modifier =
    BASE_SIZE of size
  | BASE_SIGN of sign
  | BASE_STORAGE of storage
(*
  | BASE_VOLATILE
  | BASE_CONST
*)
  | BASE_ATTR of attribute

let base_CONST = BASE_ATTR("const", [])
let base_VOLATILE = BASE_ATTR("volatile", [])
let base_CDECL = BASE_ATTR("cdecl", [])
let base_STDCALL = BASE_ATTR("stdcall", [])

let tCONST typ = ATTRTYPE(typ, ["const", []])
let tCDECL typ = ATTRTYPE(typ, ["cdecl", []])
let tSTDCALL typ = ATTRTYPE(typ, ["stdcall", []])
let tVOLATILE typ = ATTRTYPE(typ, ["volatile", []])
let tATTR typ a = ATTRTYPE(typ, a)

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
    | (BITFIELD (NO_TYPE, exp), BASE_SIGN sign) -> 
        BITFIELD (INT(NO_SIZE, sign), exp)
    | (FLOAT false, BASE_SIZE LONG) -> FLOAT true
    | (DOUBLE false, BASE_SIZE LONG) -> DOUBLE true
    | (PTR typ, _) -> PTR (mod_root typ)
    | ATTRTYPE (typ, a), _ -> ATTRTYPE (mod_root typ, a)
(*
    | (CONST typ, _) -> CONST (mod_root typ)
    | (VOLATILE typ, _) -> VOLATILE (mod_root typ)
*)
    | _ -> badModifier modi "1"
  in
  let check_access typ =
    match typ with
      PROTO _ | OLD_PROTO _ (* | CONST _ | VOLATILE _ *) -> false
    | _ -> true in
  match modi with
    BASE_SIGN _ -> (mod_root typ, sto)
  | BASE_SIZE _ -> (mod_root typ, sto)
(*
  | BASE_CONST ->
      if (check_access typ) then (CONST typ, sto)
      else badModifier modi "2"
  | BASE_VOLATILE ->
      if (check_access typ) then (VOLATILE typ, sto)
      else badModifier modi "3"
*)
  | BASE_ATTR a -> 
      if (check_access typ) then  ATTRTYPE (typ, [a]), sto
      else badModifier modi "2"

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
    | PROTO (typ, pars, ell, inl) -> PROTO (set typ, pars, ell, inl)
    | OLD_PROTO (typ, pars, ell, inl) -> OLD_PROTO (set typ, pars, ell, inl)
(*
    | CONST typ -> CONST (set typ)
    | VOLATILE typ -> VOLATILE (set typ)
*)
    | ATTRTYPE (typ, a) -> ATTRTYPE (set typ, a)
    | BITFIELD (NO_TYPE, exp) -> BITFIELD(tst, exp)
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


let __functionString = (String.make 1 (Char.chr 0)) ^ "__FUNCTION__" 


(* An attempt to clean up the declarations and types *)
type typeSpecifier = (* Merge all specifiers into one type *)
    Tvoid                             (* Type specifier ISO 6.7.2 *)
  | Tchar
  | Tshort
  | Tint
  | Tlong
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

(* Apply a type specifier. We start with NO_TYPE ISO 6.7.2 *)
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
    | INT(NO_SIZE, sign), Tlong -> INT(LONG, sign)
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

let makeNameGroup (spec: specifier) (nl : name list) : name_group = 
  let innert = makeAttrType spec.styp spec.sattr in
  if spec.stypedef || spec.sinline then
    parse_error "Invalid Typedef or inline specifier";
  let names = 
    List.map (fun (n,bt,a,i) -> (n, injectType innert bt, a, i)) nl in
  (spec.styp, spec.ssto, names)

let makeSingleName (spec: specifier) (n : name) : single_name =
  let (bt, sto, ns) = makeNameGroup spec [n] in
  match ns with 
    [n'] -> (bt, sto, n')
  | _ -> parse_error "makeSingleName: impossible"


let doDeclaration (spec: specifier) (nl: name list) : definition = 
  (* Like a makeNameGroup but also treats the typedef and inline case *)
  let innert = makeAttrType spec.styp spec.sattr in
  let names = 
    List.map (fun (n,bt,a,i) -> (n, injectType innert bt, a, i)) nl in
  let ng = (spec.styp, spec.ssto, names) in
  if spec.stypedef then begin
    (* Tell the lexer about the new type names *)
    List.iter (fun (n, _, _, _) -> Clexer.add_type n) names;
    TYPEDEF ng
  end else
    if nl = [] then
      ONLYTYPEDEF ng
    else
      DECDEF ng

let doFunctionDef (spec: specifier) (n: name) 
                  (b: body) : definition = 
  (match n with 
    (_, _, _, NOTHING) -> ()
  | _ -> parse_error "Initializer in function definition");
  let innert = makeAttrType spec.styp spec.sattr in
  let fname = (spec.styp, spec.ssto, injectTypeName innert n) in
  FUNDEF (fname, b)

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

%token ATTRIBUTE INLINE ASM TYPEOF FUNCTION__
%token STDCALL CDECL
%token <string> MSASM
%token <string> PRAGMA

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
%right  NAMED_TYPE     /* We'll use this to handle redefinitions of 
                        * NAMED_TYPE as variables */
%left   IDENT

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

%type <Cabs.attribute list> gcc_attributes
%type <Cabs.definition list * Cabs.statement> body
%type <Cabs.statement> statement opt_stats stats
%type <Cabs.constant> constant
%type <Cabs.expression> expression opt_expression 
%type <Cabs.expression> init_expression opt_init_expression
%type <Cabs.expression list> comma_expression init_comma_expression
%type <Cabs.single_name list * bool> parameters
%type <string * Cabs.base_type> param_dec
%type <string> string_list

%type <specifier> decl_spec_list
%type <typeSpecifier> type_spec
%type <Cabs.name_group list> struct_decl_list 
%type <Cabs.name list> field_decl_list init_declarator_list
%type <Cabs.name> declarator init_declarator direct_decl 
%type <Cabs.name> abs_direct_decl abstract_decl 
%type <specifier list> pointer  /* Each element is a "* <type_quals_opt>" */
%type <Cabs.single_name list> parameter_list
%type <Cabs.single_name> parameter_decl
%type <Cabs.enum_item> enumerator
%type <Cabs.enum_item list> enum_list
%type <Cabs.definition> declaration function_def
%type <Cabs.base_type> type_name

%type <Cabs.body> block block_item_list
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
/*
| global_type global_defs SEMICOLON
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
*/
| ASM LPAREN CST_STRING RPAREN SEMICOLON
                        { GLOBASM $3 }
| PRAGMA                { PRAGMA $1 }

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
|   CONST				{base_CONST}
|   VOLATILE				{base_VOLATILE}
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
|   CDECL IDENT         {($2, tCDECL NO_TYPE)}
|   STDCALL IDENT       {($2, tSTDCALL NO_TYPE)}
|   LPAREN global_dec RPAREN
			{$2}
|   STAR global_dec
			{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   CDECL STAR global_dec
			{(fst $3, set_type (PTR (tCDECL NO_TYPE)) (snd $3))}
|   STDCALL STAR global_dec
			{(fst $3, set_type (PTR (tSTDCALL NO_TYPE)) (snd $3))}
|   STAR CONST global_dec
			{(fst $3, set_type (tCONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE global_dec
			{(fst $3, set_type (tVOLATILE (PTR NO_TYPE)) (snd $3))}
|   global_dec LBRACKET comma_expression RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, smooth_expression $3)) 
                            (snd $1))}
|   global_dec LBRACKET RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
|   global_dec LPAREN parameters RPAREN			
			{(fst $1, PROTO (snd $1, fst $3, snd $3, false))}
|   LPAREN global_dec RPAREN LPAREN parameters RPAREN
			{(fst $2, 
                          set_type (PROTO (NO_TYPE, fst $5, snd $5, false)) 
                                    (snd $2))}
|   global_dec LPAREN old_parameters RPAREN
			{(fst $1, OLD_PROTO (snd $1, fst $3, snd $3, false))}
|   LPAREN global_dec RPAREN LPAREN old_parameters RPAREN
			{(fst $2, 
                          set_type (OLD_PROTO (NO_TYPE, fst $5, snd $5, 
                                               false)) 
                            (snd $2))}
/*
|   CDECL global_dec  {(fst $2,
                          set_type (tCDECL NO_TYPE) (snd $2))}
|   STDCALL global_dec {(fst $2,
                          set_type (tSTDCALL NO_TYPE) (snd $2))}
*/
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
|   CONST				{[base_CONST]}
|   REGISTER				{[BASE_STORAGE REGISTER]}
;
old_qual:
    qual_type				{$1}
|   old_qual qual_type			{apply_qual $1 $2}
|   old_qual CONST			{(fst $1, base_CONST::(snd $1))}
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
                                  set_type (tCONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE old_dec	{(fst $3, 
                                  set_type (tVOLATILE (PTR NO_TYPE)) (snd $3))}
|   old_dec LBRACKET comma_expression RBRACKET
			        {(fst $1, 
                                  set_type (ARRAY (NO_TYPE, 
                                                   smooth_expression $3)) 
                                    (snd $1))}
|   old_dec LBRACKET RBRACKET	{(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) 
                                    (snd $1))}
|  old_dec LPAREN parameters RPAREN				
        			{(fst $1, PROTO (snd $1, fst $3, snd $3, 
                                                 false))}
|  LPAREN old_dec RPAREN LPAREN parameters RPAREN
                                {(fst $2, set_type (PROTO (NO_TYPE, 
                                                           fst $5, snd $5, 
                                                           false)) 
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
|   CONST				{base_CONST}
|   VOLATILE				{base_VOLATILE}
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
			{(fst $3, set_type (tCONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE local_dec
			{(fst $3, set_type (tVOLATILE (PTR NO_TYPE)) (snd $3))}
|   local_dec LBRACKET comma_expression RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, smooth_expression $3)) 
                            (snd $1))}
|   local_dec LBRACKET RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
|   local_dec LPAREN parameters RPAREN
			{(fst $1, PROTO (snd $1, fst $3, snd $3, false))}
|   LPAREN local_dec RPAREN LPAREN parameters RPAREN
			{(fst $2, 
                          set_type (PROTO (NO_TYPE, fst $5, snd $5, false)) 
                                   (snd $2))}
|   LPAREN local_dec RPAREN
			{$2}
;


/*** Typedef Definition ***/
typedef_type:
    typedef_sub		{apply_mods (snd $1) ((fst $1), NO_STORAGE)}
|   CONST typedef_sub	{apply_mods (base_CONST::(snd $2)) 
                            ((fst $2), NO_STORAGE)}
;
typedef_sub:
    NAMED_TYPE			{(NAMED_TYPE $1, [])}
|   NAMED_TYPE CONST		{(NAMED_TYPE $1, [base_CONST])}
|   comp_type			{($1, [])}		
|   comp_type CONST		{($1, [base_CONST])}
|   typedef_qual		{$1}
;

typedef_qual:
   qual_type			{$1}
|  typedef_qual qual_type	{apply_qual $1 $2}
|  typedef_qual CONST		{(fst $1, base_CONST::(snd $1))}
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
|   CDECL IDENT		{($2, tCDECL NO_TYPE)}
|   STDCALL IDENT	{($2, tSTDCALL NO_TYPE)}
|   NAMED_TYPE          {($1, NO_TYPE)}  /* undo what the lexer did. So that 
                                            we don't throw up when a name is 
                                            defined multiple times */
|   STAR typedef_dec	{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STDCALL STAR typedef_dec	
                        {(fst $3, set_type (PTR (tSTDCALL NO_TYPE)) (snd $3))}
|   CDECL STAR typedef_dec	
                        {(fst $3, set_type (PTR (tCDECL NO_TYPE)) (snd $3))}
|   STAR CONST typedef_dec   
                        {(fst $3, set_type (tCONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE typedef_dec
			{(fst $3, set_type (tVOLATILE (PTR NO_TYPE)) (snd $3))}
|   typedef_dec LBRACKET comma_expression RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, smooth_expression $3)) 
                            (snd $1))}
|   typedef_dec LBRACKET RBRACKET
			{(fst $1, 
                          set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
|   typedef_dec LPAREN parameters RPAREN
			{(fst $1, PROTO (snd $1, fst $3, snd $3, false))}
|   LPAREN typedef_dec RPAREN LPAREN parameters RPAREN
			{(fst $2, 
                          set_type (PROTO (NO_TYPE, fst $5, snd $5, false)) 
                                   (snd $2))}
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
	CONST			       {base_CONST}
|	VOLATILE		       {base_VOLATILE}
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
|   /* empty */                 {("___missing_field_name", NO_TYPE, 
                                  [], NOTHING)};
;
field_dec:
    IDENT			{($1, NO_TYPE)}
|   CDECL IDENT                 {($2, tCDECL NO_TYPE)}
|   STDCALL IDENT               {($2, tSTDCALL NO_TYPE)}
|   NAMED_TYPE			{($1, NO_TYPE)}
|   STAR field_dec		{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STDCALL STAR field_dec	
                                 {(fst $3, set_type (PTR (tSTDCALL NO_TYPE)) 
                                                 (snd $3))}
|   CDECL STAR field_dec	
                                {(fst $3, set_type (PTR (tCDECL NO_TYPE)) 
                                                         (snd $3))}
|   STAR CONST field_dec	{(fst $3, 
                                  set_type (tCONST (PTR NO_TYPE)) (snd $3))}
|   STAR VOLATILE field_dec	{(fst $3, 
                                  set_type (tVOLATILE (PTR NO_TYPE)) (snd $3))}
|   field_dec LBRACKET comma_expression RBRACKET
                                {(fst $1, 
                                  set_type (ARRAY (NO_TYPE, 
                                                   smooth_expression $3)) 
                                    (snd $1))}
|   field_dec LBRACKET RBRACKET	{(fst $1, 
                                  set_type (ARRAY (NO_TYPE, NOTHING)) 
                                    (snd $1))}
|   field_dec LPAREN parameters RPAREN
                                {(fst $1, PROTO (snd $1, fst $3, snd $3, 
                                                 false))}
|   LPAREN field_dec RPAREN LPAREN parameters RPAREN
	      		        {(fst $2, 
                                  set_type (PROTO (NO_TYPE, fst $5, snd $5, 
                                                   false)) 
                                    (snd $2))}
|  LPAREN field_dec RPAREN	{$2}
|  IDENT COLON expression	{($1, BITFIELD (NO_TYPE, $3))}
|  COLON expression             {("___missing_field_name", 
                                  BITFIELD (NO_TYPE, $2))}


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
    CONST				{base_CONST}
|   REGISTER				{BASE_STORAGE REGISTER}
|   VOLATILE				{base_VOLATILE}
;
param_qual:
    qual_type				{$1}
|   param_qual qual_type		{apply_qual $1 $2}
|   param_qual CONST			{(fst $1, base_CONST::(snd $1))}
|   param_qual REGISTER			{(fst $1, 
                                          (BASE_STORAGE REGISTER)::(snd $1))}
|   param_qual VOLATILE			{(fst $1, base_VOLATILE::(snd $1))}
;
param_def:
    param_dec gcc_attributes		{(fst $1, snd $1, $2, NOTHING)}
;
param_dec:
    /* empty */		{("", NO_TYPE)}
|   IDENT		{($1, NO_TYPE)}
|   NAMED_TYPE		{($1, NO_TYPE)}
|   CDECL STAR param_dec   {(fst $3, set_type (PTR (tCDECL NO_TYPE)) (snd $3))}
|   STDCALL STAR param_dec {(fst $3, set_type (PTR (tSTDCALL NO_TYPE)) 
                                              (snd $3))}
|   STAR param_dec	{(fst $2, set_type (PTR NO_TYPE) (snd $2))}
|   STAR CONST param_dec
			{(fst $3, set_type (tCONST (PTR NO_TYPE)) (snd $3))} 
|   STAR VOLATILE param_dec
			{(fst $3, set_type (tVOLATILE (PTR NO_TYPE)) (snd $3))}
|   param_dec LBRACKET comma_expression RBRACKET
			{(fst $1, set_type (ARRAY (NO_TYPE, 
                                                   smooth_expression $3)) 
                          (snd $1))}
|   param_dec LBRACKET RBRACKET
			{(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) 
                              (snd $1))}
|   LPAREN param_dec RPAREN LPAREN parameters RPAREN
			{(fst $2, set_type (PROTO (NO_TYPE, fst $5, snd $5, 
                                                   false)) 
                            (snd $2))}
|   LPAREN param_dec RPAREN	{$2}
;
		

/*** Only-type Definition ****/
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
    CONST				{base_CONST}
|   VOLATILE				{base_VOLATILE}
;
only_dec:
    /* empty */		{NO_TYPE}
|   CDECL STAR only_dec {set_type (PTR (tCDECL NO_TYPE)) $3}
|   STAR only_dec	{set_type (PTR NO_TYPE) $2}
|   STAR CONST only_dec	{set_type (tCONST (PTR NO_TYPE)) $3}
|   STAR VOLATILE only_dec
			{set_type (tVOLATILE (PTR NO_TYPE)) $3}
|   only_dec LBRACKET comma_expression RBRACKET
			{set_type (ARRAY (NO_TYPE, smooth_expression $3)) $1}
|   only_dec LBRACKET RBRACKET
			{set_type (ARRAY (NO_TYPE, NOTHING)) $1}
|   LPAREN only_dec RPAREN LPAREN parameters RPAREN
			{set_type (PROTO (NO_TYPE, fst $5, snd $5, false)) $2}
|   LPAREN only_dec RPAREN
			{$2}
;


/*** Base type ***/
qual_type:
    VOID				{(VOID, [])}
|   CHAR				{(NO_TYPE, [BASE_SIZE CHAR])}
|   INT					{(INT (NO_SIZE, NO_SIGN), [])}
|   INT64                               {(NO_TYPE, [BASE_SIZE LONG_LONG])}
|   FLOAT				{(FLOAT false, [])}
|   DOUBLE				{(DOUBLE false, [])}
|   LONG				{(NO_TYPE, [BASE_SIZE LONG])}
|   SHORT				{(NO_TYPE, [BASE_SIZE SHORT])}
|   SIGNED				{(NO_TYPE, [BASE_SIGN SIGNED])}
|   UNSIGNED				{(NO_TYPE, [BASE_SIGN UNSIGNED])}
;
comp_type:
    STRUCT typename	{STRUCT $2}				
|   STRUCT LBRACE field_list RBRACE
			{STRUCTDEF ("", List.rev $3)}
|   STRUCT typename LBRACE field_list RBRACE
			{STRUCTDEF ($2, List.rev $4)}
|   UNION typename	{UNION $2} 
|   UNION LBRACE field_list RBRACE			
			{UNIONDEF ("", List.rev $3)}
|   UNION typename LBRACE field_list RBRACE
			{UNIONDEF ($2, List.rev $4)}
|   ENUM typename       {ENUM $2 }					
|   ENUM LBRACE enumlist maybecomma RBRACE
			{ENUMDEF ("", List.rev $3)}		
|   ENUM typename LBRACE enumlist maybecomma RBRACE
			{ENUMDEF ($2, List.rev $4)}
;
typename:
    IDENT				{$1}
|   NAMED_TYPE				{$1}
;
enumlist:	
    enum_name				{[$1]}
|   enumlist COMMA enum_name	        {$3::$1}
;
maybecomma:
   /* empty */                          { () }
|  COMMA                                { () }
enum_name:	
    IDENT				{($1, NOTHING)}
|   IDENT EQ expression			{($1, $3)}
;


/*** Expressions ****/

/* We cannot add compound initializers to expressions because it conflicts 
 * with the GNU BODY expression. Thus we duplicate the entire expression 
 * language, except for the body and with the compound initializer added */
init_expression:
    LBRACE init_comma_expression RBRACE
			{CONSTANT (CONST_COMPOUND (List.rev $2))}
|   constant
			{CONSTANT $1}		
|   IDENT
			{VARIABLE $1}
|   SIZEOF expression
			{EXPR_SIZEOF $2}
|   SIZEOF LPAREN type_name RPAREN               /* !!! */
			{TYPE_SIZEOF $3}
|   PLUS init_expression
			{UNARY (PLUS, $2)}
|   MINUS init_expression
			{UNARY (MINUS, $2)}
|   STAR init_expression
			{UNARY (MEMOF, $2)}
|   AND init_expression				%prec ADDROF
			{UNARY (ADDROF, $2)}
|   EXCLAM init_expression
			{UNARY (NOT, $2)}
|   TILDE init_expression
			{UNARY (BNOT, $2)}
|   PLUS_PLUS init_expression %prec CAST
			{UNARY (PREINCR, $2)}
|   init_expression PLUS_PLUS
			{UNARY (POSINCR, $1)}
|   MINUS_MINUS init_expression %prec CAST
			{UNARY (PREDECR, $2)}
|   init_expression MINUS_MINUS
			{UNARY (POSDECR, $1)}
|   init_expression ARROW IDENT
			{MEMBEROFPTR ($1, $3)}
|   init_expression ARROW NAMED_TYPE
			{MEMBEROFPTR ($1, $3)}
|   init_expression DOT IDENT
			{MEMBEROF ($1, $3)}
|   init_expression DOT NAMED_TYPE
			{MEMBEROF ($1, $3)}
|   LPAREN init_comma_expression RPAREN
			{(smooth_expression $2)}
|   LPAREN type_name RPAREN init_expression %prec CAST   /* !!! */
			{CAST ($2, $4)}
|   init_expression LPAREN opt_expression RPAREN
			{CALL ($1, list_expression $3)}
|   init_expression LBRACKET comma_expression RBRACKET
			{INDEX ($1, smooth_expression $3)}
|   init_expression QUEST opt_init_expression COLON init_expression
			{QUESTION ($1, $3, $5)}
|   init_expression PLUS init_expression
			{BINARY(ADD ,$1 , $3)}
|   init_expression MINUS init_expression
			{BINARY(SUB ,$1 , $3)}
|   init_expression STAR init_expression
			{BINARY(MUL ,$1 , $3)}
|   init_expression SLASH init_expression
			{BINARY(DIV ,$1 , $3)}
|   init_expression PERCENT init_expression
			{BINARY(MOD ,$1 , $3)}
|   init_expression AND_AND init_expression
			{BINARY(AND ,$1 , $3)}
|   init_expression PIPE_PIPE init_expression
			{BINARY(OR ,$1 , $3)}
|   init_expression AND init_expression
			{BINARY(BAND ,$1 , $3)}
|   init_expression PIPE init_expression
			{BINARY(BOR ,$1 , $3)}
|   init_expression CIRC init_expression
			{BINARY(XOR ,$1 , $3)}
|   init_expression EQ_EQ init_expression
			{BINARY(EQ ,$1 , $3)}
|   init_expression EXCLAM_EQ init_expression
			{BINARY(NE ,$1 , $3)}
|   init_expression INF init_expression
			{BINARY(LT ,$1 , $3)}	
|   init_expression SUP init_expression
			{BINARY(GT ,$1 , $3)}
|   init_expression INF_EQ init_expression
			{BINARY(LE ,$1 , $3)}
|   init_expression SUP_EQ init_expression
			{BINARY(GE ,$1 , $3)}
|   init_expression  INF_INF init_expression
			{BINARY(SHL ,$1 , $3)}
|   init_expression  SUP_SUP init_expression
			{BINARY(SHR ,$1 , $3)}		
|   init_expression EQ init_expression
			{BINARY(ASSIGN ,$1 , $3)}			
|   init_expression PLUS_EQ init_expression
			{BINARY(ADD_ASSIGN ,$1 , $3)}		
|   init_expression MINUS_EQ init_expression		
			{BINARY(SUB_ASSIGN ,$1 , $3)}
|   init_expression STAR_EQ init_expression		
			{BINARY(MUL_ASSIGN ,$1 , $3)}
|   init_expression SLASH_EQ init_expression		
			{BINARY(DIV_ASSIGN ,$1 , $3)}
|   init_expression PERCENT_EQ init_expression	
			{BINARY(MOD_ASSIGN ,$1 , $3)}
|   init_expression AND_EQ init_expression		
			{BINARY(BAND_ASSIGN ,$1 , $3)}
|   init_expression PIPE_EQ init_expression		
			{BINARY(BOR_ASSIGN ,$1 , $3)}
|   init_expression CIRC_EQ init_expression		
			{BINARY(XOR_ASSIGN ,$1 , $3)}
|   init_expression INF_INF_EQ init_expression	
			{BINARY(SHL_ASSIGN ,$1 , $3)}
|   init_expression SUP_SUP_EQ init_expression
			{BINARY(SHR_ASSIGN ,$1 , $3)}
;
opt_init_expression:
    /* empty */                                   { NOTHING }
|   init_expression                               { $1 }
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
|	 	SIZEOF LPAREN type_name RPAREN   /* !!! */
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
|		LPAREN type_name RPAREN expression %prec CAST   /* !!!! */
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

/*******************************************************/
/* This is an attempt to clean up the handling of types*/
/*******************************************************/

declaration:                                /* ISO 6.7 */
    decl_spec_list init_declarator_list SEMICOLON { doDeclaration $1 $2 }
|   decl_spec_list                      SEMICOLON { doDeclaration $1 [] }
;
init_declarator_list:                       /* ISO 6.7 */
    init_declarator                              { [$1] }
|   init_declarator COMMA init_declarator_list   { $1 :: $3 }
 
;
init_declarator:                             /* ISO 6.7 */
    declarator gcc_attributes           { addAttributesName $2 $1 }
|   declarator gcc_attributes EQ init_expression       
                                        { applyInitializer $4 
                                             (addAttributesName $2 $1) }
;

decl_spec_list:                         /* ISO 6.7 */
                                        /* ISO 6.7.1 */
|   TYPEDEF decl_spec_list_opt          { applyTypedef $2 }
|   EXTERN decl_spec_list_opt           { applyStorage (EXTERN false) $2 }
|   STATIC  decl_spec_list_opt          { applyStorage (STATIC false) $2 }
|   AUTO   decl_spec_list_opt           { applyStorage AUTO $2 }
|   REGISTER decl_spec_list_opt         { applyStorage REGISTER $2} 
                                        /* ISO 6.7.2 */
|   type_spec decl_spec_list_opt_no_named { applyTypeSpec $1 $2 }
                                        /* ISO 6.7.3 */
|   CONST decl_spec_list_opt            { applyAttribute ("const",[]) $2 }
|   VOLATILE decl_spec_list_opt         { applyAttribute ("volatile",[]) $2 }
                                        /* ISO 6.7.4 */
|   INLINE decl_spec_list_opt           { applyInline $2 }
|   attribute decl_spec_list_opt        { applyAttributes $1 $2 }
;
/* In most cases if we see a NAMED_TYPE we must shift it. Thus we declare 
 * NAMED_TYPE to have right associativity */
decl_spec_list_opt: 
    /* empty */                         { emptySpec } %prec NAMED_TYPE
|   decl_spec_list                      { $1 }
;
/* We add this separate rule to handle the special case when an appearance of 
 * NAMED_TYPE should not be considered as past of the specifiers but as part 
 * of the declarator. IDENT has higher precedence than NAMED_TYPE */
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
;
struct_decl_list: /* ISO 6.7.2. Except that we allow empty structs */
   /* empty */                           { [] }
|  decl_spec_list field_decl_list SEMICOLON struct_decl_list          
                                          { (makeNameGroup $1 $2) :: $4 }
;
field_decl_list: /* ISO 6.7.2 */
    field_decl                           { [$1] }
|   field_decl COMMA field_decl_list     
                                         { $1 :: $3 }
;
field_decl: /* ISO 6.7.2. Except that we allow unnamed fields. GCC attributes 
             * are allowed only on non-empty field declarators */
|   field_declarator_opt                   { $1 } 
|   field_declarator_opt COLON expression  gcc_attributes
                                           { addAttributesName $4
                                              (match $1 with
                                               (n, t, [], NOTHING) -> 
                                                ( n, BITFIELD (t, $3), 
                                                     [], NOTHING)
                                               | _ -> parse_error "bitfield") 
                                           } 
;   
field_declarator_opt: 
  /* empty */                           { missingFieldDecl }
| declarator gcc_attributes             { addAttributesName $2 $1 }
;
enum_list: /* ISO 6.7.2.2 */
    enumerator				{[$1]}
|   enum_list COMMA enumerator	        {$1 @ [$3]}
;
enumerator:	
    IDENT				{($1, NOTHING)}
|   IDENT EQ expression			{($1, $3)}
;


declarator:  /* ISO 6.7.5. Plus Microsoft declarators. The specification says 
              * that they are specifiers but in practice they appear to be 
              * part of the declarator */
            direct_decl            { $1 }
|   pointer direct_decl            { applyPointer $1 $2 }
|   msqual  declarator             { addAttributeName $1 $2 }
|   pointer msqual direct_decl     { applyPointer $1 (addAttributeName $2 $3) }
;
direct_decl: /* ISO 6.7.5 */
    IDENT                          { $1, NO_TYPE, [], NOTHING }
                                   /* We want to be able to redefine named 
                                    * types as variable names */
|   NAMED_TYPE                     {Clexer.add_identifier $1;
                                     ($1, NO_TYPE, [], NOTHING)}
|   LPAREN declarator RPAREN       { $2 }
|   direct_decl LBRACKET comma_expression RBRACKET
                                   { injectTypeName (ARRAY(NO_TYPE, 
                                                       smooth_expression $3))
                                                    $1 }
|   direct_decl LBRACKET RBRACKET  { injectTypeName (ARRAY(NO_TYPE, 
                                                       NOTHING)) $1}
|   direct_decl LBRACKET STAR RBRACKET { injectTypeName (ARRAY(NO_TYPE, 
                                                       NOTHING)) $1}
|   direct_decl LPAREN RPAREN      { injectTypeName (PROTO(NO_TYPE, [], 
                                                       false, false)) 
                                                $1 }
|   direct_decl LPAREN parameter_list RPAREN 
                                   { injectTypeName (PROTO(NO_TYPE, $3, 
                                                       false, false)) 
                                                $1 }
|   direct_decl LPAREN parameter_list COMMA ELLIPSIS RPAREN
                                   { injectTypeName (PROTO(NO_TYPE, $3, true, 
                                                       false)) 
                                                $1 }
;
parameter_list: /* ISO 6.7.5 */
|   parameter_decl                        { [$1] }
|   parameter_list COMMA parameter_decl   { $1 @ [$3] }
;
parameter_decl: /* ISO 6.7.5 */
   decl_spec_list declarator              { makeSingleName $1 $2 }
|  decl_spec_list abstract_decl           { makeSingleName $1 $2 }
|  decl_spec_list                         { makeSingleName $1 emptyName }
;
pointer: /* ISO 6.7.5 */ 
   STAR decl_spec_list_opt_no_named          { [$2] }
|  STAR decl_spec_list_opt_no_named pointer  { $2 :: $3 }
;

type_name: /* ISO 6.7.6 */
  decl_spec_list abstract_decl { typeOfSingleName (makeSingleName $1 $2) }
| decl_spec_list               { typeOfSingleName 
                                           (makeSingleName $1 emptyName) }
| TYPEOF expression            {TYPEOF $2} 
;
abstract_decl: /* ISO 6.7.6. Plus Microsoft attributes. See the discussion 
                * for declarator. */
  pointer abs_direct_decl_opt        { applyPointer $1 $2 }
| pointer msqual abs_direct_decl_opt { applyPointer $1 
                                                    (addAttributeName $2 $3)}
|         abs_direct_decl            { $1 }
| msqual  abstract_decl              { addAttributeName $1 $2 }
;

abs_direct_decl: /* ISO 6.7.6. We do not support optional declarator for 
                  * functions  */
|   LPAREN abstract_decl RPAREN    { $2 }
|   abs_direct_decl_opt LBRACKET comma_expression RBRACKET
                                   { injectTypeName (ARRAY(NO_TYPE, 
                                                       smooth_expression $3))
                                                    $1 }
|   abs_direct_decl_opt LBRACKET RBRACKET  { injectTypeName (ARRAY(NO_TYPE, 
                                                         NOTHING)) $1}
|   abs_direct_decl_opt LBRACKET STAR RBRACKET { injectTypeName 
                                                  (ARRAY(NO_TYPE, 
                                                         NOTHING)) $1}
|   abs_direct_decl LPAREN RPAREN  { injectTypeName (PROTO(NO_TYPE, [], 
                                                       false, false)) 
                                                $1 }
|   abs_direct_decl LPAREN parameter_list RPAREN 
                                   { injectTypeName (PROTO(NO_TYPE, $3, 
                                                       false, false)) 
                                                $1 }
|   abs_direct_decl LPAREN parameter_list ELLIPSIS RPAREN
                                   { injectTypeName (PROTO(NO_TYPE, $3, true, 
                                                       false)) 
                                                $1 }
;
abs_direct_decl_opt:
    abs_direct_decl                 { $1 }
|   /* empty */                     { emptyName }
;
function_def:  /* ISO 6.9.1 */
  decl_spec_list declarator block { doFunctionDef $1 $2 $3 }
;

/* Microsoft specific qualifiers */
msqual :
    CDECL                           { "cdecl", [] }
|   STDCALL                         { "stdcall", [] }
;

%%



