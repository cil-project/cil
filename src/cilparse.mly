%{
open Cil

let parse_error msg =
  Mllexer.display_error 
    "Syntax error" (Parsing.symbol_start ()) (Parsing.symbol_end ())

exception BadType
let badType tp where = 
  Mllexer.display_error 
    ("Syntax error: unexpected type" ^ where)
    (Parsing.symbol_start ()) (Parsing.symbol_end ());
  raise BadType


%}

%token <string> CHAR
%token <string> INT
%token <string> FLOAT
%token <string> STRING

%token EOF LPAREN RPAREN NIL NONE SOME
%token T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18
%token T19 T20
%%

interpret:
  file EOF				{$1}
;
location:
  INT INT STRING   { { line = $1; col = $2; file = $3; }
;
varinfo: 
   INT STRING BOOL typ location attr_list storage
         { { vid = $1; vname = $2; vglob = $3; vtype = $4; 
             vdecl = $5; vattr = $6; vstorage = $7; } }
;
storage: 
  T0                         { NoStorage }
| T1                         { Static }
| T2                         { Register }
| T3                         { Extern }
;
fieldinfo: 
  STRING STRING typ attr_list  { { fstruct = $1; fname = $2;
                                   ftype = $3; fattr = $4; } }
;
typ:
  T0                         { TVoid }
| T1 ikind attr_list         { TInt ($2, $3) }
| T2 ikind INT attr_list     { TBitfield ($2, $3, $4) }
| T3 fkind attr_list         { TFloat ($2, $3) }
| T4 STRING INT typ_ref      { Typedef ($2, $3, ref TVoid) }
| T5 typ exp_opt attr_list   { TArray ($2, $3, $4) }
| T6 STRING fieldinfo_list INT attr_list
                             { TStruct ($2, $3, $4, $5) }
| T7 STRING fieldinfo_list INT attr_list
                             { TUnion ($2, $3, $4, $5) }
| T8 STRING str_int_list INT attr_list
                             { TEnum ($2, $3, $4, $5) }
| T9 typ typ_list BOOL attr_list
                             { TFunc ($2, $3, $4, $5) }
;
ikind:
  T0                         { IChar }
| T1                         { ISChar }
| T2                         { IUChar }
| T3                         { IInt }
| T4                         { IUInt }
| T5                         { IShort }
| T6                         { IUShort }
| T7                         { ILong }
| T8                         { IULong }
| T9                         { ILongLong }
| T10                        { IULongLong }
;
fkind: 
  T0                         { FFloat }
| T1                         { FDouble }
| T2                         { FLongDouble }
;
attr: 
  T0 STRING                  { AId($2) }
| T1 STRING exp_list         { ACons($2, $3)                       
;
constant:
  T0 INT string_opt          { CInt ($2, $3) }
| T1 INT INT string_opt      { CLInt($2, $3, $4) }
| T2 STRING                  { CStr($2) }
| T3 CHAR                    { CChr($2) }
| T4 FLOAT string_opt        { CReal($2, $3) }
;
unop:
  T0                         { Neg }
| T1                         { LNot }
| T2                         { BNot }
;
binop: 
  T0                         { Plus }
| T1                         { Minus }
| T2                         { Mult }
| T3                         { Div }
| T4                         { Mod }
| T5                         { Shiftlt }
| T6                         { Shiftrt }
| T7                         { Lt }
| T8                         { Gt }
| T9                         { Le }
| T10                        { Ge }
| T11                        { Eq }
| T12                        { Ne }
| T13                        { BAnd }
| T14                        { BXor }
| T15                        { BOr }
;
exp: 
  T0 constant location       { Const ($2, $3) }
| T1 lval                    { Lval($2) }
| T2 typ location            { SizeOf($2, $3) }
| T3 unop exp typ location   { UnOp($2, $3, $4, $5) }
| T4 binop exp exp typ location
                             { BinOp($2, $3, $4, $5, $6) }
| T5 typ exp location        { CastE($2, $3, $4) }
| T6 lval location           { AddrOf($2, $3) }
;
lval:
  T0 varinfo offset location { Var($2, $3, $4) }
| T1 exp offset location     { Mem($2, $3, $4) }
;
offset:
  T0                         { NoOffset }
| T1 fieldinfo offset        { Field($2, $3) }
| T2 exp offset              { Index($2, $3) }
;
instr:
  T0 lval exp location       { Set($2, $3, $4) }
| T1 varinfo_option exp exp_list location
                             { Call($2, $3, $4, $5) }
| T2 string_list BOOL str_varinfo_list str_exp_list string_list
                             { Asm($2, $3, $4, $5, $6) }
;
stmt:
  T0                         { Skip }
| T1 stmt_list               { Sequence $2 }
| T2 exp stmt                { While($2, $3) }
| T3 exp stmt stmt           { IfThenElse ($2, $3, $4) }
| T4 STRING                  { Label($2) }
| T5 STRING                  { Goto($2) }
| T6 exp                     { Return($2) }
| T7 exp stmt                { Switch($2, $3) }
| T8 INT                     { Case($2) }
| T9                         { Default }
| T10                        { Break }
| T11                        { Continue }
| T12 instr                  { Instruction ($2) }
;
fundec: 
  STRING varinfo_list INT stmt typ storage attr_list
           { { sname = $1; slocals = $2; 
               smaxid = $3; sbody = $4; stype = $5; sstorage = $6; 
               sattr = $7; } }
;
global:  
   T0 fundec                      { GFun($2) }
|  T1 STRING typ                  { GType($2,$3) }
|  T3 varinfo exp_opt             { GVar($2, $3) }
|  T4 STRING                      { GAsm($2) } 
;
                                
file: 
  global_list                           { $1 }
;


/************ Helper non-terminals ****************/
global_list: 
  NIL                                   { [] }
| global global_list                    { $1 :: $2 }
;
string_opt:
  NONE                       { None }
| SOME STRING                { Some $2 }
;
exp_opt:
  NONE                       { None }
| SOME exp                   { Some $2 }
;
exp_list: 
  NIL                        { [] }
| exp exp_list               { $1 :: $2 }
;
fieldinfo_list: 
  NIL                        { [] }
| fieldinfo fieldinfo_list   { $1 :: $2 }
;
typ_list: 
  NIL                        { [] }
| typ typ_list               { $1 :: $2 }
;
varinfo_list: 
   NIL                                  { [] }
|  varinfo varinfo_list                 { $1 :: $2 }
;
attr_list: 
  NIL                        { [] }
| attr attr_list             { $1 :: $2 }
;
stmt_list: 
  NIL                        { [] }
| stmt stmt_list             { $1 :: $2 }
;
string_list: 
  NIL                        { [] }
| STRING string_list         { $1 :: $2 }
;
str_varinfo_list:
  NIL                               { [] }
| str_varinfo str_varinfo_list      { $1 :: $2 }
;
str_varinfo:
  STRING varinfo                    { $1, $2 }
;
str_exp_list:
  NIL                               { [] }
| str_exp str_exp_list              { $1 :: $2 }
;
str_exp:
  STRING exp                        { $1, $2 }
;
str_int_list:
  NIL                               { [] }
| str_int str_int_list              { $1 :: $2 }
;
str_int:
  STRING INT                        { $1, $2 }
;
%%



