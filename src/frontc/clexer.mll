(* FrontC -- lexical analyzer
**
** Project: FrontC
** File:	frontc.mll
** Version:	1.0e
** Date:	9.1.99
** Author:	Hugues Cassé
**
**	1.0	3.22.99	Hugues Cassé	First version.
**	a	4.19.99	Hugues Cassé	Now accept floating notation `<int part>.'.
**	b	4.26.99	Hugues Cassé	Correctly handle the # <lineno> <file> ...
**								directive. Previous bug was taking last
**								integer of the line as line number.
**	c	6.4.99	Hugues Cassé	Added context handling to manage local variables
**								and type definition with the same name.
**	d	8.26.99	Hugues Cassé	Now, manage escape sequences in string and
**								characters.
**	e	9.1.99	Hugues Cassé	Fix, '\0' now recognized.
**	f	10.8.99	Hugues Cassé	Understand "__const" GCC.
**
**      George Necula 12/12/00: 
**          added tokens for __inline__, __typeof__,__asm__,__volatile__
**          ignores ^L
*)
{
open Cparser
exception Eof
exception InternalError of string
let version = "Clexer V1.0f 10.8.99 Hugues Cassé"


(*
** Keyword hashtable
*)
module HashString =
struct
	type t = string
	let equal (s1 : t) (s2 : t) = s1 = s2
	let hash (s : t) = Hashtbl.hash s
end
module StringHashtbl = Hashtbl.Make(HashString)
let lexicon = StringHashtbl.create 211
let init_lexicon _ =
  StringHashtbl.clear lexicon;
  List.iter 
    (fun (key, token) -> StringHashtbl.add lexicon key token)
    [ ("auto", AUTO);
      ("const", CONST); ("__const", CONST); ("__const__", CONST);
      ("static", STATIC);
      ("extern", EXTERN);
      ("long", LONG);
      ("short", SHORT);
      ("register", REGISTER);
      ("signed", SIGNED);
      ("unsigned", UNSIGNED);
      ("volatile", VOLATILE);
      ("char", CHAR);
      ("int", INT);
      ("float", FLOAT);
      ("double", DOUBLE);
      ("void", VOID);
      ("enum", ENUM);
      ("struct", STRUCT);
      ("typedef", TYPEDEF);
      ("union", UNION);
      ("break", BREAK);
      ("continue", CONTINUE);
      ("goto", GOTO); 
      ("return", RETURN);
      ("switch", SWITCH);
      ("case", CASE); 
		("default", DEFAULT);
      ("while", WHILE);  
      ("do", DO);  
      ("for", FOR);
      ("if", IF);
		("else", ELSE);
      (*** Implementation specific keywords ***)
      ("__signed__", SIGNED);
      ("__inline__", INLINE); ("inline", INLINE); 
      ("__inline", INLINE); ("_inline", INLINE);
      ("__attribute__", ATTRIBUTE); ("__attribute", ATTRIBUTE);
      ("__asm__", ASM); ("asm", ASM);
      ("__typeof__", TYPEOF); ("__typeof", TYPEOF); ("typeof", TYPEOF); 
      ("__alignof__", ALIGNOF);
      ("__volatile__", VOLATILE);
      ("__FUNCTION__", FUNCTION__); 
      ("__func__", FUNCTION__); (* ISO 6.4.2.2 *)
      ("__PRETTY_FUNCTION__", PRETTY_FUNCTION__);
      ("__label__", LABEL__);
      (*** weimer: GCC arcana ***)
      ("__restrict", RESTRICT); ("restrict", RESTRICT);
(*      ("__extension__", EXTENSION); *)
      (**** MS VC ***)
      ("__int64", INT64);
      ("__int32", INT);
      ("_cdecl",  MSATTR ("_cdecl")); 
      ("__cdecl", MSATTR ("__cdecl"));
      ("_stdcall", MSATTR "_stdcall"); 
      ("__stdcall", MSATTR "__stdcall");
      ("_fastcall", MSATTR "_fastcall"); 
      ("__fastcall", MSATTR "__fastcall");
      ("__declspec", DECLSPEC);
    ]

(* Mark an identifier as a type name. The old mapping is preserved and will 
 * be reinstated when we exit this context *)
let add_type name =
(*   ignore (print_string ("adding type name " ^ name ^ "\n")); *)
   StringHashtbl.add lexicon name (NAMED_TYPE name)

let context : string list list ref = ref []

let push_context _ = context := []::!context

let pop_context _ = 
  match !context with
    [] -> raise (InternalError "Empty context stack")
	| con::sub ->
		(context := sub;
		List.iter (fun name -> 
                           (* ignore (print_string ("removing lexicon for " ^ name ^ "\n")); *)
                            StringHashtbl.remove lexicon name) con)

(* Mark an identifier as a variable name. The old mapping is preserved and 
 * will be reinstated when we exit this context  *)
let add_identifier name =
  match !context with
    [] -> () (* Just ignore raise (InternalError "Empty context stack") *)
  | con::sub ->
      (context := (name::con)::sub;
       (*                print_string ("adding IDENT for " ^ name ^ "\n"); *)
       StringHashtbl.add lexicon name (IDENT name))


(*
** Useful primitives
*)
let rem_quotes str = String.sub str 1 ((String.length str) - 2)
let scan_ident id = try StringHashtbl.find lexicon id
	with Not_found -> IDENT id

(* Change \ into / in file names. To avoid complications with escapes *)
let cleanFileName str = 
  let str1 = if str <> "" && String.get str 0 = '"' (* '"' *) 
        then rem_quotes str else str in
  let l = String.length str1 in
  let rec loop (copyto: int) (i: int) = 
     if i >= l then 
         String.sub str1 0 copyto
     else 
       let c = String.get str1 i in
       if c <> '\\' then begin
          String.set str1 copyto c; loop (copyto + 1) (i + 1)
       end else begin
          String.set str1 copyto '/';
          if i < l - 2 && String.get str1 (i + 1) = '\\' then
              loop (copyto + 1) (i + 2)
          else 
              loop (copyto + 1) (i + 1)
       end
  in
  loop 0 0


(*
** Buffer processor
*)
 
(*** input handle ***)
let currentLine = ref 0 (* the index of the current line *)

let currentFile = ref "" (* The file in which we are *)

let startLine = ref 0 (* the position in the buffer where the current line 
                       * starts *)

let attribDepth = ref 0 (* Remembers the nesting level when parsing 
                         * attributes *)
(* The current lexing buffer *)
let currentLexBuf = ref (Lexing.from_string "")

let init (infile: string) 
         (inc: in_channel) : Lexing.lexbuf =
  currentLine := 1;
  startLine := 0;
  currentFile := cleanFileName infile;
  attribDepth := 0;
  init_lexicon ();
  let lexbuf = Lexing.from_channel inc in
  currentLexBuf := lexbuf;
  lexbuf

let newline () = 
  incr currentLine;
  startLine := Lexing.lexeme_start !currentLexBuf


(*** syntax error building
let underline_error (buffer : string) (start : int) (stop : int) =
  let len = String.length buffer in
  let start' = min (max 0 start) (len - 1) in
  let stop' = min (max start' stop) (len - 1) in
  (
  (if start' > 0 then (String.sub buffer 0 start') else "")
  ^ "\027[4m"
  ^ (if (stop' - start') <> 0
  then (String.sub buffer start' (stop' - start' ) )
  else ""
      )
  ^ "\027[0m"
  ^ (if stop' < len then (String.sub buffer stop' (len - stop') ) else "")
      )
*)
    
let display_error msg token_start token_end =
  let adjStart = 
    if token_start < !startLine then 0 else token_start - !startLine in
  let adjEnd = 
    if token_end < !startLine then 0 else token_end - !startLine in
  output_string 
    stderr
    (!currentFile ^ "[" ^ (string_of_int !currentLine) ^ ":" 
                        ^ (string_of_int adjStart) ^ "-" 
                        ^ (string_of_int adjEnd) 
                  ^ "]"
     ^ " : " ^ msg);
  output_string stderr "\n";
  flush stderr

(*** Error handling ***)
let error msg =
  display_error msg (Parsing.symbol_start ()) (Parsing.symbol_end ());
  raise Parsing.Parse_error


(*** escape character management ***)
let scan_escape str =
  match str with
    "n" -> "\n"
  | "r" -> "\r"
  | "t" -> "\t"
  | "b" -> "\b"
  | "f" -> "\012"  (* ASCII code 12 *)
  | "v" -> "\011"  (* ASCII code 11 *)
  | "a" -> "\007"  (* ASCII code 7 *)
  | "e" -> "\027"  (* ASCII code 27. This is a GCC extension *)
  | _ -> str

let get_value chr =
  match chr with
    '0'..'9' -> (Char.code chr) - (Char.code '0')
  | 'a'..'z' -> (Char.code chr) - (Char.code 'a') + 10
  | 'A'..'Z' -> (Char.code chr) - (Char.code 'A') + 10
  | _ -> 0
let scan_hex_escape str =
  String.make 1 (Char.chr (
		 (get_value (String.get str 0)) * 16
		   + (get_value (String.get str 1))
	           ))
let scan_oct_escape str =
  String.make 1 (Char.chr (
		 (get_value (String.get str 0)) * 64
		   + (get_value (String.get str 1)) * 8
		   + (get_value (String.get str 2))
	           ))
}

let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'- 'z' 'A'-'Z']

let floatsuffix = ['f' 'F' 'l' 'L']

let usuffix = ['u' 'U']
let lsuffix = "l"|"L"|"ll"|"LL"
let intsuffix = lsuffix | usuffix | usuffix lsuffix | lsuffix usuffix

let intnum = decdigit+ intsuffix?
let octnum = '0' octdigit+ intsuffix?
let hexnum = '0' ['x' 'X'] hexdigit+ intsuffix?

let exponent = ['e' 'E']['+' '-']? decdigit+
let fraction  = '.' decdigit+
let floatraw = (intnum? fraction)
			|(intnum exponent)
			|(intnum? fraction exponent)
			|(intnum '.') 
let floatnum = floatraw floatsuffix?

let ident = (letter|'_')(letter|decdigit|'_')* 
let attribident = (letter|'_')(letter|decdigit|'_'|':')
let blank = [' ' '\t' '\012']
let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hexdigit hexdigit
let oct_escape = '\\' octdigit  octdigit octdigit

rule initial =
	parse 	"/*"			{ let _ = comment lexbuf in 
                                          initial lexbuf}
|               "//"                    { endline lexbuf }
|		blank			{initial lexbuf}
|               '\n'                    { newline (); initial lexbuf }
|		'#'			{ hash lexbuf}
	
|		'\''			{ CST_CHAR (chr lexbuf)}
|		'"'			{ (* '"' *)
                                          try CST_STRING (str lexbuf)
                                          with e -> 
                                             raise (InternalError "str")}
|		floatnum		{CST_FLOAT (Lexing.lexeme lexbuf)}
|		hexnum			{CST_INT (Lexing.lexeme lexbuf)}
|		octnum			{CST_INT (Lexing.lexeme lexbuf)}
|		intnum			{CST_INT (Lexing.lexeme lexbuf)}
|		"!quit!"		{EOF}
|		"..."			{ELLIPSIS}
|		"+="			{PLUS_EQ}
|		"-="			{MINUS_EQ}
|		"*="			{STAR_EQ}
|		"/="			{SLASH_EQ}
|		"%="			{PERCENT_EQ}
|		"|="			{PIPE_EQ}
|		"&="			{AND_EQ}
|		"^="			{CIRC_EQ}
|		"<<="			{INF_INF_EQ}
|		">>="			{SUP_SUP_EQ}
|		"<<"			{INF_INF}
|		">>"			{SUP_SUP}
| 		"=="			{EQ_EQ}
| 		"!="			{EXCLAM_EQ}
|		"<="			{INF_EQ}
|		">="			{SUP_EQ}
|		"="				{EQ}
|		"<"				{INF}
|		">"				{SUP}
|		"++"			{PLUS_PLUS}
|		"--"			{MINUS_MINUS}
|		"->"			{ARROW}
|		'+'				{PLUS}
|		'-'				{MINUS}
|		'*'				{STAR}
|		'/'				{SLASH}
|		'%'				{PERCENT}
|		'!'				{EXCLAM}
|		"&&"			{AND_AND}
|		"||"			{PIPE_PIPE}
|		'&'				{AND}
|		'|'				{PIPE}
|		'^'				{CIRC}
|		'?'				{QUEST}
|		':'				{COLON}
|		'~'				{TILDE}
	
|		'{'				{LBRACE}
|		'}'				{RBRACE}
|		'['				{LBRACKET}
|		']'				{RBRACKET}
|		'('				{LPAREN}
|		')'				{RPAREN}
|		';'				{SEMICOLON}
|		','				{COMMA}
|		'.'				{DOT}
|		"sizeof"		{SIZEOF}
|               "__asm"                 { MSASM (msasm lexbuf) }

(* __extension__ is a black. The parser runs into some conflicts if we let it 
 * pass *)
|               "__extension__"         { initial lexbuf }
|		ident			{scan_ident (Lexing.lexeme lexbuf)}
|		eof			{EOF}
|		_			{display_error
						"Invalid symbol"
						(Lexing.lexeme_start lexbuf)
						(Lexing.lexeme_end lexbuf);
						initial lexbuf}
and comment =
    parse 	
      "*/"			        { () }
|     '\n'                              { newline (); comment lexbuf }
| 		_ 			{ comment lexbuf }

(* # <line number> <file name> ... *)
and hash = parse
  '\n'		{ newline (); initial lexbuf}
| blank		{ hash lexbuf}
| intnum	{ (* We are seeing a GCC line number *)
                  currentLine := int_of_string (Lexing.lexeme lexbuf) - 1;
                  (* A file name must follow *)
		  file lexbuf }
| "line"        { hash lexbuf } (* MSVC line number info *)
| "pragma"      { PRAGMA }
| _	        { endline lexbuf}

and file =  parse 
        '\n'		        {newline (); initial lexbuf}
|	blank			{file lexbuf}
|	'"' [^ '\012' '\t' '"']* '"' 	{ (* '"' *)
                                 currentFile := 
                                     cleanFileName (Lexing.lexeme lexbuf);
(*
                                 print_string ("Found "^ !currentFile ^".\n");

*)				 endline lexbuf}
|	_			{endline lexbuf}

and endline = parse 
        '\n' 			{ newline (); initial lexbuf}
|	_			{ endline lexbuf}

and pragma = parse
   '\n'                 { newline (); "" }
|   _                   { let cur = Lexing.lexeme lexbuf in 
                          cur ^ (pragma lexbuf) }  

and str =
	parse	'"'             {""} (* '"' *)
|	hex_escape		{let cur = scan_hex_escape (String.sub
					 (Lexing.lexeme lexbuf) 2 2) in 
                                        cur ^ (str lexbuf)}
|	oct_escape		{let cur = scan_oct_escape (String.sub
					(Lexing.lexeme lexbuf) 1 3) in 
                                         cur ^ (str lexbuf)}
|	"\\0"			{(String.make 1 (Char.chr 0)) ^ 
                                         (str lexbuf)}
|	escape			{let cur = scan_escape (String.sub
					  (Lexing.lexeme lexbuf) 1 1) in 
                                            cur ^ (str lexbuf)}
|	_			 {let cur = Lexing.lexeme lexbuf in 
                                         cur ^  (str lexbuf)} 

and chr =  parse
    '\''	        {""}
|   hex_escape		{let cur = scan_hex_escape (String.sub
			 (Lexing.lexeme lexbuf) 2 2) in cur ^ (chr lexbuf)}
|   oct_escape		{let cur = scan_oct_escape (String.sub
	 		 (Lexing.lexeme lexbuf) 1 3) in cur ^ (chr lexbuf)}
|   "\\0"		{(String.make 1 (Char.chr 0)) ^ (chr lexbuf)}
|   escape		{let cur = scan_escape (String.sub
			 (Lexing.lexeme lexbuf) 1 1) in cur ^ (chr lexbuf)}
|   _			{let cur = Lexing.lexeme lexbuf in cur ^ (chr lexbuf)} 
	
and msasm = parse
    blank               { msasm lexbuf }
|   '{'                 { msasminbrace lexbuf }
|   _                   { let cur = Lexing.lexeme lexbuf in 
                          cur ^ (msasmnobrace lexbuf) }

and msasminbrace = parse
    '}'                 { "" }
|   _                   { let cur = Lexing.lexeme lexbuf in 
                          cur ^ (msasminbrace lexbuf) }  
and msasmnobrace = parse
   ['}' ';' '\n']       { lexbuf.Lexing.lex_curr_pos <- 
                               lexbuf.Lexing.lex_curr_pos - 1;
                          "" }
|  "__asm"              { lexbuf.Lexing.lex_curr_pos <- 
                               lexbuf.Lexing.lex_curr_pos - 5;
                          "" }
|  _                    { let cur = Lexing.lexeme lexbuf in 

                          cur ^ (msasmnobrace lexbuf) }

and attribute = parse
   '\n'                 { newline (); attribute lexbuf }
|  blank                { attribute lexbuf }
|  '('                  { incr attribDepth; LPAREN }
|  ')'                  { decr attribDepth;
                          if !attribDepth = 0 then
                            initial lexbuf (* Skip the last closed paren *)
                          else
                            RPAREN }
|  attribident          { IDENT (Lexing.lexeme lexbuf) }

|  '\''			{ CST_CHAR (chr lexbuf)}
|  '"'			{ (* '"' *)
                                          try CST_STRING (str lexbuf)
                                          with e -> 
                                             raise (InternalError "str")}
|  floatnum		{CST_FLOAT (Lexing.lexeme lexbuf)}
|  hexnum		{CST_INT (Lexing.lexeme lexbuf)}
|  octnum		{CST_INT (Lexing.lexeme lexbuf)}
|  intnum		{CST_INT (Lexing.lexeme lexbuf)}


{

}
