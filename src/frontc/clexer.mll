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
	List.fold_left
	(fun tbl (key, token) -> StringHashtbl.add tbl key token ; tbl)
	lexicon
	[
		("auto", AUTO);
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
		(*** Implementations ***)
		("__signed__", SIGNED);
                ("__inline__", INLINE); ("inline", INLINE);
		("__attribute__", ATTRIBUTE);
                ("__asm__", ASM); ("asm", ASM);
                ("__typeof__", TYPEOF);
                ("__volatile__", VOLATILE);
                ("__FUNCTION__", FUNCTION__);
		(*** weimer: GCC arcana ***)
		("__restrict", RESTRICT); ("restrict", RESTRICT);
		("__extension__", EXTENSION);
		("__inline", INLINE);
                (**** MS VC ***)
                ("__int64", INT64);
                ("_cdecl",  CDECL); ("__cdecl", CDECL);
                ("__stdcall", STDCALL);
	]

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
(*
** Buffer processor
*)

(*** input handle ***)
type handle =
	bool * in_channel * string * string * int * int * out_channel * string
let current_handle = ref (false, stdin, "", "", 0, 0, stdout, "")

let interactive (h : handle) = let (i, _, _, _, _, _, _, _) = h in i
let in_channel (h : handle) = let (_, c, _, _, _, _, _, _) = h in c
let line (h : handle) = let (_, _, l, _, _, _, _, _) = h in l
let buffer (h : handle) = let (_, _, _, b, _, _, _, _) = h in b
let pos (h : handle) = let (_, _, _, _, p, _, _, _) = h in p
let real_pos (i : int) (h : handle) = let (_, _, _, _, p, _, _, _) = h in i - p
let lineno (h : handle) = let (_, _, _, _, _, n, _, _) = h in n
let out_channel (h : handle) = let (_, _, _, _, _, _, out, _) = h in out
let file_name (h : handle) = let (_, _, _, _, _, _, _, name) = h in name

let set_line num =
	let (inter, cha, lin, buf, pos, _, out, name) = !current_handle in
	(* current_handle := (inter, cha, lin, buf, pos, num - 1, out, name) *)
        ()

let set_name name =
	let (inter, cha, lin, buf, pos, num, out, _) = !current_handle in
	(* current_handle := (inter, cha, lin, buf, pos, num, out, name) *)
        ()


(*** syntax error building ***)
let underline_error (buffer : string) (start : int) (stop : int) =
  let len = String.length buffer in
  let start' = max 0 start in
  let stop' = max 1 stop in
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
    
let display_error msg token_start token_end =
  output_string (out_channel !current_handle) (
  (if (interactive !current_handle)
  then ""
  else 
    (file_name !current_handle) ^ "["
    ^ (string_of_int (lineno !current_handle)) ^ "] "
		                                   )
  ^ msg ^ ": "
  ^ (underline_error
       (line !current_handle)
       (real_pos token_start !current_handle)
       (real_pos token_end !current_handle)
       )
      );
  flush (out_channel !current_handle)

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

let usuffix = ['u' 'U']
let lsuffix = ['l' 'L']
let intsuffix = (lsuffix|usuffix|(usuffix lsuffix lsuffix)|(usuffix lsuffix)|(lsuffix usuffix))?
let floatsuffix = ['f' 'F' 'l' 'L']

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
let blank = [' ' '\t' '\n' '\012']
let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hexdigit hexdigit
let oct_escape = '\\' octdigit  octdigit octdigit

rule initial =
	parse 	"/*"			{let _ = comment lexbuf in 
                                         initial lexbuf}
|		blank			{initial lexbuf}
|		"__attribute__ (" blank* "(__const__)" blank* ")" { initial lexbuf}
|		"__attribute__ ((const))" { initial lexbuf}
|		'#'			{line lexbuf}
	
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

|		ident			{scan_ident (Lexing.lexeme lexbuf)}
|		eof			{EOF}
|		_			{display_error
						"Invalid symbol"
						(Lexing.lexeme_start lexbuf)
						(Lexing.lexeme_end lexbuf);
						initial lexbuf}
and comment =
    parse 	"*/"			{()}
| 		_ 				{comment lexbuf}

(* # <line number> <file name> ... *)
and line = parse
  '\n'		{ initial lexbuf}
| blank		{ line lexbuf}
| intnum	{ set_line (int_of_string (Lexing.lexeme lexbuf));
		  file lexbuf }
| "pragma"      { PRAGMA }
| _	        { endline lexbuf}

and file =
	parse '\n'		{initial lexbuf}
|	blank			{file lexbuf}
|	'"' [^ '"']* '"' 	{ (* '"' *)
                                 set_name (rem_quotes (Lexing.lexeme lexbuf));
							endline lexbuf}
|	_					{endline lexbuf}
and endline =
	parse '\n' 				{initial lexbuf}
|	_					{endline lexbuf}

and pragma = parse
   '\n'        { "" }
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
{

(*** get_buffer ***)
let get_buffer (h : handle ref) (dst : string) (len : int) : int =
  let (inter, chan, line, buffer, pos, lineno, out, name) = !h in
  try
    let (bufferp, linep, posp, linenop) =
      if buffer <> ""
      then (buffer, line , pos, lineno)
      else
	let buffer = (input_line chan) ^ "\n" in
	(
	buffer,
	(if inter then line ^ buffer else buffer),
	(if inter then pos else pos + (String.length line)),
	lineno + 1
	  ) in
		(*let _ = print_endline ("-->" ^ linep) in*)
    let bufl = String.length bufferp in
    let lenp = min len bufl in
    let buffers = if bufl = lenp
    then ""
    else String.sub bufferp lenp (bufl - lenp) in
    begin
      String.blit bufferp 0 dst 0 lenp;
      h := (inter, chan, linep, buffers, posp, linenop, out, name);
      lenp
    end
  with End_of_file -> 0


(* init: handle -> ()
**	Initialize lexer.
*)
let init hdl =
  ignore (init_lexicon ());
  current_handle := hdl 
}
