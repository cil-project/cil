{
open Mlparser
exception Eof
exception InternalError of string

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

rule initial = parse 	
   "/*"			{let _ = comment lexbuf 
                                         in initial lexbuf}
|     blank			{initial lexbuf}

|     '\''			{CST_CHAR (chr lexbuf)}
|     '"'			/* " */ {CST_STRING (str lexbuf)} 
|     floatnum		        {CST_FLOAT (Lexing.lexeme lexbuf)}
|     intnum			{CST_INT (Lexing.lexeme lexbuf)}

|     "("                       {LPAREN}
|     ")"                       {RPAREN}
|     "none"                    {NONE}
|     "some"                    {SOME}
|     "nil"                     {NIL}
|     "T0"                      {T0}     
|     "T1"                      {T1}     
|     "T2"                      {T2}     
|     "T3"                      {T3}     
|     "T4"                      {T4}     
|     "T5"                      {T5}     
|     "T6"                      {T6}     
|     "T7"                      {T7}     
|     "T8"                      {T8}     
|     "T9"                      {T9}     
|     "T10"                     {T10}     
|     "T11"                     {T11}     
|     "T12"                     {T12}     
|     "T13"                      {T13}     
|     "T14"                      {T14}     
|     "T15"                      {T15}     
|     "T16"                      {T16}     
|     "T17"                      {T17}     
|     "T18"                      {T18}     
|     "T19"                      {T19}     
|     "T20"                      {T20}     
|     eof			{EOF}
|     _				{display_error
				    "Invalid symbol"
				    (Lexing.lexeme_start lexbuf)
				    (Lexing.lexeme_end lexbuf);
				  initial lexbuf}
and comment = parse 	
  "*/"			{()}
|  _ 		        {comment lexbuf}


and str = parse	
  '"'			{""}  /* " */
 |  hex_escape		{let cur = scan_hex_escape (String.sub
			    (Lexing.lexeme lexbuf) 2 2) in cur ^ (str lexbuf)}
 |  oct_escape		{let cur = scan_oct_escape (String.sub
			    (Lexing.lexeme lexbuf) 1 3) in cur ^ (str lexbuf)}
 |  "\\0"		{(String.make 1 (Char.chr 0)) ^ (str lexbuf)}
 |  escape		{let cur = scan_escape (String.sub
			   (Lexing.lexeme lexbuf) 1 1) in cur ^ (str lexbuf)}
 |  _                   {let cur = Lexing.lexeme lexbuf in cur ^ (str lexbuf)} 

and chr = parse	
   '\''			{""}
 | hex_escape		{let cur = scan_hex_escape (String.sub
			   (Lexing.lexeme lexbuf) 2 2) in cur ^ (chr lexbuf)}
 | oct_escape		{let cur = scan_oct_escape (String.sub
			   (Lexing.lexeme lexbuf) 1 3) in cur ^ (chr lexbuf)}
 | "\\0"		{(String.make 1 (Char.chr 0)) ^ (chr lexbuf)}
 | escape		{let cur = scan_escape (String.sub
			    (Lexing.lexeme lexbuf) 1 1) in cur ^ (chr lexbuf)}
 | _		        {let cur = Lexing.lexeme lexbuf in cur ^ (chr lexbuf)} 
	
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
     
