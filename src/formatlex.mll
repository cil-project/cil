(*
 *
 * Copyright (c) 2001 by
 *  George C. Necula	necula@cs.berkeley.edu
 *  Scott McPeak        smcpeak@cs.berkeley.edu
 *  Wes Weimer          weimer@cs.berkeley.edu
 *   
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software for research purposes only is hereby granted, 
 * provided that the following conditions are met: 
 * 1. Redistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 * 3. The name of the authors may not be used to endorse or promote products 
 * derived from  this software without specific prior written permission. 
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
(* A simple lexical analyzer for constructing CIL based on format strings *)
{
open Formatparse
exception Eof
exception InternalError of string
module H = Hashtbl

(*
** Keyword hashtable
*)
let keywords = H.create 211

(*
** Useful primitives
*)
let scan_ident id = 
  try H.find keywords id
  with Not_found -> IDENT id  (* default to variable name *)

(*
** Buffer processor
*)
 
(*** input handle ***)
let currentLine = ref 0 (* the index of the current line *)

let currentFile = ref "" (* The file in which we are *)

let startLine = ref 0 (* the position in the buffer where the current line 
                       * starts *)

(* The current lexing buffer *)
let currentLexBuf = ref (Lexing.from_string "")
    
let currentPattern = ref ""

(* Weimer: Sun Dec  9 18:13:58  2001
 * Rupak reports that scrolling too many errors can lock up his
 * terminal. *)
let num_errors = ref 0
let max_errors = ref 20 

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
     ^ " : " ^ msg ^ " : " ^ 
     String.sub !currentPattern token_start (token_end - token_start + 1));
  output_string stderr "\n";
  flush stderr

let init ~(prog: string) : Lexing.lexbuf =
  H.clear keywords;
  currentPattern := prog;
  List.iter 
    (fun (key, token) -> H.add keywords key token)
    [ ("const", CONST); ("__const", CONST); ("__const__", CONST);
      ("static", STATIC);
      ("extern", EXTERN);
      ("long", LONG);
      ("short", SHORT);
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
      ("__attribute__", ATTRIBUTE); ("__attribute", ATTRIBUTE);
      ("__int64", INT64);
      ("__builtin_va_arg", BUILTIN_VA_ARG);
    ];
  let lexbuf = Lexing.from_string prog in
  currentLexBuf := lexbuf;
  lexbuf


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
  (* weimer: wide-character constants like L'\400' may be bigger than
   * 256 (in fact, may be up to 511), so Char.chr cannot be used directly *)
  let the_value = (get_value (String.get str 0)) * 64
		   + (get_value (String.get str 1)) * 8
		   + (get_value (String.get str 2)) in
  if the_value < 256 then String.make 1 (Char.chr the_value )
  else (String.make 1 (Char.chr (the_value / 256))) ^
       (String.make 1 (Char.chr (the_value mod 256)))

(* ISO standard locale-specific function to convert a wide character
 * into a sequence of normal characters. Here we work on strings. 
 * We convert L"Hi" to "H\000i\000" *)
let wbtowc wstr =
  let len = String.length wstr in 
  let dest = String.make (len * 2) '\000' in 
  for i = 0 to len-1 do 
    dest.[i*2] <- wstr.[i] ;
  done ;
  dest

(* This function converst the "Hi" in L"Hi" to { L'H', L'i', L'\0' } *)
let wstr_to_warray wstr =
  let len = String.length wstr in
  let res = ref "{ " in
  for i = 0 to len-1 do
    res := !res ^ (Printf.sprintf "L'%c', " wstr.[i])
  done ;
  res := !res ^ "}" ;
  !res

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
                        |(intnum '.' exponent) 
let floatnum = floatraw floatsuffix?

let ident = (letter|'_')(letter|decdigit|'_')* 
let attribident = (letter|'_')(letter|decdigit|'_'|':')
let blank = [' ' '\t' '\012' '\r']
let escape = '\\' _
let hex_escape = '\\' ['x' 'X'] hexdigit hexdigit
let oct_escape = '\\' octdigit  octdigit octdigit

rule initial =
	parse 	blank			{ initial lexbuf}
|		floatnum		{CST_FLOAT (Lexing.lexeme lexbuf)}
|		hexnum			{CST_INT (Lexing.lexeme lexbuf)}
|		octnum			{CST_INT (Lexing.lexeme lexbuf)}
|		intnum			{CST_INT (Lexing.lexeme lexbuf)}
|		"..."			{ELLIPSIS}
|		"-="			{MINUS_EQ}
|		"+="			{PLUS_EQ}
|		"*="			{STAR_EQ}
|		"<<"			{INF_INF}
|		">>"			{SUP_SUP}
| 		"=="			{EQ_EQ}
| 		"!="			{EXCLAM_EQ}
|		"<="			{INF_EQ}
|		">="			{SUP_EQ}
|		"="			{EQ}
|		"<"			{INF}
|		">"			{SUP}
|		"++"			{PLUS_PLUS}
|		"--"			{MINUS_MINUS}
|		"->"			{ARROW}
|		'+'			{PLUS}
|		'-'			{MINUS}
|		'*'			{STAR}
|		'/'			{SLASH}
|		'!'			{EXCLAM}
|		'&'			{AND}
|		'|'			{PIPE}
|		'^'			{CIRC}
|		'~'			{TILDE}
|		'['			{LBRACKET}
|		']'			{RBRACKET}
|		'{'			{LBRACE}
|		'}'			{RBRACE}
|		'('			{LPAREN}
|		')'			{RPAREN}
|		';'			{SEMICOLON}
|		','			{COMMA}
|		'.'			{DOT}
|               ':'                     {COLON}
|               '?'                     {QUEST}
|		"sizeof"		{SIZEOF}
|               "%eo"                   {ARG_eo}
|               "%e"                    {ARG_e}
|               "%E"                    {ARG_E}
|               "%u"                    {ARG_u}
|               "%b"                    {ARG_b}
|               "%t"                    {ARG_t}
|               "%d"                    {ARG_d}
|               "%lo"                   {ARG_lo}
|               "%l"                    {ARG_l}
|               "%i"                    {ARG_i}
|               "%I"                    {ARG_I}
|               "%o"                    {ARG_o}
|               "%va"                   {ARG_va}
|               "%v"                    {ARG_v}
|               "%k"                    {ARG_k}
|               "%f"                    {ARG_f}
|               "%F"                    {ARG_F}
|               "%p"                    {ARG_p}
|               "%P"                    {ARG_P}
|               "%s"                    {ARG_s}
|               "%S"                    {ARG_S}
|               "%g"                    {ARG_g}
|               "%A"                    {ARG_A}
|               "%c"                    {ARG_c}

|		'%'			{PERCENT}
|		ident			{scan_ident (Lexing.lexeme lexbuf)}
|		eof			{EOF}
|		_			{display_error
						"Formatlex: Invalid symbol"
						(Lexing.lexeme_start lexbuf)
						(Lexing.lexeme_end lexbuf);
                                         raise Parsing.Parse_error
                                        }
