
(* This interface is generated manually. The corresponding .ml file is 
 * generated automatically and is placed in ../obj/clexer.ml. The reason we 
 * want this interface is to avoid confusing make with freshly generated 
 * interface files *)

(*
type handle =
	bool * in_channel * string * string * int * int * out_channel * string
*)

val init: filename:string -> inchannel:in_channel -> Lexing.lexbuf

(*
val current_handle: handle ref

val get_buffer: handle ref -> string -> int -> int
*)

(* Display an error given two positions int he parsing buffer *)
val display_error: string -> int -> int -> unit

(*
val lineno: handle -> int
val file_name: handle -> string
*)

val currentFile: string ref
val currentLine: int ref 

val push_context: unit -> unit (* Start a context *)
val add_type: string -> unit (* Add a new string as a type name *)
val add_identifier: string -> unit (* Add a new string as a variable name *)
val pop_context: unit -> unit (* Remove all names added in this context *)
(* This is the main parser function *)
val initial: Lexing.lexbuf -> Cparser.token

