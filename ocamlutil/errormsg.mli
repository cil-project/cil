
type info

val current      : info ref             (* The current error info *)
val startFile    : string -> unit       (* Record the start of a new file *)
val startNewline : int -> unit          (* Record a newline at a given 
                                         * position *)

val getLineCol : info -> int -> string
val fileName   : info -> string

val logChannel : out_channel ref
val outChannel : out_channel ref

val debugFlag  : bool ref               (* If set then print debugging info *)
val verboseFlag : bool ref

val theLexbuf     : Lexing.lexbuf ref

exception Error


   (* Error reporting. All of these functions take same arguments as a 
    * Pretty.eprintf. They raise the exception Error after they print their 
    * stuff. However, their type indicates that they return a "Pretty.doc" 
    * (due to the need to use the built-in type "format") return a doc. Thus 
    * use as follows:  E.s (E.bug "different lengths (%d != %d)" l1 l2)
     *)
val error         : ('a,unit,Pretty.doc) format -> 'a
val bug           : ('a,unit,Pretty.doc) format -> 'a
val unimp         : ('a,unit,Pretty.doc) format -> 'a
val s             : Pretty.doc -> 'a

   (* Like error but does not raise an Error. Use: ignore (E.warn ...) *)
val warn         : ('a,unit,Pretty.doc) format -> 'a

   (* Print something to the logChannel *)
val log           : ('a,unit,Pretty.doc) format -> 'a

   (* All of the error and warning reporting functions can also print a 
    * context. To register a context printing function use "pushContext". To 
    * remove the last registered one use "popContext". If one of the error 
    * reporting functions is called it will invoke all currently registered 
    * context reporting functions in the reverse order they were registered. *)
val pushContext  : (unit -> Pretty.doc) -> unit
val popContext   : unit -> unit




