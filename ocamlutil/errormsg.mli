
(** Utilities for error reporting. *)

(** An element of this type records the pertinent information about an error, 
    such as its location in the source file *)
type info

(** Information about the current error *)
val current      : info ref

(* Record the start of a new file *)
val startFile    : string -> unit

(* Record a newline at a given position *)
val startNewline : int -> unit

val getLineCol : info -> int -> string

(** Get the source file corresponding to an error *)
val fileName   : info -> string

(** A channel for printing log messages *)
val logChannel : out_channel ref

(** If set then print debugging info *)
val debugFlag  : bool ref               

val verboseFlag : bool ref

val theLexbuf     : Lexing.lexbuf ref

(** Error reporting functions raise this exception *)
exception Error


   (* Error reporting. All of these functions take same arguments as a 
    * Pretty.eprintf. They raise the exception Error after they print their 
    * stuff. However, their type indicates that they return a "Pretty.doc" 
    * (due to the need to use the built-in type "format") return a doc. Thus 
    * use as follows:  E.s (E.bug "different lengths (%d != %d)" l1 l2)
     *)

(** Prints an error message of the form [Error: ...] and then raises the 
    exception [Error]. Use in conjunction with s, for example: [E.s (E.error 
    ... )]. *)
val error         : ('a,unit,Pretty.doc) format -> 'a

(** Similar to [error] except that its output has the form [Bug: ...] *)
val bug           : ('a,unit,Pretty.doc) format -> 'a

(** Similar to [error] except that its output has the form [Unimplemented: ...] *)
val unimp         : ('a,unit,Pretty.doc) format -> 'a

val s             : Pretty.doc -> 'a

(** This is set whenever one of the above error functions are called. It must
    be cleared manually *)
val hadErrors : bool ref  

(** Like [error] but does not raise the [Error] exception. Use: [ignore (E.warn
    ...)] *)
val warn         : ('a,unit,Pretty.doc) format -> 'a

(** Print something to [logChannel] *)
val log           : ('a,unit,Pretty.doc) format -> 'a

   (* All of the error and warning reporting functions can also print a 
    * context. To register a context printing function use "pushContext". To 
    * remove the last registered one use "popContext". If one of the error 
    * reporting functions is called it will invoke all currently registered 
    * context reporting functions in the reverse order they were registered. *)

(** Registers a context printing function *)
val pushContext  : (unit -> Pretty.doc) -> unit

(** Removes the last registered context printing function *)
val popContext   : unit -> unit

(** To ensure that the context is registered and removed properly, use the 
    function below *)
val withContext  : (unit -> Pretty.doc) -> ('a -> 'b) -> 'a -> 'b

