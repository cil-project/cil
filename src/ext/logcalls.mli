

(* A simple CIL transformer that inserts calls to a runtime function to log 
 * the call in each function *)
val logCalls: Cil.file -> unit

val styleHelp: string

val setStyle: int -> unit
