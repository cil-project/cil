(** The main entry point for the optimizer *)
val optimFile: Cil.file -> Cil.file

val getStatistics: unit -> Pretty.doc

(** Which checks to remove completely. Set to None to not do this. Set to 
  * Some "" to remove all checks, set to Some "CHECK_NULL" to remove all 
  * CHECK_NULL, etc. *)
val checkToRemove: string option ref
