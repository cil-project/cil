val mainname: string ref 

val doFile: Cil.file -> Cil.file


(* Insert the global initializer in the main. Optionally you can specify the 
 * name of the function in which to insert the call to the global initializer 
 * *)
val insertGlobInit: Cil.file -> unit

