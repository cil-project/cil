
    (* Checks the well-formedness of the file. Prints warnings if errors are 
     * found  *)
type checkFlags = 
    NoCheckGlobalIds   (* Do not check that the global ids have the proper 
                        * hash value *)

val checkFile: checkFlags list -> Cil.file -> unit
