
    (* Checks the well-formedness of the file. Prints warnings if errors are 
     * found  *)
val checkFile: Cil.file -> unit


    (* If true then checks that the ids of the global variables are correct. 
     * Turn this off if you want to be able to change the name of globals *)
val checkGlobalIds: bool ref  
