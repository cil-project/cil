

    (* Checks the well-formedness of the file. Always returns true but prints 
     * warnings if errors are found *)
val checkFile: Cil.file -> bool


    (* If true then checks that the ids of the global variables are correct. 
     * Turn this off if you want to be able to change the name of globals *)
val checkGlobalIds: bool ref  
