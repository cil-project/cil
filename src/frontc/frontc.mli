   (* Signal that we are in MS VC mode *)
val setMSVCMode: unit -> unit

   (* Parse a file in *)
exception ParseError of string

    (* additional command line arguments *)
val args: (string * Arg.spec * string) list

    (* the main command to parse a file. Return a thunk that can be used to 
     * convert the AST to CIL. *)
val parse: string -> (unit -> Cil.file)

