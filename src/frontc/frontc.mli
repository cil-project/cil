   (* Parse a file in *)
exception ParseError of string

    (* additional command line arguments *)
val args  : (string * Arg.spec * string) list
    (* the main command to parse a file *)
val parse : string -> Cil.file

val parse_to_cabs : string -> Cabs.file
