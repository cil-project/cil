   (* Parse a file in *)
exception ParseError of string

    (* additional command line arguments *)
val args: (string * Arg.spec * string) list

    (* the main command to parse a file *)
val parse: string -> Cil.file

    (* Combine a bunch of files and produce another file *)
val combine: string list -> string -> unit

    (* Combine a bunch of files and produce a CIL. This is like combine + 
     * parse, except that no intermediate file is produced *)
val parse_combine: string list -> Cil.file
