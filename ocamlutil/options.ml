(** We define a type for option descriptors. Such options can be set from the 
  * command line or from the UI *)
type optionDescr = {
    optInUI: string;
    (** The way the option should appear in the UI. Use & before a letter to 
     * indicate the shortcut letter  *)

    optRestart: bool; 
    (** Whether setting this option requires restarting the Engine *)
      
    optKind: optionKind;
    (** The option kind. *)

    optExtra: unit -> unit;
    (** An extra thing to do after setting the ref.
        This can be used, for instance, to set several refs
        with one option. *)

    optCommandLine: string;
    (** How the option should appear in the command line *)

    optHelp: string;
    (** A help string that is printed when the --help argument is given or as 
      * a tooltip in the GUI *)
  } 

and optionKind = 
  | OUnit                        
  | OBool of bool ref            (** A boolean option *)
  | OInt  of int ref             (** An integer option *)
  | OString of string ref        (** A string option *)
  | OStringList of char * string list ref 
     (** A list of strings, with a separator. This means that the option can 
      * also appear multiple times *)

let splitStringList (sep: char) (str: string) : string list = 
  let len = String.length str in
  let rec loop (start: int) : string list = 
    if start >= len then 
      [] 
    else begin
      try 
        let next_sep = String.index_from str start sep in
        String.sub str start (next_sep - start) :: loop (next_sep + 1)
      with Not_found -> (* The entire thing is a string *) 
        [ String.sub str start (len - start) ]
    end
  in
  loop 0

let optionToArgs (od : optionDescr) : (string * Arg.spec * string) list = 
  let sp = 
    match od.optKind with 
      OBool oref -> Arg.Unit (fun _ -> oref := true; od.optExtra ())
    | OUnit -> Arg.Unit (fun _ -> od.optExtra ())
    | OInt iref -> Arg.Int (fun i -> iref := i; od.optExtra ())
    | OString sref -> Arg.String (fun s -> sref := s; od.optExtra ())
    | OStringList (sep, lref) -> 
        Arg.String (fun s -> lref := !lref @ splitStringList sep s; 
                             od.optExtra ())
  in
  if od.optCommandLine <> "" then begin 
    [(od.optCommandLine, sp, od.optHelp)] @
    begin match od.optKind with
    | OBool oref -> 
        ["-no" ^ od.optCommandLine, 
         Arg.Unit (fun _ -> oref := false; od.optExtra ()),
         "turn this option off"]
    | _ -> []
    end 
  end else
    []
