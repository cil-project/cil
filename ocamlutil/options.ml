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
    OBool of bool ref            (** A boolean option *)
  | OInt  of int ref             (** An integer option *)
  | OString of string ref        (** A string option *)



let optionToArgs (od : optionDescr) : (string * Arg.spec * string) list = 
  let sp = 
    match od.optKind with 
      OBool oref -> Arg.Unit (fun _ -> oref := true; od.optExtra ())
    | OInt iref -> Arg.Int (fun i -> iref := i; od.optExtra ())
    | OString sref -> Arg.String (fun s -> sref := s; od.optExtra ())
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
