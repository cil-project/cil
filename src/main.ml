module F = Frontc
module C = Cil
module CK = Check
module E = Errormsg
open Pretty
open Trace


let outChannel : out_channel option ref = ref None
let keepFiles = ref false
let heapify = ref false
let stackguard = ref false
let testcil = ref ""

let preproc = ref ""                    (* The preprocess command *)
exception Done_Processing

let preprocess inname outname = 
  (* PREPROCESSING *)
  (trace "sm" (dprintf "beginning preprocessing\n"));
  let rec replace str =
    try
      let idx = String.index str '%' in
      (if idx > 0 then String.sub str 0 idx else "")
      ^ (match String.get str (idx + 1) with
        'i' -> inname
      | 'o' -> outname
      | '%' -> "%"
      | _ -> "")
      ^ (if (idx + 2) >= (String.length str)
      then ""
      else replace
	  (String.sub str (idx + 2) ((String.length str) - idx - 2)))
    with Not_found -> str 
  in
  let com = replace !preproc in
  if !E.verboseFlag then
    ignore (E.log "Executing \"%s\"\n" com);
  if (Sys.command com) <> 0 then
    E.s (E.error "Cannot preprocess: %s" com)
      
let rec processOneFile fname =
  try begin
    (* PARSE and convert to CIL *)
    let cil_thunk =
      if !preproc = "" then
        F.parse fname
      else begin
        let tmp =
          if !keepFiles then
            try (Filename.chop_extension fname) ^ ".cpp"
            with _ -> fname ^ ".cpp"
          else
            Filename.temp_file "rewrite" ".i" in
        Stats.time "preprocess" (preprocess fname) tmp;

        let res = F.parse tmp in
        if not !keepFiles then begin
          ignore (E.log "Removing %s\n" tmp);
          Sys.remove tmp
        end else
          ignore (E.log "Keeping intermediate file %s\n" tmp);

        res
      end
    in
    let cil =
      if !outChannel <> None then
        cil_thunk ()
      else
        raise Done_Processing
    in
    if !Util.doCheck then begin
      ignore (E.log "First CIL check\n");
      CK.checkFile [] cil;
    end;

    (* sm: remove unused temps to cut down on gcc warnings  *)
    (* (Stats.time "usedVar" Rmtmps.removeUnusedTemps cil);  *)
    (trace "sm" (dprintf "removing unused temporaries\n"));
    (Rmtmps.removeUnusedTemps cil);

    if (!Util.logCalls) then begin
      Logcalls.logCalls cil 
    end ; 
    
    if (!Util.logWrites) then begin
      Logwrites.logWrites cil 
    end ; 
    
    if (!heapify) then begin
      Heapify.default_heapify cil 
    end ;
    
    if (!stackguard) then begin
      Heapify.default_stackguard cil 
    end ;
    
    (match !outChannel with
      None -> ()
    | Some c -> Stats.time "printCIL" (C.printFile c) cil);
    if !E.hadErrors then
      E.s (E.error "Cabs2cil has some errors");

    if !Util.doCheck then begin
      ignore (E.log "Final CIL check\n");
      CK.checkFile [] cil;
    end
  end with Done_Processing -> ()
        
(***** MAIN *****)  
let rec theMain () =
  let doCombine = ref "" in 
  let usageMsg = "Usage: safecc [options] source-files" in
  let files : string list ref = ref [] in
  let recordFile fname = files := fname :: (!files) in
  let openLog lfile =
    if !E.verboseFlag then
      ignore (Printf.printf "Setting log file to %s\n" lfile);
    try E.logChannel := open_out lfile with _ ->
      raise (Arg.Bad "Cannot open log file") in
  let outFile fname =
    try outChannel := Some (open_out fname) with _ ->
      raise (Arg.Bad ("Cannot open output file" ^ fname)) in
  let setDebugFlag v name = 
    E.debugFlag := v;
    if v then Pretty.flushOften := true
  in
  (* sm: for print depth *)
  let setTraceDepth n =
    Pretty.printDepth := n
  in
  (*********** COMMAND LINE ARGUMENTS *****************)
  let argDescr = [
    "--verbose", Arg.Unit (fun _ -> E.verboseFlag := true),
                "turn of verbose mode";
    "--debug", Arg.String (setDebugFlag true),
                     "<xxx> turns on debugging flag xxx";
    "--flush", Arg.Unit (fun _ -> Pretty.flushOften := true),
                     "Flush the output streams often (aids debugging)" ;
    "--check", Arg.Unit (fun _ -> Util.doCheck := true),
                     "turns on consistency checking of CIL";
    "--nocheck", Arg.Unit (fun _ -> Util.doCheck := false),
                     "turns off consistency checking of CIL";
    "--logcalls", Arg.Unit (fun _ -> Util.logCalls := true),
                     "turns on generation of code to log function calls in CIL";
    "--logwrites", Arg.Unit (fun _ -> Util.logWrites := true),
                     "turns on generation of code to log memory writes in CIL";
    "--heapify", Arg.Unit (fun _ -> heapify := true),
					"apply the `heapify' transformation";
    "--stackguard", Arg.Unit (fun _ -> stackguard := true),
					"apply the `stackguard' transformation";
    "--nodebug", Arg.String (setDebugFlag false), 
                      "<xxx> turns off debugging flag xxx";
    "--testcil", Arg.String (fun s -> testcil := s), 
          "test CIL using the give compiler";
    "--log", Arg.String openLog, "the name of the log file";
    "--out", Arg.String outFile, "the name of the output CIL file";

    "--keep", Arg.Unit (fun _ -> keepFiles := true), "Keep intermediate files";
    "--MSVC", Arg.Unit (fun _ -> C.msvcMode := true;
                                F.setMSVCMode ()),
             "Produce MSVC output. Default is GNU";
    (* sm: the next two lines appeared twice?! *)
    "--keepunused", Arg.Unit (fun _ -> Rmtmps.keepUnused := true),
                "do not remove the unused variables and types";

    "--noPrintLn", Arg.Unit (fun _ -> Cil.printLn := false;
                                     Cprint.printLn := false),
               "don't output #line directives";
    "--commPrintLn", Arg.Unit (fun _ -> Cil.printLnComment := true;
                                       Cprint.printLnComment := true),
               "output #line directives in comments";
    (* sm: some more debugging options *)
    "--tr",         Arg.String Trace.traceAddMulti,
                     "<sys>: subsystem to show debug printfs for";
    "--pdepth",     Arg.Int setTraceDepth,
                      "<n>: set max print depth (default: 5)";
  ] @ F.args in
  begin
    Stats.reset ();
    Arg.parse argDescr recordFile usageMsg;
    files := List.rev !files;
    if !testcil <> "" then begin
      Testcil.doit !testcil
    end else 
      List.iter processOneFile !files;
  end
;;
                                        (* Define a wrapper for main to 
                                         * intercept the exit *)
let failed = ref false 
let main () = 
  let term = 
    try 
      theMain (); 
      fun () -> exit (if !failed then 1 else 0)
    with e ->  
      (fun () -> 
        print_string ("Uncaught exception: " ^ (Printexc.to_string e)
                      ^ "\n");
        Stats.print stderr "Timings:\n";
        exit 2)
  in
  begin
    if !E.verboseFlag then
      Stats.print stderr "Timings:\n";
    close_out (! E.logChannel);  
    (match ! outChannel with Some c -> close_out c | _ -> ());
    term ()
  end
;;

Printexc.catch main ()
;;
