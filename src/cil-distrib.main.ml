(* A sample program to demonstrate how to use CIL *)

(* This program parses a preprocessed C source file, converts it to CIL and 
 * optionally applies one or more of the following tranformations to it: 
 * "logwrites", "logcalls" and "heapify".*)
(*----------------------------------------------------------------------*)
module F = Frontc (* The parser *)
module C = Cil (* The CIL definition *)
module E = Errormsg (* Utilities for error reporting *)
module S = Stats (* Utilities for displaying timing statistics *)

open Pretty (* Pretty printing utilities *)

let logCalls = ref false (* Whether to produce a log with all the function 
                          * calls made *)
let logWrites = ref false (* Whether to produce a log with all the mem 
                           * writes made *)
let heapify = ref false (* Whether to enable the heapify transformation *)
let outChannel : out_channel option ref = ref None (* The output channel for 
                                                    * emitting the CIL file *)

let rec processOneFile fname =
  let cil_thunk = F.parse fname in (* Parse the preprocessed source into an 
                                    * intermediate format called CABS *)
                                    
  let cil = cil_thunk () in  (* Convert the CABS representation to CIL *)
  begin
  (* Apply the specified transformations *)
    if (!logCalls) then Logcalls.logCalls cil;
    if (!logWrites) then Logwrites.logWrites cil;
    if (!heapify) then Heapify.default_heapify cil;
    
  (* Output the CIL file *)
    (match !outChannel with
      None -> ()
    | Some c -> S.time "printCIL" (C.printFile c) cil);
    
  (* Indicate if errors occurred in the conversion of CABS to CIL *)
    if !E.hadErrors then
      E.s (E.error "Cabs2cil has some errors")
  end
;;
  
(***** MAIN *****)  
let rec theMain () =
  let usageMsg = "Usage: " ^ Sys.argv.(0) ^ " [options] source-file" in
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

  (*********** COMMAND LINE ARGUMENTS *****************)
  let argDescr = [
    "-verbose", Arg.Unit (fun _ -> E.verboseFlag := true),
                "turn of verbose mode";
    "-debug", Arg.String (setDebugFlag true),
                "<xxx> turns on debugging flag xxx";
    "-flush", Arg.Unit (fun _ -> Pretty.flushOften := true),
                "Flush the output streams often (aids debugging)" ;
    "-logcalls", Arg.Unit (fun _ -> logCalls := true),
                "turns on generation of code to log function calls in CIL";
    "-logwrites", Arg.Unit (fun _ -> logWrites := true),
                "turns on generation of code to log memory writes in CIL";
    "-heapify", Arg.Unit (fun _ -> heapify := true),
                "apply the `heapify' transformation";
    "-nodebug", Arg.String (setDebugFlag false), 
                "<xxx> turns off debugging flag xxx";
    "-log", Arg.String openLog, "the name of the log file";
    "-o", Arg.String outFile, "the name of the output CIL file";

    "--MSVC", Arg.Unit (fun _ -> C.msvcMode := true;
                                F.setMSVCMode ()),
             "Produce MSVC output. Default is GNU";
    "-noPrintLn", Arg.Unit (fun _ -> Cil.printLn := false;
                                     Cprint.printLn := false),
               "don't output #line directives";
    "-commPrintLn", Arg.Unit (fun _ -> Cil.printLnComment := true;
                                       Cprint.printLnComment := true),
               "output #line directives in comments";
    "-pdepth",     Arg.Int (fun n -> Pretty.printDepth := n),
               "<n>: set max print depth (default: 5)";
  ] @ F.args in
  begin
    S.reset (); (* Initialize the timing statistics generator *)
    Arg.parse argDescr recordFile usageMsg; (* Parse command line arguments *)
    files := List.rev !files; 
    List.iter processOneFile !files;
  end
;;

(*----------------------------------------------------------------------*)
(* A wrapper for theMain that intercepts its exit so that we can print out 
 * timing statistics and perform any required cleanup *)
let main () = 
  let term = 
    try 
      theMain (); 
      fun () -> exit 0
    with e ->  
      (fun () -> 
        print_string ("Uncaught exception: " ^ (Printexc.to_string e)
                      ^ "\n");
        exit 1)
  in
  begin
    if !E.verboseFlag then
      S.print stderr "Timings:\n";
    close_out (! E.logChannel);  
    (match ! outChannel with Some c -> close_out c | _ -> ());
    term ()
  end
;;

main ()
;;

(*======================================================================*)
