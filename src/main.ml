(*
 *
 * Copyright (c) 2001 by
 *  George C. Necula	necula@cs.berkeley.edu
 *  Scott McPeak        smcpeak@cs.berkeley.edu
 *  Wes Weimer          weimer@cs.berkeley.edu
 *   
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software for research purposes only is hereby granted, 
 * provided that the following conditions are met: 
 * 1. XSRedistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 * 3. The name of the authors may not be used to endorse or promote products 
 * derived from  this software without specific prior written permission. 
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

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
let merge = ref false

exception Done_Processing


let parseOneFile (fname: string) : C.file = 
  (* PARSE and convert to CIL *)
  if !Util.printStages then ignore (E.log "Parsing %s\n" fname);
  F.parse fname ()
      
let rec processOneFile (cil: C.file) =
  try begin

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
  let usageMsg = "Usage: cilly [options] source-files" in
  let fileNames : string list ref = ref [] in
  let recordFile fname = 
    fileNames := fname :: (!fileNames) 
  in
  (* Parsing of files with additional names *)
  let parseExtraFile (s: string) = 
    try
      let sfile = open_in s in
      while true do
        let line = try input_line sfile with e -> (close_in sfile; raise e) in
        let linelen = String.length line in
        let rec scan (pos: int) (* next char to look at *)
                     (start: int) : unit (* start of the word, 
                                            or -1 if none *) =
          if pos >= linelen then 
            if start >= 0 then 
              recordFile (String.sub line start (pos - start))
            else 
              () (* Just move on to the next line *)
          else
            let c = String.get line pos in
            match c with 
              ' ' | '\n' | '\r' | '\t' -> 
                (* whitespace *)
                if start >= 0 then begin
                  recordFile (String.sub line start (pos - start));
                end;
                scan (pos + 1) (-1)

            | _ -> (* non-whitespace *)
                if start >= 0 then 
                  scan (pos + 1) start 
                else
                  scan (pos + 1) pos
        in
        scan 0 (-1)
      done
    with Sys_error _ -> E.s (E.error "Cannot find extra file: %s\n" s)
   |  End_of_file -> () 
  in
  let merge = ref false in
  let openLog lfile =
    if !E.verboseFlag then
      ignore (Printf.printf "Setting log file to %s\n" lfile);
    try E.logChannel := open_out lfile with _ ->
      raise (Arg.Bad "Cannot open log file") in
  let outName = ref "" in
  let outFile fname =
    outName := fname;
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
    "--nocil", Arg.Int (fun n -> Cabs2cil.nocil := n),
                      "Do not compile to CIL the global with the given index"; 
    "--log", Arg.String openLog, "the name of the log file";
    "--out", Arg.String outFile, "the name of the output CIL file";
    "--merge", Arg.Unit (fun _ -> merge := true), 
              "Merge all inputs into one file";
    "--keep", Arg.Unit (fun _ -> keepFiles := true), "Keep intermediate files";
    "--MSVC", Arg.Unit (fun _ -> if Machdep.hasMSVC then begin
                                   C.msvcMode := true;
                                   F.setMSVCMode ()
                                  end else 
                                     E.s (E.error "MSVC mode is not supported on this build\n")
        ),
             "Produce MSVC output. Default is GNU";
    "--stages", Arg.Unit (fun _ -> Util.printStages := true),
               "print the stages of the algorithm as they happen";
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

    "--extrafiles", Arg.String parseExtraFile,
    "<filename>: the name of a file that contains a list of additional files to process, separated by whitespace of newlines";
  ] @ F.args in
  begin
    Stats.reset ();
    Arg.parse argDescr recordFile usageMsg;
    fileNames := List.rev !fileNames;
    if !testcil <> "" then begin
      Testcil.doit !testcil
    end else 
      if !merge then begin
        let files = List.map parseOneFile !fileNames in
        let one = 
          Mergecil.merge files (if !outName = "" then "stdout" else !outName)
        in
        if !E.hadErrors then 
          E.s (E.error "There were errors during merging\n");
        processOneFile one
      end else
        List.iter (fun fname -> 
                       let cil = parseOneFile fname in
                       processOneFile cil) !fileNames
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
