(*
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(* maincil *)
(* this module is the program entry point for the 'cilly' program, *)
(* which reads a C program file, parses it, translates it to the CIL *)
(* intermediate language, and then renders that back into C *)


module F = Frontc
module C = Cil
module CK = Check
module E = Errormsg
open Pretty
open Trace


let outChannel : out_channel option ref = ref None
let mergedChannel : out_channel option ref = ref None
let keepFiles = ref false
let heapify = ref false
let stackguard = ref false
let doCallGraph = ref false
let dumpFCG = ref false
let makeCFG = ref false
let testcil = ref ""

let ptrAnalysis = ref false
let ptrResults = ref false

let doEpicenter = ref false
let epicenterName = ref ""
let epicenterHops = ref 0

exception Done_Processing


let parseOneFile (fname: string) : C.file =
  (* PARSE and convert to CIL *)
  if !Util.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  
  if (not !doEpicenter) then (
    (* sm: remove unused temps to cut down on gcc warnings  *)
    (* (Stats.time "usedVar" Rmtmps.removeUnusedTemps cil);  *)
    (trace "sm" (dprintf "removing unused temporaries\n"));
    (Rmtmps.removeUnusedTemps cil)
  );
  cil

(** These are the statically-configured features. To these we append the 
  * features defined in Feature_config.ml (from Makefile) *)
let features : C.featureDescr list = 
  [ Logcalls.feature;
    Logwrites.feature;
  ] 
  @ Feature_config.features 


let rec processOneFile (cil: C.file) =
  try begin

    if !makeCFG then (
      ignore (Partial.calls_end_basic_blocks cil) ; 
      ignore (Partial.globally_unique_vids cil) ; 
      Cil.iterGlobals cil (fun glob -> match glob with
        Cil.GFun(fd,_) -> Cil.prepareCFG fd ;
                      ignore (Cil.computeCFGInfo fd true)
      | _ -> ()) ;
    );

    if !doCallGraph then (
      let graph:Callgraph.callgraph = (Callgraph.computeGraph cil) in
      (Callgraph.printGraph stdout graph)
    );
    
    if !doEpicenter then (
      (Epicenter.sliceFile cil !epicenterName !epicenterHops)
    );

    if !Util.doCheck then begin
      ignore (E.log "First CIL check\n");
      ignore (CK.checkFile [] cil);
    end;

    if (!heapify) then begin
      Heapify.default_heapify cil 
    end ;
    
    if (!stackguard) then begin
      Heapify.default_stackguard cil 
    end ;
      
    if (!ptrAnalysis) then begin
      Ptranal.analyze_file cil;
      Ptranal.compute_results (!ptrResults);
      if (!Ptranal.debug_may_aliases) then
	Ptranal.compute_aliases true
    end;		

    if (!Canonicalize.cpp_canon) then begin
      Canonicalize.canonicalize cil
    end ;
      
    (* Scan all the features configured from the Makefile and, if they are 
     * enabled then run them on the current file *)
    List.iter 
      (fun fdesc -> 
        if ! (fdesc.C.fd_enabled) then 
          fdesc.C.fd_doit cil)
      features;


    (* sm: enabling this by default, since I think usually we
     * want 'cilly' transformations to preserve annotations; I
     * can easily add a command-line flag if someone sometimes
     * wants these suppressed *)
    C.print_CIL_Input := true;

    (match !outChannel with
      None -> ()
    | Some c -> Stats.time "printCIL" (C.dumpFile C.defaultCilPrinter c) cil);

    if !E.hadErrors then
      E.s (E.error "Cabs2cil has some errors");

    if !Util.doCheck then begin
      ignore (E.log "Final CIL check\n");
      ignore (CK.checkFile [] cil);
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
  (* Processign of output file arguments *)
  let openFile (what: string) (takeit: out_channel -> unit) (fl: string) = 
    if !E.verboseFlag then
      ignore (Printf.printf "Setting %s to %s\n" what fl);
    (try takeit (open_out fl)
    with _ ->
      raise (Arg.Bad ("Cannot open " ^ what ^ " file")))
  in
  let outName = ref "" in
  let setDebugFlag v name =
    E.debugFlag := v;
    if v then Pretty.flushOften := true
  in
  (* sm: for print depth *)
  let setTraceDepth n =
    Pretty.printDepth := n
  in
  (*********** COMMAND LINE ARGUMENTS *****************)
  (* Construct the arguments for the features configured from the Makefile *)
  let featureArgs = 
    List.fold_right
      (fun fdesc acc -> 
        ("", Arg.Unit (fun _ -> ()), "\n") ::
        ("--do" ^ fdesc.C.fd_name, 
         Arg.Unit (fun _ -> fdesc.C.fd_enabled := true), 
         "enable " ^ fdesc.C.fd_description) ::
        ("--dont" ^ fdesc.C.fd_name, 
         Arg.Unit (fun _ -> fdesc.C.fd_enabled := false), 
         "disable " ^ fdesc.C.fd_description) ::
        fdesc.C.fd_extraopt @ acc)
      features
      []
  in
  let featureArgs = 
    ("", Arg.Unit (fun () -> ()), "\n\t\tCIL Features") :: featureArgs 
  in
    
  let argDescr = [
    "--verbose", Arg.Unit (fun _ -> E.verboseFlag := true),
                "turn on verbose mode";
    "--debug", Arg.String (setDebugFlag true),
                     "<xxx> turns on debugging flag xxx";
    "--flush", Arg.Unit (fun _ -> Pretty.flushOften := true),
                     "Flush the output streams often (aids debugging)" ;
    "--check", Arg.Unit (fun _ -> Util.doCheck := true),
                     "turns on consistency checking of CIL";
    "--nocheck", Arg.Unit (fun _ -> Util.doCheck := false),
                     "turns off consistency checking of CIL";
    "--ptr_analysis",Arg.Unit (fun _ -> ptrAnalysis := true),
                     "turns on alias analysis";
    "--ptr_may_aliases", Arg.Unit (fun _ -> Ptranal.debug_may_aliases := true),
                  "print out results of may alias queries";
    "--ptr_unify", Arg.Unit (fun _ -> Ptranal.no_sub := true),
                  "make the alias analysis unification-based";
    "--ptr_results", Arg.Unit (fun _ -> ptrResults := true),
                     "print the results of the alias analysis"; 
    "--ptr_mono", Arg.Unit (fun _ -> Ptranal.analyze_mono := true),
                    "run alias analysis monomorphically"; 
    "--heapify", Arg.Unit (fun _ -> heapify := true),
					"apply the `heapify' transformation";
    "--stackguard", Arg.Unit (fun _ -> stackguard := true),
					"apply the `stackguard' transformation";    "--cppcanon", Arg.Unit (fun _ -> Canonicalize.cpp_canon := true),
		       "Fix some C-isms so that the result is C++ compliant.";

    "--nodebug", Arg.String (setDebugFlag false),
                      "<xxx> turns off debugging flag xxx";
    "--testcil", Arg.String (fun s -> testcil := s),
          "test CIL using the give compiler";
    "--nocil", Arg.Int (fun n -> Cabs2cil.nocil := n),
                      "Do not compile to CIL the global with the given index";
    "--log", Arg.String (openFile "log" (fun oc -> E.logChannel := oc)),
             "the name of the log file";
    "--out", Arg.String (openFile "output" (fun oc -> outChannel := Some oc)),
             "the name of the output CIL file";
    "--warnall", Arg.Unit (fun _ -> E.warnFlag := true),
                 "Show all warnings";
    "--keep", Arg.Unit (fun _ -> keepFiles := true), "Keep intermediate files";
    "--MSVC", Arg.Unit (fun _ ->   C.msvcMode := true;
                                   F.setMSVCMode ();
                                   if not Machdep.hasMSVC then
                                     ignore (E.warn "Will work in MSVC mode but will be using machine-dependent parameters for GCC since you do not have the MSVC compiler installed\n")
                       ), "Produce MSVC output. Default is GNU";
    "--stages", Arg.Unit (fun _ -> Util.printStages := true),
               "print the stages of the algorithm as they happen";
    "--keepunused", Arg.Unit (fun _ -> Rmtmps.keepUnused := true),
                "do not remove the unused variables and types";

    "--mergedout", Arg.String (openFile "merged output"
                                   (fun oc -> mergedChannel := Some oc)),
                "specify the name of the merged file";
    "--noPrintLn", Arg.Unit (fun _ -> Cil.printLn := false;
                                     Cprint.printLn := false),
               "don't output #line directives";
    "--commPrintLn", Arg.Unit (fun _ -> Cil.printLnComment := true;
                                       Cprint.printLnComment := true),
               "output #line directives as comments";
    "--printCilAsIs", Arg.Unit (fun _ -> Cil.printCilAsIs := true),
               "do not try to simplify the CIL when printing";
    "--sliceGlobal", Arg.Unit (fun _ -> Util.sliceGlobal := true),
               "output is the slice of #pragma cilnoremove(sym) symbols";
    "--doCallGraph", Arg.Unit (fun _ -> doCallGraph := true),
               "compute and print a static call graph" ;
    "--makeCFG", Arg.Unit (fun _ -> makeCFG := true),
          "make the file look more like a CFG";
    "--epicenter", Arg.String (fun s -> doEpicenter := true; epicenterName := s),
               "<name>: do an epicenter slice starting from function <name>";
    "--hops", Arg.Int (fun n -> epicenterHops := n),
               "<n>: specify max # of hops for epicenter slice";
    (* sm: some more debugging options *)
    "--tr",         Arg.String Trace.traceAddMulti,
                     "<sys>: subsystem to show debug printfs for";
    "--pdepth",     Arg.Int setTraceDepth,
                      "<n>: set max print depth (default: 5)";

    "--extrafiles", Arg.String parseExtraFile,
    "<filename>: the name of a file that contains a list of additional files to process, separated by whitespace of newlines";
  ] @ F.args @ featureArgs in
  begin
    (* this point in the code is the program entry point *)

    Stats.reset ();

    (* parse the command-line arguments *)
    Arg.parse argDescr recordFile usageMsg;
    Cil.initCIL ();

    fileNames := List.rev !fileNames;

    if !testcil <> "" then begin
      Testcil.doit !testcil
    end else
      (* parse each of the files named on the command line, to CIL *)
      let files = List.map parseOneFile !fileNames in

      (* if there's more than one source file, merge them together; *)
      (* now we have just one CIL "file" to deal with *)
      let one =
        match files with
          [one] -> one
        | [] -> E.s (E.error "No arguments\n")
        | _ ->
            let merged =
              Stats.time "merge" (Mergecil.merge files)
                (if !outName = "" then "stdout" else !outName) in
            if !E.hadErrors then
              E.s (E.error "There were errors during merging\n");
            (* See if we must save the merged file *)
            (match !mergedChannel with
              None -> ()
            | Some mc -> begin
                let oldpci = !C.print_CIL_Input in
                C.print_CIL_Input := true;
                Stats.time "printMerged"
                  (C.dumpFile C.defaultCilPrinter mc) merged;
                C.print_CIL_Input := oldpci
            end);
            merged
      in

      (* process the CIL file (merged if necessary) *)
      processOneFile one
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
    with F.CabsOnly -> (* This is Ok *) fun () -> exit 0
    | e ->  
      (fun () -> 
        print_string ("Uncaught exception: " ^ (Printexc.to_string e)
                      ^ "\n");
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
