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

module type FRONTEND =
  sig
   (* Signal that we are in MS VC mode *)
    val setMSVCMode: unit -> unit

   (* Parse a file in *)
    exception ParseError of string

    (* additional command line arguments *)
    val args  : (string * Arg.spec * string) list
    (* the main command to parse a file. Return a thunk that can be use to 
     * convert the AST to CIL *)
    val parse : string -> (unit -> Cil.file)

  end

                                        (* Use the frontc frontend *)
module F : FRONTEND = Frontc
module C = Cil
module CK = Check
module E = Errormsg
open Pretty
open Trace

(* 
 * [weimer] Mon Apr 29 13:33:45 PDT 2002
 * This verison of the CCured main.ml file allows the various CIL/CCured
 * stages to be performed separately. The stages:
 *
 * (1) convert post-processed input files to .cil 
 * (2) merge many .cils into one .cil
 * (3) pull out global initializers
 * (4) infer pointer annotations on .cil, emit .cil with annotations
 * (5) box annotated .cil, emit .cil with extra code
 * (6) program transformations (e.g., logcalls)
 * (7) optimize the resulting boxed code (remove redundant checks)
 *
 * If you skip a stage, the output from the previous stage is passed along.
 * You may ask for the resulting file after any particular stage to be
 * emitted. 
 *
 * For example, you might manually annotate a post-processed file and then 
 * box it with something like:
 *  ccured --cureOnly my_input_file.i --curedout my_boxed_file.c
 *
 * As a convenience, you may use "--out" to get a printout of whatever we
 * had left right before CCured was about to exit normally. 
 *
 * In addition, the command line arguments have been grouped and
 * streamlined. 
 *)

let parseOneFile fname : Cil.file = 
  if !Util.printStages then ignore (E.log "Parsing %s\n" fname);
  let cil = F.parse fname () in
  if !Util.doCheck then begin
    ignore (E.log "Checking CIL after CABS2CIL\n");
    CK.checkFile [] cil;
  end;
  cil

  (* E.logChannel *) 
  let cilChannel = ref None 
  let mergeChannel = ref None
  let globinitChannel = ref None 
  let inferChannel = ref None 
  let browserChannel = ref None
  let cureChannel = ref None 
  let annChannel = ref None 
  let transformChannel = ref None 
  let optChannel = ref None 
  let defaultChannel = ref None 


let main () = begin
  (* what stages should we run? *)
  let doMerge = ref true in 
  let doGlobInit = ref true in 
  let doInfer = ref true in
  let doCure = ref false in 
  let doTransform = ref true in 
  let doOpt = ref true in 

  let doNothing () = 
    doMerge := false ;
    doGlobInit := false ;
    doInfer := false ;
    doCure := false ;
    doTransform := false ; 
    doOpt := false ;
    ()
  in 

  let solverName = ref "none" in 
  let tableAll = ref false in
  let tableInterface = ref false in
  let typecheck = ref false in
  let newoptim  = ref false in (* Use the new optimizer *)
  let optimElimAll : string list ref = ref [] in
    (* A list of CHECK_xxx to remove  *)

  (* open and set an output channel *) 
  let outChannel what cr str = 
    match str with
      "-" | "stdout" -> cr := Some stdout ;
        if !E.verboseFlag then
          ignore (Printf.printf "Writing %s output to stdout\n" what );
    | _ -> 
      try 
        cr := Some(open_out str) ;
        if !E.verboseFlag then
          ignore (Printf.printf "Writing %s output to %s\n" what str );
      with e -> 
        raise (Arg.Bad ("Cannot open " ^ what ^ " file " ^ str))
  in 

  let setDebugFlag v name = 
    E.debugFlag := v;
    if v then Pretty.flushOften := true
  in
  let setTraceDepth n =
    Pretty.printDepth := n
  in
  let rec recursiveAddThing n =
    if (n = 0) then 0 else 1 + (recursiveAddThing (n-1))
  in
  let whichSolver sname = 
    solverName := sname ;
    if sname <> "none" then doCure := true 
  in

  let infileList = ref [] in 

  let recordFile fname = infileList := fname :: (!infileList) in

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

  let default_stages = 
    Printf.sprintf "(default stages:%s%s%s%s%s%s)\n" 
      (if !doMerge then " merge" else " (no merge)") 
      (if !doGlobInit then " globinit" else " (no globinit)") 
      (if !doInfer then " infer" else " (no infer)") 
      (if !doCure then " cure" else " (no cure)") 
      (if !doTransform then " transform" else " (no transform)") 
      (if !doOpt then " opt" else " (no opt)") 
  in 

  let argDescr = [
    (* Stage Control Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tStage Control Options\n" 
    ^ default_stages; 

    "--noMerge", Arg.Unit (fun () -> doMerge := false),
        "do not merge multiple input files" ; 
    "--noGlobInit", Arg.Unit (fun () -> doGlobInit := false),
        "do not pull out global initializers" ;
    "--noInfer", Arg.Unit (fun () -> doInfer := false),
        "do not infer pointer annotations" ;
    "--noCure", Arg.Unit (fun () -> doCure := false), 
        "do not instrument the code with run-time safety checks" ;
    "--noTransform", Arg.Unit (fun () -> doTransform := false), 
        "do not transform (e.g., logcalls) the code" ; 
    "--noOpt", Arg.Unit (fun () -> doOpt := false), 
        "do not optimize the instrumented code" ; 

    "--doMerge", Arg.Unit (fun () -> doMerge := true),
        "do merge multiple input files" ; 
    "--doGlobInit", Arg.Unit (fun () -> doGlobInit := true),
        "do pull out global initializers" ;
    "--doInfer", Arg.Unit (fun () -> doInfer := true),
        "do infer pointer annotations" ;
    "--doCure", Arg.Unit (fun () -> doCure := true), 
        "do instrument the code with run-time safety checks" ;
    "--doTransform", Arg.Unit (fun () -> doTransform := true), 
        "do transform (e.g., logcalls) the code" ; 
    "--doOpt", Arg.Unit (fun () -> doOpt := true), 
        "do optimize the instrumented code" ; 
      
    "--onlyMerge", Arg.Unit (fun () -> doNothing () ; doMerge := true),
        "only merge the input files" ;
    "--onlyGlobInit", Arg.Unit (fun () -> doNothing () ; doGlobInit := true),
        "only pull out global initializers" ; 
    "--onlyInfer", Arg.Unit (fun () -> doNothing () ; doInfer := true), 
        "only inter pointer annotations" ; 
    "--onlyCure", Arg.Unit (fun () -> doNothing () ; doCure := true), 
        "only instrument the code" ; 
    "--onlyTransform", Arg.Unit (fun () -> doNothing () ; doTransform := true), 
        "only transform (e.g., logcalls) the code" ; 
    "--onlyOpt", Arg.Unit (fun () -> doNothing () ; doOpt := true), 
        "only optimize the instrumented code" ; 

    (* Output Channel Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tOutput Options\n(you may specify '-' or 'stdout' for output file names)\n" ; 

    "--out", Arg.String (outChannel "default" defaultChannel) ,
        "the name of the final result file" ; 
    "--log", Arg.String (fun s -> 
          E.logChannel := open_out s), 
        "the name of the log file" ; 
    "--cilout", Arg.String (outChannel "cil" cilChannel) , 
        "the name of the cil file (or merged file, if many input files)" ; 
    "--mergedout", Arg.String (outChannel "merger" mergeChannel) ,
        "the name of the merged file" ; 
    "--globinitout", Arg.String (outChannel "globinit" globinitChannel) , 
        "the name of the global-initializer-containting file" ; 
    "--inferout", Arg.String (outChannel "infer" inferChannel) , 
        "the name of the inference file (with graph)" ; 
    "--browserout", Arg.String (outChannel "browser" browserChannel) ,
        "the name of the inference browser file" ; 
    "--curedout", Arg.String (outChannel "cure" cureChannel) , 
        "the name of the cured file" ; 
    "--annout", Arg.String (outChannel "annotations" annChannel) , 
        "the name of the PCC annotation file" ; 
    "--optimout", Arg.String (outChannel "optim" optChannel) , 
        "the name of the optimized cured file" ; 
    "--noPrintLn", Arg.Unit (fun _ -> Cil.printLn := false;
                                     Cprint.printLn := false),
               "don't output #line directives";
    "--commPrintLn", Arg.Unit (fun _ -> Cil.printLnComment := true;
                                       Cprint.printLnComment := true),
               "output #line directives in comments";

    (* General Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tGeneral Options\n" ; 

    "--verbose", Arg.Unit (fun _ -> E.verboseFlag := true),
                "turn on verbose mode";
    "--warnall", Arg.Unit (fun _ -> E.warnFlag := true), "Show all warnings";
    "--debug", Arg.String (setDebugFlag true),
                     "<xxx> turns on debugging flag xxx";
    "--nodebug", Arg.String (setDebugFlag false), 
                     "<xxx> turns off debugging flag xxx";
    "--flush", Arg.Unit (fun _ -> Pretty.flushOften := true),
                     "Flush the output streams often (aids debugging)" ;
    "--check", Arg.Unit (fun _ -> Util.doCheck := true),
                     "turns on consistency checking of CIL";
    "--nocheck", Arg.Unit (fun _ -> Util.doCheck := false),
                     "turns off consistency checking of CIL";
    "--stats", Arg.Unit (fun _ -> Util.printStats := true),
               "print some statistics";
    "--stages", Arg.Unit (fun _ -> Util.printStages := true),
               "print the stages of the algorithm as they happen";
    "--tr",         Arg.String Trace.traceAddMulti,
                     "<sys>: subsystem to show debug trace printfs for";
    "--pdepth",     Arg.Int setTraceDepth,
                      "<n>: set max debug trace print depth (default: 5)";
    "--recurse",    Arg.Int (fun n -> (ignore (recursiveAddThing n)); ()),
                      "<n>: deliberately make stack grow to O(n) bytes";
    "--extrafiles", Arg.String parseExtraFile,
    "<filename>: the name of a file that contains a list of additional files to process, separated by whitespace of newlines";

    (* Merging Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tMerging Options\n" ; 

    "--keepunused", Arg.Unit (fun _ -> Rmtmps.keepUnused := true),
                "do not remove the unused variables and types"; 

    (* Globinit Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tGlobal Initializer Options\n" ; 

    "--entryPoint", Arg.String (fun s -> Globinit.mainname := s),
                     "<xxx> call globinit from program entry point xxx";

    (* Inference Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tPointer-Qualifier Inference Options\n" ; 

    "--curetype", Arg.String whichSolver, ("Which solver to use of {infer,wild,none}, default is " ^ !solverName ^ "\n\t(a non-none curetype implies --doCure)");
    "--boxdefaultwild", Arg.Unit (fun _ -> Ptrnode.defaultIsWild := true),
                       "the default pointer qualifier is wild";
    "--boxallpoly", Arg.Unit (fun _ -> Ptrnode.allPoly := true),
                       "make all external functions polymorphic";
    "--boxexternpoly", Arg.Unit (fun _ -> Ptrnode.externPoly := true),
                       "make all external functions polymorphic";
    "--tableAll", Arg.Unit (fun _ -> tableAll := true), 
      "Make all pointers use an external table.";
    "--tableInterface", Arg.Unit (fun _ -> tableInterface := true), 
      "Make interface pointers use an external table.";
    "--typecheck", Arg.Unit (fun _ -> typecheck := true),
                     "turns on consistency checking of CCured types";

    (* Curing Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tCuring Options\n" ; 

    "--wildUntaggedFns", Arg.Unit 
      (fun _ -> Solveutil.wild_solve_untagged_functions := true),
                 "force all functions untagged when curetype=wild";
    "--wildTagAllFns", Arg.Unit 
      (fun _ -> Solveutil.wild_solve_tag_all_functions := true),
                 "force all functions to be tagged when curetype=wild";
    "--wildTagTheseFns", Arg.String 
      (fun fname -> Solveutil.readTagFunctionFile fname),
                 "<fname>: this file lists, one per line, functions to make tagged";
    "--noStackChecks", Arg.Unit (fun _ -> Box.stackChecks := false),
                     "turns off storing-local-variable checks";

    (* Transformation Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tTransformation Options\n" ; 
    "--logcalls", Arg.Unit (fun _ -> Util.logCalls := true),
              "turns on generation of code to log function calls in CIL";
    "--logstyle", Arg.Int (fun i -> (Logcalls.setStyle i)),
              Logcalls.styleHelp;
    "--logwrites", Arg.Unit (fun _ -> Util.logWrites := true),
          "turns on generation of code to log memory writes calls in CIL";

    (* Optimization Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tOptimization Options\n" ; 
    "--newoptim", Arg.Unit (fun _ -> newoptim := true), 
                   "Uses the new optimizer";
    "--optimelimall", 
                   Arg.String (fun s -> optimElimAll := s :: !optimElimAll), 
                   "Produce several variants of optimized code";

    (* Parsing Options *) 
    "", Arg.Unit (fun () -> ()), "\n\t\tParsing Options\n" ; 

    "--MSVC", Arg.Unit (fun _ -> 
        if Machdep.hasMSVC then begin
         C.msvcMode := true;
         F.setMSVCMode ()
        end else 
           E.s (E.error "MSVC mode is not supported on this build\n")),   
        "Produce MSVC output. Default is GNU";
  ] @ F.args in 
  let usageMsg = "Usage: ccured [options] source-files" in

  Stats.reset () ; 
  Arg.parse argDescr recordFile usageMsg;
  Cil.initCIL ();

  let files = List.rev !infileList in 

  (**********************************************************************
   * STAGE 1
   *
   * Parse input files into CIL.
   **********************************************************************)
  if !Util.printStages then ignore (E.log "Stage 1: Parsing\n") ;
  let cils = Stats.time "parsing" (fun () -> 
    List.map parseOneFile files) () in

  let send_merged_output_to_cilChannel = ref false in 

  begin 
    match cils, !cilChannel with
    | _, None -> () 
    | [one], Some x -> 
        let oldpci = !C.print_CIL_Input in
        (trace "sm" (dprintf "printing file with CIL annotations\n"));
        C.print_CIL_Input := true;
        Stats.time "printCil" (C.printFile C.defaultCilPrinter x) one;
        C.print_CIL_Input := oldpci
    | many, Some x -> send_merged_output_to_cilChannel := true 
  end ; 

  (**********************************************************************
   * STAGE 2
   *
   * Merge input files into one file. 
   **********************************************************************)
  if !Util.printStages then ignore (E.log "Stage 2: Merging\n") ;
  let merged = Stats.time "merge" (fun () -> 
    match !doMerge, cils with
  | _, [] -> E.s (E.error "No arguments\n") 
  | _, [one] -> one 
  | false, lst -> E.s (E.error "Too many input files with no merging")
  | true, lst -> Mergecil.merge lst "merged"
  ) () in 

  (trace "sm" (dprintf "removing unused temporaries\n"));
  ignore (E.log "removing unused temporaries\n");
  Rmtmps.removeUnusedTemps merged;

  begin 
    match !mergeChannel with
      None -> ()
    | Some(x) -> 
      let oldpci = !C.print_CIL_Input in
      (trace "sm" (dprintf "printing merged file with CIL annotations\n"));
      C.print_CIL_Input := true;
      Stats.time "printCil" (C.printFile C.defaultCilPrinter x) merged;
      C.print_CIL_Input := oldpci
  end ;
  if !send_merged_output_to_cilChannel then begin
    match !cilChannel with
      None -> ()
    | Some(x) -> 
      let oldpci = !C.print_CIL_Input in
      (trace "sm" (dprintf "printing merged file with CIL annotations\n"));
      C.print_CIL_Input := true;
      Stats.time "printCil" (C.printFile C.defaultCilPrinter x) merged;
      C.print_CIL_Input := oldpci
  end ; 

  (**********************************************************************
   * STAGE 3
   *
   * Pull out global initializers
   **********************************************************************)
  if !Util.printStages then ignore (E.log "Stage 3: Glob-Init\n") ;
  let globinit = match !doGlobInit with
    true -> Globinit.doFile merged 
  | false -> merged
  in 
    
  begin 
    match !globinitChannel with
      None -> ()
    | Some(x) -> 
      (trace "sm" (dprintf "printing globinit file with CIL annotations\n"));
      Stats.time "printCil" (C.printFile Ptrnode.ccuredPrinter x) globinit;
  end ; 

  (**********************************************************************
   * STAGE 4
   *
   * Perform pointer qualifier inference. 
   **********************************************************************)
  if !Util.printStages then ignore (E.log "Stage 4: Inference\n") ;
  let infer = match !doInfer, !solverName with
    false, _ -> globinit
  | true, "none" -> globinit
  | true, _ -> begin
      (trace "sm" (dprintf "calling markptr\n"));
      if !Util.printStages then ignore (E.log "Collecting constraints\n");
      let marked = Stats.time "markptr" Markptr.markFile globinit in
      (* Solve the graph *)
      if !Util.printStages then ignore (E.log "Solving constraints\n");
      (trace "sm" (dprintf "invoking the solver\n"));
      (Util.tryFinally
         (fun _ ->
           Stats.time "solver"
             (fun graph ->
               (* Here we actually call the solver *)
               let should_typecheck = match !solverName with
               | "wild" -> Solveutil.wild_solve graph 
               | _ -> Solver.solve marked graph
               in 
               if should_typecheck && not !typecheck then begin 
                 ignore (E.warn "The inference results should be type-checked.")
               end ; 
               if (!tableInterface) then begin
                 Ptrnode.useLeanFats := true;
                 Solveutil.table_interface graph;
               end ;
               if (!tableAll) then begin
                 Ptrnode.useLeanFats := true;
                 Solveutil.table_it_all graph
               end
             ) Ptrnode.idNode
         )
         (fun _ ->
           (match !inferChannel with
             None -> ()
           | Some c ->
               Stats.time "printInfer" (Ptrnode.printInfer c) marked);
           (match !browserChannel with
             None -> ()
           | Some c ->
               Stats.time "printBrowser" (Ptrnode.printBrowser c) marked))
         ());
      marked 
    end
  in 

  if (!E.verboseFlag || !Util.printStats) && !solverName <> "none" then 
    Stats.time "graphstats" Ptrnode.printGraphStats ();
  if !typecheck then begin
    Stats.time "typecheck" Typecheck.typecheck_file infer
  end ; 
  (* weimer: remember the size of types *)
  Stats.time "type-size-check"
    (fun x -> ignore (C.visitCilFile (new Boxstats.typeSizeSetVisitor) x))
    infer;

  (**********************************************************************
   * STAGE 5
   *
   * Cured the program with run-time checks. 
   **********************************************************************)
  if !Util.printStages then ignore (E.log "Stage 5: Curing\n") ;

  let cured = match !doCure with 
    false -> infer
  | true -> begin
        (trace "sm" (dprintf "invoking the boxer\n"));
        let box = Stats.time "box" Box.boxFile infer in
        if !Util.doCheck then
          CK.checkFile [CK.NoCheckGlobalIds] box;

        ignore (E.log "printing the cured file\n");
        begin 
          match !cureChannel with
            Some(c) -> Stats.time "printCured"
            (C.printFile Ptrnode.ccuredPrinter c) box;
          | None -> () 
        end ; 
        ignore (E.log "finished printing the cured file\n");
        if !E.hadErrors then begin
          E.s (E.error "There were errors during boxing\n");
        end;
        (trace "sm" (dprintf "boxer has finished\n"));
        box
    end 
  in 

  (* spr: emit an annotated file for use by PCC *)
  let annotated : Cil.file =
    match !annChannel with
      None -> cured (* don't annotate *)
    | Some c ->  (* annotate *)
        if !Util.printStages then ignore (E.log "Adding PCC annotations\n");
        let annot = 
          Switch.make_cfg cured;
          Stats.time "annot" Annot.annotate cured
        in
        Stats.time "printAnnot" (C.printFile Annot.ccuredPrinter c) annot;
        annot
  in

  (**********************************************************************
   * STAGE 6
   *
   * Program Transformations
   **********************************************************************)
  if !Util.printStages then ignore (E.log "Stage 6: Transformations\n") ;
  if !doTransform then begin 
    if !Util.logCalls then
      Logcalls.logCalls cured;
    if !Util.logWrites then
      Logwrites.logWrites cured;
  end ; 

  (**********************************************************************
   * STAGE 7
   *
   * Optimize the boxed output. 
   **********************************************************************)
  if !Util.printStages then ignore (E.log "Stage 7: Optimization\n") ;
  let optim = match !doOpt with
    false -> annotated
  | true -> begin
      (trace "sm" (dprintf "invoking the optimizer\n"));
      Optim.checkToRemove := None;
      let optimized1 = 
        if !newoptim then Stats.time "Optim" Seoptim.optim annotated
        else annotated
      in
      let optimized2 = 
        Stats.time "Optim" Choptim.optim optimized1
      in 
      let optimized3 = Stats.time "Optim" Optim.optimFile optimized2 in
      ignore (E.log "Optimizer statistics : %t\n" Optim.getStatistics);
      begin 
        match !optChannel with
          None -> () 
        | Some(c) -> Stats.time "printOptim" 
            (C.printFile Ptrnode.ccuredPrinter c) optimized3;
      end ; 
      (* Return the optimized version *)
      optimized3
    end 
  in 

  if !E.verboseFlag || !Util.printStats then begin
    Boxstats.print cured;
  end;

  (* Now see if we must also produce a number of optimizer variants *)
  let optimFileName : string ref = ref "" in 
  List.iter 
    (fun name -> 
      let toremove = 
        if name = "CHECKS" then "" (* remove all *)
        else "CHECK_" ^ name
      in
      (* Find the new file name *)
      let newname = 
        if Filename.check_suffix !optimFileName ".optim.c" then 
          (Filename.chop_suffix !optimFileName ".optim.c") 
          ^ ".no" ^ name ^ ".optim.c"
        else
          E.s (E.bug "doOneVariant: wrong suffix in ouput file name")
      in
      (* Make a private copy of the file *)
      let copyGlobal = function
          C.GFun (fd, l) -> 
            C.GFun (C.copyFunction fd fd.C.svar.C.vname, l)
        | g -> g
      in
      let copy = 
        { optim with
               C.globals = List.map copyGlobal optim.C.globals;
               C.globinit = 
                match optim.C.globinit with
                   None -> None
                 | Some g -> Some (C.copyFunction g
                                     g.C.svar.C.vname)} in
      if !newoptim then 
        if toremove = "" then Seoptim.forcefullyRemoveAll := true 
        else Seoptim.forcefullyRemove := [toremove];
      
      Optim.checkToRemove := Some toremove;
      if !E.verboseFlag then 
        ignore (E.log " removing checks: %s\n" toremove);
      let removed = 
        (if !newoptim then Seoptim.optim else Optim.optimFile) copy in
      let variant_out = open_out newname in
      C.printFile Ptrnode.ccuredPrinter variant_out removed;
      close_out variant_out)
    !optimElimAll;
    
  if !Util.printStages then ignore (E.log "CCured complete\n");
  (* weimer: check those types *)
  ignore (Stats.time "type-size-check"
            (C.visitCilFile (new Boxstats.typeSizeCheckVisitor)) optim);

  begin 
    match !defaultChannel with
      None -> ()
    | Some(x) -> 
      (trace "sm" (dprintf "printing final file with CIL annotations\n"));
      Stats.time "printCil" (C.printFile Ptrnode.ccuredPrinter x) optim;
  end ; 
  () 
end ;; 

(* this mystery code was copied from George's original main.ml *)
let failed = ref false 
let wrapMain () = 
  let term = 
    try 
      main (); 
      fun () -> exit (if !failed then 1 else 0)
    with e ->  
      (fun () -> 
        print_string ("Uncaught exception: " ^ (Printexc.to_string e)
                      ^ "\n");
        if !E.verboseFlag then Stats.print stderr "Timings:\n";
        exit 2)
  in
  begin
    if !E.verboseFlag || !Util.printStats then
      Stats.print stderr "Timings:\n";
    close_out (! E.logChannel);  
    (match ! cilChannel with Some c -> close_out c | _ -> ());
    (match ! mergeChannel with Some c -> close_out c | _ -> ());
    (match ! globinitChannel with Some c -> close_out c | _ -> ());
    (match ! inferChannel with Some c -> close_out c | _ -> ());
    (match ! browserChannel with Some c -> close_out c | _ -> ());
    (match ! cureChannel with Some c -> close_out c | _ -> ());
    (match ! annChannel with Some c -> close_out c | _ -> ());
    (match ! transformChannel with Some c -> close_out c | _ -> ());
    (match ! optChannel with Some c -> close_out c | _ -> ());
    (match ! defaultChannel with Some c -> close_out c | _ -> ());

    term ()
  end
;;

Printexc.catch wrapMain ()
;;
