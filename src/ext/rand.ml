(*
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 *  Sumit Gulwani       <gulwani@cs.berkeley.edu>
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

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let enabled = ref false

let doit (f: file) = 
  let rec doOneFunction (fi: fundec) = 
    ignore (E.log "RAND: doing function %s\n" fi.svar.vname);
    (* Let's do a topological sort of the statements. We scan statements and 
     * we remember those that we have done. We also keep a todo list. These 
     * are statements that are waiting on some predecessor to be done *)
    let root: stmt option = 
      match fi.sbody.bstmts with 
        [] -> (* Empty function *) None
      | f :: _ -> Some f
    in
    let todo: stmt list ref = 
      ref (match root with None -> [] | Some r -> [r]) in
    let doneStmts : (int, unit) H.t = H.create 13 in
    let doOneStatement (s: stmt) = 
      if H.mem doneStmts s.sid then () else begin
        ignore (E.log " %d," s.sid);
        H.add doneStmts s.sid ();
        (* Now add all successors to the todo list *)
        todo := s.succs @ !todo 
      end
    in
    let rec loop () = 
      match !todo with 
        [] -> (* We are done *) ()
      | n :: rest -> 
          (* Pick one that has all the predecessors done *)
          let ready, notready = 
            List.partition
              (fun n -> 
                H.mem doneStmts n.sid || 
                List.for_all (fun p -> H.mem doneStmts p.sid) n.preds)
              !todo
          in
          (* See if there are no ready statements *)
          if ready = [] then begin
            (* Break a cycle on the first element in todo *)
            ignore (E.log "(*)");
            todo := rest;
            doOneStatement n;
            loop ();
          end else begin
            todo := notready; (* notready is shorter now *)
            (* Do all the ready ones *)
            List.iter doOneStatement ready;
            loop ()
          end
    
    in 
    loop ();
    ignore (E.log "\n");
  in
  List.iter 
    (fun g -> 
      match g with 
        GFun (fi, _) -> doOneFunction fi
      | _ -> ())
    f.globals


let feature : featureDescr = 
  { fd_name = "rand";
    fd_enabled = enabled;
    fd_description = "randomized global value numbering";
    fd_extraopt = [];
    fd_doit = doit }

