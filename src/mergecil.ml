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

open Pretty
open Trace      (* sm: 'trace' function *)
module E = Errormsg
module H = Hashtbl

let debugMerge = true

(** Merge a number of CIL files *)

(** A number of name spaces *)
type nameSpace = NType | NStruct | NUnion | NEnum | NVar

let ns2string = function
    NType -> "NType"
  | NStruct -> "NStruct"
  | NUnion -> "NUnion"
  | NEnum -> "NEnum"
  | NVar -> "NVar"

(** A number of global environments *)
  (* Types *)
let tEnv : (string, typ * location) H.t = H.create 111
  (* Structures and unions *)
let cEnv : (string, compinfo * location) H.t = H.create 111
let eEnv : (string, enuminfo * location) H.t = H.create 111 (* enumerations *)
let vEnv : (string, varinfo * location) H.t = H.create 111 (* variables and functions *)

let env : (nameSpace * string, global) = H.create
(** A number of alpha conversion tables *)
let tAlpha : (string, int ref) H.t = H.create 57 (* Types *)

let init () = 
  H.clear tAlpha;
  H.clear sAlpha;
  H.clear uAlpha;
  H.clear eAlpha;
  H.clear vAlpha;

  H.clear tEnv;
  H.clear sEnv;
  H.clear uEnv;
  H.clear eEnv;
  H.clear vEnv

let merge (files: file list) (newname: string) : file = 
  init ();
  (* Scan all files and initialize the alpha tables with all the global names 
   * so that when we rename later we do not generate new names *)
  List.iter 
    (fun fl -> 
      List.iter 
        (function 
            GType (n, _, _) -> 
              if not (H.mem tAlpha n) then 
                H.add tAlpha n (ref -1)
          | GDecl (vi, _) | GVar (vi, _, _) ->
              if not (H.mem vAlpha vi.vname) then 
                H.add vAlpha vi.vname (ref -1)
          | GFun (fdec, _) ->
              if not (H.mem vAlpha fdec.svar.vname) then 
                H.add vAlpha fdec.svar.vname (ref -1)
          | GCompTag (ci, _) -> 
              if ci.cstruct then begin
                if not (H.mem sAlpha ci.cname) then 
                  H.add sAlpha ci.cname (ref -1)
              end else begin 
                if not (H.mem uAlpha ci.cname) then 
                  H.add uAlpha ci.cname (ref -1)
              end
          | GEnumTag (ei, _) ->
              if not (H.mem eAlpha ci.cname) then 
                H.add eAlpha ci.cname (ref -1)
          | _ -> ())
        fl.globals)
    files;

  (* Do one file at a time *)

  (* Accumulate here the globals in the merged file *)
  let theFileTypes = ref [] in
  let theFile      = ref [] in
  
  let rec doOneFile (f:file) : unit = 
    if f.globinitcalled || f.globinit <> None then
      E.s (E.warn "Merging file %s has global initializer" f.fileName);
    (* First scan the file and see for what globals we can reuse their name *)
    ()
  in
  List.iter doOneFile files;
  (* Now reverse the result and return the resulting file *)
  let rec revonto acc = function
      [] -> acc
    | x :: t -> revonto (x :: acc) t
  in
  let res = 
    { fileName = newname;
      globals  = revonto (revonto [] !theFile) !theFileTypes;
      globinit = None;
      globinitcalled = false } in
  init (); (* Make the GC happy *)
  res



