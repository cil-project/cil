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
 * 1. Redistributions of source code must retain the above copyright notice, 
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

(* Keep here the globally-visible flags *)
let doCheck= ref false   (* Whether to check CIL *)

let logCalls = ref false (* Whether to produce a log with all the function 
                          * calls made *)
let logWrites = ref false (* Whether to produce a log with all the mem 
                          * writes made *)

let printStats = ref false

(* when 'sliceGlobal' is set, then when 'rmtmps' runs, only globals*)
(* marked with #pragma cilnoremove(whatever) are kept; when used with *)
(* cilly.asm.exe, the effect is to slice the input on the noremove symbols *)
let sliceGlobal = ref false


let printStages = ref false

(* A tryFinally function *)
let tryFinally 
    (main: 'a -> 'b) (* The function to run *)
    (final: 'b option -> unit)  (* Something to run at the end *)
    (arg: 'a) : 'b = 
  let res =
    try
      main arg
    with e -> begin
      final None;
      raise e
    end
  in
  final (Some res)


let doCxxPP = ref false

let libDir = ref ""
