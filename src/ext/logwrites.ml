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

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

class logWriteVisitor = object
  inherit nopCilVisitor
  (* Create a prototype for the logging function, but don't put it in the 
   * file *)
  val printfFun =   
    let fdec = emptyFunction "syslog" in
    fdec.svar.vtype <- TFun(intType, 
                            Some [ ("prio", intType, []);
                                   ("format", charConstPtrType, []) ], 
                            true, []);
    fdec

  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Set(lv, e, l) -> begin
        (* Check if we need to log *)
        match lv with 
          (Var(v), off) when not v.vglob -> SkipChildren
        | _ -> let str = Pretty.sprint 80 
                (Pretty.dprintf "Write %%p to 0x%%08x at %%s:%%d (%a)\n" d_lval lv)
              in 
              ChangeTo 
              [ Call((None), (Lval(Var(printfFun.svar),NoOffset)), 
                     [ one ; Const(CStr str) ; e ; AddrOf lv; Const(CStr
                     l.file); integer l.line], locUnknown);
              i]
      end 
    | Call(Some lv, f, args, l) -> begin
        (* Check if we need to log *)
        match lv with 
          (Var(v), off) when not v.vglob -> SkipChildren
        | _ -> let str = Pretty.sprint 80 
                (Pretty.dprintf "Write retval to 0x%%08x at %%s:%%d (%a)\n" d_lval lv)
              in 
              ChangeTo 
              [ Call((None), (Lval(Var(printfFun.svar),NoOffset)), 
                     [ one ; Const(CStr str) ; AddrOf lv; Const(CStr
                     l.file); integer l.line], locUnknown);
              i]
      end 
    | _ -> SkipChildren

end

let logWrites (f: file) : unit = 
  let lwVisitor = new logWriteVisitor in
  visitCilFileSameGlobals lwVisitor f
