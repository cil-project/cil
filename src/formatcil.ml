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
open Cil
open Pretty
open Trace      (* sm: 'trace' function *)
module E = Errormsg
module H = Hashtbl



let doParse (prog: string) 
            (theParser: (Lexing.lexbuf -> Formatparse.token) 
                                          -> Lexing.lexbuf -> 'a) = 
  let lexbuf = Formatlex.init prog in
  try
    Formatparse.initialize Formatlex.initial lexbuf;
    theParser Formatlex.initial lexbuf
  with Parsing.Parse_error -> begin
    E.s (E.error "Parsing error: %s" prog)
  end
  | e -> begin
      ignore (E.log "Caught %s while parsing\n" (Printexc.to_string e));
      raise e
  end

let fExp (prog: string) : formatArg list -> exp = 
  let cf = doParse prog Formatparse.expression in
  (fst cf)

let fLval (prog: string) : formatArg list -> lval = 
  let cf = doParse prog Formatparse.lval in
  (fst cf)

let fType (prog: string) : formatArg list -> typ = 
  let cf = doParse prog Formatparse.typename in
  (fst cf)

let fInstr (prog: string) : location -> formatArg list -> instr = 
  let cf = doParse prog Formatparse.instr in
  (fst cf)

  
let makeProg (fl: formatArg list) : string * formatArg list = 
  (* Construct the program *)
  List.fold_right 
    (fun f (progacc, argacc) -> 
      match f with 
        FX s -> (s ^ progacc, argacc)
      | Fe _ -> ("%e" ^ progacc, f :: argacc)
      | FE _ -> ("%E" ^ progacc, f :: argacc)
      | Fv _ -> ("%v" ^ progacc, f :: argacc)
      | Fc _ -> ("%c" ^ progacc, f :: argacc)
      | Fd _ -> ("%d" ^ progacc, f :: argacc)
      | Fo _ -> ("%o" ^ progacc, f :: argacc)
      | Fl _ -> ("%l" ^ progacc, f :: argacc)
      | Fi _ -> ("%i" ^ progacc, f :: argacc)
      | Ft _ -> ("%t" ^ progacc, f :: argacc))
    fl
    ("", [])

let fExp' (fl: formatArg list) : exp = 
  let prog, args = makeProg fl in
  fExp prog args




(* Match an expression *)
let mExp (prog: string) : exp -> formatArg list option = 
  let df = doParse prog Formatparse.expression in
  (snd df)

(* Match an lvalue *)
let mLval (prog: string) : lval -> formatArg list option = 
  let df = doParse prog Formatparse.lval in
  (snd df)


(* Match a type *)
let mType (prog: string) : typ -> formatArg list option = 
  let df = doParse prog Formatparse.typename in
  (snd df)



(* Match an instruction *)
let mInstr (prog: string) : instr -> formatArg list option = 
  let df = doParse prog Formatparse.instr in
  (snd df)

