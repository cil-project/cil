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

open Pretty

type info =
    { mutable  linenum: int      ;
      mutable  linepos: int list ;
      fileName        : string   ;
      mutable   errors: bool     }
      
let current : info ref = 
  ref 
    { linenum  = 1 ;
      linepos  = [1] ;
      fileName = "" ;
      errors   = false }

let fileName i = i.fileName

let startFile fname =
  current := { linenum  = 1 ;
               linepos  = [0] ;
               fileName = fname ; 
               errors   = false ; }


let startNewline n =
  let i = !current in
  begin
    i.linenum <- i.linenum + 1 ;
    i.linepos <- n :: i.linepos
  end

let getLineCol (i : info) pos = 
  let rec look n = function
      a::_ when (a<=pos) -> (n, pos - a)
    | _::rest -> look (n - 1) rest
    | _ -> (0, 0)
  in
  let (lin,col) = look i.linenum i.linepos in
  Printf.sprintf "%d.%d" lin col

let debugFlag  = ref false              (* If set then print debugging info *)
let verboseFlag = ref false

(**** Error reporting ****)  
exception Error
let s (d : doc) = raise Error

let hadErrors = ref false

let errorContext = ref []
let pushContext f = errorContext := f :: (!errorContext)
let popContext () = 
  match !errorContext with 
    _ :: t -> errorContext := t
  | [] -> s (eprintf "Bug: cannot pop error context")


let withContext ctx f x = 
  pushContext ctx;
  try
    let res = f x in
    popContext ();
    res
  with e -> begin
    popContext ();
    raise e
  end
  
                                        (* Make sure that showContext calls 
                                         * each f with its appropriate 
                                         * errorContext as it was when it was 
                                         * pushed *)
let showContext () = 
  let rec loop = function
      [] -> ()
    | f :: rest -> (errorContext := rest; (* Just in case f raises an error *)
                    ignore (eprintf "  Context : %t@!" f);
                    loop rest)
  in
  let old = !errorContext in
  try 
    loop old;
    errorContext := old
  with e -> begin
    errorContext := old;
    raise e
  end

let contextMessage name d = 
  ignore (eprintf "@!%s: %a@!" name insert d);
  showContext ()

let warnFlag = ref false

let bug (fmt : ('a,unit,doc) format) : 'a = 
  let f d =  hadErrors := true; contextMessage "Bug" d; raise Error in
  Pretty.gprintf f fmt

let error (fmt : ('a,unit,doc) format) : 'a = 
  let f d = hadErrors := true; contextMessage "Error" d; raise Error in
  Pretty.gprintf f fmt

let unimp (fmt : ('a,unit,doc) format) : 'a = 
  let f d = hadErrors := true; contextMessage "Unimplemented" d; raise Error in
  Pretty.gprintf f fmt

let warn (fmt : ('a,unit,doc) format) : 'a = 
  let f d = contextMessage "Warning" d; nil in
  Pretty.gprintf f fmt

let warnOpt (fmt : ('a,unit,doc) format) : 'a = 
  let f d = 
    if !warnFlag then contextMessage "Warning" d; 
    nil in
  Pretty.gprintf f fmt


let logChannel : out_channel ref = ref stderr

let log (fmt : ('a,unit,doc) format) : 'a = 
  let f d = fprint !logChannel 80 d; flush !logChannel; d in
  Pretty.gprintf f fmt



let theLexbuf = ref (Lexing.from_string "")

let fail format = Pretty.gprintf (fun x -> Pretty.fprint stderr 80 x; 
                                           raise (Failure "")) format
