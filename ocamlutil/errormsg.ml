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

(***** Handling parsing errors ********)
type parseinfo =
    { mutable  linenum: int      ; (* Current line *)
      mutable  linestart: int    ; (* The position in the buffer where the 
                                    * current line starts *)
      mutable fileName  : string   ; (* Current file *)
      lexbuf          : Lexing.lexbuf;
      inchan          : in_channel option;
      mutable   num_errors : int;  (* Errors so far *)
    }
      
let dummyinfo = 
    { linenum   = 1;
      linestart = 0;
      fileName  = "" ;
      lexbuf    = Lexing.from_string "";
      inchan    = None;
      num_errors    = 0;
    }

type parseWhat = 
    ParseString of string
  | ParseFile of string

let current = ref dummyinfo



(* Change \ into / in file names. To avoid complications with escapes *)
let cleanFileName str = 
  (* ignore (log "cleanFilename(%s)\n" str);  *)
  let l = String.length str in
  let rec loop (copyto: int) (i: int) = 
     if i >= l then 
         String.sub str 0 copyto
     else 
       let c = String.get str i in
       if c <> '\\' then begin
          String.set str copyto c; loop (copyto + 1) (i + 1)
       end else begin
          String.set str copyto '/';
          if i < l - 2 && String.get str (i + 1) = '\\' then
              loop (copyto + 1) (i + 2)
          else 
              loop (copyto + 1) (i + 1)
       end
  in
  loop 0 0

let startParsing (fname: parseWhat) = 
  let inchan, fileName, lexbuf = 
    match fname with 
      ParseString s ->
        None, "<string>", Lexing.from_string s
    | ParseFile fname ->  
        let inchan = 
          try open_in fname with 
            _ -> s (error "Cannot find input file %s" fname) in
        let lexbuf = Lexing.from_channel inchan in
        Some inchan, cleanFileName fname, lexbuf
  in
  let i = 
    { linenum = 1; linestart = 0; 
      fileName = fileName;
      lexbuf = lexbuf; inchan = inchan;
      num_errors = 0 } in
  current := i;
  lexbuf

let finishParsing () = 
  let i = !current in
  (match i.inchan with 
    Some inch -> close_in inch | _ -> ());
  current := dummyinfo


(* Call this function to announce a new line *)
let newline () = 
  let i = !current in
  i.linenum <- 1 + i.linenum;
  i.linestart <- Lexing.lexeme_start i.lexbuf


let setCurrentLine (i: int) = 
  !current.linenum <- i

let setCurrentFile (n: string) = 
  !current.fileName <- cleanFileName n


let max_errors = 20  (* Stop after 20 errors *)

let parse_error (msg: string) 
                (token_start: int) 
                (token_end: int) : unit =
  let i = !current in
  let adjStart = 
    if token_start < i.linestart then 0 else token_start - i.linestart in
  let adjEnd = 
    if token_end < i.linestart then 0 else token_end - i.linestart in
  output_string 
    stderr
    (i.fileName ^ "[" ^ (string_of_int i.linenum) ^ ":" 
                        ^ (string_of_int adjStart) ^ "-" 
                        ^ (string_of_int adjEnd) 
                  ^ "]"
     ^ " : " ^ msg);
  output_string stderr "\n";
  flush stderr ;
  i.num_errors <- i.num_errors + 1;
  if i.num_errors > max_errors then begin
    output_string stderr "Too many errors. Aborting.\n" ;
    exit 1 
  end


(* Keep here the current pattern for formatparse *)
let currentPattern = ref ""


(* More parsing support functions: line, file, char count *)
let getPosition () : int * string * int = 
  let i = !current in 
  i.linenum, i.fileName, Lexing.lexeme_start i.lexbuf


(* Keep here some pointers to lexer functions *)
let push_context = 
    ref (fun _ -> raise (Failure "Errormsg.push_context not set"))
let add_type = 
    ref (fun _ -> raise (Failure "Errormsg.add_type not set"))
let add_identifier = 
    ref (fun _ -> raise (Failure "Errormsg.add_identifier not set"))
let pop_context = 
    ref (fun _ -> raise (Failure "Errormsg.pop_context not set"))
