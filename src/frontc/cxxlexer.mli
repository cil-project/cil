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


(* This interface is generated manually. The corresponding .ml file is 
 * generated automatically and is placed in ../obj/clexer.ml. The reason we 
 * want this interface is to avoid confusing make with freshly generated 
 * interface files *)

(*
type handle =
	bool * in_channel * string * string * int * int * out_channel * string
*)

val init: filename:string -> inchannel:in_channel -> Lexing.lexbuf

(*
val current_handle: handle ref

val get_buffer: handle ref -> string -> int -> int
*)

(* Display an error given two positions int he parsing buffer *)
val display_error: string -> int -> int -> unit

(*
val lineno: handle -> int
val file_name: handle -> string
*)

val currentFile: string ref
val currentLine: int ref 

val push_context: unit -> unit (* Start a context *)

val add_type: string -> unit (* Add a new string as a type name *)
val add_class: string -> unit (* Add a new string as a class name *)
val add_namespace: string -> unit (* Add a new string as a namespace *)
val add_identifier: string -> unit (* Add a new string as a variable name *)

val pop_context: unit -> unit (* Remove all names added in this context *)
(* This is the main parser function *)
val initial: Lexing.lexbuf -> Cxxparser.token

