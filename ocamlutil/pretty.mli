(*
 * Description:
 *
 * Copyright (c) 2000 by
 *  George C. Necula	necula@cs.berkeley.edu
 *   
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software for research purposes only is hereby granted, 
 * provided that the following conditions are met: 
 * 1.  Redistributions of source code must retain the above copyright notice, 
 * this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 * this list of conditions and the following disclaimer in the documentation 
 * and/or other materials provided with the distribution. 
 * 3. The name of the authors may not be used to endorse or promote products derived from 
 * this software without specific prior written permission. 
 *
 * DISCLAIMER:
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR IMPLIED 
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(* George Necula. 8/1/99. *)

(* Pretty printer. Works by first constructing a "doc" from the data 
 * structure to print and then issueing the formatting command. The "doc" 
 * contains all the strings, integers to be printed along with indications of 
 * hard line breaks and optional line breaks. The formatter decided which of 
 * the optional line breaks to take. It uses a greedy algorithm that is fast 
 * but not always optimal. A previous version used a better algorithm but it 
 * was not scaling up to large priting jobs. *)

type doc

(* empty document *)
val nil          : doc

(* concatenate two documents *)
val (++)         : doc -> doc -> doc    (* infix operator *)

val text         : string -> doc
val num          : int    -> doc
val chr          : char   -> doc


(* a hard line break *)
val line         : doc

(* keep left flushed .. should be preceded by a line *)
val leftflushline : doc


(* a soft line break. Such a break will be taken only if necessary to fit the 
 * document in a given width. If the break is not taken a space is printed  *)
val break        : doc

(* all taken line breaks between an align and a matching unalign are indented 
 * to the column of the align. Align and unalign do not take any space.  *)
val align        : doc
val unalign      : doc



(************** Now some syntactic sugar *****************)

(* indents the document. Same as text "  " ++ align ++ doc ++ unalign *)
val nest         : int -> doc -> doc

(* formats a sequence. The first argument is a separator *)
val seq          : doc -> ('a ->doc) -> 'a list ->doc


(************** The output functions *********************)
val fprint       : out_channel -> int -> doc -> unit
val sprint       :                int -> doc -> string



(* sprintf-like function. It supports everything that the Printf.printf 
 * function does and in addition: 
 *     @[ : inserts an align. Every format string must have matching 
 *          align/unaligns. See printDepth below. 
 *     @] : inserts an unalign
 *     @! : inserts a line (hard line break)
 *     @? : inserts a break (soft line break)
 *     @@ : inserts a @
 *
 * Produces a doc. The %a arguments take an unit as the first argument 
 * For example, if dExp : exp -> doc then define
 *    dExpU () = dExpU 
 * and write
 *    dprintf "%a" dExpU e
 * to produce a document obtained from applying dExp to e
 * This unit business is because I want to use the predefined "format" type. 
 * Such a type cannot be defined by the user in Caml.
 *)

val dprintf      : ('a, unit, doc) format -> 'a  

(* Like dprintf but more general. It also has a function that is invoked 
 * after the formating was done. *)
val gprintf      : (doc -> doc) -> ('a, unit, doc) format -> 'a

(* Keep track of nested align in gprintf. (Each gprintf format string must 
 * have properly nested align/unalign pairs. When the nesting depth surpasses 
 * !printDepth then we print ... and we skip until the matching unalign *)
val printDepth   : int ref

val noBreaks  : bool ref  (* If true then replaces all optional breaks with 
                           * space *)
val noAligns  : bool ref  (* If true then does not indent *)

val fastMode  : bool ref  (* If true the it takes breaks only when has 
                           * surpassed the given width. Used only for the 
                           * "george" algorithm. *)

val flushOften   : bool ref  (* If true the it flushes after every print *)

val withPrintDepth : int -> (unit -> unit) -> unit

(* And some instances of gprintf. Use with ignore because they return doc. *)
val fprintf      : out_channel -> ('a, unit, doc) format -> 'a  
val printf        : ('a, unit, doc) format -> 'a   (* to stdout *)
val eprintf       : ('a, unit, doc) format -> 'a  (* to stderr *)

(* We have a few handy functions that can be used with %a specifier in 
 * dprintf *)
val insert       : unit -> doc -> doc

(* formats an array. *)
val docArray     : (int -> 'a -> doc) -> unit -> 'a array -> doc
 


(* for an option *)
val docOpt       : (unit -> 'a -> doc) -> unit -> 'a option -> doc

(* for a list *)
val docList      : doc -> ('a -> doc) -> unit -> 'a list -> doc

(* A descrptive string with version, flags etc. *)
val getAboutString : unit -> string
