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

(******************************************************************************)
(* Pretty printer - Aman's linked-list-doc version*)

(* Debugging stuff *)
let debug =  false
let dbgPrintf x = Printf.fprintf stderr x

(* Some flags etc. *)
let flushOften = ref false
let noBreaks = ref false  (* Replace all soft breaks with space *)
let noAligns = ref false (* Whether to obey align/unalign *)

(* Keep track of nested align in gprintf. Each gprintf format string must 
 * have properly nested align/unalign pairs. When the nesting depth surpasses 
 * !printDepth then we print ... and we skip until the matching unalign *)
let printDepth = ref 100 (* WRW: must see whole thing *)
let alignDepth = ref 0
    
(******************************************************************************)
(* The doc data structure *)

type 'a linkedListCell               = {mutable next : 'a} (* pref. abbrev = c *)
type ('a, 'b) linkedListCellWithData = {mutable data : 'b ; mutable dnext : 'a} (* pref. abbrev = dc *)

type doc =
    Nil      (* a terminator *)
  | Text     of (doc, string) linkedListCellWithData
  | Break    of doc linkedListCell
  | Line     of doc linkedListCell
  | Align    of doc linkedListCell
  | Unalign  of doc linkedListCell
  | HeadTail of (doc, doc) linkedListCellWithData (* data is the tail ... assumption: data <> Nil *)


(******************************************************************************)
(* Constructing docs ... each one is O(1) *)

let nill         = Nil     (* Also see nildoc *)
let headtail (head : doc) (tail : doc) = HeadTail {dnext=head; data=tail}
let text    s   = Text    {dnext=nill; data=s}
let chr     c   = text (String.make 1 c) 
let num     n   = text (string_of_int n)
let break   ()  = Break   {next=nill}
let line    ()  = Line    {next=nill}
let align   ()  = Align   {next=nill}
let unalign ()  = Unalign {next=nill}
let nildoc  ()  = text "" (* Serves as a doc to start from *)

(******************************************************************************)
(* Small operations on docs ... each one is O(1)*)

let setnext (doc : doc) (newnext : doc) =
  match doc with
    Text dc     -> dc.dnext <- newnext
  | HeadTail dc -> dc.dnext <- newnext
  | Break c | Line c | Align c | Unalign c -> c.next <- newnext
  | Nil         -> raise (Failure "setnext: Nil ??")

let getnext (doc : doc) : doc =
  match doc with
    Text dc     -> dc.dnext
  | HeadTail dc -> dc.dnext
  | Break c | Line c | Align c | Unalign c -> c.next
  | Nil         -> raise (Failure "getnext: Nil ??")
  
(******************************************************************************)
(* Larger operations on docs ... each one is O(n) *)

(* copy:
   Compacts and copies a doc deeply
   The result contains no HeadTails
*)
let rec copy = function
    Nil       -> Nil
  | Text   dc -> Text {data=dc.data; dnext=copy dc.dnext}
  | Break   c -> Break {next=copy c.next}
  | Line    c -> Line  {next=copy c.next}
  | Align   c -> Align {next=copy c.next}
  | Unalign c -> Unalign {next=copy c.next}
  | HeadTail c -> copy c.dnext

(* findtail:
   Returns a HeadTail representing the head and tail of the given doc
   (findtail Nil) throws a Failure
*)
let rec findtail (doc : doc) : doc =
  match doc with
    HeadTail dc -> findtail dc.data
  | Text dc -> 
      if dc.dnext = Nil then doc
      else findtail dc.dnext	      
  | Break c | Line c | Align c | Unalign c -> 
      if c.next = Nil then doc
      else findtail c.next	
  | Nil -> raise (Failure "Nil has no tail")


let findheadtail (doc : doc) : doc =
  match doc with
    HeadTail _ -> doc
  | _ -> headtail doc (findtail doc)


(* +=:
   Appends the second doc to the first and returns the appended result
   Warning: a += a will create a cycle
*)
let rec (+=) (d1 : doc) (d2 : doc) : doc = (* amortized O(1) *)
  match d2 with                      (* These 3 lines cause the doc to always have one HeadTail (at the start) *)
    HeadTail dc -> d1 += dc.dnext    (* ... *)
  | _ ->                             (* ... *)
      match d1,d2 with Nil,_ -> d2 | _,Nil -> d1 | _ ->
	let ht = findheadtail d1
	and tl = findtail d2 in
	match ht with
	  HeadTail dc -> 
	    setnext dc.data d2;
	    dc.data <- tl;
	    ht
	| _ -> raise (Failure "+=: huh?")
	      
(* ++:
   Appends d2 to a copy of d1 .. so d1 is left unchanged
   Warning: Twice as slow!
*)
let (++) d1 d2 =
  (copy d1) += d2


(* seq: 
   Produces a doc from the elements of a given list separated by sep
   The function f translates the list elements into docs
   Note: sep should produce new separator docs at each call
*)
let seq (sep : unit -> doc) (f : 'a -> doc) (dl : 'a list) : doc =
  let rec seqloop sep f dl acc =
    match dl with 
      [] -> acc
    | h :: t -> 
	let fh = f h in
	seqloop sep f t (acc += (sep ()) += fh)
  in match dl with
    [] -> nildoc ()
  | h :: t -> seqloop sep f t (f h)
  

(******************************************************************************)
(* I just have these here because it was there in pretty.ml 
*)
let docList sep f () dl = seq sep f dl

let docArray f () a = 
  let len = Array.length a in
  if len = 0 then 
    nildoc ()
  else
    (* make it tail-recursive *)
    let rec loop (acc: doc) i =
      if i >= len then acc else
      let fi = f i a.(i) in (* Make sure this is done first *)
      loop (acc += fi) (i + 1)
    in
    let f0 = f 0 a.(0) in
    loop f0 1
      
let docOpt delem () = function
    None -> text "None"
  | Some e -> text "Some(" += (delem () e) += text ")"
  
let insert () d = d

let nest   n  d = text " " += align () += d += unalign ()


      
(******************************************************************************)
let rec dbgPrintDoc ?(chn = stdout) doc =
  let countNodes = ref 0
  and countHTs   = ref 0 in
  let rec dbgPrintDocLoop doc = 
    countNodes := !countNodes + 1;
    match doc with
      Nil ->     Printf.fprintf chn ".\n"
    | HeadTail dc -> countHTs := !countHTs +1; dbgPrintDocLoop dc.dnext
    | Text   dc -> Printf.fprintf chn "%s " dc.data; dbgPrintDocLoop dc.dnext
    | Break   c -> Printf.fprintf chn "@? "; dbgPrintDocLoop c.next
    | Line    c -> Printf.fprintf chn "@! "; dbgPrintDocLoop c.next
    | Align   c -> Printf.fprintf chn "@[ "; dbgPrintDocLoop c.next
    | Unalign c -> Printf.fprintf chn "@] "; dbgPrintDocLoop c.next
  in
  dbgPrintDocLoop doc;
  Printf.fprintf chn "Summary: %d nodes with %d HeadTail's\n" !countNodes !countHTs
  
  
  
(******************************************************************************)

(* Based on earlier pretty.ml based on printf.ml *)
external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let gprintf (finish : doc -> doc)  
    (format : ('a, unit, doc) format) : 'a =
  let format = (Obj.magic format : string) in

  (* Record the starting align depth *)
  let startAlignDepth = !alignDepth in
  (* Special concatenation functions *)
  let dconcat (acc: doc) (another: doc) = 
    if !alignDepth > !printDepth then acc else acc += another in
  let dctext1 (acc: doc) (str: string) = 
    if !alignDepth > !printDepth then acc else 
    acc += text str in
  (* Special finish function *)
  let dfinish dc = 
    if !alignDepth <> startAlignDepth then
      prerr_string ("Unmatched align/unalign in " ^ format ^ "\n");
    finish dc
  in
  let flen    = String.length format in
                                        (* Reading a format character *)
  let fget    = String.unsafe_get format in
                                        (* Output a literal sequence of 
                                         * characters, starting at i. The 
                                         * character at i does not need to be 
                                         * checked.  *) 
  let rec literal acc i = 
    let rec skipChars j = 
      if j >= flen || 
      (match fget j with 
        '%' -> true 
      | '@' -> true 
      | _ -> false) then
        collect (dctext1 acc (String.sub format i (j-i))) j
      else
        skipChars (succ j)
    in
    skipChars (succ i)
                                        (* the main collection function *)
  and collect (acc: doc) (i: int) = 
    if i >= flen then begin
      Obj.magic (dfinish acc) 
    end else begin
      let c = fget i in
      if c = '%' then begin
        let j = skip_args (succ i) in
        match fget j with
          '%' -> literal acc j 
        | 's' ->
            Obj.magic(fun s ->
              let str = 
                if j <= i+1 then
                  s
                else
                  let sl = String.length s in
                  let p =
                    try
                      int_of_string (String.sub format (i+1) (j-i-1))
                    with _ ->
                      invalid_arg "fprintf: bad %s format" in
                  if p > 0 && sl < p then
                    (String.make (p - sl) ' ') ^ s
                  else if p < 0 && sl < -p then
                    s ^ (String.make (-p - sl) ' ')
                  else
                    s
              in
              collect (dctext1 acc str) (succ j))
        | 'c' ->
            Obj.magic(fun c ->
              collect (dctext1 acc (String.make 1 c)) (succ j))
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun n ->
              collect (dctext1 acc
                         (format_int (String.sub format i 
                                                  (j-i+1)) n))
                (succ j))
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            Obj.magic(fun f ->
              collect (dctext1 acc
                         (format_float (String.sub format i (j-i+1)) f))
                (succ j))
        | 'b' ->
            Obj.magic(fun b ->
              collect (dctext1 acc (string_of_bool b)) (succ j))
        | 'a' ->
            Obj.magic(fun pprinter arg ->
              collect (dconcat acc (pprinter () arg)) (succ j))
        | 't' ->
            Obj.magic(fun pprinter ->
              collect (dconcat acc (pprinter ())) (succ j))
        | c ->
            invalid_arg ("dprintf: unknown format %s" ^ String.make 1 c)

      end else if c = '@' then begin
        if i + 1 < flen then begin
          match fget (succ i) with

                                        (* Now the special format characters *)
            '[' ->                      (* align *)
              let newacc = 
                if !alignDepth > !printDepth then
                  acc
                else if !alignDepth = !printDepth then
                  acc += text "..."
                else
                  acc += align ()
              in
              incr alignDepth;
              collect newacc (i + 2)
                
          | ']' ->                        (* unalign *)
              decr alignDepth;
              let newacc = 
                if !alignDepth >= !printDepth then
                  acc
                else
                  acc += unalign ()
              in
              collect newacc (i + 2)
          | '!' ->                        (* hard-line break *)
              collect (dconcat acc (line())) (i + 2)
          | '?' ->                        (* soft line break *)
              collect (dconcat acc (break())) (i + 2)
          | '@' -> 
              collect (dctext1 acc "@") (i + 2)
          | c ->
              invalid_arg ("dprintf: unknown format @" ^ String.make 1 c)
        end else
          invalid_arg "dprintf: incomplete format @"
      end else if c = '\\' then begin
        if i + 1 < flen then begin
          match fget (succ i) with
            'n' ->                      
              collect (dconcat acc (line())) (i + 2)
          | _ -> literal acc i
        end else
          invalid_arg "dprintf: incomplete escape \\"
      end else
        literal acc i
    end

  and skip_args j =
    match String.unsafe_get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j

  in
  collect (nildoc()) 0

let withPrintDepth dp thunk = 
  let opd = !printDepth in
  printDepth := dp;
  thunk ();
  printDepth := opd


(****************************************************************************************)
(* qprint *)
(* Prints a document with indentation, without backtracking *)

(* Abstraction for qprint-channels *)
type qprintChannel =
    QOut_channel of out_channel
  | QBuffer of Buffer.t

let writeString (s : string) = function
    QOut_channel chn -> output_string chn s
  | QBuffer buf -> Buffer.add_string buf s

let writeChar (c : char) = function 
    QOut_channel chn -> output_char chn c
  | QBuffer buf -> Buffer.add_char buf c



let rec writeIndent_out n chn =
  if n > 0 then begin
    output_char chn ' ';
    writeIndent_out (n - 1) chn
  end 
let rec writeIndent_buf n buf =
  if n > 0 then begin
    Buffer.add_char buf ' ';
    writeIndent_buf (n - 1) buf
  end
let writeIndent (n : int) = function
    QOut_channel chn -> writeIndent_out n chn     
  | QBuffer buf -> writeIndent_buf n buf


(* Calculate qalignData.sizes *)
type docStream = unit -> doc

let qPreprocess doc width =
  let stack = ref doc in  
  let rec getNext () = match !stack with
    Nil -> nill
  | _ -> stack := getnext !stack; !stack
  in
  let docstr = getNext in
  let breakList : int ref list ref = ref [] in
  let updateBreakList = ref false in
  let rec getSize (acc : int) : int  =
    match docstr () with
      Text s -> getSize (acc + String.length s.data)
    | Break _ -> 
	if (!updateBreakList) then breakList := (ref (-1)) :: !breakList 
	else updateBreakList := true; 
	getSize (acc + 1)
    | Align _ -> 
	let x = ref (-2) in
	if !updateBreakList then begin
	  updateBreakList := false;
	  breakList := x :: !breakList;
	end;
	   x := getSize 0;	
	getSize (acc + !x)
    | Unalign _ -> 
	(*if !updateBreakList then breakList := (ref (-1)) :: !breakList;
	updateBreakList := false; *)
	acc
    | Line _ -> 
	if (!updateBreakList) then breakList := (ref (-1)) :: !breakList;
	updateBreakList := false; 
	getSize 0
    | Nil -> acc
    | _ -> raise (Failure "docStream returned nonleaf")
  in 
  let _ = getSize 0 in begin
    if !updateBreakList then breakList := (ref (-1)) :: !breakList;
    List.rev !breakList
  end
   


let qprint qchn width doc = 
  (* let curPos = ref 0 in *)
  let alignStack  = ref [0] in
  let breakList :  int ref list ref = ref (qPreprocess doc width) in
  let breakLine () = begin
    writeChar '\n' qchn;
    (* for i=1 to (List.hd !alignStack) do writeChar ' ' qchn done;*)
    writeIndent (List.hd !alignStack) qchn; (* AB: Why doesn't this make it faster ? *)
    List.hd !alignStack 
  end in
  let decide2break curPos = begin
    match !breakList with
      size :: rest -> 
	breakList := rest;
	(curPos >= width) || 
	if (!size <= 0) then false
	else (width <= curPos + !size)
    | _ -> raise (Failure "breakList contains too few breaks")
  end  in
  let rec qprintLoop (curPos : int) : doc -> int = function
      Nil -> curPos
    | HeadTail dc -> qprintLoop curPos dc.dnext
    | Text s -> 
	writeString s.data qchn;
	qprintLoop (curPos + String.length s.data) s.dnext
    | Break c -> 
	(* if (!curPos + 10 >= width) then begin *)
	if (decide2break curPos) then begin
	  qprintLoop (breakLine()) c.next
	end
	else begin
	  writeChar ' ' qchn;
	  qprintLoop (curPos + 1) c.next
	end	  
    | Line c -> 
	qprintLoop (breakLine ()) c.next
    | Align c -> 
	alignStack := curPos :: !alignStack;
	qprintLoop curPos c.next
    | Unalign c -> 
	alignStack := List.tl !alignStack;
	qprintLoop curPos c.next
  in 
  ignore (qprintLoop 0 doc)
  

let rec qveryfastprint qchn = function
    Nil -> ()
  | Text s -> writeString s.data qchn ; qveryfastprint qchn s.dnext
  | Break c -> writeChar ' ' qchn ; qveryfastprint qchn c.next
  | Line c ->  writeChar '\n' qchn ; qveryfastprint qchn c.next
  | Align c | Unalign c -> qveryfastprint qchn c.next
  | HeadTail dc -> qveryfastprint qchn dc.dnext


let qprintf format doc =
  qveryfastprint (QOut_channel stdout) doc 
(* qprint (QOut_channel stdout) 80 doc  *)

let qfprint chn width doc =
(*  qveryfastprint (QOut_channel chn) doc e*)
  qprint (QOut_channel chn) width doc 

let qsprint width doc =
  let buf = Buffer.create 1024 in
  qprint (QBuffer buf) width doc;
  Buffer.contents buf


(******************************************************************************)
(* TODO !! *)
let fprint = qfprint
let sprint = qsprint

(******************************************************************************)
let dprintf format     = gprintf (fun x -> x) format
let fprintf chn format = 
  let f d = fprint chn 80 d; d in
	(* weimeric hack begins -- flush output to streams *)
  let res = gprintf f format in
	(* save the value we would have returned, flush the channel and then 
         * return it -- this allows us to see debug input near infinite loops 
         * *)
  if !flushOften then flush chn;
  res
	(* weimeric hack ends *)

let printf format = fprintf stdout format
let eprintf format = fprintf stderr format

(******************************************************************************)
