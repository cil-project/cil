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
(* Pretty printer 
   This module contains several fast, but sub-optimal heuristics to pretty-print 
   structured text. 
*)

let debug =  false

(* Choose an algorithm *)
type algo = George | Aman | Gap
let  algo = George
let fastMode       = ref false


(******************************************************************************)	
(* The doc type and constructors *)

type doc = 
    Nil
  | Text     of string
  | Concat   of doc * doc
  | CText    of doc * string
  | Break
  | Line 
  | LeftFlush
  | Align
  | Unalign  

let nil  = Nil
let text s        = Text s
let num  i        = text (string_of_int i)
let chr  c        = text (String.make 1 c) 
let align         = Align
let unalign       = Unalign
let line          = Line
let leftflush     = LeftFlush
let break         = Break  

(* Note that all infix operators in Ocaml are left-associative. This means 
 * that if you have a long list of ++ then the whole thing is very unbalanced 
 * towards the left side. This is the worst possible case since scanning the 
 * left side of a Concat is the non-tail recursive case. *)

let (++) d1 d2 = Concat (d1, d2)

let indent n d   = text " " ++ align ++ d ++ unalign


(* Format a sequence. The first argument is a separator *)
let seq ~(sep:doc)  ~(doit:'a -> doc) ~(elements: 'a list) = 
  let rec loop (acc: doc) = function
      []     -> acc
    | h :: t -> 
        let fh = doit h in  (* Make sure this is done first *)
        loop (acc ++ sep ++ fh) t
  in
  (match elements with
    [] -> nil
  | h :: t -> 
      let fh = doit h in loop fh t)


let docArray ~(sep:doc) ~(doit:int -> 'a -> doc) () ~(elements:'a array) = 
  let len = Array.length elements in
  if len = 0 then 
    nil
  else
    let rec loop (acc: doc) i =
      if i >= len then acc else
      let fi = doit i elements.(i) in (* Make sure this is done first *)
      loop (acc ++ sep ++ fi) (i + 1)
    in
    let f0 = doit 0 elements.(0) in
    loop f0 1

let docOpt delem () = function
    None -> text "None"
  | Some e -> text "Some(" ++ (delem () e) ++ chr ')'



let docList ~(sep:doc) ~(doit:'a -> doc) () ~(elements:'a list) = 
  seq sep doit elements

let insert () d = d


(******************************************************************************)	
(* Some debugging stuff *)

let dbgprintf x = Printf.fprintf stderr x

let rec dbgPrintDoc = function
    Nil -> dbgprintf "(Nil)"
  | Text s -> dbgprintf "(Text %s)" s
  | Concat (d1,d2) -> dbgprintf ""; dbgPrintDoc  d1; dbgprintf " ++\n "; 
      dbgPrintDoc  d2; dbgprintf ""
  | CText (d,s) -> dbgPrintDoc  d; dbgprintf " ++ \"%s\"" s; 
  | Break -> dbgprintf "(Break)" 
  | Line -> dbgprintf "(Line)"
  | LeftFlush -> dbgprintf "(LeftFlush)"
  | Align -> dbgprintf "(Align)"
  | Unalign -> dbgprintf "(Unalign)"


(******************************************************************************)	
(* The "george" algorithm *)

(* When we construct documents, most of the time they are heavily unbalanced 
 * towards the left. This is due to the left-associativity of ++ and also to 
 * the fact that constructors such as docList construct from the let of a 
 * sequence. We would prefer to shift the imbalance to the right to avoid 
 * consuming a lot of stack when we traverse the document *)
let rec flatten (acc: doc) = function
  | Concat (d1, d2) -> flatten (flatten acc d2) d1
  | CText (d, s) -> flatten (Concat(Text s, acc)) d
  | Nil -> acc (* Get rid of Nil *)
  | d -> Concat(d, acc)

(* We keep a stack of active aligns. *)
type align = 
    { mutable gainBreak: int;  (* This is the gain that is associated with 
                                 * taking the break associated with this 
                                 * alignment mark. If this is 0, then there 
                                 * is no break associated with the mark *)
      mutable isTaken: bool ref; (* If breakGain is > 0 then this is a ref 
                                  * cell that must be set to true when the 
                                  * break is taken. These ref cells are also 
                                  * int the "breaks" list  *)
            deltaFromPrev: int ref; (* The column of this alignment mark - 
                                     * the column of the previous mark. 
                                     * Shared with the deltaToNext of the 
                                     * previous active align  *)
             deltaToNext: int ref  (* The column of the next alignment mark - 
                                    * the columns of this one. Shared with 
                                    * deltaFromPrev of the next active align *)
    } 
      
(* We use references to avoid the need to pass data around all the time *)
let aligns: align list ref =  (* The current stack of active alignment marks, 
                               * with the top at the head. Never empty.  *)
  ref [{ gainBreak = 0; isTaken = ref false; 
         deltaFromPrev = ref 0; deltaToNext = ref 0; }]

let topAlignAbsCol = ref 0 (* The absolute column of the top alignment *)

let pushAlign (abscol: int) = 
  let topalign = List.hd !aligns in
  let res = 
    { gainBreak = 0; isTaken = ref false; 
      deltaFromPrev = topalign.deltaToNext; (* Share with the previous *)
      deltaToNext = ref 0; (* Allocate a new ref *)} in
  aligns := res :: !aligns;
  res.deltaFromPrev := abscol - !topAlignAbsCol;
  topAlignAbsCol := abscol

let popAlign () = 
  match !aligns with
    top :: t when t != [] -> 
      aligns := t; 
      topAlignAbsCol := !topAlignAbsCol - !(top.deltaFromPrev)
  | _ -> failwith "Unmatched unalign\n"


(* Keep a list of ref cells for the breaks, in the same order that we see 
 * them in the document *)
let breaks: bool ref list ref = ref []

(* The maximum column that we should use *)
let maxCol = ref 0

(* Sometimes we take all the optional breaks *)
let breakAllMode = ref false

(* We are taking a newline and moving left *)
let newline () =
  let topalign = List.hd !aligns in (* aligns is never empty *)
  if debug then
    dbgprintf "Taking a newline: reseting gain of %d\n" topalign.gainBreak;
  topalign.gainBreak <- 0;        (* Erase the current break info *)
  if !breakAllMode && !topAlignAbsCol < !maxCol then 
    breakAllMode := false;
  !topAlignAbsCol                          (* This is the new column *)



(* Choose the align with the best gain. We outght to find a better way to 
 * keep the aligns sorted, especially since they gain never changes (when the 
 * align is the top align) *)
let chooseBestGain () : align option =        
  let bestGain = ref 0 in
  let rec loop (breakingAlign: align option) = function
      [] -> breakingAlign
    | a :: resta -> 
        if debug then dbgprintf "Looking at align with gain %d\n" a.gainBreak;
        if a.gainBreak > !bestGain then begin
          bestGain := a.gainBreak;
          loop (Some a) resta
        end else
          loop breakingAlign resta
  in
  loop None !aligns


(* Another one that chooses the break associated with the current align only *)
let chooseLastGain () : align option = 
  let topalign = List.hd !aligns in
  if topalign.gainBreak > 0 then Some topalign else None

(* We have just advanced to a new column. See if we must take a line break *)
let movingRight (abscol: int) : int = 
  (* Keep taking the best break until we get back to the left of maxCol or no 
   * more are left *)
  let rec tryAgain abscol = 
    if abscol <= !maxCol then abscol else 
    begin
      if debug then
        dbgprintf "Looking for a break to take in column %d\n" abscol;
      (* Find the best gain there is out there *)
      match if !fastMode then None else chooseBestGain () with 
        None -> begin
          (* No breaks are available. Take all breaks from now on *)
          breakAllMode := true;
          if debug then
            dbgprintf "Can't find any breaks\n";
          abscol
        end 
      | Some breakingAlign -> begin
          let topalign = List.hd !aligns in
          let theGain = breakingAlign.gainBreak in
          assert (theGain > 0);
          if debug then dbgprintf "Taking break at %d. gain=%d\n" abscol theGain;
          breakingAlign.isTaken := true;
          breakingAlign.gainBreak <- 0;
          if breakingAlign != topalign then begin
            breakingAlign.deltaToNext := 
               !(breakingAlign.deltaToNext) - theGain;
            topAlignAbsCol := !topAlignAbsCol - theGain
          end;
          tryAgain (abscol - theGain)
      end
    end
  in
  tryAgain abscol


(* Pass the current absolute column and compute the new column *)
let rec scan (abscol: int) (d: doc) : int = 
  match d with 
    Nil -> abscol
  | Concat (d1, d2) -> scan (scan abscol d1) d2
  | Text s -> 
      let sl = String.length s in 
      if debug then 
        dbgprintf "Done string: %s from %d to %d\n" s abscol (abscol + sl);
      movingRight (abscol + sl)
  | CText (d, s) -> 
      let abscol' = scan abscol d in
      let sl = String.length s in 
      if debug then 
        dbgprintf "Done string: %s from %d to %d\n" s abscol' (abscol' + sl);
      movingRight (abscol' + sl)

  | Align -> pushAlign abscol; abscol
  | Unalign -> popAlign (); abscol 
  | Line -> (* A forced line break *) newline ()
  | LeftFlush -> (* Keep cursor left-flushed *) 0

  | Break -> (* An optional line break. Always a space followed by an 
              * optional line break  *)
      let takenref = ref false in
      breaks := takenref :: !breaks;
      let topalign = List.hd !aligns in (* aligns is never empty *)
      if !breakAllMode then begin
        takenref := true;
        newline ()
      end else begin
        (* If there was a previous break there it stays not taken, forever. 
         * So we overwrite it. *)
        topalign.isTaken <- takenref;
        topalign.gainBreak <- 1 + abscol - !topAlignAbsCol;
        if debug then
          dbgprintf "Registering a break at %d with gain %d\n" 
            (1 + abscol) topalign.gainBreak;
        movingRight (1 + abscol)
      end
    
      
(* The actual function that takes a document and prints it *)
let emitDoc 
    (emitString: string -> int -> unit) (* emit a number of copies of a 
                                         * string *)
    (d: doc) = 
  let aligns: int list ref = ref [0] in (* A stack of alignment columns *)

  let wantIndent = ref false in
  (* Use this function to take a newline *)
  (* AB: modified it to flag wantIndent. The actual indentation is done only 
     if leftflush is not encountered *)
  let newline () =
    match !aligns with
      [] -> failwith "Ran out of aligns"
    | x :: _ ->
	emitString "\n" 1;
	wantIndent := true;
	x
  in
  (* Print indentation if wantIndent was previously flagged ; reset this flag *)
  let indentIfNeeded () =
    if !wantIndent then ignore (
      match !aligns with
	[] -> failwith "Ran out of aligns"
      | x :: _ -> 
          if x > 0 then emitString " "  x;
          x);
    wantIndent := false	  
  in
  (* A continuation passing style loop *)
  let rec loopCont (abscol: int) (d: doc) (cont: int -> unit) : unit 
      (* the new column *) =
    match d with
      Nil -> cont abscol
    | Concat (d1, d2) -> 
        loopCont abscol d1 (fun abscol' -> loopCont abscol' d2 cont)

    | Text s -> 
        let sl = String.length s in
	indentIfNeeded ();
        emitString s 1;
        cont (abscol + sl)

    | CText (d, s) -> 
        loopCont abscol d 
          (fun abscol' -> 
            let sl = String.length s in
	    indentIfNeeded ();
            emitString s 1; 
            cont (abscol' + sl))

    | Align -> 
        aligns := abscol :: !aligns;
        cont abscol

    | Unalign -> begin
        match !aligns with
          [] -> failwith "Unmatched unalign"
        | _ :: rest -> aligns := rest; cont abscol
    end
    | Line -> cont (newline ())
    | LeftFlush -> wantIndent := false;  cont (0)
    | Break -> begin
        match !breaks with
          [] -> failwith "Break without a takenref"
        | istaken :: rest -> 
            breaks := rest; (* Consume the break *)
            if !istaken then cont (newline ())
            else begin
	      indentIfNeeded ();
              emitString " " 1; 
              cont (abscol + 1)
            end
    end
  in

  (* A directy style loop *)
  let rec loop (abscol: int) (d: doc) : int (* the new column *) =
    match d with
      Nil -> abscol
    | Concat (d1, d2) -> loop (loop abscol d1) d2

    | Text s -> 
        let sl = String.length s in
	indentIfNeeded ();
        emitString s 1;
        abscol + sl

    | CText (d, s) -> 
        let abscol' = loop abscol d in
        let sl = String.length s in
	indentIfNeeded ();
        emitString s 1; 
        abscol' + sl

    | Align -> 
        aligns := abscol :: !aligns;
        abscol

    | Unalign -> begin
        match !aligns with
          [] -> failwith "Unmatched unalign"
        | _ :: rest -> aligns := rest; abscol
    end
    | Line -> newline ()
    | LeftFlush -> wantIndent := false; 0
    | Break -> begin
        match !breaks with
          [] -> failwith "Break without a takenref"
        | istaken :: rest -> 
            breaks := rest; (* Consume the break *)
            if !istaken then newline ()
            else begin
	      indentIfNeeded ();
              emitString " " 1; 
              abscol + 1
            end
    end
  in
  loopCont 0 d (fun x -> ()) 
(*  loop 0 d *)


(* Print a document on a channel *)
let fprint_george (chn: out_channel) ~(width: int) doc =
  maxCol := width;
(*let doc = flatten Nil doc in *)
  ignore (scan 0 doc);
  breaks := List.rev !breaks;
  ignore (emitDoc 
            (fun s nrcopies -> 
              for i = 1 to nrcopies do
                output_string chn s
              done) doc);
  breaks := [] (* We must do this especially if we don't do emit (which 
                * consumes breaks) because otherwise we waste memory *)

(* Print the document to a string *)
let sprint_george ~(width : int)  doc : string = 
  maxCol := width;
  ignore (scan 0 doc);
  breaks := List.rev !breaks;
  let buf = Buffer.create 1024 in
  let rec add_n_strings str num =
    if num <= 0 then ()
    else begin Buffer.add_string buf str; add_n_strings str (num - 1) end
  in
  emitDoc add_n_strings doc;
  breaks  := [];
  Buffer.contents buf

let a =3    

                                        (* The rest is based on printf.ml *)
external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"


(* Keep track of nested align in gprintf. Each gprintf format string must 
 * have properly nested align/unalign pairs. When the nesting depth surpasses 
 * !printDepth then we print ... and we skip until the matching unalign *)
let printDepth = ref 100 (* WRW: must see whole thing *)
let alignDepth = ref 0
    
let gprintf (finish : doc -> doc)  
    (format : ('a, unit, doc) format) : 'a =
  let format = (Obj.magic format : string) in

  (* Record the starting align depth *)
  let startAlignDepth = !alignDepth in
  (* Special concatenation functions *)
  let dconcat (acc: doc) (another: doc) = 
    if !alignDepth > !printDepth then acc else acc ++ another in
  let dctext1 (acc: doc) (str: string) = 
    if !alignDepth > !printDepth then acc else 
    CText(acc, str) in
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
                  CText(acc, "...")
                else
                  acc ++ align
              in
              incr alignDepth;
              collect newacc (i + 2)
                
          | ']' ->                        (* unalign *)
              decr alignDepth;
              let newacc = 
                if !alignDepth >= !printDepth then
                  acc
                else
                  acc ++ unalign
              in
              collect newacc (i + 2)
          | '!' ->                        (* hard-line break *)
              collect (dconcat acc (line)) (i + 2)
          | '?' ->                        (* soft line break *)
              collect (dconcat acc (break)) (i + 2)
	  | '<' ->                        (* left-flushed *)
	      collect (dconcat acc (leftflush)) (i + 2)
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
              collect (dconcat acc (line)) (i + 2)
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
  collect Nil 0

let withPrintDepth dp thunk = 
  let opd = !printDepth in
  printDepth := dp;
  thunk ();
  printDepth := opd





(****************************************************************************************)
(* qprint - The "aman" and "gap" algorithms *)
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


(* Options for Aman's algorithm *)
let qdontthink = !fastMode

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
let writeIndent (n : int) (spaceBuf : string) = function
    QOut_channel chn -> output chn spaceBuf 0 n (* AB: Must put a try here for safety *)
  | QBuffer buf -> Buffer.add_substring buf spaceBuf 0 n



(* Preprocess the doc, and calculate a list of numbers for corresponding to all the 
   breaks in the doc.
   These numbers refer to the size of the next unbreakable thing following the break.
   Text is always unbreakable, and whatever occurs between alignment marks is also
   treated as unbreakable here to avoid breaking in the middle of deeply nested aligns
*)
let qPreprocess doc width : int ref list =
  (* A stream-like interface to get the leaves of a doc tree in order *)
  let stack = ref [doc] in  
  let rec getNext  () : doc = match !stack with
    doc :: rest -> begin 
      stack := rest;
      match doc with
       	Concat (d1,d2) -> 
	  stack := d1 :: (d2 :: !stack);
	  getNext ()
      |	CText (d,s) ->
	  stack := d :: (Text s :: !stack);
	  getNext ()
      |	Nil -> getNext ()
      | Text _ | Line | LeftFlush | Break | Align | Unalign -> doc
    end
  | [] -> nil
  in
  let breakList : int ref list ref = ref [] in
  let updateBreakList = ref false in

  (******* The "aman" algo ******)
  let rec getSize (acc : int) : int  =
    match getNext () with
      Text s -> getSize (acc + String.length s)
    | Break -> 
	if (!updateBreakList) then breakList := (ref (-1)) :: !breakList 
	else updateBreakList := true; 
	getSize (acc + 1)
    | Align -> 
	let x = ref (-2) in
	if !updateBreakList then begin
	  updateBreakList := false;
	  breakList := x :: !breakList;
	end;
	   x := getSize 0;	
	getSize (acc + !x)
    | Unalign -> 
	if !updateBreakList then breakList := (ref (-1)) :: !breakList;
	updateBreakList := false; 
	acc
    | Line | LeftFlush -> 
	if (!updateBreakList) then breakList := (ref (-1)) :: !breakList;
	updateBreakList := false; 
	getSize 0
    | Nil -> acc
    | Concat _ | CText _ -> raise (Failure "docStream returned nonleaf")
  in 

  (******* The "gap" algo ******)
  let breaks    = ref [] in
  let dummybreak = ref (-9999) in
  let rec getGap 
      (acc : int) 
      (lastBreak : int ref)
      (lastBreakPos : int)
      : int =
    match getNext () with
      Text s -> getGap (acc + String.length s) lastBreak lastBreakPos
    | Break ->
	let newBrk = ref (-acc-1) in
	lastBreak := lastBreakPos + acc;
	breaks    := newBrk :: !breaks;
	getGap (succ acc) newBrk !newBrk
    | Line | LeftFlush->
	getGap 0 lastBreak lastBreakPos
    | Align ->
	let x = getGap 0 dummybreak !dummybreak in
	getGap (acc + x) lastBreak lastBreakPos
    | Unalign ->
	lastBreak := lastBreakPos + acc;
	acc
    | Nil -> 
	lastBreak := lastBreakPos + acc;
	acc   
    | Concat _ | CText _ -> raise (Failure "docStream returned nonleaf")
  in
  (* Choose the right one *)
  if algo == Gap then
    let _ = getGap 0 dummybreak !dummybreak in begin
      List.rev !breaks;
    end
  else
    let _ = getSize 0 in begin
      if !updateBreakList then breakList := (ref (-1)) :: !breakList;
      List.rev !breakList
    end

   
(* Use the preprocessed data to pretty-print the doc on a given qchannel within a given width. 
   If the qdontthink flag is true, no preprocessing is done
*)
let qprint qchn width doc = 
  let alignStack  = ref [0] in
  let breakList :  int ref list ref = if qdontthink then ref [] else ref (qPreprocess doc width) in
  let spaceBuf = String.make (1024+width) ' ' in
  let wantIndent = ref false in
  let debugSoftBreaks = false in (* Print the soft breaks as ANSI color characters *)
  let debugSoftBreakChar (pos : int) : char =
    char_of_int (33 + (pos mod 25)) in
  let decide2break curPos = begin
    if debugSoftBreaks then begin
      writeString "\027[1;33m" qchn;
      writeChar (debugSoftBreakChar curPos) qchn;
      writeString "\027[0;0m" qchn;
    end;
    match !breakList with
      size :: rest -> 
	breakList := rest;
	if debugSoftBreaks then dbgprintf "%c c=%d s=%d\n" (debugSoftBreakChar curPos) curPos !size;
	(curPos >= width) || 
	((!size > 0) && (width < curPos + !size ))
    | [] -> raise (Failure "breakList contains too few breaks")
  end  in
  let qprintText curPos (s : string) =
    writeString s qchn;
    curPos + String.length s in
  let breakLine () = begin
    writeChar '\n' qchn;
    wantIndent := true;
    List.hd !alignStack 
  end in
  let indentIfNeeded () =
    if !wantIndent then  writeIndent (List.hd !alignStack) spaceBuf qchn;
    wantIndent := false;
  in
  let rec qprintLoop curPos = function
      Nil -> curPos
    | Text s -> 
	indentIfNeeded ();
	writeString s qchn;
	curPos + String.length s
    | Concat (d1, d2) -> qprintLoop (qprintLoop curPos d1) d2
    | CText (d,s) -> 
	let m = qprintLoop curPos d in
	indentIfNeeded ();
	qprintText m s
    | Break -> 
	indentIfNeeded ();
	if qdontthink then
	  if (curPos + 10 >= width)  then  (breakLine())
	  else begin 
	    writeChar ' ' qchn; 
	    curPos + 1 
	  end
	else
	  if (decide2break curPos) then (breakLine())
	  else begin
	    if not debugSoftBreaks then writeChar ' ' qchn;
	    curPos + 1	    
	  end	  
    | Line -> 
	breakLine ();
    | LeftFlush ->
	wantIndent := false;
	curPos
    | Align -> 
	alignStack := curPos :: !alignStack;
	curPos
    | Unalign -> 
	alignStack := List.tl !alignStack;
	curPos
  in 
  ignore (qprintLoop 0 doc)
  


let qprintf format doc =
  qprint (QOut_channel stdout) 80 doc

let qfprint chn ~width doc =
  qprint (QOut_channel chn) width doc

let qsprint ~width doc =
  let buf = Buffer.create 1024 in
  qprint (QBuffer buf) width doc;
  Buffer.contents buf




(******************************************************************************)
(* Decude which printer to use *)

let fprint = match algo with
  George     -> fprint_george
| Aman | Gap -> qfprint

let sprint = match algo with
  George     -> sprint_george
| Aman | Gap -> qsprint


let flushOften = ref false

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


(*
let sprintf format = gprintf (fun x -> sprint 80 x) format
let printf  format = gprintf (fun x -> fprint stdout 80 x) format
*)



(******************************************************************************)
let getAlgoName = function
    George -> "George"
  | Aman   -> "Aman"
  | Gap    -> "Gap"

let getAboutString () : string =
  "(Pretty: ALGO=" ^ (getAlgoName algo) ^ ")"

