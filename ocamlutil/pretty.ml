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

(* Pretty printer *)

let debug = false
let aman           = false
let mode           = try Sys.getenv "Mode" with Not_found -> "" (* I hope ocamlopt realizes that this is dead if aman = false *)
let aman_no_layout = (aman && (mode = "SkipLayout"))            (* Constant-foldable if aman = false? *)
let aman_no_output = (aman && (mode = "SkipOutput"))
let aman_no_fit    = (aman && (mode = "SkipFit"))

let george = true


let noBreaks = ref false  (* Replace all soft breaks with space *)
let noAligns = ref false (* Whether to obey align/unalign *)

let fprintf x = Printf.fprintf stderr x

type doc = 
    Nil
  | Text     of string
  | Spaces   of int        (* A number of spaces *)
  | Concat   of doc * doc
  | CText    of doc * string
  | Line     
  | Break    
  | Align
  | Unalign


let nil  = Nil

let (++) d1 d2 = Concat (d1, d2)
let text s     = Text s
let num  i     = text (string_of_int i)
let chr  c     = text (String.make 1 c) 
let align      = Align
let unalign    = Unalign
let line       = Line
let break      = Break  (* Line *) (* Aman's benchmarking goo *)


let nest n d   = Spaces n ++ align ++ d ++ unalign

let  seq sep f dl = 
  let rec loop = function
      []     -> nil
    | h :: t -> 
        let fh = f h in  (* Make sure this is done first *)
        sep ++ fh ++ loop t
  in
  (match dl with
    [] -> nil
  | h :: t -> 
      let fh = f h in fh ++ loop t)

let docArray f () a = 
  let len = Array.length a in
  if len = 0 then 
    nil
  else
    let rec loop i =
      if i >= len then nil else
      let fi = f i a.(i) in (* Make sure this is done first *)
      fi ++ loop (i + 1)
    in
    let f0 = f 0 a.(0) in
    f0  ++ loop 1
      
let docOpt delem () = function
    None -> text "None"
  | Some e -> text "Some(" ++ (delem () e) ++ chr ')'
  
let docList sep f () dl = seq sep f dl

let insert () d = d


(************* LAYOUT functions ********************)

type status = 
    Open                                (* We have not yet decided whether to 
                                         * take this break or not. Both the 
                                         * col field and   *)
  | Taken                               (* We are definitely taking this one *)
  | NotTaken                            (* We are definitely not talking this 
                                         * one *)
type brk = {         id     : int;
             mutable col    : int;
                   align    : int ref;
             mutable status : status; } 

type breakData = 
    Brk  of brk
  | Algn of int ref


let printData breaks = 
  let lastCol = ref 99999 in
  let printOne = function
      Brk b -> 
        if b.status = Open then begin
          fprintf  "B%d(%d,%d), " b.id b.col (! (b.align));
            lastCol := b.col
        end else
          ()
  | Algn ar -> 
        if !ar <= !lastCol then begin
          fprintf "A(%d), " (!ar);
          lastCol := !ar
        end else
          ()
  in
  List.iter printOne breaks



let fit (start: 'a) 
        (accString: 'a -> string -> int -> 'a) (* accumulate a number of 
                                                * copies of a string *)
        (width: int) = 
  let width = if width = 0 then 99999 else width in
                                        (* A flag to say that we shouldn't try 
                                         * to perform the expensive line 
                                         * breaking for a while *)
  let breakAllMode = ref false in
  let breakId = ref 0  in

                                        (* Next mark all the still Open 
                                         * breaks as not taken up to the 
                                         * align that we are taking 
                                         * (starting at the most recent 
                                         * break)  *)
  let rec markNotTaken align = function
      Brk ({status=Open;} as b) :: rest ->
        b.status <- NotTaken;
        markNotTaken align rest
    | Algn ar :: rest -> 
        if align == ar then () else markNotTaken align rest
    | _ :: rest -> markNotTaken align rest
    | [] -> ()  in

                                        (* Function to perform some breaks. 
                                         * Takes a stack of breaks in scope, 
                                         * the width and the current column. 
                                         * Marks some breaks and returns the 
                                         * new column *)
  let performBreaks breaks col =
    let oneBrk col = 
      let _ = 
        if debug then begin
          fprintf "Before oneBrk (col = %d): " col;
          printData breaks;
          fprintf "\n"
        end else 
          () in
      let maxDelta = ref 0 in
      let maxCol   = ref 0 in
      let maxBrk   : brk ref = ref {id=0;col=0;align=ref 0;status=Open;} in
      let rec findMaxDelta = function
          [] -> ()
        | Brk b :: rest when b.status = Open ->
            if !maxDelta < b.col - ! (b.align) then begin
              maxDelta := b.col - ! (b.align);
              maxCol   := b.col;      (* Create a private copy *)
              maxBrk   := b;
            end else
              ();
            findMaxDelta rest
        | _ :: rest -> findMaxDelta rest
      in
      let _ = findMaxDelta breaks in
      let _ = 
        if !maxDelta > 0 then 
                                        (* Adjust the positions of following 
                                         * breaks and aligns  *)
          let delta = !maxDelta in
          let align = !maxBrk.align in
                                        (* Now we can adjust all the column 
                                         * of all the Open breaks and of all 
                                         * the aligns until we hit the break 
                                         * we are taking. This is because the 
                                         * breaks are in reversed order *)
          let rec adjustColumns = function 
              Algn ar :: rest -> 
                ar := !ar - delta; 
                assert (!ar >= 0);
                adjustColumns rest
            | Brk ({status=Open;} as b) :: rest -> 
                if b == !maxBrk then begin
                  b.status <- Taken;
                  b.col <- b.col - delta;
                  markNotTaken align rest
                end else begin
                  b.col <- b.col - delta;  
                  adjustColumns rest
                end
            | _ :: rest -> adjustColumns rest
            | _ -> failwith "Pretty: cannot find the break"
          in
          if debug then fprintf "Adjusting for break at %d\n" (!maxCol) else();
          adjustColumns breaks
        else
          ()
      in
      begin 
        if debug then begin
          fprintf "  B=%d, D=%d: " (!maxCol) (!maxDelta);
          printData breaks;
          fprintf "\n"
        end else
          ();
        !maxDelta
      end
          (* oneBrk *)        
    in
                                        (* Iterate oneBrk while progress can 
                                         * be made, until necessary *)
    let rec loop col = 
      if col > width then
        let delta = oneBrk col in
        if delta > 0 then          (* If some progress was made *)
          begin
            if debug && col - delta > width then
              begin fprintf "  Retrying oneBrk\n"; flush stderr end
            else
              ();
            loop (col - delta)
          end
        else                            (* Cannot improve *)
          col
      else   
        col
    in
    loop col
    (* performBreaks *)
  in

                                        (* Collect here all the breaks in the 
                                         * order in which they are 
                                         * encountered *)
  let allBreaks : brk list ref = ref [] in


     (* Scan the document and initialize the allBreaks data structure. Carry 
      * a stack of aligns and a stack of breakData that are in scope for the 
      * current line. Also carry the current column and a continuation. *)
  let rec scan aligns breaks col cont = function 
      Nil -> cont aligns breaks col
    | Concat(d1,d2) -> 
        scan aligns breaks col (fun a b c -> scan a b c cont d2) d1
    | Spaces n -> cont aligns breaks (col + n)
    | Text s -> 
        let tl = String.length s in
        cont aligns breaks (col + tl)
    | CText (d, s) -> 
        let tl = String.length s in
        scan aligns breaks col 
          (fun a b c -> cont a b (c + tl)) d
    | Break when !noBreaks -> 
        (* Pretend we have seen a space *)
        cont aligns breaks (col + 1)

    | Line when !noBreaks ->
        let ar =                        (* Find the matching align. There 
                                         * must always be one because we 
                                         * start with an align in the column 
                                         * 0  *)
          match aligns with
            ar :: _ -> ar
          | _ -> failwith "Bug. Missing align"
        in
        cont aligns breaks !ar

    | (Break | Line) as doc ->            
        let ar =                        (* Find the matching align. There 
                                         * must always be one because we 
                                         * start with an align in the column 
                                         * 0  *)
          match aligns with
            ar :: _ -> ar
          | _ -> failwith "Bug. Missing align"
        in
                                        (* See if it's time to switch 
                                         * breakAllMode off *)
        let _ = 
          if !breakAllMode && col < width - 20 then
            breakAllMode := false else () in
          
                                        (* See if we need to perform some 
                                         * breaks on what we have so far  *)
        let newCol = 
          if !breakAllMode then col else
          performBreaks breaks col  in
                                        (* Maybe we need to switch breakAll 
                                         * mode on *)
        let _ = if newCol > width then breakAllMode := true else () in

                                        (* Create a new breakData *)
        let bid    = incr breakId; !breakId in
        let bdata  = 
          if (match doc with Break -> true | _ -> false) 
              && not !breakAllMode then
            {id=bid;col=newCol + 1;align=ar;status=Open}
          else
            begin
              markNotTaken ar breaks;
              {id=bid;col= !ar      ;align=ar;status=Taken}
            end
        in
                                        (* Memorize it *)
        let _ = allBreaks := bdata :: (!allBreaks) in
        cont aligns (Brk bdata :: breaks) bdata.col

    | Align -> 
        if !noAligns then 
          cont aligns breaks col
        else
          let adata = ref col in
          cont (adata :: aligns) (Algn adata :: breaks) col


    | Unalign -> 
        if !noAligns then 
          cont aligns breaks col
        else
          match aligns with
            _ :: t when t != [] -> cont t breaks col
          | _ -> failwith "Unmatched unalign\n"

  (* scan *)
  in
  let indentLen = ref 0 in
  let doString (acc: 'a) (s: string) (nrcopies: int) : 'a = 
    let ind = !indentLen in
    let acc' = 
      if ind > 0 then begin
        indentLen := 0; 
        accString acc " " ind
    end else 
        acc 
    in
    accString acc' s nrcopies
  in
  let rec layout acc = function
      Nil -> acc
    | Align -> acc
    | Unalign -> acc
    | Spaces n -> doString acc " " n
    | Text s -> doString acc s 1
    | Concat (d1, d2) -> layout (layout acc d1) d2
    | CText (d, s) -> doString (layout acc d) s 1
    | Break when !noBreaks -> 
        (* Pretend we have a space *)
        doString acc " " 1
    | Line when !noBreaks -> 
        accString acc "\n" 1
        
    | Break | Line -> 
        let b = 
          match !allBreaks with
            b :: t -> allBreaks := t; b
          | [] -> failwith "Pretty: allbreaks list is too short"
        in
        if b.status = Taken then begin
          let _ = if debug then 
            fprintf "taking branch %d to %d\n" b.id (!(b.align)) else () in
          indentLen := !(b.align);
          accString acc "\n" 1
        end else
          doString acc " " 1
  in
  function doc -> 
    let scanCont _ _ _ =  
      allBreaks := List.rev (!allBreaks); 
      layout start doc
    in
    if !noBreaks && !noAligns then begin
      if aman then fprintf "************ SKIPPING SCAN\n" ;
      layout start doc
    end else
      if aman_no_layout then begin (* Aman's benchmarking goo *)
	if aman then fprintf "************ SCAN WITH DUMMY CONTINUATION\n" ;
        scan [ref 0] [] 0 (fun _ _ _ -> start) doc
      end else begin
	if aman then fprintf "************ FULL LAYOUT + SCAN\n" ;
        scan [ref 0] [] 0 scanCont doc
      end
	  

(***** A new fit function ****)


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
      mutable deltaFromPrev: int (* The column of this alignment mark - the 
                                  * column of the previous mark *)
    } 
      
let mkAlign (deltacol: int) = 
  { gainBreak = 0; isTaken = ref false; deltaFromPrev = deltacol; }

let topAlignAbsCol = ref 0 (* The absolute column of the top alignment *)

(* We use references to avoid the need to pass data around all the time *)
let aligns: align list ref = 
  ref [mkAlign 0] (* The current stack of active alignment marks, with the 
                    * top at the head. Never empty. *)

(* Keep a list of ref cells for the breaks, in the same order that we see 
 * them in the document *)
let breaks: bool ref list ref = ref []

(* The maximum column that we should use *)
let maxCol = ref 0

(* Sometimes we take all the optional breaks *)
let breakAllMode = ref false

(* We are taking a newline and moving left *)
let newline (abscol: int) =
  let topalign = List.hd !aligns in (* aligns is never empty *)
  topalign.gainBreak <- 0;        (* Erase the current break info *)
  if !breakAllMode && !topAlignAbsCol < !maxCol then 
    breakAllMode := false;
  !topAlignAbsCol                          (* This is the new column *)


(* We have just advanced to a new column. See if we must take a line break *)
let movingRight (abscol: int) : int = 
  (* Keep taking the best break until we get back to the left of maxCol or no 
   * more are left *)
  let rec tryAgain abscol = 
    if abscol < !maxCol then abscol else 
    begin
      (* Find the best gain there is out there *)
      let bestGain = ref 0 in
      let breakingAlign : align option ref = ref None (* the align with the 
          * best gain *)
      in
      let breakingAlignNext: align option ref = ref None (* The align that is 
                                                          * on top of the 
                                                          * best one in the 
                                                          * stack  *)
      in
      let rec loop (next: align option) = function
          [] -> ()
        | a :: resta -> 
            if a.gainBreak > !bestGain then begin
              bestGain := a.gainBreak;
              breakingAlign := Some a;
              breakingAlignNext := next
            end;
            loop (Some a) resta
      in
      loop None !aligns;
      
      if !bestGain = 0 then begin
        (* No breaks are available. Take all breaks from now on *)
        breakAllMode := true;
        abscol
      end else begin
        let breakingAlign = 
          match !breakingAlign with
            Some x -> x 
          | None -> failwith "Have a gain but can't find the align" in
        breakingAlign.isTaken := true;
        breakingAlign.gainBreak <- 0;
        if debug then 
          fprintf "Taking break at %d. gain=%d\n" abscol !bestGain;
        (* Now adjust the align that follows the one taken, and maybe also the 
        * topAlignAbsCol *)
        match !breakingAlignNext with
          None -> (* We are taking the break of the top align *) 
            tryAgain (abscol - !bestGain)
        | Some next -> begin
            next.deltaFromPrev <- next.deltaFromPrev - !bestGain;
            (* We must have moved the top align left also *)
            topAlignAbsCol := !topAlignAbsCol - !bestGain;
            tryAgain (abscol - !bestGain)
        end
      end
    end
  in
  tryAgain abscol


(* Pass the current absolute column and compute the new column *)
let rec scan (abscol: int) (d: doc) : int = 
  match d with 
    Nil -> abscol
  | Concat (d1, d2) -> scan (scan abscol d1) d2
  | Spaces n -> abscol + n
  | Text s -> 
      let sl = String.length s in 
      movingRight (abscol + sl)
  | CText (d, s) -> 
      let sl = String.length s in 
      movingRight ((scan abscol d) + sl)
  | Align -> 
      let topAlign = mkAlign (abscol - !topAlignAbsCol) in
      aligns :=  topAlign :: !aligns;
      topAlignAbsCol := abscol;
      abscol
  | Unalign -> begin
      match !aligns with
        top :: t when t != [] -> 
          aligns := t; 
          topAlignAbsCol := !topAlignAbsCol - top.deltaFromPrev;
          abscol

      | _ -> failwith "Unmatched unalign\n"
  end
  | Line -> (* A forced line break *) newline abscol


  | Break -> (* An optional line break. Always a space followed by an 
              * optional line break  *)
      let takenref = ref false in
      breaks := takenref :: !breaks;
      let topalign = List.hd !aligns in (* aligns is never empty *)
      if !breakAllMode then begin
        takenref := true;
        newline abscol 
      end else begin
        (* If there was a previous break there it stay not taken, forever *)
        topalign.isTaken <- takenref;
        topalign.gainBreak <- 1 + abscol - !topAlignAbsCol;
        movingRight (1 + abscol)
      end
    
      
(* The actual function that takes a document and prints it *)
let emitDoc 
    (emitString: string -> int -> unit) (* emit a number of copies of a 
                                         * string *)
    (d: doc) = 
  let aligns: int list ref = ref [0] in (* A stack of alignment columns *)

  (* Use this function to take a newline *)
  let newline () = 
    match !aligns with
      [] -> failwith "Ran out of aligns"
    | x :: _ -> 
        emitString "\n" 1;
        emitString " "  x;
        x
  in
    
  let rec loop (abscol: int) (d: doc) : int (* the new column *) =
    match d with
      Nil -> abscol
    | Concat (d1, d2) -> loop (loop abscol d1) d2
    | Spaces n -> 
        emitString " " n;
        abscol + n

    | Text s -> 
        let sl = String.length s in
        emitString s 1;
        abscol + sl

    | CText (d, s) -> 
        let sl = String.length s in
        let newcol = loop abscol d in
        emitString s 1;
        newcol + sl

    | Align -> 
        aligns := abscol :: !aligns;
        abscol

    | Unalign -> begin
        match !aligns with
          [] -> failwith "Unmatched unalign"
        | _ :: rest -> aligns := rest; abscol
    end
    | Line -> newline ()
    | Break -> begin
        match !breaks with
          [] -> failwith "Break without a takenref"
        | istaken :: rest -> 
            breaks := rest; (* Consume the break *)
            if !istaken then newline ()
            else begin
              emitString " " 1; 
              abscol + 1
            end
    end
  in
  loop 0 d

let flushOften = ref false

(* Aman's benchmarking goo *)
let fprint = 
  if aman_no_output then 
    fun chn width doc -> fit () (fun _ _ _ -> ()) width doc
  else if aman_no_fit then
    fun chn width doc -> ()
  else if george then 
    fun chn width doc -> begin
      maxCol := width;
      ignore (scan 0 doc);
      breaks := List.rev !breaks;
      ignore (emitDoc 
                (fun s nrcopies -> 
                  for i = 1 to nrcopies do
                    output_string chn s
                  done) doc)
    end
  else
    fun chn width doc ->
      fit () 
        (fun _ s nrcopies -> 
          for i = 1 to nrcopies do
            output_string chn s
          done;
          if !flushOften then flush chn) 
        width doc

let sprint width doc =
  fit "" 
    (fun acc s nrcopies -> 
      let acc' = 
        if String.length s = 1 then
          acc ^ String.make nrcopies (String.get s 0)
        else
          let rec loop acc left = 
            if left = 0 then acc
            else loop (acc ^ s) (left - 1)
          in
          loop acc nrcopies
      in
      acc')
    width doc

let gprint = fit

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
    if i >= flen then Obj.magic (dfinish acc) else begin
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
              collect (dconcat acc line) (i + 2)
          | '?' ->                        (* soft line break *)
              collect (dconcat acc break) (i + 2)
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
              collect (dconcat acc line) (i + 2)
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

let withPrintDepth dp thunk = 
  let opd = !printDepth in
  printDepth := dp;
  thunk ();
  printDepth := opd

