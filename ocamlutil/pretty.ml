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

let debug =  false
let aman           = false
let mode           = try Sys.getenv "Mode" with Not_found -> "" (* I hope ocamlopt realizes that this is dead if aman = false *)
let aman_no_layout = (aman && (mode = "SkipLayout"))            (* Constant-foldable if aman = false? *)
let aman_no_output = (aman && (mode = "SkipOutput"))
let aman_no_fit    = (aman && (mode = "SkipFit"))

(* use getenv("ALGO") to decide which algorithm to use
   ALGO=old    : old pretty printer (not george)
   ALGO=george
   ALGO=aman   (default)
*)
let envAlgo = let s = (try Sys.getenv ("ALGO") with Not_found -> "aman") in if (s <> "george") && (s <> "old") then "aman" else s

let use_old_version = (envAlgo = "old") || (envAlgo = "george")
let use_Qversion = (not use_old_version) && (envAlgo = "aman")
let george = true && (envAlgo = "george")

let _ = if debug then Printf.fprintf stderr "********** ALGO = %s *************\n" (if use_Qversion then "Aman" else if george then "George" else "Old")

let george_no_scan = george && false
let george_no_emit = george && (george_no_scan || false)
let george_no_out  = george && (george_no_scan || george_no_emit || false)

(* use MARSHALWRITE=filename to marshal the document to the specified file *)
let marshalFilename = (try Sys.getenv ("MARSHALWRITE") with Not_found -> "")



let noBreaks = ref false  (* Replace all soft breaks with space *)
let noAligns = ref false (* Whether to obey align/unalign *)

let fprintf x = Printf.fprintf stderr x



type status = 
    Open                                (* We have not yet decided whether to 
                                         * take this break or not. Both the 
                                         * col field and   *)
  | Taken                               (* We are definitely taking this one *)
  | NotTaken                            (* We are definitely not talking this 
                                         * one *)

(* We need a parametric type because of the mutual recursion in the definitions of brk and doc *)
type 'd dbrk = {      id    : int;
             mutable col    : int;
                   align    : int ref;
             mutable status : status; 
	            (* pdoc    : 'd; *)
	   } 

type 'd dqbrkData = {
    mutable qstatus : status;
    mutable nextAlign : 'd;
  } 

type qalignData = {
    alignId : int;
    mutable size : int;
  } 

type qunalignData = unit

type doc = 
    Nil
  | Text     of string
(*  | Spaces   of int *)
  | Concat   of doc * doc
  | CText    of doc * string
  | Break
  | Line 
  | Align
  | Unalign  


type qbrkData = doc dqbrkData

type brk = doc dbrk

type breakData = 
    Brk  of brk
  | Algn of int ref


let nil  = Nil

(* Note that all infix operators in Ocaml are left-associative. This means 
 * that if you have a long list of ++ then the whole thing is very unbalanced 
 * towards the left side. This is the worst possible case since scanning the 
 * left side of a Concat is the non-tail recursive case. *)

let (++) d1 d2 = Concat (d1, d2)

let text s     = Text s
let num  i     = text (string_of_int i)
let chr  c     = text (String.make 1 c) 
(* let align      = let count = ref 0 in function () -> count := !count +1 ; Align {alignId = !count;size=0}*)
let align      = Align
let unalign    = Unalign
let line       = Line
let break      = Break  (* Line *) (* Aman's benchmarking goo *)



(* Some debugging stuff *)

let nest n d   = text (String.make 1 ' ') ++ align ++ d ++ unalign



let dbgPrintStatus = function
    Open -> fprintf "Open"
  | Taken -> fprintf "Taken"
  | NotTaken -> fprintf "NotTaken"

let rec dbgPrintBrk (b : brk) = begin
  fprintf "(Brk: id=%d col=%d !align=%d status=" b.id b.col !(b.align);
  dbgPrintStatus b.status;
  fprintf ")\n"      
end    

let rec dbgPrintDoc (* recurseBreaks : bool *) = function
    Nil -> fprintf "(Nil)"
  | Text s -> fprintf "(Text %s)" s
(*  | Spaces n -> fprintf "(Spaces %d)" n *)
  | Concat (d1,d2) -> fprintf ""; dbgPrintDoc  d1; fprintf " ++\n "; 
      dbgPrintDoc  d2; fprintf ""
  | CText (d,s) -> dbgPrintDoc  d; fprintf " ++ \"%s\"" s; 
  | Break -> 
      fprintf "(Break)" (* (Obj.magic d : int); dbgPrintStatus qb.qstatus; *)
      (* fprintf " nextAlign=%d)" (match qb.nextAlign with Align a -> a.alignId | Nil -> -1 | _ -> 0) *)
  | Line -> fprintf "(Line)"
  | Align -> fprintf "(Align)"
  | Unalign -> fprintf "(Unalign)"
(*
  | Break b when recurseBreaks -> fprintf "(Break "; dbgPrintBrk !b; fprintf ")"
  | Break b when not recurseBreaks -> fprintf "(Break %d)" (!b).id
  | Line b when recurseBreaks -> fprintf "(Line "; dbgPrintBrk !b; fprintf ")"
  | Line b when not recurseBreaks -> fprintf "(Line %d)" (!b).id
  | Align _ -> fprintf "Align"
  | Unalign _ -> fprintf "Unalign"
  | _ -> fprintf "Whats this??"
*)
	

(* let nest n d   = (if n > 0 then Spaces n else nil) ++ align ++ d ++ unalign*)
let nest n d   = text (String.make 1 ' ') ++ align ++ d ++ unalign

(* Rewrite seq to be tail recursive *)

let  seq sep f dl = 
  let rec loop (acc: doc) = function
      []     -> acc
    | h :: t -> 
        let fh = f h in  (* Make sure this is done first *)
        loop (acc ++ sep ++ fh) t
  in
  (match dl with
    [] -> nil
  | h :: t -> 
      let fh = f h in loop fh t)

let docArray f () a = 
  let len = Array.length a in
  if len = 0 then 
    nil
  else
    (* make it tail-recursive *)
    let rec loop (acc: doc) i =
      if i >= len then acc else
      let fi = f i a.(i) in (* Make sure this is done first *)
      loop (acc ++ fi) (i + 1)
    in
    let f0 = f 0 a.(0) in
    loop f0 1
      
let docOpt delem () = function
    None -> text "None"
  | Some e -> text "Some(" ++ (delem () e) ++ chr ')'
  
let docList sep f () dl = seq sep f dl

let insert () d = d


(************* LAYOUT functions ********************)

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
  let rec markNotTaken (align : int ref)  = function
      Brk ({status=Open;} as b) :: rest -> begin
        b.status <- NotTaken;
        markNotTaken align rest
      end
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
      let maxBrk   : brk ref = ref {id=0;col=0;align=ref 0;status=Open} in
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
  let rec layout (acc : 'a) : doc -> 'a = function
      Nil -> acc
    | Align -> acc
    | Unalign -> acc
    | Text s -> doString acc s 1
    | Concat (d1, d2) -> layout (layout acc d1) d2
    | CText (d, s) -> doString (layout acc d) s 1
    | Break when !noBreaks -> 
        (* Pretend we have a space *)
        doString acc " " 1
    | Line when !noBreaks -> 
        accString acc "\n" 1
        
    | (Break  | Line) as brkDoc -> 
        let b =  
          (match !allBreaks with
            b :: t -> begin 
	      (*
	      (if debug then fprintf "(%d = %d)"  b.id (!brkRef).id); 
	      (if !brkRef == b || true then allBreaks := t else fprintf "*SKIPPED*");
	      (if debug && !brkRef == dummyBrk then fprintf "*DUMMY*\n");
	      *)
	      allBreaks := t;
	      (* !brkRef; *) b (* Aman *) 
	    end	      
          | [] -> failwith "Pretty: allbreaks list is too short";)
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
      (if debug then dbgPrintDoc doc);
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
    fprintf "Taking a newline: reseting gain of %d\n" topalign.gainBreak;
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
        if debug then fprintf "Looking at align with gain %d\n" a.gainBreak;
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
        fprintf "Looking for a break to take in column %d\n" abscol;
      (* Find the best gain there is out there *)
      match chooseBestGain () with 
        None -> begin
          (* No breaks are available. Take all breaks from now on *)
          breakAllMode := true;
          if debug then
            fprintf "Can't find any breaks\n";
          abscol
        end 
      | Some breakingAlign -> begin
          let topalign = List.hd !aligns in
          let theGain = breakingAlign.gainBreak in
          assert (theGain > 0);
          if debug then fprintf "Taking break at %d. gain=%d\n" abscol theGain;
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
        fprintf "Done string: %s from %d to %d\n" s abscol (abscol + sl);
      movingRight (abscol + sl)
  | CText (d, s) -> 
      let abscol' = scan abscol d in
      let sl = String.length s in 
      if debug then 
        fprintf "Done string: %s from %d to %d\n" s abscol' (abscol' + sl);
      movingRight (abscol' + sl)

  | Align -> pushAlign abscol; abscol
  | Unalign -> popAlign (); abscol 
  | Line -> (* A forced line break *) newline ()
 

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
          fprintf "Registering a break at %d with gain %d\n" 
            (1 + abscol) topalign.gainBreak;
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
        if x > 0 then emitString " "  x;
        x
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
        emitString s 1;
        cont (abscol + sl)

    | CText (d, s) -> 
        loopCont abscol d 
          (fun abscol' -> 
            let sl = String.length s in
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
    | Break -> begin
        match !breaks with
          [] -> failwith "Break without a takenref"
        | istaken :: rest -> 
            breaks := rest; (* Consume the break *)
            if !istaken then cont (newline ())
            else begin
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
        emitString s 1;
        abscol + sl

    | CText (d, s) -> 
        let abscol' = loop abscol d in
        let sl = String.length s in
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
(*  loopCont 0 d (fun x -> ()) *)
  loop 0 d

let flushOften = ref false

(* Aman's benchmarking goo *)
let fprint_old = 
  if aman_no_output then 
    fun chn width doc -> fit () (fun _ _ _ -> ()) width doc
  else if aman_no_fit then
    fun chn width doc -> ()
  else if george then 
    fun chn width doc -> begin
      maxCol := width;
      if george_no_scan then () 
      else
        begin
(*         let doc = flatten Nil doc in *)
          ignore (scan 0 doc);
          if george_no_emit then ()
          else begin
(*            Printf.fprintf stderr "Right before rev\n"; flush stderr; *)
            breaks := List.rev !breaks;
(*            Printf.fprintf stderr "Right after rev\n"; flush stderr; *)
            ignore (emitDoc 
                      (fun s nrcopies -> 
                        if george_no_out then () else
                        for i = 1 to nrcopies do
                          output_string chn s
                        done) doc)
          end
        end;
      breaks := []; (* We must do this especially if we don't do emit (which 
                     * consumes breaks) because otherwise we waste memory *)
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

let sprint_old width doc =
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


let flushOften = ref false




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
  let stack = ref [doc] in
  
  let rec getNext  () = match !stack with
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
      | Text _ | Line | Break | Align | Unalign -> doc
    end
  | _ -> nil
  (*
     let rec getNext  () = 
     let rec processStackElement doc = 
     match doc with
     Nil -> getNext ()
     |	Text _ | Line | Break | Align | Unalign -> doc
     |	Concat (d1,d2) ->
     stack := d2 :: !stack;
     processStackElement d1
     |	CText (d,s) ->
     stack := Text s :: !stack;
     processStackElement d
     in
     match !stack with
     doc :: rest -> begin 
     stack := rest;
     processStackElement doc
     end
     | _ -> nil
  *)
  in
  let docstr = getNext in
  let breakList : int ref list ref = ref [] in
  let updateBreakList = ref false in
  let rec getSize (acc : int) : int  =
    match docstr () with
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
	(*if !updateBreakList then breakList := (ref (-1)) :: !breakList;
	updateBreakList := false; *)
	acc
    | Line -> 
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
	(curPos > width) || 
	if (!size <= 0) then false
	else (width <= curPos + !size)
    | _ -> raise (Failure "breakList contains too few breaks")
  end  in
  let rec qprintLoop curPos = function
      Nil -> curPos
    | Text s -> 
	writeString s qchn;
	curPos + String.length s
    | Concat (d1, d2) -> qprintLoop (qprintLoop curPos d1) d2
    | CText (d,s) -> qprintLoop (qprintLoop curPos d) (Text s)
    | Break -> 
	(* if (!curPos + 10 >= width) then begin *)
	if (decide2break curPos) then begin
	  breakLine()
	end
	else begin
	  writeChar ' ' qchn;
	  curPos + 1	    
	end	  
    | Line -> 
	breakLine ()
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

let qfprint chn width doc =
  qprint (QOut_channel chn) width doc

let qsprint width doc =
  let buf = Buffer.create 1024 in
  qprint (QBuffer buf) width doc;
  Buffer.contents buf




let fprint_no_marshal = if use_Qversion then qfprint else fprint_old
let sprint_no_marshal = if use_Qversion then qsprint else sprint_old

let marshal_chn = if (marshalFilename = "") then stdout else begin
  fprintf "Marshaling doc to file (append/create): %s\n" marshalFilename;  
  (* AB: Why the @#$#@$ doesn't this work ? *)
  (*
  (open_out_gen 
     [Open_append; Open_creat] 
     666 marshalFilename);
  *)
  open_out_bin marshalFilename 
end


let fprint = if (marshalFilename = "") 
then fprint_no_marshal 
else begin
  fun chn width doc ->
    let mchn = marshal_chn  in
    Marshal.to_channel mchn doc [Marshal.No_sharing];
    flush mchn;
    fprint_no_marshal chn width doc
end

let sprint = if (marshalFilename = "") 
then sprint_no_marshal 
else begin
  fun width doc ->
    let mchn = marshal_chn  in
    Marshal.to_channel mchn doc [Marshal.No_sharing];
    flush mchn;
    sprint_no_marshal width doc
end



(*
let fprint = qfprint
let sprint = qsprint
*)

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


