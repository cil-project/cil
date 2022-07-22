(* See copyright notice at the end of the file *)
open GoblintCil
open Feature

(*****************************************************************************
   A transformation to make every function call end its statement. So
   { x=1; Foo(); y=1; }
   becomes at least:
   { { x=1; Foo(); }
     { y=1; } }
   But probably more like:
   { { x=1; } { Foo(); } { y=1; } }
 ****************************************************************************)
let rec contains_call il =
  match il with
      [] -> false
    | Call _ :: tl -> true
    | _ :: tl -> contains_call tl

class callBBVisitor =
object
  inherit nopCilVisitor

  method! vstmt s =
    match s.skind with
        Instr il when contains_call il ->
          begin
            let list_of_stmts =
              Util.list_map (fun one_inst -> mkStmtOneInstr one_inst) il in
            let block = mkBlock list_of_stmts in
              ChangeDoChildrenPost
                (s, (fun _ -> s.skind <- Block block; s))
          end
      | _ -> DoChildren

  method! vvdec _ = SkipChildren
  method! vexpr _ = SkipChildren
  method! vlval _ = SkipChildren
  method! vtype _ = SkipChildren
end

let calls_end_basic_blocks f =
  let thisVisitor = new callBBVisitor in
    visitCilFileSameGlobals thisVisitor f

(*****************************************************************************
   A transformation that gives each variable a unique identifier.
 ****************************************************************************)
class vidVisitor = object
  inherit nopCilVisitor
  val count = ref 0

  method! vvdec vi =
    vi.vid <- !count;
    incr count;
    SkipChildren
end

let globally_unique_vids f =
  let thisVisitor = new vidVisitor in
    visitCilFileSameGlobals thisVisitor f

let makeCFGFeature =
  { fd_name = "makeCFG";
    fd_enabled = false;
    fd_description = "make the program look more like a CFG" ;
    fd_extraopt = [];
    fd_doit = (fun f ->
      ignore (calls_end_basic_blocks f) ;
      ignore (globally_unique_vids f) ;
      iterGlobals f (fun glob -> match glob with
        GFun(fd,_) -> prepareCFG fd ;
                      ignore (computeCFGInfo fd true)
      | _ -> ())
    );
    fd_post_check = true;
  }

let () = Feature.register makeCFGFeature

(*

   Copyright (c) 2001-2002,
    George C. Necula    <necula@cs.berkeley.edu>
    Scott McPeak        <smcpeak@cs.berkeley.edu>
    Wes Weimer          <weimer@cs.berkeley.edu>
    Christoph L. Spiel  <Christoph.Spiel@partner.bmw.de>
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. The names of the contributors may not be used to endorse or promote
   products derived from this software without specific prior written
   permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 *)
