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

(* 
 * Given a fixed CIL file, modify it so that it behaves more like a
 * control-flow graph for the purposes of analysis.
 *
 * The CIL file will not have break, default or continue statements. 
 * The "succs" and "preds" fields for every statement should be set
 * correctly. 
 *
 * In addition, all statements are given unique IDs. 
 *)
open Cil
open Errormsg

(*
 * Here is the magic handling to
 *  (1) replace switch statements with if/goto
 *  (2) remove "break"
 *  (3) remove "default"
 *  (4) remove "continue"
 *)
let is_case_label l = match l with
  Case _ | Default _ -> true
  | _ -> false

let switch_count = ref (-1) 
let get_switch_count () = 
  switch_count := 1 + !switch_count ;
  !switch_count

let rec xform_switch_stmt s break_dest cont_dest label_index = begin
  s.labels <- List.map (fun lab -> match lab with
    Label _ -> lab
  | Case(e,l) -> let str = Pretty.sprint 80 
									(Pretty.dprintf "switch_%d_%a" label_index d_exp e) in 
                 (Label(str,l,false))
  | Default(l) -> (Label(Printf.sprintf "switch_%d_default" label_index,l,false))
  ) s.labels ; 
  match s.skind with
    Instr _ -> ()
  | Return _ -> ()
  | Goto _ -> ()
  | Break(l) -> begin try 
                  s.skind <- Goto(break_dest (),l)
                with e ->
                  ignore (error "cfg: break: %a@!" d_stmt s) ;
                  raise e
                end
  | Continue(l) -> begin try
                  s.skind <- Goto(cont_dest (),l)
                with e ->
                  ignore (error "cfg: continue: %a@!" d_stmt s) ;
                  raise e
                end
  | If(e,b1,b2,l) -> xform_switch_block b1 break_dest cont_dest label_index ;
                     xform_switch_block b2 break_dest cont_dest label_index
  | Switch(e,b,sl,l) -> begin
      (* change 
       * switch (se) {
       *   case 0: s0 ;
       *   case 1: s1 ; break;
       *   ...
       * }
       *
       * into:
       *
       * if (se == 0) goto label_0;
       * else if (se == 1) goto label_1;
       * ...
       * else if (0) { // body_block
       *  label_0: s0;
       *  label_1: s1; goto label_break;
       *  ...
       * } else if (0) { // break_block
       *  label_break: ; // break_stmt
       * } 
       *  
       *)
      let i = get_switch_count () in 
      let break_stmt = mkStmt (Instr []) in
      break_stmt.labels <- 
				[Label((Printf.sprintf "switch_%d_break" i),l,false)] ;
      let break_block = mkBlock [ break_stmt ] in
      let body_block = b in 
      let body_if_stmtkind = (If(Cil.zero,body_block,break_block,l)) in
      let rec handle_choices sl = match sl with
        [] -> body_if_stmtkind
      | stmt_hd :: stmt_tl -> begin
        let rec handle_labels lab_list = begin
          match lab_list with
            [] -> handle_choices stmt_tl 
          | Case(ce,cl) :: lab_tl -> 
              let pred = BinOp(Eq,e,ce,voidType) in
              let then_block = mkBlock [ mkStmt (Goto(ref stmt_hd,cl)) ] in
              let else_block = mkBlock [ mkStmt (handle_labels lab_tl) ] in
              If(pred,then_block,else_block,cl)
          | Default(dl) :: lab_tl -> 
              let pred = Cil.one in
              let then_block = mkBlock [ mkStmt (Goto(ref stmt_hd,dl)) ] in
              let else_block = mkBlock [ mkStmt (handle_labels lab_tl) ] in
              If(pred,then_block,else_block,dl)
          | Label(_,_,_) :: lab_tl -> handle_labels lab_tl
        end in
        handle_labels stmt_hd.labels
      end in
      s.skind <- handle_choices sl ;
      xform_switch_block b (fun () -> ref break_stmt) cont_dest i 
    end
  | Loop(b,l) -> 
          let i = get_switch_count () in 
          let break_stmt = mkStmt (Instr []) in
          break_stmt.labels <- 
						[Label((Printf.sprintf "while_%d_break" i),l,false)] ;
          let cont_stmt = mkStmt (Instr []) in
          cont_stmt.labels <- 
						[Label((Printf.sprintf "while_%d_continue" i),l,false)] ;
          b.bstmts <- cont_stmt :: b.bstmts ;
          let this_stmt = mkStmt (s.skind) in
          let break_dest () = ref break_stmt in
          let cont_dest () = ref cont_stmt in 
          xform_switch_block b break_dest cont_dest label_index ;
          break_stmt.succs <- s.succs ; 
          let new_block = mkBlock [ this_stmt ; break_stmt ] in
          s.skind <- Block new_block
  | Block(b) -> xform_switch_block b break_dest cont_dest label_index
end and xform_switch_block b break_dest cont_dest label_index = 
  try 
    let rec link_succs sl = match sl with
    | [] -> ()
    | hd :: tl -> (if hd.succs = [] then hd.succs <- tl) ; link_succs tl
    in 
    link_succs b.bstmts ;
    List.iter (fun stmt -> xform_switch_stmt stmt break_dest cont_dest label_index) b.bstmts ;
  with e ->
    List.iter (fun stmt -> ignore
      (Pretty.printf "xform_switch_block: %a@!" d_stmt stmt)) b.bstmts ;
    raise e

let remove_switch_file f = 
  iterGlobals f (fun g -> match g with
    GFun(fd,_) -> xform_switch_block fd.sbody 
      (fun () -> failwith "cfg: break with no enclosing loop") 
      (fun () -> failwith "cfg: continue with no enclosing loop") (-1)
  | _ -> ()
  )

let make_cfg (f : file) = begin
  remove_switch_file f ; 
  iterGlobals f (fun glob -> match glob with 
    GFun(fd,_) -> ignore (computeCFGInfo fd)
  | _ -> ())
end
