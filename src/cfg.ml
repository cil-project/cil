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

let sid_counter = ref 0 
  
class clear = object
  inherit nopCilVisitor
  method vstmt s = begin
    s.sid <- !sid_counter ;
    incr sid_counter ;
    s.succs <- [] ;
    s.preds <- [] ;
    DoChildren
  end
end

let link source dest = begin
  if not (List.mem dest source.succs) then
    source.succs <- dest :: source.succs ;
  if not (List.mem source dest.preds) then
    dest.preds <- source :: dest.preds 
end
let trylink source dest_option = match dest_option with
  None -> ()
| Some(dest) -> link source dest 

let rec succpred_block b fallthrough =
  let rec handle sl = match sl with
    [] -> ()
  | [a] -> succpred_stmt a fallthrough 
  | hd :: tl -> succpred_stmt hd (Some(List.hd tl)) ;
                handle tl 
  in handle b.bstmts
and succpred_stmt s fallthrough = 
  match s.skind with
    Instr _ -> trylink s fallthrough
  | Return _ -> ()
  | Goto(dest,l) -> link s !dest
  | Break _ -> failwith "succpred: break"
  | Continue _ -> failwith "succpred: continue"
  | Switch _ -> failwith "succpred: switch"
  | If(e1,b1,b2,l) -> 
      (match b1.bstmts with
        [] -> trylink s fallthrough
      | hd :: tl -> (link s hd ; succpred_block b1 fallthrough )) ;
      (match b2.bstmts with
        [] -> trylink s fallthrough
      | hd :: tl -> (link s hd ; succpred_block b2 fallthrough ))
  | Loop(b,l) -> begin match b.bstmts with
                   [] -> failwith "succpred: empty loop!?" 
                 | hd :: tl -> 
                    link s hd ; 
                    succpred_block b (Some(hd))
                 end
  | Block(b) -> begin match b.bstmts with
                  [] -> trylink s fallthrough
                | hd :: tl -> link s hd ;
                    succpred_block b fallthrough
                end

let make_cfg (f : file) = begin
  remove_switch_file f ; 
  let clear_it = new clear in 
  iterGlobals f (fun glob -> match glob with 
    GFun(fd,_) -> 
      sid_counter := 0 ; 
      ignore (visitCilBlock clear_it fd.sbody) ;
      fd.smaxstmtid <- Some(!sid_counter) ;
      succpred_block fd.sbody (None) ;
  | _ -> ())
end
