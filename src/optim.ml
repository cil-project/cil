(* Written by S. P. Rahul *)

(* Optimizes the placement of boxing code *)

open Pretty
open Cil
module E=Errormsg

let debug = false
(*-----------------------------------------------------------------*)
let dummyStmt = {labels = []; sid = -1; skind = Break {line = -1;col = -1;file = ""};
                  succs= []; preds = []} (* a place holder so that we can create
                                         an array of stmts *)
let sCount = ref 0   (* to generate consecutive sid *)
let numNodes = ref 0 (* number of nodes in the CFG *)
let nodes = ref [| |] (* ordered nodes of the CFG. Eg. for forward problems,
                         reverse depth-first postorder *)
let markedNodes = ref [] (* Used in dfs for recording visited nodes *)
(*------------------------------------------------------------*)
(* Notes regarding CFG computation:
   1) Initially only succs and preds are computed. sid are filled in
      later in whatever order is suitable.
   2) If a stmt (return, break or continue) has no successors, then 
      function return must follow.
      No predecessors means it is the start of the function
   3) We use the fact that initially all the succs and preds are assigned []
*)
       
(* Fill in the CFG info for the stmts in a block
   next = succ of the last stmt in this block
   break = succ of any Break in this block
   cont  = succ of any Continue in this block
   None means the succ is the function return. It does not mean the break/cont
   is invalid. We assume the validity has already been checked.
*)
(* At the end of CFG computation, numNodes = total number of CFG nodes 
 * (stmts) *)
let rec cfgBlock blk (next:stmt option) (break:stmt option) (cont:stmt option) =
  match blk with
    [] -> ();
  | [s] -> cfgStmt s next break cont 
  | hd::tl -> 
      cfgStmt hd (Some (List.hd tl))  break cont;
      cfgBlock tl next break cont

(* Fill in the CFG info for a stmt
   Meaning of next, break, cont should be clear from earlier comment
*)
and cfgStmt s (next:stmt option) (break:stmt option) (cont:stmt option) =
  numNodes := !numNodes + 1;
  match s.skind with 
    Instr _  -> 
      (match next with
      None -> ()
    | Some n -> s.succs <- [n]; n.preds <- s::n.preds)
  | Return _  -> ()
  | Goto (p,_) ->
      s.succs <- [!p]; (!p).preds <- s::(!p).preds
  | Break _ ->
      (match break with
        None -> ()
      | Some b -> s.succs <- [b]; b.preds <- s::b.preds)
  | Continue _ -> 
      (match cont with
        None -> ()
      | Some c -> s.succs <- [c]; c.preds <- s::c.preds)
  | If (_, blk1, blk2, _) ->
      (* The succs of If is [true branch;false branch] *)
      (match blk2 with
        [] -> (match next with
          None -> ()
        | Some n -> s.succs <- n::s.succs; n.preds <- s::n.preds)
      | (hd::tl) -> s.succs <- hd::s.succs; hd.preds <- s::hd.preds);
      (match blk1 with
        [] -> (match next with
          None -> ()
        | Some n -> s.succs <- n::s.succs; n.preds <- s::n.preds)
      | (hd::tl) -> s.succs <- hd::s.succs; hd.preds <- s::hd.preds);
      cfgBlock blk1 next break cont;
      cfgBlock blk2 next break cont
  | Switch(_,blk,l,_) ->
      s.succs <- l; (* in order *)
      List.iter (function br -> br.preds <- s::br.preds) l;
      cfgBlock blk next next cont
  | Loop(blk,_) ->
      if (blk <> []) then begin
        s.succs <- [List.hd blk]; 
        (List.hd blk).preds <- s::(List.hd blk).preds
      end;
      cfgBlock blk (Some s) next (Some s)
      (* Since all loops have terminating condition true, we don't put
         any direct successor to stmt following the loop *)
        

let rec printCfgBlock blk =
  List.iter (function s ->
    printCfgStmt s)
    blk
    
and printCfgStmt s =
  ignore (printf "-------------------------------------------------\n");
  ignore (printf "Id: %d\n" s.sid);
  ignore (printf "Succs: ");
  List.iter (function s -> ignore (printf "%d " s.sid)) s.succs; 
  ignore (printf "\n");
  ignore (printf "Preds: ");
  List.iter (function s -> ignore (printf "%d " s.sid)) s.preds; 
  ignore (printf "\n");
  match s.skind  with 
  | If (test, blk1, blk2, _) -> 
      ignore (printf "Cond: %a\n" d_exp test);
      printCfgBlock blk1; printCfgBlock blk2
  | Switch(test,blk,_,_) -> 
      ignore (printf "Switch: %a\n" d_exp test);
      printCfgBlock blk
  | Loop(blk,_) ->
      ignore (printf "Loop\n"); 
      printCfgBlock blk
  | _ -> ignore (printf "%a\n" d_stmt s)

(* Assign sid based on reverse depth-first postorder *)
let rec orderBlock blk =
  if (blk <> []) then begin
    dfs (List.hd  blk)
  end
  else ()

and dfs s = 
  if not (List.memq s !markedNodes) then begin
    markedNodes := s::!markedNodes;
    List.iter dfs s.succs;
    s.sid <- !sCount;
    !nodes.(!sCount) <- s; (* add to ordered array *)
    sCount := !sCount - 1;
  end

(*-----------------------------------------------------------------*)
let nullChecksOptim f =
  f

(*------------------------------------------------------------*)
(* Carry out a series of optimizations on a function definition *)

let optimFun f =
  if debug then begin
    ignore (printf "===================================\n");
    ignore (printf "Analyzing %s\n" f.svar.vname);
  end;

  (* Fill in the CFG information*)
  numNodes := 0;
  cfgBlock f.sbody None None None;
  sCount := !numNodes-1;
  nodes := Array.create !numNodes dummyStmt;
  orderBlock f.sbody; (* assign sid *)
  
  if debug then printCfgBlock f.sbody;
  
  let optimizedF = ref f in
  (* Carry out the optimizations, one at a time *)

  (* Remove redundant CHECK_NULL *)
  optimizedF := nullChecksOptim !optimizedF;
  
  (* Return the final optimized version *)
  !optimizedF
    
(*------------------------------------------------------------*)
let optimFile file =
  if debug
  then begin
    ignore(printf "\n-------------------------------------------------\n");
    ignore(printf "OPTIM MODULE STARTS\n\n")
  end;
  
  (* Replace every function definition by its optimized version*)
  file.globals <- List.map 
      (function 
          GFun(f,l) -> GFun(optimFun f, l)
        | _ as other -> other)
      file.globals;
  
  if debug
  then begin
    ignore(printf "\n\nOPTIM MODULE ENDS\n");
    ignore(printf "--------------------------------------------------\n")
  end;
  
  file
(*------------------------------------------------------------*)
    

  











