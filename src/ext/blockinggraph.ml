(*
 *
 * Copyright (c) 2001-2002, 
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
module H = Hashtbl
open Cil
open Pretty
module E = Errormsg

let debug = false


type blockkind =
    NoBlock
  | BlockTrans
  | BlockPoint
  | EndPoint

(* For each function we have a node *)
type node =
{
  name: string;
  mutable scanned: bool;
  mutable fds: fundec option;
  mutable bkind: blockkind;
  mutable preds: node list;
  mutable succs: node list;
  mutable predstmts: (stmt * node) list;
}

type blockpt =
{
  id: int;
  point: stmt;
  callfun: string;
  infun: string;
  mutable leadsto: blockpt list;
}

(* We add a dummy function whose name is "@@functionPointer@@" that is called 
 * at all invocations of function pointers and itself calls all functions 
 * whose address is taken.  *)
let functionPointerName = "@@functionPointer@@"

(* We map names to nodes *)
let functionNodes: (string, node) H.t = H.create 113 
let getFunctionNode (n: string) : node = 
  Util.memoize 
    functionNodes
    n
    (fun _ -> { name = n; fds = None; scanned = false; bkind = NoBlock;
                preds = []; succs = []; predstmts = []; })

(* We map types to nodes for function pointers *)
let functionPtrNodes: (typsig, node) H.t = H.create 113
let getFunctionPtrNode (t: typ) : node =
  Util.memoize
    functionPtrNodes
    (typeSig t)
    (fun _ -> { name = functionPointerName; fds = None; scanned = false;
                bkind = NoBlock; preds = []; succs = []; predstmts = []; })


(** Dump the function call graph. *)
let dumpGraph = true
let dumpFunctionCallGraph (start: node) = 
  H.iter (fun _ x -> x.scanned <- false) functionNodes;
  let rec dumpOneNode (ind: int) (n: node) : unit = 
    output_string !E.logChannel "\n";
    for i = 0 to ind do 
      output_string !E.logChannel "  "
    done;
    output_string !E.logChannel (n.name ^ " ");
    begin
      match n.bkind with
        NoBlock -> ()
      | BlockTrans -> output_string !E.logChannel " <blocks>"
      | BlockPoint -> output_string !E.logChannel " <blockpt>"
      | EndPoint -> output_string !E.logChannel " <endpt>"
    end;
    if n.scanned then (* Already dumped *)
      output_string !E.logChannel " <rec> "
    else begin
      n.scanned <- true;
      List.iter (dumpOneNode (ind + 1)) n.succs
    end
  in
  dumpOneNode 0 start;
  output_string !E.logChannel "\n\n"
  

let addCall (callerNode: node) (calleeNode: node) (sopt: stmt option) =
  if not (List.exists (fun n -> n.name = calleeNode.name)
                      callerNode.succs) then begin
    if debug then
      ignore (E.log "found call from %s to %s\n"
                    callerNode.name calleeNode.name);
    callerNode.succs <- calleeNode :: callerNode.succs;
    calleeNode.preds <- callerNode :: calleeNode.preds;
  end;
  match sopt with
    Some s ->
      if not (List.exists (fun (s', _) -> s' = s) calleeNode.predstmts) then
        calleeNode.predstmts <- (s, callerNode) :: calleeNode.predstmts
  | None -> ()


class findCallsVisitor (host: node) : cilVisitor = object
  inherit nopCilVisitor

  val mutable curStmt : stmt ref = ref (mkEmptyStmt ())

  method vstmt s =
    curStmt := s;
    DoChildren

  method vinst i =
    match i with
    | Call(_,Lval(Var(vi),NoOffset),args,l) -> 
        addCall host (getFunctionNode vi.vname) (Some !curStmt);
        SkipChildren

    | Call(_,e,_,l) -> (* Calling a function pointer *)
        addCall host (getFunctionPtrNode (typeOf e)) (Some !curStmt);
        SkipChildren

    | _ -> SkipChildren (* No calls in other instructions *)

  (* There are no calls in expressions and types *)          
  method vexpr e = SkipChildren
  method vtype t = SkipChildren

end


let endPt = { id = 0; point = mkEmptyStmt (); callfun = "end"; infun = "end";
              leadsto = []; }

(* These values will be initialized for real in makeBlockingGraph. *)
let curId : int ref = ref 1
let startName = ref ""
let blockingPoints = ref []
let blockingPointsNew = ref []
let blockingPointsHash = Hashtbl.create 117

let getFreshNum () : int =
  let num = !curId in
  curId := !curId + 1;
  num

let getBlockPt (s: stmt) (cfun: string) (ifun: string) : blockpt =
  try
    Hashtbl.find blockingPointsHash s
  with Not_found ->
    let num = getFreshNum () in
    let bpt = { id = num; point = s; callfun = cfun; infun = ifun;
                leadsto = []; } in
    Hashtbl.add blockingPointsHash s bpt;
    blockingPoints := bpt :: !blockingPoints;
    blockingPointsNew := bpt :: !blockingPointsNew;
    bpt


type action =
    Process of stmt * node
  | Next of stmt * node
  | Return of node

let getStmtNode (s: stmt) : node option =
  match s.skind with
    Instr instrs -> begin
      let len = List.length instrs in
      if len > 0 then
        match List.nth instrs (len - 1) with
          Call (_, Lval (Var vi, NoOffset), args, _) -> 
            Some (getFunctionNode vi.vname)
        | Call (_, e, _, _) -> (* Calling a function pointer *)
            Some (getFunctionPtrNode (typeOf e))
        | _ ->
            None
      else
        None
      end
  | _ -> None

let addBlockingPointEdge (bptFrom: blockpt) (bptTo: blockpt) : unit =
  if not (List.exists (fun bpt -> bpt = bptTo) bptFrom.leadsto) then
    bptFrom.leadsto <- bptTo :: bptFrom.leadsto

let findBlockingPointEdges (bpt: blockpt) : unit =
  let seenStmts = Hashtbl.create 117 in
  let worklist = ref [Next (bpt.point, getFunctionNode bpt.infun)] in
  while !worklist <> [] do
    let act = List.hd !worklist in
    worklist := List.tl !worklist;
    match act with
      Process (curStmt, curNode) -> begin
        Hashtbl.add seenStmts curStmt ();
        match getStmtNode curStmt with
          Some node -> begin
            if debug then
              ignore (E.log "processing node %s\n" node.name);
            match node.bkind with
              NoBlock ->
                worklist := Next (curStmt, curNode) :: !worklist
            | BlockTrans -> begin
                let processFundec (fd: fundec) : unit =
                  let s = List.hd fd.sbody.bstmts in
                  if not (Hashtbl.mem seenStmts s) then
                    let n = getFunctionNode fd.svar.vname in
                    worklist := Process (s, n) :: !worklist
                in
                match node.fds with
                  Some fd ->
                    processFundec fd
                | None ->
                    List.iter
                      (fun n ->
                         match n.fds with
                           Some fd -> processFundec fd
                         | None -> E.s (bug "expected fundec"))
                      node.succs
              end
            | BlockPoint ->
                addBlockingPointEdge bpt
                  (getBlockPt curStmt node.name curNode.name)
            | EndPoint ->
                addBlockingPointEdge bpt endPt
          end
        | _ ->
            worklist := Next (curStmt, curNode) :: !worklist
      end
    | Next (curStmt, curNode) -> begin
        match curStmt.Cil.succs with
          [] ->
            if debug then
              ignore (E.log "hit end of %s\n" curNode.name);
            worklist := Return curNode :: !worklist
        | _ ->
            List.iter (fun s ->
                         if not (Hashtbl.mem seenStmts s) then
                           worklist := Process (s, curNode) :: !worklist)
                      curStmt.Cil.succs
      end
    | Return curNode when curNode.name = !startName ->
        addBlockingPointEdge bpt endPt
    | Return curNode ->
        List.iter (fun (s, n) -> worklist := Next (s, n) :: !worklist)
                  curNode.predstmts;
        List.iter (fun n -> if n.fds = None then
                              worklist := Return n :: !worklist)
                  curNode.preds
  done

let makeBlockingGraph (start: node) =
  let startStmt =
    match start.fds with
      Some fd -> List.hd fd.sbody.bstmts
    | None -> E.s (bug "expected fundec")
  in
  curId := 1;
  startName := start.name;
  blockingPoints := [endPt];
  blockingPointsNew := [];
  Hashtbl.clear blockingPointsHash;
  ignore (getBlockPt startStmt "start" "start");
  while !blockingPointsNew <> [] do
    let bpt = List.hd !blockingPointsNew in
    blockingPointsNew := List.tl !blockingPointsNew;
    findBlockingPointEdges bpt;
  done

let dumpBlockingGraph () =
  List.iter
    (fun bpt ->
       if bpt.id < 2 then begin
         ignore (E.log "bpt %d (%s): " bpt.id bpt.callfun)
       end else begin
         ignore (E.log "bpt %d (%s in %s): " bpt.id bpt.callfun bpt.infun)
       end;
       List.iter (fun bpt -> ignore (E.log "%d " bpt.id)) bpt.leadsto;
       ignore (E.log "\n"))
    !blockingPoints;
  ignore (E.log "\n")


let startNodes : node list ref = ref []

let makeAndDumpBlockingGraphs () : unit =
  List.iter
    (fun n ->
       makeBlockingGraph n;
       dumpFunctionCallGraph n;
       dumpBlockingGraph ())
    !startNodes


let blockingNodes : node list ref = ref []

let markBlockingFunctions () : unit =
  let rec markFunction (n: node) : unit =
    if debug then
      ignore (E.log "marking %s\n" n.name);
    if n.bkind = NoBlock then begin
      n.bkind <- BlockTrans;
      List.iter markFunction n.preds;
    end
  in
  List.iter (fun n -> List.iter markFunction n.preds) !blockingNodes

let markVar (vi: varinfo) : unit =
  let node = getFunctionNode vi.vname in
  if node.bkind = NoBlock then begin
    if hasAttribute "yield" vi.vattr then begin
      node.bkind <- BlockPoint;
      blockingNodes := node :: !blockingNodes;
    end else if hasAttribute "noreturn" vi.vattr then begin
      node.bkind <- EndPoint;
    end
  end

let makeFunctionCallGraph (f: Cil.file) : unit = 
  H.clear functionNodes;
  (* Scan the file and construct the control-flow graph *)
  List.iter
    (function
        GFun(fdec, _) -> 
          let curNode = getFunctionNode fdec.svar.vname in
          if fdec.svar.vaddrof then begin
            addCall (getFunctionPtrNode (TPtr (fdec.svar.vtype, [])))
                    curNode None;
          end;
          if hasAttribute "start" fdec.svar.vattr then begin
            startNodes := curNode :: !startNodes;
          end;
          markVar fdec.svar;
          curNode.fds <- Some fdec;
          let vis = new findCallsVisitor curNode in
          ignore (visitCilBlock vis fdec.sbody)

      | GVarDecl(vi, _) ->
          (* TODO: what if we take the addr of an extern? *)
          markVar vi

      | _ -> ())
    f.globals


let makeAndDumpFunctionCallGraph (f: file) = 
  makeFunctionCallGraph f;
  markBlockingFunctions ();
  makeAndDumpBlockingGraphs ();
