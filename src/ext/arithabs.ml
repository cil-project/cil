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
open Cil
open Pretty
module E = Errormsg
module CG = Callgraph
module H = Hashtbl 
module U = Util
module IH = Inthash
module VS = Usedef.VS

module S = Ssa

let debug = false

let arithAbsOut = ref stdout
let setArithAbsFile (s: string) =
  try 
    arithAbsOut := open_out s
  with _ -> ignore (E.warn "Cannot open the output file %s" s)

(* Print out *)
let pd ?(ind=0) (d: doc) : unit = 
  Pretty.fprint !arithAbsOut 80 (indent ind d)

let p ?(ind=0) (fmt : ('a,unit,doc) format) : 'a = 
  let f d =  
    pd ~ind:ind d;
    nil 
  in
  Pretty.gprintf f fmt


(** The globals written, indexed by Id of the function variable. Each inner 
 * table is indexed by the global id *)
let globalsWritten: (varinfo IH.t) IH.t = IH.create 13
let currentGlobalsWritten: (varinfo IH.t) ref = ref (IH.create 13)


(** The transitive closure of the globals written *)
let globalsWrittenTransitive: (varinfo IH.t) IH.t = IH.create 13

let globalsRead: (varinfo IH.t) IH.t = IH.create 13
let currentGlobalsRead: (varinfo IH.t) ref = ref (IH.create 13)

let globalsReadTransitive: (varinfo IH.t) IH.t = IH.create 13

let considerVariable (v: varinfo) : bool = 
  not v.vaddrof && isIntegralType v.vtype

class gwVisitorClass : cilVisitor = object (self) 
  inherit nopCilVisitor

  method vexpr = function
      Lval (Var v, _) when v.vglob && considerVariable v -> 
        IH.replace !currentGlobalsRead v.vid v;
        DoChildren

    | _ -> DoChildren

  method vinst = function
      Set ((Var v, _), _, _) 
    | Call (Some (Var v, _), _, _, _) when v.vglob && considerVariable v -> 
        IH.replace !currentGlobalsWritten v.vid v;
        DoChildren
    | _ -> DoChildren
end

let gwVisitor = new gwVisitorClass


(** Functions can be defined or just declared *)
type funinfo = 
    Decl of varinfo
  | Def of fundec

(* All functions indexed by the variable ID *)
let allFunctions: funinfo IH.t = IH.create 13


(** Compute the SSA form *)
 


    
let fundecToCFGInfo (fdec: fundec) : S.cfgInfo = 
  (* Go over the statments and make sure they are numbered properly *)
  let count = ref 0 in
  List.iter (fun s -> s.sid <- !count; incr count) fdec.sallstmts;

  let start: stmt = 
    match fdec.sbody.bstmts with
      [] -> E.s (E.bug "Function %s with no body" fdec.svar.vname)
    | fst :: _ -> fst
  in
  if start.sid <> 0 then 
    E.s (E.bug "The first block must have index 0");
  
  
  let ci = 
    { S.start = start.sid;
      S.size  = !count;
      S.successors = Array.make !count [];
      S.predecessors = Array.make !count [];
      S.blocks = Array.make !count { S.bstmt = start; 
                                     S.instrlist = [];
                                     S.livevars = [] };
      S.nrRegs = 0;
      S.regToVarinfo = Array.make 0 dummyFunDec.svar;
    } 
  in

  (* Map a variable to a register *)
  let varToRegMap: S.reg IH.t = IH.create 13 in 
  let regToVarMap: varinfo IH.t = IH.create 13 in 
  let varToReg (v: varinfo) : S.reg = 
    try IH.find varToRegMap v.vid
    with Not_found -> 
      let res = ci.S.nrRegs in 
      ci.S.nrRegs <- 1 + ci.S.nrRegs;
      IH.add varToRegMap v.vid res;
      IH.add regToVarMap res v;
      res
  in
  (* For functions, we use the transitively computed set of globals and 
   * locals as the use/def *)
  Usedef.getUseDefFunctionRef := 
    (fun f ->
      match f with 
        Lval (Var fv, NoOffset) -> 
          let varDefs = ref VS.empty in 
          let varUsed = ref VS.empty in 
          (try 
            let gw = IH.find globalsWrittenTransitive fv.vid in
            IH.iter 
              (fun _ g -> varDefs := VS.add g !varDefs) gw
          with Not_found -> (* Do not have a definition for it *)
            ());
          (* Now look for globals read *)
          (try 
            let gr = IH.find globalsReadTransitive fv.vid in 
            IH.iter
              (fun _ g -> varUsed := VS.add g !varUsed) gr
          with Not_found -> ());

          !varUsed, !varDefs

      | _ -> VS.empty, VS.empty);

  let vsToRegList (vs: VS.t) : int list = 
    VS.fold (fun v acc -> varToReg v :: acc) vs [] 
  in
  List.iter 
    (fun s -> 
      ci.S.successors.(s.sid) <- List.map (fun s' -> s'.sid) s.succs;
      ci.S.predecessors.(s.sid) <- List.map (fun s' -> s'.sid) s.preds;
      ci.S.blocks.(s.sid) <- begin
        let instrs: (S.reg list * S.reg list) list = 
          match s.skind with 
            Instr il -> 
              (* Each instruction is transformed independently *)
              List.map (fun i -> 
                let vused, vdefs = Usedef.computeUseDefInstr i in 
                (vsToRegList vdefs, vsToRegList vused)) il
                
          | Return (Some e, _) 
          | If (e, _, _, _) 
          | Switch (e, _, _, _) ->
              let vused = Usedef.computeUseExp e in 
              [ ([], vsToRegList vused) ]
                
          | Break _ | Continue _ | Goto _ | Block _ | Loop _ | Return _ -> [ ]
          | TryExcept _ | TryFinally _ -> assert false
        in
        { S.bstmt = s;
          S.instrlist = instrs;
          S.livevars = []; (* Will be filled in later *)
        }
      end
    ) fdec.sallstmts;

  (* Set the mapping from registers to variables *)
  ci.S.regToVarinfo <-
    Array.make ci.S.nrRegs dummyFunDec.svar;
  IH.iter (fun rid v -> 
    ci.S.regToVarinfo.(rid) <- v) regToVarMap;

  ci



let globalsDumped = IH.create 13

(** We define a new printer *)
class absPrinterClass (callgraph: CG.callgraph) : cilPrinter = 

  let lastFreshId= ref 0 in 
  let freshVarId () = incr lastFreshId; !lastFreshId in
  

  object (self) 
    inherit defaultCilPrinterClass as super
        
    val mutable idomData: stmt option IH.t = IH.create 13

    val mutable cfgInfo: S.cfgInfo option = None 

        (** For each block end, a mapping from IDs of variables that were 
         * defined in this block, to their fresh ID *)
    val mutable blockEndData: int IH.t array = 
      Array.make 0 (IH.create 13)

        (** For each block start, remember the starting newFreshId as we 
         * start the block *)
    val mutable blockStartData: int array = 
      Array.make 0 (-1) 


    val mutable varRenameState: int IH.t = IH.create 13


    method private initVarRenameState (b: S.cfgBlock) =
      IH.clear varRenameState;
      
      (* Convert livevars, to use varinfo *)
      (* Initialize it based on the livevars info in the block *)
      List.iter 
        (fun (rid, defblk) -> 
          let v = 
            match cfgInfo with
              None -> assert false
            | Some cfg -> cfg.S.regToVarinfo.(rid)
          in
          if defblk = b.S.bstmt.sid then
            IH.add varRenameState v.vid (freshVarId ())
          else begin 
            let fid = 
              try IH.find blockEndData.(defblk) v.vid
              with Not_found -> 
                E.s (E.bug "Cannot find data for variable %s in block %d"
                       v.vname defblk)
            in
            IH.add varRenameState v.vid fid
          end)
        b.S.livevars;
      
      ()
        
    method private variableUse (v: varinfo) : doc = 
      let freshId = 
        try IH.find varRenameState v.vid
        with Not_found -> 
          E.s (E.bug "varRenameState does not know anything about %s" v.vname)
      in
      text (v.vname ^ "___" ^ string_of_int freshId)
        
    method private variableDef (v: varinfo) : doc = 
      IH.replace varRenameState v.vid (freshVarId ());
      self#variableUse v
    
    method pExp () = function
      | Const (CInt64(i, _, _)) -> text (Int64.to_string i)
      | Const (CStr _) -> text "(rand)"
      | BinOp (bop, e1, e2, _) -> 
          dprintf "(%a @[%a@?%a@])" 
            d_binop bop
            self#pExp e1 self#pExp e2
      | UnOp (uop, e1, _) -> 
          dprintf "(%a @[%a@])"
            d_unop uop self#pExp e1
      | CastE (t, e) -> self#pExp () e (* Ignore casts *)
            
      | Lval (Var v, NoOffset) when considerVariable v -> self#variableUse v
            
            (* We ignore all other Lval *)
      | Lval _ -> text "(rand)"
            
      | e -> super#pExp () e
            
    method pInstr () = function
      | Set ((Var v, NoOffset), e, _) when considerVariable v -> 
          self#variableDef v ++ text "=" ++ self#pExp () e
            
      | Call (Some (Var v, NoOffset), f, args, _)  when considerVariable v -> 
          self#variableDef v ++ text "=" ++ 
            dprintf "(%a @[%a@])" 
            self#pExp f
            (docList ~sep:break (self#pExp ())) args
            
      | Call (None, f, args, _) -> 
          dprintf "(%a @[%a@])" 
            self#pExp f
            (docList ~sep:break (self#pExp ())) args
            
      | _ -> nil (* Ignore the other instructions *)        
            

    method dBlock (out: out_channel) (ind: int) (b: block) : unit = 
      ignore (p ~ind:ind "<block\n");
      List.iter (self#dStmt out (ind+ 2)) b.bstmts;
      ignore (p ~ind:ind ">\n")
        
    method dStmt (out: out_channel) (ind: int) (s: stmt) : unit = 

      (* Initialize the renamer for this statement *)
      lastFreshId := blockStartData.(s.sid);
      assert (!lastFreshId >= 0);

      let blk = 
        match cfgInfo with
          Some cfg -> cfg.S.blocks.(s.sid)
        | None -> assert false
      in
      self#initVarRenameState blk;

      pd (self#pLineDirective (get_stmtLoc s.skind));
      (* Lookup its dominator *)
      let idom: doc = 
        match Dominators.getIdom idomData s with 
          Some dom -> num dom.sid
        | None -> nil
      in
      ignore (p ~ind:ind
                "<stmt %d <succs %a> <preds %a> <idom %a>\n"
                s.sid (** Statement id *)
                (d_list "," (fun _ s' -> num s'.sid)) s.succs
                (d_list "," (fun _ s' -> num s'.sid)) s.preds
                insert idom);
      (* Now the statement kind *)
      let ind = ind + 2 in 
      (match s.skind with 
      | Instr il -> 
          List.iter 
            (fun i -> 
              pd ~ind:ind (self#pInstr () i ++ line))
            il
      | Block b -> List.iter (self#dStmt out ind) b.bstmts
      | Goto (s, _) -> ignore (p ~ind:ind "goto %d\n" !s.sid)
      | Return (None, _) -> ignore (p ~ind:ind "return;\n")
      | Return (Some e, _) -> ignore (p ~ind:ind "return %a;\n" self#pExp e);
      | If(e, b1, b2, _) -> 
          ignore (p ~ind:ind "<if %a\n" self#pExp e);
          self#dBlock out (ind + 2) b1;
          self#dBlock out (ind + 2) b2;
          ignore (p ~ind:ind ">\n")
            
      | Loop (b, _, Some co, Some br) -> 
          ignore (p ~ind:ind "<loop <cont %d> <break %d>\n" co.sid br.sid);
          List.iter (self#dStmt out (ind+ 2)) b.bstmts;
          ignore (p ~ind:ind ">\n")
            
            (* The other cases should have been removed already *)
      | _ -> E.s (E.unimp "try except"));
      
      (* The termination *)
      let ind = ind - 2 in
      ignore (p ~ind:ind ">\n")
        
        
    method dGlobal (out: out_channel) (g: global) : unit = 
      match g with 
        GFun (fdec, l) -> 
          (* Make sure we use one return at most *)
          Oneret.oneret fdec;
          
          (* Now compute the immediate dominators. This will fill in the CFG 
           * info as well *)
          idomData <- Dominators.computeIDom fdec;
          
          (** Get the callgraph node for this function *)
          let cg_node: CG.callnode = 
            try H.find callgraph fdec.svar.vname
            with Not_found -> E.s (E.bug "Cannot find call graph info for %s"
                                     fdec.svar.vname)
          in
          
          (** Get the globals read and written *)
          let glob_read =
            (try IH.find globalsRead fdec.svar.vid
            with Not_found -> assert false) in
          let glob_read_trans = 
            (try IH.find globalsReadTransitive fdec.svar.vid
            with Not_found -> assert false) in 
          
          
          let glob_written = 
            (try IH.find globalsWritten fdec.svar.vid
            with Not_found -> assert false) in 
          let glob_written_trans = 
            (try IH.find globalsWrittenTransitive fdec.svar.vid
            with Not_found -> assert false) in 
          
          (* Compute the control flow graph info, for SSA computation *)
          let cfgi = fundecToCFGInfo fdec in 
          cfgInfo <- Some cfgi;
          (* Call here the SSA function to fill-in the cfgInfo *)
          S.add_ssa_info cfgi;

          (* Now do the SSA renaming. *)
          
          (* Remember the return block *)
          let retBlk: int option ref = ref None in 
          
          lastFreshId := 0;

          Array.iteri (fun i (b: S.cfgBlock) -> 
            (* compute the initial state *)
            blockStartData.(i) <- !lastFreshId;
            
            (* Initialize the renaming state *)
            self#initVarRenameState b;
            
            (* Now scan the block and keep track of the definitions *)
            (match b.S.bstmt.skind with 
              Instr il -> begin
                List.iter
                  (fun i -> 
                    match i with 
                      Set ((Var v, NoOffset), _, _) 
                    | Call (Some (Var v, NoOffset), _, _, _) 
                      when considerVariable v ->
                        IH.replace varRenameState v.vid (freshVarId ())
                    | _ -> ())
                  il
              end
            | Return _ -> 
                assert (!retBlk = None);
                retBlk := Some i
                    
            | _ -> (* No definitions *)
                ()
            );
            
            blockEndData.(i) <- varRenameState;
          ) 
          (match cfgInfo with 
           Some cfg -> cfg.S.blocks
         | _ -> assert false);
          
          (** For each basic block *)
        
            
        (* The header *)
        pd (self#pLineDirective ~forcefile:true l);

        ignore (p "<function %s\n  <formals %a>\n  <locals %a>\n  <globalsread %a>\n  <globalsreadtransitive %a>\n  <globalswritten %a>\n  <globalswrittentransitive %a>\n  <calls %a>\n  <calledby %a>\n"
          fdec.svar.vname
          (d_list "," (fun () v -> text v.vname)) fdec.sformals
          (d_list "," (fun () v -> text v.vname)) fdec.slocals
          (d_list "," (fun () (_, v) -> text v.vname)) (IH.tolist glob_read)
          (d_list "," (fun () (_, v) -> text v.vname)) (IH.tolist glob_read_trans)
          (d_list "," (fun () (_, v) -> text v.vname)) (IH.tolist glob_written)
          (d_list "," (fun () (_, v) -> text v.vname)) (IH.tolist glob_written_trans)
          (U.docHash (fun k _ -> text k)) cg_node.CG.cnCallees
          (U.docHash (fun k _ -> text k)) cg_node.CG.cnCallers);

        (* The block *)
        self#dBlock out 2 fdec.sbody;

        (* The end *)
        ignore (p "\n>\n\n")

    (* Emit the globals whose address is not taken *)
    | GVarDecl (vi, l) | GVar (vi, _, l) when 
        not vi.vaddrof && isIntegralType vi.vtype 
          && not (IH.mem globalsDumped vi.vid) 
      -> 
        IH.add globalsDumped vi.vid ();
        pd (self#pLineDirective ~forcefile:true l);
        ignore (p "<global %s>\n" vi.vname)
        
    | _ -> ()
end


let arithAbs (absPrinter: cilPrinter) (g: global) = 
  dumpGlobal absPrinter !arithAbsOut g

let feature : featureDescr = 
  { fd_name = "arithabs";
    fd_enabled = ref false;
    fd_description = "generation of an arithmetic abstraction";
    fd_extraopt = [
       ("--arithabs_file", Arg.String setArithAbsFile, 
        "the name of the file to dump the arithmetic abstraction to") ];
    fd_doit = 
    (function (f : file) ->
      (* Call the simplify *)
      Simplify.feature.fd_doit f;
      (* Compute the call graph *)
      let graph = CG.computeGraph f in 

      (* Compute the globals written by each function *)
      IH.clear globalsWritten;
      IH.clear globalsWrittenTransitive;
      IH.clear globalsRead;

      IH.clear allFunctions;


      (* Compute the globals read and written *)
      iterGlobals 
        f
        (function
            GFun(fdec, _) -> 
              IH.replace allFunctions fdec.svar.vid (Def fdec);
              currentGlobalsRead := IH.create 13;
              IH.add globalsRead fdec.svar.vid !currentGlobalsRead;
              currentGlobalsWritten := IH.create 13;
              IH.add globalsWritten fdec.svar.vid !currentGlobalsWritten;
              ignore (visitCilBlock gwVisitor fdec.sbody)

          | GVarDecl (vd, _) when isFunctionType vd.vtype &&
                                  not (IH.mem allFunctions vd.vid) 
            -> 
              IH.add allFunctions vd.vid (Decl vd)
          | _ -> ());

      (* Now do transitive closure of the globals written by each function *)
      (* Initialize each function with the globals it writes itself *)
      IH.iter 
        (fun fid gw -> 
          IH.add globalsWrittenTransitive fid (IH.copy gw))
        globalsWritten;

      IH.iter 
        (fun fid gr -> 
          IH.add globalsReadTransitive fid (IH.copy gr))
        globalsRead;

      (* A work list initialized with all functions, that are defined *)
      let worklist: int Queue.t = Queue.create () in
      IH.iter (fun fid finfo -> 
        match finfo with 
          Def _ -> Queue.add fid worklist
        | _ -> ())
        
        allFunctions;

      (* Now run until we reach a fixed point *)
      let rec fixedpoint () = 
        try 
          let next = Queue.take worklist in 
          (* Get the function info for this one *)
          let finfo = 
            try IH.find allFunctions next
            with Not_found -> 
              E.s (E.bug "Function id=%d not in allFunctions" next)
          in
          (* If this is just a declaration, we ignore *)
          match finfo with 
            Decl _ -> ()
          | Def fdec -> begin
              (* Find the callnode for it *)
              let cnode: CG.callnode = 
                try H.find graph fdec.svar.vname
                with Not_found -> 
                  E.s (E.bug "Function %s does not have a call node" 
                         fdec.svar.vname)
              in
              (* Union in all the variables modified by the functions this 
               * calls. Remember if we made a change. If we do, we add to the 
               * worklist the callers of this one. *)
              let changeMade = ref false in 

              (* Our written *)
              let ourWritten = 
                try IH.find globalsWrittenTransitive fdec.svar.vid
                with Not_found -> 
                  E.s (E.bug "Function %s not in globalsWrittenTransitive"
                      fdec.svar.vname)
              in

              (* Our read *)
              let ourRead = 
                try IH.find globalsReadTransitive fdec.svar.vid
                with Not_found -> 
                  E.s (E.bug "Function %s not in globalsReadTransitive"
                      fdec.svar.vname)
              in

              H.iter
                (fun n cn -> 
                  (* Get the callee's written *)
                  (try 
                    let callee_written = 
                      IH.find globalsWrittenTransitive cn.CG.cnInfo.vid in
                    IH.iter 
                      (fun gwid gw -> 
                        if not (IH.mem ourWritten gwid) then begin
                          IH.add ourWritten gwid gw;
                          changeMade := true
                        end)
                      callee_written;
                  with Not_found -> (* Callee not defined here *)
                    ());

                  (* Get the callee's read *)
                  (try 
                    let callee_read = 
                      IH.find globalsReadTransitive cn.CG.cnInfo.vid in
                    IH.iter 
                      (fun grid gr -> 
                        if not (IH.mem ourRead grid) then begin
                          IH.add ourRead grid gr;
                          changeMade := true
                        end)
                      callee_read;
                  with Not_found -> (* Callee not defined here *)
                    ());


                )
                cnode.CG.cnCallees;

              if !changeMade then begin
                H.iter 
                  (fun _ caller -> Queue.add caller.CG.cnInfo.vid worklist)
                  cnode.CG.cnCallers
              end
          end
                
        with Queue.Empty -> ()
      in
      fixedpoint ();
      

      let absPrinter: cilPrinter = new absPrinterClass graph in 
      IH.clear globalsDumped;
      iterGlobals f
        (arithAbs absPrinter););
    fd_post_check = false;
  } 
