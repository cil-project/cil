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


let getGlobalsWrittenTransitive (f: varinfo) = 
  let glob_written_trans = 
    (try IH.find globalsWrittenTransitive f.vid
    with Not_found -> assert false) in 
  IH.fold
    (fun _ g acc -> g :: acc)
    glob_written_trans
    []

let getGlobalsReadTransitive (f: varinfo) = 
  let glob_read_trans = 
    (try IH.find globalsReadTransitive f.vid
    with Not_found -> assert false) in 
  IH.fold
    (fun _ g acc -> g :: acc)
    glob_read_trans
    []
  
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
    { S.name  = fdec.svar.vname;
      S.start = start.sid;
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

(* Compute strongly-connected components *)
let stronglyConnectedComponents (cfg: S.cfgInfo) : S.sccInfo = 
  S.stronglyConnectedComponents cfg


let globalsDumped = IH.create 13

(** We define a new printer *)
class absPrinterClass (callgraph: CG.callgraph) : cilPrinter = 

  let lastFreshId= ref 0 in 

  (* freshVarId returns at least 1 *)
  let freshVarId () = incr lastFreshId; !lastFreshId in
  

  object (self) 
    inherit defaultCilPrinterClass as super
        
    val mutable idomData: stmt option IH.t = IH.create 13

    val mutable cfgInfo: S.cfgInfo option = None 
        
    val mutable currentFundec = dummyFunDec

        (** For each block end, a mapping from IDs of variables that were 
         * defined in this block, to their fresh ID *)
    val mutable blockEndData: int IH.t array = 
      Array.make 0 (IH.create 13)

        (** For each block start, remember the starting newFreshId as we 
         * start the block *)
    val mutable blockStartData: int array = 
      Array.make 0 (-1) 


    val mutable varRenameState: int IH.t = IH.create 13

          (* All the fresh variables *)
    val mutable freshVars: string list = []

          (* The uninitialized variables are those that are live on input but 
           * not globals or formals. *)
    val mutable uninitVars: string list = []

    method private initVarRenameState (b: S.cfgBlock) =
      IH.clear varRenameState;
      
      let cfgi = 
        match cfgInfo with
          None -> assert false
        | Some cfgi -> cfgi
      in
        
      (* Initialize it based on the livevars info in the block *)
      List.iter 
        (fun (rid, defblk) -> 
          let v = cfgi.S.regToVarinfo.(rid) in
          if defblk = b.S.bstmt.sid then
            (* Is a phi variable or a live variable at start *)
            if defblk = cfgi.S.start then begin
              (* For the start block, use ID=0 for all variables, except the 
               * locals that are not function formals. Those are fresh 
               * variables. *)
              let isUninitializedLocal = 
                not v.vglob &&
                (not (List.exists (fun v' -> v'.vid = v.vid) 
                        currentFundec.sformals)) in
              IH.add varRenameState v.vid 0;
              let vn = self#variableUse varRenameState v in
              if isUninitializedLocal then 
                uninitVars <- vn :: uninitVars;
            end else begin
              IH.add varRenameState v.vid (freshVarId ());
              let vn = self#variableUse varRenameState v in
              freshVars <- vn :: freshVars
            end
          else begin 
            let fid = 
              try IH.find blockEndData.(defblk) v.vid
              with Not_found -> 
                E.s (E.bug "In block %d: Cannot find data for variable %s in block %d"
                       b.S.bstmt.sid v.vname defblk)
            in
            IH.add varRenameState v.vid fid
          end)
        b.S.livevars;
      
      ()
        
    method private variableUse (state: int IH.t) (v: varinfo) : string = 
      let freshId = 
        try IH.find state v.vid
        with Not_found -> 
          E.s (E.bug "%a: varUse: varRenameState does not know anything about %s" 
                 d_loc !currentLoc v.vname )
      in
      if freshId = 0 then 
        v.vname
      else
        v.vname ^ "___" ^ string_of_int freshId
        
    method private variableDef (state: int IH.t) (v: varinfo) : string = 
      IH.replace state v.vid (freshVarId ());
      let n = self#variableUse state v in
      freshVars <- n :: freshVars;
      n
    
    method pExp () = function
      | Const (CInt64(i, _, _)) -> text (Int64.to_string i)
      | Const (CStr _) -> text "(@rand)"
      | BinOp (bop, e1, e2, _) -> 
          dprintf "(%a @[%a@?%a@])" 
            d_binop bop
            self#pExp e1 self#pExp e2
      | UnOp (uop, e1, _) -> 
          dprintf "(%a @[%a@])"
            d_unop uop self#pExp e1
      | CastE (t, e) -> self#pExp () e (* Ignore casts *)
            
      | Lval (Var v, NoOffset) when considerVariable v -> 
          text (self#variableUse varRenameState v)
            
            (* We ignore all other Lval *)
      | Lval _ -> text "(@rand)"
            
      | e -> super#pExp () e
            
    method pInstr () = function
      | Set ((Var v, NoOffset), e, _) when considerVariable v -> 
          text (self#variableDef varRenameState v) 
            ++ text "=" ++ self#pExp () e ++ text ";"
            
      | Call (Some (Var v, NoOffset), 
              Lval (Var f, NoOffset), args, _) ->
          let gwt: varinfo list = getGlobalsWrittenTransitive f in
          let grt: varinfo list = getGlobalsReadTransitive f in

          let gwt' = 
            if considerVariable v then gwt @ [v] else gwt in 
          (* Prepare the arguments first *)
          let argdoc: doc = 
            (docList ~sep:break (self#pExp ())) 
              ()
              (args @ (List.map (fun v -> Lval (Var v, NoOffset)) grt))
          in 
          dprintf "%a = (%s @[%a@]);"
            (docList 
               (fun v -> 
                 text (self#variableDef varRenameState v)))
            gwt'
            f.vname
            insert argdoc
            
      | Call (None, Lval (Var f, NoOffset), args, _) -> 
          let gwt: varinfo list = getGlobalsWrittenTransitive f in
          let grt: varinfo list = getGlobalsReadTransitive f in
          (* Prepare the arguments first *)
          let argdoc: doc = 
            (docList ~sep:break (self#pExp ())) 
              ()
              (args @ (List.map (fun v -> Lval (Var v, NoOffset)) grt))
          in 
          dprintf "%a = (%s @[%a@]);"
            (docList 
               (fun v -> 
                 text (self#variableDef varRenameState v)))
            gwt
            f.vname
            insert argdoc
            
      | _ -> nil (* Ignore the other instructions *)        
            

    method dBlock (out: out_channel) (ind: int) (b: block) : unit = 
      ignore (p ~ind:ind "<block\n");
      List.iter (self#dStmt out (ind+ 2)) b.bstmts;
      ignore (p ~ind:ind ">\n")
        
    method dStmt (out: out_channel) (ind: int) (s: stmt) : unit = 

      (* Initialize the renamer for this statement *)
      lastFreshId := blockStartData.(s.sid);
      assert (!lastFreshId >= 0);

      let cfgi = 
        match cfgInfo with
          Some cfgi -> cfgi
        | None -> assert false
      in
      let blk: S.cfgBlock = cfgi.S.blocks.(s.sid) in 

      self#initVarRenameState blk;

      let phivars: varinfo list = 
        List.fold_left
          (fun acc (i, defblk) -> 
            if defblk = s.sid then 
              cfgi.S.regToVarinfo.(i) :: acc
            else 
              acc)
          []
          blk.S.livevars 
      in
      (* do not emit phi for start block *)
      let phivars = 
        if s.sid = cfgi.S.start then 
          []
        else
          phivars
      in

      (* Get the predecessors information *)
      let getPhiAssignment (v: varinfo) : (string * string list) = 
        (* initVarRenameState has already set the state for the phi register *)
        let lhs: string = self#variableUse varRenameState v in 
        let rhs: string list = 
          List.map
            (fun p -> 
              self#variableUse blockEndData.(p) v)
            cfgi.S.predecessors.(s.sid)
        in
        (lhs, rhs)
      in

      pd (self#pLineDirective (get_stmtLoc s.skind));
      (* Lookup its dominator *)
      let idom: doc = 
        match Dominators.getIdom idomData s with 
          Some dom -> num dom.sid
        | None -> nil
      in

        
      ignore (p ~ind:ind
                "<stmt %d <succs %a> <preds %a> <idom %a>\n  @[%a@]\n"
                s.sid (** Statement id *)
                (d_list "," (fun _ s' -> num s'.sid)) s.succs
                (d_list "," (fun _ s' -> num s'.sid)) s.preds
                insert idom
                (docList ~sep:line 
                   (fun pv -> 
                     let (lhs, rhs) = getPhiAssignment pv in 
                     dprintf "%s = (@@phi %a);" 
                       lhs (docList ~sep:break text) rhs))
                phivars
                );
      (* Now the statement kind *)
      let ind = ind + 2 in 
      (match s.skind with 
      | Instr il -> 
          List.iter 
            (fun i -> 
              pd ~ind:ind (self#pInstr () i ++ line))
            il
      | Block b -> List.iter (self#dStmt out ind) b.bstmts
      | Goto (s, _) -> ignore (p ~ind:ind "<goto %d>\n" !s.sid)
      | Return (what, _) -> begin

          let gwt: varinfo list = 
            getGlobalsWrittenTransitive currentFundec.svar
          in
          let res: varinfo list = 
            match what with 
              None -> gwt
            | Some (Lval (Var v, NoOffset)) when v.vname = "__retres" -> 
                gwt @ [ v ]
            | Some _ -> E.s (E.bug "Return with no __retres")
          in
          ignore (p ~ind:ind
                    "return %a;"
                    (docList 
                       (fun v -> 
                         text (self#variableUse varRenameState v)))
                 res);
      end

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
          currentFundec <- fdec;

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

          blockStartData <- Array.make cfgi.S.size (-1);
          blockEndData <- Array.make cfgi.S.size (IH.create 13);
          
          lastFreshId := 0;

          freshVars <- [];
          uninitVars <- [];
          
          Array.iteri (fun i (b: S.cfgBlock) -> 
            (* compute the initial state *)
            blockStartData.(i) <- !lastFreshId;
            
            (* Initialize the renaming state *)
            self#initVarRenameState b;
          
            (* Now scan the block and keep track of the definitions. This is 
             * a huge hack. We try to rename the variables in the same order 
             * in which we will rename them during actual printing of the 
             * block. It would have been cleaner to print the names of the 
             * variables after printing the function.  *)
            (match b.S.bstmt.skind with 
              Instr il -> begin
                List.iter
                  (fun i -> 
                    match i with 
                      Set ((Var v, NoOffset), _, _)
                        when considerVariable v ->
                          ignore (self#variableDef varRenameState v)
                    | Call (Some (Var v, NoOffset), 
                            Lval (Var f, NoOffset), _, _) 
                      ->
                        let gwt: varinfo list = 
                          getGlobalsWrittenTransitive f in
                        let gwt' = 
                          if considerVariable v then 
                            gwt @ [ v ] else gwt 
                        in
                        List.iter (fun v -> 
                          ignore (self#variableDef varRenameState v))
                          gwt'


                    | Call (None, 
                            Lval (Var f, NoOffset), _, _) -> 
                          let gwt: varinfo list = 
                            getGlobalsWrittenTransitive f in
                          List.iter (fun v -> 
                            ignore (self#variableDef varRenameState v))
                            gwt

                    | _ -> ())
                  il
              end
                    
            | _ -> (* No definitions *)
                ()
            );
            
            blockEndData.(i) <- IH.copy varRenameState;
          ) 
          cfgi.S.blocks;


          (* Compute strongly connected components *)
          let scc: (int list * int list) list = 
            stronglyConnectedComponents cfgi in 

          (** For each basic block *)
        
            
        (* The header *)
        pd (self#pLineDirective ~forcefile:true l); 

        ignore (p "<function %s\n  <formals %a>\n  <globalsreadtransitive %a>\n  <globalswrittentransitive %a>\n  <locals %a>\n  <uninitlocals %a>\n  <globalsread %a>\n  <globalswritten %a>\n  <calls %a>\n  <calledby %a>\n  %a"
          fdec.svar.vname
          (docList (fun v -> text v.vname)) fdec.sformals
          (d_list "," (fun () v -> text v.vname)) 
                  (getGlobalsReadTransitive fdec.svar)
          (d_list "," (fun () v -> text v.vname)) 
                  (getGlobalsWrittenTransitive fdec.svar)
          (docList text) freshVars
          (docList text) uninitVars
          (d_list "," (fun () (_, v) -> text v.vname)) (IH.tolist glob_read)
          (d_list "," (fun () (_, v) -> text v.vname)) (IH.tolist glob_written)
          (U.docHash (fun k _ -> text k)) cg_node.CG.cnCallees
          (U.docHash (fun k _ -> text k)) cg_node.CG.cnCallers
          (docList ~sep:line
             (fun (headers, nodes) -> 
               dprintf "<SCC <headers %a> <nodes %a>>\n"
                 (docList num) headers
                 (docList num) nodes))
                  scc);


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
      Simplify.onlyVariableBasics := true;
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
          (match finfo with 
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
(*
              ignore (E.log "fixedpoint: doing %s\n read so far: %a\n written so far: %a\n"
                        fdec.svar.vname
                        (docList (fun (_, v) -> text v.vname))
                        (IH.tolist ourRead)
                        (docList (fun (_, v) -> text v.vname))
                        (IH.tolist ourRead));
*)
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
          end);

          fixedpoint ();
                
        with Queue.Empty -> ()
      in
      fixedpoint ();
      

      let absPrinter: cilPrinter = new absPrinterClass graph in 
      IH.clear globalsDumped;
      iterGlobals f
        (arithAbs absPrinter);

      (* compute SCC for the call-graph *)
      let nodeIdToNode: CG.callnode IH.t = IH.create 13 in
      let funidToNodeId: int IH.t = IH.create 13 in 
      let nrNodes = ref 0 in 
      let mainNode = ref 0 in 
      H.iter 
        (fun vn cn -> 
          if vn= "main" then mainNode := !nrNodes;
          IH.add nodeIdToNode !nrNodes cn;
          IH.add funidToNodeId cn.CG.cnInfo.vid !nrNodes;
          incr nrNodes) graph;

      let ci: S.cfgInfo = 
        { S.name = "call-graph";
          S.start = !mainNode;
          S.size = !nrNodes;
          S.successors = Array.make !nrNodes [];
          S.predecessors = Array.make !nrNodes [];
          S.blocks = Array.make !nrNodes { S.bstmt = mkEmptyStmt ();
                                           S.instrlist = [];
                                           S.livevars = [] };
         S.nrRegs = 0;
         S.regToVarinfo = Array.create 0 dummyFunDec.svar;
        }
      in
      nrNodes := 0;
      IH.iter (fun idx cn -> 
        let cnlistToNodeList (cnl: (string, CG.callnode) H.t) : int list = 
          List.map 
            (fun (_, sn) -> 
              try IH.find funidToNodeId sn.CG.cnInfo.vid
              with Not_found -> assert false
            )
            (U.hash_to_list cnl)
        in
        ci.S.successors.(idx) <- cnlistToNodeList cn.CG.cnCallees;
        ci.S.predecessors.(idx) <- cnlistToNodeList cn.CG.cnCallers; 
           
        ) nodeIdToNode;

      let scc: (int list * int list) list = 
        stronglyConnectedComponents ci in 
      List.iter 
        (fun (headers, nodes) -> 
          ignore (p "<SCC <headers %a> <nodes %a>>\n"
                    (docList 
                       (fun n -> 
                         (try text (IH.find nodeIdToNode n).CG.cnInfo.vname
                         with Not_found -> assert false)))
                    headers
                    (docList 
                       (fun n -> 
                         (try text (IH.find nodeIdToNode n).CG.cnInfo.vname
                         with Not_found -> assert false)))
                    nodes))
        scc;
   );
        
        
      

    fd_post_check = false;
  } 
