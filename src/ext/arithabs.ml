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

(** Keep track of the globals we have dumped in this file *)
let globalsDumped: unit IH.t = IH.create 13

(** have a visitor for preprocessing the function bodies *)
let globalsRead: varinfo IH.t = IH.create 13
let globalsWritten: varinfo IH.t = IH.create 13

let considerGlobal (v: varinfo) : bool = 
  v.vglob && not v.vaddrof && isIntegralType v.vtype

class preVisitorClass : cilVisitor = object (self) 
  inherit nopCilVisitor

  method vexpr = function
      Lval (Var v, _) when considerGlobal v -> 
        IH.replace globalsRead v.vid v;
        DoChildren

    | _ -> DoChildren

  method vinst = function
      Set ((Var v, _), _, _) 
    | Call (Some (Var v, _), _, _, _) when considerGlobal v -> 
        IH.replace globalsWritten v.vid v;
        DoChildren
    | _ -> DoChildren
end

let preVisitor = new preVisitorClass

(** We define a new printer *)
class absPrinterClass (callgraph: CG.callgraph) : cilPrinter = object (self) 
  inherit defaultCilPrinterClass as super

  method pExp () = function
    | Const (CInt64(i, _, _)) -> text (Int64.to_string i)
    | BinOp (bop, e1, e2, _) -> 
        dprintf "(%a @[%a@?%a@])" 
          d_binop bop
          self#pExp e1 self#pExp e2
    | UnOp (uop, e1, _) -> 
        dprintf "(%a @[%a@])"
          d_unop uop self#pExp e1
    | CastE (t, e) -> self#pExp () e
    | Lval (Mem _, _) -> text "(rand)"
    | Lval (Var v, _) when v.vaddrof -> text "(rand)"
    | e -> super#pExp () e

  method pInstr () = function
      Set ((Mem _, _), _, _)  (* ignore writes *) 
    | Asm _ -> nil
    | Set ((Var v, _), _, _) when v.vaddrof -> nil
    | Call (Some (Mem _, _), f, args, loc) -> 
        super#pInstr () (Call (None, f, args, loc))
    | Call (None, f, args, _) -> 
        dprintf "(%a @[%a@])" 
          self#pExp f
          (docList break (self#pExp ())) args
    | Call (Some v, f, args, _) -> 
        dprintf "%a = (%a @[%a@])" 
          self#pExp (Lval v)
          self#pExp f
          (docList break (self#pExp ())) args

    | i -> super#pInstr () i


  method dBlock (out: out_channel) (ind: int) (b: block) : unit = 
    ignore (p ~ind:ind "<block\n");
    List.iter (self#dStmt out (ind+ 2)) b.bstmts;
    ignore (p ~ind:ind ">\n")

  method dStmt (out: out_channel) (ind: int) (s: stmt) : unit = 
    pd (self#pLineDirective (get_stmtLoc s.skind));
    ignore (p ~ind:ind
              "<stmt %d <succs %a> <preds %a>\n"
              s.sid (** Statement id *)
              (d_list "," (fun _ s' -> num s'.sid)) s.succs
              (d_list "," (fun _ s' -> num s'.sid)) s.preds);
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
        pd (self#pLineDirective ~forcefile:true l);
        prepareCFG fdec;
        ignore (computeCFGInfo fdec true);
        let cg_node: CG.callnode = 
          try H.find callgraph fdec.svar.vname
          with Not_found -> E.s (E.bug "Cannot find call graph info for %s"
                                   fdec.svar.vname)
        in
        IH.clear globalsRead;
        IH.clear globalsWritten;
        ignore (visitCilBlock preVisitor fdec.sbody);

        (* The header *)
        ignore (p "<function %s\n  <formals %a>\n  <locals %a>\n  <globalsread %a>\n  <globalswritten %a>\n  <calls %a>\n  <calledby %a>\n"
          fdec.svar.vname
          (d_list "," (fun () v -> text v.vname)) fdec.sformals
          (d_list "," (fun () v -> text v.vname)) fdec.slocals
          (d_list "," (fun () (_, v) -> text v.vname)) (IH.tolist globalsRead)
          (d_list "," (fun () (_, v) -> text v.vname)) (IH.tolist globalsWritten)
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
      (* Compute the call graph *)
      let graph = CG.computeGraph f in 
      (* Call the simplify *)
      Simplify.feature.fd_doit f;

      let absPrinter: cilPrinter = new absPrinterClass graph in 
      IH.clear globalsDumped;
      iterGlobals f
        (arithAbs absPrinter););
    fd_post_check = false;
  } 
