(*
 *
 * Copyright (c) 2004, 
 *  Jeremy Condit       <jcondit@cs.berkeley.edu>
 *  George C. Necula    <necula@cs.berkeley.edu>
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

let debug : bool ref = ref false
let verbose : bool ref = ref false
let suppress : bool ref = ref false

let curFunc : fundec ref = ref dummyFunDec

let compareTypes (t1 : typ) (t2 : typ) : bool =
  (typeSig t1) = (typeSig t2)

let checkTypes (t1 : typ) (t2 : typ) : unit =
  if not (compareTypes t1 t2) then
    E.log "%a: type mismatch\n" d_loc !currentLoc

let rec checkExp (e : exp) : typ =
  match e with
  | Lval lv -> checkLval lv
  | CastE (t, e') -> checkTypes t (checkExp e'); t
  | _ -> typeOf e

and checkLval (lv : lval) : typ =
  typeOfLval lv

let checkInstr (instr : instr) : unit =
  currentLoc := get_instrLoc instr;
  match instr with
  | Call (lvo, fn, args, _) ->
      begin
        match checkExp fn with
        | TFun (returnType, argInfo, varags, _) ->
            let lvType =
              match lvo with
              | Some lv -> checkLval lv
              | None -> voidType
            in
            checkTypes returnType lvType;
            List.iter2
              (fun (argName, argType, _) arg ->
                 checkTypes argType (checkExp arg))
              (argsToList argInfo)
              args
        | _ -> E.log "%a: calling non-function type\n" d_loc !currentLoc
      end
  | Set (lv, e, _) ->
      checkTypes (checkLval lv) (checkExp e)
  | Asm _ -> E.s (E.unimp "asm unsupported\n")

let rec checkStmt (s : stmt) : unit =
  currentLoc := get_stmtLoc s.skind;
  match s.skind with
  | Instr instrs ->
      List.iter checkInstr instrs
  | Return (eo, _) ->
      begin
        match eo with
        | Some e ->
            let returnType =
              match !curFunc.svar.vtype with
              | TFun (returnType, _, _, _) -> returnType
              | _ -> E.s (E.bug "expected function type")
            in
            checkTypes returnType (checkExp e)
        | None -> ()
      end
  | If (e, b1, b2, _) ->
      checkTypes intType (checkExp e);
      checkBlock b1;
      checkBlock b2;
  | Switch (e, b, _, _) ->
      checkTypes intType (checkExp e);
      checkBlock b
  | Loop (b, _, _, _)
  | Block b -> checkBlock b
  | Goto _
  | Break _
  | Continue _ -> ()
  | TryFinally _
  | TryExcept _ -> E.s (E.unimp "exceptions not supported\n")

and checkBlock (b : block) : unit =
  List.iter checkStmt b.bstmts

let checkFundec (fd : fundec) : unit =
  curFunc := fd;
  checkBlock fd.sbody;
  curFunc := dummyFunDec

let checkFile (f : file) : unit =
  List.iter
    (fun global ->
       match global with
       | GFun (fd, _) -> checkFundec fd
       | _ -> ())
    f.globals

let feature : featureDescr = 
  { fd_name = "Deputy";
    fd_enabled = ref false;
    fd_description = "Typecheck and instrument the program using Deputy.";
    fd_extraopt = [
      "--deputyverbose", Arg.Set verbose, "Enable verbose output for Deputy";
      "--deputysuppress", Arg.Set suppress, "Suppress some Deputy warnings";
    ];
    fd_doit = checkFile;
    fd_post_check = true;
  } 
