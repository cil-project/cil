open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let i = ref 0 
let name = ref ""

(* instrument every instruction! aie! *)
class verboseLogVisitor printfFun funstr = object
  inherit nopCilVisitor 
  (*
  method vinst (inst  : instr) = begin
    let str = Printf.sprintf "%s::%d\n" !name !i in
    incr i ; 
    let newinst = ((Call (None, Lval(var printfFun.svar),
                                ( [ one ; Const(CStr(str)) ]),
                                locUnknown)) : instr )in
    let ilist = ([ inst ; newinst ] : instr list) in
    (ChangeTo(ilist))
  end *)
  method vinst i = begin
    match i with
      Call(lo,e,al,l) -> 
      let str1 = Pretty.sprint 800 ( Pretty.dprintf "Calling %a(%a)\n" d_exp e
        (docList (chr ',') (fun _ -> text "%p")) al) in
      let str2 = Pretty.sprint 800 ( Pretty.dprintf "Returned from %a\n" d_exp e) in
      let newinst str args = ((Call (None, Lval(var printfFun.svar),
                                ( [ one ; Const(CStr(str)) ] @ args),
                                locUnknown)) : instr )in
      let ilist = ([ (newinst str1 al) ; i ; (newinst str2 []) ] : instr list) in
    (ChangeTo(ilist))
    | _ -> DoChildren 
  end
  method vstmt (s : stmt) = begin
    match s.skind with
      Return(Some(e),l) ->
      let str = Pretty.sprint 800 ( Pretty.dprintf
        "Return(%%p) from %s\n" funstr ) in
      let newinst = ((Call (None, Lval(var printfFun.svar),
                                ( [ one ; Const(CStr(str)) ; e ]),
                                locUnknown)) : instr )in
      let new_stmt = mkStmtOneInstr newinst in 
      let slist = [ new_stmt ; s ] in 
      (ChangeTo(mkStmt(Block(mkBlock slist))))
    | Return(None,l) ->
      let str = Pretty.sprint 800 ( Pretty.dprintf
        "Return void from %s\n" funstr ) in
      let newinst = ((Call (None, Lval(var printfFun.svar),
                                ( [ one ; Const(CStr(str)) ]),
                                locUnknown)) : instr )in
      let new_stmt = mkStmtOneInstr newinst in 
      let slist = [ new_stmt ; s ] in 
      (ChangeTo(mkStmt(Block(mkBlock slist))))
    | _ -> DoChildren
  end
end


let logCalls (f: file) : unit = 
  (* Create a prototype for the logging function *)
  let printfFun =   
    let fdec = emptyFunction "syslog" in
    let argi  = makeLocalVar fdec "prio" intType in
    let argf  = makeLocalVar fdec "format" charConstPtrType in
    fdec.svar.vtype <- TFun(intType, [ argi ; argf ], true, []);
    fdec
  in
  
  let doGlobal = function
      GFun (fdec, loc) -> 
        (* Collect expressions that denote the actual arguments, and a format 
         * string for printing them *)
        let actargs, formatstr = 
          List.fold_left
            (fun (accargs, formatstr) f -> 
              match unrollType f.vtype with
                (* Log only the integer and floating point arguments *)
                TInt _ | TEnum _ -> 
                  (Lval (var f) :: accargs, "%d, " ^ formatstr)
              | TFloat _ -> 
                  (Lval (var f) :: accargs, "%f, " ^ formatstr)
              | _ -> 
                  (Lval (var f) :: accargs, "%p, " ^ formatstr))
            ([], ")\n")
            fdec.sformals
        in
        i := 0 ;
        name := fdec.svar.vname ; 
        let thisVisitor = new verboseLogVisitor printfFun !name in 
        fdec.sbody <- visitCilBlock thisVisitor fdec.sbody; 
        fdec.sbody.bstmts <- 
              mkStmt (Instr [Call (None, Lval(var printfFun.svar),
                                ( one :: Const(CStr("call to " ^
                                fdec.svar.vname ^ "(" ^ formatstr)) ::
                                actargs),
                                loc)]) :: fdec.sbody.bstmts

    | _ -> ()
  in
  Stats.time "logCalls" (iterGlobals f) doGlobal
