open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let i = ref 0 
let name = ref ""

(* instrument every instruction! aie! *)
class verboseLogVisitor printfFun = object
  inherit nopCilVisitor 
  method vinst (inst  : instr) = begin
    let str = Printf.sprintf "%s::%d\n" !name !i in
    incr i ; 
    let newinst = ((Call (None, Lval(var printfFun.svar),
                                ( [ one ; Const(CStr(str)) ]),
                                locUnknown)) : instr )in
    let ilist = ([ inst ; newinst ] : instr list) in
    (ChangeTo(ilist))
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
  let thisVisitor = new verboseLogVisitor printfFun in 
  
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
              | _ -> (accargs,formatstr))
            ([], ")\n")
            fdec.sformals
        in
        i := 0 ;
        name := fdec.svar.vname ; 
        visitCilBlock thisVisitor fdec.sbody ; 
        fdec.sbody.bstmts <- 
              mkStmt (Instr [Call (None, Lval(var printfFun.svar),
                                ( one :: Const(CStr("call to " ^
                                fdec.svar.vname ^ "(" ^ formatstr)) ::
                                actargs),
                                loc)]) :: fdec.sbody.bstmts

    | _ -> ()
  in
  Stats.time "logCalls" (iterGlobals f) doGlobal
