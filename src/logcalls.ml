open Pretty
open Cil
module E = Errormsg
module H = Hashtbl



let logCalls (f: file) : unit = 
  (* Create a prototype for the logging function *)
  let printfFun =   
    let fdec = emptyFunction "printf" in
    let argf  = makeLocalVar fdec "format" charConstPtrType in
    fdec.svar.vtype <- TFun(intType, [ argf ], true, []);
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
              | _ -> (accargs,formatstr))
            ([], ")\n")
            fdec.sformals
        in
        fdec.sbody <- 
           mkStmt (Instr [Call (None, Lval(var printfFun.svar),
                                ( Const(CStr("call to " ^ fdec.svar.vname ^ 
                                             "(" ^ formatstr)) :: actargs),
                                loc)]) :: fdec.sbody

    | _ -> ()
  in
  Stats.time "logCalls" (iterGlobals f) doGlobal
