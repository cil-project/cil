open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

class logWriteVisitor = object
  inherit nopCilVisitor
  (* Create a prototype for the logging function, but don't put it in the 
   * file *)
  val printfFun =   
    let fdec = emptyFunction "syslog" in
    let argp  = makeLocalVar fdec "prio" intType in
    let argf  = makeLocalVar fdec "format" charConstPtrType in
    fdec.svar.vtype <- TFun(intType, [ argp ; argf ], true, []);
    fdec

  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Set(lv, e, l) -> begin
        (* Check if we need to log *)
        match lv with 
          (Var(v), off) when not v.vglob -> SkipChildren
        | _ -> let str = Pretty.sprint 80 
                (Pretty.dprintf "Write %%p to 0x%%08x at %%s:%%d (%a)\n" d_lval lv)
              in 
              ChangeTo 
              [ Call((None), (Lval(Var(printfFun.svar),NoOffset)), 
                     [ one ; Const(CStr str) ; e ; AddrOf lv; Const(CStr
                     l.file); integer l.line], locUnknown);
              i]
      end 
    | Call(Some lv, f, args, l) -> begin
        (* Check if we need to log *)
        match lv with 
          (Var(v), off) when not v.vglob -> SkipChildren
        | _ -> let str = Pretty.sprint 80 
                (Pretty.dprintf "Write retval to 0x%%08x at %%s:%%d (%a)\n" d_lval lv)
              in 
              ChangeTo 
              [ Call((None), (Lval(Var(printfFun.svar),NoOffset)), 
                     [ one ; Const(CStr str) ; AddrOf lv; Const(CStr
                     l.file); integer l.line], locUnknown);
              i]
      end 
    | _ -> SkipChildren

end

let logWrites (f: file) : unit = 
  let lwVisitor = new logWriteVisitor in
  ignore (visitCilFile lwVisitor f)
