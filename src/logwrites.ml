open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

class logWriteVisitor = object
  inherit nopVisitor
  (* Create a prototype for the logging function, but don't put it in the 
   * file *)
  val printfFun =   
    let fdec = emptyFunction "printf" in
    let argf  = makeLocalVar fdec "format" charConstPtrType in
    fdec.svar.vtype <- TFun(intType, [ argf ], true, []);
    fdec

  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Set(lv, e, l) -> begin
        (* Check if we need to log *)
        match lv with 
          Var(v, off) when not v.vglob -> SkipChildren
        | _ -> ChangeTo 
              [ Call(None, Lval(var printfFun), 
                     [ CStr "Write to 0x%08x at %s:%d\n";
                       AddrOf lv; CStr l.file; integer l.line]); i]
      end 
    | _ -> SkipChildren

end

let logWrites (f: file) : file = 
  let lwVisitor = new logWriteVisitor in
  visitCilFile lwVisitor
