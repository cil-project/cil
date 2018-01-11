open Cil

let todo = raise (Failure "todo")

let rec flatten_if (cond, xen, ilse, loc) =
  todo

let flatten_stm {labels; skind; sid; succs; preds} =
  match skind with
  | If (cond, xen, ilse, loc) -> todo
  | Instr (_)
    | Return (_, _)
    | Goto (_, _)
    | ComputedGoto (_, _)
    | Break (_)
    | Continue (_)
    | Switch (_, _, _, _)
    | Loop (_, _, _, _)
    | Block (_)
    | TryFinally (_, _, _)
    | TryExcept (_, _, _, _) -> 
     {labels = labels; skind = skind; sid = sid; succs = succs; preds = preds}
    

let flatten_block {battrs; bstmts} = todo



  
let trans_stmt {labels; skind; sid; succs; preds} =
  match skind with
  | If (cond, xen, ilse, loc) -> todo
  | Instr (_)
    | Return (_, _)
    | Goto (_, _)
    | ComputedGoto (_, _)
    | Break (_)
    | Continue (_)
    | Switch (_, _, _, _)
    | Loop (_, _, _, _)
    | Block (_)
    | TryFinally (_, _, _)
    | TryExcept (_, _, _, _) ->
     ([]
     , []
     ,{labels = labels; skind = skind; sid = sid; succs = succs; preds = preds})

let trans_block {battrs; bstmts} =
  let r = List.map trans_stmt bstmts in
  let func = List.flatten (List.map (fun (x, _, _) -> x) r) in
  let locals = List.flatten (List.map (fun (_, y, _) -> y) r) in
  let b = List.map (fun (_, _, z) -> z) r in
  (func, locals, {battrs = battrs; bstmts = b})


let trans_function {svar
                  ; sformals
                  ; slocals
                  ; smaxid
                  ; sbody
                  ; smaxstmtid
                  ; sallstmts} loc =
  let (func, locals, b) = trans_block sbody in
  let f = GFun({svar = svar
               ; sformals = sformals
               ; slocals = List.append slocals locals
               ; smaxid = smaxid
               ; sbody = b
               ; smaxstmtid = smaxstmtid
               ; sallstmts = sallstmts}, loc) in
  List.rev (f :: func)
                                               
  
  
let trans_global t =
  match t with
  | GFun(x, y) -> trans_function x y
  | _ -> [t]
       
let trans_file {fileName; globals; globinit; globinitcalled} =
  let x = List.map trans_global globals in
  {fileName = fileName
  ; globals = List.flatten x
  ; globinit = globinit
  ; globinitcalled = globinitcalled}
  
  
let doit t = trans_file t
