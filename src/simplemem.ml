(*
 * Simplemem: Transform a program so that all memory expressions are
 * "simple". Introduce well-type temporaries to hold intermediate values
 * for expressions that would normally involve more than one memory
 * reference. 
 *)
open Cil

let thefunc = ref None 

let assignment_list = ref [] 

let target_type tau = 
  match unrollType tau with
  | TPtr(dest,_) -> dest
  | TArray(dest,_,_) -> dest
  | _ -> tau

let rec array_to_pointer tau = 
  match unrollType tau with
    TArray(dest,_,al) -> TPtr(array_to_pointer dest,al)
  | _ -> tau

let make_temp tau = 
  let tau = array_to_pointer tau in 
  match !thefunc with
    Some(fundec) -> makeTempVar fundec ~name:"mem_temp" tau
  | None -> failwith "simplemem: temporary needed outside a function"

(* separate loffsets into "scalar addition parts" and "memory parts" *)
let rec separate_loffsets lo = 
  match lo with
    NoOffset -> NoOffset, NoOffset
  | Field(fi,rest) -> 
      let s,m = separate_loffsets rest in
      Field(fi,s) , m
  | Index(_) -> NoOffset, lo

let rec handle_lvalue (lb,lo) = 
  let s,m = separate_loffsets lo in 
  match lb with
    Var(vi) -> 
      handle_loffset (lb,s) m 
  | Mem(Lval(Var(_),NoOffset)) -> 
      handle_loffset (lb,s) m 
  | Mem(e) -> 
      begin
        let new_vi = make_temp (typeOf e) in
        assignment_list := (new_vi, e, !currentLoc) 
          :: ! assignment_list ;
        handle_loffset (Mem(Lval(Var(new_vi),NoOffset)),NoOffset) lo
      end
and handle_loffset lv lo = 
  match lo with
    NoOffset -> lv
  | Field(f,o) -> 
      handle_loffset (addOffsetLval (Field(f,NoOffset)) lv) o
  | Index(exp,o) -> 
      handle_loffset (addOffsetLval (Index(exp,NoOffset)) lv) o

class simpleVisitor = object
  inherit nopCilVisitor

  method vfunc fundec =
    thefunc := Some(fundec) ;
    DoChildren

  method vlval lv = 
    (* let res = handle_lvalue lv in *)
    ChangeDoChildrenPost(lv,
      (fun lv -> handle_lvalue lv))

  method vinst i = 
    ChangeDoChildrenPost([i],
      (fun i_list -> 
        let new_instr_list = List.map (fun (vi,e,loc) ->
          Set((Var(vi),NoOffset),e,loc)) 
            (List.rev !assignment_list) in
        assignment_list := [] ;
        new_instr_list @ i_list))

  method vstmt s =
    ChangeDoChildrenPost(s,
      (fun s -> 
        if !assignment_list = [] then s
        else begin
          let new_instr_list = List.map (fun (vi,e,loc) ->
            Set((Var(vi),NoOffset),e,loc)) 
              (List.rev !assignment_list) in
          assignment_list := [] ;
          let new_stmt = mkStmt (Instr(new_instr_list)) in 
          let new_block = mkBlock ([new_stmt ; s ;]) in
          mkStmt (Block(new_block))
        end
    ))
end

let simplemem (f : file) =
  try 
    visitCilFile (new simpleVisitor) f
  with e -> Printf.printf "Error in simplemem: %s"
    (Printexc.to_string e) ; f
