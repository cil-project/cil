(*
 * Simplemem: Transform a program so that all memory expressions are
 * "simple". Introduce well-type temporaries to hold intermediate values
 * for expressions that would normally involve more than one memory
 * reference. 
 *
 * If simplemem succeeds, each lvalue should contain only one Mem()
 * constructor. 
 *)
open Cil

(* current context: where should we put our temporaries? *)
let thefunc = ref None 

(* build up a list of assignments to temporary variables *)
let assignment_list = ref (ref [])

(* turn "int a[5][5]" into "int ** temp" *)
let rec array_to_pointer tau = 
  match unrollType tau with
    TArray(dest,_,al) -> TPtr(array_to_pointer dest,al)
  | _ -> tau

(* create a temporary variable in the current function *)
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

(* Recursively decompose the lvalue so that what is under a "Mem()"
 * constructor is put into a temporary variable. *)
let rec handle_lvalue (lb,lo) = 
  let s,m = separate_loffsets lo in 
  match lb with
    Var(vi) -> 
      handle_loffset (lb,s) m 
  | Mem(Lval(Var(_),NoOffset)) ->
			(* special case to avoid generating "tmp = ptr;" *)
      handle_loffset (lb,s) m 
  | Mem(e) -> 
      begin
        let new_vi = make_temp (typeOf e) in
        !assignment_list := (new_vi, e, !currentLoc) 
          :: ! (!assignment_list) ;
        handle_loffset (Mem(Lval(Var(new_vi),NoOffset)),NoOffset) lo
      end
and handle_loffset lv lo = 
  match lo with
    NoOffset -> lv
  | Field(f,o) -> 
      handle_loffset (addOffsetLval (Field(f,NoOffset)) lv) o
  | Index(exp,o) -> 
      handle_loffset (addOffsetLval (Index(exp,NoOffset)) lv) o

(* the transformation is implemented as a Visitor *)
class simpleVisitor = object 
  inherit nopCilVisitor

  method vfunc fundec = (* we must record the current context *)
    thefunc := Some(fundec) ;
    DoChildren

  method vlval lv = 
    ChangeDoChildrenPost(lv,
      (fun lv -> handle_lvalue lv))

  method vinst i = 
		(* this "my_alist" idiocy is to make sure that temporary statements
		 * that get generated to deal with, e.g., the predicate of an
		 * 'if' statement get put before the 'if' statement and not inside
		 * one of the branches *)
		let my_alist = ref [] in
		assignment_list := my_alist ;
    ChangeDoChildrenPost([i],
      (fun i_list -> 
        let new_instr_list = List.map (fun (vi,e,loc) ->
          Set((Var(vi),NoOffset),e,loc)) 
            (List.rev !my_alist) in
        new_instr_list @ i_list))

  method vstmt s = 
		let my_alist = ref [] in
		assignment_list := my_alist ;
    ChangeDoChildrenPost(s,
      (fun s -> 
        if !my_alist = [] then s
        else begin
          let new_instr_list = List.map (fun (vi,e,loc) ->
            Set((Var(vi),NoOffset),e,loc)) 
              (List.rev !my_alist) in
          let new_stmt = mkStmt (Instr(new_instr_list)) in 
          let new_block = mkBlock ([new_stmt ; s ;]) in
          mkStmt (Block(new_block))
        end
    ))
end

(* Main entry point: apply the transformation to a file *)
let simplemem (f : file) =
  try 
    visitCilFile (new simpleVisitor) f
  with e -> Printf.printf "Exception in Simplemem.simplemem: %s\n"
    (Printexc.to_string e) ; raise e
