(*
 * My fourth attempt at constraint solving for the pointer-qualifier graph.
 *)

open Cil
open Ptrnode

let wild_solve (node_ht : (int,node) Hashtbl.t) = begin
  Hashtbl.iter (fun id n -> 
    if n.kind <> ROString || n.why_kind <> PrintfArg then begin
      n.kind <- Wild ; 
      n.why_kind <- Default 
    end
  ) node_ht 
end

let wild_safe_solve (node_ht : (int,node) Hashtbl.t) = begin
  Thirdsolve.solve node_ht ; 
  Hashtbl.iter (fun id n -> 
    if n.kind <> Safe && 
      (n.kind <> ROString || n.why_kind <> PrintfArg) then begin
      n.kind <- Wild 
    end
  ) node_ht 
end

let table_it_all (node_ht : (int,node) Hashtbl.t) = begin
  Hashtbl.iter (fun id n -> 
    if n.interface then 
    let newkind = 
      match n.kind with
        Seq -> SeqT
      | FSeq -> FSeqT
      | SeqN -> SeqNT
      | FSeqN -> FSeqNT
      | Index -> IndexT
      | Wild -> WildT
      | x -> x
    in 
    n.kind <- newkind
  ) node_ht 
end

