(*
 * My third attempt at constraint solving for the pointer-qualifier graph.
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

