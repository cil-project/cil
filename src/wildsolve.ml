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

(* turn a pointer kind into its table equivalent *) 
let table_this_node n = 
  let newkind = 
    match n.kind with
      Seq -> SeqT
    | FSeq -> FSeqT
    | SeqN -> SeqNT
    | FSeqN -> FSeqNT
    | Index -> IndexT
    | Wild -> WildT
    | x -> x
  in n.kind <- newkind

(* table every node in the graph *)
let table_it_all (node_ht : (int,node) Hashtbl.t) = begin
  Hashtbl.iter (fun id n -> 
    table_this_node n ;
  ) node_ht 
end

(* table all the interface nodes in the graph *)
let table_interface (node_ht : (int,node) Hashtbl.t) = begin
  let finished = ref false in 
  while (not !finished) do
    finished := true ; 
    Hashtbl.iter (fun id n -> 
      if n.interface then begin
        table_this_node n ;
      end ;
      if (is_table_kind n.kind) then begin
        List.iter (fun e ->
          if e.ekind = ECompat && not (is_table_kind e.eto.kind) then begin
            table_this_node e.eto; 
            finished := false 
          end) n.succ ;
        List.iter (fun e ->
          if e.ekind = ECompat && not (is_table_kind e.efrom.kind) then begin
            table_this_node e.efrom; 
            finished := false 
          end) n.pred ;
      end
    ) node_ht ; 
  done
end

