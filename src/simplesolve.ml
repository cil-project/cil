(*
 * A model the implements simple constraint solving for the
 * pointer-qualifier graph. Assigning "wild" to everything is a trivial
 * solution: we'll try to do better than that.
 *
 * Main idea: everything starts out "safe" and we use a worklist algorithm
 * to push updates around the graph, eventually polluting things toward
 * "wild". 
 *)
open Ptrnode

let solve (node_ht : (int,node) Hashtbl.t) = begin
	let all : node list ref = ref [] in
	Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;

	(* this is our worklist of nodes to visit *)
	let worklist = all in

	(* add a node to the worklist unless it is already there *)
	let add_node n = begin
		if not (List.mem n !worklist) then 
			worklist := n :: !worklist
	end in

	(* add all of the successors of teh given node *)
	let add_succ n = List.iter (fun e -> add_node e.eto) n.succ in
	let add_pred n = List.iter (fun e -> add_node e.efrom) n.pred in

	(* reminder: in "8 = 12", we have that 8's predecessor is 12 *)
	while (!worklist <> []) do
		(* pick out our current node *)
		let cur = List.hd !worklist in

		(* if y is used in a write operation and we have x=y, then x has also
		 * been used in a write operation *)
		if (cur.updated) then begin
			(* mark all of the successors of y with "updated" *)
			List.iter (fun e -> 
				if not e.eto.updated then begin
					e.eto.updated <- true ;
					add_node e.eto
				end) cur.succ
		end ;

		worklist := List.tl !worklist 
	done
end

