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

	(* add all of the successors of the given node *)
	let add_succ n = List.iter (fun e -> add_node e.eto) n.succ in
	let add_pred n = List.iter (fun e -> add_node e.efrom) n.pred in

	(* Setup:
	 *				int *x,*y,*z;
	 *				x = y;
	 *				y = z;
	 *
	 * Gives:
	 * 				[x] <- [y] <- [z]
	 *)
	while (!worklist <> []) do 
		(* pick out our current node *)
		let cur = List.hd !worklist in

		(* In the above example, if we write through y we have also written
		 * through z. Imagine that y is the formal parameter to a function that
		 * has been declared "readonly". We cannot "get around the readonly" by
		 * assigning y=z and writing through y. *)
		if (cur.updated) then begin
			(* mark all of the predecessors of y with "updated" *)
			List.iter (fun e -> 
				if not e.efrom.updated then begin
					e.efrom.updated <- true ;
					add_node e.efrom
				end) cur.pred
		end ;

		(* Now imagine that y might contain a stack address. In that case, x 
		 * may also contain a stack address. *)
		if (cur.onStack) then begin
			(* mark all of the successors of y with "updated" *)
			List.iter (fun e -> 
				if not e.eto.onStack then begin
					e.eto.onStack <- true ;
					add_node e.eto
				end) cur.succ
		end ;

		(* Now imagine that the number zero might be stored in y. Then the
		 * zero might be stored in x as well. *)
		if (cur.null) then begin
			(* mark all of the successors of y with "null" *)
			List.iter (fun e -> 
				if not e.eto.null then begin
					e.eto.null <- true ;
					add_node e.eto
				end) cur.succ
		end ;

		(* Similarly, if some int might be stored in y. Then
		 * some in might be stored in x as well. *)
		if (cur.intcast) then begin
			(* mark all of the successors of y with "intcast" *)
			List.iter (fun e -> 
				if not e.eto.intcast then begin
					e.eto.intcast <- true ;
					add_node e.eto
				end) cur.succ
		end ;

		(* Now, if we have y[e], we'll probably want to represent y as an index
		 * or wild pointer. That does not mean that x or z have to be index or
		 * wild pointers -- we could have some sort of magical cast. But in
		 * this simple solver, we'll try to assign qualifiers so that we do not
		 * have such casts. So if y requires positive arithmetic, then so do x
		 * and z. *)
		if (cur.posarith) then begin
			(* mark all of the successors of y with "posarith" *)
			List.iter (fun e -> 
				if not e.eto.posarith then begin
					e.eto.posarith <- true ;
					add_node e.eto
				end) cur.succ ;
			List.iter (fun e -> 
				if not e.efrom.posarith then begin
					e.efrom.posarith <- true ;
					add_node e.efrom
				end) cur.pred
		end ;

		(* Similar deal here for arbitrary arithmetic. *)
		if (cur.arith) then begin
			(* mark all of the successors of y with "arith" *)
			List.iter (fun e -> 
				if not e.eto.arith then begin
					e.eto.arith <- true ;
					add_node e.eto
				end) cur.succ ;
			List.iter (fun e -> 
				if not e.efrom.arith then begin
					e.efrom.arith <- true ;
					add_node e.efrom
				end) cur.pred
		end ;

		worklist := List.tl !worklist 
	done ;

	(* OK, now we have all of those attributes propagated. Now it's time to
	 * assign qualifiers to the pointers. *)

	(* Step 1: Find all of the "naturally wild" pointers. spread wildness to
	 * all and sundry. *)
	Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;
	while (!worklist <> []) do
		(* pick out our current node *)
		let cur = List.hd !worklist in
		if (cur.intcast && cur.kind <> Wild) then begin
			cur.kind <- Wild ;
			(* mark all of the succ/pred of y with "wild" *)
			let contaminated_list = cur.pointsto @
				(List.map (fun e -> e.eto) cur.succ) @
				(List.map (fun e -> e.efrom) cur.pred) in
			List.iter (fun n ->
				if not (n.kind = Wild) then begin
					n.kind <- Wild ;
					add_node n
				end) contaminated_list
		end ;
		worklist := List.tl !worklist 
	done ;

	(* Step 2: Find all of the "naturally index" pointers. spread index to
	 * all and sundry. *)
	Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;
	while (!worklist <> []) do
		(* pick out our current node *)
		let cur = List.hd !worklist in
		if ((cur.arith || cur.posarith) && cur.kind = Unknown) then begin
			cur.kind <- Index ;
			(* mark all of the succ/pred of y with "index" *)
			let contaminated_list = 
				(List.map (fun e -> e.eto) cur.succ) @
				(List.map (fun e -> e.efrom) cur.pred) in
			List.iter (fun n ->
				if (n.kind = Unknown) then begin
					n.kind <- Index ;
					add_node n
				end) contaminated_list
		end ;
		worklist := List.tl !worklist 
	done ;

	(* Step 3: everything else is "safe" *)
	Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;
	while (!worklist <> []) do
		(* pick out our current node *)
		let cur = List.hd !worklist in
		if (cur.kind = Unknown) then begin cur.kind <- Safe end ;
		worklist := List.tl !worklist 
	done

end

