(* Weimer
 *
 * CCUred pointer kind inference using an extension of the algorithm
 * described in the paper submitted to PLDI'02. 
 *
 * INPUT ASSUMPTIONS: 
 *
 * The node hashtable includes the following edges:
 *      n1 ECAST n2     for the top-level nodes of casts
 *      n1 ENULL n2     cast or assignment where value of exp of n1 is 0 
 *      n1 ESAFE n2     from the top of a struct to its non-array fields
 *      n1 EINDEX n2    from the top of a struct to array fields
 *
 * Nodes have the following bits set iff the corresponding fact actually
 * originate at that node: 
 *      pkPosArith
 *      pkArith
 *      pkNull
 *      pkUpdated
 *      pkIntCast
 *      pkInterface
 *      pkSized
 *
 * The "is_p x y" function returns true if x--cast-->y is from a polymorphic
 * function and should not be considered. 
 *
 * Fri Dec 14 13:46:04 PST 2001
 *)

(*
 * How does the solver work? 
 *
 * (1) We examine every CAST in the program. While scanning the
 *			types to see if the cast is valid we add ECompat edges between
 *			inner types that must be equal and mark as Wild types that
 *			do not match up. We also mark nodes as Not Safe in casts where
 *			the sizes are wrong. 
 * (2) We assign base-case flags. For example, we assign
 *      "pkReachString" to nodes that are of type string.
 * (3) We push those flags around using standard data-flow tricks. Nodes
 *      connected by ECompat edges have identical flags. 
 * (4) Once we have all the flags in place we distinguish between all
 *      the kinds of sequences. For example, a node that cannot be safe
 *      and has an integer cast into it but does not reach a string becomes 
 *      FSEQ. 
 * (5) We turn all string-like nodes that lead only into ROString nodes 
 *      into ROString nodes themselves. Note all nodes connected by ECompat
 *      edges must have the same kind. 
 * (6) We push WILD as far as it can go. Generally WILD contaminates all
 *      outgoing edges and all points-to targets, however an edge from
 *      WILD to ROString is allowed. 
 * (7) All otherwise unconstrainted nodes become SAFE. 
 *
 * Compared to previous solvers, this solver is much stricter about
 * strings. No "safe -> string" or "safe -> rostring" casts should remain. 
 *)

open Cil
open Ptrnode
module E = Errormsg

(* Set this to true and initialize the watch_which_nodes with a list of nodes
 * to be watched *)
let logUpdates = false
let logNodeUpdate = 
  let which_nodes = [ 1; ] in
  let which_nodes = 
    let h : (int, bool) Hashtbl.t = Hashtbl.create 13 in
    List.iter (fun nid -> Hashtbl.add h nid true) which_nodes;
    h
  in
  fun  (n: node) (k: opointerkind) (new_w : whykind) (old_w: whykind) ->
    if Hashtbl.mem which_nodes n.id then
      ignore (E.warn "updating %d to %a (%a). Old kind: %a (%a)"
                n.id d_opointerkind k d_whykind new_w
                d_opointerkind n.kind
								d_whykind old_w)

(* helper function: does a node represent a pointer to a character? *)
let is_char_pointer n =
  match n.btype with
    TInt(IChar,_) -> true
  | TInt(ISChar,_) -> true
  | TInt(IUChar,_) -> true
  | _ -> false

(* helper function: is this node's kind some form of string? *)
let is_string k = (k = String || k = ROString) 

(* was the node's kind set by some outside force? *)
let set_outside n = n.why_kind = UserSpec || n.why_kind = PrintfArg

(* equiv classes of nodes connected by ECompat edges. *)
module OrderedNode = struct type t = node let compare = compare end
module NodeSet = Set.Make(OrderedNode)
module NodeUF = Unionfind.Make(NodeSet)

(*
 **
 *** The Solver!
 ** 
 *)
let solve (node_ht : (int, node) Hashtbl.t) : unit = begin
	let node_eq = ref (NodeUF.empty) in (* ECompat equivalence classes *)
	
	(* whenever we have to "loop until things settle", we used
	 * finished to keep track of our status *)
  let finished = ref false in 

	(* update changes node "n"'s kind to "k" because of "w" and sets
	 * finished to false. It also does logging and sanity checking. *)
  let update n k w = begin
    if (k <> n.kind) then begin
			if n.why_kind = UserSpec && not (is_string n.kind) && 
				 not (is_string k) then ignore (E.warn 
				"Solver: changing User Specified %a node %d to %a because %a" 
				d_opointerkind n.kind 
				n.id d_opointerkind k d_whykind w);
      if logUpdates then logNodeUpdate n k w n.why_kind;
      n.kind <- k ; 
      n.why_kind <- w ;
      finished := false 
    end
  end in

	(* Help Function: find the attributes of a type *)
	let attrlist_of_type tau = match unrollType tau with
      TVoid(al) | TInt(_,al) | TFloat(_,al) 
    | TFloat(_,al) | TEnum(_,al) | TPtr(_,al) 
    | TArray(_,_,al) | TComp(_,al) | TFun(_,_,_,al) 
    | TNamed(_,_,al) -> al
	in 

	(* Helper Function: find the node of a type *)
  let node_of_type tau = 
		nodeOfAttrlist (attrlist_of_type tau)
  in

  (* Step 1
   * ~~~~~~
   * Consider every cast.
   *
   * Generate ECOMPAT edges between aligned sub-pointers.
   * Generate BADCAST constaints on failed pointers (make 'em WILD)
   * Generate ARITH constaints on upcasts (make 'em not SAFE). 
   *)
  if !E.verboseFlag then ignore (E.log "Solver: Step 1  (Casts)\n") ;

  (* Whenever we compare two types for equality we should mark all
   * matching inner pointers with ECOMPAT edges. This function is called
	 * by the type-scanning phase on all pairs of pointers that should
	 * match up. *)
  let handle_inner_pointers tau1 tau2 = 
    try begin
      match (node_of_type tau1),(node_of_type tau2) with
        Some(n1),Some(n2) -> 
					if List.exists (fun e -> e.ekind = ECompat && e.eto = n2) n1.succ ||
						 List.exists (fun e -> e.ekind = ECompat && e.eto = n1) n2.succ then
						(* avoid duplicate ECompat edges *) ()
					else begin
						addEdge n1 n2 ECompat (-1) ;
						node_eq := NodeUF.make_equal (!node_eq) n1 n2 
					end
      | _ -> (* in this unfortunate case, we don't know how to get
							* to the nodes of these types *)
				E.s (E.warn "Solver: cannot link inner pointers %a, %a"
					d_type tau1 d_type tau2) 
    end with _ -> ()
  in 

  (* In the process of scanning types involved in a cast, we may come
   * up with two pointer types that fail to match up. This function is
	 * called on those types. We make them WILD. *)
  let handle_failure e tau1 tau2 =
    let handle tau = try
      match (node_of_type tau) with
        Some(n1) -> update n1 Wild (BadCast(e))
      | None -> 
					if (tau = unrollType e.eto.btype || 
							tau = unrollType e.efrom.btype) then begin
						update e.eto   Wild (BadCast(e)) ;
						update e.efrom Wild (BadCast(e)) ;
					end else ignore (E.s (E.bug "Solver: unknown failure case %a"
							d_type tau))
    with _ -> () 
    in handle tau1 ; handle tau2
  in 
    
	(* 
	 * Step 1 Loop : examine every cast
	 *)
  Hashtbl.iter (fun id cur ->
    List.iter (fun e -> (* look at every forward edge *)
			(* skip polymorphic functions *)
      if e.ekind = ECast && not (Solveutil.is_p e.efrom e.eto) then begin
        let from_t = e.efrom.btype in
        let to_t = e.eto.btype in 
        let from_size = Type.bytesSizeOf(from_t) in
        let to_size = Type.bytesSizeOf(to_t) in

        if from_size < to_size then begin
          setFlag e.efrom pkNotSafe ; (* ARITH constraint *)
        end ;

        ignore (Type.subtype 
					~compat:(handle_inner_pointers)
          ~failure:(handle_failure e) 
					to_t from_t) 

      end else if e.ekind = EIndex then begin
				(* while we're here, these arrays cannot be safe *)
        setFlag e.eto pkNotSafe ;
        setFlag e.eto pkReachSeq 
			end
    ) cur.succ
  ) node_ht ; 

	(* Now we have equivalence classes of ECompat-joined nodes *)
	let compat_eq_classes = NodeUF.eq_classes !node_eq in 

  (* Step 2
   * ~~~~~~
   * Our second pass over the set of nodes. 
   * Set all of the flag starting conditions that we know about.  
   *)
  if !E.verboseFlag then ignore (E.log "Solver: Step 2  (Base Case)\n") ;

	(* share all flags within equivalence classes *)
	List.iter (fun eq_class ->
		List.iter (fun from_elt ->
			List.iter (fun to_elt ->
				setFlag to_elt from_elt.flags
			) eq_class
		) eq_class
	) compat_eq_classes ;

  (* loop over all the nodes ... *)
  Hashtbl.iter (fun id n -> 
    (* Mark interface character pointers as strings. *)
    if hasFlag n pkInterface && is_char_pointer n then begin
      (* the user had something to say here *)
      if (set_outside n) then begin
        match n.kind with
          String | ROString -> ()
        | _ -> ignore (E.warn 
                         "Solver: %a annotation on interface (char *)@!%a" 
                         d_opointerkind n.kind d_node n)
      end else 
				update n String BoolFlag 
    end ;

		(* Set initial flags based on the pointer kind *)
		match n.kind with
		|	String
		| ROString -> setFlag n pkReachString

		| Seq
		| Seq			 -> setFlag n pkReachSeq

		| FSeqN
		| SeqN		 -> setFlag n pkReachString ;
									setFlag n pkReachSeq

		| Index    -> setFlag n pkReachIndex

		| _				 -> () 

  ) node_ht ; 

  (* Step 3
   * ~~~~~~
   * Push all of the boolean flags around. 
   *)
  if !E.verboseFlag then ignore (E.log "Solver: Step 3  (Data-Flow)\n") ;
  (* loop over all the nodes ... *)
  finished := false ; 
  while (not !finished) do 
    finished := true ; 
    Hashtbl.iter (fun id cur -> 
			(* First consider all ECompat edges:
			 * flags should be equal across them. This is motivated by
			 * test/small1/apachebuf.c. Merely making ECompat-linked nodes have
			 * the same kind does not suffice: a pred of an ecompat-linked node
			 * may need to be made FSEQ because of a posarith on the other side
			 * of the ecompat link. *)
      List.iter (fun e ->
        if e.ekind = ECompat then begin
          let old_flags = e.efrom.flags in setFlag e.efrom e.eto.flags ;
          finished := !finished && (old_flags = e.efrom.flags) ;
          let old_flags = e.eto.flags in setFlag e.eto e.efrom.flags ;
          finished := !finished && (old_flags = e.eto.flags)
        end
      ) (cur.pred @ cur.succ) ;

			(* Consider all Successor Edges, do data-flow *)
			List.iter (fun e ->
				let old_flags = e.eto.flags in 
				(match e.ekind with
					 ECast -> setFlag e.eto (cur.flags land pkCastSuccFlags) ;
				 | ESafe -> setFlag e.eto (cur.flags land pkSafeSuccFlags) ;
				 | _ -> ()) ;
				finished := !finished && (old_flags = e.eto.flags)
			) cur.succ ;

			(* Consider all Predecessor Edges, do data-flow *)
			List.iter (fun e ->
				let old_flags = e.efrom.flags in 
				(match e.ekind with
					 ECast ->  setFlag e.efrom (cur.flags land pkCastPredFlags) ;
                     setFlag e.efrom (cur.flags land pkCNIPredFlags) ;
         | ENull 
				 | EIndex -> setFlag e.efrom (cur.flags land pkCNIPredFlags) ;
				 | _ -> ()) ;
				finished := !finished && (old_flags = e.efrom.flags)
			) cur.pred ;
				
    ) node_ht ; 
  done ; 

  (* Step 4
   * ~~~~~~
	 * Distinguish between sequences. We must do this after boolean flags
	 * (otherwise we cannot tell what reaches what, etc.) but before we do
	 * WILDs (because they interact with read-only strings). 
   *
   * Also generate ARITH constraints: q != SAFE. 
   *)
  if !E.verboseFlag then ignore (E.log "Solver: Step 4  (sequences)\n") ;

  Hashtbl.iter (fun id cur -> 
    (* Generate "ARITH" style constraints: q != SAFE *)
    if     hasFlag cur pkArith 
				|| hasFlag cur pkPosArith 
				|| hasFlag cur pkIntCast then begin 
				setFlag cur pkNotSafe ;
		end ;

    if hasFlag cur pkInterface && is_char_pointer cur && cur.kind = Unknown 
			then begin
				update cur String BoolFlag 
		end ;

		if cur.kind <> Wild && 
				(hasFlag cur pkNotSafe || 
				 hasFlag cur pkReachString ||
				 hasFlag cur pkReachSeq || 
				 hasFlag cur pkReachIndex || 
				 hasFlag cur pkIntCast) then 
		begin
			let new_kind = 
				if		 (hasFlag cur pkReachIndex)		then Index
				else if 
							 (hasFlag cur pkReachString)  &&
					 not (hasFlag cur pkReachSeq)			&&
					 not (hasFlag cur pkIntCast)			&&
					 not (hasFlag cur pkArith)				&&
					 not (cur.kind = ROString)				then String
				else if 
							 (hasFlag cur pkReachString)  &&
					 not (hasFlag cur pkReachSeq)			&&
					 not (hasFlag cur pkIntCast)			&&
					 not (hasFlag cur pkArith)				&&
							 (cur.kind = ROString)				then ROString
				else if 
							 (hasFlag cur pkReachString)  &&
							 (hasFlag cur pkArith)				then SeqN
				else if 
					 not (hasFlag cur pkReachString)  &&
							 (hasFlag cur pkArith)				then Seq
				else if 
							 (hasFlag cur pkReachString)  then FSeqN

				else																		 FSeq
			in 
			update cur new_kind BoolFlag 
    end
  ) node_ht ; 

  (* Step 5
   * ~~~~~~
	 * Determine which Strings/FSeqNs should be ROStrings. 
	 *
	 * Note that nodes connected by ECompat edges must all agree. 
   *)
  if !E.verboseFlag then ignore (E.log "Solver: Step 5  (ROString)\n") ;

	(* helper function to determine if a node is ready to become ROString *)
	let ready_for_rostring cur = (* modulo edges *)
		(cur.kind = FSeqN || cur.kind = Safe || cur.kind = String) &&
		not (hasFlag cur pkUpdated) &&
		not (hasFlag cur pkIntCast) &&
		not (hasFlag cur pkArith) &&
		List.length cur.succ > 0 &&
		List.fold_left (fun acc e ->     
			acc && ( (e.ekind = ECompat) ||
							 (e.ekind = ECast && e.eto.kind = ROString))) true cur.succ 
	in 
  finished := false ; 
  while not !finished do 
    finished := true ; 
		(* 
		 * Consider every node in the graph in two phases.
		 *  (1) Consider all nodes w/o ECompat edges
		 *  (2) Consider all Eq-Classes of ECompat nodes
		 *)
    Hashtbl.iter (fun id cur -> 
			let any_ecompat = 
				List.fold_left (fun acc e -> acc || e.ekind = ECompat)
					false (cur.succ @ cur.pred) in
			if not any_ecompat && 
				 ready_for_rostring cur 
				 then begin
					update cur ROString (SpreadFromEdge ((List.hd cur.succ).eto)) ;
			end
    ) node_ht ;

		(* now look at an equivalence class of ECompat nodes *)
		List.iter (fun ecompat_node_list ->
			(* if every node in this list is ready, move every node in this
			 * list to ROString *)
			let all_are_ready = 
				List.fold_left (fun acc cur -> acc &&
					 ready_for_rostring cur && 
					 (cur.why_kind <> UserSpec || cur.kind = ROString)) 
					 true ecompat_node_list 
			in
			if all_are_ready then 
				List.iter (fun cur -> 
					update cur ROString (SpreadFromEdge ((List.hd cur.succ).eto)) ;
				) ecompat_node_list 
		) compat_eq_classes 
  done ;

  (* Step 6
   * ~~~~~~
   * Spread WILD as far as it will go. 
   *)
  if !E.verboseFlag then ignore (E.log "Solver: Step 6  (Wild)\n") ;
  finished := false ; 
  while not !finished do
    finished := true ; 
    Hashtbl.iter (fun id cur -> 
      if cur.kind = Wild then begin
        (* consider all of the successor edges *)
				List.iter (fun e ->
          if e.eto.kind <> ROString then 
            update e.eto Wild (SpreadFromEdge cur)
				) cur.succ ;

        (* consider all of the predecessor edges *)
				List.iter (fun e ->
          if e.efrom.kind <> ROString then 
            update e.efrom Wild (SpreadFromEdge cur)
				) cur.pred ;

        (* handle points-to information *)
        List.iter (fun n ->
          update n Wild (SpreadPointsTo cur) ;
        ) cur.pointsto ;

      end
    ) node_ht ;
  done ;

  (* Step 7
   * ~~~~~~~
   * All other nodes are safe. 
   *)
  if !E.verboseFlag then ignore (E.log "Solver: Step 7  (Safe)\n") ;
  Hashtbl.iter (fun id n -> 
    if n.kind = Unknown then begin
			update n Safe Unconstrained
		end ;

		(* Sanity Check! Typecheck.ml does much more than this. *)
		if n.kind = Safe &&
			(hasFlag n pkNotSafe || 
			 hasFlag n pkReachString ||
			 hasFlag n pkReachSeq || 
			 hasFlag n pkReachIndex || 
			 hasFlag n pkIntCast) then begin
			E.s (E.bug "Solver: botched node (left/made it safe)@!%a"
				d_node n)
		end ;
  ) node_ht ;

  () 
end 

(* 
 * TODO (The Wishlist of Wes): 
 * (1) change markptr so that ENULL is never used, use ECAST instead
 * (2) shrink the graph instead of using ECompat edges
 *)
