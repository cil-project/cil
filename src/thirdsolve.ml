(*
 * My third attempt at constraint solving for the pointer-qualifier graph.
 *)

open Cil
open Ptrnode

let warn = ref false  

(*          
 * In this diagram, X <- Y means that Y can be cast to X. This may involve
 * a run-time check. 
 *
 * ROString <- Wild
 * ROString         <- String              <- FSeqN        <- SeqN 
 *                            Safe <- FSeq <- FSeqN
 *                                    FSeq          <- Seq         <- Index
 *                                                     Seq <- SeqN         
 *
 * Scalar <- AnyPointerKind <- Unknown
 *)

(* returns one step casts from this kind *)
let one_step_cast from_k = 
  match from_k with
    Index -> [Seq]
  | SeqN -> [Seq;FSeqN]
  | Seq -> [FSeq]
  | FSeqN -> [FSeq;String]
  | FSeq -> [Safe]
  | String -> [ROString]
  | Wild -> [ROString]
  | ROString -> []
  | _ -> [] 

(* returns true if you can get from from_k to to_k in any number of steps *)
let rec can_cast from_k to_k =
  List.fold_left (fun acc k -> acc || k = to_k || can_cast k to_k) 
    false (one_step_cast from_k)

(* helper functions *)
let ecast_edges_only l = List.filter (fun e -> e.ekind = ECast) l 
let ecompat_edges_only l = List.filter (fun e -> e.ekind = ECompat) l 
let non_safe_edges_only l = List.filter (fun e -> e.ekind <> ESafe) l 
let ecastandenull_edges_only l = 
  List.filter (fun e -> e.ekind = ECast || e.ekind = ENull) l 
let eCNI_edges_only l = List.filter (fun e -> e.ekind = ECast || 
  e.ekind = ENull || e.ekind = EIndex) l 
let eNI_edges_only l = List.filter (fun e ->
  e.ekind = ENull || e.ekind = EIndex) l 
(* helper function: does a node represent a pointer to a character? *)
let is_char_pointer n =
  match n.btype with
    TInt(IChar,_) -> true
  | TInt(ISChar,_) -> true
  | TInt(IUChar,_) -> true
  | _ -> false
(* does the node represent an array? *)
let is_array n =
  match n.btype with
    TArray(_) -> true
  | _ -> false

(*
 * The Heart of the Solver
 *)
let solve (node_ht : (int,node) Hashtbl.t) = begin
  let set_outside n = 
    n.why_kind = UserSpec || n.why_kind = PrintfArg
  in

  (* in this solver, an update is forceful *)
  let update n k w = begin
    n.kind <- k ; 
    n.why_kind <- w ;
  end in


  (* Step 1
   * ~~~~~~
   * Set all of the little boolean flags correctly. Use whatever
   * method simplesolve uses. This covers everything except reaches_string.
   *)
  ignore (Pretty.printf "Solver: Step 1\n") ;
  Simplesolve.set_flags node_ht ; 

  (* Step 2
   * ~~~~~~
   * Mark all of the interface character pointers as strings. 
   *)
  ignore (Pretty.printf "Solver: Step 2\n") ;
  Hashtbl.iter (fun id n -> 
    if n.interface && is_char_pointer n then begin
      (* the user had something to say here *)
      if (set_outside n) then begin
        match n.kind with
          String | ROString -> ()
        | _ -> ignore (E.warn "Solver: %a annotation on interface (char *)@!%a" d_pointerkind n.kind d_node n)
      end else begin
        assert(not(set_outside n)) ;
        if (n.updated || (List.length n.succ) <> 0) then
          update n String BoolFlag
        else
          update n ROString BoolFlag
      end
    end
  ) node_ht ;

  (* Step 3
   * ~~~~~~
   * Mark all of the nodes that can reach a string.
   *)
  ignore (Pretty.printf "Solver: Step 3\n") ;
  let rec mark_string a = 
    if not (a.can_reach_string) then begin
        a.can_reach_string <- true ;
        List.iter (fun e -> 
          if (e.ekind = ECast || e.ekind = ENull || e.ekind = EIndex) then
              mark_string e.efrom
          ) a.pred
    end
  in 
  Hashtbl.iter (fun id n ->
    if n.kind = String || n.kind = ROString then mark_string n
  ) node_ht ;

  (* Step 4
   * ~~~~~~
   * Add ECompat edges for later use. If we see
   *  int * 1 * 2 x;
   *  int * 3 * 4 y;
   *  x = y; Then we add a compat edge between 1 and 3. They must both
   *  either be safe, wild or index. 
   *)
  ignore (Pretty.printf "Solver: Step 4\n") ;
  (* consider every node in the graph! *)
  Hashtbl.iter (fun id cur -> 
    (* consider all of the successor edges *)
    List.iter (fun e -> 
       match (nodeOfAttrlist (typeAttrs cur.btype)),
             (nodeOfAttrlist (typeAttrs e.eto.btype)) with
         Some(n1),Some(n3) -> addEdge n1 n3 ECompat (-1)
       | _ -> ()
    ) (ecast_edges_only cur.succ);
  ) node_ht ;

  (* Step 5
   * ~~~~~~
   * Turn all bad casts into Wild Pointers. 
   *)
  ignore (Pretty.printf "Solver: Step 5\n") ;
  Hashtbl.iter (fun id cur ->
    let make_wild n e =
      if n.kind = ROString then begin
        ()
      end else if n.kind <> Wild && set_outside n then begin
        E.s (E.bug "Solver: bad annotation (should be wild because of cast)@!%a" d_node n)
      end else begin
        assert(not(set_outside n) || n.kind = Wild) ;
        update n Wild (BadCast e)
      end
    in 
    (* pick out all successors of our current node *)
    List.iter (fun e -> 
      let n_from, n_to = e.efrom, e.eto in
      let t_from, t_to = e.efrom.btype, e.eto.btype in 
      if Simplesolve.subtype t_from Safe t_to Safe then begin
        ()
      end else if Simplesolve.subtype t_from Safe 
          (TArray(t_to,(Some(Const(CInt(1024,ILong,None)))),[])) Safe then begin
          ()
      end else if Simplesolve.is_p n_from n_to ||
         Simplesolve.is_p n_to n_from then begin
        ()
      end else begin
        (* must be wild! *)
        make_wild e.efrom e ;
        make_wild e.eto e ;
      end
    ) (ecast_edges_only cur.succ) ;
   (* also, the nodes related by ECompat edges must either have the same
    * type or be wild! *)
    List.iter (fun e ->
      let n_from, n_to = e.efrom, e.eto in
      let t_from, t_to = e.efrom.btype, e.eto.btype in 
      if not (Simplesolve.type_congruent n_from.btype n_from.kind 
                                     n_to.btype n_to.kind) then begin
        (* must make both WILD! *)
        make_wild e.efrom e ;
        make_wild e.eto e ;
      end
    ) (ecompat_edges_only cur.succ) ;
  ) node_ht ;

  (* Step 6
   * ~~~~~~
   * Spread wild pointers.
   *)
  ignore (Pretty.printf "Solver: Step 6\n") ;
  let finished = ref false in 
  let update n k w = begin
    if (k <> n.kind) then begin
      assert(n.why_kind <> UserSpec) ;
      n.kind <- k ; 
      n.why_kind <- w ;
      finished := false 
    end
  end in
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* actually, only look at wild nodes now! *)
      if (cur.kind = Wild) then begin
        (* consider all of the successor edges *)
        List.iter (fun e -> 
          if e.eto.kind = ROString then begin
            ()
          end else if e.eto.kind <> Wild && e.efrom.why_kind = UserSpec then begin
            E.s (E.bug "Solver: bad annotation (should be wild because of successor edge)@!%a" d_node e.eto)
          end else begin
            (if (e.eto.why_kind = UserSpec) then assert (e.eto.kind = Wild)) ;
            update e.eto Wild (SpreadFromEdge cur) ;
          end
        ) cur.succ ;

        (* consider all predecessor edges *)
        List.iter (fun e -> 
          if e.efrom.kind = ROString then begin
            ()
          end else if e.efrom.kind <> Wild && e.efrom.why_kind = UserSpec then begin
            E.s (E.bug "Solver: bad annotation (should be wild because of predecessor edge)@!%a" d_node e.efrom)
          end else begin
            (if (e.efrom.why_kind = UserSpec) then assert (e.efrom.kind = Wild)) ;
            update e.efrom Wild (SpreadFromEdge cur) ;
          end
        ) cur.pred ;

        (* handle points-to information *)
        List.iter (fun n ->
          if n.kind <> Wild && set_outside n then begin
            E.s (E.bug "Solver: bad annotation (should be wild because of pointsto)@!%a" d_node n)
          end ;
          (if (n.why_kind = UserSpec) then assert (n.kind = Wild)) ;
          update n Wild (SpreadPointsTo cur) ;
        ) cur.pointsto ;
      end
    ) node_ht 
  done ;

  ignore (Pretty.printf "Solver: Check 1\n") ;
  (* Consistency check: edges work correctly ... *)
  Hashtbl.iter (fun id cur ->
    (* All of my successors should have a predecessor edge
     * that points to me! *)
    List.iter (fun e ->
      let points_back_to_me = ref false in
      List.iter (fun be ->
        if be.efrom = cur then points_back_to_me := true
      ) e.eto.pred ;
      if (not !points_back_to_me) then begin
        E.s (E.bug "Succ edge from %d to %d, no reverse pred edge!\n%a\n%a"
          cur.id e.efrom.id 
          d_node cur
          d_node e.efrom)
      end ;
    ) cur.succ ;

    (* all of my predecessors should have a successor edge that
     * points forward to me! *)
    List.iter (fun e ->
      let points_for_to_me = ref false in
      List.iter (fun fe ->
        if fe.eto = cur then points_for_to_me := true
      ) e.efrom.succ ;
      if (not !points_for_to_me) then begin
        E.s (E.bug "Pred edge from %d to %d, no reverse succ edge!\n%a\n%a"
          cur.id e.efrom.id 
          d_node cur
          d_node e.efrom)
      end ;
    ) cur.pred ;

  ) node_ht;

  ignore (Pretty.printf "Solver: Check 2\n") ;
  (* Consistency check: wild nodes should form strongly connected
   * components *)
  Hashtbl.iter (fun id cur -> 
    if (cur.kind = Wild) then begin
      List.iter (fun e ->
        assert(e.eto.kind = Wild || e.eto.kind = ROString)
      ) cur.succ ;
      List.iter (fun e ->
        assert(e.efrom.kind = Wild || e.efrom.kind = ROString)
      ) cur.pred ;
      List.iter (fun n ->
        assert(n.kind = Wild)
      ) cur.pointsto ;
    end else if cur.kind <> ROString then begin
      List.iter (fun e ->
        assert(e.eto.kind <> Wild) ;
      ) cur.succ ;
      List.iter (fun e ->
        assert(e.efrom.kind <> Wild) ; 
      ) cur.pred ;
    end
  ) node_ht ;

  (* Step 7
   * ~~~~~~
   * Set can_reach_seq and can_reach_index. 
   *)
  ignore (Pretty.printf "Solver: Step 7\n") ;
  let rec mark_seq a = 
    if not (a.can_reach_seq) then begin
        a.can_reach_seq <- true ;
        List.iter (fun e ->
          if (e.ekind = ECompat || e.ekind = ECast || e.ekind = ENull ) then
            mark_seq e.eto
        ) a.succ ;
        List.iter (fun e ->
          if (e.ekind = ECompat || e.ekind = ECast || e.ekind = ENull || e.ekind = EIndex) then
            mark_seq e.efrom
        ) a.pred ;
    end
  in 
  let rec mark_index a = 
    if not (a.can_reach_index) then begin
      a.can_reach_index <- true; 
        List.iter (fun e ->
          if (e.ekind = ECompat || e.ekind = ECast || e.ekind = ENull ) then
            mark_index e.eto
        ) a.succ ;
        List.iter (fun e ->
          if (e.ekind = ECompat || e.ekind = ECast || e.ekind = ENull || e.ekind = EIndex) then
            mark_index e.efrom
        ) a.pred ;
        match nodeOfAttrlist (typeAttrs a.btype) with
          Some(n) -> mark_index n 
        | _ -> () 
    end
  in 
  Hashtbl.iter (fun id n ->
    (if n.kind = Seq || n.kind = SeqN then mark_seq n) ;
    (if n.kind = Index then mark_index n) ;
    (* consider all of the casts out of this node *)
    List.iter (fun e -> 
      let n_from, n_to = e.efrom, e.eto in
      let t_from, t_to = e.efrom.btype, e.eto.btype in 
      if Simplesolve.subtype t_from Safe t_to Safe then begin
        ()
      end else if Simplesolve.subtype t_from Safe 
          (TArray(t_to,(Some(Const(CInt(1024,ILong,None)))),[])) Safe then begin
          mark_seq e.efrom ;
          mark_seq e.eto ;
      end
    ) (ecast_edges_only n.succ) ;
  ) node_ht ;

(*
  Hashtbl.iter (fun id n ->
    if n.can_reach_index && n.can_reach_seq && n.why_kind <> UserSpec then
      ignore (E.warn "Solver: incompatible annotations (SEQ, INDEX)@!%a" d_node n)
  ) node_ht ;
  *)

  (* Step 8
   * ~~~~~~
   * Attempt to make FSEQ[N] nodes. 
   *)
  ignore (Pretty.printf "Solver: Step 8\n") ;
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* is this node "innately" FSeq? *)
      if (not (cur.can_reach_seq || cur.can_reach_index)) &&
         (cur.posarith || (cur.null && cur.intcast)) &&
         cur.kind <> Wild && 
         not(set_outside cur) then begin
         if cur.can_reach_string then begin
           assert(not(set_outside cur) || cur.kind = FSeqN) ;
           (update cur FSeqN BoolFlag)
         end else begin
           assert(not(set_outside cur) || cur.kind = FSeq) ;
           (update cur FSeq BoolFlag)
         end
      end ;
      (* consider all successor edges *)
      List.iter (fun e -> 
        if (
            (e.ekind = ECast && e.eto.kind = FSeqN) || 
            (e.ekind = ECompat && (e.eto.kind = FSeq || e.eto.kind = FSeqN))
           ) &&
           not (set_outside cur) then begin
          assert(not(cur.can_reach_seq)) ;
          assert(not(cur.can_reach_index)) ;
          assert(not(cur.kind = Wild)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if cur.can_reach_string || e.eto.kind = FSeqN then begin
            (update cur FSeqN (SpreadFromEdge e.eto)) ;
            cur.can_reach_string <- true 
          end else begin
            (update cur FSeq (SpreadFromEdge e.eto))
          end
        end
      )  cur.succ ;
      (* consider all pred edges *)
      List.iter (fun e -> 
        if (e.ekind = ECast || e.ekind = ENull || e.ekind = EIndex) &&
           (e.eto.kind = FSeq || e.eto.kind = FSeqN) && 
           not (set_outside cur) &&
           cur.kind <> ROString then begin
          assert(not(cur.can_reach_seq)) ;
          assert(not(cur.can_reach_index)) ;
          assert(not(cur.kind = Wild)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if cur.can_reach_string then
            (update cur FSeqN (SpreadFromEdge e.efrom))
          else
            (update cur FSeq (SpreadFromEdge e.efrom))
        end ;
        if e.ekind <> ESafe && 
           (e.efrom.kind = String || e.efrom.kind = FSeqN) &&
           (not ((is_array e.efrom) || (cur.arith))) &&
           (cur.posarith || e.efrom.kind = FSeqN) && 
           (not(set_outside cur)) && 
           cur.kind <> ROString then begin
          assert(not(cur.can_reach_seq)) ;
          assert(not(cur.can_reach_index)) ;
          assert(not(cur.kind = Wild)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if cur.can_reach_string then
            (update cur FSeqN (SpreadFromEdge e.efrom))
          else
            (update cur FSeq (SpreadFromEdge e.efrom))
        end
      ) cur.pred ;
      
      (if (cur.kind = FSeqN) then 
        match nodeOfAttrlist (typeAttrs cur.btype) with
          Some(n) -> 
            assert(not(n.can_reach_seq)) ;
            assert(not(n.can_reach_index)) ;
            assert(n.why_kind <> UserSpec) ; 
            assert(n.can_reach_string) ;
            update n FSeqN (SpreadToArrayFrom cur)
        | None -> ()) 
    ) node_ht
  done ;

  (* Step 9
   * ~~~~~~
   * Attempt to make SEQ[N] nodes. 
   *)
  ignore (Pretty.printf "Solver: Step 9\n") ;
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* if it would have been an FSEQ, but for that pesky user annotation *)
      if cur.can_reach_seq && (not cur.can_reach_index) &&
         (cur.posarith || (cur.null && cur.intcast)) && 
         not (set_outside cur) && cur.kind <> Wild then begin
         assert(cur.why_kind <> UserSpec) ; 
         if cur.can_reach_string then
           (update cur SeqN BoolFlag)
         else
           (update cur Seq BoolFlag)
      end ;
      (* if it is a natural seq pointer ... *)
      if (cur.arith || cur.intcast) && (not cur.can_reach_index) &&
         not (set_outside cur) && cur.kind <> Wild then begin
         assert(cur.why_kind <> UserSpec) ; 
         if cur.can_reach_string then
           (update cur SeqN BoolFlag)
         else
           (update cur Seq BoolFlag)
      end ;
      (* handle successor edges *)
      List.iter (fun e -> 
        if (e.ekind = ECast && 
            (e.eto.kind = String || e.eto.kind = FSeqN || e.eto.kind = SeqN) &&
            (is_array e.eto || e.eto.kind = SeqN)) && 
            not (set_outside cur) then begin
          assert(cur.kind <> Wild) ;
          assert(not(cur.can_reach_index)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if cur.can_reach_string || e.eto.kind = SeqN then
           (update cur SeqN (SpreadFromEdge e.eto))
          else
           (update cur Seq (SpreadFromEdge e.eto))
        end ;
        if (e.ekind = ECompat && (e.eto.kind = Seq || e.eto.kind = SeqN)) && 
          not (set_outside cur) then begin
          assert(cur.kind <> Wild) ;
          assert(not(cur.can_reach_index)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if cur.can_reach_string then
           (update cur SeqN (SpreadFromEdge e.eto))
          else
           (update cur Seq (SpreadFromEdge e.eto))
        end
      ) cur.succ ;

      (* consider all of the casts out of this node *)
      if cur.kind <> Wild then 
      List.iter (fun e -> 
        let n_from, n_to = e.efrom, e.eto in
        let t_from, t_to = e.efrom.btype, e.eto.btype in 
        if Simplesolve.subtype t_from Safe t_to Safe then begin
          ()
        end else if Simplesolve.subtype t_from Safe 
            (TArray(t_to,(Some(Const(CInt(1024,ILong,None)))),[])) Safe then begin
            (* the magic seq-making cast *)
            let mark n m = begin
              if (set_outside n) then
                ()
              else begin
                assert(n.kind <> Wild) ;
                assert(not(n.can_reach_index)) ;
                assert(n.why_kind <> UserSpec) ; 
                update n Seq (BadCast e)
              end
            end in
            mark n_to n_from ;
            mark n_from n_to ;
        end
      ) (ecast_edges_only cur.succ) ;

      (* consider all pred edges *)
      List.iter (fun e -> 
        if (e.ekind = ECompat || e.ekind = ECast || e.ekind = ENull)
          && (e.efrom.kind = Seq || e.efrom.kind = SeqN) && 
          not (set_outside cur) && 
          cur.kind <> ROString then begin
          assert(cur.kind <> Wild) ;
          assert(not(cur.can_reach_index)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if cur.can_reach_string then
           (update cur SeqN (SpreadFromEdge e.efrom))
          else
           (update cur Seq (SpreadFromEdge e.efrom))
        end ;
        if e.ekind = EIndex && not cur.can_reach_index && not (set_outside cur) && cur.kind <> Wild then begin
          assert(cur.why_kind <> UserSpec) ; 
          if cur.can_reach_string then
           (update cur SeqN (SpreadFromEdge e.efrom))
          else
           (update cur Seq (SpreadFromEdge e.efrom))
        end
      ) cur.pred ;

      (if (cur.kind = SeqN) then 
        match nodeOfAttrlist (typeAttrs cur.btype) with
          Some(n) -> 
            assert(not(n.can_reach_index)) ;
            assert(not(n.kind = Wild)) ;
            assert(n.why_kind <> UserSpec) ; 
            update n SeqN (SpreadToArrayFrom cur)
        | None -> ()) 
    ) node_ht ;
  done ;

  (* Step 10
   * ~~~~~~~
   * Chance certain FSeqNs to ROStrings.
   *)
  ignore (Pretty.printf "Solver: Step 10\n") ;
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* consider all succ edges *)
      List.iter (fun e -> 
        if e.ekind = ECast && e.eto.kind = ROString &&
            cur.kind = FSeqN && (List.length cur.succ) = 1 then begin 
            assert(cur.why_kind <> UserSpec) ; 
            update cur ROString (SpreadFromEdge e.eto) ;
        end
      ) cur.succ
  ) node_ht ;
  done ;

  (* Step 11
   * ~~~~~~~
   * Note all index nodes.
   *)
  ignore (Pretty.printf "Solver: Step 11\n") ;
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* if it would have been an [F]SEQ, but for that pesky user annotation *)
      if (cur.can_reach_index) &&
         (cur.posarith || (cur.null && cur.intcast)) && 
         not (set_outside cur) && cur.kind <> Wild then begin
         assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
         assert(not(cur.can_reach_seq) || (cur.can_reach_seq && cur.can_reach_index)) ;
         (update cur Index BoolFlag)
      end ;
      (* if it is a natural seq pointer ... *)
      if (cur.arith || cur.intcast) && (cur.can_reach_index) &&
         not (set_outside cur) && cur.kind <> Wild then begin
         assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
         assert(not(cur.can_reach_seq) || (cur.can_reach_seq && cur.can_reach_index)) ;
         (update cur Index BoolFlag)
      end ;

      (* consider all succ nodes *)
      List.iter (fun e -> 
        if (e.ekind = ECompat || e.ekind = ECast) && (e.eto.kind = Index) && 
          not (set_outside cur) then begin
          assert(cur.kind <> Wild) ;
          assert(not(cur.can_reach_seq) || (cur.can_reach_seq && cur.can_reach_index)) ;
          assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
          update cur Index (SpreadFromEdge e.eto)
        end
      ) cur.succ ;

      (* handle all pred nodes *)
      List.iter (fun e ->
        if (e.ekind = ECast || e.ekind = ECompat || e.ekind = ENull || e.ekind = EIndex) &&
          e.efrom.kind = Index && not (set_outside cur) then begin
          assert(cur.kind <> Wild) ;
          assert(not(cur.can_reach_seq) || (cur.can_reach_seq && cur.can_reach_index)) ;
          assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
          update cur Index (SpreadFromEdge e.efrom)
        end
      ) cur.pred  ;

      (* handle array types *)
      if (cur.kind = Index || cur.kind = FSeqN || cur.kind = SeqN) then begin
        match nodeOfAttrlist (typeAttrs cur.btype) with
          Some(n) -> 
            if not (set_outside n) then begin
              assert(n.kind <> Wild) ;
              assert(not(n.can_reach_seq) || (n.can_reach_seq && n.can_reach_index)) ;
              assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
              update n Index (SpreadToArrayFrom cur)
            end
        | None -> () 
      end ;
    ) node_ht
  done ;

  (* Step 12
   * ~~~~~~~
   * All other nodes are safe. 
   *)
  ignore (Pretty.printf "Solver: Step 12\n") ;
  Hashtbl.iter (fun id n -> 
    if n.kind = Unknown then begin
      assert(n.why_kind <> UserSpec) ;
      (update n Safe Unconstrained)
    end
  ) node_ht ;

end
