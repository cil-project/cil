(*
 * SOLVER: Infer pointer kinds and other data-flow-like information on the 
 * program-wide pointer graph. 
 *
 * My last attempt at a solver before leaving for the summer. 
 *)

open Cil
open Ptrnode
open Solveutil

let show_steps = true 

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
 *
 * String -> FSeqN is also allowed but is not represented in this figure or
 * the function below. 
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
let cast_compat_edges l = List.filter (fun e -> e.ekind = ECast || e.ekind = ECompat) l 
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
(* was the node's kind set by some outside force? *)
let set_outside n = 
  n.why_kind = UserSpec || n.why_kind = PrintfArg

(* This is our little test to see if we can get away with some sort of
 * sequence test between these two nodes (rather than making them both
 * WILD). By the time we get here we know they are not "SAFE". 
 * Loosely, this implements "\Exists K. t_from = t_to[K]" by picking
 * a big K and checking t_from <= t_to[K]. *)
let sequence_condition t_from t_to = 
  subtype t_from Safe (TArray(t_to,(Some(Const(CInt(1024,ILong,None)))),[])) Safe 

(*
 * The Heart of the Solver
 *)
let solve (node_ht : (int,node) Hashtbl.t) = begin
  (* Step 1
   * ~~~~~~
   * Add ECompat edges.
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 1  (ECompat)\n") ;
  (* loop over all the nodes ... *)
  let finished = ref false in 
  while (not !finished) do 
    finished := true ; 
    Hashtbl.iter (fun id cur -> 
      (* Add ECompat edges. For example, in
       * int *(1) *(2) x;
       * int *(3) *(4) y = x;
       * We add an edge between (1) and (3). They must have the same kind
       * (i.e., they must both be WILD or both be SAFE). *)
      List.iter (fun e -> 
         match (nodeOfAttrlist (typeAttrs cur.btype)),
               (nodeOfAttrlist (typeAttrs e.eto.btype)) with
           Some(n1),Some(n3) -> begin
             (* check and see if there is already such an edge *)
             if List.exists (fun e -> e.eto == n3 &&
                e.ekind = ECompat) n1.succ then
                () (* already done *)
             else begin
               addEdge n1 n3 ECompat e.ecallid; (* use same callid *)
               finished := false ; 
             end
           end
         | _ -> ()
      ) (cast_compat_edges cur.succ);
    ) node_ht ; 
  done ;

  (* Step 2
   * ~~~~~~
   * Our second pass over the set of nodes. 
   * Set all of the flag starting conditions that we know about.  
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 2  (Base Case)\n") ;
  (* loop over all the nodes ... *)
  Hashtbl.iter (fun id n -> 
    (* Add in starting flag conditions. For example, we can identify
     * strings. *)
    if n.interface && is_char_pointer n then begin
      (* the user had something to say here *)
      if (set_outside n) then begin
        match n.kind with
          String | ROString -> ()
        | _ -> ignore (E.warn "Solver: %a annotation on interface (char *)@!%a" d_opointerkind n.kind d_node n)
      end else begin
        assert(not(set_outside n)) ;
        n.kind <- String ; (* we'll find ROStrings later *)
        n.why_kind <- BoolFlag ; 
      end
    end ;
    (* And then set the "reaches string" flag. *)
    if n.kind = String || n.kind = ROString || n.kind = FSeqN || 
       n.kind = SeqN then begin
       setFlag n pkReachString
    end 

    (* Now look for Sequence and Index nodes *)
    else if n.kind = Seq || n.kind = SeqN then begin
      setFlag n pkReachSeq
    end else if n.kind = Index then begin
      setFlag n pkReachIndex 
    end ; 

    (* Consider all of the casts out of this node. Certain conditions will
     * result in a sequence inference later on: we'll mark those now. *)
    List.iter (fun e -> 
      let t_from, t_to = e.efrom.btype, e.eto.btype in 
      if subtype t_from Safe t_to Safe then begin
        () (* no sequence-ness here *)
      end else if sequence_condition t_from t_to then begin
          setFlag e.efrom pkReachSeq ;
          setFlag e.eto pkReachSeq ;
      end
    ) (cast_compat_edges n.succ) ;

  ) node_ht ; 

  (* Step 3
   * ~~~~~~
   * Push all of the boolean flags around. 
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 3  (Data-Flow)\n") ;
  (* loop over all the nodes ... *)
  finished := false ; 
  while (not !finished) do 
    finished := true ; 
    Hashtbl.iter (fun id cur -> 

      (* first consider all Predecessor Cast edges *)
      List.iter (fun e -> 
        if e.ekind = ECast || e.ekind = ECompat then begin
          let old_flags = e.efrom.flags in 
            setFlag e.efrom (cur.flags land pkCastPredFlags) ;
            finished := !finished && (old_flags = e.efrom.flags)
        end
      ) cur.pred ;

      (* now consider all Successor Cast edges *)
      List.iter (fun e -> 
        if e.ekind = ECast || e.ekind = ECompat then begin
          let old_flags = e.eto.flags in 
            setFlag e.eto (cur.flags land pkCastSuccFlags) ;
            finished := !finished && (old_flags = e.eto.flags)
        end
      ) cur.succ ;

      (* now consider all Predecessor Cast/Null/Index edges *)
      List.iter (fun e -> 
        if e.ekind = ECast || e.ekind = ENull || e.ekind = EIndex || e.ekind = ECompat then begin
          let old_flags = e.efrom.flags in 
            setFlag e.efrom (cur.flags land pkCNIPredFlags) ;
            finished := !finished && (old_flags = e.efrom.flags)
        end
      ) cur.pred ;
    ) node_ht ;
  done ; 

  (* Step 4
   * ~~~~~~
   * Distinguish between strings and read-only strings. We must do this
   * after boolean flags (otherwise we cannot tell the "read-only" part)
   * but before we do WILDs (because they interact with read-only strings). 
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 4  (read-only strings)\n") ;
  Hashtbl.iter (fun id n ->
    (* If a string is never updated and it never goes anywhere then
     * we can give it the special terminating "ro-string" status. *)
    if n.kind = String && (not (hasFlag n pkUpdated)) && n.succ = [] then
       n.kind <- ROString
  ) node_ht ;

  (* By the time we call "update" we are sure that the update should go
   * through. All consistency checks must be done beforehand. *)
  let update n k w = begin
    n.kind <- k ; 
    n.why_kind <- w ;
  end in

  (* Step 5
   * ~~~~~~
   * Turn all bad casts into Wild Pointers. There is a small amount of
   * black magic in this step. 
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 5  (bad casts)\n") ;
  Hashtbl.iter (fun id cur ->
    (* First, what should we do when we think something should be wild? *)
    let make_wild n e =
      if n.kind = ROString then begin (* WILD->ROSTRING is allowed ... *)
        () (* so leave ROSTRINGs alone *)
      end else if n.kind <> Wild && set_outside n then begin
        E.s (E.bug "Solver: bad annotation (should be wild because of cast)@!%a@!%a" d_node e.eto d_node e.efrom)
      end else begin
        assert(not(set_outside n) || n.kind = Wild) ; (* self-check *)
        update n Wild (BadCast e)
      end
    in 
    (* pick out all successors of our current node *)
    List.iter (fun e -> 
      let n_from, n_to = e.efrom, e.eto in
      let t_from, t_to = e.efrom.btype, e.eto.btype in 
      if subtype t_from Safe t_to Safe then begin
        () (* safe cast *)
      end else if sequence_condition t_from t_to then begin
        () (* sequence cast *)
      end else if is_p n_from n_to || (* polymorphic function *)
         is_p n_to n_from then begin (* argument or retval *)
        ()
      end else begin
        (* If you fail all of the other cases, you must be wild! *)
        make_wild e.efrom e ;
        make_wild e.eto e ;
      end
    ) (ecast_edges_only cur.succ) ;
   (* also, the nodes related by ECompat edges must either have the same
    * type or be wild! *)
    List.iter (fun e ->
      let n_from, n_to = e.efrom, e.eto in
      if not (type_congruent n_from.btype n_from.kind 
                                     n_to.btype n_to.kind) then begin
        (* must make both WILD! *)
        make_wild n_from e ;
        make_wild n_to e ;
      end
    ) (ecompat_edges_only cur.succ) ;
  ) node_ht ;

  (* Step 6
   * ~~~~~~
   * Spread wild pointers.
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 6  (spread WILD)\n") ;
  (* Now "update" becomes more complicated because we can do some
   * consistency checks. A successful update also tells us to loop again.
   * A more efficient algorithm would use a true worklist. *)
  let update n k w = begin
    if (k <> n.kind) then begin
      assert(n.why_kind <> UserSpec) ; (* shouldn't override those! *)
      n.kind <- k ; 
      n.why_kind <- w ;
      (* ignore (E.warn "Update %d to %a" n.id d_opointerkind k) ; *)
      finished := false 
    end
  end in
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* actually, only look at wild nodes now! *)
      if (cur.kind = Wild) then begin
        (* consider all of the successor edges *)
        List.iter (fun e -> 
          if e.eto.kind = ROString then begin 
            () (* WILD->ROSTRING OK *)
          end else if e.eto.kind <> Wild && e.eto.why_kind = UserSpec then begin
            E.s (E.bug "Solver: bad annotation (should be wild because of successor edge)@!%a@!%a" d_node e.eto d_node e.efrom)
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
            E.s (E.bug "Solver: bad annotation (should be wild because of predecessor edge)@!%a@!%a" d_node e.efrom d_node e.eto)
          end else begin
            (if (e.efrom.why_kind = UserSpec) then assert (e.efrom.kind = Wild)) ;
            update e.efrom Wild (SpreadFromEdge cur) ;
          end
        ) cur.pred ;

        (* handle points-to information *)
        List.iter (fun n ->
          if n.kind <> Wild && set_outside n then begin
            E.s (E.bug "Solver: bad annotation (should be wild because of pointsto)@!%a@!%a" d_node n d_node cur)
          end ;
          (if (n.why_kind = UserSpec) then assert (n.kind = Wild)) ;
          update n Wild (SpreadPointsTo cur) ;
        ) cur.pointsto ;
      end
    ) node_ht 
  done ;

  (* Step 7
   * ~~~~~~
   * Attempt to SEQ-FSEQ-INDEX nodes in a unified fashion. 
   *)
  let can_reach_string n = hasFlag n pkReachString in
  let can_reach_seq n = hasFlag n pkReachSeq in
  let can_reach_index n = hasFlag n pkReachIndex in 

  if show_steps then ignore (Pretty.printf "Solver: Step 7  (unified SEQ handling)\n") ;

  (* this helper function picks the right kind of Sequence for a node *)
  let pick_the_right_kind_of_seq n base why = begin
    let final = match base, (can_reach_string n) with
      Seq, true -> SeqN   (* does it need to be null-terminated? *)
    | FSeq, true -> FSeqN
    | _ -> base
    in 
    if (set_outside n && n.kind <> final) then begin
      (* consistency check *)
      E.s (E.bug "Solver: would override@!%a@!with %a %a"
        d_node n d_opointerkind final d_whykind why)
    end  ;
    (* to prevent infinite loops in this portion of the show, we assume
     * that INDEX wins over SEQ/FSEQ and SEQ wins over FSEQ. *)
    if ((n.kind = Seq || n.kind = SeqN) &&
        (final = FSeq || final = FSeqN)) || 
        (n.kind = Index) then
      () (* do nothing, we already have a more powerful kind *)
    else 
      update n final why
  end in 

  finished := false ; 
  while not !finished do 
    finished := true ;
    (* consider every node in the graph! *)

    Hashtbl.iter (fun id cur -> 
      (* is this node "innately" Seq/FSeq/Index? *)
      if (hasFlag cur pkPosArith || hasFlag cur pkIntCast ||
          hasFlag cur pkArith) && cur.kind <> Wild &&
          not (set_outside cur) then begin
          let base = 
            if can_reach_index cur then Index
            else if can_reach_seq cur || hasFlag cur pkArith then Seq
            else FSeq 
          in 
          pick_the_right_kind_of_seq cur base BoolFlag
      end ;

      (* consider all the successor edges of this node that might cause
       * this node to be SEQ-ish *)
      if not (set_outside cur) then 
      List.iter (fun e ->
        (* This is basically a condensed version of the three versions of
         * this code that I wrote originally. The old versions are
         * available at the bottom of this file. With all of the conditions
         * in one place you should have an easier time changing things
         * if I'm not around. *)
        match e.ekind, e.eto.kind with
          ECast, FSeqN 
        | ECompat, FSeq
        | ECompat, FSeqN -> 
            (pick_the_right_kind_of_seq cur FSeq (SpreadFromEdge e.eto))
        | ECast, String
        | ECast, FSeqN
        | ECast, SeqN ->
            if is_array e.eto || e.eto.kind = SeqN then 
              (pick_the_right_kind_of_seq cur Seq (SpreadFromEdge e.eto))
        | ECompat, Seq
        | ECompat, SeqN ->
            (pick_the_right_kind_of_seq cur Seq (SpreadFromEdge e.eto))
        | ECompat, Index
        | ECast, Index ->
            (pick_the_right_kind_of_seq cur Index (SpreadFromEdge e.eto))
        | _ -> () 
      ) cur.succ ;

      (* also handle all ECast edges, apply the sequence_condition *)
      if cur.kind <> Wild then 
        List.iter (fun e -> 
          let t_from, t_to = e.efrom.btype, e.eto.btype in 
          if (not (subtype t_from Safe t_to Safe)) &&  
             sequence_condition t_from t_to then begin
              pick_the_right_kind_of_seq e.eto Seq (BadCast e) ;
              pick_the_right_kind_of_seq e.efrom Seq (BadCast e)
          end
        ) (ecast_edges_only cur.succ) ;

      (* consider all the predecessor edges *)
      if not (set_outside cur) then 
      List.iter (fun e -> 
        match e.ekind, e.efrom.kind with 
          ENull, FSeq  | ENull, FSeqN
        | EIndex, FSeq | EIndex, FSeqN -> 
            if (cur.kind <> ROString) then 
              (pick_the_right_kind_of_seq cur FSeq (SpreadFromEdge e.efrom))
        | ESafe, FSeqN
        | ESafe, String ->
            if (not ((is_array e.efrom) || (hasFlag cur pkArith))) &&
               (hasFlag cur pkPosArith || e.efrom.kind = FSeqN) &&
               (cur.kind <> ROString) then 
              (pick_the_right_kind_of_seq cur FSeq (SpreadFromEdge e.efrom))
        | ECompat, Seq | ECompat, SeqN
        | ECast, Seq   | ECast, SeqN 
        | ENull, Seq   | ENull, SeqN -> 
            if (cur.kind <> ROString) then 
              (pick_the_right_kind_of_seq cur Seq (SpreadFromEdge e.efrom))
        | EIndex, _ ->
             if not (can_reach_index cur) && not (set_outside cur) && 
                cur.kind <> Wild && cur.kind <> FSeq && cur.kind <> FSeqN then 
              (pick_the_right_kind_of_seq cur Seq (SpreadFromEdge e.efrom))
        | ECast, Index
        | ECompat, Index
        | ENull, Index
        | EIndex, Index -> 
              (pick_the_right_kind_of_seq cur Index (SpreadFromEdge e.efrom))
        | _ -> () 
      ) cur.pred ;

      (* consider points-to information *)
      (match cur.kind with
        FSeqN | SeqN | Index -> 
          begin
          match nodeOfAttrlist (typeAttrs cur.btype) with
            Some(n) -> 
              if not (set_outside n) then 
                pick_the_right_kind_of_seq n cur.kind (SpreadToArrayFrom cur) 
          | None -> ()
          end
      | _ -> () 
      );

    ) node_ht ; 
  done ;

  (* Step 9
   * ~~~~~~
   * Change certain FSeqNs to ROStrings. An FSeqN with only one successor
   * becomes an ROString if that successor is an ROString. 
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 8  (FSEQN->ROSTRING)\n") ;
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* consider all succ edges *)
      List.iter (fun e -> 
        if e.ekind = ECast && e.eto.kind = ROString &&
            cur.kind = FSeqN && not (hasFlag cur pkUpdated) &&
            (List.length cur.succ) = 1 then begin 
            assert(cur.why_kind <> UserSpec) ; 
            update cur ROString (SpreadFromEdge e.eto) ;
        end
      ) cur.succ
  ) node_ht ;
  done ;


  (* Step 11
   * ~~~~~~~
   * All other nodes are safe. 
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 9  (SAFE)\n") ;
  Hashtbl.iter (fun id n -> 
    if n.kind = Unknown then begin
      assert(n.why_kind <> UserSpec) ;
      (update n Safe Unconstrained)
    end
  ) node_ht ;

end

(* Old Solver Code: *)

(*
  if show_steps then ignore (Pretty.printf "Solver: Step 7  (FSEQ[N] nodes)\n") ;
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* is this node "innately" FSeq? *)
      if (not (can_reach_seq cur || can_reach_index cur)) &&
         (hasFlag cur pkPosArith || hasFlag cur pkIntCast) &&
         cur.kind <> Wild && 
         not(set_outside cur) then begin
         if can_reach_string cur then begin
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
          assert(not(can_reach_seq cur)) ;
          assert(not(can_reach_index cur)) ;
          assert(not(cur.kind = Wild)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if (e.eto.kind = FSeqN) then assert(cur.can_reach_string) ; 
          if can_reach_string cur then begin
            (update cur FSeqN (SpreadFromEdge e.eto)) ;
          end else begin
            (update cur FSeq (SpreadFromEdge e.eto))
          end
        end
      )  cur.succ ;
      (* consider all pred edges *)
      List.iter (fun e -> 
        if (e.ekind = ECast || e.ekind = ENull || e.ekind = EIndex) &&
           (e.eto.kind = FSeq || e.eto.kind = FSeqN) && 
           (* WEIMER: this line cannot possibly be correct! e.eto = cur *)
           not (set_outside cur) &&
           cur.kind <> ROString then begin
          assert(not(can_reach_seq cur)) ;
          assert(not(can_reach_index cur)) ;
          assert(not(cur.kind = Wild)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if can_reach_string cur then
            (update cur FSeqN (SpreadFromEdge e.efrom))
          else
            (update cur FSeq (SpreadFromEdge e.efrom))
        end ;
        if e.ekind <> ESafe && 
           (e.efrom.kind = String || e.efrom.kind = FSeqN) &&
           (not ((is_array e.efrom) || (hasFlag cur pkArith))) &&
           (hasFlag cur pkPosArith || e.efrom.kind = FSeqN) && 
           (not(set_outside cur)) && 
           cur.kind <> ROString then begin
          assert(not(can_reach_seq cur)) ;
          assert(not(can_reach_index cur)) ;
          assert(not(cur.kind = Wild)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if can_reach_string cur then
            (update cur FSeqN (SpreadFromEdge e.efrom))
          else
            (update cur FSeq (SpreadFromEdge e.efrom))
        end
      ) cur.pred ;
      
      (if (cur.kind = FSeqN) then 
        match nodeOfAttrlist (typeAttrs cur.btype) with
          Some(n) -> 
            assert(not(can_reach_seq n)) ;
            assert(not(can_reach_index n)) ;
            assert(n.why_kind <> UserSpec) ; 
            assert(n.kind <> Wild) ; 
            assert(can_reach_string n) ;
            update n FSeqN (SpreadToArrayFrom cur)
        | None -> ()) 
    ) node_ht
  done ;

  (* Step 8
   * ~~~~~~
   * Attempt to make SEQ[N] nodes. 
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 8  (SEQ[N])\n") ;
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* if it would have been an FSEQ, but for that pesky user annotation *)
      if can_reach_seq cur && (not(can_reach_index cur)) &&
         (hasFlag cur pkPosArith || hasFlag cur pkIntCast) && 
         not (set_outside cur) && cur.kind <> Wild then begin
         assert(cur.why_kind <> UserSpec) ; 
         if can_reach_string cur then
           (update cur SeqN BoolFlag)
         else
           (update cur Seq BoolFlag)
      end ;
      (* if it is a natural seq pointer ... *)
      if (hasFlag cur pkArith) && 
         (not(can_reach_index cur)) &&
         not (set_outside cur) && cur.kind <> Wild then begin
         assert(cur.why_kind <> UserSpec) ; 
         if can_reach_string cur then
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
          assert(not(can_reach_index cur)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if can_reach_string cur || e.eto.kind = SeqN then
           (update cur SeqN (SpreadFromEdge e.eto))
          else
           (update cur Seq (SpreadFromEdge e.eto))
        end ;
        if (e.ekind = ECompat && (e.eto.kind = Seq || e.eto.kind = SeqN)) && 
          not (set_outside cur) then begin
          assert(cur.kind <> Wild) ;
          assert(not(can_reach_index cur)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if can_reach_string cur then
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
        if subtype t_from Safe t_to Safe then begin
          ()
        end else if sequence_condition t_from t_to then begin
            (* the magic seq-making cast *)
            let mark n m = begin
              if (set_outside n) then
                ()
              else begin
                assert(n.kind <> Wild) ;
                assert(not(can_reach_index n)) ;
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
          assert(not(can_reach_index cur)) ;
          assert(cur.why_kind <> UserSpec) ; 
          if can_reach_string cur then
           (update cur SeqN (SpreadFromEdge e.efrom))
          else
           (update cur Seq (SpreadFromEdge e.efrom))
        end ;

        if e.ekind = EIndex && 
           not (can_reach_index cur) && 
           not (set_outside cur) && 
           cur.kind <> Wild && 
           cur.kind <> FSeq &&
           cur.kind <> FSeqN then begin
          assert(cur.why_kind <> UserSpec) ; 
          if can_reach_string cur then
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
  *)

(*
  (* Step 10
   * ~~~~~~~
   * Note all index nodes.
   *)
  if show_steps then ignore (Pretty.printf "Solver: Step 10 (INDEX)\n") ;
  finished := false ; 
  while not !finished do 
    finished := true ; 
    (* consider every node in the graph! *)
    Hashtbl.iter (fun id cur -> 
      (* if it would have been an [F]SEQ, but for that pesky user annotation *)
      if (can_reach_index cur) &&
         (hasFlag cur pkPosArith || hasFlag cur pkIntCast) && 
         not (set_outside cur) && cur.kind <> Wild then begin
         assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
         assert(not(can_reach_seq cur) || (can_reach_seq cur && can_reach_index cur)) ;
         (update cur Index BoolFlag)
      end ;
      (* if it is a natural seq pointer ... *)
      if (hasFlag cur pkArith) && (can_reach_index cur) &&
         not (set_outside cur) && cur.kind <> Wild then begin
         assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
         assert(not(can_reach_seq cur) || (can_reach_seq cur && can_reach_index cur)) ;
         (update cur Index BoolFlag)
      end ;

      (* consider all succ nodes *)
      List.iter (fun e -> 
        if (e.ekind = ECompat || e.ekind = ECast) && (e.eto.kind = Index) && 
          not (set_outside cur) then begin
          assert(cur.kind <> Wild) ;
          assert(not(can_reach_seq cur) || (can_reach_seq cur && can_reach_index cur)) ;
          assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
          update cur Index (SpreadFromEdge e.eto)
        end
      ) cur.succ ;

      (* handle all pred nodes *)
      List.iter (fun e ->
        if (e.ekind = ECast || e.ekind = ECompat || e.ekind = ENull || e.ekind = EIndex) &&
          e.efrom.kind = Index && not (set_outside cur) then begin
          assert(cur.kind <> Wild) ;
          assert(not(can_reach_seq cur) || (can_reach_seq cur && can_reach_index cur)) ;
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
              assert(not(can_reach_seq n) || (can_reach_seq n && can_reach_index n)) ;
              assert(cur.why_kind <> UserSpec || cur.kind = Index) ; 
              update n Index (SpreadToArrayFrom cur)
            end
        | None -> () 
      end ;
    ) node_ht
  done ;
  *)
