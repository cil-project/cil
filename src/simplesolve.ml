(*
 * A model that implements simple constraint solving for the
 * pointer-qualifier graph. Assigning "wild" to everything is a trivial
 * solution: we'll try to do better than that.
 *
 * Main idea: everything starts out "safe" and we use a worklist algorithm
 * to push little boolean flags around the graph. 
 *
 * Next we inspect invalid casts and make wild pointers. This may involve
 * examining the types of pointers. 
 *
 * Then we inspect pointer arithmetic and make index pointers.
 *
 * Then we inspect casts from ints and make sequence pointers.
 *
 * Everything that survives is safe. 
 *)
open Cil
open Ptrnode
open Solveutil

(* see infer.tex : this predicate checks to see if the little attributes
 * match when casting *)
let q_predicate (n1 : node) (n2 : node) = true
(*  ((not n1.onStack) || (n2.onStack)) && 
    ((n1.updated) || (not n2.updated)) && 
    ((not n1.null) || (n2.null)) && 
    ((not n1.intcast) || (n2.intcast)) *)

(* returns a pair of opointerkinds p1,p2 such that if we assign the
 * qualifiers t1=p1 and t2=p2, the cast is legal *)
let can_cast (n1 : node) (n2 : node) = begin
  let t1 = n1.btype in
  let t2 = n2.btype in 
  if subtype t1 Safe t2 Safe then
    (Safe,Safe,false)
  else if subtype t1 Safe 
     (TArray(t2,(Some(Const(CInt32(Int32.of_int 1024,ILong,None)))),
             [])) Safe then
    (Seq,Seq,false)
  else begin
(*    E.warn "Cannot cast %a <= %a\n" 
      d_type (unrollType t1 )
      d_type (unrollType t2 ) ; *)
  if is_p n1 n2 || is_p n2 n1 then
    (Safe,Safe,true)
  else 
    (Wild,Wild,false)
  end
end

let set_flags (node_ht : (int,node) Hashtbl.t) = begin
  (* Setup:
   *        int *x,*y,*z;
   *        x = y;
   *        y = z;
   *
   * Gives:
   *        [x] <- [y] <- [z]
   *)
  let finished = ref false in (* we repeat until things settle down *)

  while not !finished do 
    finished := true ; 
    Hashtbl.iter (fun id cur ->
    (* In the above example, if we write through y we have also written
     * through z. Imagine that y is the formal parameter to a function that
     * has been declared "readonly". We cannot "get around the readonly" by
     * assigning y=z and writing through y. *)
    if (cur.updated) then begin
      (* mark all of the predecessors of y with "updated" *)
      List.iter (fun e -> 
        if e.ekind = ECast then 
        if not e.efrom.updated then begin
          e.efrom.updated <- true ;
          finished := false ; 
        end) cur.pred
    end ;

    (* Now imagine that y might contain a stack address. In that case, x 
     * may also contain a stack address. *)
    if (cur.onStack) then begin
      (* mark all of the successors of y with "updated" *)
      List.iter (fun e -> 
        if e.ekind = ECast then 
        if not e.eto.onStack then begin
          e.eto.onStack <- true ;
          finished := false ; 
        end) cur.succ
    end ;

    (* Now imagine that the number zero might be stored in y. Then the
     * zero might be stored in x as well. *)
    if (cur.null) then begin
      (* mark all of the successors of y with "null" *)
      List.iter (fun e -> 
        if e.ekind = ECast then 
        if not e.eto.null then begin
          e.eto.null <- true ;
          finished := false ; 
        end) cur.succ
    end ;

    (* Similarly, if some int might be stored in y. Then
     * some in might be stored in x as well. *)
    if (cur.intcast) then begin
      (* mark all of the successors of y with "intcast" *)
      List.iter (fun e -> 
        if e.ekind = ECast then 
        if not e.eto.intcast then begin
          e.eto.intcast <- true ;
          finished := false ;
        end) cur.succ
    end ;

    (* Now, if we have y[e], we'll probably want to represent y as an index
     * or wild pointer. That does not mean that x or z have to be index or
     * wild pointers -- we could have some sort of magical cast. In
     * particular, we'll just say that z has that flag as well.
     *)
    if (cur.posarith) then begin
      (* mark all of the predecessors of y with "posarith" *)
      List.iter (fun e -> 
        if e.ekind = ECast then 
        if not e.efrom.posarith then begin
          e.efrom.posarith <- true ;
          finished := false ;
        end) cur.pred
    end ;

    (* Similar deal here for arbitrary arithmetic. *)
    if (cur.arith) then begin
      (* mark all of the predecessors of y with "arith" *)
      List.iter (fun e -> 
        if e.ekind = ECast then 
        if not e.efrom.arith then begin
          e.efrom.arith <- true ;
          finished := false 
        end) cur.pred
    end 
    ) node_ht
  done ;
end

(* this is the heart of the Simple Solver
 * currently it is very un-optimized! *)
let solve (node_ht : (int,node) Hashtbl.t) = begin

  ignore (E.log "Solving constraints\n");

  (* returns true if k2 is "farther from safe" than k1: in particular, this
   * tells us if we should do an update of the kind of some node from k1 to
   * k2 *)
  let moving_up k1 k2 =
    if k1 = k2 || k2 = Unknown then false
    else match k1 with
      Wild -> false
    | Safe | Unknown -> true
    | ROString -> not (k2 = Safe)
    | String -> not (k2 = Safe) && not (k2 = ROString)
    | FSeq | FSeqN -> not (k2 = Safe) && not (k2 = ROString)
    | Seq | SeqN -> not (k2 = Safe || k2 = FSeq || k2 = FSeqN) && not (k2 = ROString)
    | Index -> k2 = Wild
    | _ -> E.s (E.bug "cannot handle %a in simplesolve" d_opointerkind k1)
  in

  (* filters an edgelist so that it contains only ECast edges *)
  let ecast_edges_only l = List.filter (fun e -> e.ekind = ECast) l in
  let non_safe_edges_only l = List.filter (fun e -> e.ekind <> ESafe) l in 
  let ecastandenull_edges_only l = 
    List.filter (fun e -> e.ekind = ECast || e.ekind = ENull) l in
  let eCNI_edges_only l = List.filter (fun e -> e.ekind = ECast || 
    e.ekind = ENull || e.ekind = EIndex) l in
  let eNI_edges_only l = List.filter (fun e ->
    e.ekind = ENull || e.ekind = EIndex) l in


  (* set all of the boolean flags correctly *)
  set_flags node_ht ; 

  (* OK, now we have all of those attributes propagated. Now it's time to
   * assign qualifiers to the pointers. *)
  (* Step 1: Look at all of the arrows and see if any are "bad casts".*)
  let update_kind n k why =
    if (moving_up n.kind k)  then begin
        if (n.why_kind = UserSpec) then begin
          ignore (E.warn "Pointer Kind Inference would upgrade to %a for\n%a" d_opointerkind k d_node n) ;
          false
        end else begin
          n.kind <- k ;
          n.why_kind <- why ;
          true
        end
    end else false
  in

  let finished = ref false in (* we repeat until things settle down *)

  Hashtbl.iter (fun id cur ->
    (* pick out all successors of our current node *)
    List.iter (fun e -> 
      let (k1,k2,f) = can_cast e.efrom e.eto in
      let why = if f then 
                  PolyCast(e)
                else
                  BadCast(e) 
        in
      ignore (update_kind e.eto k1 why) ;
      ignore (update_kind e.efrom k2 why) ;
    ) (ecast_edges_only cur.succ) ;
    ) node_ht ;

  (* now take all of our wild nodes and make sure that all of their
   * successors, predecessors and points-to nodes are wild. *)
  finished := false ; 
  while not !finished do 
    finished := true ; 
    Hashtbl.iter (fun id cur ->
    if (cur.kind = Wild) then begin
      (* mark all of the succ/pred/pointsto of y with "wild" *)
      let why = SpreadFromEdge(cur) in
      let f = (fun n -> if (update_kind n Wild why) then finished := false) in
      let contaminated_list = 
        (List.map (fun e -> e.eto) cur.succ  ) @
        (List.map (fun e -> e.efrom) cur.pred ) in
      List.iter f contaminated_list ;
      let why = SpreadPointsTo(cur) in
      let f = (fun n -> if (update_kind n Wild why) then finished := false) in
      let contaminated_list = cur.pointsto in
      List.iter f contaminated_list ;
      match nodeOfAttrlist (typeAttrs cur.btype) with
          Some(n) -> f n
        | None -> ()
    end 
    ) node_ht
  done ;

  (* now take all of the posarith/intcast pointers and make them seq/fseq *)
  Hashtbl.iter (fun id cur ->
    (* arithmetic can make something an index *)
    if (cur.posarith || (cur.null && cur.intcast)) then begin
      ignore (update_kind cur FSeq BoolFlag)
    end else 
    if (cur.arith || cur.intcast ) then begin
      ignore (update_kind cur Seq BoolFlag)
    end ;
    (* being the target of an EIndex edge can as well *)
    if (cur.kind <> FSeq) then 
    List.iter (fun e -> 
      if e.ekind = EIndex then ignore
        (update_kind cur Seq (SpreadFromEdge(e.efrom)))
      ) cur.pred ;
    ) node_ht ;

  (* now spread all of the seq/index pointers back along ECast edges *)
  finished := false ; 
  while not !finished do 
    finished := true ; 
    Hashtbl.iter (fun id cur ->
    if (cur.kind = Seq || cur.kind = FSeq || 
        cur.kind = Index) then begin
      (* mark all of the predecessors of y along ECast with "index" *)
      let why = SpreadFromEdge(cur) in
      let f = (fun n -> if (update_kind n cur.kind why) then 
                          finished := false) in
      let contaminated_list = 
        (List.map (fun e -> e.efrom) (eCNI_edges_only cur.pred)) (* @ 
        (List.map (fun e -> e.eto) (ecast_edges_only cur.succ) ) *) in
      List.iter f contaminated_list ;
      (* mark all cast edges at least equal to this! *)

    end) node_ht
  done ;

  (* helper function: does a node represent a pointer to a character? *)
  let is_char_pointer n =
    match n.btype with
      TInt(IChar,_) -> true
    | TInt(ISChar,_) -> true
    | TInt(IUChar,_) -> true
    | _ -> false
  in

  let is_array n =
    match n.btype with
      TArray(_) -> true
    | _ -> false
  in

  (* mark all interface char * nodes with no arith as string *)
  Hashtbl.iter (fun id n -> 
    if n.interface (* && not (n.arith || n.posarith) *) && 
       is_char_pointer n then begin
      ignore (update_kind n String BoolFlag)
    end) node_ht ;

  (* push the String type backward so that it reaches buffers *)
  finished := false ; 
  while not !finished do 
    finished := true ; 
    Hashtbl.iter (fun id cur ->
    if (cur.kind = String (* || cur.kind = ROString *) ||  cur.kind = FSeqN || cur.kind = SeqN) then begin
      (* mark all of the predecessors of y along ECast with "String" *)
      let why = SpreadFromEdge(cur) in
      let f = (fun n -> 
        if (if n.kind = FSeq then
              update_kind n FSeqN why 
            else if n.kind = Seq then
              update_kind n SeqN why
            else update_kind n (if is_array n then SeqN else cur.kind) why) then
              finished := false) in
      let contaminated_list = 
        (List.map (fun e -> e.efrom) (non_safe_edges_only cur.pred)) @
        (List.map (fun e -> e.eto) (ecast_edges_only cur.succ))   
        in
      List.iter f contaminated_list ;
      (* mark all cast edges at least equal to this! *)

    end) node_ht
  done ;

  (* push back SeqN, FSeqN, Index back to array types. *)
  finished := false ; 
  while not !finished do 
    finished := true ; 
    Hashtbl.iter (fun id cur ->
    if (cur.kind = Index || cur.kind = FSeqN || cur.kind = SeqN) then begin
      (* mark all array types *) 
      let why = SpreadToArrayFrom(cur) in
      let f = (fun n -> 
            if update_kind n cur.kind why then
              finished := false) in
      match nodeOfAttrlist (typeAttrs cur.btype) with
          Some(n) -> f n
        | None -> ()
    end) node_ht
  done ;

  (* all otherwise unconstrained nodes become safe *)
  Hashtbl.iter (fun id n -> 
    if n.kind = Unknown then begin
      n.kind <- Safe ;
      n.why_kind <- Unconstrained 
    end (* else if n.kind = String && not n.updated then begin
      n.kind <- ROString ;
    end *) ) node_ht ;

  ignore (E.log "Finished solving constraints\n");

end

      (* 
      Unknown -> true
    | Safe -> true
    | FSeq -> k2 = Seq || k2 = Index || k2 = Wild || k2 = String
    | Seq -> k2 = Index || k2 = Wild || k2 = String || k2 = 
    | FSeqN | SeqN -> k2 = Index || k2 = Wild || k2 = String
    | Index -> k2 = Seq || k2 = Wild
    | String -> k2 = Wild || k2 = Seq || k2 = Index
    | Wild -> false
    | Scalar -> E.s (E.bug "cannot handle scalars in simplesolve")
    *)
