(*
 * My second attempt at constraint solving for the pointer-qualifier graph.
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

(* the "height" of a given kind. If X <- Y then we must have height(X) >=
 * height(Y). *)
let height k = 
  match k with
    Unknown -> 0
  | Index -> 1
  | SeqN -> 2
  | Seq -> 3
  | FSeqN -> 4
  | FSeq -> 5
  | Safe -> 6
  | String -> 7
  | Wild -> 8
  | ROString -> 9
  | Scalar -> 10

let solve (node_ht : (int,node) Hashtbl.t) = begin
  (* _(1)_ Set all of the little boolean flags correctly. Use whatever
   * method simplesolve uses *)
  Simplesolve.set_flags node_ht ; 

  (* helper functions *)
  let ecast_edges_only l = List.filter (fun e -> e.ekind = ECast) l in
  let non_safe_edges_only l = List.filter (fun e -> e.ekind <> ESafe) l in 
  let ecastandenull_edges_only l = 
    List.filter (fun e -> e.ekind = ECast || e.ekind = ENull) l in
  let eCNI_edges_only l = List.filter (fun e -> e.ekind = ECast || 
    e.ekind = ENull || e.ekind = EIndex) l in
  let eNI_edges_only l = List.filter (fun e ->
    e.ekind = ENull || e.ekind = EIndex) l in

  for i = 1 to 2 do begin
  let update_kind n k why = 
    if (height k) > (height n.kind) then begin
      if (n.why_kind = UserSpec || n.locked) then begin
        (if (!warn) then ignore (E.warn "Pointer Kind Inference would upgrade to %a for\n%a" d_pointerkind k d_node n)) ;
        false
      end else begin
        n.kind <- k ;
        n.why_kind <- why ;
        ( if i = 1 then n.locked <- true ) ;
        true
      end
    end else false
  in

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

  let finished = ref false in 

  let update a b c = 
    let true_b = 
      if (b = Seq || b = FSeq) && a.can_reach_string then
        match b with
          Seq -> SeqN
        | FSeq -> FSeqN
        | _ -> E.s (E.bug "update")
      else b 
    in 
    if update_kind a true_b c then begin
      finished := false
    end
  in 

  let rec mark_string a = 
    if not (a.can_reach_string) then begin
        a.can_reach_string <- true ;
        List.iter (fun e -> 
          if (e.ekind = ECast || e.ekind = ENull || e.ekind = EIndex) then
              mark_string e.efrom
          ) a.pred
    end
  in 

  if i = 2 then 
  (* _(2)_ mark all interface char * nodes as [ro]strings and mark all of
   * the posarith/intcast pointers as seq/fseq *)
  Hashtbl.iter (fun id n -> 
    if n.interface (* && not (n.arith || n.posarith) *) && 
       is_char_pointer n then begin
      if (n.updated || (List.length n.succ) <> 0) then
        ignore (update_kind n String BoolFlag )
      else
        ignore (update_kind n ROString BoolFlag )
    end
  ) node_ht ;

  if i = 2 then 
  Hashtbl.iter (fun id n ->
    if n.kind = String || n.kind = ROString then mark_string n
  ) node_ht ;

  if i = 2 then 
  Hashtbl.iter (fun id n -> 
    if n.posarith || (n.null && n.intcast) then 
      (update n FSeq BoolFlag)
    else if (n.arith || n.intcast) then
      (update n Seq BoolFlag)
  ) node_ht ;

  (* _(3)_ Now we do wild pointers *)
  if i = 2 then 
  Hashtbl.iter (fun id cur ->
    (* pick out all successors of our current node *)
    List.iter (fun e -> 
      let n_from, n_to = e.efrom, e.eto in
      let t_from, t_to = e.efrom.btype, e.eto.btype in 
      if Simplesolve.subtype t_from Safe t_to Safe then begin
        ()
      end else if Simplesolve.subtype t_from Safe 
          (TArray(t_to,(Some(Const(CInt(1024,ILong,None)))),[])) Safe then begin
        (update e.efrom Seq (BadCast e));
        (update e.eto Seq (BadCast e));
      end else if Simplesolve.is_p n_from n_to ||
         Simplesolve.is_p n_to n_from then begin
        ()
      end else begin
        (* must be wild! *)
        (update e.efrom Wild (BadCast e));
        (update e.eto Wild (BadCast e));
      end
    ) (ecast_edges_only cur.succ) ;
  ) node_ht ;

  (* our wild-rule helper function: for n1 and n3 to be compatible *)
  let wild_rule n1 n3 = 
    let internal_wild_rule n1 n3 = 
      if (n1.kind = Wild) || (n1.kind = Index) || (n1.kind = FSeq) ||
          (n1.kind = Seq) || (n1.kind = SeqN) || (n1.kind = FSeqN) then
          update n3 n1.kind (SpreadFromEdge n1)
    in
    internal_wild_rule n1 n3 ;
    internal_wild_rule n3 n1 
  in

  (* _(4)_
   * Now we have all of the "basic kinds" in the graph. Must must propagate
   * them to the whole graph and resolve conflicts. *)
  while not !finished do 
    finished := true ; 
    (* consider every edge in the graph! *)
    Hashtbl.iter (fun id cur -> 

      (* handle successor edges *)
      List.iter (fun e -> 
        (if cur.kind = Wild then update e.eto Wild (SpreadFromEdge cur)) ;
        (if cur.kind <> Wild && e.ekind = ECast && e.eto.kind <> Wild then begin
          (* "The Wild Rule"
           * int * 1 * 2 x;
           * int * 3 * 4 y;
           * x=y;
           * The types int * 1 and int * 3 must be compatible. Either both
           * must be wild, or both must be index, or both must be safe. *)
           match (nodeOfAttrlist (typeAttrs cur.btype)),
                 (nodeOfAttrlist (typeAttrs e.eto.btype)) with
             Some(n1),Some(n3) -> wild_rule n1 n3
           | _ -> ()
        end); 
        (if e.ekind = ECast &&
            (e.eto.kind = String || 
             e.eto.kind = FSeqN || e.eto.kind = SeqN) then
            update cur (if is_array e.eto then SeqN else e.eto.kind) (SpreadFromEdge e.eto));
        (if e.ekind = ECast && e.eto.kind = ROString &&
            cur.kind = FSeqN && (List.length cur.succ) = 1 then update cur ROString (SpreadFromEdge e.eto)) ;
      ) cur.succ ;

      List.iter (fun e -> 
        if e.ekind = ECast || e.ekind = ENull then
           wild_rule cur e.eto 
      ) cur.succ;

      (* handle predecessor edges *)
      List.iter (fun e -> 
        (if cur.kind = Wild then update e.efrom Wild (SpreadFromEdge cur)) ;
        (if e.ekind = EIndex then update cur Seq (SpreadFromEdge e.efrom)) ;
        (if (e.ekind = ECast || e.ekind = ENull || e.ekind = EIndex) &&
            (e.efrom.kind = Seq || e.efrom.kind = FSeq || e.efrom.kind = Index) then
            update cur e.efrom.kind (SpreadFromEdge e.efrom)) ;
        (if (e.ekind <> ESafe) && 
            (e.efrom.kind = String || e.efrom.kind = FSeqN || e.efrom.kind = SeqN) then
            update cur (if is_array e.efrom || cur.arith then SeqN 
                        else if cur.posarith then FSeqN else
                        e.efrom.kind) 
              (SpreadFromEdge e.efrom));
      ) cur.pred ;

      (* handle points-to information *)
      List.iter (fun n ->
        (if cur.kind = Wild then update n Wild (SpreadPointsTo cur)) ;
      ) cur.pointsto ;

      (* push wild *)
      if (cur.kind = Wild) then begin
        match nodeOfAttrlist (typeAttrs cur.btype) with
          Some(n) -> update n Wild (SpreadPointsTo cur)
        | None -> ()
      end ;

      (* handle array types *)
      if (cur.kind = Index || cur.kind = FSeqN || cur.kind = SeqN) then begin
        match nodeOfAttrlist (typeAttrs cur.btype) with
          Some(n) -> update n cur.kind (SpreadToArrayFrom cur)
        | None -> () 
      end ;
    ) node_ht
  done ;


  (* _(5)_
   * All other nodes are safe. *)
  if i = 2 then
  Hashtbl.iter (fun id n -> 
    if n.kind = Unknown then begin
      ignore (update_kind n Safe Unconstrained)
    end
  ) node_ht ;
  end done ; (* for i = 1 to 2 do *) 

  (* Check! *)
  Hashtbl.iter (fun id n ->
    List.iter (fun e -> 
      (if n.kind = Safe && e.eto.kind = FSeqN && e.ekind <> EIndex then
        ignore (E.s (E.bug "I can see SAFE->FSEQN! : %a" d_ekind e.ekind )));
      (*
      (if n.kind = String && e.eto.kind = FSeqN then
        ignore (E.s (E.bug "I can see STRING->FSEQN! : %a" d_ekind e.ekind )));
      *)
    ) (n.succ) ;
  ) node_ht;

end
