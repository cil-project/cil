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

(* are the given two types congurent? see infer.tex 
 * also remember that two wild pointers are always considered congruent *)
let rec type_congruent (t1 : typ) (q1 : pointerkind) 
                       (t2 : typ) (q2 : pointerkind) = begin
  (* t[n] and struct { t ; t[n-1] ; } are congruent *)
  let t1 = unrollType t1 in
  let t2 = unrollType t2 in 
  let array_helper_function t eo al x = begin
    match eo with
      Some(Const(CInt(n,a,b),c)) when n > 1 -> begin
        let our_compinfo = {
          cstruct = true ;
          cname = "" ;
          ckey = -1;
          cfields = [] ;
          cattr = [] ;
        } in
        our_compinfo.cfields <- 
          [ { fcomp = our_compinfo ; fname = "" ;
              ftype = t ;
              fattr = [] ; } ;
            { fcomp = our_compinfo ; fname = "" ;
              ftype = TArray(t,(Some(Const(CInt(n-1,a,b),c))),[]) ;
              fattr = [] ; } ] ; 
        type_congruent t q1 (TComp(our_compinfo)) q2
      end
    | _ -> false
  end in 
  if (q1 = Wild && q2 = Wild) then 
    true
  else match (t1,t2) with
    (* unions can be reordered without loss *)
  | TComp(c1),TComp(c2) when (not c1.cstruct) && (not c2.cstruct) -> begin
      let fields_match l1 l2 = 
        List.for_all (fun l1_elt ->
          List.exists (fun l2_elt -> type_congruent l1_elt.ftype q1 
                                                    l2_elt.ftype q2)
            l2) l1
      in
        (fields_match c1.cfields c2.cfields) &&
        (fields_match c2.cfields c1.cfields)
    end

    (* a structure with one element is equal to that element *)
  | TComp(c1),_ when ((List.length c1.cfields) = 1) ->
    let f1 = List.hd c1.cfields in type_congruent f1.ftype q1 t2 q2 
  | _,TComp(c2) when ((List.length c2.cfields) = 1) ->
    let f2 = List.hd c2.cfields in type_congruent t1 q1 f2.ftype q2

    (* structures match if all of their fields match in order *)
  | TComp(c1),TComp(c2) when (c1.cstruct) && (c2.cstruct) -> 
    (c1.cname = c2.cname) || 
    List.for_all2 (fun f1 f2 -> type_congruent f1.ftype q1 f2.ftype q2) 
      c1.cfields c2.cfields

    (* t and t[1] are the same *)
  | (x,TArray(t,eo,al)) when (type_congruent x q1 t q2) -> begin
    match eo with
      Some(Const(CInt(1,_,_),_)) -> true
    | _ -> false
  end
  | (TArray(t,eo,al),x) when (type_congruent x q2 t q1) -> begin
    match eo with
      Some(Const(CInt(1,_,_),_)) -> true
    | _ -> false
  end

    (* t[n] and struct { t ; t[n-1] ; } are congruent *)
  | (x,TArray(t,eo,al)) -> array_helper_function t eo al x
  | (TArray(t,eo,al),x) -> array_helper_function t eo al x

  | TVoid(_),TVoid(_) -> true
  | TInt(_),TInt(_) -> true
  (* fails to unify bitfields *)
  | TEnum(_),TEnum(_) -> true
  | TFun(_),TFun(_) -> true
  | TPtr(_),TPtr(_) -> true

  | _ -> false
end

(* returns the first n elements of l *)
let rec sublist l n = begin
  if n <= 0 then [] 
  else match l with
      [] -> []
  | hd :: tl -> hd :: (sublist tl (n-1))
end

(* do we have t1,q1 <= t2,q2 (as in infer.tex)? *)
let rec subtype (t1 : typ) (q1 : pointerkind) 
            (t2 : typ) (q2 : pointerkind) =
  let t1 = unrollType t1 in
  let t2 = unrollType t2 in 
  if (type_congruent t1 q1 t2 q2) then
    true
  else match (t1,t2) with 
    (* t1 x t2 x t3 ... <= t1 x t2, general case  *)
    TComp(c1),TComp(c2) when c1.cstruct && c2.cstruct -> begin
      (* is t2 congruent to a prefix of t1? *)
      (* we'll do it the expensive way: try all prefices of t1 *)
      let found_one = ref false in 
      for l = 1 to (List.length c1.cfields) do 
        if (not (!found_one)) then begin
          let prefix_struct_c1 = { c1 with cfields = (sublist c1.cfields l) } in
          if (type_congruent t2 q2 (TComp(prefix_struct_c1)) q1) then
            found_one := true
        end
      done ; !found_one
    end
    (* t1 x t2 <= t1 *)
  | TComp(c1),_ when c1.cstruct -> begin
    (* this is true if t2 is congruent to some prefix of c1, as above *)
      let found_one = ref false in 
      for l = 1 to (List.length c1.cfields) do 
        if (not (!found_one)) then begin
          let prefix_struct_c1 = { c1 with cfields = (sublist c1.cfields l) } in
          if (type_congruent (TComp(prefix_struct_c1)) q1) t2 q2 then
            found_one := true
        end
      done ; !found_one
    end
    (* x <= a + b  iff x <= a && x <= b *)
   | _,TComp(c2) when not c2.cstruct -> begin
      List.for_all (fun elt -> subtype t1 q1 elt.ftype q2) c2.cfields 
   end
    (* a+b <= x    iff a <= x || b <= x *)
   | TComp(c1),_ when not c1.cstruct -> begin
      List.for_all (fun elt -> subtype elt.ftype q1 t2 q2) c1.cfields 
   end

  | _,_ -> false

(* see infer.tex : this predicate checks to see if the little attributes
 * match when casting *)
let q_predicate (n1 : node) (n2 : node) = true
(*  ((not n1.onStack) || (n2.onStack)) && 
    ((n1.updated) || (not n2.updated)) && 
    ((not n1.null) || (n2.null)) && 
    ((not n1.intcast) || (n2.intcast)) *)

let is_malloc n = match n.where with
    PGlob(s),_ when String.contains s '*' -> true
  | _ -> false

(* returns a pair of pointerkinds p1,p2 such that if we assign the
 * qualifiers t1=p1 and t2=p2, the cast is legal *)
let can_cast (n1 : node) (n2 : node) = begin
  let t1 = n1.btype in
  let t2 = n2.btype in 
  if is_malloc n1 then
    (Safe,Safe)
  else if subtype t1 Safe t2 Safe then
    (Safe,Safe)
  else if subtype t1 Safe 
     (TArray(t2,(Some(Const(CInt(1024,ILong,None),locUnknown))),[])) Safe then
    (Seq,Seq)
  else begin
(*    E.warn "Cannot cast %a <= %a\n" 
      d_type (unrollType t1 )
      d_type (unrollType t2 ) ; *)
    (Wild,Wild)
  end
end

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

  let ecast_edges_only l = List.filter (fun e -> e.ekind = ECast) l in

  (* Setup:
   *        int *x,*y,*z;
   *        x = y;
   *        y = z;
   *
   * Gives:
   *        [x] <- [y] <- [z]
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
        if e.ekind = ECast then 
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
        if e.ekind = ECast then 
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
        if e.ekind = ECast then 
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
        if e.ekind = ECast then 
        if not e.eto.intcast then begin
          e.eto.intcast <- true ;
          add_node e.eto
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
          add_node e.efrom
        end) cur.pred
    end ;

    (* Similar deal here for arbitrary arithmetic. *)
    if (cur.arith) then begin
      (* mark all of the predecessors of y with "arith" *)
      List.iter (fun e -> 
        if e.ekind = ECast then 
        if not e.efrom.arith then begin
          e.efrom.arith <- true ;
          add_node e.efrom
        end) cur.pred
    end ;

    worklist := List.tl !worklist 
  done ;

  (* OK, now we have all of those attributes propagated. Now it's time to
   * assign qualifiers to the pointers. *)
  (* Step 1: Look at all of the arrows and see if any are "bad casts".*)
  let update_kind n k why =
    if (n.kind = Unknown) ||
       ((n.kind = Safe) && (k <> Safe)) ||
       ((n.kind = Seq) && (k <> Seq) && (k <> Safe)) ||
       ((n.kind = Index) && (k <> Index) && (k <> Safe)) then begin
        if (n.why_kind = UserSpec) then begin
          ignore (E.warn "Pointer Kind Inference would override user-specified kind for\n%a" d_node n) ;
          false
        end else begin
          n.kind <- k ;
          n.why_kind <- why ;
          true
        end
    end else false
  in

  Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;
  while (!worklist <> []) do
    let cur = List.hd !worklist in
    (* pick out all successors of our current node *)
    List.iter (fun e -> 
      let (k1,k2) = can_cast e.efrom e.eto in
      let why = BadCast(e.efrom.btype,e.eto.btype) in
      ignore (update_kind e.eto k1 why) ;
      ignore (update_kind e.efrom k2 why) ;
    ) (ecast_edges_only cur.succ) ;
    worklist := List.tl !worklist 
  done ;

  (* now take all of our wild nodes and make sure that all of their
   * successors, predecessors and points-to nodes are wild. *)
  Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;
  while (!worklist <> []) do
    (* pick out our current node *)
    let cur = List.hd !worklist in
    if (cur.kind = Wild) then begin
      (* mark all of the succ/pred/pointsto of y with "wild" *)
      let why = SpreadFromEdge(cur) in
      let f = (fun n -> if (update_kind n Wild why) then add_node n) in
      let contaminated_list = 
        (List.map (fun e -> e.eto) cur.succ  ) @
        (List.map (fun e -> e.efrom) cur.pred ) in
      List.iter f contaminated_list ;
      let why = SpreadPointsTo(cur) in
      let f = (fun n -> if (update_kind n Wild why) then add_node n) in
      let contaminated_list = cur.pointsto in
      List.iter f contaminated_list
    end ;
    worklist := List.tl !worklist 
  done ;

  (* now take all of the arith/posarith pointers and make them seq *)
  Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;
  while (!worklist <> []) do
    (* pick out our current node *)
    let cur = List.hd !worklist in
    (* arithmetic can make something an index *)
    if (cur.arith || cur.posarith) then begin
      ignore (update_kind cur Seq BoolFlag)
    end ;
    (* being the target of an EIndex edge can as well *)
    List.iter (fun e -> 
      if e.ekind = EIndex then 
        ignore (update_kind cur Seq (SpreadFromEdge(e.efrom)))
      ) cur.pred ;
    worklist := List.tl !worklist 
  done ;

  (* now spread all of the index pointers back along ECast edges *)
  Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;
  while (!worklist <> []) do
    (* pick out our current node *)
    let cur = List.hd !worklist in
    if (cur.kind = Seq || cur.kind = Index) then begin
      (* mark all of the predecessors of y along ECast with "index" *)
      let why = SpreadFromEdge(cur) in
      let f = (fun n -> if (update_kind n cur.kind why) then add_node n) in
      let contaminated_list = 
        (List.map (fun e -> e.efrom) (ecast_edges_only cur.pred)) in
      List.iter f contaminated_list ;
    end ;
    worklist := List.tl !worklist 
  done ;

  (* now take all of the intcast pointers and make them seq *)
  Hashtbl.iter (fun id n -> all := n :: !all) node_ht ;
  while (!worklist <> []) do
    (* pick out our current node *)
    let cur = List.hd !worklist in
    if (cur.intcast) then begin
      ignore (update_kind cur Seq BoolFlag)
    end ;
    worklist := List.tl !worklist 
  done ;

  (* all otherwise unconstrained nodes become safe *)
  Hashtbl.iter (fun id n -> 
    if n.kind = Unknown then begin
      n.kind <- Safe ;
      n.why_kind <- Unconstrained 
    end) node_ht ;


end

