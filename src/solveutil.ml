(*
 * A set of solver utilities and wrappers. 
 * 
 * This includes implementations of "type_congruent" and "subtype", as well
 * as solver-wrappers.
 *)
open Cil
open Ptrnode

(* are the given two types congurent? see infer.tex 
 * also remember that two wild pointers are always considered congruent *)
let rec type_congruent (t1 : typ) (q1 : opointerkind) 
                       (t2 : typ) (q2 : opointerkind) = begin
  (* t[n] and struct { t ; t[n-1] ; } are congruent *)
  let t1 = unrollType t1 in
  let t2 = unrollType t2 in 
  let array_helper_function t eo al x = begin
    match eo with
      Some(Const(CInt(n,a,b))) when n > 1 -> begin
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
              ftype = TArray(t,(Some(Const(CInt(n-1,a,b)))),[]) ;
              fattr = [] ; } ] ; 
        type_congruent t q1 (TComp(false, our_compinfo, [])) q2
      end
    | _ -> false
  end in 
  if (q1 = Wild && q2 = Wild) then 
    true
  else match (t1,t2) with
    (* unions can be reordered without loss *)
  | TComp(_,c1,_),TComp(_,c2,_) 
      when (not c1.cstruct) && (not c2.cstruct) -> begin
      let fields_match l1 l2 = 
        List.for_all (fun l1_elt ->
          List.exists (fun l2_elt -> type_congruent l1_elt.ftype q1 
                                                    l2_elt.ftype q2)
            l2) l1
      in
        (fields_match c1.cfields c2.cfields) &&
        (fields_match c2.cfields c1.cfields)
    end
    (* structures match if all of their fields match in order *)
  | TComp(_,c1,_),TComp(_,c2,_) when (c1.cstruct) && (c2.cstruct) -> 
    (c1.cname = c2.cname) || 
    (((List.length c1.cfields) = (List.length c2.cfields)) && 
    List.for_all2 (fun f1 f2 -> type_congruent f1.ftype q1 f2.ftype q2) 
      c1.cfields c2.cfields)


    (* a structure with one element is equal to that element *)
  | TComp(_,c1,_),_ when ((List.length c1.cfields) = 1) ->
    let f1 = List.hd c1.cfields in type_congruent f1.ftype q1 t2 q2 
  | _,TComp(_,c2,_) when ((List.length c2.cfields) = 1) ->
    let f2 = List.hd c2.cfields in type_congruent t1 q1 f2.ftype q2

    (* t and t[1] are the same *)
  | (x,TArray(t,eo,al)) when (type_congruent x q1 t q2) -> begin
    match eo with
      Some(Const(CInt(1,_,_))) -> true
    | _ -> false
  end
  | (TArray(t,eo,al),x) when (type_congruent x q2 t q1) -> begin
    match eo with
      Some(Const(CInt(1,_,_))) -> true
    | _ -> false
  end

    (* t[n] and struct { t ; t[n-1] ; } are congruent *)
  | (x,TArray(t,eo,al)) -> array_helper_function t eo al x
  | (TArray(t,eo,al),x) -> array_helper_function t eo al x

  | TVoid(_),TVoid(_) -> true
  | TInt(_),TInt(_) -> true
  | TFloat _, TFloat _ -> true
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
(* t1 = from, t2 = to *)
let rec subtype (t1 : typ) (q1 : opointerkind) 
            (t2 : typ) (q2 : opointerkind) =
  let t1 = unrollType t1 in
  let t2 = unrollType t2 in 
  if t1 == t2 || (type_congruent t1 q1 t2 q2) then
    true
  else match (t1,t2) with 
    (* t1 x t2 x t3 ... <= t1 x t2, general case  *)
    TComp(isf1,c1,a1),TComp(_,c2,_) when c1.cstruct && c2.cstruct -> begin
      (* is t2 congruent to a prefix of t1? *)
      (* we'll do it the expensive way: try all prefices of t1 *)
      let found_one = ref false in 
      for l = 1 to (List.length c1.cfields) do 
        if (not (!found_one)) then begin
          let prefix_struct_c1 = { c1 with cfields = (sublist c1.cfields l) } in
          if (type_congruent t2 q2 (TComp(isf1,prefix_struct_c1,a1)) q1) then
            found_one := true
        end
      done ; !found_one
    end
    (* t1 x t2 <= t1 *)
  | TComp(isf1,c1,a1),_ when c1.cstruct -> begin
    (* this is true if t2 is congruent to some prefix of c1, as above *)
      let found_one = ref false in 
      for l = 1 to (List.length c1.cfields) do 
        if (not (!found_one)) then begin
          let prefix_struct_c1 = { c1 with cfields = (sublist c1.cfields l) } in
          if (type_congruent (TComp(isf1,prefix_struct_c1,a1)) q1) t2 q2 then
            found_one := true
        end
      done ; !found_one
    end
    (* x <= a + b  iff x <= a && x <= b *)
   | _,TComp(_,c2,_) when not c2.cstruct -> begin
      List.for_all (fun elt -> subtype t1 q1 elt.ftype q2) c2.cfields 
   end
    (* a+b <= x    iff a <= x || b <= x *)
   | TComp(_,c1,_),_ when not c1.cstruct -> begin
      List.exists (fun elt -> subtype elt.ftype q1 t2 q2) c1.cfields 
   end

  | _,_ -> false

(* a predicate to determine if a polymorphic function call is involved *)
let rec is_p n other_n = match n.where with
    PGlob(s),_ when String.contains s '*' -> true
  | (PAnon(_),0) |
    (PLocal(_,_,_),1) -> 
      if ((List.length n.succ) = 1) &&
         ((List.length n.pred) = 1) then begin
         if (List.hd n.succ).eto = other_n then
           is_p (List.hd n.pred).efrom n
         else
           is_p (List.hd n.succ).eto n
      end else false
  | _ -> false

(* This "solver" turns almost all nodes WILD *)
let wild_solve (node_ht : (int,node) Hashtbl.t) = begin
  Hashtbl.iter (fun id n -> 
    if n.kind <> ROString || n.why_kind <> PrintfArg then begin
      n.kind <- Wild ; 
      n.why_kind <- Default 
    end
  ) node_ht 
end

(* This solver-wrapper should be run after another actual solver. It makes
 * everything either WILD or SAFE (or ROSTRING). *)
let wild_safe_solve (node_ht : (int,node) Hashtbl.t) = begin
  Hashtbl.iter (fun id n -> 
    if n.kind <> Safe && 
      (n.kind <> ROString || n.why_kind <> PrintfArg) then begin
      n.kind <- Wild 
    end
  ) node_ht 
end

(* turn a pointer kind into its table equivalent *) 
let table_this_node n = n.kind <- addT n.kind 

    
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
      if isT n.kind then begin
        List.iter (fun e ->
          if e.ekind = ECompat && addT e.eto.kind <> e.eto.kind then begin
            table_this_node e.eto;
            finished := false 
          end) n.succ ;
        List.iter (fun e ->
          if e.ekind = ECompat && addT e.efrom.kind <> e.efrom.kind then begin
            table_this_node e.efrom; 
            finished := false 
          end) n.pred ;
      end
    ) node_ht ; 
  done;
end

