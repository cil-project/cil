(*
 * A set of solver utilities and wrappers. 
 * 
 * This includes implementations of "type_congruent" and "subtype", as well
 * as solver-wrappers.
 *)
open Cil
open Ptrnode
open Pretty
open Trace
module E = Errormsg

let safe_voidstar = true

(* are the given two types congurent? see infer.tex 
 * also remember that two wild pointers are always considered congruent *)
let rec type_congruent (t1 : typ) (q1 : opointerkind) 
                       (t2 : typ) (q2 : opointerkind) = begin
                         
(*  ignore (E.log "Checking t1: %a, t2: %a\n" d_plaintype t1 d_plaintype t2);*)

  (* t[n] and struct { t ; t[n-1] ; } are congruent *)
  let t1 = unrollType t1 in
  let t2 = unrollType t2 in 

  let array_helper_function t eo al x = begin
    match eo with
      Some(Const(CInt64(n,a,b))) when n > Int64.one -> begin
        let our_compinfo = {
          cstruct = true ;
          cname = "" ;
          ckey = -1;
          cfields = [] ;
          cattr = [] ;
          creferenced = false ;
        } in
        our_compinfo.cfields <- 
          [ { fcomp = our_compinfo ; fname = "" ;
              ftype = t ;
              fbitfield = None;
              fattr = [] ; } ;
            { fcomp = our_compinfo ; fname = "" ;
              ftype = TArray(t,(Some(Const(CInt64(Int64.pred n,a,b)))),[]) ;
              fbitfield = None;
              fattr = [] ; } ] ; 
        type_congruent t q1 (TComp(our_compinfo, [])) q2
      end
    | _ -> false
  end in 

  if (q1 = Wild && q2 = Wild) then 
    true
  else match (t1,t2) with

    (* bugfix for arrays *)
  | (TArray(typ1, expr1, attrlist1), TArray(typ2, expr2, attrlist2)) when
      (type_congruent typ1 q1 typ2 q2) ->
        begin
          match (expr1, expr2) with
            Some(Const(CInt64(val1, _, _))), Some(Const(CInt64(val2, _, _))) ->
              val1 = val2
          | _ -> false
        end
        
    (* unions can be reordered without loss *)

  | TComp(c1,_),TComp(c2,_) 
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
  | TComp(c1,_),TComp(c2,_) when (c1.cstruct) && (c2.cstruct) -> 
    (c1.cname = c2.cname) || 
    (((List.length c1.cfields) = (List.length c2.cfields)) && 
    List.for_all2 (fun f1 f2 -> type_congruent f1.ftype q1 f2.ftype q2) 
      c1.cfields c2.cfields)


    (* a structure with one element is equal to that element *)
  | TComp(c1,_),_ when ((List.length c1.cfields) = 1) ->
    let f1 = List.hd c1.cfields in type_congruent f1.ftype q1 t2 q2 
  | _,TComp(c2,_) when ((List.length c2.cfields) = 1) ->
    let f2 = List.hd c2.cfields in type_congruent t1 q1 f2.ftype q2

    (* t and t[1] are the same *)
  | (x,TArray(t,eo,al)) when (type_congruent x q1 t q2) ->
      begin
        match eo with
          Some(Const(CInt64(one,_,_))) when one = Int64.one -> true
        | _ -> false
      end
  | (TArray(t,eo,al),x) when (type_congruent x q2 t q1) -> begin
    match eo with
      Some(Const(CInt64(one,_,_))) when one = Int64.one -> true
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
  | TFun(rt1, args1,va1,_),TFun(rt2,args2,va2,_) -> 
      not va1 && not va2 && List.length args1 = List.length args2 &&
      type_congruent rt1 q1 rt2 q2 &&
      (List.for_all2 (fun a1 a2 -> type_congruent a1.vtype q1 a2.vtype q2)
         args1 args2)

  | TPtr(_),TPtr(_) -> true
(*
  | TVoid _, _ -> true
  | _, TVoid _ -> true
*)
  | _ -> false
end

(* returns the first n elements of l *)
let rec sublist l n = begin
  if n <= 0 then [] 
  else match l with
      [] -> []
  | hd :: tl -> hd :: (sublist tl (n-1))
end

let rec product_subtype (product_typ1: typ list) (q1: opointerkind)
                        (product_typ2: typ list) (q2: opointerkind) =
  match (product_typ1, product_typ2) with
    _, [] -> true           (* everything is a subtype of the empty product *)
  | [], _ -> false
  | hd1::tl1, hd2::tl2 ->
      (type_congruent hd1 q1 hd2 q2) && (product_subtype tl1 q1 tl2 q2)

(* do we have t1,q1 <= t2,q2 (as in infer.tex)? *)
(* t1 = from, t2 = to *)
let rec subtype (t1 : typ) (q1 : opointerkind) 
                (t2 : typ) (q2 : opointerkind) =
  let t1 = unrollType t1 in
  let t2 = unrollType t2 in 
  if t1 == t2 || (type_congruent t1 q1 t2 q2) then
    true
  else
    let extract_typ = function x -> x.ftype in
    match (t1,t2) with 
      TComp(c1,a1),TComp(c2,_) when c1.cstruct && c2.cstruct -> 
        (* this is a weird addition because "subtyping" in C is based
           on memory alignment.  rule should look like
           (TComp, AnyType) -> subtype Struct.hd AnyType || ... 
           For now this is just a hack until I modify the solver to
           add compatibility edges correctly. *)
        ((c1.cfields <> []) &&
         type_congruent (extract_typ (List.hd c1.cfields)) q1 t2 q2) ||
        product_subtype (List.map extract_typ c1.cfields) q1
                        (List.map extract_typ c2.cfields) q2
    (* t1 x t2 <= t1 *)
    | TComp(c1,a1),_ when c1.cstruct ->
    (* this is true if t2 is congruent to some prefix of c1, as above *)
        product_subtype (List.map extract_typ c1.cfields) q1 [t2] q2 
    (* x <= a + b  iff x <= a && x <= b *)
    | _,TComp(c2,_) when not c2.cstruct -> begin
        List.for_all (fun elt -> subtype t1 q1 elt.ftype q2) c2.cfields 
    end
    (* a+b <= x    iff a <= x || b <= x *)
    | TComp(c1,_),_ when not c1.cstruct -> begin
        List.exists (fun elt -> subtype elt.ftype q1 t2 q2) c1.cfields 
    end
    | TVoid(_), _ -> safe_voidstar
    | _, TVoid(_) -> safe_voidstar
    | TArray(et, eo, _), t ->
        type_congruent et q1 t q2
    | _,_ -> false
          
(* a predicate to determine if a polymorphic function call is involved *)
let rec is_p n other_n = match n.where with
    (PGlob(s),_) | (PStatic(_, s), _) when String.contains s '*' -> true
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
    if (n.kind <> ROString || n.why_kind <> PrintfArg) then begin
      (* Do not make WILD those functions that are not cast and are not used 
       * without prototype *)
      if (match n.btype with TFun _ -> true | _ -> false) &&
         (* Only functions, not functions embedded in pointers *)
         (match n.where with PGlob _, 0 -> true 
                          | PStatic _, 0 -> true 
                          | _ -> false) &&
         (not (hasFlag n pkNoPrototype) && n.succ=[] && n.pred=[]) then begin
           n.kind <- Safe;
           n.why_kind <- Default
         end else begin
           n.kind <- Wild ; 
           n.why_kind <- Default 
         end
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
  (trace "sm" (dprintf "beginning of table_interface\n"));
  let finished = ref false in
  while (not !finished) do
    finished := true ;
    Hashtbl.iter (fun id n ->
      if hasFlag n pkInterface then begin
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
  (trace "sm" (dprintf "end of table_interface\n"));
end

