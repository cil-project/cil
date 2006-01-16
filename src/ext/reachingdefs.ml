(* Calculate reaching definitions for each instruction.
 * A definition must reach all predecessors of a block
 * in order to reach that block because this module will
 * be used to eliminate CIL generated temporary variables.
 *
 * Set mayReach to true if may reach should be calculated
 * instead of must reach.
 *)

open Cil
open Pretty

module E = Errormsg
module DF = Dataflow
module UD = Usedef
module IH = Inthash
module U = Util

let debug = ref false

(* convert a set of varinfos into a
   hash from ints to varinfos where
   the ints start at start *)
let vs_to_ivih vs ivih start =
  let vil = UD.VS.elements vs in
  let rec adder vl i =
    match vl with
      [] -> ()
    | v::vlp -> 
	(IH.add ivih i v;
	 adder vlp (i+1))
  in
  adder vil start


    (* return the intersection of
       Inthashes ih1 and ih2 *)
let ih_inter ih1 ih2 =
  let ih' = IH.copy ih1 in
  IH.iter (fun id vi ->
    if not(IH.mem ih2 id) then
      IH.remove ih' id else
      ()) ih1;
  ih'

let ih_union ih1 ih2 =
  let ih' = IH.copy ih1 in
  IH.iter (fun id vi ->
    if not(IH.mem ih' id) 
    then IH.add ih' id vi
    else ()) ih2;
  ih'

(* 'a IH.t -> 'a -> int option *)
let ih_reverse_lookup ih d =
  IH.fold (fun i d' o ->
    match o with
      Some j -> Some j
    | None -> 
	if Util.equals d' d
	then Some i
	else None)
    ih None

(* remove definitions that are killed.
   add definitions that are gend *)
(* Takes the defs, the data, and a function for
   obtaining the next def id *)
(* VS.t -> varinfo IH.t -> (unit->int) -> unit *)
let proc_defs vs ivih f = 
  let pd vi =
    (match ih_reverse_lookup ivih vi with
      Some(i) ->
	if !debug then
	  ignore (E.log "proc_defs: killing %d\n" i);
	IH.remove ivih i
    | None -> ());
    let newi = f() in
    if !debug then
      ignore (E.log "proc_defs: genning %d\n" newi);
    IH.add ivih newi vi
  in
  UD.VS.iter pd vs

let idMaker () start =
  let counter = ref start in
  fun () ->
    let ret = !counter in
    counter := !counter + 1;
    ret

module ReachingDef =
  struct

    let name = "Reaching Definitions"

    let debug = debug

    (* Should the analysis calculate may-reach
       or must-reach *)
    let mayReach = ref false

    (* hash from ints to variables and
       an integer that tells the id number of
       the first definition *)
    type t = (varinfo IH.t * int)

    let copy (ivih,i) = (IH.copy ivih,i)

    (* entries for starting statements must
       be added before calling compute *)
    let stmtStartData = IH.create 32

    (* a mapping from definition ids to
       the statement corresponding to that id *)
    let defIdStmtHash = IH.create 32

    let pretty () (ivih,s) =
      seq line (fun x -> x) (IH.fold (fun id vi l ->
	(text " def: "
	   ++ (num id)
	   ++ (text " var: ")
	   ++ (text vi.vname)
	   ++ (text "\n"))::l) ivih [])

    (* The first id to use when computeFirstPredecessor
       is next called *)
    let nextDefId = ref 0

    (* the first predecessor is just the data in along with
       the id of the first definition of the statement,
       which we get from nextDefId *)
    let computeFirstPredecessor stm (ivih,s) =
      let startDefId = max !nextDefId s in
      let _, defd = UD.computeUseDefStmtKind stm.skind in
      let rec loop n =
	if n < 0
	then ()
	else
	  (if !debug then
	    ignore (E.log "RD: defId %d -> stm %d\n" (startDefId + n) stm.sid);
	   IH.add defIdStmtHash (startDefId + n) stm;
	   loop (n-1))
      in
      loop (UD.VS.cardinal defd - 1);
      nextDefId := startDefId + UD.VS.cardinal defd;
      (IH.copy ivih, startDefId)

     (* This has to return None when oivih and ivih
       are the same, otherwise the work list algorithm
       will never terminate. *)
    (* In order to propegate only definitions that reach
       out of all predecessors, this must do an intersection *)
    let combinePredecessors (stm:stmt) ~(old:t) ((ivih, s):t) =
      match old with (oivih,os) ->
	if U.equals oivih ivih then None else
	if !mayReach
	then Some(ih_union oivih ivih, os)
	else Some(ih_inter oivih ivih, os)

    (* return an action that removes things that
       are redefinied and adds the generated defs *)
    let doInstr inst (ivih,s) = 
      let transform (ivih',s') =
	let _, defd = UD.computeUseDefInstr inst in
	proc_defs defd ivih' (idMaker () s');
	(ivih', s' + UD.VS.cardinal defd)
      in
      DF.Post transform

    let doStmt stm (ivih,s) = DF.Default

    let filterStmt stm = true

end

module RD = DF.ForwardsDataFlow(ReachingDef)

(* Computes the reaching definitions for a
   function. *)
(* Cil.fundec -> (varinfo IH.t * int) IH.t *)
let computeRDs fdec =
   let bdy = fdec.sbody in
   let slst = bdy.bstmts in
   let _ = IH.clear ReachingDef.stmtStartData in
   let _ = IH.clear ReachingDef.defIdStmtHash in
   let _ = ReachingDef.nextDefId := 0 in
   let fst_stm = List.hd slst in
   let fst_pred = IH.create 32 in
   let _ = IH.add ReachingDef.stmtStartData fst_stm.sid (fst_pred, 0) in
   let _ = ReachingDef.computeFirstPredecessor fst_stm (fst_pred, 0) in
   if !debug then
     ignore (E.log "computeRDs: fst_stm.sid=%d\n" fst_stm.sid);
   RD.compute [fst_stm]
   (* now ReachingDef.stmtStartData has the reaching def data in it *)

(* return the definitions that reach the statement
   with statement id sid *)
let getRDs sid = 
  try
    Some (IH.find ReachingDef.stmtStartData sid)
  with Not_found ->
    None
(*    E.s (E.error "getRDs: sid %d not found\n" sid) *)

(* Pretty print the reaching definition data for
   a function *)
let ppFdec fdec =
  seq line (fun stm ->
    let ivih = IH.find ReachingDef.stmtStartData stm.sid in
    ReachingDef.pretty () ivih) fdec.sbody.bstmts

(* given reaching definitions into a list of
   instructions, figure out the definitions that
   reach in/out of each instruction *)
(* if out is true then calculate the definitions that
   go out of each instruction, if it is false then
   calculate the definitions reaching into each instruction *)
(* instr list -> (varinfo IH.t * int) -> bool -> (varinfo IH.t * int) list *)
let instrRDs il (ivih, s) out =

  let print_instr i (ivih',s') =
    let d = d_instr () i
	++ line
	++ ReachingDef.pretty () (ivih',s')
    in
    fprint stdout 80 d;
    flush stdout
  in

  let proc_one hil i =
    match hil with
    | [] -> 
	let _, defd = UD.computeUseDefInstr i in
	if UD.VS.is_empty defd 
	then (if !debug then print_instr i (ivih,s);
	      [(IH.copy ivih, s)])
	else 
	  let ivih' = IH.copy ivih in
	  proc_defs defd ivih' (idMaker () s);
	  if !debug then
	    print_instr i (ivih', s + UD.VS.cardinal defd);
	  (ivih', s + UD.VS.cardinal defd)::hil
    | (ivih', s')::hrst as l ->
	let _, defd = UD.computeUseDefInstr i in
	if UD.VS.is_empty defd 
	then 
	  (if !debug then
	    print_instr i (ivih',s');
	   (IH.copy ivih', s')::l)
	else let ivih'' = IH.copy ivih' in
	proc_defs defd ivih'' (idMaker () s');
	if !debug then
	  print_instr i (ivih'', s' + UD.VS.cardinal defd);
	(ivih'',s' + UD.VS.cardinal defd)::l
  in
  if out then
    List.tl (List.rev (List.fold_left proc_one [(ivih,s)] il))
  else
    List.rev (List.tl (List.fold_left proc_one [(ivih,s)] il))

type rhs = RDExp of exp | RDCall of instr

(* take the id number of a definition and return
   the rhs of the definition if there is one.
   Returns None if, for example, the definition is
   caused by an assembly instruction *)
(* int -> (rhs * varinfo IH.t) option *)
let getDefRhs defId =
  let stm = 
    try IH.find ReachingDef.defIdStmtHash defId 
    with Not_found -> E.s (E.error "getDefRhs: defId %d not found in defIdStmtHash\n" defId) in
  let (ivih,s) = 
    try IH.find ReachingDef.stmtStartData stm.sid
    with Not_found -> E.s (E.error "getDefRhs: sid %d not found \n" stm.sid) in
  match stm.skind with
    Instr il ->
      let ivihl = instrRDs il (ivih,s) true in
      let iihl = List.combine il ivihl in
      let iihl' = List.filter (fun (_,(ih,_)) ->
	if IH.mem ih defId 
	then true 
	else false) iihl in
      (try 
	let (i,(divih,_)) = List.hd iihl' in
	let vi = 
	  try IH.find divih defId
	  with Not_found -> E.s (E.error "getDefRhs: defId %d doesn't reach first instr?!\n" defId) in
	(match i with
	  Set((lh,_),e,_) ->
	    (match lh with
	      Var(vi') ->
		if Util.equals vi vi'
		then Some(RDExp(e), divih)
		else E.s (E.error "Reaching Defs: getDefRhs: right vi not first\n")
	    | _ -> E.s (E.error "Reaching Defs getDefRhs: right vi not first\n"))
	| Call(lvo,e,el,_) -> Some(RDCall(i), divih)
	| Asm(a,sl,slvl,sel,sl',_) -> None) (* ? *)
      with Failure _ ->
	E.s (E.error "Reaching Defs: getDefRhs: No instruction defines %d\n" defId))
  | _ -> None

(* ok_to_replace *)
(* is it alright to replace a variable use with the expression
   that the variable was defined to be? *)
(* Takes the definitions that reached the place where the
   variable was defined and the definitions that reach the
   place the variable is used. If the same definitions for
   the variables used in the expression reach both places,
   then it is okay to replace the variable with the expression. *)
(* With regards to globals and parameters there are two
   possibilities if the reverse lookup returns None for both
   sets of reaching definitions:
   1) The global or parameter is actually not redefined.
   2) At both points no one definition *must* reach there.
   For this reason, this function also takes the fundec,
   so that it can be figured out which is the case *)
(* varinfo IH.t -> varinfo IH.t -> fundec -> rhs -> bool *)
let ok_to_replace curivih defivih f r =
  let uses = match r with
    RDExp e -> UD.computeUseExp e
  | RDCall i -> 
      let u,d = UD.computeUseDefInstr i in
      u 
  in
  let fdefs = List.fold_left (fun d s ->
    let _, d' = UD.computeDeepUseDefStmtKind s.skind in
    UD.VS.union d d') UD.VS.empty f.sbody.bstmts in
  let _ = if !debug then ignore (E.log "ok_to_replace: card fdefs = %d\n" (UD.VS.cardinal fdefs)) in
  let _ = if !debug then ignore (E.log "ok_to_replace: card uses = %d\n" (UD.VS.cardinal uses)) in
  UD.VS.fold (fun vi b ->
      let curido = ih_reverse_lookup curivih vi in
      let defido = ih_reverse_lookup defivih vi in
      match curido, defido with
	Some(curid), Some(defid) -> 
	  (if !debug then ignore (E.log "ok_to_replace: curido: %d defido: %d\n" curid defid);
	  curid = defid && b)
      | None, None -> 
	  if not(UD.VS.mem vi fdefs) then
	    (if !debug then ignore (E.log "ok_to_replace: %s not defined in function\n" vi.vname);
	     b)
	  else
	    (if !debug then ignore (E.log "ok_to_replace: %s IS defined in function\n" vi.vname);
	    false)
      | _, _ -> 
	  (if !debug then ignore (E.log "ok_to_replace: %s has conflicting definitions\n" vi.vname);
	  false)) 
    uses true
