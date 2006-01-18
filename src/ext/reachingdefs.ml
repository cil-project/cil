(* Calculate reaching definitions for each instruction.
 * Determine when it is okay to replace some variables with
 * expressions.
 *
 * After calling computeRDs on a fundec,
 * ReachingDef.stmtStartData will contain a mapping from
 * statement ids to data about which definitions reach each
 * statement. ReachingDef.defIdStmtHash will contain a
 * mapping from definition ids to the statement in which
 * that definition takes place.
 *
 * instrRDs takes a list of instructions, and the
 * definitions that reach the first instruction, and
 * for each instruction figures out which definitions
 * reach into or out of each instruction.
 *
 * TODO:
 * 1. Some, Some case in ok_to_replace_with_incdec is
 *    just a special case of the Some, None case.
 * 2. Phase out use of varinfo IH.t. Use IOS.t IH.t instead.
 * 3. Handle pre-increments.
 *)

open Cil
open Pretty

module E = Errormsg
module DF = Dataflow
module UD = Usedef
module IH = Inthash
module U = Util

module IOS = 
  Set.Make(struct
    type t = int option
    let compare io1 io2 =
      match io1, io2 with
	Some i1, Some i2 -> Pervasives.compare i1 i2
      | Some i1, None -> 1
      | None, Some i2 -> -1
      | None, None -> 0
  end)

let debug = ref false

(* convert a set of varinfos into a
   hash from ints to varinfos where
   the ints start at start *)
(*
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
*)

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
(*
let ih_reverse_lookup ih d =
  IH.fold (fun i d' o ->
    match o with
      Some j -> Some j
    | None -> 
	if Util.equals d' d
	then Some i
	else None)
    ih None
*)

(* Lookup varinfo in iosh. If the set contains None
   or is not a singleton, return None, otherwise
   return Some of the singleton *)
(* IOS.t IH.t -> varinfo -> int option *)
let iosh_singleton_lookup iosh vi =
  if IH.mem iosh vi.vid then
    let ios = IH.find iosh vi.vid in
    if not (IOS.cardinal ios = 1) then None
    else IOS.choose ios
  else None

(* The resulting iosh will contain the
   union of the same entries from iosh1 and
   iosh2. If iosh1 has an entry that iosh2
   does not, then the result will contain
   None in addition to the things from the
   entry in iosh1. *)
let iosh_combine iosh1 iosh2 =
  let iosh' = IH.copy iosh1 in
  IH.iter (fun id ios1 ->
    if IH.mem iosh2 id then
      let ios2 = IH.find iosh2 id in
      let newset = IOS.union ios1 ios2 in
      IH.replace iosh' id newset;
    else
      let newset = IOS.add None ios1 in
      IH.replace iosh' id newset) iosh1;
  IH.iter (fun id ios2 ->
    if not(IH.mem iosh1 id) then
      let newset = IOS.add None ios2 in
      IH.add iosh' id newset) iosh2;
  iosh'

(* add a definition to the set for a variable.
   create a set if there isn't one already *)
(* IOS.t IH.t -> int -> varinfo -> unit *)
(*
let iosh_add iosh i vi =
  if IH.mem iosh vi.vid then
    let ios = IH.find iosh vi.vid in
    let newset = IOS.add (Some i) ios in
    IH.replace iosh vi.vid newset
  else
    let newset = IOS.singleton (Some i) in
    IH.add iosh vi.vid newset
*)

(* replace an entire set with a singleton.
   if nothing was there just add the singleton *)
(* IOS.t IH.t -> int -> varinfo -> unit *)
let iosh_replace iosh i vi =
  if IH.mem iosh vi.vid then
    let ios = IH.find iosh vi.vid in
    let newset = IOS.singleton (Some i) in
    IH.replace iosh vi.vid newset
  else
    let newset = IOS.singleton (Some i) in
    IH.add iosh vi.vid newset

(* remove definitions that are killed.
   add definitions that are gend *)
(* Takes the defs, the data, and a function for
   obtaining the next def id *)
(* VS.t -> varinfo IH.t -> IOS.t IH.t -> (unit->int) -> unit *)
let proc_defs vs ivih iosh f = 
  let pd vi =
    (match iosh_singleton_lookup iosh vi with
      Some(i) ->
	if !debug then
	  ignore (E.log "proc_defs: killing %d\n" i);
	IH.remove ivih i;
    | None -> ());
    let newi = f() in
    if !debug then
      ignore (E.log "proc_defs: genning %d\n" newi);
    IH.add ivih newi vi;
    iosh_replace iosh newi vi
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
    (* Also a hash from variable ids to a set of 
       definition ids that reach this statement.
       None means there is a path to this point on which
       there is no definition of the variable *)
    type t = (varinfo IH.t * int * IOS.t IH.t)

    let copy (ivih, i, iosh) = (IH.copy ivih, i, IH.copy iosh)

    (* entries for starting statements must
       be added before calling compute *)
    let stmtStartData = IH.create 32

    (* a mapping from definition ids to
       the statement corresponding to that id *)
    let defIdStmtHash = IH.create 32

    let pretty () (ivih, s, iosh) =
      line ++ text "ivih:\n" ++
	seq line (fun x -> x) (IH.fold (fun id vi l ->
	  (text " def: "
	     ++ (num id)
	     ++ (text " var: ")
	     ++ (text vi.vname)
	     ++ (text "\n"))::l) ivih [])
	++ 
	seq line (fun (vid,ios) ->
	  num vid ++ text ": " ++
	  IOS.fold (fun io d -> match io with
	    None -> d ++ text "None "
	  | Some i -> d ++ num i ++ text " ") ios nil)
	(IH.tolist iosh)

    (* The first id to use when computeFirstPredecessor
       is next called *)
    let nextDefId = ref 0

    (* Count the number of variable definitions in
       a statement *)
    let num_defs stm =
      match stm.skind with
	Instr(il) -> List.fold_left (fun s i ->
	  let _, d = UD.computeUseDefInstr i in
	  s + UD.VS.cardinal d) 0 il
      | _ -> let _, d = UD.computeUseDefStmtKind stm.skind in
	UD.VS.cardinal d

    (* the first predecessor is just the data in along with
       the id of the first definition of the statement,
       which we get from nextDefId *)
    let computeFirstPredecessor stm (ivih, s, iosh) =
      let startDefId = max !nextDefId s in
      let numds = num_defs stm in
      let rec loop n =
	if n < 0
	then ()
	else
	  (if !debug then
	    ignore (E.log "RD: defId %d -> stm %d\n" (startDefId + n) stm.sid);
	   IH.add defIdStmtHash (startDefId + n) stm;
	   loop (n-1))
      in
      loop (numds - 1);
      nextDefId := startDefId + numds;
      (IH.copy ivih, startDefId, IH.copy iosh)

     (* This has to return None when oivih and ivih
       are the same, otherwise the work list algorithm
       will never terminate. *)
    (* In order to propegate only definitions that reach
       out of all predecessors, this must do an intersection *)
    let combinePredecessors (stm:stmt) ~(old:t) ((ivih, s, iosh):t) =
      match old with (oivih, os, oiosh) ->
	if U.equals oivih ivih then None else
	if !mayReach
	then Some(ih_union oivih ivih, os, iosh_combine oiosh iosh)
	else Some(ih_inter oivih ivih, os, iosh_combine oiosh iosh)

    (* return an action that removes things that
       are redefinied and adds the generated defs *)
    let doInstr inst (ivih, s, iosh) = 
      let transform (ivih',s', iosh') =
	let _, defd = UD.computeUseDefInstr inst in
	proc_defs defd ivih' iosh' (idMaker () s');
	(ivih', s' + UD.VS.cardinal defd, iosh')
      in
      DF.Post transform

    let doStmt stm (ivih, s, iosh) = DF.Default

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
   let fst_iosh = IH.create 32 in
   let _ = IH.add ReachingDef.stmtStartData fst_stm.sid (fst_pred, 0, fst_iosh) in
   let _ = ReachingDef.computeFirstPredecessor fst_stm (fst_pred, 0, fst_iosh) in
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
let instrRDs il (ivih, s, iosh) out =

  let print_instr i (ivih',s', iosh') =
    let d = d_instr () i
	++ line
	++ ReachingDef.pretty () (ivih',s', iosh')
	++ line
    in
    fprint stdout 80 d;
    flush stdout
  in

  let proc_one hil i =
    match hil with
    | [] -> 
	let _, defd = UD.computeUseDefInstr i in
	if UD.VS.is_empty defd 
	then (if !debug then print_instr i (ivih, s, iosh);
	      [(IH.copy ivih, s, iosh)])
	else 
	  let ivih' = IH.copy ivih in
	  let iosh' = IH.copy iosh in
	  proc_defs defd ivih' iosh' (idMaker () s);
	  if !debug then
	    print_instr i (ivih', s + UD.VS.cardinal defd, iosh');
	  (ivih', s + UD.VS.cardinal defd, iosh')::hil
    | (ivih', s', iosh')::hrst as l ->
	let _, defd = UD.computeUseDefInstr i in
	if UD.VS.is_empty defd 
	then 
	  (if !debug then
	    print_instr i (ivih',s', iosh');
	   (IH.copy ivih', s', iosh')::l)
	else let ivih'' = IH.copy ivih' in
	let iosh'' = IH.copy iosh' in
	proc_defs defd ivih'' iosh'' (idMaker () s');
	if !debug then
	  print_instr i (ivih'', s' + UD.VS.cardinal defd, iosh'');
	(ivih'',s' + UD.VS.cardinal defd, iosh'')::l
  in
  if out then
    List.tl (List.rev (List.fold_left proc_one [(ivih,s,iosh)] il))
  else
    List.rev (List.tl (List.fold_left proc_one [(ivih,s,iosh)] il))

type rhs = RDExp of exp | RDCall of instr

(* take the id number of a definition and return
   the rhs of the definition if there is one.
   Returns None if, for example, the definition is
   caused by an assembly instruction *)
(* int -> (rhs * varinfo IH.t * IOS.t IH.t) option *)
let getDefRhs defId =
  let stm =
    try IH.find ReachingDef.defIdStmtHash defId 
    with Not_found -> E.s (E.error "getDefRhs: defId %d not found in defIdStmtHash\n" defId) in
  let (ivih,s,iosh) = 
    try IH.find ReachingDef.stmtStartData stm.sid
    with Not_found -> E.s (E.error "getDefRhs: sid %d not found \n" stm.sid) in
  match stm.skind with
    Instr il ->
      let ivihl = instrRDs il (ivih,s,iosh) true in (* defs that reach out of each instr *)
      let ivihl_in = instrRDs il (ivih,s,iosh) false in (* defs that reach into each instr *)
      let iihl = List.combine (List.combine il ivihl) ivihl_in in
      let iihl' = List.filter (fun ((_,(ih,_,_)),_) ->
	if IH.mem ih defId 
	then true 
	else false) iihl in
      (try 
	let ((i,(divih,_,_)),(divih_in,_,iosh_in)) = List.hd iihl' in
	let vi = 
	  try IH.find divih defId
	  with Not_found -> E.s (E.error "getDefRhs: defId %d doesn't reach first instr?!\n" defId) in
	(match i with
	  Set((lh,_),e,_) ->
	    (match lh with
	      Var(vi') ->
		if Util.equals vi vi'
		then Some(RDExp(e), divih_in, iosh_in)
		else E.s (E.error "Reaching Defs: getDefRhs: right vi not first\n")
	    | _ -> E.s (E.error "Reaching Defs getDefRhs: right vi not first\n"))
	| Call(lvo,e,el,_) -> Some(RDCall(i), divih_in, iosh_in)
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
let ok_to_replace (curivih,curiosh) (defivih,defiosh) f r =
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
      let curido = iosh_singleton_lookup curiosh vi in
      let defido = iosh_singleton_lookup defiosh vi in
      match curido, defido with
	Some(curid), Some(defid) -> 
	  (if !debug then ignore (E.log "ok_to_replace: curido: %d defido: %d\n" curid defid);
	  curid = defid && b)
      | None, None -> 
	  if not(UD.VS.mem vi fdefs) then
	    (if !debug then ignore (E.log "ok_to_replace: %s not defined in function\n" vi.vname);
	     b)
	  else (* if the same set of definitions reaches, we can replace, also *)
	    let curios = IH.find curiosh vi.vid in
	    let defios = IH.find defiosh vi.vid in
	    IOS.compare curios defios == 0
(*
	    (if !debug then ignore (E.log "ok_to_replace: %s IS defined in function\n" vi.vname);
	    false)
*)
      | _, _ ->
	  (if !debug then ignore (E.log "ok_to_replace: %s has conflicting definitions\n" vi.vname);
	  false)) 
    uses true

let useList = ref []
(* Visitor for making a list of statements that use a definition *)
class useListerClass (defid:int) (vi:varinfo) : cilVisitor = object(self)
    inherit nopCilVisitor

  (* which statement are we working on  *)
  val mutable sid = -1

  (* if a list of instructions is being
     processed, then this is the corresponding
     list of reaching defs *)
  val mutable ivihl = []

  (* these are the reaching defs for the
     current instruction if there is one *)
  val mutable inst_iviho = None

  method vstmt stm =
    sid <- stm.sid;
    match getRDs sid with
      None -> DoChildren
    | Some(ivih, s, iosh) ->
	match stm.skind with
	  Instr il ->
	    ivihl <- instrRDs il (ivih, s, iosh) false;
	    DoChildren
	| _ -> DoChildren

  method vinst i =
    inst_iviho <- Some(List.hd ivihl);
    ivihl <- List.tl ivihl;
    DoChildren

  method vexpr e =
    match e with
      Lval(Var vi', off) ->
	(match inst_iviho with
	  Some(inst_ivih,_,_) ->
	    if Util.equals vi vi' &&
	      IH.mem inst_ivih defid
	    then (useList := sid::(!useList);
		  DoChildren)
	    else DoChildren
	| None -> let iviho = getRDs sid in
	  (match iviho with
	    Some (ivih,_,_) ->
	      if Util.equals vi vi' &&
		IH.mem ivih defid
	      then (useList := sid::(!useList);
		    DoChildren)
	      else DoChildren
	  | None -> E.s (E.error "useLister: no ivih for statement\n")))
    | _ -> DoChildren

end

(* ok_to_replace_with_incdec *)
(* Find out if it is alright to replace the use of a variable
   with a post-incrememnt/decrement of the variable it is assigned to be *)
(* Takes the definitions reaching the variable use, the definitions
   reaching the place where the variable was defined, the fundec,
   the varinfo for the variable being considered and the right
   hand side of its definition. *)
let ok_to_replace_with_incdec (curivih,curiosh) (defivih,defiosh) f id vi r =

  (* number of uses of vi where definition id reaches *)
  let num_uses () = 
    let _ = useList := [] in
    let _ = visitCilFunction (new useListerClass id vi) f in
    List.length (!useList)
  in

  (* Is e the addition or subtraction of one to vi?
     Return Some(PlusA) if it's an addition,
     Some(MinusA) if it's a subtraction,
     and None otherwise *)
  let inc_or_dec e vi =
    match e with
      BinOp((PlusA|PlusPI|IndexPI), Lval(Var vi', NoOffset), 
	    Const(CInt64(one,_,_)),_) ->
	      if vi.vid = vi'.vid && one = Int64.one 
	      then Some(PlusA)
	      else None
    | BinOp((MinusA|MinusPI), Lval(Var vi', NoOffset),
	    Const(CInt64(one,_,_)),_) ->
	      if vi.vid = vi'.vid && one = Int64.one
	      then Some(MinusA)
	      else None
    | BinOp((PlusA|PlusPI|IndexPI), Lval(Var vi', NoOffset),
	    Const(CInt64(mone,_,_)),_) ->
	      if vi.vid = vi'.vid && mone = Int64.minus_one 
	      then Some(MinusA)
	      else None
    | _ -> None
  in

  match r with
    RDExp(Lval(Var rhsvi, NoOffset)) ->
      let curido = iosh_singleton_lookup curiosh rhsvi in
      let defido = iosh_singleton_lookup defiosh rhsvi in
      (match  curido, defido with
	Some(curid), Some(defid) -> (* XXX: This is just a special case of the Some, None case *)
	  if curid = defid then None (* Should have been replaced already *)
	  else let redefrhso = getDefRhs curid in
	  (match redefrhso with
	    None -> (if !debug then ignore (E.log "ok_to_replace: couldn't get rhs for redef: %d\n" curid);
		     None)
	  | Some(redefrhs, redefivih, redefiosh) ->
	      let redefido = iosh_singleton_lookup redefiosh rhsvi in
	      (* The same definition of rhsvi must reach both the definition of
		 vi and the redefinition of rhsvi *)
	      (match redefido with
		None -> (if !debug then ignore (E.log "ok_to_replace: a def of %s doesn't reach redefinition\n" rhsvi.vname);
			 None)
	      | Some redefid ->
		  let curdef_stmt = try IH.find ReachingDef.defIdStmtHash curid
		  with Not_found -> E.s (E.error "ok_to_replace: couldn't find statement defining %d\n" curid) in
		  if not(defid = redefid) 
		  then (if !debug then ignore (E.log "ok_to_replace: different definitions of %s reach the definition of %s and %s's redefinition: %d and %d\n"
					      rhsvi.vname vi.vname rhsvi.vname defid redefid);
			None)
		  else
		    (match redefrhs with
		      RDExp(e) -> (match inc_or_dec e rhsvi with
			Some(PlusA) ->
			  if num_uses () = 1 then 
			    Some(curdef_stmt.sid, curid, rhsvi, PlusA)
			  else (if !debug then ignore (E.log "ok_to_replace: tmp used more than once\n");
				None)
		      | Some(MinusA) ->
			  if num_uses () = 1 then 
			    Some(curdef_stmt.sid, curid, rhsvi, MinusA)
			  else (if !debug then ignore (E.log "ok_to_replace: tmp used more than once\n");
				None)
		      | None ->
			  (if !debug then ignore (E.log "ok_to_replace: redef isn't adding or subtracting one from itself\n");
			   None))
		    | _ -> (if !debug then ignore (E.log "ok_to_replace: redef a call\n");
			    None))))
      | Some(curid), None ->
	  let defios = try IH.find defiosh rhsvi.vid 
	  with Not_found -> IOS.empty in
	  let redefrhso = getDefRhs curid in
	  (match redefrhso with
	    None -> (if !debug then ignore (E.log "ok_to_replace: couldn't get rhs for redef: %d\n" curid);
		     None)
	  | Some(redefrhs, redefivih, redefiosh) ->
	      let tmprdido = iosh_singleton_lookup redefiosh vi in
	      match tmprdido with
		None -> (if !debug then ignore (E.log "ok_to_replace: conflicting defs of %s reach redef of %s\n" vi.vname rhsvi.vname);
			 None)
	      | Some tmprdid ->
		  if not (tmprdid = id) then
		    (if !debug then ignore (E.log "ok_to_replace: initial def of %s doesn't reach redef of %s\n" vi.vname rhsvi.vname);
		     None)
		  else let redefios = try IH.find redefiosh rhsvi.vid 
		  with Not_found -> IOS.empty in
		  let curdef_stmt = try IH.find ReachingDef.defIdStmtHash curid
		  with Not_found -> E.s (E.error "ok_to_replace: couldn't find statement defining %d\n" curid) in
		  if not (IOS.compare defios redefios = 0) then
		    (if !debug then ignore (E.log "ok_to_replace: different sets of definitions of %s reach the def of %s and the redef of %s\n"
					      rhsvi.vname vi.vname rhsvi.vname);
		     None)
		  else
		    (match redefrhs with
		      RDExp(e) -> (match inc_or_dec e rhsvi with
			Some(PlusA) ->
			  if num_uses () = 1 then 
			    Some(curdef_stmt.sid, curid, rhsvi, PlusA)
			  else (if !debug then ignore (E.log "ok_to_replace: tmp used more than once\n");
				None)
		      | Some(MinusA) ->
			  if num_uses () = 1 then 
			    Some(curdef_stmt.sid, curid, rhsvi, MinusA)
			  else (if !debug then ignore (E.log "ok_to_replace: tmp used more than once\n");
				None)
		      | None ->
			  (if !debug then ignore (E.log "ok_to_replace: redef isn't adding or subtracting one from itself\n");
			   None))
		    | _ -> (if !debug then ignore (E.log "ok_to_replace: redef a call\n");
			    None)))
      | _ -> (if !debug then ignore (E.log "ok_to_replace: %s has conflicting definitions\n" rhsvi.vname);
	      None))
  | _ -> (if !debug then ignore (E.log "ok_to_replace: rhs not of correct form\n");
	  None)
