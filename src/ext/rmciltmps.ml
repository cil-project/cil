(* These are functions etc. for removing CIL generated
   temporary variables. Some can be removed immediately,
   others must wait until pretty printing *)

(* 
 *  TODO:
 *  1. provide a list of prefixes and suffixes that the
 *  names of variables to be eliminated may have.
 *)

open Cil
open Pretty

module E = Errormsg
module RD = Reachingdefs
module UD = Usedef
module IH = Inthash

let debug = ref false

(* The right hand side of an assignment is either
   a function call or an expression *)
type rhs = RDExp of exp | RDCall of instr

(* Type for the form of temporary variable names *)
type nameform = Suffix of string | Prefix of string | Exact of string

(* take the id number of a definition and return
   the rhs of the definition if there is one.
   Returns None if, for example, the definition is
   caused by an assembly instruction *)
(* int -> (rhs * unit * IOS.t IH.t) option *)
let getDefRhs defId =
  let stm =
    try IH.find RD.ReachingDef.defIdStmtHash defId 
    with Not_found -> E.s (E.error "getDefRhs: defId %d not found in defIdStmtHash\n" defId) in
  let (_,s,iosh) = 
    try IH.find RD.ReachingDef.stmtStartData stm.sid
    with Not_found -> E.s (E.error "getDefRhs: sid %d not found \n" stm.sid) in
  match stm.skind with
    Instr il ->
      let ivihl = RD.instrRDs il ((),s,iosh) true in (* defs that reach out of each instr *)
      let ivihl_in = RD.instrRDs il ((),s,iosh) false in (* defs that reach into each instr *)
      let iihl = List.combine (List.combine il ivihl) ivihl_in in
      let iihl' = List.filter (fun ((i,(_,_,iosh')),_) -> (* find the defining instr *)
	match RD.iosh_defId_find iosh' defId with
	  Some vid -> 
	    (match i with
	      Set((Var vi',_),_,_) -> vi'.vid = vid
	    | Call(Some(Var vi',_),_,_,_) -> vi'.vid = vid
	    | Call(None,_,_,_) -> false
	    | _ -> false)
	| None -> false) iihl in
      (try 
	let ((i,(_,_,diosh)),(_,_,iosh_in)) = List.hd iihl' in
	let vid = 
	  match RD.iosh_defId_find diosh defId with
	    None -> E.s (E.error "getDefRhs: defId %d doesn't reach first instr?!\n" defId)
	  | Some(vid') -> vid' in
	(match i with
	  Set((lh,_),e,_) ->
	    (match lh with
	      Var(vi') -> Some(RDExp(e), (), iosh_in)
	    | _ -> E.s (E.error "Reaching Defs getDefRhs: right vi not first\n"))
	| Call(lvo,e,el,_) -> Some(RDCall(i), (), iosh_in)
	| Asm(a,sl,slvl,sel,sl',_) -> None) (* ? *)
      with Failure _ ->
	E.s (E.error "getDefRhs: No instruction defines %d\n" defId))
  | _ -> E.s (E.error "getDefRhs: defining statement not an instruction list %d\n" defId)
      (*None*)

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
let ok_to_replace curiosh defiosh f r =
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
      let curido = RD.iosh_singleton_lookup curiosh vi in
      let defido = RD.iosh_singleton_lookup defiosh vi in
      match curido, defido with
	Some(curid), Some(defid) -> 
	  (if !debug then ignore (E.log "ok_to_replace: curido: %d defido: %d\n" curid defid);
	  curid = defid && b)
      | None, None -> 
	  if not(UD.VS.mem vi fdefs) then
	    (if !debug then ignore (E.log "ok_to_replace: %s not defined in function\n" vi.vname);
	     b)
	  else (* if the same set of definitions reaches, we can replace, also *)
	    let curios = try IH.find curiosh vi.vid
	    with Not_found -> RD.IOS.empty in
	    let defios = try IH.find defiosh vi.vid 
	    with Not_found -> RD.IOS.empty in
	    RD.IOS.compare curios defios == 0
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
class useListerClass (defid:int) (vi:varinfo) = object(self)
    inherit RD.rdVisitorClass

  method vexpr e =
    match e with
      Lval(Var vi', off) ->
	(match cur_rd_dat with
	  Some(_,_,inst_iosh) ->
	    let vido = RD.iosh_defId_find inst_iosh defid in
	    let exists = match vido with Some _ -> true | None -> false in
	    if Util.equals vi vi' && exists
	    then (useList := sid::(!useList);
		  DoChildren)
	    else DoChildren
	| None -> let iviho = RD.getRDs sid in
	  (match iviho with
	    Some (_,_,iosh) ->
	      let vido = RD.iosh_defId_find iosh defid in
	      let exists = match vido with Some _ -> true | None -> false in
	      if Util.equals vi vi' && exists
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
let ok_to_replace_with_incdec curiosh defiosh f id vi r =

  (* number of uses of vi where definition id reaches *)
  let num_uses () = 
    let _ = useList := [] in
    let ulc = new useListerClass id vi in
    let _ = visitCilFunction (ulc :> cilVisitor) f in
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
      let curido = RD.iosh_singleton_lookup curiosh rhsvi in
      let defido = RD.iosh_singleton_lookup defiosh rhsvi in
      (match  curido, defido with
	Some(curid), _ ->
	  let defios = try IH.find defiosh rhsvi.vid 
	  with Not_found -> RD.IOS.empty in
	  let redefrhso = getDefRhs curid in
	  (match redefrhso with
	    None -> (if !debug then ignore (E.log "ok_to_replace: couldn't get rhs for redef: %d\n" curid);
		     None)
	  | Some(redefrhs, _, redefiosh) ->
	      let tmprdido = RD.iosh_singleton_lookup redefiosh vi in
	      match tmprdido with
		None -> (if !debug then ignore (E.log "ok_to_replace: conflicting defs of %s reach redef of %s\n" vi.vname rhsvi.vname);
			 None)
	      | Some tmprdid ->
		  if not (tmprdid = id) then
		    (if !debug then ignore (E.log "ok_to_replace: initial def of %s doesn't reach redef of %s\n" vi.vname rhsvi.vname);
		     None)
		  else let redefios = try IH.find redefiosh rhsvi.vid 
		  with Not_found -> RD.IOS.empty in
		  let curdef_stmt = try IH.find RD.ReachingDef.defIdStmtHash curid
		  with Not_found -> E.s (E.error "ok_to_replace: couldn't find statement defining %d\n" curid) in
		  if not (RD.IOS.compare defios redefios = 0) then
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

(* A hash from variable ids to Call instruction
   options. If a variable id is in this table,
   and it is mapped to Some(Call()), then the
   function call can be printed instead of the
   variable *)
let iioh = IH.create 16

(* A hash from variable ids to information that
   can be used to print a post increment/decrement
   that can replace the variable *)
let incdecHash = IH.create 16

(* A hash from variable ids to a list of statement ids.
   Because a post-inc/dec will be printed elsewhere,
   the assignments of the variable in these statements
   don't need to be printed *)
let idDefHash = IH.create 16

(* Add a pair to the list for vid and create a list if one
   doesn't exist *)
let id_dh_add vid p =
  if IH.mem idDefHash vid then
    let oldlist = IH.find idDefHash vid in
    let newlist = p::oldlist in
    IH.replace idDefHash vid newlist
  else
    IH.add idDefHash vid [p]

(* check if a name matches a form *)
(* string -> nameform -> bool *)
let check_form s f =
    match f with
      Suffix sfx ->
	let frmlen = String.length sfx in
	let slen = String.length s in
	slen >= frmlen &&
	compare (String.sub s (slen - frmlen) frmlen) sfx = 0
    | Prefix pfx ->
	let frmlen = String.length pfx in
	String.length s >= frmlen &&
	compare (String.sub s 0 frmlen) pfx = 0
    | Exact ext ->
	let frmlen = String.length ext in
	String.length s = frmlen &&
	compare s ext = 0

(* check a name against a list of forms 
   if it matches any then return true *)
(* string -> nameform list -> bool *)
let check_forms s fl =
  List.fold_left (fun b f -> b || check_form s f) 
    false fl

let forms = [Exact "tmp";
	     Prefix "tmp___";
	     Prefix "__cil_tmp";
	     Suffix "__e";
	     Suffix "__b";]

(* if the temp with varinfo vi can be
   replaced by an expression then returns
   Some of that expression. o/w None.
   If b is true, then don't check the form *)
(* IOS.t IH.t -> varinfo -> fundec -> bool -> exp option *)
let tmp_to_exp iosh vi fd nofrm =
  if nofrm || check_forms vi.vname forms then
  let ido = RD.iosh_singleton_lookup iosh vi in
  match ido with None -> 
    if !debug then ignore(E.log "tmp_to_exp: non-sigle def: %s\n" vi.vname);
    None
  | Some(id) -> let defrhs = getDefRhs id in
    match defrhs with None -> 
      if !debug then ignore(E.log "tmp_to_exp: no def of %s\n" vi.vname);
      None
    | Some(RDExp(e) as r, _, defiosh) ->
	if ok_to_replace iosh defiosh fd r
	then 
	  (if !debug then ignore(E.log "tmp_to_exp: changing %s to %a\n" vi.vname d_plainexp e);
	  Some e)
	else 
	  (if !debug then ignore(E.log "tmp_to_exp: not ok to replace %s\n" vi.vname);
	   None)
    | _ -> 
	if !debug then ignore(E.log "tmp_to_exp: rhs is call %s\n" vi.vname);
	None
  else None

(* Where it is possible, replace temps
   with expressions in e *)
(* IOS.t IH.t -> exp -> fundec -> bool -> exp *)
let rec rm_tmps_from_exp iosh e fd nofrm =
  if !debug then ignore(E.log "rm_tmps_from_exp: looking at: %a\n" d_plainexp e);
  match e with
    Lval(Var vi, NoOffset) as lv->
      (match tmp_to_exp iosh vi fd nofrm with
	None -> lv
      | Some e' -> e')
  | Lval(Var vi, off) ->
      let off' = rm_tmps_from_off iosh off fd nofrm in
      Lval(Var vi, off')
  | Lval(Mem e', off) ->
      let e'' = rm_tmps_from_exp iosh e' fd nofrm in
      let off' = rm_tmps_from_off iosh off fd nofrm in
      (* don't substitute constants in memory lvals *)
      (match e'' with Const _ -> Lval(Mem e',off')
      | _ -> Lval(Mem e'', off'))
  | SizeOfE e' -> SizeOfE(rm_tmps_from_exp iosh e' fd nofrm)
  | AlignOfE e' -> AlignOfE(rm_tmps_from_exp iosh e' fd nofrm)
  | UnOp(u,e',t) -> UnOp(u, rm_tmps_from_exp iosh e' fd nofrm, t)
  | BinOp(b,e1,e2,t) ->
      let e1' = rm_tmps_from_exp iosh e1 fd nofrm in
      let e2' = rm_tmps_from_exp iosh e2 fd nofrm in
      BinOp(b,e1',e2',t)
  | CastE(t, e') -> CastE(t, rm_tmps_from_exp iosh e' fd nofrm)
  | _ -> e

and rm_tmps_from_off iosh off fd nofrm=
  match off with
    NoOffset -> NoOffset
  | Field(fi, off') -> Field(fi, rm_tmps_from_off iosh off' fd nofrm)
  | Index(e,off') ->
      let e' = rm_tmps_from_exp iosh e fd nofrm in
      let off'' = rm_tmps_from_off iosh off' fd nofrm in
      Index(e',off'')

class expTempElimClass (fd:fundec) = object (self)
  inherit RD.rdVisitorClass

  method vexpr e =

    let do_change iosh vi =
      let ido = RD.iosh_singleton_lookup iosh vi in
      (match ido with
	Some id ->
	  let riviho = getDefRhs id in
	  (match riviho with
	    Some(RDExp(e) as r, _, defiosh) ->
	      if !debug then ignore(E.log "Can I replace %s with %a?\n" vi.vname d_exp e);
	      if ok_to_replace iosh defiosh fd r
	      then 
		(if !debug then ignore(E.log "Yes.\n");
		 ChangeTo(e))
	      else (if !debug then ignore(E.log "No.\n");
		    DoChildren)
	  | _ -> DoChildren)
      | _ -> DoChildren)
    in

    match e with
      Lval (Var vi,off) ->
	(if check_forms vi.vname forms then
	 (* only allowed to replace a tmp with a function call once *)
	  (match cur_rd_dat with
	    Some(_,s,iosh) -> do_change iosh vi
	  | None -> let iviho = RD.getRDs sid in
	    match iviho with
	      Some(_,s,iosh) -> 
		(if !debug then ignore (E.log "Try to change %s outside of instruction.\n" vi.vname);
		 do_change iosh vi)
	    | None -> 
		(if !debug then ignore (E.log "%s in statement w/o RD info\n" vi.vname);
		 DoChildren))
	else DoChildren)
    | _ -> DoChildren

end

class incdecTempElimClass (fd:fundec) = object (self)
  inherit RD.rdVisitorClass

  method vexpr e =

    let do_change iosh vi =
      let ido = RD.iosh_singleton_lookup iosh vi in
      (match ido with
	Some id ->
	  let riviho = getDefRhs id in
	  (match riviho with
	    Some(RDExp(e) as r, _, defiosh) ->
	      (match ok_to_replace_with_incdec iosh defiosh fd id vi r with
		Some(curdef_stmt_id,redefid, rhsvi, b) ->
		  (if !debug then ignore(E.log "No, but I can replace it with a post-inc/dec\n");
		   if !debug then ignore(E.log "cdsi: %d redefid: %d name: %s\n"
					   curdef_stmt_id redefid rhsvi.vname);
		   IH.add incdecHash vi.vid (redefid, rhsvi, b);
		   id_dh_add rhsvi.vid (curdef_stmt_id, redefid);
		   DoChildren)
	      | None ->
		  (if !debug then ignore(E.log "No.\n");
		   DoChildren))
	  | _ -> DoChildren)
      | _ -> DoChildren)
    in

    match e with
      Lval (Var vi,off) ->
	(if check_forms vi.vname forms then
	 (* only allowed to replace a tmp with a function call once *)
	  (match cur_rd_dat with
	    Some(_,s,iosh) -> do_change iosh vi
	  | None -> let iviho = RD.getRDs sid in
	    match iviho with
	      Some(_,s,iosh) -> 
		(if !debug then ignore (E.log "Try to change %s outside of instruction.\n" vi.vname);
		 do_change iosh vi)
	    | None -> 
		(if !debug then ignore (E.log "%s in statement w/o RD info\n" vi.vname);
		 DoChildren))
	else DoChildren)
    | _ -> DoChildren

end

class callTempElimClass (fd:fundec) = object (self)
  inherit RD.rdVisitorClass

  method vexpr e =

    let do_change iosh vi =
      let ido = RD.iosh_singleton_lookup iosh vi in
      (match ido with
	Some id ->
	  let riviho = getDefRhs id in
	  (match riviho with
	    Some(RDCall(i) as r, _, defiosh) ->
	      if !debug then ignore(E.log "Can I replace %s with %a?\n" vi.vname d_instr i);
	      if ok_to_replace iosh defiosh fd r
	      then (if !debug then ignore(E.log "Yes.\n");
		    IH.add iioh vi.vid (Some(i));
		    DoChildren)
	      else (if !debug then ignore(E.log "No.\n");
		    DoChildren)
	  | _ -> DoChildren)
      | _ -> DoChildren)
    in

    match e with
      Lval (Var vi,off) ->
	(if check_forms vi.vname forms then
	 (* only allowed to replace a tmp with a function call once *)
	  if IH.mem iioh vi.vid
	  then (IH.replace iioh vi.vid None; DoChildren)
	  else
	    (match cur_rd_dat with
	      Some(_,s,iosh) -> do_change iosh vi
	    | None -> let iviho = RD.getRDs sid in
	      match iviho with
		Some(_,s,iosh) -> 
		  (if !debug then ignore (E.log "Try to change %s outside of instruction.\n" vi.vname);
		   do_change iosh vi)
	      | None -> 
		  (if !debug then ignore (E.log "%s in statement w/o RD info\n" vi.vname);
		   DoChildren))
	  else DoChildren)
    | _ -> DoChildren

end

(* Remove temp variables that are set but not used *)
(* This is different from dead code elimination because
   temps that can be eliminated during pretty printing
   are also considered *)
class unusedRemoverClass : cilVisitor = object(self)
    inherit nopCilVisitor

  val mutable unused_set = UD.VS.empty
  val mutable cur_func = dummyFunDec

  (* figure out which locals aren't used *)
  method vfunc f =	
    cur_func <- f;
    (* the set of used variables *)
    let used = List.fold_left (fun u s ->
      let u', _ = UD.computeDeepUseDefStmtKind s.skind in
      UD.VS.union u u') UD.VS.empty f.sbody.bstmts in

    (* the set of unused locals *)
    let unused = List.fold_left (fun un vi ->
      if UD.VS.mem vi used
      then un
      else (if !debug then ignore (E.log "unusedRemoverClass: %s is unused\n" vi.vname);
	    UD.VS.add vi un)) UD.VS.empty f.slocals in
    
    (* a filter function for picking out
       the local variables that need to be kept *)
    let good_var vi =
      not(UD.VS.mem vi unused) &&
      (not(IH.mem iioh vi.vid) ||
      (match IH.find iioh vi.vid with
	None -> true | Some _ -> false)) &&
      not(IH.mem incdecHash vi.vid)
    in
    let good_locals = List.filter good_var f.slocals in
    f.slocals <- good_locals;
    unused_set <- unused;
    DoChildren

  (* remove instructions that set variables
     that aren't used. Also remove instructions
     that set variables mentioned in iioh *)
  method vstmt stm =

    (* return the list of pairs with fst = f *)
    let findf_in_pl f pl =
      List.filter (fun (fst,snd) ->
	if fst = f then true else false)
	pl
    in

    (* Return true if the assignment of this
       variable in this statement is going to be
       replaced by a post-inc/dec *)
    let check_incdec vi e =
      if IH.mem idDefHash vi.vid then
	let pl = IH.find idDefHash vi.vid in
	match findf_in_pl stm.sid pl with (sid,redefid)::l ->
	  let rhso = getDefRhs redefid in
	  (match rhso with
	    None -> (if !debug then ignore (E.log "check_incdec: couldn't find rhs for def %d\n" redefid);
		     false)
	  | Some(rhs, _, indiosh) ->
	      (match rhs with
		RDCall _ -> (if !debug then ignore (E.log "check_incdec: rhs not an expression\n");
				false)
	      | RDExp e' -> 
		  if Util.equals e e' then true
		  else (if !debug then ignore (E.log "check_incdec: rhs of %d: %a, and needed redef %a not equal\n"
					      redefid d_plainexp e' d_plainexp e);
			false)))
	| [] -> (if !debug then ignore (E.log "check_incdec: current statement not in list: %d. %s = %a\n"
					    stm.sid vi.vname d_exp e);
		   false)
      else (if !debug then ignore (E.log "check_incdec: %s not in idDefHash\n" vi.vname);
	    false)
    in

    (* return true if the rhs will get
       pretty printed as a function call *)
    let will_be_call e =
      match e with
	Lval(Var vi,NoOffset) -> 
	  if not(IH.mem iioh vi.vid) then false
	  else (match IH.find iioh vi.vid with
	    None -> false | Some _ -> true)
      | _ -> false
    in
    
    (* a filter function for picking out
       the instructions that we want to keep *)
    (* instr -> bool *)
    let good_instr i =
      match i with
	Set((Var(vi),_),e,_) ->
	  if will_be_call e &&
	    not(List.mem vi cur_func.slocals)
	  then cur_func.slocals <- vi::cur_func.slocals;
	  (not (UD.VS.mem vi unused_set) &&
	   not (IH.mem incdecHash vi.vid) &&
	   not (check_incdec vi e)) ||
	   will_be_call e
	 | Call (Some(Var(vi),_),_,_,_) ->
	     (* If not in the table or entry is None,
		then it's good *)
	     not (IH.mem iioh vi.vid) ||
	     (match IH.find iioh vi.vid with
	       None -> true | Some _ -> false)
	   | _ -> true
    in

    (* If the result of a function call isn't used,
       then change to Call(None,...) *)
    let call_fixer i =
      match i with
	Call (Some(Var(vi),_),e,el,l) as c ->
	  if UD.VS.mem vi unused_set then
	    Call(None,e,el,l)
	  else c
      | _ -> i
    in

    match stm.skind with
      Instr il ->
	let newil = List.filter good_instr il in
	let newil' = List.map call_fixer newil in
	stm.skind <- Instr(newil');
	SkipChildren
    | _ -> DoChildren

end


(* eliminate some temporaries *)
(* Cil.fundec -> Cil.fundec *)
let eliminate_temps f =
  RD.computeRDs f;
  IH.clear iioh;
  IH.clear incdecHash;
  IH.clear idDefHash;
  let etec = new expTempElimClass f in
  let f' = visitCilFunction (etec :> cilVisitor) f in
  let idtec = new incdecTempElimClass f' in
  let f' = visitCilFunction (idtec :> cilVisitor) f' in
  let ctec = new callTempElimClass f' in
  let f' = visitCilFunction (ctec :> cilVisitor) f' in
  visitCilFunction (new unusedRemoverClass) f'
