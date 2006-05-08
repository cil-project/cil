(* Eliminate assignment instructions whose results are not
   used *)

open Cil
open Pretty

module E = Errormsg
module RD = Reachingdefs
module UD = Usedef
module IH = Inthash
module S = Stats
module RCT = Rmciltmps

module IS = Set.Make(
  struct
    type t = int
    let compare = compare
  end)

let debug = RD.debug


let usedDefsSet = ref IS.empty
(* put used def ids into usedDefsSet *)
(* assumes reaching definitions have already been computed *)
class usedDefsCollectorClass = object(self)
    inherit RD.rdVisitorClass

  method vexpr e =
    let u = UD.computeUseExp e in
    let add_defids iosh =
      UD.VS.iter (fun vi ->
	if IH.mem iosh vi.vid then 
	  let ios = IH.find iosh vi.vid in
	  if !debug then ignore(E.log "DCE: IOS size for vname=%s at stmt=%d: %d\n" 
				  vi.vname sid (RD.IOS.cardinal ios));
	  RD.IOS.iter (function
	      Some(i) -> 
		if !debug then ignore(E.log "DCE: def %d used: %a\n" i d_plainexp e);
		usedDefsSet := IS.add i (!usedDefsSet)
	    | None -> ()) ios
	else if !debug then ignore(E.log "DCE: vid %d:%s not in stm:%d iosh at %a\n"
				     vi.vid vi.vname sid d_plainexp e)) u
    in
    match self#get_cur_iosh() with
      Some(iosh) -> add_defids iosh; DoChildren
    | None ->
	if !debug then ignore(E.log "DCE: use but no rd data: %a\n" d_plainexp e);
	DoChildren

end


let removedCount = ref 0
(* Filter out instructions whose definition ids are not
   in usedDefsSet *)
class uselessInstrElim : cilVisitor = object(self)
  inherit nopCilVisitor

  method vstmt stm =
    let is_volatile vi =
      let vi_vol =
	List.exists (function (Attr("volatile",_)) -> true 
	  | _ -> false) vi.vattr in
      let typ_vol =
	List.exists (function (Attr("volatile",_)) -> true 
	  | _ -> false) (typeAttrs vi.vtype) in
      if !debug && (vi_vol || typ_vol) then 
	ignore(E.log "DCE: %s is volatile\n" vi.vname);
      if !debug && not(vi_vol || typ_vol) then 
	ignore(E.log "DCE: %s is not volatile\n" vi.vname);
      vi_vol || typ_vol
    in

    let test (i,(_,s,iosh)) =
      match i with 
	Call _ -> true 
      | Set((Var vi,NoOffset),_,_) ->
	  if vi.vglob || (is_volatile vi) then true else
	  let _, defd = UD.computeUseDefInstr i in
	  let rec loop n =
	    if n < 0 then false else
	    if IS.mem (n+s) (!usedDefsSet)
	    then true
	    else loop (n-1)
	  in
	  if loop (UD.VS.cardinal defd - 1)
	  then true
	  else (incr removedCount; false)
      | _ -> true
    in

    let filter il stmdat =
      let rd_dat_lst = RD.instrRDs il stm.sid stmdat false in
      let ildatlst = List.combine il rd_dat_lst in
      let ildatlst' = List.filter test ildatlst in
      let (newil,_) = List.split ildatlst' in
      newil
    in

    match RD.getRDs stm.sid with
      None -> DoChildren
    | Some(_,s,iosh) ->
	match stm.skind with
	  Instr il ->
	    stm.skind <- Instr(filter il ((),s,iosh));
	    SkipChildren
	| _ -> DoChildren
	    
end

(* until fixed point is reached *)
let elim_dead_code_fp (fd : fundec) :  fundec =
  (* fundec -> fundec *)
  let rec loop fd =
    usedDefsSet := IS.empty;
    removedCount := 0;
    IH.clear RCT.udDeepSkindHtbl;
    Hashtbl.clear RCT.wbHtbl;
    S.time "reaching definitions" RD.computeRDs fd;
    ignore(visitCilFunction (new usedDefsCollectorClass :> cilVisitor) fd);
    let fd' = visitCilFunction (new uselessInstrElim) fd in
    if !removedCount = 0 then fd' else loop fd'
  in
  loop fd

(* just once *)
let elim_dead_code (fd : fundec) :  fundec =
  (* fundec -> fundec *)
  usedDefsSet := IS.empty;
  removedCount := 0;
  IH.clear RCT.udDeepSkindHtbl;
  Hashtbl.clear RCT.wbHtbl;
  S.time "reaching definitions" RD.computeRDs fd;
  ignore(visitCilFunction (new usedDefsCollectorClass :> cilVisitor) fd);
  let fd' = visitCilFunction (new uselessInstrElim) fd in
  fd'

class deadCodeElimClass : cilVisitor = object(self)
    inherit nopCilVisitor

  method vfunc fd =
    let fd' = elim_dead_code fd in
    ChangeTo(fd')

end

let dce f =
  if !debug then ignore(E.log "DCE: starting dead code elimination\n");
  visitCilFile (new deadCodeElimClass) f
