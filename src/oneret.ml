(* Make sure that there is exactly one Return statement in the whole body. 
 * Replace all the other returns with Goto. This is convenient if you later 
 * want to insert some finalizer code, since you have a precise place where 
 * to put it *)
open Cil
open Pretty

module E = Errormsg

let dummyVisitor = new nopCilVisitor

let oneret (f: Cil.fundec) : unit = 
  let fname = f.svar.vname in
  (* Get the return type *)
  let retTyp = 
    match f.svar.vtype with
      TFun(rt, _, _, _) -> rt
    | _ -> E.s (E.bug "Function %s does not have a function type\n" 
                  f.svar.vname)
  in
  (* Does it return anything ? *)
  let hasRet = match retTyp with TVoid _ -> false | _ -> true in
  (* Memoize the return result variable. Use only if hasRet *)
  let lastloc = ref locUnknown in 
  let retVar : varinfo option ref = ref None in
  let getRetVar (x: unit) : varinfo = 
    match !retVar with
      Some rv -> rv
    | None -> begin
        let rv = makeLocalVar f "__retres" retTyp in (* don't collide *)
        retVar := Some rv;
        rv
    end
  in
  (* Remember if we have introduced goto's *)
  let haveGoto = ref false in
  (* Memoize the return statement *)
  let retStmt : stmt ref = ref dummyStmt in
  let getRetStmt (x: unit) : stmt = 
    if !retStmt == dummyStmt then begin
      (* Must create a statement *)
      let rv = 
        if hasRet then Some (Lval(Var (getRetVar ()), NoOffset)) else None
      in
      let sr = mkStmt (Return (rv, !lastloc)) in
      retStmt := sr;
      sr
    end else
      !retStmt
  in
  (* Now scan all the statements. Know if you are the main body of the 
   * function and be prepared to add new statements at the end *)
  let rec scanStmts (mainbody: bool) = function
    | [] when mainbody -> (* We are at the end of the function. Now it is 
                           * time to add the return statement *)
        let rs = getRetStmt () in
        if !haveGoto then
          rs.labels <- (Label("return_label", !lastloc)) :: rs.labels;
        [rs]

    | [] -> []
    | ({skind=Return (None, l)} as s) :: rests -> 
        if mainbody && rests == [] then 
          scanStmts mainbody rests
        else begin
          let sgref = ref (getRetStmt ()) in
          s.skind <- Goto (sgref, l);
          haveGoto := true;
          s :: (scanStmts mainbody rests)
        end

    | ({skind=Return (Some rval, l)} as s) :: rests -> 
        if not hasRet then 
          E.s (E.unimp "Found return in subroutine %s\n" fname);
        s.skind <- Instr [Set((Var (getRetVar ()), NoOffset), rval, l)];
        if mainbody && rests == [] then
          s :: scanStmts mainbody rests
        else begin
          let sgref = ref (getRetStmt ()) in
          let sg = mkStmt (Goto (sgref, l)) in
          haveGoto := true;
          s :: sg :: (scanStmts mainbody rests)
        end

    | ({skind=If(eb,t,e,l)} as s) :: rests -> 
        s.skind <- If(eb, scanBlock false t, scanBlock false e, l);
        s :: scanStmts mainbody rests
    | ({skind=Loop(b,l)} as s) :: rests -> 
        s.skind <- Loop(scanBlock false b, l);
        s :: scanStmts mainbody rests
    | ({skind=Switch(e, b, cases, l)} as s) :: rests -> 
        s.skind <- Switch(e, scanBlock false b, cases, l);
        s :: scanStmts mainbody rests
    | s :: rests -> s :: scanStmts mainbody rests

  and scanBlock (mainbody: bool) (b: block) = 
    { bstmts = scanStmts mainbody b.bstmts; battrs = b.battrs; }

  in
  ignore (visitCilBlock dummyVisitor f.sbody) ; (* sets CurrentLoc *)
  lastloc := !currentLoc ;  (* last location in the function *)
  f.sbody <- scanBlock true f.sbody
        
      
  
