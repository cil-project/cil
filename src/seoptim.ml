(* An optimizer based on symbolic evaluation (to account for simple 
 * assignments) *)
open Cil
open Pretty
open Clist

module E = Errormsg
module H = Hashtbl

let debug = true


(* A list of functions to be forcefully removed from the file. This only work 
 * for functions that return void *)
let forcefullyRemove : string list ref = ref []
let forcefullyRemoveAll : bool ref = ref false


(* Keep some stats: seen and eliminated *)
let seen : (string, int ref * int ref) H.t = H.create 29

let keepOne (name: string) = 
  try
    let s, _ = H.find seen name in
    incr s
  with Not_found -> H.add seen name (ref 1, ref 0)

let elimOne (name: string) = 
  if debug then
    ignore (E.log "Dropping %s at %t\n" name d_thisloc);
  try
    let s, e = H.find seen name in
    incr s; incr e
  with Not_found -> H.add seen name (ref 1, ref 1)


(* The checks that we have seen. Indexed by the name of the check and the 
 * list of arguments *)
let checks : (string * exp list, bool) H.t = H.create 29

(* Register file. Maps identifiers of local variables to expressions. We also 
 * remember if the expression depends on memory or depends on variables that 
 * depend on memory *)
let regFile : (int, exp * bool) H.t = H.create 29

let resetRegisterFile () = H.clear regFile; H.clear checks

let setRegister (id: int) (v: exp * bool) : unit = 
  H.remove regFile id;
  H.add regFile id v
  
let setMemory () = 
  (* Get a list of all mappings that depend on memory *)
  let depids = ref [] in
  H.iter (fun id v -> if snd v then depids := id :: !depids) regFile;
  (* And remove them from the register file *)
  List.iter (fun id -> H.remove regFile id) !depids

let dependsOnMem = ref false

  (* Rewrite an expression based on the current register file *)
class rewriteExpClass : cilVisitor = object
  inherit nopCilVisitor
  
  method vexpr = function
    | Lval (Var v, NoOffset) -> begin
        try
          let defined = H.find regFile v.vid in
          if (snd defined) then dependsOnMem := true;
          ChangeTo (fst defined)
        with Not_found -> DoChildren
    end
          
    | Lval (Mem _, _) -> dependsOnMem := true; DoChildren

    | _ -> DoChildren


end

let rewriteVisitor = new rewriteExpClass

(* Rewrite an expression and return the new expression along with an 
 * indication of whether it depends on memory *)
let rewriteExp (e: exp) : exp * bool = 
  dependsOnMem := false;
  let e' = visitCilExpr rewriteVisitor e in
  e', !dependsOnMem


(*** For each kind of check we have a class of checks that are relevant ***)



let doOneInstr (acc: instr clist) (i: instr) : instr clist = 
  match i with 
  | Set ((Var v, NoOffset), e, _) -> 
      setRegister v.vid (rewriteExp e);
      CConsR (acc, i)
      

  | Set ((Mem _, _), _, _) -> 
      setMemory ();
      CConsR (acc, i)

  | Call (None, Lval(Var check, NoOffset), args, _) 
      when (String.length check.vname >= 6 &&
            String.sub check.vname 0 6 = "CHECK_") 
    ->
      let keepIt = 
        if !forcefullyRemoveAll ||
        List.exists (fun x -> x = check.vname) !forcefullyRemove then
          false
        else begin
          if debug then ignore (E.log "Looking at call to %s\n" check.vname);
          (* Rewrite the arguments *)
          let args' = List.fold_right 
              (fun a acc -> 
                let a', _ = rewriteExp a in
                a' :: acc) args []
          in
          if H.mem checks (check.vname, args') then 
            false
          else begin
            H.add checks (check.vname, args') true;
            true
          end
        end
      in
      if keepIt then begin
        keepOne check.vname;
        CConsR (acc, i)
      end else begin
        elimOne check.vname;
        acc
      end

  | Call (Some (Var v, _), _, _, _) -> 
      (* This call might have set the memory *)
      setMemory ();
      (* Must discard all register file entries that depend on this value *)
      CConsR (acc, i)

  | Call (Some (Mem _, _), _, _, _) -> setMemory (); CConsR (acc, i)

  | i -> CConsR (acc, i)


class basicBlockOptimClass : cilVisitor = object
    inherit nopCilVisitor

    method vstmt (s: stmt) = 
      match s.skind with 
        Instr il -> 
          resetRegisterFile ();
          let il' = toList (List.fold_left doOneInstr empty il) in
          resetRegisterFile (); (* To make the GC happy *)
          s.skind <- Instr il';
          SkipChildren

      | _ -> DoChildren

  method vexpr _ = SkipChildren
  method vtype _ = SkipChildren
end

let basicBlockOptim = new basicBlockOptimClass

let optim (f: file) : file = 
  H.clear seen;
  let f' = Stats.time "seoptim" (visitCilFile basicBlockOptim) f in
  if !E.verboseFlag then begin
    let totalChecks = ref 0 in
    let elimChecks  = ref 0 in
    H.iter (fun name (seen, elim) -> 
      totalChecks := !totalChecks + !seen;
      elimChecks := !elimChecks + !elim;
      ignore (E.log "  %-30s seen %d, eliminated %d (%6.2f%%)\n"
                name !seen !elim (100.0 *. (float_of_int !elim) /. (float_of_int !seen)));
      ) seen;
      ignore (E.log "  %-30s seen %d, eliminated %d (%6.2f%%)\n"
                "Total" !totalChecks !elimChecks 
                (100.0 *. (float_of_int !elimChecks) /. (float_of_int !totalChecks)));
  end;
  f'
    
  
