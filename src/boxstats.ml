open Cil
open Pretty
module E = Errormsg
module H = Hashtbl


(******** STATISTICS **********)
(* Collect statistics about the code *)
let counters : (string, int ref) H.t = H.create 23
class collectCureStats = object (self)
  inherit nopCilVisitor




  method vinst (i: instr) : instr list visitAction = 
    let isCheck name = 
      let prefix p s = 
        let lp = String.length p in
        let ls = String.length s in
        lp <= ls && String.sub s 0 lp = p
      in
      prefix "CHECK_" name
    in
    let incrCount (name: string) = 
      try
        let vr = H.find counters name in
        incr vr
      with Not_found -> begin
        let vr = ref 1 in
        H.add counters name vr
      end
    in
    match i with 
      Call(_, Lval(Var fvi, NoOffset), _, _) when isCheck fvi.vname -> 
        incrCount fvi.vname;
        SkipChildren
    | _ -> SkipChildren



end

let print (f: file) = 
  H.clear counters;
  ignore (visitCilFile (new collectCureStats) f);
  let allChecks = ref 0 in
  let l = 
    H.fold (fun name counter acc -> 
      allChecks := !allChecks + !counter;
      (name, !counter) :: acc) counters [] in
  (* Now sort for most frequent first *)
  let l = List.sort (fun (_, c1) (_, c2) -> (compare c2 c1)) l in
  H.clear counters;
  if !allChecks = 0 then 
    ignore (E.log "No checks were inserted!\n")
  else begin
    ignore (E.log "Static count of CHECKs inserted:\n");
    List.iter 
      (fun (n, c) -> ignore (E.log " %-30s %6d (%6.2f%%)\n"
                               n c (100.0 *. (float c) /. (float !allChecks))))
      l
  end
