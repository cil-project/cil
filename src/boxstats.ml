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

(* weimer: visitor routines to check and see if major types have
 * changed *)
let typeSizeHT = Hashtbl.create 555
class typeSizeSetVisitor = object
  inherit nopCilVisitor 
  method vglob g = 
    match g with
      GType (s, t, _) when s <> "" -> begin
        let bs = try bitsSizeOf t with _ -> 0 in
        Hashtbl.add typeSizeHT s bs;
        SkipChildren
      end
    | _ -> SkipChildren
end

class typeSizeCheckVisitor = object
  inherit nopCilVisitor 
  method vglob g =
    match g with 
      GType (s, t, l) when s <> "" && Hashtbl.mem typeSizeHT s ->  begin
        let new_bs = try bitsSizeOf t with _ -> 0 in
        let old_bs = Hashtbl.find typeSizeHT s in
        if (new_bs <> old_bs) then begin
          ignore (printf "Sizeof %s changed from %d to %d\n" s old_bs new_bs);
          let used_in_interface = ref None in
          Hashtbl.iter (fun k n -> 
            if n.Ptrnode.btype = t && Ptrnode.hasFlag n Ptrnode.pkInterface then
              used_in_interface := Some(n.Ptrnode.where)
                   ) Ptrnode.idNode ; 
          match !used_in_interface with
            None -> ()
          | Some(p,i) -> ignore (printf "  and is used in interface function %a\n" Ptrnode.d_place p)
        end;
        SkipChildren
      end
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
