
(* A module that pulls out certain global initializers and generates 
 * initialization code in a special per-module function *)
open Cil
open Pretty 
open Trace

module H = Hashtbl
module E = Errormsg


let doFile (fl: file) : file = 
  let rec doGlobal = function
      GVar (vi, Some init, l) as g -> 
        let hasPointers = 
          existsType 
            (fun t ->
              match t with 
                TPtr _ -> ExistsTrue
              | _ -> ExistsMaybe) vi.vtype in
        if hasPointers then 
          let finit = getGlobInit fl in
          (* Now generate the code. Baseoff is the offset to the current 
           * compound  *)
          let rec initone (baseoff: offset) off what t acc = 
            let off' = addOffset off baseoff in
            match what with
              Compound (t, initl) -> 
                foldLeftCompound (initone off') t initl acc
            | _ -> mkSet (Var vi, off') what :: acc
          in
          let inits = initone NoOffset NoOffset init vi.vtype [finit.sbody] in 
          finit.sbody <- mkSeq (List.rev inits);
          GVar (vi, None, l)
        else g
          
        (* Leave alone all other globals *)
    | g -> g
  in
  let newglobals = List.map doGlobal fl.globals in (* Do this first *)
  let newfile = {fl with globals = newglobals} in
  if !Util.doCheck then begin
    ignore (E.log "Checking after globinit\n");
    Check.checkFile [] newfile
  end;
  newfile
  

