
(* A module that pulls out certain global initializers and generates 
 * initialization code in a special per-module function *)
open Cil
open Pretty 
open Trace

module H = Hashtbl
module E = Errormsg


(* Memoize a function  *)
let fileInit : fundec  option ref = ref None

(* See if we have created an initializer already *)
let getFileInit fileName =    
  match !fileInit with 
    Some f -> f
  | None -> begin
      let f = emptyFunction ("__globinit_" ^ 
                             (Filename.chop_extension
                                (Filename.basename fileName))) 
      in
      fileInit := Some f;
      f
  end


let doFile (fl: file) : file = 
  fileInit := None;
  let rec doGlobal = function
      GVar (vi, Some init, l) as g -> 
        let hasPointers = 
          existsType 
            (fun t ->
              match t with 
                TPtr _ -> ExistsTrue
              | _ -> ExistsMaybe) vi.vtype in
        if hasPointers then 
          let finit = getFileInit fl.fileName in
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
  let rec doAllGlobals = function
      [] ->  begin  (* Now see if we must add the global initializer *)
        match !fileInit with
          Some f -> 
            ignore (E.warn "Added global initializer %s" f.svar.vname);
            [GFun (f, locUnknown)]
        | _ -> []
      end
    | g :: grest -> 
        let g' = doGlobal g in (* Make sure this is done first *)
        g' :: doAllGlobals grest
  in
  let newfile = {fl with globals = doAllGlobals fl.globals} in
  if !Util.doCheck then begin
    ignore (E.log "Checking after globinit\n");
    Check.checkFile [] newfile
  end;
  newfile
  

