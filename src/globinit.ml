
(* A module that pulls out certain global initializers and generates 
 * initialization code in a special per-module function *)
open Cil
open Pretty 
open Trace

module H = Hashtbl
module E = Errormsg

let mainname = ref "main"

(* Insert the global initializer in the main *)
let insertGlobInit (file: file) : unit = 
  match file.globinit with 
  | Some gi when not file.globinitcalled -> 
      let theFile : global list ref = ref [] in
      let inserted = ref false in
      List.iter 
        begin
          fun g ->
            (match g with
              GFun(m, lm) when m.svar.vname = !mainname ->
                (* Prepend a prototype *)
                theFile := GDecl (gi.svar, lm) :: !theFile;
                m.sbody.bstmts <- 
                   compactStmts (mkStmt (Instr [Call(None, 
                                                     Lval(var gi.svar), 
                                                     [], locUnknown)]) 
                                 :: m.sbody.bstmts);
                inserted := true;
                if !E.verboseFlag then
                  ignore (E.log "Inserted the globinit\n");
                file.globinitcalled <- true;
            | _ -> ());
            theFile := g :: !theFile (* Now put the global back *)
        end
        file.globals;
      if not !inserted then 
        ignore (E.warn "Cannot find %s to add global initializer %s" 
                  !mainname gi.svar.vname);
      file.globals <- List.rev !theFile
  | _ -> ()


let doFile (fl: file) : file = 
  let boxing = ref true in
  let rec doGlobal = function
      GVar (vi, Some init, l) as g -> 
        let hasPointers = 
          existsType 
            (fun t ->
              match t with 
                TPtr _ -> ExistsTrue
              | _ -> ExistsMaybe) vi.vtype in
        if !boxing && hasPointers then 
          let finit = getGlobInit fl in
          (* Now generate the code. Baseoff is the offset to the current 
           * compound  *)
          let rec initone (baseoff: offset) off what t acc = 
            let off' = addOffset off baseoff in
            match what with
              CompoundInit (t, initl) -> 
                foldLeftCompound (initone off') t initl acc
            | SingleInit ei -> 
                mkStmt (Instr [Set ((Var vi, off'), ei, locUnknown)]) 
                :: acc
          in
          let inits = 
            initone NoOffset NoOffset init vi.vtype finit.sbody.bstmts in 
          finit.sbody.bstmts <- compactStmts (List.rev inits);
          GVar (vi, None, l)
        else g
          
        (* Leave alone all other globals *)
    | GPragma (a, _) as g -> begin
        (match a with
        | Attr("box", [ACons("on",[])]) -> boxing := true
        | Attr("box", [ACons("off",[])]) -> boxing := false
        | _ -> ());
        g
    end
  | g -> g
  in
  let newglobals = List.map doGlobal fl.globals in (* Do this first *)
  let newfile = {fl with globals = newglobals} in
  if !Util.doCheck then begin
    ignore (E.log "Checking after globinit\n");
    Check.checkFile [] newfile
  end;
  insertGlobInit newfile;
  newfile
  


