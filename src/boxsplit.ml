open Cil
open Pretty
open Trace

module H = Hashtbl
module E = Errormsg

let splitArguments = false
let lu = locUnknown

(* Go over the code and split fat temporary variables into several separate 
 * variables. The hope is that the compiler will have an easier time to do 
 * standard optimizations with the resulting scalars *)

(* Map a variable name to a list of component variables, along with the 
 * accessor field name. The first field is supposed to always be the actual 
 * data. *)
let newvars : (string, (fieldinfo * varinfo) list) H.t 
    = H.create 13
let splitVar (mkcompvar: string -> typ -> varinfo) 
             (v: varinfo) 
             (fstfld: fieldinfo)
             (restflds: fieldinfo list) : (fieldinfo * varinfo) list = 
  (* Update the type of the variable *)
  v.vtype <- fstfld.ftype;
  (fstfld, v) ::
  (List.map (fun f -> (f, mkcompvar (v.vname ^ f.fname) f.ftype)) restflds)

(* May raise Not_found if the variable does not have components *)
let getVarComponent (vname: string)
                    (fname: string) : varinfo =  
  match List.filter (fun (fn, compvar) -> fn.fname = fname) 
                    (H.find newvars vname) with
    [ (_, compvar) ] -> compvar
  | _ -> E.s (E.bug "Cannot find component %s of %s\n" fname vname)




(* A visitor that finds all locals that appear in a call or have their 
 * address taken *)

let dontSplit : (string, bool) H.t = H.create 111
class findVarsToSplitClass : cilVisitor = object (self) 
  inherit nopCilVisitor
        
        (* expressions, to see the address being taken *)
  method vexpr (e: exp) : exp visitAction =
    match e with 
      AddrOf (Var v, NoOffset) -> H.add dontSplit v.vname true; SkipChildren
    | _ -> DoChildren

          (* variables involved in call instructions *)
  method vinst (i: instr) : instr list visitAction = 
    match i with 
      Call (res, f, args, _) -> 
        (match res with 
          Some (Var v, NoOffset) -> H.add dontSplit v.vname true
        | _ -> ());
        if not splitArguments then
          List.iter (fun a -> 
            match a with
              Lval (Var v, NoOffset) -> H.add dontSplit v.vname true
            | _ -> ()) args; 
        (* Now continue the visit *)
        DoChildren
    | _ -> DoChildren

          (* Variables used in return should not be split *)
  method vstmt (s: stmt) : stmt visitAction = 
    match s.skind with 
      Return (Some (Lval (Var v, NoOffset)), _) -> 
        H.add dontSplit v.vname true; DoChildren
    | Return (Some e, _) -> 
        DoChildren
    | _ -> DoChildren

end
let findVarsToSplit = new findVarsToSplitClass


class splitVarVisitorClass : cilVisitor = object (self)
  inherit nopCilVisitor

      (* Whenever we see a variable with a field access we try to replace it 
       * by its components *)
  method vlval ((b, off) : lval) : lval visitAction = 
    try
      match b, off with
        Var v, Field (fi, restoff) -> 
          ChangeTo (Var (getVarComponent v.vname fi.fname), restoff)
      | _ -> DoChildren
    with Not_found -> DoChildren
        
        (* Sometimes we pass the variable as a whole to a function or we 
         * assign it to something *)
  method vinst (i: instr) : instr list visitAction = 
    match i with
      (* Split into two instructions and then do children. Howver, v._p might 
       * appear in the lv and if we duplicate the instruction we might get 
       * bad results. To fix this problem we make sure that the update to _p 
       * happens last. Other fields of v should not appear in lv. *)
      Set ((Var v, NoOffset), Lval lv, l) when H.mem newvars v.vname -> begin
        match H.find newvars v.vname with
          fst :: rest -> 
            ChangeTo 
              (List.map 
                 (fun (fld, newv) ->  
                   let lv' = 
                     visitCilLval (self :> cilVisitor)
                       (addOffsetLval (Field(fld, NoOffset)) lv) in
                   Set((Var newv, NoOffset), Lval lv', l))
                 (rest @ [fst]))
        | _ -> E.s (errorLoc l "No first field (in splitFats)")
      end 
 
      | Set (lv, Lval (Var v, NoOffset), l) when H.mem newvars v.vname ->
          begin
            match H.find newvars v.vname with
              fst :: rest -> 
                ChangeTo  
                  (List.map 
                     (fun (fld, newv) -> 
                       let lv' = 
                         visitCilLval (self :> cilVisitor)
                           (addOffsetLval (Field(fld, NoOffset)) lv) in
                       Set(lv', Lval (Var newv, NoOffset), l))
                     (rest @ [fst]))
            | _ -> E.s (errorLoc l "No first field (in splitFats)")
          end 

      | Call (ret, f, args, l) when splitArguments ->
          (* Visit the children first and then see if we must change the 
           * arguments *)
          let finishArgs = function
              [Call (ret', f', args', l')] as i' -> 
                let mustChange = ref false in
                let newargs = 
                  List.fold_right
                    (fun a acc -> 
                      match a with
                        Lval (Var v, NoOffset) when H.mem newvars v.vname -> 
                          begin
                            mustChange := true;
                            (List.map 
                               (fun (_, newv) -> 
                                 Lval (Var newv, NoOffset)) 
                               (H.find newvars v.vname))
                            @ acc
                          end
                      | _ -> a :: acc)
                    args'
                    []
                in
                if !mustChange then 
                  [Call (ret', f', newargs, l')]
                else
                  i'
            | _ -> E.s (E.bug "splitVarVisitorClass: expecting call")
          in
          ChangeDoChildrenPost ([i], finishArgs)

      | _ -> DoChildren

            (* Split the arguments in function types *)
  method vtype (t: typ) : typ visitAction = 
    (* Do the children first *)
    let funafter (t: typ) = 
      match t with
        TFun(rt', args', va', a') -> 
          let argsChanged = ref false in
          let newargs =
            List.fold_right 
              (fun a acc ->
                match unrollType a.vtype with
                  TComp (comp, _) when comp.cstruct -> begin
                    argsChanged := true;
                    match comp.cfields with
                      ({fname="_p"} as fstfld) :: restflds -> begin
                        let mkcompvar (n: string) (ct: typ) = 
                          { vname = n; vid = 0; 
                            vglob = false; vtype = ct; 
                            vdecl = lu; vattr = [];
                            vstorage = NoStorage;
                            vaddrof = false;
                            vreferenced = false }
                        in
                        let splits = splitVar mkcompvar a fstfld restflds in
                        (List.map (fun (_, a') -> a) splits) @ acc
                      end
                    | _ -> a :: acc
                  end
                | _ -> a :: acc)
              args'
              []
          in
          if !argsChanged then
            TFun(rt', newargs, va', a')
          else
            t
      | _ -> t
    in
    if splitArguments then 
      ChangeDoChildrenPost (t, funafter)
    else
      DoChildren


end

let splitVarVisitor = new splitVarVisitorClass

let splitLocals (func: fundec) : unit = 
  (* Go over the locals and find the candidates *)
  ignore (visitCilBlock findVarsToSplit func.sbody);
  let splitOneVar (v: varinfo) = 
    if not (H.mem dontSplit v.vname) then begin
      match unrollType v.vtype with
        TComp (comp, _) when comp.cstruct -> begin 
          match comp.cfields with
            ({fname="_p"} as fstfld) :: restflds -> begin
              let splits = 
                splitVar (fun s -> makeTempVar func ~name:s) 
                  v fstfld restflds in
              H.add newvars v.vname splits
            end
          | _ -> ()
        end
      | _ -> ()
    end
  in
  (* Now go over the locals and create the splits *)
  List.iter splitOneVar func.slocals;
  (* Now visit the body and change references to these variables *)
  visitCilFunction splitVarVisitor func;
  H.clear newvars;
  H.clear dontSplit
  

