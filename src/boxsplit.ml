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



(* A visitor that finds all locals that appear in a call or have their 
 * address taken *)

let dontSplit : (string, bool) H.t = H.create 111
class findVarsCantSplitClass : cilVisitor = object (self) 
  inherit nopCilVisitor
        
        (* expressions, to see the address being taken *)
  method vexpr (e: exp) : exp visitAction =
    match e with 
      AddrOf (Var v, NoOffset) -> H.add dontSplit v.vname true; SkipChildren
    | SizeOfE (Lval(Var v, NoOffset)) -> 
        H.add dontSplit v.vname true; SkipChildren
    | _ -> DoChildren

          (* Sometimes we have a gratuituous use of AddrOf 

  !!! Note. This is not correct without changes in the splitVisitor !!!
  method vlval (lv: lval) : lval visitAction = 
    match lv with 
      (Mem (CastE(_, AddrOf (Var v, NoOffset))) as b), off -> 
        (* Don't let vexpr see this AddrOf *)
        let off' = visitCilOffset (self :> cilVisitor) off in
        if off' != off then ChangeTo (b, off') else SkipChildren
    | _ -> DoChildren
*)

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

  method vtype t = SkipChildren

end
let findVarsCantSplit = new findVarsCantSplitClass



(* Map a variable name to a list of component variables, along with the 
 * accessor field name. The first field is supposed to always be the actual 
 * data. (the _p field) *)
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

        (* Split all function arguments in calls *)
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

        
  method vfunc (func: fundec) : fundec visitAction = 
    H.clear newvars;
    H.clear dontSplit;
    (* Visit the type of the function itself *)
    if splitArguments then 
      func.svar.vtype <- visitCilType (self :> cilVisitor) func.svar.vtype;
    (* Go over the locals and find the candidates *)
    ignore (visitCilBlock findVarsCantSplit func.sbody);
    let splitOneVar (makeit: string -> typ -> varinfo) (v: varinfo) = 
      (* Visit first the type of the variable *)
      if splitArguments then 
        v.vtype <- visitCilType (self :> cilVisitor) v.vtype;
      match unrollType v.vtype with
        TComp (comp, _) when comp.cstruct -> begin 
          match comp.cfields with
            ({fname="_p"} as fstfld) :: restflds -> begin
              let splits = splitVar makeit v fstfld restflds in
              H.add newvars v.vname splits
            end
          | _ -> ()
        end
      | _ -> ()
    in
    (* Now go over the formals and create the splits *)
    if splitArguments then begin
      (* Split all formals because we will split all arguments in function 
      * types *)
      let newformals = 
        List.fold_right 
          (fun f acc -> 
            if H.mem dontSplit f.vname then
              E.s (E.unimp "boxsplit: can't split formal %s in %s\n"
                     f.vname func.svar.vname)
            else begin
              (* Just split the variables *)
              splitOneVar (fun s t -> makeLocalVar func ~insert:false s t) f;
              try
                let splits = H.find newvars f.vname in
                List.fold_right (fun (_, v) forms -> v :: forms) splits acc
              with Not_found -> 
                (* No splits were created *)
                f :: acc
            end)
          func.sformals [] 
      in
      (* Now make sure we fix the type *)
      setFormals func newformals
    end;
    (* Now go over the locals and create the splits *)
    List.iter 
      (fun l -> if not (H.mem dontSplit l.vname) then 
        splitOneVar (fun s t -> makeTempVar func ~name:s t) l) 
      func.slocals;
    (* Now visit the body and change references to these variables *)
    ignore (visitCilBlock (self :> cilVisitor) func.sbody);
    H.clear newvars;
    H.clear dontSplit;
    SkipChildren  (* We are done with this function *)

  method vglob (g: global) : global list visitAction = 
    match g with 
     GFun (f, _) -> DoChildren
    | _ -> SkipChildren

  method vtype t = SkipChildren

end

let splitVarVisitor = new splitVarVisitorClass

(* We must split the types in a second pass, since the first pass uses the 
 * types to decide when to split *)
class splitTypeVisitorClass : cilVisitor = object (self)
  inherit nopCilVisitor

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
                    match comp.cfields with
                      ({fname="_p"} as fstfld) :: restflds -> begin
                        argsChanged := true;
                        let splits = splitVar makeVarinfo a fstfld restflds in
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

let splitTypeVisitor = new splitTypeVisitorClass

let splitLocals (f: file) : file = 
  let f' = visitCilFile splitVarVisitor f in
  if splitArguments then visitCilFile splitTypeVisitor f' else f'
  



